---
title: Scaling from Pandas to Dask
description: Two Group By Examples in Pandas and Dask
toc: true
---

# 1. Introduction

Scaling [Pandas](https://pandas.pydata.org/) DataFrame aggregations can be
quite tricky. I have had a very specific problem to solve that involved
aggregates on group by expressions.<!--more-->

While it is certainly possible to squeeze out a lot of performance in Pandas in
a single-threaded environment, the nature of Pandas makes it hard to scale
_embarrassingly parallel_ problems. This frequently involves calculations on
DataFrames where the individual results are dependent only on specific segments
of a DataFrame, such as weeks, months, and so on. In this case, a DataFrame
could be split apart into multiple DataFrame across segment boundaries and the
calculations could theoretically be distributed across several threads, or even
machines.

At the same time, Pandas already is a complex library with a lot of moving
parts, so I can understand the decision to not build in parallelization
functionality. This is where [Dask](https://dask.pydata.org/en/latest/) comes
in quite handy. Dask allows users to parallelize computations on Pandas
DataFrames. Not only can those computations be parallelized on one machine,
they can also be easily scaled to multiple machines. It is also important to
mention that it allows computations on DataFrames that are bigger than
available RAM. It achieves this by only reading the parts into memory that are
currently needed and immediately freeing up memory as soon as data is not
needed anymore. This can be especially handy when a lot of intermediate steps
are needed for computation.

This article covers two topics.

- How to create DataFrames in Pandas and Dask
- How to perform group by operations on Pandas and Dask DataFrames

First, we will quickly go through DataFrames creation in both Pandas and Dask.
We use randomly created weather data for our data analysis and after that show
how to calculate certain metrics and statistics by just relying on group by
functionality.


# 2. Creating the DataFrames

First, we take a look at how data frames are created in Pandas and Dask.

## 2.1. Pandas

The preferred way of creating DataFrames from `pandas.Series` objects is by
creating a dictionary of the data and passing it to the `pandas.DataFrame`
constructor. So one would effectively run

```python
pd.DataFrame(
    {
        'column_a': pd.Series(...),
        'column_b': pd.Series(...),
    },
    index=pd.Series(...)
)
```

But first, we load some libraries. We import `dask.dataframe` and `pandas` to
create DataFrames and series in both libraries. We also import `numpy` for
random data creation.


```python
import pandas as pd
import dask.dataframe as dd
import numpy as np
```

```python
np.random.seed(1)
```

We set the start date to `2001-01-01`.


```python
start = '2001-01-01'
```

The data we initialize consist of two columns and one index:

- Index: A date and timestamp combination starting in `2001-01-01` ranging up
to n and filled with one entry per day.
- Rain: Indicates whether it rained on that day. Stored as a boolean. The value
is true with a 50 % likelihood.
- Temperature: The temperature on that day. Uniformly selected from `[5, 30)`,
a half-open interval.


```python
def create_df(n_days):
    dates = pd.date_range(start, periods=n_days, freq='D', name='Date')
    rainy_days = np.random.choice([False, True], n_days)
    temperatures = np.random.randint(5, 30, n_days)
    # Combine Pandas Series into DataFrame
    return pd.DataFrame(
        {
            'Rain': rainy_days,
            'Temperature': temperatures,
        },
        index=dates,
    )
```

We create sample data for 2 weeks.


```python
create_df(14)
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
     Date
    </th>
    <th>
     Rain
    </th>
    <th>
     Temperature
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     2001-01-01
    </th>
    <td>
     True
    </td>
    <td>
     18
    </td>
   </tr>
   <tr>
    <th>
     2001-01-02
    </th>
    <td>
     True
    </td>
    <td>
     11
    </td>
   </tr>
   <tr>
    <th>
     2001-01-03
    </th>
    <td>
     False
    </td>
    <td>
     23
    </td>
   </tr>
   <tr>
    <th>
     2001-01-04
    </th>
    <td>
     False
    </td>
    <td>
     25
    </td>
   </tr>
   <tr>
    <th>
     2001-01-05
    </th>
    <td>
     True
    </td>
    <td>
     10
    </td>
   </tr>
   <tr>
    <th>
     2001-01-06
    </th>
    <td>
     True
    </td>
    <td>
     23
    </td>
   </tr>
   <tr>
    <th>
     2001-01-07
    </th>
    <td>
     True
    </td>
    <td>
     25
    </td>
   </tr>
   <tr>
    <th>
     2001-01-08
    </th>
    <td>
     True
    </td>
    <td>
     16
    </td>
   </tr>
   <tr>
    <th>
     2001-01-09
    </th>
    <td>
     True
    </td>
    <td>
     15
    </td>
   </tr>
   <tr>
    <th>
     2001-01-10
    </th>
    <td>
     False
    </td>
    <td>
     19
    </td>
   </tr>
   <tr>
    <th>
     2001-01-11
    </th>
    <td>
     False
    </td>
    <td>
     23
    </td>
   </tr>
   <tr>
    <th>
     2001-01-12
    </th>
    <td>
     True
    </td>
    <td>
     9
    </td>
   </tr>
   <tr>
    <th>
     2001-01-13
    </th>
    <td>
     False
    </td>
    <td>
     28
    </td>
   </tr>
   <tr>
    <th>
     2001-01-14
    </th>
    <td>
     True
    </td>
    <td>
     28
    </td>
   </tr>
  </tbody>
 </table>
</section>

## 2.2. Dask

In order to create a Dask DataFrame, we use the existing Pandas DataFrame
creation function and use `Dask.dataframe.from_pandas` to convert it into a
chunked Dask DataFrame. As chunk size we use the amount of days in a year. The
idea with chunk sizes is to use a big enough number as this will dictate the
size of an individual Pandas DataFrame.

Since Dask operations will be performed on individual Pandas DataFrames, it is
important to choose a number that is useful for the type of operation you want
to perform on a DataFrame. For example, if you group by years, then choosing a
chunk size of one year worth of data lets you group more easily.


```python
def create_ddf(n_days):
    df = create_df(n_days)
    return dd.from_pandas(df, chunksize=365)
```

We can see that Dask DataFrames do not immediately return a result. Rather, we
have to compute it first:


```python
create_ddf(7)
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
     npartitions=1
    </th>
    <th>
     Rain
    </th>
    <th>
     Temperature
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     2001-01-01
    </th>
    <td>
     bool
    </td>
    <td>
     int64
    </td>
   </tr>
   <tr>
    <th>
     2001-01-07
    </th>
    <td>
     ...
    </td>
    <td>
     ...
    </td>
   </tr>
  </tbody>
 </table>
</section>

A Dask DataFrame can be evaluated by calling the `compute()` method on it.
Since we are not using `Dask.distributed` right now, this will simply be
executed on one thread.


```python
create_ddf(7).compute()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
     Date
    </th>
    <th>
     Rain
    </th>
    <th>
     Temperature
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     2001-01-01
    </th>
    <td>
     False
    </td>
    <td>
     13
    </td>
   </tr>
   <tr>
    <th>
     2001-01-02
    </th>
    <td>
     False
    </td>
    <td>
     12
    </td>
   </tr>
   <tr>
    <th>
     2001-01-03
    </th>
    <td>
     True
    </td>
    <td>
     8
    </td>
   </tr>
   <tr>
    <th>
     2001-01-04
    </th>
    <td>
     True
    </td>
    <td>
     11
    </td>
   </tr>
   <tr>
    <th>
     2001-01-05
    </th>
    <td>
     True
    </td>
    <td>
     26
    </td>
   </tr>
   <tr>
    <th>
     2001-01-06
    </th>
    <td>
     False
    </td>
    <td>
     22
    </td>
   </tr>
   <tr>
    <th>
     2001-01-07
    </th>
    <td>
     True
    </td>
    <td>
     8
    </td>
   </tr>
  </tbody>
 </table>
</section>

# 3. Longest number of consecutive rainy days

## 3.1. Algorithm

For our first group by, we would like to find the longest chain of rainy days
in the whole DataFrame. We use a neat trick for this. Using a combination of
`Series.shift()` and `Series.cumsum()`, we can create an auxiliary series that
tracks the difference and allows us to perform a group by on the difference
list.

Let's take a look at an example. First, a DataFrame is created with 2
consecutive rainy days


```python
sunny_example = pd.DataFrame(
    {
        'Rain': [False, False, False, True, True, False],
        'Date': [0, 1, 2, 3, 4, 5],
    }
)
sunny_example
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Date
    </th>
    <th>
     Rain
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     0
    </th>
    <td>
     0
    </td>
    <td>
     False
    </td>
   </tr>
   <tr>
    <th>
     1
    </th>
    <td>
     1
    </td>
    <td>
     False
    </td>
   </tr>
   <tr>
    <th>
     2
    </th>
    <td>
     2
    </td>
    <td>
     False
    </td>
   </tr>
   <tr>
    <th>
     3
    </th>
    <td>
     3
    </td>
    <td>
     True
    </td>
   </tr>
   <tr>
    <th>
     4
    </th>
    <td>
     4
    </td>
    <td>
     True
    </td>
   </tr>
   <tr>
    <th>
     5
    </th>
    <td>
     5
    </td>
    <td>
     False
    </td>
   </tr>
  </tbody>
 </table>
</section>

Using `Series.shift()` a difference list is calculated.


```python
diff = sunny_example.Rain != sunny_example.Rain.shift()
diff.to_frame()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Rain
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     0
    </th>
    <td>
     True
    </td>
   </tr>
   <tr>
    <th>
     1
    </th>
    <td>
     False
    </td>
   </tr>
   <tr>
    <th>
     2
    </th>
    <td>
     False
    </td>
   </tr>
   <tr>
    <th>
     3
    </th>
    <td>
     True
    </td>
   </tr>
   <tr>
    <th>
     4
    </th>
    <td>
     False
    </td>
   </tr>
   <tr>
    <th>
     5
    </th>
    <td>
     True
    </td>
   </tr>
  </tbody>
 </table>
</section>

Now, the value is `True` only when the weather changes from rainy to sunny or
vice versa. Following this, the difference list is summed up using
`Series.cumsum()`.


```python
diff.cumsum().to_frame()
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Rain
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     0
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     1
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     3
    </th>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     4
    </th>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     5
    </th>
    <td>
     3
    </td>
   </tr>
  </tbody>
 </table>
</section>

Using these values, rainy and non-rainy consecutive days can be grouped:


```python
sunny_example_result = sunny_example.groupby(diff.cumsum()).Rain.agg(
    ['min', 'max', 'count'],
)
sunny_example_result
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
     Rain
    </th>
    <th>
     min
    </th>
    <th>
     max
    </th>
    <th>
     count
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     1
    </th>
    <td>
     False
    </td>
    <td>
     False
    </td>
    <td>
     3
    </td>
   </tr>
   <tr>
    <th>
     2
    </th>
    <td>
     True
    </td>
    <td>
     True
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     3
    </th>
    <td>
     False
    </td>
    <td>
     False
    </td>
    <td>
     1
    </td>
   </tr>
  </tbody>
 </table>
</section>

And the longest series of rainy days can be retrieved. `min` and `max` being
`True` tells us that the days in question only contain the boolean value `True`
and are therefore rainy days.


```python
query_result = sunny_example_result.query('min == max == True')['count']
max_idx = query_result.idxmax()
print(max_idx)
sunny_example_result.loc[max_idx].to_frame()
```

__Output:__

```
2
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     2
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     min
    </th>
    <td>
     True
    </td>
   </tr>
   <tr>
    <th>
     max
    </th>
    <td>
     True
    </td>
   </tr>
   <tr>
    <th>
     count
    </th>
    <td>
     2
    </td>
   </tr>
  </tbody>
 </table>
</section>

## 3.2. Pandas

First, the number of days for the sample calculation is defined.


```python
years = 100
days = years * 365

aggregate = [
    'min', 'max', 'count',
]
```

Using the steps outlined before, the function operating on Pandas DataFrames is
defined.


```python
def df_consecutive(df):
    df = df.reset_index()
    # Create difference list
    diff = (df.Rain != df.Rain.shift()).cumsum()
    # Aggregate longest consecutive occurences
    agg = df.groupby(diff).Rain.agg(aggregate)
    # Return length of longest consecutive occurence
    return agg.query('min == max == True')['count'].max()
```

Let's quickly verify the result by running our function on the previous example
DataFrame.


```python
df_consecutive(sunny_example)
```

__Output:__

```
2
```

And we see that everything works as it is supposed to. Lastly, we time the
execution using the `%%timeit` IPython magic command.


```python
%%timeit
df_consecutive(create_df(days))
```

__Output:__

```
21.5 ms ± 1.06 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
```

## 3.3. Dask

Dask allows us to take any algorithm that we have developed using Pandas and
turn it into a Dask compatible execution graph. As the code below demonstrates,
in simple cases we do not have to adjust anything at all and can just go on
using our existing Pandas code.


```python
def ddf_consecutive(ddf):
    ddf = ddf.reset_index()
    # Create difference list
    diff = (ddf.Rain != ddf.Rain.shift()).cumsum()
    # Aggregate longest consecutive occurences
    agg = ddf.groupby(diff).Rain.agg(aggregate)
    # Return length of longest consecutive occurence
    return agg.query('min == max == True')['count'].max()
```

As an intermediate step, the previous example DataFrame is turned into a Dask
DataFrame.


```python
sunny_example_Dask = dd.from_pandas(sunny_example, chunksize=365)
sunny_example_Dask
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
     npartitions=1
    </th>
    <th>
     Date
    </th>
    <th>
     Rain
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     0
    </th>
    <td>
     int64
    </td>
    <td>
     bool
    </td>
   </tr>
   <tr>
    <th>
     5
    </th>
    <td>
     ...
    </td>
    <td>
     ...
    </td>
   </tr>
  </tbody>
 </table>
</section>

The computation graph for creating a Dask DataFrame from Pandas and retrieving
the maximum can be visualized quite easily.


```python
sunny_example_Dask.max().visualize(optimize_graph=True, layout='circo')
```

__Output:__

<img alt="IPython.core.display.Image object" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABhIAAAETCAYAAADTUJwFAAAAAXNSR0IArs4c6QAAQABJREFUeAHsnQm4FMX1tw9w2QyiIiYCiuBCVBbFXVBcUBH3YAR3QxQ1RNRETeLfJYtRop9blMRdo8YFJOCCgoqoIK6oEVFQUTHucQNEBQH7q18lfTN3mKVnn55563nunZnu6qpTb3VXn6pTdapZ4IIRIAABCEAAAhCAAAQgAAEIQAACEIAABCAAAQhAAAIQgEAKAs1THOMQBCAAAQhAAAIQgAAEIAABCEAAAhCAAAQgAAEIQAACEPAEMCRwI0AAAhCAAAQgAAEIQAACEIAABCAAAQhAAAIQgAAEIJCWAIaEtGg4AQEIQAACEIAABCAAAQhAAAIQgAAEIAABCEAAAhCAQAMIIAABCEAAAvVI4KuvvrIFCxbYu+++a5999pl9+umnq3wuWbLEli1bZt9++63/C7/rs0WLFtaqVSv/17p168ZPfV9zzTWtY8eOtvbaazf5XGeddWyDDTaw9ddf319fj9wpMwQgAAEIQAACEIAABCAAAQhAAALxI9CMzZbjV2lIDAEIQAAC0Qh8+eWX9vLLL9ucOXPszTff9IaDt99+239+8sknjYm0bNlylUF/GQHat2/faCBINhp89913qxgXZHBYunSpffHFF02MEjJU6Jh75/o8GxoabL311rPu3btbt27d/Ocmm2xiffr0sR49epjOEyAAAQhAAAIQgAAEIAABCEAAAhCAQLUQwJBQLTWBHBCAAAQgUBCBt956y55//nmbPXt2498777zjB+9lENhoo40aB+3DwXt9du3a1RsMCso8wsUrV670qx4WuFUQoTEj8VPfV6xYYVrRsPnmm1vv3r29YUHGhe22287WWGONCLkQBQIQgAAEIAABCEAAAhCAAAQgAAEIFJ8AhoTiMyVFCEAAAhAoMQENuL/44os2c+bMxr8PP/zQuwsKZ/ZrAD4cjJfBoNqD3CXNnTvXG0G0iiI0iHz00UfWvHlz69mzp/Xv39//7bTTTt4oUu1lQj4IQAACEIAABCAAAQhAAAIQgAAEaoMAhoTaqEdKAQEIQKDmCWhwffLkyfbggw/a008/bV9//bV3R9SvX7/GAfZtttnG2rRpU1MsPv74Y3vqqafsiSee8EaTF154wbtU6ty5s+266662995726BBg+z7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rlxpe+yxh3fvKqPCGmusEbsyITAEIACBuBHAkBC3GkNeCEAAAo6AZhntu+++NmfOHL90WUuVCRCodQLyoTtq1Ci78sor7aSTTqr14lI+CEAAAhCAAAQg0ISA9kW755577LrrrrNHHnnEVlttNTvggAP8YLpWH7Ru3bpJ/Fr9oVXZ9957r40dO9avVlA5xWHEiBHeuNC8efNaLTrlggAEIFBRAhgSKoqfzCEAAQjkTmD58uV+TwRtmvboo4/aFltskXsiXAGBmBK48MIL7cwzz7QbbrjBhg8fHtNSIDYEIAABCEAAAhCITuC1116z66+/3m6++Wb7/PPP/YoD6UFy8dO2bdvoCdVgzEWLFtnEiRO9bqiVGdow+thjj/V6YpcuXWqwxBQJAhCAQOUIYEioHHtyhgAEIJAzAS3hPeyww/yS3ocffti23377nNPgAgjEncA555xjF1xwgd1xxx02dOjQuBcH+SEAAQhAAAIQgEBKApo0dPHFF3uXPl27drWf/vSn/m+99dZLGb/eD86bN8+v1rjlllts4cKFfqXGaaedZn379q13NJQfAhCAQFEIsN6rKBhJBAIQgEDpCQRBYMcdd5xNmjTJ7rvvPowIpUdODlVK4LzzzrOTTz7ZjjzySP88VKmYiAUBCEAAAhCAAARyJrBixQq7/fbbbauttrLdd9/dtC+a3BlpX7Rzzz3XMCKkRyp3r5dccolpo+Ybb7zRXn75Zc9x4MCB3hiT/krOQAACEIBAFAIYEqJQIg4EIACBKiCgWdi33XabTZgwwXbZZZcqkAgRIFA5ApdddpkdffTRfqbZc889VzlByBkCEIAABCAAAQgUgcB3331nt956q2kwXDpOjx49TDrOY489Zvvvv7/h9z865FatWtlRRx1lL730kj300EPW0NDg3UBts8029sADD0RPiJgQgAAEINCEAIaEJjj4AQEIQKA6CWhGzfnnn2/XXHON94lanVIiFQTKS0DPw4ABA3znesGCBeXNnNwgAAEIQAACEIBAEQho1bE2De7Zs6f367/zzjvbG2+8YXfeeadp4JtQGIE999zTb8j84osvWufOnW3fffe1fv362dSpUwtLmKshAAEI1CEBDAl1WOkUGQIQiBcBKbknnHCCnX322WwuG6+qQ9oSE2jRooWNGzfO1l13Xd8plC9cAgQgAAEIQAACEIgLgccf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7XE3btw4O/LII23IkCH+e8yKgLgQgAAEIhPAtVFkVESEAAQgUBwC7777rm2++eZ2yimn2B//+MfiJEoqEIBAJALLli3zK4G6d+9uDzzwQKRriAQBCEAAAhCAAATOPfdcr7v/+c9/tlGjRgEEAk0IBEFgWmk+ZswYP1nsJz/5SZPz/IAABCBQCwRa/M6FWigIZYAABCAQFwJHHHGEyV+7fKvK3QoBAhAoHwE9c3379vWbL2+88cbWp0+f8mVOThCAAAQgAAEIxJLAmWeeaX/605/8APGJJ54YyzIgdGkJaB+uvffe27799ls7/fTTbb311vP74ZU2V1KHAAQgUF4CjGCVlze5QQACdU5g/Pjxfhb0448/bq1bt65zGhQfApUhoE2XNQigWWP77befrbHGGpURhFwhAAEIQAACEKh6Aueff75deOGF9re//c2OPvroqpcXAStLQCvOW7RoYccff7x973vfs0MPPbSyApE7BCAAgSISwLVREWGSFAQgAIFMBJYuXWqbbbaZDRgwwG6++eZMUTkHAQiUmMDChQttk002saOOOsouvfTSEudG8hCAAAQgAAEIxJGA3BideuqpdtVVV/lJCHEsAzJXhoBWJej+0USyAw88sDJCkCsEIACBIhNgs+UiAyU5CEAAAukIaEO2Tz75xEaPHp0uCschAIEyEVhzzTXtvPPO835sX3/99TLlSjYQgAAEIAABCMSFwA033OBXL1588cUYEeJSaVUkp+6bY4891oYNG2ZTp06tIskQBQIQgED+BFiRkD87roQABCAQmcAHH3xgPXr0MPlXPeussyJfR0QIQKB0BFauXOn3S+jatatNmjSpdBmRMgQgAAEIQAACsSIwZcoU23///b3u/oc//CFWsiNs9RDQBsxHHnmk1zOfeOIJ6927d/UIhyQQgAAE8iCAISEPaFwCAQhAIFcCxxxzjE2fPt3mzp1rbdq0yfVy4kMAAiUiMG3aNBs4cKBpwGDQoEElyoVkIQABCEAAAhCIC4GXXnrJdt55ZxsyZIjfFyEuciNndRLQ5svSMd988017+umnrXPnztUpKFJBAAIQiEAAQ0IESESBAAQgUAiB5557zrbffnsbO3asHXLIIYUkxbUQgEAJCGigYN68eTZ79mxraGgoQQ4kCQEIQAACEIBAHAi8//77Xm//4Q9/6CcZtGzZMg5iI2OV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dKkdffTRcRAXGSEAgRwJyEg4fvx4W7x4cY5XEh0CEIAABCAAgUoS+Oijj+yvf/2rnXnmmbb66qtXUhTyhkBkAhtuuKFpBc35559vy5Yti3wdESEAAQiUmgCGhFITJn0IQKDmCdx55502ePBg69ixY82XlQJCoB4JDBs2zLT53d13312PxafMEIAABCAAgdgS0GqEDh062IknnhjbMiB4fRI466yz7JNPPrFrr722PgFQaghAoCoJYEioympBKAhAIC4EPvvsM5s6dappoJEAAQjUJoE111zT9tprLxs3blxtFpBSQQACEIAABGqQwHvvvecHYTUg26ZNmxosIUWqZQKdO3f2BrDRo0fbN998U8tFpWwQgECMCGBIiFFlISoEIFB9BCZMmGAtW7a0/fffv/qEQyIIQKBoBGQsfOihh+yLL74oWpokBAEIQAACEIBA6QhoAHbddde1Y489tnSZkDIESkjgN7/5jS1atMiuueaaEuZC0hCAAASiE8CQEJ0VMSEAAQisQkAzlPfZZx9r167dKuc4AAEI1A6BAw44wJo3b457o9qpUkoCAQhAAAI1TECrhm+66SY7/fTTrVWrVjVcUopWywR+8IMf+L0SLr/8cluxYkUtF5WyQQACMSGAISEmFYWYEIBA9RGQz8pHH33Uhg4dWn3CIREEIFBUAu3bt/d7oYwdO7ao6ZIYBCAAAQhAAALFJ6ANltu2bWvDhw8vfuKkCIEyEjj11FNNbrrGjx9fxlzJCgIQgEBqAhgSUnPhKAQgAIGsBP7xj394f6v77bdf1rhEgAAE4k9ARsNHHnnENMuRAAEIQAACEIBAdRJYunSpjRkzxvuX/973vledQiIVBCIS6N69uw0ZMsQuueSSiFcQDQIQgEDpCGBIKB1bUoYABGqcwKRJk/wGrKuttlqNl5TiQQACIiCjYbNmzezBBx8ECAQgAAEIQAACVUrg73//u/crP2rUqCqVELEgkBsBueiaNWuWPf7447ldSGwIQAACRSaAIaHIQEkOAhCoDwLLli3zbo0GDx5cHwWmlBCAgK2++urWv39/mzx5MjQgAAEIQAACEKhSAldddZV3PaqNlgkQqAUC2223ne2www6me5sAAQhAoJIEMCRUkj55QwACsSUwffp0+/rrr23vvfeObRkQHAIQyJ2AjIdakRAEQe4XcwUEIAABCEAAAiUl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iy7y96pc1iSGp59+2v7+978nHor0PVu6EydOjJQOkaqPgPZHUOjQoUP1CVchicrdfl966aW+/a5Qcck2RwKJukCOlzZGj1ObqbYhbCcaC8CX2BCYNWuWff31117eTLrrkiVL7L777vM6ZLkKN2PGDPvjH/9oRx55pN1zzz3lyrbu85k3b57vo0ybNq0sLN5880278sorrRjtnvpVH374oZf78ccft3/9619NyiAf2ldddZX97W9/S9lPy/QMNEko6Ue2dJOip/2ZTZ9Oe2GKE5nSeuGFF1ZhkyIJDlUpgaVLl/p2u55WC1fiHVTt+nec2+piP1oyqjGppdhUy5deYt8p1XuYsczodVGJtlLSZdO/whJIn9c7LDmkqvfkOKl+Z9K/SqXrFNWQsGjRIvt//+//We/eve2bb76xc8891/75z3+mKqtJYb7mmmvsjDPOKJsvt5/+9KfWs2dP+/3vf2+33nqrXX755Sll42C8CVx77bV22223+ULceeeddv311zcWSIatzTbbzHccdD/cfffddsABB3j/92EkdeJkcMolREn3Bz/4gY0YMcL0kihXkJHkk08+KVd2NZtP2NC3bdu2ZsuYS8Eq0X7feOONdsstt+QiJnErRCBRFyhEhEq0mfnKq7YhbCdySWPlypUsQ88FWAniyjCgSQerrbZaVt1VExNOPvlkk25RjvD88897vfpXv/qV9ejRw4YNG9Zo8ChH/vWax/vvv29XXHGF76OUY+NKvVOlq+remj17dkHY1Q5Jr/32228tCAL78Y9/bBp8CMOFF15o0n8HDhxoG2+8se26664mY1UYsvXfwnjJn9nSTY6f7ncUfTrdtcnHs6XVp08fGz16tE2fPj350pL+1jsyn/dFSYWKYeKhC5d6MiSU+x2k26Ka9e84t9WleOQKWR2r9lLvDEJlCCT2ndK9hxnLjF43lWgrs+lfkv7++++3bbbZxg466CDf50gsUbp6T4yT6ns2/atkuo5rMJqEjz/+WC1I4GawNDme7Yfb2CXYf//9A7cpaWPUtdZaK3AKcePv5C+vvvqqz8sNDiWfyvj75ptvzng+1clnnnkmaGhoCNzAqj/tBlcDV9mponIs5gR23HHH4I477vClcJ2pwBkW/Hc3YBPstNNOgY6FwQ3qBxtssEHgVqiEh4I///nPwZAhQxp/Z/sSNV2lM3ny5GD4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"/>

Using the same Dask function we have specified above, we verify one more time
that the result is calculated correctly.


```python
b = ddf_consecutive(sunny_example_Dask)
b.compute()
```

__Output:__

```
2
```

The function is timed using the IPython `%%timeit` command. The Dask example is
at least 10 times slower.


```python
%%timeit
ddf_consecutive(create_ddf(days)).compute()
```

__Output:__

```
2.49 s ± 97.8 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
```

# 4. Determine the coldest weeks

For the second example, the coldest week in a DataFrame is determined.

## 4.1. Algorithm

In order to retrieve the coldest week, a combination of resampling and indexing
is used. We create a sample DataFrame:


```python
cold_example = pd.DataFrame(
    {
        'Temperature': [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1]
    }, index=pd.date_range(start='2017-01-01', periods=14)
)
cold_example
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     Temperature
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     2017-01-01
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-02
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-03
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-04
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-05
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-06
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-07
    </th>
    <td>
     0
    </td>
   </tr>
   <tr>
    <th>
     2017-01-08
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2017-01-09
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2017-01-10
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2017-01-11
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2017-01-12
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2017-01-13
    </th>
    <td>
     1
    </td>
   </tr>
   <tr>
    <th>
     2017-01-14
    </th>
    <td>
     1
    </td>
   </tr>
  </tbody>
 </table>
</section>

In order to resample the temperature by weeks, we can use the
`DataFrame.resample()` method. We take extra care to sample only full weeks and
only those that fully start in the year 2017.


```python
grouper = cold_example.Temperature.resample('W-MON', label='left')
grouper
```

__Output:__

```
DatetimeIndexResampler [freq=<Week: weekday=0>, axis=0, closed=right, label=left, convention=start, base=0]
```

For our purposes, the semantics of a resampler and Pandas grouper are the same.
We can apply aggregate functions such as `sum()` and `count()`. In this case,
`count()` tells us the length of a grouped week and the sum of the daily
temperatures.

It becomes evident below that we have two partial weeks in the dataset. One
begins in the last year, and the other one does not have enough days in the
current year. Therefore, the only usable week is the one starting in
`2017-01-02`.


```python
agg = grouper.aggregate(['sum', 'count'])
agg
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     sum
    </th>
    <th>
     count
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     2016-12-26
    </th>
    <td>
     0
    </td>
    <td>
     2
    </td>
   </tr>
   <tr>
    <th>
     2017-01-02
    </th>
    <td>
     2
    </td>
    <td>
     7
    </td>
   </tr>
   <tr>
    <th>
     2017-01-09
    </th>
    <td>
     5
    </td>
    <td>
     5
    </td>
   </tr>
  </tbody>
 </table>
</section>

We can therefore easily filter out partial weeks with the following query:


```python
agg.query("count == 7")
```

__Output:__

<section class="table-wrapper">
 <table>
  <thead>
   <tr>
    <th>
    </th>
    <th>
     sum
    </th>
    <th>
     count
    </th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <th>
     2017-01-02
    </th>
    <td>
     2
    </td>
    <td>
     7
    </td>
   </tr>
  </tbody>
 </table>
</section>

And the coldest week is the following:


```python
agg.query("count == 7")['sum'].idxmin()
```

__Output:__

```
Timestamp('2017-01-02 00:00:00', freq='W-MON')
```

## 4.2. Pandas

For Pandas, we simply put the above explained steps into one convenient
function.


```python
def df_coldest_week(df):
    weeks = df.Temperature.resample(
        'W-MON',
        label='left',
    ).agg(['count', 'sum']).query('count == 7')
    return weeks['sum'].idxmin()
```

We can easily verify that the result is correct.


```python
df_coldest_week(cold_example)
```

__Output:__

```
Timestamp('2017-01-02 00:00:00', freq='W-MON')
```

A quick `%%timeit` is run on the Pandas function.


```python
%%timeit
df_coldest_week(create_df(days))
```

__Output:__

```
151 ms ± 3.53 ms per loop (mean ± std. dev. of 7 runs, 10 loops each)
```

## 4.3. Dask

Since Dask resamplers do not support aggregates at the time of writing, we have
to trick around a little bit to get it to work.


```python
def ddf_coldest_week(df):
    weeks = df.Temperature.resample(
        'W-MON',
        label='left',
    )
    # Calculate week lengths
    count = weeks.count()
    # Sum temperatures
    sum = weeks.sum()
    # Select results by week day lengths.
    loc = count == 7
    return sum[loc].idxmin()
```

We evaluate the function on our sample DataFrame. First, we turn it into a Dask
DataFrame.


```python
cold_example_Dask = dd.from_pandas(cold_example, npartitions=1)
```

As we can see, the computed result is exactly the same.


```python
ddf_coldest_week(cold_example_Dask).compute()
```

__Output:__

```
Timestamp('2017-01-02 00:00:00')
```

Now we run the benchmark and discover that the Dask example takes about 10
times longer to execute.


```python
%%timeit
ddf_coldest_week(create_ddf(days)).compute()
```

__Output:__

```
2.45 s ± 26.5 ms per loop (mean ± std. dev. of 7 runs, 1 loop each)
```

# 5. Benchmarks

The benchmark results indicate that using Dask performs slower on the small
datasets that we have created. Whether this applies to bigger sized datasets as
well was not established in this article. What stands true in any case is that
Dask allows calculation on datasets that are larger than the available memory,
and are especially larger than any piece of continuously available memory in
the OS. This allows us to easily scale a calculation using almost the same code
to data sets in the 500 GB to 1 TB range.

I was able to successfully apply these insights for a large amount of
non-trivial A/B test calculations on user click data without having to set up a
complicated computing cluster.

Taking the weather calculations that we have performed as an example, one can
start by developing the aggregation functions using Pandas or Dask on a small
data set and test it thoroughly. Using the same function with minimal
adjustments, the process can then be scaled to data sets with hundreds of
measurement locations and still get reasonable performance across a multi-core
machine or even a cluster. This can be done quite easily on AWS EC2. The dask
documentation [has a tutorial on how to do
this.](https://distributed.readthedocs.io/en/latest/setup.html).


# 6. Further Reading

The official Dask documentation is tremendously helpful when trying to figure
out how to translate Pandas calculations to distributed Dask calculations. It
is also always worth taking a look at Pandas tutorials when figuring out how to
efficiently vectorize calculations.

- [Dask documentation](https://Dask.pydata.org/en/latest/)
- [Dask.distributed
documentation](https://distributed.readthedocs.io/en/latest/index.html)
- [Pandas documentation](https://pandas.pydata.org/)
- [Dask scaling
benchmarks](http://matthewrocklin.com/blog/work/2017/07/03/scaling)

