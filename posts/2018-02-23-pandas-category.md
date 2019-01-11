---
toc: True
title: Memory Usage of Pandas Categoricals
---

A great method to improve memory usage of Pandas DataFrames is by converting
columns with [categorical
variables](https://en.wikipedia.org/wiki/Categorical_variable) to use the data
type `categorical`.<!--more--> The Pandas documentation explains further how to
use this data type in your columns
[here](https://pandas.pydata.org/pandas-docs/stable/categorical.html). In this
article we want to explore what memory savings we can typically expect when
working with them.

# 1. Setup

First, we import Pandas, NumPy and Matplotlib. Furthermore, we adjust the
maximum amount of rows shown when displaying Pandas DataFrames in Jupyter
notebooks and the default figure size for Matplotlib.


```python
%matplotlib inline
import string

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

pd.set_option('display.max_rows', 6)

plt.rcParams["figure.figsize"] = 10, 5
```

# 2. Test Data

We will then create some test data. Since we are concerned with efficiently
storing categorical variables, we will create random strings that we then put
into a Pandas Series.

First, we define the function that creates `n` random words. Here, we simply
choose random letters out of `string.ascii_letters`, which can be found in the
Python 3 standard library. We also seed `numpy.random` with `1` just to get
consistent results.


```python
def random_words(n, word_length=8):
    np.random.seed(1)
    random_chars = np.random.choice(
        list(string.ascii_letters),
        (n, word_length),
    )
    return np.array([
        "".join(word)
        for i, word in enumerate(random_chars)
    ])
```

Let's take a look at two example random words.


```python
random_words(2)
```

__Output:__

```
array(['LRmijlfp', 'aqbmhTgz'], dtype='<U8')
```

We then move on to create the function that gives us a random Pandas Series of
words. For this, we invoke the `random_words` function we created before and
choose however many different words we need to create a Series of length
`length` with `n_categories` different categories.

We also allow specifying a third argument called `categorical`, which will us
to decide whether we want the data to be stored as plain Python objects or
memory efficient Pandas categoricals. This is important for later benchmarking
as we will see.


```python
def random_words_series(length, n_categories, categorical=True):
    np.random.seed(1)
    categories = random_words(n_categories)
    words = np.random.choice(
        categories,
        length,
    )
    return pd.Series(
        words,
        dtype='category' if categorical else object,
    )
```

As an example, this is how it would look like to create a series of random
words out of 3 different categories with total series length 6.


```python
example = random_words_series(
    6, 3
)
example.to_frame()
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
   </th>
   <th>
    0
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    aqbmhTgz
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    YuLsulQC
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    YuLsulQC
   </td>
  </tr>
  <tr>
   <th>
    3
   </th>
   <td>
    LRmijlfp
   </td>
  </tr>
  <tr>
   <th>
    4
   </th>
   <td>
    aqbmhTgz
   </td>
  </tr>
  <tr>
   <th>
    5
   </th>
   <td>
    aqbmhTgz
   </td>
  </tr>
 </tbody>
</table>

We can then easily see that this Series has exactly 3 categories by inspecting
its `.dtype` attribute.


```python
len(example.dtype.categories)
```

__Output:__

```
3
```

Now, to compare a few examples, we create Series with 3 different amounts of
categories: 1, 10 and 100. Each Series will have a total length of 10000
values. Furthermore, for comparison we create each Series twice: once with the
data type set to categorical and once to just using Python objects. We get 6
series in total.

The 3 series ending on `_category` contain data stored as category data in
Pandas. The 3 series ending on `_object` contain data stored as Python objects.


```python
n = 10000

series = {
    'one_category': random_words_series(
        n, 1,
    ),
    'several_categories': random_words_series(
        n, 10,
    ),
    'many_categories': random_words_series(
        n, 100,
    ),
    'one_object': random_words_series(
        n, 1, False,
    ),
    'several_objects': random_words_series(
        n, 10, False,
    ),
    'many_objects': random_words_series(
        n, 100, False,
    ),
}
```

After we have created the 6 different series using our `random_words_series`
method, we want to move on to analyzing memory usage. This will allow us to
find out how big the memory savings are.

# 3. Memory

For each example series, we analyze the memory usage by using the standard
Pandas `.memory_usage()` method. We print it out immediately.


```python
memory_usage = pd.Series(
    {k: v.memory_usage(deep=True) for k, v in series.items()},
    name='memory_usage',
    dtype='uint64',
).sort_values()
memory_usage.to_frame()
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
   </th>
   <th>
    memory_usage
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    one_category
   </th>
   <td>
    10225
   </td>
  </tr>
  <tr>
   <th>
    several_categories
   </th>
   <td>
    11050
   </td>
  </tr>
  <tr>
   <th>
    many_categories
   </th>
   <td>
    21700
   </td>
  </tr>
  <tr>
   <th>
    many_objects
   </th>
   <td>
    650080
   </td>
  </tr>
  <tr>
   <th>
    one_object
   </th>
   <td>
    650080
   </td>
  </tr>
  <tr>
   <th>
    several_objects
   </th>
   <td>
    650080
   </td>
  </tr>
 </tbody>
</table>

We can see that category data in Pandas use considerably less memory than plain
Python objects.

Furthermore, we print out a logarithmic plot of memory size for each Series.


```python
fig, ax = plt.subplots()
ax.set_ylabel('Bytes')
memory_usage.to_frame().plot(
    kind='bar',
    logy=True,
    ax=ax,
);
```

__Output:__

<img alt="matplotlib.figure.Figure at 0x112f24908" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAYgAAAFQCAYAAAClLulkAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEgAACxIB0t1+/AAAADl0RVh0U29mdHdhcmUAbWF0cGxvdGxpYiB2ZXJzaW9uIDIuMS4xLCBodHRwOi8vbWF0cGxvdGxpYi5vcmcvAOZPmwAAIABJREFUeJzt3Xu8VXWd//HXGyQJQrwxXUQFk4sXUBMQsl9iTuYlDCwytNG8O6lgqWNe+mU1OmlOTWCNYgpFmaHoz2uT4mSZiQiKeMN0jBKdEhFBMRXk8/tjrQ3b42KfA2fvs/Za5/18PPbDs9c++5zP9hzOe6/v+n4/X0UEZmZmLXXJuwAzM2tODggzM8vkgDAzs0wOCDMzy+SAMDOzTA4IMzPL5IAwM7NMDggzM8vkgDAzs0yb5V3AppA0BhjTq1evEwcOHJh3OWZmhTJ//vyXIqJPa5+nIrfaGDZsWMybNy/vMszMCkXS/IgY1trneYjJzMwyOSDMzCyTA8LMzDIV8iJ1LatXr2bJkiW88cYbeZdim6B79+707duXbt265V2KWadXuoBYsmQJvXr1ol+/fkjKuxzbCBHBsmXLWLJkCf3798+7HLNOr5BDTJLGSJq6YsWKdz32xhtvsM022zgcCkgS22yzjc/+zJpEIQMiIm6NiJN69+6d+bjDobj8szNrHoUMCDMza7zSXYNoqd/Xbq/r11v8nUPr+vXMqtX797U1Hf377NdXX41+fT6DKLE1a9bkXYKZFZgDogEWL17M4MGD+dKXvsTAgQM56qijmD17Nvvuuy8DBgxg7ty5rFq1iuOOO44RI0aw1157cfPNNwMwffp0xo4dyyc/+Un69evH5Zdfzve+9z322msvRo4cycsvvwzAggULGDlyJEOHDmXcuHEsX74cgNGjR3PGGWcwbNgwLrroIvr378/q1asBWLly5TvutzR69GgqrUteeukl+vXrB8Djjz/OiBEj2HPPPRk6dChPP/00AGPHjmXvvfdmt912Y+rUqeu+ztVXX83AgQMZMWIEJ554IqeddhoAS5cu5bOf/SzDhw9n+PDh3HfffXX+P29m9eSAaJBnnnmGM888k0WLFrFo0SKuvfZafv/733PZZZdx8cUXc9FFF/GJT3yCuXPn8pvf/Iazzz6bVatWAfDYY49x44038uCDD3L++efTo0cPHn74YUaNGsVPf/pTAI4++mguueQSFi5cyJAhQ/jmN7+57nu/9dZbzJs3j2984xuMHj2a229PTnuvu+46Dj/88I1eY3DFFVcwadIkFixYwLx58+jbty8A11xzDfPnz2fevHlMnjyZZcuW8cILL/Dtb3+bOXPmcN9997Fo0aJ1X2fSpEl85Stf4cEHH2TWrFmccMIJ7fp/bGaNVchrEJVurjvvvHPepWxQ//79GTJkCAC77bYbBxxwAJIYMmQIixcvZsmSJdxyyy1cdtllQDI99y9/+QsA+++/P7169aJXr1707t2bMWPGADBkyBAWLlzIihUreOWVV9hvv/0AOOaYYxg/fvy6733EEUes+/iEE07g0ksvZezYsUybNo2rrrpqo1/LqFGjuOiii1iyZAmHH344AwYMAGDy5MncdNNNADz33HM8/fTT/PWvf2W//fZj6623BmD8+PH88Y9/BGD27Nk88cQT677uypUree2113jf+9630TWZWeMVMiAi4lbg1mHDhp2Ydy0bsvnmm6/7uEuXLuvud+nShTVr1tC1a1dmzZrFoEGD3vG8Bx54oNXntqZnz57rPt53331ZvHgx99xzD2+//Ta77777Bp+32WabsXbtWoB3rEU48sgj2Weffbj99ts55JBDuPLKK+nSpQuzZ8/m/vvvp0ePHowePbrV9Qtr165lzpw5dO/evdXXYGb58xBTTj71qU8xZcoUKu3WH3744TY/t3fv3my11Vbce++9AMyYMWPd2USWo48+miOPPJJjjz225tft168f8+fPB+CGG25Yd/zZZ59lp512YuLEiXzmM59Zdxaz1VZb0aNHDxYtWsScOXMAGD58OL/97W9Zvnw5a9asYdasWeu+zoEHHsiUKVPW3V+wYEGbX7OZdbxCnkFsjGadlvr1r3+dM844g6FDh7J27Vr69+/Pbbfd1ubn/+QnP+GUU07h9ddfZ6eddmLatGkb/NyjjjqKCy64gAkTJtT8mmeddRaf//znmTp1Koceuv7/28yZM5kxYwbdunXjAx/4AOeddx49e/bkiiuuYJdddmHQoEGMHDkSgO22247zzjuPESNGsPXWWzN48GAqCxonT57MqaeeytChQ1mzZg0f//jHueKKK9r8ms2sY5Vuw6Ann3ySXXbZJaeKmtMNN9zAzTffzIwZMzrk+1WuK6xZs4Zx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"/>

Finally, we will try to see how memory usage behaves when the amount of
different categories inside a Series grows.

# 4. Memory usage as function of size

For this analysis, we will create 2000 series with up to 2000 different
categories. As before, in order to allow us to understand the memory usage of
category data in Pandas, we create the series once using the Pandas category
data type and once just using plain Python objects.

First, we calculate the memory usage for the plain Python object series:


```python
size_n = 2000
size = pd.Series(
    {
        i: random_words_series(
            size_n, i, False,
        ).memory_usage(deep=True)
        for i in range(1, size_n + 1)
    },
    name="Memory usage for n categories (object data type)",
).sort_values()
```

Right after that, we calculate the memory usage for all of the categorical
series:


```python
size_categorical = pd.Series(
    {
        i: random_words_series(
            size_n, i,
        ).memory_usage(deep=True)
        for i in range(1, size_n + 1)
    },
    name="Memory usage for n categories (categorical data type)",
).sort_values()
```

Now, we can put the sizes next to each other in a Pandas DataFrame. We will
immediately see that despite the number of categories nearing 2000, the memory
usage never comes close to that of the Series using plain Python objects.


```python
sizes = pd.DataFrame([size, size_categorical]).T
sizes
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
   </th>
   <th>
    Memory usage for n categories (object data type)
   </th>
   <th>
    Memory usage for n categories (categorical data type)
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    1
   </th>
   <td>
    130080
   </td>
   <td>
    2225
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    130080
   </td>
   <td>
    2290
   </td>
  </tr>
  <tr>
   <th>
    3
   </th>
   <td>
    130080
   </td>
   <td>
    2355
   </td>
  </tr>
  <tr>
   <th>
    ...
   </th>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    1998
   </th>
   <td>
    130080
   </td>
   <td>
    127460
   </td>
  </tr>
  <tr>
   <th>
    1999
   </th>
   <td>
    130080
   </td>
   <td>
    127395
   </td>
  </tr>
  <tr>
   <th>
    2000
   </th>
   <td>
    130080
   </td>
   <td>
    127460
   </td>
  </tr>
 </tbody>
</table>

We can plot these values against each other. Since the Python object Series
always contain the same amount of data regardless of the number of categories
we see that the memory usage is constant as well. For the category data type
series we clearly see an increase in memory usage, but it never touches the
constant line of the Python object series in the plot.


```python
fig, ax = plt.subplots(1)
ax.set_xlabel('Random Words')
ax.set_ylabel('Bytes')
sizes.plot(
    ax=ax
);
```

__Output:__

<img alt="matplotlib.figure.Figure at 0x113660550" src="data:image/png;base64,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zHfffed9j8+0r8uWLWPEiBFUq1YNgPPPPz/XY5Xp8OHDHD58mA4dOgDO+/n5587l3LS0tAId94Iu56tjx4488MAD7N+/n/nz5zNgwAAqVHC+ert27UrdunUBuPnmm1m5ciUVKlRg7dq13s/GyZMnueCCC7zru+CCC/j999+9rystLNkEyIQbm59emJ4CSb86yaZaPbflWe5fXkXVt29fHn30UeLi4khKSvKWqyrz58/nqquuyrb8t99+S+XKlb3T5cqV806XK1euQNcpqlfPauDQrl07EhISiIuLIyMjI9cLsxUqVMDjnkr0eDykpqYC0KFDB7788ks+++wzhg8fziOPPEL79u15+eWXWbNmDXXq1GH48OH5NsvMa1/Plu/+5eR77MqXL1/g6zqTJ0+mQYMGrF+/Ho/HQ5Uquf/4UFVef/11unfvnq180aJFBdpOTrnti6rSvHlzVq1addq8zz77jC+//JL//Oc/PPfcc/z444/eL1CAqlWrFriZbM5WTjmn89rXxYsXF2j9BVHQ417Q5XK68847+eCDD5g9ezaxsd7Ly7nuu6oybNgw/v73v+e6rpSUFKpWrVrAPSs57JpNMJ046CSauldA7cZ+SzQAI0eOZMKECYSFhWUr7969O6+//jrqtqH44YcfCrzOWrVqUadOHe81lPfff99by8nNnXfeyR133MGIESNynR8SEsLatWsBWLhwofeazs6dO2nQoAH33HMPd999N99//z1HjhyhevXq1KpVi71793p/oV511VVs376dhIQEAObMmVOkfb322muZPXs2AB9++CHt27fP9zUFcdVVV7Fnzx7WrFkDONc40tPTSU5OpmHDhpQrV47333/fe66+Ro0aHD16NNu+vP32295jtHXrVo4fP067du2YP38+Ho+HvXv3EhcX591eQkIC27ZtA/J/rzJfs3//fm+ySUtLY+PGjXg8Hnbt2kXnzp158cUXSU5O5tixY9lee/XVV3u3BdC+fXs+/PBDAOLi4qhXr563NrdgwQJSUlJISkoiLi7O+4s+v33t2rUrsbGxnDhxAsB7HTHnscpUu3ZtateuzcqVKwG88QAFPu55Lecrt+0PHz6cV199FXBqupmWLl3KwYMHOXnyJJ988gnt2rWjS5cuzJs3j3379nn3a+fOnYCTeP/44w9CQkJO225JZzWbYFIPSHmoXMPv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"/>

At this point we should ask ourselves whether string data should always be
stored as categorical data in Pandas. While one would assume that the added
indirection should provide no advantage when using non-categorical values, the
memory usage analysis here implies otherwise. We need to further inspect
whether the method to determine memory usage is sound, i.e. whether Pandas
`.memory_usage(deep=True)` correctly takes in account all nested data. On the
other hand, with a low number of categories the memory savings are quite
significant. Here it always makes sense to switch the data type to categorical.

To sum it up: If category data is encountered, the Pandas category data type
should always be used.

