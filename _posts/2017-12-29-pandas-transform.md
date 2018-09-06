---
title: transform()ing Like a Pro in Pandas
excerpt: Using `.transform()`, you can transform the way you aggregate DataFrames.
toc: true
---

# 1. Introduction

The Pandas API surprises me with a new feature or method almost every day, and I have yet again discovered an interesting piece of functionality. It turns out to be quite useful in some situations. 

Using `.transform()`, you can, excuse the pun, transform the way you aggregate DataFrames. While not applicable to every use case in the [split-apply-combine workflow](https://pandas.pydata.org/pandas-docs/stable/groupby.html), it makes one common task quite easy: Joining back aggregate values to the original DataFrame.

First, we shall create a sample DataFrame in order to then explore the various ways we can utilize `.transform()` in our Data Analysis work flow.

We import Pandas for DataFrame creation, `random` for generation of random data in our DataFrame, and `datetime.datetime` and `datetime.timedelta` for purchase date creation. We furthermore import Matplotlib to inspect the data.

```python
import pandas as pd
import random
from datetime import datetime, timedelta

import matplotlib.pyplot as plt
```

Just while creating this notebook I was struggling with one thing in particular while playing around with DataFrame creation. DataFrames printed inside IPython are sometimes just too long and have too many rows. Just typing `df` and having a perfectly formatted HTML table of the head and tail of the DataFrame you want to inspect come out is just too convenient.

So naturally, I wondered whether there was any way to shorten the number of rows printed when evaluating a DataFrame. `pandas.set_option` to the rescue. The [documentation](https://pandas.pydata.org/pandas-docs/stable/generated/pandas.set_option.html) has a full list of all options available, and one option that we find interesting in particular is `max_rows`. It determines the maximum amount of rows pandas will output when outputting truncated DataFrames. A DataFrame is truncated depending on how much space is available for printing it. In the case of the terminal, this is pretty easy to find out. Pandas just needs to see what the terminal's character dimensions are. In the case of IPython and Jupyter, this is not so easy. There is no API available for determining the browser window dimensions inside Pandas. So we're stuck with manually setting a value.

We choose 6 as the maximum number of rows, including the head _and_ tail of a DataFrame.

```python
pd.set_option('display.max_rows', 6)
```

Today's DataFrame, readily filled with fake data, consists of the following data.

Since we want to keep it entertaining, the data we look at today is e-commerce purchase data. We look at a table containing purchase items, and data that is associated with them. The purchase items are linked to a specific order ID. One purchase can have several purchase items. To sum it up, we create the following columns.

- `date`: Date of purchase
- `category`: Category of item, either __Food__, __Beverage__ or __Magazine__
- `value`: Purchase value of item, ranges from 10 to 100
- `customer`: ID of customer that purchased this item, a random ID from 1 to 10
- `purchase`: ID of purchase, there are 30 purchases with IDs ranging from 1 to 30

```python
random.seed(1)
category_names = [
    "Food",
    "Beverage",
    "Magazine",
]
start_date = datetime(2017, 1, 1)
rows = 100
df = pd.DataFrame(
    [
        [
            start_date + timedelta(
                days=random.randint(0, 30)
            ),
            random.choice(category_names),
            random.randint(10, 100),
            random.randint(1, 10),
            random.randint(1, 30),
        ]
        for _ in range(rows)
    ],
    columns=[
        "date",
        "category",
        "value",
        "customer",
        "purchase",
    ],
)
df.index.name = "item_id"
df
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    date
   </th>
   <th>
    category
   </th>
   <th>
    value
   </th>
   <th>
    customer
   </th>
   <th>
    purchase
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    2017-01-05
   </td>
   <td>
    Magazine
   </td>
   <td>
    18
   </td>
   <td>
    5
   </td>
   <td>
    4
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    2017-01-16
   </td>
   <td>
    Beverage
   </td>
   <td>
    70
   </td>
   <td>
    7
   </td>
   <td>
    26
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    2017-01-07
   </td>
   <td>
    Food
   </td>
   <td>
    72
   </td>
   <td>
    1
   </td>
   <td>
    29
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
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    2017-01-30
   </td>
   <td>
    Beverage
   </td>
   <td>
    41
   </td>
   <td>
    6
   </td>
   <td>
    4
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    2017-01-18
   </td>
   <td>
    Magazine
   </td>
   <td>
    84
   </td>
   <td>
    10
   </td>
   <td>
    3
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    2017-01-08
   </td>
   <td>
    Food
   </td>
   <td>
    12
   </td>
   <td>
    4
   </td>
   <td>
    13
   </td>
  </tr>
 </tbody>
</table>

That should give us some useful purchase data to work with.

# 2. The Problem

The other day I was working on an interesting problem that I could only solve in a cumbersome way with Pandas before. For each purchase in a similar DataFrame, I wanted to calculate a purchase item's overall contribution to the total purchase value. So for example, if a customer bought two USD 10 items for a total value of USD 20, one purchase item would contribute 50 % to the total purchase value.

In order to find the total purchase value, we would typically use a group by together with a `.sum()`. Since we need to calculate the ratio of a purchase item's value to the total purchase value, we would need to join that data back to the original DataFrame. Or at least, this is how I would have done it before finding out about `.transform()`.

Let's take it step by step and look at the necessary calculation steps. First, we need to group by a purchase ID and sum up the total purchase value. Therefore, we need to group by `purchase` and sum up the `value` column:

```python
values = df.groupby('purchase').value.sum()
values.to_frame()
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    purchase
   </th>
   <th>
    value
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    2
   </th>
   <td>
    146
   </td>
  </tr>
  <tr>
   <th>
    3
   </th>
   <td>
    398
   </td>
  </tr>
  <tr>
   <th>
    4
   </th>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    ...
   </th>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    28
   </th>
   <td>
    89
   </td>
  </tr>
  <tr>
   <th>
    29
   </th>
   <td>
    329
   </td>
  </tr>
  <tr>
   <th>
    30
   </th>
   <td>
    63
   </td>
  </tr>
 </tbody>
</table>

What we get is a Pandas Series containing the total purchase values for every purchase ID. Since we used a group by on the purchase ID, `purchase` is the index of this Series. We can visualize this data with a bar plot.

```python
fig, ax = plt.subplots(1, figsize=(20, 5))
values.plot(kind='bar', ax=ax)
fig
```

__Output:__

<img alt="<matplotlib.figure.Figure at 0x1074f6710>" src="data:image/png;base64,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
K8lZa/T8qaqt3b1lrbY/hkXImCxGThnHswg5ZRzPIuSUcTyLkFPG8SxCThnHswg5ZRzPIuSUcTyL
kFPG8cwr51odVvaxJIdX1f2r6ruSPCXJeWv0XAAAAADsoTWZOdTdN1fVs5P8WZL9kry+u69Yi+cC
AAAAYM+t1WFl6e7zk5y/VttfgTU7ZG1Ei5AxWYycMo5nEXLKOJ5FyCnjeBYhp4zjWYScMo5nEXLK
OJ5FyCnjeBYhp4zjmUvONTkhNQAAAACLYa3OOQQAAADAAlAOAQAAAEyYcggAAABgwvaZcqiqjqiq
Y6vqHjuMHzevTDuqqqOr6uHD8oOq6jeq6nHzznVnqupN886wo6r68ar6vmH57lX1e1X1P6rqxVV1
wLzzJUlV/XpVHTbvHHemqr6rqp5WVY8Zbv9SVf2Xqjqlqvafd74lVfWDVfWbVfXyqnpZVT1z6d8f
AGBqquo+886wr6iqe887A6wX+0Q5VFW/nuTcJM9JcnlVnbDs7v8wn1S3V1UvTPKKJK+qqv+Y5L8k
+Z4kp1bV78413KCqztvh438k+YWl2/POt8zrk3xzWH55kgOSvHgYe8O8Qu3gD5JcXFUfrqpnVdXG
eQfaiTckOT7Jc6vqzUmemOTiJA9P8tp5Blsy/Gy/OsndMst11ySHJbmoqh49x2iwkOxQjMPOBPNU
VQdU1elVdWVVfaWq/q6qtg1jB84730pU1fvmnSFJquq+VfWqqjqzqu5dVadV1WVVdU5VHTLvfElS
Vffa4ePeST5aVfesqnvNO9+S5W/ID/9HX1dVl1bVW6vq4HlmWzL8jBw0LG+pqs9n9nr9i1X1z+cc
L0lSVZ+oqhdU1QPmneXODN+/C6vqv1XVYVX1gaq6qao+VlUPnXe+JKmqe1TV71fVFUO27VV1UVX9
yryzLamqDVX1jKp6//DzcmlVvW94M3yvv1m/T1ytrKouS/LI7v5GVW1O8s4kb+7ul1fVJ7t77v9B
h4xHZbZz+6Ukm7r761V19yQXd/dD5hows19GST6dWTHQSSrJ25I8JUm6+y/nl+42VbWtu48clj/R
3Q9bdt8l3X3U/NLdmuOTSf5JksckeXKSn0vy8cy+n+/u7r+fY7wkSVVd2t0PqaoNSa5L8v3dfUtV
VZJPrZP/k5clOWrI9d1Jzu/uR1fV/ZKcux5+tpPZi6Akz0/y+CT3yezn58bMSuvTu/trc4y3IlX1
vu5+7DrI8X2ZfS83JXlfd7912X2v7O5nzS3cbTnum+SFSb6d5N9n9sbELybZluS53X39HOPdaic7
DZXZ76GHZvb3/yt7P9XtVdVx3f3+YfmAJC/LrAi+PMm/7e4b5pkvme1MJPmj7v5yVW1Jck5m//b7
J3naOvrb+Ikk707ytu7+3Lzz7Mzw/ftPmf3NeX5mb/YcneRvkpzc3Z+cY7wks52JJL+d2c/0piT/
mORzSV7d3W+cY7RbVdWfJbkgydnd/aVh7L5JTkxybHf/9DzzLamqh93RXUne091zL1+q6v1J3pvZ
G7a/lOQtSd6a2d/zx3T3CXfy8L2iqr6d5Is7DG9Kcm2S7u4f3PupvtPy1+RV9drM9ndek+QXkvzz
7n78PPMls9eV3f3gYfnCJL/d3R+rqh9K8tbu3jLfhElVfSHJu5I8KbPv4duSvKO7//dcg+2gqj6a
2WuhA5O8JLO/2e+sqmOT/GF3P3KuAZNU1blJ/iTJX2T2/fyeJG9P8oIk13X378wxXpKkqt6W5GtJ
zs7sZzqZ/XyfmORe3f3kvZpnHymHrujuH1l2+x6ZFUSfTnLMeikLlnZkdyys1lGhcZckz03yuCS/
1d2XVNXn18sfnSVV9d8zKwneUFVvSHJmd28dfrG/pbsfPueIOyut9k/y2CRPzezFxtxnElXV5Uke
ltkvyr9N8gPd/ZWquluSTy4VcPM0lENbuvtbVXXPJB9Y+sNdVZd394/ON+GMF+rjqap3JflskouS
/GqS/5fkl4b/A7f7uZqXRdiZSBZjh8LOxHgWYYfCzsQ4quoz3f3Du3vf3lZVtyT5y8z+xuzoEd19
970c6Tvs8Pr8b7v7fsvuWy+vz5+X5Kcye21+2TD2he6+/3yT3d4Ov89v971bR9/LbUke3N03V9VF
3f2IZffd+rt+nnb4Pj4qs32HX8jsDai3dfdZ88y3ZBc/O+tlcsanuvvHlt3+WHc/fNjn/XR3HzHH
eEuZ/qa7f2h371srG/bmk62hG6rqqO6+JEmGGUQ/m9k7UnP/IR/8Y1V9d3d/M7MZJUlufaf02/OL
dZvu/naSM4by5YyquiHr8//Iv0ry8qp6QZIvJ/nrqromyTXDfevB7V4Idff/S3JekvOGGTDrweuS
XJlkvyS/m+S/12x67SMyeyG8Hrw2yceq6uIkj8rs8MHU7DC9uc96WGZzd794+cBQEr24qn51Tpl2
5mO54xfq6+VQhAd09y8Oy39as8NuL6iqn5tnqB0c3N1/nCRV9axl//Z/XFUnzTHXjn4rC7BDscyW
ZTsPZ1TViXNNc5sNVbWhu29Ocvfu/liSdPffVNVd55xtua92928m+c1lOxSfGHaG1ssOxf7d/b4k
qaoXd/c7k6S7P1hVfzTfaLfavGyG0MuGnYk/qKqnZ/am49zLoSRfrKrfzuwNiRuSpGaH7fxKZq+F
1ottSZ7R3Z/d8Y7hddt6sPwUGzueZ3O/vRnkjnT3S6vqHZn9Xrwms4J1Pb67f5+q+o3MXmN8X1VV
3zYLYb2cyuSVSc4fZoS+v6pentmMy2OSXDLXZDvR3R9O8uGqek5mf8+fnGQ9/C5Pkn+oqp/O7PQe
XVWP7+4/rdnhebfMOduS/1NV/7S7PzK8jvxKMtvnraqdvRaeh69U1ROTvGvYF1+asPHEJF/d22HW
447/nnhakpuXDwwv4p5WVf91PpG+wz/r7m8lt5YwS/bPbHbButHd1yZ5YlUdn+Tr886zo+6+Kcmv
1Ozwk/tn9v/42vVw+MEydzgFcCgI5667zxhebKS7/3fNTj7+mCSv6e6PzjfdTM8ODf2LJEcmeWl3
XzmMb0/yz+Ya7va8UB/PXavqLku/J7v7RVV1XZK/SnKPO3/oXrPudyaShdmhsDOxBtbxDoWdiXE8
OcmpSf5y+FvTSW7I7E2oJ80z2A5Oyx3/HD9nL+a4M+dW1T26+xvd/YKlwap6YJLPzDHX7Sx7bf5z
ST6QZL280bjca5J877B8dpKDkmwfZlKvi9+V3f3Hw6z0X0vyQ5ntQxye5E+T/OE8sy3zNzsOdPct
Sd4/fKwXz8xsBui3k/xMkl+rqjdmdtjwv55jruV+LclrqurwJFckOSm59U3mM+cZbJmnZPbm95lV
tXQaigOTXDjct1ftE4eVAczTcMjbqUlOyOycQ8ltL9RP7+693vzvTFU9Icll3f0dL3iXdtLmEGvH
HC9J8ufd/Rc7jB+X5I+7+/D5JLtdlt9P8pLu/sYO4w/M7N/7CfNJdseGHYrfyWxWxH3nnWdJzS7W
sNwru3tpZ+Il3f20eeTaUc1OgL98Z+KazHYmXj+8GTV3VfX27t7rLyR3R1X9WG7bmfi3mX1PT8yw
M9Hd/2uO8ZIkVfWQzGatLu1M/OowS2xjkqd29yvmGnBQVUdkdpjoRct/F9Wy83itB0POQzM7v+a6
zLloGTMrUh/Q3Zevp4zJ4n0vZVydqjoyyfdnHeccMh6adfy7sqp+PLOS/3NJjkjyyMwOezt/r2dR
DgGsnap6enevl6vo3aFFyCnj6tTsAghLOxTrNucSGcezCDll3K0cv57klMxmgx6V2Ynwzx3uWxfn
ZksWI+cwu+7ZWd8Z1/33MfF9AEeVAAAD4klEQVS9HMsifB+TW7+Xz8rsFBXrMueCZHxhZuel3ZDZ
rMCjk3wos1m/f9bdL9qreZRDAGundjhJ33q1CDllHM8i5JRxPIuQU8bdyrHur9KbLEZOGcezCDll
HM8i5FygjOvmiub7yjmHAOamqi69o7uSHLw3s9yZRcgp43gWIaeM41mEnDKO5i5Lh0d099XDYY/v
rKofyM4vODAvi5BTxvEsQk4Zx7MIORch483DOaW+WVWf6+6vJ0l3/9+aXXV2r1IOAazewZmdjG/H
cwtVkrmfQ2OZRcgp43gWIaeM41mEnDKOYxGu0pssRk4Zx7MIOWUczyLkXISM6+qK5sohgNV7T5J7
LP3xWa6qPrT349yhRcgp43gWIaeM41mEnDKOYxGu0pssRk4Zx7MIOWUczyLkXISM6+qK5s45BAAA
ADBhd5l3AAAAAADmRzkEAAAAMGHKIQCA3VBVV1fVQfPOAQAwFuUQAMAOqspFOwCAyVAOAQD7pKra
XFVXVtVbqmpbVb2zqr57+cyfqtqydBWqqjqtqt5cVf8zyZurar+q+qOquryqLq2q5yzb/HOq6hNV
dVlVHTE8/uiq+uuq+mRV/a+q+uFh/Eeq6qNVdcmwncOH8X+5bPy/VtV+e/UbBAAwUA4BAPuyH07y
yu4+MsnXkzxrF+s/KMljuvupSU5OsjnJUd39kCRvWbbel7v7YUleleQ3h7Erkzyqux+a5N8n+Q/D
+DOTvLy7j0qyJcm1VXVkkicn+Ylh/JYk/2JVXykAwB4yZRoA2Jdd093/c1j+b0l+fRfrn9fd/3dY
fkySV3f3zUnS3V9Ztt67h88fT/ILw/IBSc4eZgZ1kv2H8b9O8rtVtSnJu7v7s1V1bJJ/kuRjVZUk
d09y4558gQAAq6UcAgD2Zb2T2zfnttnTd9vh/v+zwu1+a/h8S257PfUHSS7s7p+vqs1JPpQk3f3W
qro4yfFJzq+qZySpJGd39/NX+HwAAGvGYWUAwL7sflX1yGH5l5J8JMnVmc3aSZJfvJPHfiDJM5ZO
Tl1V99rFcx2Q5Lph+VeWBqvqB5N8vrtfkeTcJA9J8sEkT6iq+yxtu6p+YIVfEwDAqJRDAMC+7DNJ
TqmqbUnumdk5gn4vycuramtmM3/uyGuT/G2SS6vqU5mVS3fmJUn+Y1V9Mrefnf2kJJdX1SVJfjTJ
m7r700lekOTPq+rSzIqoQ3b7qwMAGEF17zjbGgBg8Q2Hdr2nu390zlEAANY1M4cAAAAAJszMIQAA
AIAJM3MIAAAAYMKUQwAAAAATphwCAAAAmDDlEAAAAMCEKYcAAAAAJkw5BAAAADBh/x9DUw8YocE1
agAAAABJRU5ErkJggg=="/>

Now that we have the total purchase value for each purchase ID, we can join this data back to the purchase DataFrame. For this, we perform a left join of the purchase values to the DataFrame on `purchase`. We further specify that the value of the right side should receive the suffix `_total`. If we take a look again at our total purchase values, we can see that the name of the Series is the following.

```python
values.name
```

__Output:__

```
'value'
```

So if we join without specifying a suffix, Pandas will complain about a name collision. This is because both sides specify a column named `value`, which can obviously not be joined, since it is unclear which column should take precedence. Therefore, we need to have the right side column in the Series that we are joining change its name by using the suffix `_total`. Indicating that it is a total value in the name of course makes a lot of sense, since the column contains the total purchase values.

Let's perform the actual join then 🚀

```python
df.join(values, on='purchase', rsuffix='_total')
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    date
   </th>
   <th>
    category
   </th>
   <th>
    value
   </th>
   <th>
    customer
   </th>
   <th>
    purchase
   </th>
   <th>
    value_total
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    2017-01-05
   </td>
   <td>
    Magazine
   </td>
   <td>
    18
   </td>
   <td>
    5
   </td>
   <td>
    4
   </td>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    2017-01-16
   </td>
   <td>
    Beverage
   </td>
   <td>
    70
   </td>
   <td>
    7
   </td>
   <td>
    26
   </td>
   <td>
    213
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    2017-01-07
   </td>
   <td>
    Food
   </td>
   <td>
    72
   </td>
   <td>
    1
   </td>
   <td>
    29
   </td>
   <td>
    329
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
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    2017-01-30
   </td>
   <td>
    Beverage
   </td>
   <td>
    41
   </td>
   <td>
    6
   </td>
   <td>
    4
   </td>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    2017-01-18
   </td>
   <td>
    Magazine
   </td>
   <td>
    84
   </td>
   <td>
    10
   </td>
   <td>
    3
   </td>
   <td>
    398
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    2017-01-08
   </td>
   <td>
    Food
   </td>
   <td>
    12
   </td>
   <td>
    4
   </td>
   <td>
    13
   </td>
   <td>
    241
   </td>
  </tr>
 </tbody>
</table>

As we have discussed before, we want to calculate the ratio of a single purchase item to the total purchase value. We therefore need to calculate `value / value_total * 100` to retrieve the ratio as a percentage.

```python
df.assign(
    value_pct=(
        df.value /
        df.join(
            values,
            on='purchase',
            rsuffix='_total',
        ).value_total *
        100
    )
).round(2)
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    date
   </th>
   <th>
    category
   </th>
   <th>
    value
   </th>
   <th>
    customer
   </th>
   <th>
    purchase
   </th>
   <th>
    value_pct
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    2017-01-05
   </td>
   <td>
    Magazine
   </td>
   <td>
    18
   </td>
   <td>
    5
   </td>
   <td>
    4
   </td>
   <td>
    4.77
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    2017-01-16
   </td>
   <td>
    Beverage
   </td>
   <td>
    70
   </td>
   <td>
    7
   </td>
   <td>
    26
   </td>
   <td>
    32.86
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    2017-01-07
   </td>
   <td>
    Food
   </td>
   <td>
    72
   </td>
   <td>
    1
   </td>
   <td>
    29
   </td>
   <td>
    21.88
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
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    2017-01-30
   </td>
   <td>
    Beverage
   </td>
   <td>
    41
   </td>
   <td>
    6
   </td>
   <td>
    4
   </td>
   <td>
    10.88
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    2017-01-18
   </td>
   <td>
    Magazine
   </td>
   <td>
    84
   </td>
   <td>
    10
   </td>
   <td>
    3
   </td>
   <td>
    21.11
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    2017-01-08
   </td>
   <td>
    Food
   </td>
   <td>
    12
   </td>
   <td>
    4
   </td>
   <td>
    13
   </td>
   <td>
    4.98
   </td>
  </tr>
 </tbody>
</table>

To be honest, for a long time I thought that this was the only way to do it. At the same time I was more than concerned with how burdensome it is to join the values back to the original DataFrame.

Browsing through the Pandas documentation let me discover an extremely useful method, the `.transform()`. Time after time, aimlessly wandering through documentation has brought positive change into my humble life.

Using transform, we will simplify the above process and make the present an even more exciting time to be alive in.

# 3. Transform to the Rescue

While `.transform()`'s API is fairly well documented, I could only find a few hints to what the use cases might be [in the documentation](https://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.transform.html#pandas.DataFrame.transform).

Now I would like to present the perfect use case for `.transform()`. First, let's take a look at what the method exactly returns. We will define a method that prints the value it receives and returns the same value.

```python
def return_print(value):
    print(value)
    return value
```

We then use `.transform()` to apply this function on our DataFrame. To avoid the result from getting too long we only apply it to the category and value column. We will immediately see that the category column is evaluated twice, which seems strange. Read on to find out why.

```python
df[['category', 'value']].transform(return_print)
```

__Output:__

```
item_id
0     Magazine
1     Beverage
2         Food
        ...   
97    Beverage
98    Magazine
99        Food
Name: category, Length: 100, dtype: object
item_id
0     Magazine
1     Beverage
2         Food
        ...   
97    Beverage
98    Magazine
99        Food
Name: category, Length: 100, dtype: object
item_id
0     18
1     70
2     72
      ..
97    41
98    84
99    12
Name: value, Length: 100, dtype: int64
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    category
   </th>
   <th>
    value
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    Magazine
   </td>
   <td>
    18
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    Beverage
   </td>
   <td>
    70
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    Food
   </td>
   <td>
    72
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
    97
   </th>
   <td>
    Beverage
   </td>
   <td>
    41
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    Magazine
   </td>
   <td>
    84
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    Food
   </td>
   <td>
    12
   </td>
  </tr>
 </tbody>
</table>

It is interesting to see that Pandas executes `return_print` on the first column twice. Therefore, we also see it being printed twice. This is done for reasons of optimization: Pandas needs to find out which code path it can execute, as there is a fast and slow way of transforming ndarrays. Therefore, the first column is evaluated twice. As always, [the documentation](https://pandas.pydata.org/pandas-docs/stable/groupby.html#flexible-apply) describes this mechanism quite well (look for the __Warning__ section).

We can furthermore observe that the result is a completely unchanged DataFrame. This is reassuring and lets us understand the next example even better.

We would like to calculate the [standard score](https://en.wikipedia.org/wiki/Standard_score) for each value. The standard score of a value $$x$$ is defined as

$$z=\frac{x - \mu}{\sigma}$$

where

- $$\sigma$$ is the standard deviation of all values, and
- $$\mu$$ is the mean of all values.

We can quite simple express this as a `.transform()` call, by calculating

```python
x - x.mean() / x.std()
```

where `x` denotes a column we are calculating the standard scores for. Since not every column is numerical, we limit ourselves to the `value` column and calculate standard scores for all purchase item values.

```python
df.value.transform(
    lambda x: (x - x.mean()) / x.std()
).to_frame('value_standard_score')
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    value_standard_score
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    -1.354992
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    0.643862
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    0.720741
   </td>
  </tr>
  <tr>
   <th>
    ...
   </th>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    -0.470884
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    1.182015
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    -1.585630
   </td>
  </tr>
 </tbody>
</table>

An observant reader will quickly notice that we could also perform the following calculation to retrieve the same result:

```python
(
    (df.value - df.value.mean()) /
    df.value.std()
).to_frame('value_standard_score_alt')
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    value_standard_score_alt
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    -1.354992
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    0.643862
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    0.720741
   </td>
  </tr>
  <tr>
   <th>
    ...
   </th>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    -0.470884
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    1.182015
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    -1.585630
   </td>
  </tr>
 </tbody>
</table>

That is absolutely correct. I can see both forms having their advantages and disadvantages. I see the advantage of using `.transform()` instead of operating on raw columns as the following:

- Define reusable methods, for example for standard score calculation, that can be applied over any DataFrame. Code reuse for the win.
- Avoid retyping the column name that you operate on. This means typing `x` versus typing `df.value` all the time.
- `.transform()` can apply several transformations at once.

Let's take a look at an example for multiple transformations applied at the same time.

```python
def standard_score(series):
    """Return standard score of Series."""
    return (series - series.mean()) / series.std()

def larger_median(series):
    """Return True for values larger than median in Series."""
    return series > series.median()

df.transform({
    'value': [
        standard_score,
        larger_median,
    ],
    'date': lambda ts: ts.day
})
```

__Output:__

<table>
 <thead>
  <thead>
   <tr>
    <th>
    </th>
    <th colspan="2" halign="left">
     value
    </th>
    <th>
     date
    </th>
   </tr>
   <tr>
    <th>
    </th>
    <th>
     standard_score
    </th>
    <th>
     larger_median
    </th>
    <th>
     &lt;lambda&gt;
    </th>
   </tr>
   <tr>
    <th>
     item_id
    </th>
    <th>
    </th>
    <th>
    </th>
    <th>
    </th>
   </tr>
  </thead>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    -1.354992
   </td>
   <td>
    False
   </td>
   <td>
    5
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    0.643862
   </td>
   <td>
    True
   </td>
   <td>
    16
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    0.720741
   </td>
   <td>
    True
   </td>
   <td>
    7
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
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    -0.470884
   </td>
   <td>
    False
   </td>
   <td>
    30
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    1.182015
   </td>
   <td>
    True
   </td>
   <td>
    18
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    -1.585630
   </td>
   <td>
    False
   </td>
   <td>
    8
   </td>
  </tr>
 </tbody>
</table>

While we have observed in a [previous article](https://www.datakernel.co/2017/12/26/pandas-aggregates) that it is possible to assign names to function calls in `.aggregate()` to give result columns new names, it does not appear to be possible with transform.

So for example, we were able to do the following with a tuple:

```python
df.aggregate({
    'value': [
        ('mean', lambda x: x.sum() - x.count()),
    ],
})
```

We were then able to retrieve a column named `mean` that will contain the result of the `lambda`, but we cannot do the same using `.transform()` unfortunately. So the following is not possible

```python
df.value.transform({
    'value': [
        ('standard_score', lambda x: (x - x.mean()) / x.std()),
    ],
})
```

We would instead just be greeted by an irritated Exception telling us to reflect on our deeds.

Leaving that aside, I would like to show you how transform becomes really useful in the next section.

# 4. The Problem, Solved

It turns out that `.transform()` can also be used in group by objects. So what would we like to do? Easy. We would like to calculate an aggregate value, similar to how it has already been possible using `.aggregate()`, and then join it back to the index of the original grouped by object. So, if we are calculating a `sum` for every group, we then add the result back to each index that corresponds to that group.

In the case of grouping by purchases and calculating a sum, we would be then adding the sum back to every purchase item. Then, we can easily look at one purchase item and know both the purchase item's value, as well as the total purchase value.

Let's try it out then, shall we?

```python
ts = df.groupby('purchase').value.transform('sum')
ts.to_frame()
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    value
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    213
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    329
   </td>
  </tr>
  <tr>
   <th>
    ...
   </th>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    398
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    241
   </td>
  </tr>
 </tbody>
</table>

As we can see above, instead of directly calling `.transform()` on our well-known and beloved DataFrame, we first group by the purchase ID column. Since we have approximately 30 purchases, we will create approximately 30 groups.

Then, for each group we calculate the sum in the `.transform('sum')` call and directly join that value back to the index used before the `.groupby()` call.

We can now join that value back to our original DataFrame quite easily.

```python
df.join(ts, rsuffix='_total')
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    date
   </th>
   <th>
    category
   </th>
   <th>
    value
   </th>
   <th>
    customer
   </th>
   <th>
    purchase
   </th>
   <th>
    value_total
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    2017-01-05
   </td>
   <td>
    Magazine
   </td>
   <td>
    18
   </td>
   <td>
    5
   </td>
   <td>
    4
   </td>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    2017-01-16
   </td>
   <td>
    Beverage
   </td>
   <td>
    70
   </td>
   <td>
    7
   </td>
   <td>
    26
   </td>
   <td>
    213
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    2017-01-07
   </td>
   <td>
    Food
   </td>
   <td>
    72
   </td>
   <td>
    1
   </td>
   <td>
    29
   </td>
   <td>
    329
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
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    2017-01-30
   </td>
   <td>
    Beverage
   </td>
   <td>
    41
   </td>
   <td>
    6
   </td>
   <td>
    4
   </td>
   <td>
    377
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    2017-01-18
   </td>
   <td>
    Magazine
   </td>
   <td>
    84
   </td>
   <td>
    10
   </td>
   <td>
    3
   </td>
   <td>
    398
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    2017-01-08
   </td>
   <td>
    Food
   </td>
   <td>
    12
   </td>
   <td>
    4
   </td>
   <td>
    13
   </td>
   <td>
    241
   </td>
  </tr>
 </tbody>
</table>

Even more exciting, we can perform the same calculation that we have performed before, and achieve what required an additional, unpleasant `.join()` before.

```python
df.assign(
    value_pct=(
        df.value /
        ts *
        100
    )
).round(2)
```

__Output:__

<table>
 <thead>
  <tr>
   <th>
    item_id
   </th>
   <th>
    date
   </th>
   <th>
    category
   </th>
   <th>
    value
   </th>
   <th>
    customer
   </th>
   <th>
    purchase
   </th>
   <th>
    value_pct
   </th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <th>
    0
   </th>
   <td>
    2017-01-05
   </td>
   <td>
    Magazine
   </td>
   <td>
    18
   </td>
   <td>
    5
   </td>
   <td>
    4
   </td>
   <td>
    4.77
   </td>
  </tr>
  <tr>
   <th>
    1
   </th>
   <td>
    2017-01-16
   </td>
   <td>
    Beverage
   </td>
   <td>
    70
   </td>
   <td>
    7
   </td>
   <td>
    26
   </td>
   <td>
    32.86
   </td>
  </tr>
  <tr>
   <th>
    2
   </th>
   <td>
    2017-01-07
   </td>
   <td>
    Food
   </td>
   <td>
    72
   </td>
   <td>
    1
   </td>
   <td>
    29
   </td>
   <td>
    21.88
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
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
   <td>
    ...
   </td>
  </tr>
  <tr>
   <th>
    97
   </th>
   <td>
    2017-01-30
   </td>
   <td>
    Beverage
   </td>
   <td>
    41
   </td>
   <td>
    6
   </td>
   <td>
    4
   </td>
   <td>
    10.88
   </td>
  </tr>
  <tr>
   <th>
    98
   </th>
   <td>
    2017-01-18
   </td>
   <td>
    Magazine
   </td>
   <td>
    84
   </td>
   <td>
    10
   </td>
   <td>
    3
   </td>
   <td>
    21.11
   </td>
  </tr>
  <tr>
   <th>
    99
   </th>
   <td>
    2017-01-08
   </td>
   <td>
    Food
   </td>
   <td>
    12
   </td>
   <td>
    4
   </td>
   <td>
    13
   </td>
   <td>
    4.98
   </td>
  </tr>
 </tbody>
</table>

That wasn't so bad, was it? Let's see what we can take away from today's article

In the end, we were able to avoid one unpleasant `.join()` call and have made the intent behind our summation a lot clearer. And if you want to improve the comprehensibility of your IPython notebooks, clear intent, terseness and readability are king.

# 5. Summary

With `.transform()` in our toolbox, we've acquired yet another useful tool for Data Analysis. It is always a delight to dig in the Pandas documentation to find out what other delights this great library has to offer. I hope to be able to write even more articles about obscure niches in the Pandas API and surprise you with one or two things that neither of us knew about.

So I would like to encourage you to stay tuned and go out, dig up some new Pandas DataFrame methods, and enjoy working with data.
