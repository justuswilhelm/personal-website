
# Analyzing Consumer Complaints with Pandas and Python 3
I found a really neat data source online on unwanted robocalls that the FCC (Federal Communications Commission, a United States government agency) is tracking. Consumers complaints are being tracked in an openly available database.

Let's download it and see what it contains!


```python
%%bash
wget https://consumercomplaints.fcc.gov/hc/theme_assets/513073/200051444/Telemarketing_RoboCall_Weekly_Data.csv
```

    --2015-11-07 16:54:19--  https://consumercomplaints.fcc.gov/hc/theme_assets/513073/200051444/Telemarketing_RoboCall_Weekly_Data.csv
    Resolving consumercomplaints.fcc.gov... 192.161.148.10
    Connecting to consumercomplaints.fcc.gov|192.161.148.10|:443... connected.
    HTTP request sent, awaiting response... 200 OK
    Length: 1789741 (1.7M) [application/vnd.ms-excel]
    Saving to: 'Telemarketing_RoboCall_Weekly_Data.csv'
    
         0K .......... .......... .......... .......... ..........  2%  108K 16s
        50K .......... .......... .......... .......... ..........  5%  233K 11s
       100K .......... .......... .......... .......... ..........  8%  667K 8s
       150K .......... .......... .......... .......... .......... 11%  420K 7s
       200K .......... .......... .......... .......... .......... 14%  978K 6s
       250K .......... .......... .......... .......... .......... 17%  698K 5s
       300K .......... .......... .......... .......... .......... 20%  430K 4s
       350K .......... .......... .......... .......... .......... 22% 3.14M 4s
       400K .......... .......... .......... .......... .......... 25% 1.54M 3s
       450K .......... .......... .......... .......... .......... 28%  798K 3s
       500K .......... .......... .......... .......... .......... 31%  379K 3s
       550K .......... .......... .......... .......... .......... 34% 2.18M 3s
       600K .......... .......... .......... .......... .......... 37% 5.34M 2s
       650K .......... .......... .......... .......... .......... 40% 3.08M 2s
       700K .......... .......... .......... .......... .......... 42% 1.20M 2s
       750K .......... .......... .......... .......... .......... 45%  390K 2s
       800K .......... .......... .......... .......... .......... 48% 2.28M 2s
       850K .......... .......... .......... .......... .......... 51% 1.66M 2s
       900K .......... .......... .......... .......... .......... 54%  845K 1s
       950K .......... .......... .......... .......... .......... 57% 3.45M 1s
      1000K .......... .......... .......... .......... .......... 60% 2.61M 1s
      1050K .......... .......... .......... .......... .......... 62% 2.24M 1s
      1100K .......... .......... .......... .......... .......... 65%  174K 1s
      1150K .......... .......... .......... .......... .......... 68% 2.52M 1s
      1200K .......... .......... .......... .......... .......... 71% 47.1M 1s
      1250K .......... .......... .......... .......... .......... 74% 38.8M 1s
      1300K .......... .......... .......... .......... .......... 77% 68.4M 1s
      1350K .......... .......... .......... .......... .......... 80% 77.9M 1s
      1400K .......... .......... .......... .......... .......... 82% 55.9M 0s
      1450K .......... .......... .......... .......... .......... 85% 53.2M 0s
      1500K .......... .......... .......... .......... .......... 88% 79.7M 0s
      1550K .......... .......... .......... .......... .......... 91% 85.2M 0s
      1600K .......... .......... .......... .......... .......... 94%  200K 0s
      1650K .......... .......... .......... .......... .......... 97% 2.10M 0s
      1700K .......... .......... .......... .......... .......   100% 5.02M=2.3s
    
    2015-11-07 16:54:25 (750 KB/s) - 'Telemarketing_RoboCall_Weekly_Data.csv' saved [1789741/1789741]
    


## Reading the Data
Using the convenient `read_csv` method, we can automatically turn a `.csv` file into a pandas DataFrame.


```python
from pandas import read_csv
s = read_csv('Telemarketing_RoboCall_Weekly_Data.csv')
s
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Phone Issues</th>
      <th>Time of Issue</th>
      <th>Caller ID Number</th>
      <th>Advertiser Business Phone Number</th>
      <th>Type of Call or Message (Robocalls)</th>
      <th>Type of Call or Message (Telemarketing)</th>
      <th>State</th>
      <th>Date (Ticket Date of Issue)</th>
      <th>Date (Ticket Created)</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>-</td>
      <td>-</td>
      <td>-</td>
      <td>NaN</td>
      <td>NaN</td>
      <td>Pennsylvania</td>
      <td>NaN</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>8:20 AM</td>
      <td>610-990-4243</td>
      <td>610-990-4243</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>Pennsylvania</td>
      <td>7/1/2014</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>2:00 p.m.</td>
      <td>469-656-8497</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Pennsylvania</td>
      <td>9/20/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>3</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:46 AM</td>
      <td>717-657-3334</td>
      <td>-</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>Pennsylvania</td>
      <td>9/23/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>4</th>
      <td>Robocalls</td>
      <td>3:19 p.m.</td>
      <td>415-946-5707</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Pennsylvania</td>
      <td>9/28/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>5</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>12:00 PM</td>
      <td>215-624-8359</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>9/29/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>6</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:00 AM</td>
      <td>202-470-3314</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>7</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>10:21 AM</td>
      <td>567-263-0009</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>8</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>1:02 PM</td>
      <td>215-763-3788</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>9</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>12:58 p.m.</td>
      <td>717-245-2434</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>10</th>
      <td>Robocalls</td>
      <td>11:11 AM</td>
      <td>410-844-5606</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>11</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>1:30 PM</td>
      <td>-</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>12</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>10:35 AM</td>
      <td>610-265-9391</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>13</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>6:50 a.m.</td>
      <td>321-608-4860</td>
      <td>321-608-4860</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>14</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:59 AM</td>
      <td>710-935-3478</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>15</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>10:49 a.m.</td>
      <td>469-248-4184</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>16</th>
      <td>Robocalls</td>
      <td>10:51 a.m.</td>
      <td>617-947-7532</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>17</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>8:41 PM</td>
      <td>-</td>
      <td>847-234-9229</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>18</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:21 a.m.</td>
      <td>330-577-1035</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>19</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>4:24 PM</td>
      <td>559-395-0316</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Pennsylvania</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>20</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:30 AM</td>
      <td>251-653-0805</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Alabama</td>
      <td>9/25/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>21</th>
      <td>Robocalls</td>
      <td>-</td>
      <td>-</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>22</th>
      <td>Robocalls</td>
      <td>12:00 P.M.</td>
      <td>443-451-4525</td>
      <td>-</td>
      <td>Abandoned Call</td>
      <td>NaN</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>23</th>
      <td>Robocalls</td>
      <td>1:10 PM</td>
      <td>786-452-4865</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>24</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>2:00 PM</td>
      <td>713-714-5831</td>
      <td>713-714-5831</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>25</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>1:45 PM</td>
      <td>952-777-8073</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>26</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:25 AM</td>
      <td>876-254-1483</td>
      <td>876-254-1483</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>27</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>2:58 p.m.</td>
      <td>609-270-0113</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Alabama</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>28</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>2:15 PM</td>
      <td>240-345-1622</td>
      <td>240-345-1622</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Alaska</td>
      <td>10/1/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>29</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>9:43 AM</td>
      <td>-</td>
      <td>786-275-5986</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>Arkansas</td>
      <td>9/28/2015</td>
      <td>10/1/2015</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>16284</th>
      <td>Robocalls</td>
      <td>9:17 AM</td>
      <td>313-307-4199</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Michigan</td>
      <td>10/31/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16285</th>
      <td>Robocalls</td>
      <td>7:07 P.M.</td>
      <td>201-340-5267</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Mississippi</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16286</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>10:30 AM</td>
      <td>-</td>
      <td>609-750-7300</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>New Jersey</td>
      <td>10/3/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16287</th>
      <td>Robocalls</td>
      <td>1:19 PM</td>
      <td>-</td>
      <td>-</td>
      <td>Autodialed Live Voice Call</td>
      <td>NaN</td>
      <td>New Jersey</td>
      <td>10/28/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16288</th>
      <td>Robocalls</td>
      <td>2:17 P.M.</td>
      <td>-</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>New Jersey</td>
      <td>10/30/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16289</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>8:30 PM</td>
      <td>866-580-4780</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>New Jersey</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16290</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>11:27 a.m.</td>
      <td>844-468-2992</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>North Carolina</td>
      <td>10/31/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16291</th>
      <td>Robocalls</td>
      <td>3:00 PM</td>
      <td>-</td>
      <td>866-609-8298</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>North Carolina</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16292</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>10:12 PM</td>
      <td>646-315-7184</td>
      <td>-</td>
      <td>NaN</td>
      <td>Live Voice</td>
      <td>North Carolina</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16293</th>
      <td>Robocalls</td>
      <td>10:19 AM</td>
      <td>202-779-4536</td>
      <td>202-779-4536</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Nebraska</td>
      <td>10/30/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16294</th>
      <td>Robocalls</td>
      <td>1:31 p.m.</td>
      <td>509-872-5242</td>
      <td>509-872-5242</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Nebraska</td>
      <td>10/30/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16295</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>1:00 PM</td>
      <td>844-210-5817</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Maryland</td>
      <td>10/30/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16296</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>2:55 PM</td>
      <td>410-916-0690</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Maryland</td>
      <td>10/31/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16297</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>6:16 PM</td>
      <td>206-777-1088</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Maryland</td>
      <td>10/31/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16298</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>7:00 PM</td>
      <td>301-842-7252</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Maryland</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16299</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>6:02 p.m.</td>
      <td>410-454-0198</td>
      <td>410-454-0198</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>Maryland</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16300</th>
      <td>Robocalls</td>
      <td>5:13 PM</td>
      <td>-</td>
      <td>-</td>
      <td>Abandoned Call</td>
      <td>NaN</td>
      <td>New Hampshire</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16301</th>
      <td>Robocalls</td>
      <td>11:16 AM</td>
      <td>913-353-9186</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Kansas</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16302</th>
      <td>Robocalls</td>
      <td>4:47 P.M.</td>
      <td>415-358-4862</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Kansas</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16303</th>
      <td>Robocalls</td>
      <td>8:31 p.m.</td>
      <td>800-900-1381</td>
      <td>-</td>
      <td>Prerecorded Voice</td>
      <td>NaN</td>
      <td>Kansas</td>
      <td>11/1/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16304</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>6:00 AM</td>
      <td>516-962-1271</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Idaho</td>
      <td>10/28/2015</td>
      <td>11/1/2015</td>
    </tr>
    <tr>
      <th>16305</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>1:11 p.m.</td>
      <td>239-841-3783</td>
      <td>239-841-3783</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Florida</td>
      <td>10/30/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16306</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>5:39 p.m.</td>
      <td>818-927-0729</td>
      <td>-</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>California</td>
      <td>11/1/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16307</th>
      <td>Robocalls</td>
      <td>1:00 PM</td>
      <td>-</td>
      <td>415-358-4862</td>
      <td>Abandoned Call</td>
      <td>NaN</td>
      <td>Georgia</td>
      <td>11/1/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16308</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>3:40 p.m.</td>
      <td>414-369-6670</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Minnesota</td>
      <td>11/2/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16309</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>3:13 p.m.</td>
      <td>307-271-6262</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Minnesota</td>
      <td>11/2/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16310</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>3:32 p.m.</td>
      <td>551-888-0364</td>
      <td>-</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>Minnesota</td>
      <td>11/2/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16311</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>5:45 a.m.</td>
      <td>254-236-6545</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Minnesota</td>
      <td>11/2/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16312</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>3:54 PM</td>
      <td>313-918-6488</td>
      <td>313-918-6488</td>
      <td>NaN</td>
      <td>Abandoned Calls</td>
      <td>Michigan</td>
      <td>10/26/2015</td>
      <td>11/2/2015</td>
    </tr>
    <tr>
      <th>16313</th>
      <td>Telemarketing (including do not call and spoof...</td>
      <td>3:59 p.m.</td>
      <td>769-257-0523</td>
      <td>-</td>
      <td>NaN</td>
      <td>Prerecorded Voice</td>
      <td>Mississippi</td>
      <td>11/1/2015</td>
      <td>11/2/2015</td>
    </tr>
  </tbody>
</table>
<p>16314 rows × 9 columns</p>
</div>



Excellent! As we can see, the DataFrame contains a lot of useful information. There is the number of a caller, the type of call, the reason for reporting it, in which state of the US it happened and finally time and date.

## Extracting Information
What interests me the most, is in which hour of the day most robocalls are being placed at consumer's homes. First, let's create a Series from the 'Time of Issue' column.


```python
s['Time of Issue']
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Time of Issue</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>-</td>
    </tr>
    <tr>
      <th>1</th>
      <td>8:20 AM</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2:00 p.m.</td>
    </tr>
    <tr>
      <th>3</th>
      <td>11:46 AM</td>
    </tr>
    <tr>
      <th>4</th>
      <td>3:19 p.m.</td>
    </tr>
    <tr>
      <th>5</th>
      <td>12:00 PM</td>
    </tr>
    <tr>
      <th>6</th>
      <td>11:00 AM</td>
    </tr>
    <tr>
      <th>7</th>
      <td>10:21 AM</td>
    </tr>
    <tr>
      <th>8</th>
      <td>1:02 PM</td>
    </tr>
    <tr>
      <th>9</th>
      <td>12:58 p.m.</td>
    </tr>
    <tr>
      <th>10</th>
      <td>11:11 AM</td>
    </tr>
    <tr>
      <th>11</th>
      <td>1:30 PM</td>
    </tr>
    <tr>
      <th>12</th>
      <td>10:35 AM</td>
    </tr>
    <tr>
      <th>13</th>
      <td>6:50 a.m.</td>
    </tr>
    <tr>
      <th>14</th>
      <td>11:59 AM</td>
    </tr>
    <tr>
      <th>15</th>
      <td>10:49 a.m.</td>
    </tr>
    <tr>
      <th>16</th>
      <td>10:51 a.m.</td>
    </tr>
    <tr>
      <th>17</th>
      <td>8:41 PM</td>
    </tr>
    <tr>
      <th>18</th>
      <td>11:21 a.m.</td>
    </tr>
    <tr>
      <th>19</th>
      <td>4:24 PM</td>
    </tr>
    <tr>
      <th>20</th>
      <td>11:30 AM</td>
    </tr>
    <tr>
      <th>21</th>
      <td>-</td>
    </tr>
    <tr>
      <th>22</th>
      <td>12:00 P.M.</td>
    </tr>
    <tr>
      <th>23</th>
      <td>1:10 PM</td>
    </tr>
    <tr>
      <th>24</th>
      <td>2:00 PM</td>
    </tr>
    <tr>
      <th>25</th>
      <td>1:45 PM</td>
    </tr>
    <tr>
      <th>26</th>
      <td>11:25 AM</td>
    </tr>
    <tr>
      <th>27</th>
      <td>2:58 p.m.</td>
    </tr>
    <tr>
      <th>28</th>
      <td>2:15 PM</td>
    </tr>
    <tr>
      <th>29</th>
      <td>9:43 AM</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
    </tr>
    <tr>
      <th>16284</th>
      <td>9:17 AM</td>
    </tr>
    <tr>
      <th>16285</th>
      <td>7:07 P.M.</td>
    </tr>
    <tr>
      <th>16286</th>
      <td>10:30 AM</td>
    </tr>
    <tr>
      <th>16287</th>
      <td>1:19 PM</td>
    </tr>
    <tr>
      <th>16288</th>
      <td>2:17 P.M.</td>
    </tr>
    <tr>
      <th>16289</th>
      <td>8:30 PM</td>
    </tr>
    <tr>
      <th>16290</th>
      <td>11:27 a.m.</td>
    </tr>
    <tr>
      <th>16291</th>
      <td>3:00 PM</td>
    </tr>
    <tr>
      <th>16292</th>
      <td>10:12 PM</td>
    </tr>
    <tr>
      <th>16293</th>
      <td>10:19 AM</td>
    </tr>
    <tr>
      <th>16294</th>
      <td>1:31 p.m.</td>
    </tr>
    <tr>
      <th>16295</th>
      <td>1:00 PM</td>
    </tr>
    <tr>
      <th>16296</th>
      <td>2:55 PM</td>
    </tr>
    <tr>
      <th>16297</th>
      <td>6:16 PM</td>
    </tr>
    <tr>
      <th>16298</th>
      <td>7:00 PM</td>
    </tr>
    <tr>
      <th>16299</th>
      <td>6:02 p.m.</td>
    </tr>
    <tr>
      <th>16300</th>
      <td>5:13 PM</td>
    </tr>
    <tr>
      <th>16301</th>
      <td>11:16 AM</td>
    </tr>
    <tr>
      <th>16302</th>
      <td>4:47 P.M.</td>
    </tr>
    <tr>
      <th>16303</th>
      <td>8:31 p.m.</td>
    </tr>
    <tr>
      <th>16304</th>
      <td>6:00 AM</td>
    </tr>
    <tr>
      <th>16305</th>
      <td>1:11 p.m.</td>
    </tr>
    <tr>
      <th>16306</th>
      <td>5:39 p.m.</td>
    </tr>
    <tr>
      <th>16307</th>
      <td>1:00 PM</td>
    </tr>
    <tr>
      <th>16308</th>
      <td>3:40 p.m.</td>
    </tr>
    <tr>
      <th>16309</th>
      <td>3:13 p.m.</td>
    </tr>
    <tr>
      <th>16310</th>
      <td>3:32 p.m.</td>
    </tr>
    <tr>
      <th>16311</th>
      <td>5:45 a.m.</td>
    </tr>
    <tr>
      <th>16312</th>
      <td>3:54 PM</td>
    </tr>
    <tr>
      <th>16313</th>
      <td>3:59 p.m.</td>
    </tr>
  </tbody>
</table>
<p>16314 rows × 1 columns</p>
</div>



The column is not well formatted, as 'AM' and 'PM' appear in different spellings. In order to extract useful information, we need to parse the time information first.


```python
from datetime import datetime, time
def parse_time(raw_time):
    if raw_time == '-':
        return None
    # This is why we can't have nice things
    raw_time = raw_time.upper().replace(".", "").replace(",", "").replace(
        ":", "").replace(">", "").replace("?", "").replace("  ", " ").replace("MM", "M").zfill(7)
    if raw_time[:2] == "00":
        raw_time = raw_time[1:]
    # Need to use datetime, time has no "strptime"
    dt =  datetime.strptime(
        raw_time,
        "%I%M %p",
    )
    return dt.hour # All this sweat and labor for an hour! Oh heavens!
actual_times = s['Time of Issue'].apply(parse_time)
```

Wasn't that fun? Dealing with messy data is a challenge, but can be mastered through continuous application of brute-force and being in denial about reality. A steady supply of coffee helps as well.

Now that we have the actual times, I would like to retrieve the most frequent hour. Let's do this!


```python
%matplotlib inline
actual_times.hist(bins=24)
```




    <matplotlib.axes._subplots.AxesSubplot at 0x114d9e860>




![png](output_9_1.png)


Aha! Most robo calls get placed around 10-11 AM!


```python

```
