from csv import DictReader
with open('rows.csv') as fd:
    reader = DictReader(fd)
    rows = tuple(filter(
        lambda s: s['Assignment Category'] != 'Parttime-Regular',
        reader,
    ))

print(rows[0].keys())

from statistics import mean, median
from operator import itemgetter

salaries = tuple(map(itemgetter('Current Annual Salary'), rows))
print(salaries[:10])

salaries = tuple(map(lambda s: float(s[1:]), salaries))
print(salaries[:10])

print("Mean: {}, Median: {}".format(mean(salaries), median(salaries)))

from itertools import filterfalse
is_female = lambda s: s['Gender'] == 'F'
female_employees = tuple(filter(is_female, rows))
male_employees = tuple(filterfalse(is_female, rows))

print("Number of female employees: {}".format(len(female_employees)))
print("Number of male employees: {}".format(len(male_employees)))


def get_salaries(rows):
    return map(
        lambda s: float(s[1:]),
        map(itemgetter('Current Annual Salary'), rows)
    )


print("Mean salaries female: {:.2f}, male: {:.2f}".format(
    mean(get_salaries(female_employees)),
    mean(get_salaries(male_employees)),
))

print("Median salaries female: {:.2f}, male: {:.2f}".format(
    median(get_salaries(female_employees)),
    median(get_salaries(male_employees)),
))

print("Mean salaries ratio: {:.2f}".format(
    mean(get_salaries(female_employees)) /
    mean(get_salaries(male_employees))
))

from statistics import pstdev

print("Standard deviation all employees: {:.2f}".format(
    pstdev(get_salaries(rows)),
))
print("Population standard deviation female: {:.2f}, male: {:.2f}".format(
    pstdev(get_salaries(female_employees)),
    pstdev(get_salaries(male_employees)),
))

from statistics import mode
position_title = itemgetter('Position Title')
print("Most common female employee position title: {}".format(
    mode(map(position_title, female_employees))
))
print("Most common male employee position title: {}".format(
    mode(map(position_title, male_employees))
))

from collections import Counter
female_position_titles = tuple(map(position_title, female_employees))
male_position_titles = tuple(map(position_title, male_employees))

from pprint import pprint
pprint(Counter(female_position_titles).most_common(10))
pprint(Counter(male_position_titles).most_common(10))


def probability_distribution(content):
    def _helper():
        absolute_distribution = Counter(content)
        length = len(content)
        for value, frequency in absolute_distribution.items():
            yield value, float(frequency) / length * 100
    return Counter(dict(_helper()))

female_distribution = probability_distribution(
    female_position_titles)
male_distribution = probability_distribution(
    male_position_titles)

pprint(female_distribution.most_common(10))
pprint(male_distribution.most_common(10))

print("""The 10 most common position titles for female employees cover {:.2f}\
 of all employees""".format(
    sum(map(itemgetter(1), female_distribution.most_common(10)))))
print("""The 10 most common position titles for male employees cover {:.2f}\
 of all employees""".format(
    sum(map(itemgetter(1), male_distribution.most_common(10)))))
