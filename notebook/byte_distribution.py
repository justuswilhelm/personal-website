with open("/bin/pwd", "rb") as fd:
    contents = fd.read()

from collections import Counter
c = Counter(contents)

print("Absolute Distribution")
for value, frequency in c.most_common(10):
    print("0x{:02x}: {}".format(value, frequency))


def probability_distribution(content):
    def _helper():
        absolute_distribution = Counter(content)
        length = len(content)
        for value, frequency in absolute_distribution.items():
            yield int(value), float(frequency) / length
    return Counter(dict(_helper()))

c = probability_distribution(contents)

print("Probability Distribution")
for value, frequency in c.most_common(10):
    print("0x{:02x}: {:.04f}".format(value, frequency))


c = probability_distribution(list(filter(bool, contents)))

max_prob = c.most_common(1)[0][1]


for value, frequency in c.most_common():
    print("0x{:02x}: {}".format(value, "█" * int(frequency * 80/max_prob)))
