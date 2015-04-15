#!/usr/bin/env python
import argparse, json, locale, subprocess, sys

# {NAME: [NUM_FOLLOWS, COUNT, WHO]}
friends = {}

for n in ("fylwind", "ninji", "syfaro"):
    with open(n + ".friends.json", "rt") as f:
        for friend in json.load(f):
            name = friend["screen_name"]
            if name in friends:
                friends[name][1] += 1
                friends[name][2] = "".join(sorted(friends[name][2] + n[0]))
            else:
                friends[name] = [friend["followers_count"], 1, n[0]]

friends = sorted(((num_follows, count, name, who)
                  for name, [num_follows, count, who] in friends.items()),
                 reverse=True)
for num_follows, _, name, who in friends:
    print("{0}\t{1}\t{2}".format(num_follows, who, name))
