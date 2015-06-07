#!/usr/bin/env python
import json, os, re, sys

users_dir = sys.argv[1]

def find_user(user_id):
    try:
        with open(os.path.join(users_dir, user_id)) as f:
            return user_id + " @{0}".format(json.load(f)["screen_name"])
    except Exception:
        return user_id

s = sys.stdin.read()
for line in s.strip().split("\n"):
    m = re.match("(.*(?:users/|[ +-]))([0-9]+)(.*)$", line)
    if m:
        l1, user_id, l2 = m.groups()
        print(l1 + find_user(user_id) + l2)
        continue
    print(line)
