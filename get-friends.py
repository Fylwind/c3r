#!/usr/bin/env python
import argparse, json, locale, subprocess, sys

opts = argparse.ArgumentParser(description="Downloads your Twitter friends.")
opts.add_argument(
    "NAME",
    nargs=1,
    help="Twitter handle",
)
args = opts.parse_args()
name = args.NAME[0]

encoding = locale.getpreferredencoding()
count = 200
cursor = "-1"
users = []
while cursor != "0":
    result = json.loads(subprocess.check_output((
        "twurl",
        "/1.1/friends/list.json?screen_name={0}&cursor={1}&count={2}"
        .format(name, cursor, count)
    )).decode(encoding))
    cursor = result["next_cursor_str"]
    users.extend(result["users"])

json.dump(users, sys.stdout, sort_keys=True, indent=4, separators=(",", ": "))
