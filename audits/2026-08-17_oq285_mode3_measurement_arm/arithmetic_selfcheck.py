#!/usr/bin/env python3
"""Mechanical re-check of every total asserted across this audit's documents.

Written because arithmetic and table dedup are the weakest link in the human
review loop (operator, 2026-08-17) — so the sums are checked by a machine
rather than by a reader who has already read the argument.

NOTE on its own fallibility: the first version of this script flagged the
phrase "next free OQ-295" in WRITEUP.md as a stale claim. The prose was
correct — it QUOTES the stale claim in order to mark it stale. A control that
fires is not automatically right; the tightened assertion below tests the
actual hazard (an UNMARKED assertion) rather than the substring.
"""
import csv, re, sys, collections

rows = list(csv.DictReader(open('seats_testsets.tsv'), delimiter='\t'))
ag = [r for r in rows if r['agent_seat'] == 'true']
fails = []


def chk(label, got, want):
    ok = got == want
    print(f"  {'OK ' if ok else '*** FAIL ***'} {label:62s} {got} {'==' if ok else '!='} {want}")
    if not ok:
        fails.append(label)


print("=== sums asserted in WRITEUP.md / FINDINGS_INCIDENTAL.md ===")
chk("group split 0+0+152+0 == total unknown tokens", 0 + 0 + 152 + 0,
    sum(1 for r in ag if r['type_token'] == 'unknown'))
chk("group (iii) by signature 103+40+9 == 152", 103 + 40 + 9, 152)
chk("A1 cells 152+29+0+1152 == agent seats", 152 + 29 + 0 + 1152, len(ag))
chk("2x2 has_unk 16+41+0+0+3 == 60", 16 + 41 + 0 + 0 + 3, 60)
chk("2x2 no_unk 111+40+15+4+0 == 170", 111 + 40 + 15 + 4 + 0, 170)
chk("2x2 tot 127+81+15+4+3 == constraints", 127 + 81 + 15 + 4 + 3, len({r['cid'] for r in ag}))
chk("2x2 unkSeats 40+103+0+0+9 == 152", 40 + 103 + 0 + 0 + 9, 152)
chk("per-constraint 170+58+2 == 230", 170 + 58 + 2, 230)
chk("affected 58+2 == 60", 58 + 2, 60)
chk("cross-leg agent seats 1355+3186+3802+4549+6522", 1355 + 3186 + 3802 + 4549 + 6522, 19414)
chk("imm-hole seats 198+570+452+551+663", 198 + 570 + 452 + 551 + 663, 2434)
chk("(power,exit) cells 15 mixed + 15 typed + 0 unk == 30", 15 + 15 + 0, 30)
chk("full-coord cells 40+19+248 == 307", 40 + 19 + 248, 307)
chk("twin shared-name hist 332+52+5+2+1 == 392", 332 + 52 + 5 + 2 + 1, 392)
chk("always-unknown support 18*1 + 1*2 == 20 seats", 18 * 1 + 1 * 2, 20)

print("\n=== percentages asserted (rounded to 1dp) ===")
for label, n, d, want in [("hole abstention rate", 44, 193, 22.8),
                          ("non-hole abstention rate", 108, 1140, 9.5),
                          ("hole share of live unknowns", 44, 152, 28.9),
                          ("descends(unknown) share", 12, 70, 17.1),
                          ("signature constructed_high has_unk", 16, 127, 12.6),
                          ("signature false_ci_rope has_unk", 41, 81, 50.6)]:
    chk(f"{label} {n}/{d}", round(100 * n / d, 1), want)

print("\n=== dedup: incidental findings must be 9 DISTINCT items ===")
txt = open('FINDINGS_INCIDENTAL.md').read()
heads = re.findall(r'^## (\d+)\. (.+?) ·', txt, re.M)
nums = [int(h[0]) for h in heads]
titles = [h[1].strip() for h in heads]
chk("item numbers are 1..9 with no repeats", nums, list(range(1, 10)))
chk("item titles are distinct", len(set(titles)), len(titles))
for n, t in zip(nums, titles):
    print(f"      {n}. {t[:78]}")

print("\n=== cross-document consistency ===")
w = open('WRITEUP.md').read()
chk("WRITEUP says 9 findings (matches the file)", w.count('9 findings outside'), 2)
chk("'21,414' appears only in the correction note", w.count('21,414'), 1)
# The hazard is an UNMARKED assertion that OQ-295 is free — not the substring,
# which both documents legitimately quote in order to mark it stale.
unmarked = [ln for ln in (w + txt).splitlines()
            if 'OQ-295' in ln and 'stale' not in ln and 'taken' not in ln and 'claimed' not in ln]
chk("no UNMARKED 'OQ-295 is next free' assertion", unmarked, [])

print()
print("RESULT:", "ALL CHECKS PASS" if not fails else f"{len(fails)} FAILURE(S): {fails}")
sys.exit(1 if fails else 0)
