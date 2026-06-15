#!/usr/bin/env python3
"""Reproduces the adjudication's held-key-vs-executor table and the two-sided gate
control from the two committed JSONs. Run from this audit directory."""
import json

held = {h['sample_id']: h for h in json.load(open('adjudicator_held_key.json'))['held']}
judge = {j['sample_id']: j for j in json.load(open('judge_results.json'))}

def core(diag):
    diag = diag.replace('Ω_', '').replace('hybrid(', '').replace(')', '')
    return set(t.strip() for t in diag.replace('+', ',').split(',')
               if t.strip() and t.strip() != 'restatement')

print("HELD-KEY vs EXECUTOR (14 sealed omegas)\n")
print(f"{'id':>3} {'name':42} {'adj(held)':18} {'exec':6} verdict")
agree = disagree = 0
for sid in sorted(held):
    h, j = held[sid], judge[sid]
    hd, jd = h['diagnosis'], j['diagnosis']
    hset, jset = core(hd), core(jd)
    if hd == jd:
        v, ok = 'AGREE', True
    elif 'restatement' in hd and 'restatement' not in jd:
        v, ok = 'ADJ=restate / EXEC=external', False
    elif jset and hset and jset <= hset:
        v, ok = 'partial (exec ⊆ adj hybrid)', True
    else:
        v, ok = 'DISAGREE', False
    agree += ok; disagree += (not ok)
    print(f"{sid:>3} {h['name'][:42]:42} {hd:18} {jd:6} {v}")
print(f"\nAGREE (incl hybrid-subset): {agree}/14 ; adj-types-differently: {disagree}/14")

print("\nTWO-SIDED GATE CONTROL (adjudicator's seeded controls):")
for sid in sorted(held):
    h, j = held[sid], judge[sid]
    role = h.get('control_role', 'none')
    if role == 'none':
        continue
    jd = j['diagnosis']; restated = 'restatement' in jd
    if role == 'KNOWN_EXTERNAL':
        print(f"  id{sid:>2} KNOWN_EXTERNAL   exec={jd:6} -> {'PASSED external ✓' if not restated else 'FALSE-RESTATED ✗'}")
    elif role == 'KNOWN_RESTATEMENT':
        print(f"  id{sid:>2} KNOWN_RESTATEMENT exec={jd:6} -> {'CAUGHT ✓' if restated else 'MISSED false-neg ✗'}")
    elif role == 'UNDER_DECLARATION':
        print(f"  id{sid:>2} UNDER_DECLARATION exec={jd:6} -> {'routed external ✓' if not restated else 'mis-restated ✗'}")
