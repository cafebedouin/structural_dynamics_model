# OQ-138 FNL conversion — twin pipeline OLD-vs-NEW projection diff (Verification step 3).
# Extends the FCR extract_verdicts.py projection to carry the FULL verdict surface:
# claimed | sig | base | join | cap | grade | alerts | sheaf | h1 | diag verdict | tensions.
# Grade-only reads are exactly what produced FSM's wrong "expected green" (the surprise
# lived in dirac second_class + cohomology fails_descent, which surface here via
# diag tensions + sheaf_status).
import json, sys

def project(path):
    d = json.load(open(path))
    out = {}
    for rec in d['per_constraint']:
        vj = rec.get('verdict_join') or {}
        dv = rec.get('diagnostic_verdict') or {}
        alerts = ";".join(f"{a.get('type')}:{a.get('severity')}" for a in (vj.get('alerts') or []))
        tensions = ";".join(sorted(
            t.get('subsystem', '?') if isinstance(t, dict) else str(t)
            for t in (dv.get('tensions') or [])))
        out[rec['id']] = (
            f"claimed={rec.get('claimed_type')}|sig={rec.get('signature')}"
            f"|base={vj.get('base_verdict')}|join={vj.get('verdict')}|cap={vj.get('cap_applied')}"
            f"|grade={vj.get('signature_grade')}|alerts=[{alerts}]"
            f"|sheaf={rec.get('sheaf_status')}|h1={rec.get('h1_band')}"
            f"|diag={dv.get('verdict')}|tensions=[{tensions}]"
        )
    return out, d['manifest']

old_path, new_path, label = sys.argv[1], sys.argv[2], sys.argv[3]
old, mo = project(old_path)
new, mn = project(new_path)
print(f"== {label} ==")
print(f"OLD manifest: run_at={mo['pipeline_run_at']} n={mo['n_constraints']} code={mo['code_commit_short']} dirty={mo.get('code_dirty')}")
print(f"NEW manifest: run_at={mn['pipeline_run_at']} n={mn['n_constraints']} code={mn['code_commit_short']} dirty={mn.get('code_dirty')}")
assert set(old) == set(new), "id sets differ — not a like-for-like diff"
changed = [c for c in sorted(old) if old[c] != new[c]]
print(f"n={len(old)} | changed={len(changed)}")
for c in changed:
    print(f"\n{c}")
    print(f"  OLD {old[c]}")
    print(f"  NEW {new[c]}")
