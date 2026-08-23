"""Two-sided control for the OQ-301 R7 retry warning, exercised through run_prolog
itself (not by calling the helper directly) so it witnesses that it is CALLED."""
import io, sys, contextlib
sys.path.insert(0, "python")
import run_pipeline as rp

calls = {"n": 0}

def fake_once(modules, goal, timeout, attempt, attempts):
    calls["n"] += 1
    if attempt < attempts:
        e = rp.PrologError("simulated death by signal")
        e.signalled = True
        raise e
    return "OK"

rp._run_prolog_once = fake_once

def run(modules, goal, label):
    calls["n"] = 0
    err = io.StringIO()
    with contextlib.redirect_stderr(err):
        res = rp.run_prolog(modules, goal, attempts=3)
    out = err.getvalue()
    fired = "[OQ-301]" in out
    print(f"{label:<34} attempts_made={calls['n']}  result={res!r}  "
          f"warning={'FIRED' if fired else 'declined'}")
    for line in out.splitlines():
        print("      " + line)
    return fired

print("== control A: the giant_comp goal (must FIRE, twice — once per retry) ==")
a = run(["stack.pl", "giant_component_analysis.pl"], "run_giant_component_analysis",
        "giant_comp / signalled retry")

print()
print("== control B: an unrelated goal, same failure (must DECLINE) ==")
b = run(["stack.pl", "commentary_census.pl"], "run_commentary_census",
        "other goal / signalled retry")

print()
print("== control C: giant_comp named only in the MODULES, not the goal (must FIRE) ==")
c = run(["stack.pl", "giant_component_analysis.pl"], "run_phase2",
        "giant_comp module / other goal")

print()
print("== control D: no retry at all — first attempt succeeds (must DECLINE) ==")
calls["n"] = 0
rp._run_prolog_once = lambda modules, goal, timeout, attempt, attempts: "OK"
err = io.StringIO()
with contextlib.redirect_stderr(err):
    rp.run_prolog(["giant_component_analysis.pl"], "run_giant_component_analysis", attempts=3)
d = "[OQ-301]" in err.getvalue()
print(f"{'clean giant_comp run':<34} warning={'FIRED' if d else 'declined'}")

print()
ok = a and (not b) and c and (not d)
print("TWO-SIDED CONTROL:", "PASS" if ok else "*** FAIL ***",
      f"(A={a} B={b} C={c} D={d}; want True/False/True/False)")
sys.exit(0 if ok else 1)
