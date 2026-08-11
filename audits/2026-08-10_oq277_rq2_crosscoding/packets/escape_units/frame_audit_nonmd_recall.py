"""Second frame defect, measured mechanically — no judging involved.

The proxy is `grep -rl <6 patterns> --include='*.md'`. A directory can therefore be
SEEABLE (has .md) and still be classified non-incident-bearing because the prose that
reports its incident lives in a .txt/.out/.py/.pl file the filter never opens.

Question: of the 101 non-census dirs, how many contain the proxy's OWN patterns in a
NON-.md file? Those are directories the proxy would have flagged had its file filter
been wider. This is the same grep, same patterns, one filter changed.
"""
import subprocess, pathlib

PAT = r'for its whole life\|never fired\|never ran\|read.*0 for\|was never\|silently'
FRAME = pathlib.Path("audits/2026-08-10_oq277_rq2_crosscoding/frame")
non_census = FRAME.joinpath("non_census_dirs.txt").read_text().split()
incident   = FRAME.joinpath("incident_bearing_dirs.txt").read_text().split()

def hits(d, include_md):
    cmd = ["/usr/bin/grep", "-rl", PAT, f"audits/{d}"]
    if include_md:
        cmd.insert(3, "--include=*.md")
    else:
        cmd.insert(3, "--exclude=*.md")
    r = subprocess.run(cmd, capture_output=True, text=True)
    return [l for l in r.stdout.split("\n") if l]

print("=== CONTROL A (positive): the same grep must reproduce the proxy on the 73 ===")
repro = sum(1 for d in incident if hits(d, include_md=True))
print(f"  incident-bearing dirs hit by my .md grep: {repro}/{len(incident)} "
      f"{'PASS' if repro == len(incident) else 'FAIL — not the same grep'}")

print("=== CONTROL B (negative): by construction the 101 must have ZERO .md hits ===")
leak = [d for d in non_census if hits(d, include_md=True)]
print(f"  non-census dirs hit by my .md grep: {len(leak)} "
      f"{'PASS' if not leak else 'FAIL — frame and my grep disagree: ' + str(leak)}")

print("\n=== RESULT: same patterns, non-.md files only, over the 101 ===")
found = {d: hits(d, include_md=False) for d in non_census}
found = {d: v for d, v in found.items() if v}
print(f"  directories the proxy could have flagged but never opened: {len(found)}/101")
for d, files in sorted(found.items()):
    exts = sorted({pathlib.Path(f).suffix for f in files})
    print(f"    {d:52s} {len(files):3d} file(s) {exts}")
