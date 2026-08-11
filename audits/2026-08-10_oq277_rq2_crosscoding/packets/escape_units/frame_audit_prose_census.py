"""Frame audit: how many of the 101 non-census directories could the keyword proxy
see at all? The proxy is `grep -rl <patterns> --include='*.md' audits/` — so a
directory with zero .md files (recursively) is UNSEEABLE: it can be neither a hit
nor a miss, and counting it in the denominator inflates n without adding coverage."""
import subprocess, pathlib, sys

FRAME = pathlib.Path("audits/2026-08-10_oq277_rq2_crosscoding/frame")
dirs = FRAME.joinpath("non_census_dirs.txt").read_text().split()
print(f"population read from frozen listing: {len(dirs)} dirs (manifest says 101)")

# md5 the listing we actually read, against the frozen manifest value
md5 = subprocess.run(["/usr/bin/md5sum", str(FRAME / "non_census_dirs.txt")],
                     capture_output=True, text=True).stdout.split()[0]
print(f"listing md5 = {md5}  (manifest: ecc91562c0888aeb246d90fa6dd56da2)  "
      f"{'MATCH' if md5 == 'ecc91562c0888aeb246d90fa6dd56da2' else 'MISMATCH — STOP'}")

def md_files(d):
    p = pathlib.Path("audits") / d
    return sorted(x for x in p.rglob("*.md") if x.is_file())

unseeable, seeable, empty_md = [], [], []
for d in dirs:
    mds = md_files(d)
    if not mds:
        unseeable.append(d)
    else:
        seeable.append(d)
        if sum(x.stat().st_size for x in mds) == 0:
            empty_md.append(d)

print(f"\n=== RESULT ===")
print(f"seeable by the proxy (>=1 .md, recursive) : {len(seeable)}")
print(f"UNSEEABLE (zero .md anywhere)             : {len(unseeable)}")
print(f"  of the seeable, .md total 0 bytes       : {len(empty_md)}")
print(f"partition check: {len(seeable)} + {len(unseeable)} = {len(seeable)+len(unseeable)} (want {len(dirs)})")
print("\nunseeable directories:")
for d in unseeable:
    kinds = sorted({x.suffix or '(noext)' for x in (pathlib.Path('audits')/d).rglob('*') if x.is_file()})
    print(f"  {d}   file types: {kinds}")

print("\n=== TWO-SIDED CONTROL (the census must separate cases I already know) ===")
KNOWN_SEEABLE   = ["2026-06-11_oq46_close", "2026-06-11_oq98_verdict_join",
                   "2026-02-25_spectral_laplacian"]   # last one: .md only inside outputs*/ subdirs
KNOWN_UNSEEABLE = ["2026-06-04_oq71_depth_lineage", "2026-07-24_oq153_update_authority_step2"]
ok = True
for d in KNOWN_SEEABLE:
    hit = bool(md_files(d)); ok &= hit
    print(f"  positive control {d:52s} seeable={hit} {'PASS' if hit else 'FAIL'}")
for d in KNOWN_UNSEEABLE:
    hit = not md_files(d); ok &= hit
    print(f"  negative control {d:52s} unseeable={hit} {'PASS' if hit else 'FAIL'}")
print(f"  control verdict: {'GREEN — the census separates both directions' if ok else 'RED'}")
sys.exit(0 if ok else 1)
