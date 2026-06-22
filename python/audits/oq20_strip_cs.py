#!/usr/bin/env python3
"""OQ-20 Arm 2 — strip cs_* facts and declarations from kernel_v1 testsets.

Introduced-instrument discipline (CLAUDE.md Build Discipline): the strip is itself
a claim, so this script ships its own positive controls and refuses to emit a
corpus that fails them.

Targets (line-based; kernel_v1 facts are module-qualified single-line clauses):
  - fact clauses:      `narrative_ontology:cs_<name>(...).`
  - multifile decls:   `narrative_ontology:cs_<name>/N ,`  (inside a `:- multifile` block)

If a removed decl line terminated its directive (ended in `.`), the prior kept
decl line's trailing comma is repaired to a period so the directive stays valid.

Usage:
    python3 python/audits/oq20_strip_cs.py <src_dir> <dst_dir>
Controls run automatically after the strip; exit 0 only if all pass.
"""
import sys, os, re, glob, subprocess, hashlib

CS_FACT = re.compile(r'^\s*narrative_ontology:cs_[a-z_]+\(.*\)\.\s*$')
CS_DECL = re.compile(r'^\s*narrative_ontology:cs_[a-z_]+/[0-9]+\s*([,.])\s*$')
# a generic multifile/discontiguous decl entry line (qualified pred indicator)
DECL_ENTRY = re.compile(r'^\s*[a-z_]+:[a-z_]+/[0-9]+\s*([,.])\s*$')


def strip_lines(lines):
    """Return (new_lines, n_removed). Repairs a dangling comma if a removed
    decl was the directive terminator."""
    out = []
    removed = 0
    for ln in lines:
        if CS_FACT.match(ln):
            removed += 1
            continue
        m = CS_DECL.match(ln)
        if m:
            removed += 1
            if m.group(1) == '.':
                # this cs decl terminated the multifile directive: repair the
                # most recent kept decl-entry line's trailing comma -> period
                for i in range(len(out) - 1, -1, -1):
                    dm = DECL_ENTRY.match(out[i])
                    if dm:
                        if dm.group(1) == ',':
                            out[i] = re.sub(r',(\s*)$', r'.\1', out[i])
                        break
                    if out[i].strip():  # hit a non-blank, non-decl line first
                        break
            continue
        out.append(ln)
    return out, removed


def strip_file(src, dst):
    with open(src, 'r') as f:
        lines = f.readlines()
    new, removed = strip_lines(lines)
    with open(dst, 'w') as f:
        f.writelines(new)
    return removed


def file_has_cs(path):
    with open(path) as f:
        return bool(re.search(r'narrative_ontology:cs_[a-z_]+\(', f.read()))


def sha(path):
    return hashlib.sha256(open(path, 'rb').read()).hexdigest()


def parse_ok(path):
    """swipl consult check: file must load without error."""
    g = f"catch(consult('{path}'),E,(print_message(error,E),halt(1))), halt(0)"
    r = subprocess.run(['swipl', '-q', '-g', g, '-t', 'halt(1)'],
                       capture_output=True, text=True, timeout=60)
    return r.returncode == 0, (r.stderr or r.stdout)


def main():
    if len(sys.argv) != 3:
        print(__doc__); sys.exit(2)
    src_dir, dst_dir = sys.argv[1], sys.argv[2]
    os.makedirs(dst_dir, exist_ok=True)
    files = sorted(glob.glob(os.path.join(src_dir, '*.pl')))
    if not files:
        print(f"ABORT: no .pl files in {src_dir}"); sys.exit(1)

    cs_free, cs_bearing = [], []
    for src in files:
        if file_has_cs(src):
            cs_bearing.append(src)
        else:
            cs_free.append(src)
    print(f"[strip] {len(files)} files: {len(cs_bearing)} cs-bearing, {len(cs_free)} cs-free")

    for src in files:
        strip_file(src, os.path.join(dst_dir, os.path.basename(src)))

    # ---- Control 1: cs-free files come out byte-identical ----
    bad = []
    for src in cs_free:
        dst = os.path.join(dst_dir, os.path.basename(src))
        if sha(src) != sha(dst):
            bad.append(os.path.basename(src))
    if bad:
        print(f"CONTROL 1 FAIL: {len(bad)} cs-free files changed (e.g. {bad[:5]}); ABORT")
        sys.exit(1)
    print(f"[control 1 PASS] all {len(cs_free)} cs-free files byte-identical after strip")

    # ---- Control 2: on cs-bearing files, ONLY cs_ lines removed ----
    bad2 = []
    for src in cs_bearing:
        dst = os.path.join(dst_dir, os.path.basename(src))
        srcl = open(src).readlines()
        dstl = open(dst).readlines()
        # every line removed (in src not in dst, positionally) must contain cs_
        import difflib
        diff = difflib.ndiff(srcl, dstl)
        for d in diff:
            if d.startswith('- ') and 'cs_' not in d:
                bad2.append((os.path.basename(src), d.rstrip()))
                break
    if bad2:
        print(f"CONTROL 2 FAIL: non-cs lines removed (e.g. {bad2[:3]}); ABORT")
        sys.exit(1)
    print(f"[control 2 PASS] on all {len(cs_bearing)} cs-bearing files, only cs_ lines removed")

    # ---- Control 3: residual cs in dst == 0 ----
    residual = [os.path.basename(p) for p in glob.glob(os.path.join(dst_dir, '*.pl'))
                if file_has_cs(p)]
    if residual:
        print(f"CONTROL 3 FAIL: {len(residual)} stripped files still carry cs facts; ABORT")
        sys.exit(1)
    print(f"[control 3 PASS] zero residual cs facts in {dst_dir}")

    # ---- Control 4: a sample of stripped files parse/load cleanly ----
    # (full-corpus parse happens at load time; sample cs-bearing + cs-free here)
    sample = cs_bearing[:5] + cs_free[:2]
    for src in sample:
        dst = os.path.join(dst_dir, os.path.basename(src))
        ok, msg = parse_ok(os.path.abspath(dst))
        if not ok:
            print(f"CONTROL 4 FAIL: {os.path.basename(dst)} does not parse:\n{msg}\nABORT")
            sys.exit(1)
    print(f"[control 4 PASS] {len(sample)} sampled stripped files parse cleanly")

    print(f"[strip] DONE -> {dst_dir} ({len(files)} files)")


if __name__ == '__main__':
    main()
