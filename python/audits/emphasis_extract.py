#!/usr/bin/env python3
"""OQ-259 emphasis-aware docx→md converter (audits/2026-08-05_oq259_emphasis_discriminator).

Splices in-text emphasis markers (⟦HL⟧…⟦/HL⟧ for highlight, ⟦MIN⟧…⟦/MIN⟧ for minimized
font size) into word/document.xml by RAW-STRING surgery — never ElementTree
re-serialization, which breaks pandoc image extraction — then converts with the pinned
recipe `pandoc -f docx -t gfm --wrap=none` and post-fixes (underline-span strip +
re-applying pandoc line-start escapes suppressed by marker prefixes).

Self-checks: balanced/non-empty marker pairs in the output .md; with --verify-against,
stripping all markers from the output must reproduce the committed baseline .md
BYTE-EXACT (the conversion-environment control).

--scramble SEED reuses the injection machinery with randomized placement matched on
region count + span char-length distribution (Arm-2 control; seed pinned in the
PROPOSAL_ADDENDUM).

Usage:
  python3 python/audits/emphasis_extract.py INPUT.docx OUTPUT.md \
      --highlight-colors yellow --min-sz-cutoff 16 \
      [--verify-against BASELINE.md] [--keep-marked-docx PATH] [--scramble SEED]
"""
import argparse
import collections
import random
import re
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path

HL = ('⟦HL⟧', '⟦/HL⟧')
MIN = ('⟦MIN⟧', '⟦/MIN⟧')
ALL_MARKERS = (HL[0], HL[1], MIN[0], MIN[1])

W_NS = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
TOKEN = re.compile(r'<w:p[ >]|</w:p>|<w:r(?: [^>]*)?>.*?</w:r>', re.S)
WT = re.compile(r'(<w:t(?: [^>]*)?>)([^<]*)(</w:t>)', re.S)
RPR = re.compile(r'<w:rPr>.*?</w:rPr>', re.S)
PSTYLE = re.compile(r'<w:pStyle w:val="([^"]+)"')
PPR = re.compile(r'<w:pPr>.*?</w:pPr>', re.S)
MARKER_RE = re.compile(r'⟦/?(?:HL|MIN)⟧')


def style_map(zin):
    """Return (resolve(styleId)->sz-or-None, docDefault sz) from word/styles.xml.

    basedOn-chain recursion with cycle guard; docDefault fallback 20 if absent.
    Regex-based on purpose — same raw-string discipline as the document pass.
    """
    xml = zin.read('word/styles.xml').decode('utf-8')
    mdd = re.search(
        r'<w:docDefaults>.*?<w:rPrDefault>.*?<w:sz w:val="(\d+)"', xml, re.S)
    default = int(mdd.group(1)) if mdd else 20
    smap = {}
    for ms in re.finditer(r'<w:style [^>]*w:styleId="([^"]+)".*?</w:style>', xml, re.S):
        body = ms.group(0)
        mb = re.search(r'<w:basedOn w:val="([^"]+)"', body)
        msz = re.search(r'<w:rPr>(?:(?!</w:rPr>).)*?<w:sz w:val="(\d+)"', body, re.S)
        smap[ms.group(1)] = (mb.group(1) if mb else None,
                             int(msz.group(1)) if msz else None)

    def resolve(sid):
        seen = set()
        while sid and sid in smap and sid not in seen:
            seen.add(sid)
            base, sz = smap[sid]
            if sz is not None:
                return sz
            sid = base
        return None

    return resolve, default


def iter_runs(doc, resolve, default, colors, cutoff):
    """Yield (cls, run_start_offset, run_body) for every non-transparent run, in order.

    cls ∈ {'HL','MIN','NONE'}; paragraph boundaries are yielded as ('P', offset, None).
    Whitespace-only runs are transparent (skipped — they neither open, close, nor split
    a region).
    """
    psz = None
    for tok in TOKEN.finditer(doc):
        t = tok.group(0)
        if t.startswith('<w:p'):
            yield ('P', tok.start(), None)
            tagend = doc.find('>', tok.start()) + 1
            psz = None
            if doc.startswith('<w:pPr>', tagend):
                mp = PPR.match(doc, tagend)
                if mp:
                    ps = PSTYLE.search(mp.group(0))
                    if ps:
                        psz = resolve(ps.group(1))
        elif t == '</w:p>':
            yield ('P', tok.start(), None)
            psz = None
        else:
            joined = ''.join(m.group(2) for m in WT.finditer(t))
            if not joined.strip():
                continue
            mrpr = RPR.search(t)
            rpr = mrpr.group(0) if mrpr else ''
            hl = re.search(r'<w:highlight w:val="([^"]+)"', rpr)
            szm = re.search(r'<w:sz w:val="([^"]+)"', rpr)
            rsm = re.search(r'<w:rStyle w:val="([^"]+)"', rpr)
            if hl and hl.group(1) in colors:
                cls = 'HL'
            else:
                eff = (int(szm.group(1)) if szm
                       else (resolve(rsm.group(1)) if rsm else None))
                if eff is None:
                    eff = psz if psz is not None else default
                cls = 'MIN' if eff <= cutoff else 'NONE'
            yield (cls, tok.start(), t)


def collect_regions(doc, resolve, default, colors, cutoff):
    """Group consecutive same-class runs into regions; close at paragraph boundaries.

    Returns list of (cls, [(run_start, run_body), ...]).
    """
    regions = []
    cur, runs = None, []

    def flush():
        nonlocal cur, runs
        if cur in ('HL', 'MIN') and runs:
            regions.append((cur, runs))
        cur, runs = None, []

    for cls, off, body in iter_runs(doc, resolve, default, colors, cutoff):
        if cls == 'P':
            flush()
        elif cls == cur and cls != 'NONE':
            runs.append((off, body))
        else:
            flush()
            if cls in ('HL', 'MIN'):
                cur, runs = cls, [(off, body)]
    flush()
    return regions


def region_insertions(regions):
    """Open marker at first non-ws char of first run's w:t; close after last non-ws
    char of last run's w:t — always INSIDE an existing w:t (new runs break pandoc's
    Strong-merge)."""
    ins = []
    stats = collections.Counter()
    for cls, runs in regions:
        op, cl = HL if cls == 'HL' else MIN
        base, body = runs[0]
        for m in WT.finditer(body):
            s = m.group(2)
            if s.strip():
                lead = len(s) - len(s.lstrip())
                ins.append((base + m.start(2) + lead, op))
                break
        base, body = runs[-1]
        for m in reversed(list(WT.finditer(body))):
            s = m.group(2)
            if s.strip():
                ins.append((base + m.start(2) + len(s.rstrip()), cl))
                break
        stats[cls] += 1
    return ins, stats


def scramble_regions(doc, resolve, default, colors, cutoff, seed):
    """Arm-2 control: same region count + span char-length distribution as the real
    marking, placement randomized over non-transparent runs (paragraph-bounded),
    non-overlapping. Returns (regions, match_report)."""
    real = collect_regions(doc, resolve, default, colors, cutoff)
    targets = []  # (cls, char_len)
    for cls, runs in real:
        n = sum(len(m.group(2))
                for _, body in runs for m in WT.finditer(body))
        targets.append((cls, n))

    # paragraphs as lists of (idx, off, body, charlen) over ALL non-transparent runs
    paras, cur = [], []
    for cls, off, body in iter_runs(doc, resolve, default, colors, cutoff):
        if cls == 'P':
            if cur:
                paras.append(cur)
            cur = []
        else:
            cur.append((off, body,
                        sum(len(m.group(2)) for m in WT.finditer(body))))
    if cur:
        paras.append(cur)

    flat = [(pi, ri) for pi, p in enumerate(paras) for ri in range(len(p))]
    rng = random.Random(seed)
    rng.shuffle(flat)
    used = set()
    out_regions, achieved = [], []
    it = iter(flat)
    order = sorted(range(len(targets)), key=lambda i: -targets[i][1])
    placed = {}
    for ti in order:
        cls, want = targets[ti]
        got = None
        for pi, ri in flat:
            if (pi, ri) in used:
                continue
            runs, ln, rj = [], 0, ri
            while rj < len(paras[pi]) and (pi, rj) not in used:
                off, body, cl = paras[pi][rj]
                runs.append((off, body))
                ln += cl
                if ln >= want:
                    break
                rj += 1
            if runs:
                got = (runs, ln, pi, ri, rj)
                break
        if got is None:
            raise AssertionError('scramble: ran out of unused runs')
        runs, ln, pi, ri, rj = got
        for k in range(ri, rj + 1):
            used.add((pi, k))
        placed[ti] = (cls, runs, ln)
    for ti in range(len(targets)):
        cls, runs, ln = placed[ti]
        out_regions.append((cls, runs))
        achieved.append((cls, targets[ti][1], ln))
    return out_regions, achieved


def splice(doc, ins):
    """Single-pass piecewise splice (O(n)) — never repeated string copies."""
    pieces, prev = [], 0
    for off, s in sorted(ins, key=lambda x: (x[0],)):
        pieces.append(doc[prev:off])
        pieces.append(s)
        prev = off
    pieces.append(doc[prev:])
    return ''.join(pieces)


def make_marked_docx(inp, outp, colors, cutoff, scramble_seed=None):
    zin = zipfile.ZipFile(inp)
    resolve, default = style_map(zin)
    doc = zin.read('word/document.xml').decode('utf-8')
    assert 'txbxContent' not in doc, 'text boxes present — splice offsets unsafe'
    assert '<w:ins ' not in doc and '<w:del ' not in doc, \
        'tracked changes present — splice offsets unsafe'
    for mk in ALL_MARKERS:
        assert mk not in doc, f'marker collision: {mk!r} already in document.xml'
    report = None
    if scramble_seed is None:
        regions = collect_regions(doc, resolve, default, colors, cutoff)
    else:
        regions, report = scramble_regions(
            doc, resolve, default, colors, cutoff, scramble_seed)
    ins, stats = region_insertions(regions)
    new = splice(doc, ins)
    with zipfile.ZipFile(outp, 'w', zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)  # byte-copy rezip for every other member
            if item.filename == 'word/document.xml':
                data = new.encode('utf-8')
            zout.writestr(item, data)
    zin.close()
    return stats, report


def pandoc_convert(docx_path, md_path):
    cmd = ['pandoc', '-f', 'docx', '-t', 'gfm', '--wrap=none',
           str(docx_path), '-o', str(md_path)]
    subprocess.run(cmd, check=True)
    return ' '.join(cmd)


def postfix(text):
    """Underline-span strip + re-apply pandoc line-start escapes suppressed by marker
    prefixes (ported verbatim from the verified prototype postfix.py)."""
    text = text.replace('<span class="underline">', '').replace('</span>', '')

    def fix(m):
        pre, rest = m.group(1), m.group(2)
        r2 = re.sub(r'^(\d+)([.)])(?=\s|$)', r'\1\\\2', rest)
        if re.match(r'^([-+*]\s|#{1,6}\s|>)', r2):
            r2 = re.sub(r'^([-+*#>])', r'\\\1', r2)
        return pre + r2

    return re.sub(r'(?m)^((?:⟦/?(?:HL|MIN)⟧)+)(.*)$', fix, text)


def self_check(md_text):
    """Balanced, properly sequenced, non-empty marker pairs."""
    open_of = {HL[0]: 'HL', MIN[0]: 'MIN'}
    close_of = {HL[1]: 'HL', MIN[1]: 'MIN'}
    stack = None
    last_open_end = None
    counts = collections.Counter()
    for m in MARKER_RE.finditer(md_text):
        tok = m.group(0)
        if tok in open_of:
            assert stack is None, f'nested open {tok} at {m.start()}'
            stack = open_of[tok]
            last_open_end = m.end()
        else:
            assert stack == close_of[tok], \
                f'unmatched close {tok} at {m.start()} (open={stack})'
            assert md_text[last_open_end:m.start()].strip(), \
                f'empty marker pair ending at {m.start()}'
            counts[stack] += 1
            stack = None
    assert stack is None, f'unclosed {stack} region at EOF'
    return counts


def strip_markers(md_text):
    return MARKER_RE.sub('', md_text)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument('input_docx')
    ap.add_argument('output_md')
    ap.add_argument('--highlight-colors', default='yellow',
                    help='comma-separated w:highlight values counted as HL')
    ap.add_argument('--min-sz-cutoff', type=int, default=16,
                    help='effective w:sz (half-points) at or below which a run is MIN')
    ap.add_argument('--verify-against', default=None,
                    help='baseline .md; stripping markers from output must match it byte-exact')
    ap.add_argument('--keep-marked-docx', default=None)
    ap.add_argument('--scramble', type=int, default=None, metavar='SEED',
                    help='Arm-2 control: randomized placement, matched count + span-length distribution')
    a = ap.parse_args(argv)

    colors = set(a.highlight_colors.split(','))
    with tempfile.TemporaryDirectory() as td:
        marked = Path(a.keep_marked_docx) if a.keep_marked_docx else Path(td) / 'marked.docx'
        stats, report = make_marked_docx(
            a.input_docx, marked, colors, a.min_sz_cutoff, a.scramble)
        recipe = pandoc_convert(marked, Path(td) / 'raw.md')
        text = postfix((Path(td) / 'raw.md').read_text())
    Path(a.output_md).write_text(text)

    md_counts = self_check(text)
    print(f'[emphasis_extract] docx regions spliced: HL={stats["HL"]} MIN={stats["MIN"]}'
          f'{" (SCRAMBLED, seed=%d)" % a.scramble if a.scramble is not None else ""}')
    print(f'[emphasis_extract] md marker pairs (self-check PASS): '
          f'HL={md_counts["HL"]} MIN={md_counts["MIN"]}')
    print(f'[emphasis_extract] pandoc recipe: {recipe}')
    if report is not None:
        diffs = [abs(w - g) for _, w, g in report]
        print(f'[emphasis_extract] scramble span-length match: n={len(report)}, '
              f'mean |target-achieved| = {sum(diffs)/len(diffs):.1f} chars, '
              f'max = {max(diffs)}')
    if a.verify_against:
        baseline = Path(a.verify_against).read_bytes()
        stripped = strip_markers(text).encode('utf-8')
        if stripped == baseline:
            print(f'[emphasis_extract] VERIFY: strip(output) == {a.verify_against} '
                  f'BYTE-EXACT ({len(baseline)} bytes)')
        else:
            print(f'[emphasis_extract] VERIFY FAILED: strip(output) != '
                  f'{a.verify_against} ({len(stripped)} vs {len(baseline)} bytes)',
                  file=sys.stderr)
            return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
