# Frame volume — measured, because a handoff estimate was wrong by 4x

**Measured:** 2026-08-10, by the receiving extractor, before planning against the estimate.
**Method:** total bytes of all `*.md` under each directory, recursive (`find -name '*.md' -printf '%s\n'`).
Frame listing: `incident_bearing_dirs.txt`, 73 rows, md5 `57149263fef05f1439d9ed98e755a363`.

## The numbers

| set | n | total | mean | median | top-5 share |
|---|---|---|---|---|---|
| primary sample | 22 | **737 KB** | 33.5 KB | 23.8 KB | 49% |
| full incident-bearing frame | 73 | **5,176 KB** | 70.9 KB | 23.9 KB | 58% |

## Consequence 1 — the handoff's estimate was 4.0x low

`HANDOFF.md` states direction (ii) needs "~184 KB of audit prose read *carefully*." Measured: **737 KB.**

The error matters because of what the handoff itself says two sentences earlier: skimming produces
thin units, thin units bias toward `other`, and that is precisely what control (c) exists to
measure — so a degraded extraction would confound the control meant to catch it. An estimate 4x
low is an estimate that makes exactly that degradation look affordable.

**`Fired: latent` in the handoff instrument.** Nothing downstream was corrupted, because the
receiving extractor measured before planning. But the defect is real and conditional only on
someone *not* measuring: an instance running low on context, describing the work it was handing
off, produced a volume estimate wrong by 4x **in the direction that made the handoff look
smaller**. The direction is the part worth noticing.

**Rider, general (operator, 2026-08-10):** *a handoff's volume estimates are claims, and the
receiver measures them before planning against them.* Same class as any other inherited premise —
a number written by a context-starved instance about work it is shedding has the weakest provenance
of anything in the handoff, and it is the number most likely to be planned against unexamined.

## Consequence 2 — the staged extension is 7x, not 3.3x

The pinned extension rule reads "the extension changes n and NOTHING else," which invites pricing
it by n. By n it is 3.3x (22 → 73). **By the quantity that actually binds the extractor it is
7.0x** (737 KB → 5,176 KB): the extension reads 4,439 KB *more* than stage 1, six times the
stage-1 volume again.

**Because the frame is heavy-tailed, and the sample missed the tail.**

- Full frame top 5: **1,540 KB**, 841 KB, 242 KB, 190 KB, 177 KB — 58% of all prose in 7% of dirs.
- Sample top 5: 116 KB, 76 KB, 71 KB, 54 KB, 44 KB.

The seeded draw was not unfair — **the medians are 23.8 KB vs 23.9 KB, near-identical**, so the
sample represents the body of the distribution well. It simply cannot represent tail mass at n=22.
The two largest directories in the frame are each larger than a quarter of the entire sample.

**This is priced now, before H5, per operator ruling** — deliberately not left until the extension
is authorized, when the cost would be discovered by whoever had already committed to paying it.

**What it implies for the extension, stated as a constraint and not a plan:** at ~1.3M tokens of
prose, the full 73 is not extractable by any single instance, nor by the 11-unit split used at
stage 1. Extending would need either a materially larger extractor fleet or a different protocol
for the tail directories — and a different protocol is *not* "changes n and nothing else." **If
the extension requires changing how units are extracted, that is a new experiment under the pinned
rule, not an extension.** Flagging the collision now; the ruling is the operator's at the H5 gate.

## Reproduction

```
python3 - <<'PY'
import json,subprocess,statistics
frame=[l.strip() for l in open('audits/2026-08-10_oq277_rq2_crosscoding/frame/incident_bearing_dirs.txt') if l.strip()]
def md_bytes(d):
    out=subprocess.run(['find',f'audits/{d}','-name','*.md','-type','f','-printf','%s\n'],
                       capture_output=True,text=True).stdout.split()
    return sum(int(x) for x in out)
print(sum(md_bytes(d) for d in frame)/1024, 'KB over', len(frame), 'dirs')
PY
```

Run from the repository root. Note the same pinning discipline the frame command needs: this walks
`audits/<dir>` with an explicit prefix, so it does not depend on the invoking shell's `grep` or on
the current working directory.
