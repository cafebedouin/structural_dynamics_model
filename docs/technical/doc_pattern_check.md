# doc_pattern_check.py — pins, extraction assumptions, and failure semantics

`python/doc_pattern_check.py` makes the canonicity of the build-discipline defect taxonomy a
**checked fact** rather than a memory. It is the direct application of the taxonomy's own
Pattern 2 rule — *"one canonical location per thing, and canonicity must be a checked fact (a
path in docs, a CI check), not a memory"* — to the pattern list itself.

Gate row: `doc patterns` in `scripts/gate.sh`, immediately after `spec enums`. Owning issue:
**OQ-278**.

## What it does

Two always-read documents publish the same numbered taxonomy:

| document | encoding | read by |
|---|---|---|
| `CLAUDE.md` | numbered bold list under `## Build Discipline` | every instance, on load |
| `docs/technical/build_discipline.md` | `## Pattern N` headings | anyone following the pointer |
| `docs/technical/build_discipline.md` | the spine table under `## The spine:` | a second encoding **inside the same file** |

The checker extracts index→name from each, normalizes, and reports any index where the two
documents disagree without that disagreement being declared.

**Why cardinality checks cannot work here, and this is the whole design constraint.** The two
lists were born divergent and their *counts* converged at the exact commit where their
*contents* diverged:

```
7af6b945  2026-05-29   CLAUDE=3  BD=2   <- unequal, for one day
220739b8  2026-05-30   CLAUDE=4  BD=4   <- counts CONVERGE; contents DIVERGE at 3 and 4
aaba00e0  2026-05-31   5  5     f8f9eb6b  2026-06-10   6  6     HEAD  6  6
```

Anything comparing member counts reads green from 2026-05-30 forever. The collision then went
undetected across 151 commits touching one or both files. **So names are compared per index,
never totals.**

## The manifest holds LOCATIONS and STATES — never slugs

Storing the authored pattern names in the checker would make it a **third authored copy** of
the taxonomy, hand-synced at every renumbering, inside an instrument whose entire subject is
that failure. `spec_enum_check.py` opens by recording that manual spec/code sync *"has failed
structurally twice."* Both idioms this checker is modelled on hold *locations* or *derived
output*, not authored content.

```python
SITES = {...}                        # (path, kind) per index->name encoding
DECLARED_COLLISIONS = {}             # index -> (owning OQ, state). EMPTY since 2026-08-17:
                                     # OQ-278 ruled and both entries retired in the same change.
DECLARED_SPINE_LAG = {"missing_from_spine": [], "owning_oq": "OQ-278", "reason": "..."}
# agreement is COMPUTED from the documents.
```

**The `state` field is load-bearing whenever an entry exists.** While the collision was live
it carried the difference between index 4 (an unruled collision with two claimants) and index 3
(a **ruled** one — `CLAUDE.md` vacated it 2026-08-11 — with an unrepaired second site still
publishing `bound-probe`). Without the state, an `UNDECLARED RESOLUTION` red is uninterpretable
without opening the OQ.

**Both entries retired 2026-08-17, in the same change that landed the ruling** (R2 = C2,
R1b′ = B1′): index 4 is `fabricated-default` in both documents, index 3 is a grave in both, and
the spine table publishes every index its headings do. **An empty allowlist is the strong state,
not a lapsed one** — every index is compared with nothing exempted, so any divergence is a new
fork and reds immediately. Leaving either entry behind would have fired `UNDECLARED RESOLUTION`,
which is the guard working.

## Extraction assumptions (read before editing the pinned files)

| assumption | where | what breaks it |
|---|---|---|
| `CLAUDE.md`'s list lives under a heading matching `^## Build Discipline\b`, ending at the next `^## ` | section scoping | renaming that heading ⇒ `RuntimeError`, not a silent empty |
| items are `**N. Name (gloss).**` … | `CM_BOLD_RE` | dropping the bold, or the trailing period |
| …**and the bold run may span a line break** | `re.DOTALL` on `CM_BOLD_RE` | **This bit twice during construction.** Items 3 and 6 hard-wrap *inside* the bold run, so the closing `**` is on the next line. Without `DOTALL` those two extract to nothing and the check reads green on four of six — the checker's own Pattern 5. |
| an **unbolded** `N. Name (gloss). ` form also parses | `CM_PLAIN_RE` | required only for `--pairwise` at `7af6b945`/`220739b8`, where item 3 was authored unbolded. Do not remove it: it is what lets the discrimination record reach the defect's own commit. |
| `build_discipline.md` headings are `^## Pattern N[—:-] Name` | `BD_HEADING_RE` | changing the separator, or renumbering a heading |
| the vacated index is recognized by the literal string `VACATED` | `normalize()` | rewording that marker. The empty split is **not** relied on — index 3's text begins with an em-dash, so a naive head-phrase split yields `''`, which is indistinguishable from a failed match. |
| names normalize to the head phrase before the first `(` or `—` | `normalize()` | parentheticals differ even for **shared** members (`CM-P5` "…vs absent" vs `BD-P5` "…vs absent conflation"), so a whole-string compare would fire on gloss drift rather than divergence |

Empty extraction on either side raises `RuntimeError` rather than returning `{}` — a regex over
prose that matches nothing is exactly the absence-satisfies-the-gate shape.

## Failure semantics

| verdict | meaning |
|---|---|
| `MISSING INDEX` | one document publishes an index the other does not |
| `DIVERGENT` | the two disagree at an index and the disagreement is **not** declared — a NEW fork, the thing that went unnoticed for 151 commits |
| `UNDECLARED RESOLUTION` | a declared collision where the documents now **agree**: a ruling landed in the documents without updating the allowlist. Same stale-entry report `prolog/axis_boundary_allowlist.txt` makes — a silent resolution is as much a defect as a silent fork |
| `UNKNOWN COLLISION` | the allowlist names an index no document publishes (points at nothing) |
| `SPINE LAG` | `build_discipline.md`'s spine table publishes a different **index set** than its own headings, beyond the declared lag |

Exit 0 green, 1 red, 2 usage. Last stdout line is the one-line verdict — `scripts/gate.sh`
displays only `tail -1`.

## Scope of the verdict line — stated, because an unstated selection rule is this taxonomy's own subject

**Names** are compared between `CLAUDE.md`'s list and `build_discipline.md`'s **headings**.

The spine table is checked for **index-set agreement only, not names.** It deliberately uses
short forms — `Silent fork` for `One-canonical-thing-became-two`, `Bound-probe bypasses cut` for
`…bypasses clause-order` — so a name comparison there would fire on abbreviation rather than on
divergence. Checking its *index set* is what caught the live defect: the spine table published
1–5 and its prose said *"The five patterns"* while the headings published 1–6. That omission was
**declared** (`DECLARED_SPINE_LAG`) rather than repaired, because editing the spine before
OQ-278's ruling would have destroyed the evidence of what each document published when. The
ruling landed 2026-08-17, the spine now publishes 1–8 (index 3 as an explicit empty row), and the
declaration retired with it — symmetric with the collision allowlist.

## The silent edges (what the checker does NOT cover)

Enumerated rather than left implicit — each is a real index→pattern encoding this row does not
read, so a green `doc patterns` says nothing about them:

- `CLAUDE.md:158` — cardinality claim ("seven members at eight indices").
- `README.md:170` — cardinality claim ("the seven defect patterns").
- `docs/amnesiac_institution*.md` — six paper versions, each publishing the list.
- `docs/design/design_discipline.md` `:464` `:600` `:710`; ~10 `KNOWN_STATE.md` index citations.
- `audits/2026-08-10_oq277_rq2_crosscoding/PREREGISTRATION.md:350,356` — **md5-frozen and
  gate-enforced** (`scripts/gate.sh`, `oq277 freeze`). This checker must **never write it**, and
  does not read it either: it is a point-in-time record of what an out-of-harness coder was
  shown, so it stays *correct* while expressing a numbering the live documents may leave.
- 228 machine-generated JSON payloads under the oq277 audit — point-in-time.

Second edge: the checker guards **agreement**, not **aptness**. Two documents can agree on a
name at an index that is the wrong home for the mechanism, and that reads green forever. That is
the same residual `claim_cite_check.py` declares for citations.

## Controls

Seven, all mutating **in memory only** — never a scratch copy on disk, which would be Pattern 2
on the checker's own substrate. Six force a distinct violation shape; the seventh is the negative
control asserting the unmutated pair is green (without it, the six show only that the instrument
*can* fire, never that its firing carries information).

**Two are forced by a synthetic MANIFEST rather than by mutating a document** (`UNDECLARED
RESOLUTION` on a collision, and its spine-lag twin), because since the 2026-08-17 retirement
there is no declared entry left for a document edit to contradict. Dropping them when the
allowlists emptied would have retired two controls for a reason unrelated to whether the code
paths still work — an unexercised branch that reads green is the orphan shape this taxonomy names
(*a control must witness that it is CALLED*).

**Discrimination record — naturally arising, both sides, neither authored to be found.** Per
CLAUDE.md's *"When a defect is found, its before-commit is a free negative control"*, the git
history holds both. `--check` cannot be run at a historical commit (the manifest pins today's
indices, so any older commit reds for `MISSING INDEX` rather than for the collision), which is
why `--pairwise` — manifest-free agreement at an optional rev — is a **requirement, not a
nicety**:

```
$ python3 python/doc_pattern_check.py --pairwise 4f623017     # the defect's immediate PARENT
4f623017  shared=[1, 2]  DISAGREE=[]  <- DECLINES

$ python3 python/doc_pattern_check.py --pairwise 220739b8     # the commit that CREATED it
220739b8  shared=[1, 2, 3, 4]  DISAGREE=[3, 4]  <- FIRES
    idx 3: CM='destructive-replace-without-proof'  BD='bound-probe-bypasses-clause-order'
    idx 4: CM='recap-as-witness-substitution'      BD='fabricated-default'
```

The in-memory fixtures are the authored floor and are reported at that altitude. Note the
parent is `4f623017`, **not** `7af6b945` — `7af6b945` is the commit that created
`build_discipline.md` (three commits earlier) and also declines, but it is not the
before-commit.

## Provenance

Built 2026-08-14 under OQ-278 (`audits/2026-08-14_oq278_index_collision/`), whose
`PREREGISTRATION.md` carries the R1a/R1b/R2/R4 branch conditions the eventual index ruling is
made against. Separate from `python/claim_cite_check.py` by that audit's pre-registered
criterion (*distinct object AND no shared manifest ⇒ separate*): distinct object (index→name
agreement across documents vs a citation pinned to a claim row), no shared manifest, no shared
extraction.

---

# Sibling: `pattern_citation_check.py` — the displaced-consumer gate

Gate row `displaced cites`. Same OQ (OQ-278), different object: `doc_pattern_check` guards the
two **specifications** against each other; this guards the **citation record** against a member
whose index moved out from under it.

## Why it is an instrument and not a note

`build_discipline.md:1392` (*a correction landed in PROSE is not landed until every instrument
encoding the same assumption is checked*) and `:2558` (*a correction is not done until the old
value's consumers are swept*) have fired **three times on this one taxonomy**: the 2026-08-11
vacating that lived only in the paper for a day; that same ruling leaving `CLAUDE.md` publishing
the old six; and the nine stale citations the archaeology found. Operator, 2026-08-14: *three
instances of one failure mode wants an instrument, not a fourth note.*

## Declaration-based, not repair-based

**One manifest block per displaced member, carrying the STATE that says why its citations are
stale** — because the state decides the repair. `destructive-replace` was *vacated* (2026-08-11):
its citations get the surviving witness rule named, no index. `bound-probe` was *renumbered*
3 → 7 (2026-08-17): its citations get the new index. The second block was declared **before** the
renumbering landed — the 2026-08-11 vacating created nine stale pointers and nobody swept, and
renumbering without capturing the displaced member's consumers first would have committed that
same defect in the commit that closes the entry.

Declared per file, never per line — line numbers drift under ordinary editing and a line-keyed
manifest would go red on churn instead of on substance:

| verdict | meaning |
|---|---|
| `UNSWEPT CONSUMER` | a site cites a displaced member and is not in that member's manifest block |
| `DECLARED CONSUMER GONE` | a declared site stopped citing it — either the repair landed (retire the entry in the **same change**) or the detector stopped seeing it |
| `COUNT CHANGED` | the per-file count moved |

The phantom-entry control runs **once per displaced member**: a control that fires for one block
licenses nothing about the other's plumbing.

**A residual count after repair is expected and is declared residue, not backlog.** Not every
declared row is a repair target — discussion *of* the displacement is correct as written, and
point-in-time audit records are not retro-edited. The manifest deliberately declines to encode
which is which: a count is checkable, a disposition is a judgment, and the dispositions live in
`audits/2026-08-14_oq278_index_collision/WRITEUP.md` §4.

## It has already earned its keep, and the lesson generalizes

Built from the hand-adjudicated list in the audit's `WRITEUP.md` §4.6, its **first run disagreed
on 7 of 9 sites** — it could recover only 2. The hand list was right; the sweep's own regexes were
wrong: `faith merge` never matched `faith-merge` (3 sites), `old-vs-new diff` never matched
`Old-vs-new **output** diff` (1), and three phrasings were absent entirely.

**Under-recovery is silent — a missed match presents as `unrecoverable`, which reads like a
result rather than a miss** — so `LABEL_SET.tsv` had been shipping under-recovered rows to
OQ-294, the artifact whose whole purpose is to be *cleaned* ground truth. Neither the hand pass
nor the machine pass would have caught it alone; **the disagreement was the instrument.**

## Two traps this file fell into, both worth knowing before touching it

- **It read its own output.** `LABEL_SET.tsv` is committed, so `git grep` finds it and every row
  it already held became a candidate on the next run — the census compounded to 1421 rows against
  671 real candidates and would have grown at every run. **A producer that consumes its own
  artifact reports growth as discovery.** Guarded by `SELF_OUTPUT`; the witness is that
  consecutive `--sweep` runs are now byte-identical.
- **A regex literal containing `Pattern-3` made the checker cite itself.** Exactly the shape
  `claim_cite_check.py` documents (*"Documentation ABOUT a citation otherwise registers AS one —
  the same shape, three times now"*). Keep index literals out of the mechanism patterns.

## Performance

`git ls-files` + read-everything took **32s** — too slow to gate. `git grep -lIE` prefilters in C
and skips binaries itself, giving **~1s**. The `-I` flag also closes the decoded-binary hole
independently of the NUL check, so that hazard is now guarded twice.
