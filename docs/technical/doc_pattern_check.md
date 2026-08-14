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
DECLARED_COLLISIONS = {              # index -> (owning OQ, state)
    3: ("OQ-278", "ruled_pending_R1b"),
    4: ("OQ-278", "unruled"),
}
DECLARED_SPINE_LAG = {"missing_from_spine": [6], "owning_oq": "OQ-278", "reason": "..."}
# agreement is COMPUTED from the documents.
```

**The `state` field is load-bearing, not decoration.** Indices 3 and 4 are both owned by
OQ-278 but are *different states*: index 4 is an unruled collision with two live claimants;
index 3 is a **ruled** one — `CLAUDE.md` vacated it 2026-08-11 — with an unrepaired second site
still publishing `bound-probe`. Without the state, a future `UNDECLARED RESOLUTION` red is
uninterpretable without opening the OQ.

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
divergence. Checking its *index set* is what catches the live defect: the spine table publishes
1–5 and its prose says *"The five patterns"* while the headings publish 1–6. That omission is
**declared** (`DECLARED_SPINE_LAG`) rather than repaired, because editing the spine before
OQ-278's ruling destroys the evidence of what each document published when. When the repair
lands, the declaration disappearing goes red — symmetric with the collision allowlist.

## The silent edges (what the checker does NOT cover)

Enumerated rather than left implicit — each is a real index→pattern encoding this row does not
read, so a green `doc patterns` says nothing about them:

- `CLAUDE.md:158` — cardinality claim ("five live, index 3 vacated").
- `README.md:170` — cardinality claim ("the six defect patterns"), **already wrong** since the
  2026-08-11 vacating.
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

Six, all mutating **in memory only** — never a scratch copy on disk, which would be Pattern 2 on
the checker's own substrate. Five force a distinct violation shape; the sixth is the negative
control asserting the unmutated pair is green (without it, the five show only that the
instrument *can* fire, never that its firing carries information).

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
