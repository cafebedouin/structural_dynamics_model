# Package S's "phantom quotation" is not a phantom — the plan's probe was line-based against a hard-wrapped file

**Executed:** 2026-08-20. **Refusal**, per `build_discipline.md` → *The receiver's license to
refuse*: the instruction is correct in prose and wrong when executed.

## The instruction

The plan's *Fourth finding: a phantom quotation, carried at two sites* states:

> `crosswalk_v04_to_v06.md:69` and `ISSUES.md:16121` both attribute to §5.4, **in quotation marks**,
> the phrase *"the losses are independent."* It returns **0 hits** file-wide.

and Package S directs: *"Fix the phantom quotation at `crosswalk_v04_to_v06.md:69` and
`ISSUES.md:16121`."*

## Observed

The phrase is PRESENT in §5.4, split across a hard wrap at `:1357-1358`, inside a blockquote:

```
1357:> extraction. Each of those three choices has since been shown to lose incidents, and the losses are
1358:> independent:
```

Line-based grep is blind to it and returns 0; the plan's OWN verification step 2 — the wrap-safe
normaliser, prescribed precisely because *"the file is hard-wrapped AND blockquoted"* — finds 1.

```
$ /usr/bin/grep -c "the losses are independent" docs/amnesiac_institution/amnesiac_institution_v0_6.md
0

$ .venv/bin/python -c "import re,pathlib; t=re.sub(r'\s+',' ',re.sub(r'(?m)^[ \t]*>[ \t]?','',pathlib.Path('docs/amnesiac_institution/amnesiac_institution_v0_6.md').read_text())); print(t.count('the losses are independent'))"
1
```

## Two-sided control on the probe used to refuse

The wrap-safe probe fires on two phrases known present and declines on two constructed near-misses
over the same path — so its `1` is not a probe that fires on anything:

| probe string | count |
|---|---|
| `the losses are independent` | **1** (fires) |
| `a floor, and it should be reported at that altitude` | **1** (fires — §7.3, known present) |
| `the losses are uncorrelated` | **0** (declines) |
| `the gains are independent` | **0** (declines) |

Decline grade: **authored decoy** — a floor, reported at that altitude. The plant/decline pair
establishes the probe is bidirectional on this path, nothing wider.

## Verdict

Both citations are **correct**. `crosswalk_v04_to_v06.md:69` and `ISSUES.md:16142` (the plan's
`:16121` is the OQ-328 entry head; the quote is at `:16142`) quote §5.4 accurately. Package S's
correction is **refused** — executing it would have amended two accurate citations into inaccurate
ones and dated the corruption.

## The finding about the plan

The plan calls its own fourth finding *"item 31's error with the sign flipped: a paraphrase false
presence."* Neither half of that is right, and the project's record already carries the correct
name for what it is.

Per KNOWN_STATE 2026-08-20 (*a PARAPHRASE false absence is a distinct species from the wrap-trap
class, and no normaliser fixes it*), false absences on this document sort into two species:

| species | mechanism | does a normaliser fix it? | instances |
|---|---|---|---|
| **storage-form (wrap-trap)** | hard wrap, blockquote markers split the phrase | **yes** | 1–5, and **this is 7** |
| **paraphrase** | the claim is made in other words than the probe's | **no** — it returns the same 0 while raising confidence in it | item 31 |

So this is **wrap-trap instance 7** — the sixth of the *storage-form* species (item 31's paraphrase
instance already holds the number 6 in the ledger) — the species the normaliser *does* close, missed
because the probe never ran the normaliser. It is not item 31's error and not its inverse; it is the older,
already-named, already-solved species, recurring in the plan written to discharge item 31.

That is a sharper indictment than the plan's own framing, because the fix existed and was in the
same document: the plan's verification section prescribes the wrap-safe normaliser explicitly, on
the stated grounds that *"the file is hard-wrapped AND blockquoted"*, four sections below the
finding that needed it.

**Scope rule that applies here, from the same KNOWN_STATE entry:** *an absence verdict licensed by
keyword hits alone is scoped to the keywords, and the keywords are the author's, not the
document's.* The plan's `0 hits` was scoped to a line-oriented match, which is narrower still.

**Consequence for Package C row 11 — the sign flips the other way.** The plan's row-11 hazard 1
says *"The collision may not exist at the wording anyone has read. Do not reproduce the phantom
quote."* The collision **does** exist at exactly that wording, so row 11's disambiguation is live
and quotes it as written.

**Prior art:** grepped `build_discipline.md` for `false absence`, `wrap`, `normaliser`, and
KNOWN_STATE for the same. Hit: *A textual probe's zero is a fact about the probe*
(`build_discipline.md`, with its disjoint-fix table), and KNOWN_STATE 2026-08-20's correction-key
entry naming the two species. **This is a RE-DISCOVERY**, and specifically of the *storage-form*
half — the half that already has a mechanical fix. Recording it as a novel finding would have been
the second error.
