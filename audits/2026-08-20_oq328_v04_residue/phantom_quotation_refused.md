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

The plan diagnoses item 31 as *"the walk's probe published a paraphrase false absence"* and calls
its own fourth finding *"item 31's error with the sign flipped: a paraphrase false presence."*
Observed: it is **item 31's error repeated, not flipped** — a third false absence, produced by the
same class of instrument (a line-scoped probe over wrapped prose), inside the plan written to
discharge item 31's class. `build_discipline.md` → *False-absence*: *"before any 'absent / can't /
no X,' owe a positive control."* The plan's finding carried none; its verification section already
contained the control it needed, four sections below, unapplied to itself.

**Consequence for Package C row 11 — the sign flips the other way.** The plan's row-11 hazard 1
says *"The collision may not exist at the wording anyone has read. Do not reproduce the phantom
quote."* The collision **does** exist at exactly that wording, so row 11's disambiguation is live
and quotes it as written.

**Prior art:** `build_discipline.md` → *Over-confident moves on the synthesis side* (1) False-absence
— grepped `False-absence`, `positive control`, hit at `build_discipline.md` (rule present, dated
2026-06-xx). This is a **RE-DISCOVERY** of that rule's sub-clause (c), the concept→surface mapping
claim, at a new surface: *the surface is right and the probe's line-scope is wrong*.
