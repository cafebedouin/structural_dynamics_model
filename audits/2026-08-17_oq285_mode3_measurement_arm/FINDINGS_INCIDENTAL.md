# Incidental findings from the OQ-285 review — minting proposal

**Executed:** 2026-08-17 · **Parent:** `WRITEUP.md` (OQ-285) · **Status:** proposal only, nothing
minted. Next free OQ label is **OQ-296** (OQ-295 was claimed by the surfacing entry, commit
`2caaf77b`; the authorising plan's "next free OQ-295" is stale).

These are outside OQ-285's scope. They were found while reading the seat/H¹ machinery for it, so
they are filed here rather than lost — but **items 1 and 2 are wrong output shipping today** and
their correctness does not depend on anything OQ-285 resolves. Burying them behind a Claude-web
check would delay a live defect behind an open question, so they go to the operator directly.

Each item states its verification status honestly: **verified** (run this session, witness in this
directory), **verified statically** (parsed from source with a transcription control), or **latent**
(real defect, zero live fires on the leg checked).

---

## 1. `descent_status/2` reports `descends(unknown)` on 12 live constraints — SHIPPING NOW · **verified**

`grothendieck_cohomology.pl:252-254`:

```prolog
descent_status(C, descends(Type)) :-
    cohomological_obstruction(C, 1, 0),
    orbit_vector(C, [Type|_]), !.
```

It takes the **first element** of the orbit vector with **no `is_real_type/1` guard**, while
`H0`/`H1` were computed over real seats only. So an orbit vector `[unknown, snare, snare, snare]`
yields `H0=1, H1=0, descends(unknown)` — a *global-section* claim naming the absence token as the
type. This contradicts the file's own OQ-51 rule, stated 33 lines above at `:215-219`.

`fails_descent/2` has the dual: it sorts the **full** vector, so `unknown` enters `UniqueTypes`.

```
descent_status descends(unknown):                 12
descent_status descends(_) total:                 70
fails_descent with unknown in UniqueTypes:        12
```

(`swipl -g "[stack], corpus_loader:load_all_testsets, ..."` on `testsets/`, 273 files loaded.)

**12 of 70 descent claims — 17% — name `unknown` as the descended type.** Fix is a one-line
`is_real_type/1` guard plus an explicit token for the mixed case; it is an **output-changing**
change, so it lands on its own commit with a before/after diff.

## 2. `effective_immutability/3` silent table hole — CORPUS-WIDE · **verified statically, all five legs**

`constraint_indexing.pl:195-227` has no row for
`{immediate, biographical, generational} × analytical`. Only `historical` has a catch-all
(`effective_immutability(historical, _, rope)`) and only `civilizational` has an explicit
`analytical` row. `exit_modulation(analytical, 0.00)` exists (`:495`), so the *directionality* path
is fine — it is only the perception table that has the hole.

`seat_perceived_vs_real/4:161-165` reads the missing row through an if-then-else:

```prolog
(   constraint_indexing:effective_immutability(T, E, mountain)
->  Perceived = immutable
;   Perceived = changeable          % <- a MISSING ROW lands here
)
```

**A table gap becomes a substantive perceptual claim.** `item2_immutability_hole_cross_leg.txt`
(transcription control included — the checker must accept `generational/constrained` and reject
`generational/analytical`):

```
leg                              seats   agent    hole   hole%
testsets                          1545    1355     198    14.6
testsets_haiku                    3774    3186     570    17.9
testsets_flash                    4144    3802     452    11.9
testsets_kimi                     5252    4549     551    12.1
testsets_sonnet                   7331    6522     663    10.2
archives/datasets/kernel_v1          0       0       0       -
```

**~1 in 8 agent seats corpus-wide — 2,434 across the five live legs — is told "changeable" by a
missing table row.** And per `WRITEUP.md` §3, hole seats abstain at 22.8% versus 9.5% elsewhere, so
44 of the 152 live `testsets/` unknowns (28.9%) trace to it. Ranks with item 1 for attention;
unlike item 1 the fix needs a **ruling**, because whether `analytical × immediate` *should* read
`mountain` or `rope` is a design call, not a typo.

## 3. `is_real_type/1` is a blacklist of one · **verified by inspection**

`grothendieck_cohomology.pl:219`: `is_real_type(T) :- T \== unknown.` So `untyped`, `null`, and an
**unbound variable** all pass as REAL. The `untyped` footgun is documented (CLAUDE.md, OQ-217); the
`var`/`null` case is not. A whitelist over the authored type vocabulary would fail closed.

## 4. No dedup on the seat vector · **latent (0 live)**

`stakeholder_agent_seats/2` (`stakeholder_seats.pl:179-182`) uses `findall` with no `sort`, and
`dr_type_for_stakeholder/3` re-queries the fact table twice (`:120-124`), so *k* duplicate
`constraint_stakeholder/7` facts for one seat give up to *k²* solutions and double-counted H¹ pairs.

Measured: **0 distinct `(cid, seat)` pairs are authored more than once on `testsets/`**, so this
does not fire today. It is one duplicated authored fact away from silently inflating H¹.

## 5. Frame fork between the two seat consumers · **verified by inspection**

`report_generator.pl:279-283` (`seat_type_reading/3`, `stakeholder` source) enumerates **every**
`constraint_stakeholder/7` — no `Role \= excluded` filter, no `stakeholder_non_agent` check —
while `stakeholder_seats.pl:179-182` excludes both. The Pattern-2 "cannot fork" comment at
`stakeholder_seats.pl:176-178` is accurate for the three `stakeholder_seats` consumers and does
**not** cover `report_generator`. Two live definitions of "the seats", 1333 vs 1545 on `testsets/`.

## 6. `plural(RealTypes)` carries no absence marker · **verified by inspection**

`stakeholder_seats.pl:196-203` and `commentary_census.pl:132`
(`consensus_bucket(plural(_), plural)`) flatten the verdict to a bare `plural`, so a constraint with
5 of 7 seats `unknown` is indistinguishable at the census from one with 0. This is *deliberate* for
the verdict itself (the header at `:195-203` argues plurality is existential and unknowns cannot
undermine it) — the finding is about the **census bucket**, which drops the `NReal`/`NSeats`
coverage that the verdict deliberately kept in-band.

## 7. Comment pin drift · **verified**

`commentary_census.pl:163` cites `stakeholder_seats.pl:182` for the text *"out-of-domain (no seats
to compare)"*. That text is now at `stakeholder_seats.pl:252`. One-line fix.

## 8. Housekeeping — stale worktree · **verified**

`.claude/worktrees/oq-48-recalibration/` still exists and holds a divergent copy of
`test_seat_totality.pl`. Machine-local and gitignored, so not a repository fork — but it is the
Pattern-2 shape, and a future instance grepping the tree will find two versions of that test.

---

## Suggested disposition

| item | kind | needs a ruling? | proposed home |
|---|---|---|---|
| 1 | live wrong output | no — the OQ-51 rule already settles it | fix + output-changing commit; note in `KNOWN_STATE.md` |
| 2 | live wrong output | **yes** — `analytical × short-horizon` semantics | OQ-296 |
| 3 | latent footgun | no | fold into the item-1 fix or `KNOWN_STATE.md` tripwire |
| 4 | latent | no | comment at `stakeholder_seats.pl:179` + `sort/2` |
| 5 | live fork | no | OQ-296, or a `no-fork` bridge like the existing one |
| 6 | live coverage loss | no | `commentary_census.pl` bucket carries `NReal/NSeats` |
| 7 | doc drift | no | fix on sight |
| 8 | housekeeping | no | remove the worktree |

Items 1, 3, 4, 6, 7 are inside the *Fix simple errors* threshold. **None was fixed in this
session**, because OQ-285's gate says "No code" and items 1 and 5 are output-changing — the
operator's call on whether the no-code gate is scoped to OQ-285's own surface or to the whole visit.
