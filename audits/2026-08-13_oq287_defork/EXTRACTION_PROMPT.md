# Receiver's prompt — extract the practice paper (OQ-287, Limb 1)

**Written 2026-08-14, to be executed later.** Written now rather than promised because the plan's
named risk was that the intermediate state becomes permanent: v0.6 is hollowed at §2, §2.8/§2.9 are
declared-temporary, and Limb 2 is dated 2026-09-14. A promise standing where the plan asked for an
artifact *is* that risk.

**Read this whole file before writing anything.** It enumerates concrete actions. If any instruction
below is correct in prose and wrong when executed, **say so and stop** — see *Your licence to refuse*.

---

## 0. GATES — check these first, in this order. Do not author past a red one.

| # | gate | how to check | if red |
|---|---|---|---|
| G1 | **OQ-278's P4 index ruling has landed** | `grep -n 'OQ-278' ISSUES.md` and read its Status line; the ruling is landed only when `CLAUDE.md` and `docs/technical/build_discipline.md` **agree** on what index 4 names | **STOP. Do not author section III.** Sections I, II, IV, V may proceed. Authoring a fresh failure-taxonomy section into a third document while the two documents disagree at index 4 propagates the collision — and OQ-278's own text records that *"splitting one question across three entries is how the index collision happened in the first place."* |
| G2 | repo gate is green | `./scripts/gate.sh` | fix or report before starting |
| G3 | citation apparatus is live | `python3 python/claim_cite_check.py --check && python3 python/claim_cite_check.py --selftest` | fix before writing a single `CWC:` pin |
| G4 | v0.6's structure is as this prompt assumes | `audits/2026-08-13_oq287_defork/checks.sh all` | the salvage map below is stale; re-derive before trusting it |

**G1 is a gate you check, not context you have been given.** "Be aware of OQ-278" would be a pointer
wearing an instruction's clothes. Run the grep, read the status, decide.

---

## 1. Subject — what this paper is

**The discipline documents as a development practice.** `build_discipline.md`, `design_discipline.md`,
`ISSUES.md`, `KNOWN_STATE.md`, `CLAUDE.md` — described as a working practice for research whose
workers do not persist, and **machine-enforced rather than described**: gate checks each carrying
their own selftest, a spec/code enum pin, a dangling-deps resolver, a writeup-header gate, a memory
channel cap, and (since 2026-08-13) a claim-citation digest checker. A reader can clone the repo and
watch it go red.

**The gap it addresses** is between that and what is discussed as "prompt engineering" or as the
frontier of AI research. Much of what happens in this repository is not software development.

**What it is NOT.** Not a failure taxonomy — that is the weakest part, the part Wu (2026) owns, and
the part that keeps attracting the duplication objection. Not a manual. Not "this worked for us,"
which is unfalsifiable and uninteresting.

**Standing hazard, which has fired four times on the parent document: GENRE DRIFT.** Each recast was
argued as though the document were one kind of thing. If a reviewer says *"isn't this just X"* — run
the comparison, then decide. A suspicion is not a run.

## 1b. Where it goes, and what you may touch

**Destination:** `docs/practice/practice_paper_v0_1.md`, plus `docs/practice/README.md` naming it
canonical — the same convention as `docs/concealment/README.md` and
`docs/amnesiac_institution/README.md`. Canonicity is a checked fact here, not a memory; a new
directory without a README repeats the defect OQ-287 closed.

**Files you may edit:** the new `docs/practice/` directory; `ISSUES.md` (OQ-287's limbs);
`KNOWN_STATE.md`; your own `audits/<date>_<slug>/`.

**`amnesiac_institution_v0_6.md` admits POINTER-ONLY edits** — forward pointers and redirect notes,
no content edits. That is a declared exception to its freeze, with its bound stated (KNOWN_STATE
2026-08-14). `concealment_without_a_concealer_v0_4.md`: do not edit at all for this task; if the
extraction seems to need an upstream change, that is a ruling to route back, not a change to make.

**One writer at a time.** If you hold `file:line` pins from this prompt or from
`A3_MAPPING_RULE.md` while another instance edits those files, your pins shift silently — you will
locate by content and record a correction with two line numbers, which is a fabricated provenance
note about churn that never happened. Re-derive anchors at the moment you use them.

## 2. Outline

- **I** — practice, not prompt engineering
- **II** — the documents as instruments
- **III** — the failure taxonomy the practice answers to · **GATED ON G1**
- **IV** — the recursion: the practice failing on itself
- **V** — self-instrumentation and honest limits

## 3. Salvage map — what moves, from where

| from v0.6 | to | note |
|---|---|---|
| §2.8 (unmarked perturbation; the trifurcation→repair table) | **III** | canonical destination per the A2-pre ruling 2026-08-13 |
| §2.9 (the negative control: (a) three exclusions, (b) the break at the instrument stratum) | **V** | same ruling |
| §7 (witness calculus), §7.3 (positive controls), §7.5, §7.6, §7.7 | II and V | |
| §7.4 / §7.4.1 (the recursion; nine defects, none caught by a gate) | **IV** | |
| §8 (memory economy), §9 (organizational form), §10 (self-instrumentation) | II and V | |

**v0.6's §2.8/§2.9 are the SUPERSEDED side once this lands.** They stay at their numbers there and
gain forward pointers; they are not deleted.

## 4. The redirect table — Limb 2, and it is owed at SUB-ITEM granularity

`audits/2026-08-10_oq277_rq2_crosscoding/LETTER_2026-08-11_wu.md:15` cites **`§2.9(b)`** and **has
been sent to another author. It cannot be edited.** A section-level redirect does not discharge this.

Author this table in the practice paper and add the reciprocal forward pointers in v0.6:

| v0.6 anchor | practice-paper anchor |
|---|---|
| §2.8 — "reading an abstraction at a framing other than its own is an unmarked perturbation" | III, § |
| §2.8 — "a witness is that perturbation performed deliberately at the boundary" | III, § |
| §2.8 — "a positive control is the same move run on the probe" | III, § |
| §2.8 — "the recursion is the principle's reflexive axis" | IV, § |
| §2.8 — the trifurcation → repair table (Types A/B/C) | III, § |
| **§2.9(a)** — the three exclusions (structural contradiction · stochastic churn · loud destructive replacement) | V, § |
| **§2.9(b)** — the break: at the instrument stratum, carriage does not repair | V, § |

**If this table is not authored, OQ-287 does not close** and Limb 2's review date (2026-09-14)
governs instead.

## 5. Acceptance condition — pre-registered, operator-stated, WITH ITS TEST

> The extraction is done when **the unmarked perturbation (§2.8) and the negative control (§2.9) are
> LOAD-BEARING in III and V** — not appended. They are the guard against the practice paper degrading
> into a manual or into "this worked for us."

**THE TEST, because "load-bearing" is otherwise satisfied by placing them well:**

> **Delete §2.8's and §2.9's material from your draft. Do III and V still stand?**
> **If yes, they are appended and the acceptance condition is NOT met.**

Run it as a real deletion on a scratch copy, not as a thought experiment. What should break: III's
account of *why* a witness works (it is a deliberate perturbation) and V's honest limit (the account
excludes real defect classes, and carriage is not sufficient at the instrument stratum). If III
survives as a list of practices and V as a caveats section, the material is decoration.

**If they cannot be made load-bearing, that is the signal to revisit Subject BEFORE authoring, not
after** — and Subject 3 (the recursion) becomes competitive again.

## 6. Section IV — candidate material, with a scope bound

`audits/2026-08-13_oq287_defork/WRITEUP.md` finding 4 records **twelve false absences produced by the
OQ-287 pass's own instruments**, ten of which the red light *discovered* rather than confirmed, five
of them committed inside instruments built to catch the earlier ones. Finding 5 records the first
live digest fire: one row's meaning changed, six citations fired, all six were re-read and one
improved, and **fifteen other digests recomputed identical** — the discrimination being the pair,
since a scheme that fires on everything is indistinguishable from one that fires on nothing.

**This is CANDIDATE material for IV. It is not a commitment, and the distinction is load-bearing.**

It is the strongest evidence available that §2.9 can be load-bearing rather than appended. It is also
exactly the material that could pull this paper back toward **the recursion** — which was Subject 3,
the option deliberately not taken. **v0.2's genre recast happened this way**: strong material argued
the document into being a different kind of thing, one section at a time.

**Scope bound — hold all four:**

1. IV is *the practice failing on itself*, one section of a practice paper. If IV grows past roughly
   its share of the paper, or if I/II start serving it, the genre has drifted — stop and say so.
2. The count is **a floor, not a measurement**: self-observed, same party, same pass, **no
   denominator**. Cite as *"at least twelve, self-observed, undenominated."* **Never as a rate, and
   never against §7.4's nine**, which were counted differently.
3. The claim it supports is narrow: *declaring a framing produces a new artifact with a new framing;
   the remedy is not self-terminating.* It does **not** support "verification is futile."
4. What terminated all twelve is the finding, and it is not an instrument: **not one was caught by a
   gate reading its own output green; ten were caught by a party comparing a claimed value against
   the artifact it described.** Report that as §7.4.1's finding **independently re-derived** — fresh
   arc, different route, prospective, with an adversarial second party the earlier arc did not have.

## 7. Citation discipline

- The derivation is **cited, never restated.** `concealment_without_a_concealer_v0_4.md` is canonical
  for it (`docs/concealment/README.md`). This paper cites it; it never cites this paper for the
  derivation. **No second copy may exist** — that is what OQ-287 closed.
- Every claim citation carries a content digest: `CWC:A2@31548228`. Get digests from
  `audits/2026-08-13_oq287_defork/claim_digest.sh <label>` — **never** reimplement the recipe, and
  never hand-copy a digest from a document, including this one (the pins here are as-of 2026-08-14).
- The digest covers the **whole Appendix A row, kill condition included.** A row edit fires every
  citing site. **On a fire: re-read the site and decide. Never bump the hex.**
- A new citation is **unguarded until its digest lands in the same change** — the declared opt-in
  hazard. Run `claim_cite_check --check` before committing.
- Concealment §5.1, §5.4, §9.1 and §3.2 have **no Appendix A row** and cannot be pinned; write them
  `` `CWC` §5.4 ``. **Do not mint Appendix A rows to make your citations checkable** — that is the
  instrument reshaping the substrate to fit itself.
- Quoting v0.6 material that moved: it is the superseded side, so quote from your own paper and
  redirect (§4), do not cite v0.6 as authority for it.

## 8. What is UNGUARDED — carry these forward, none is checkable

**R1.** `COVERAGE_DIFF.md`'s coverage calls decided which concealment claim each vacated v0.6 unit
maps to, by hand, pre-`C1`. A dated re-check ran 2026-08-14. Carried verbatim, because the re-check
otherwise reads as validation of the whole table:

> The re-check verifies the anchors, not the coverage calls — a row marked COVERED in A0 that was wrong then is still wrong, and nothing in this pass would catch it.

**R2. Aptness is not merely unguarded — it is ANTI-GUARDED, and this is a hazard, not a caveat.**

`claim_cite_check` verifies that a pin matches its row. It cannot verify that the row is the **right**
one to cite at that site. A citation aimed at `A2` where the argument needs `A4` reads green forever
and stays green through every future narrowing of either row.

**Here is why that is worse than having no instrument.** When you run the gate you will see something
like *"claim_cite_check: 61 live citation(s)"* and a green tick. **That green reads as verification of
the citation set, and it is not.** It verifies one relation — pin-matches-row — and is silent on the
one that carries the meaning. A receiver who trusts it will check *less* than a receiver facing no
instrument at all. That is a success-shaped token filling the hole where aptness review would go: this
paper's own signature, produced by this paper's own apparatus, on the surface the apparatus cannot
see. **Expect it, and do not let the green stand in for the reading.**

Its first real test was the `§2.3` split: v0.6 merged two claims concealment keeps apart — **`A3` is
about what a PROCEDURE can do (analytic); `E1` is about what PARTIES do (empirical)** — split 3/3 by
site in `A3_MAPPING_RULE.md` §4. Two of the three `E1` sites turned out to be **vocabulary borrows,
not assertions**, and say so in their own prose, because `E1` is unevenly supported and its owed
Prediction 1 was run 2026-08-13 and **withdrawn as a test of it**. Every one of those calls was made
by hand and none is machine-checkable.

**REQUIRED PRACTICE, because the aptness check that does not exist is cheap to approximate.** For
**every new citation** in the practice paper, write a one-line note recording *which claim it leans
on, and why that row rather than a sibling* — the same discipline applied by hand to the six `§2.3`
sites. Keep them together (a table in your audit dir, or footnotes in the draft).

It will not be machine-checkable. It makes aptness **reviewable**, which is what `COVERAGE_DIFF.md`
was supposed to do and now cannot, being pre-`C1` and unverified by its own statement. And it is the
shape §7.4.1 keeps pointing at: **not a better instrument — a second party comparing a claim against
the artifact.** A citation without such a note is unreviewed, whatever the gate says.

**R3.** Finding 4's count is a floor (see §6.2 above).

**R5.** Section-only citations are unpinnable by construction; counted via
`claim_cite_check --list --unpinnable`, never checked. If that count grows without anyone deciding it
should, the scheme is eroding at its edge while every gate stays green.

## 9. Your licence to refuse — stated, because an unstated licence is not exercised

**If an instruction in this file is correct in prose and wrong when executed, say so rather than
comply.** "The prompt said to" is not a witness, and a compliant receiver's output looks exactly like
work while carrying this author's error into the substrate.

Report the refusal **at the volume of a completion** and route it back — do not silently repair the
instruction, because that leaves the defect in the design, where it fires on the next receiver.

Scope is narrow: *executed as written, this produces what the design forbids.* Not "hard," "unclear,"
or "I would do it differently" — those are the one-sentence flag, which proceeds.

**Your ignorance is load-bearing here.** I hold context you do not, and I supplied missing halves
silently while writing this. You cannot, which is why you will find the gaps I could not.

Five refusals were recorded in the OQ-277 arc; none was caught by a sender re-reading.

## 10. Definition of done

1. G1 checked and recorded; III authored only if green.
2. I–V drafted; the §5 deletion test **run on a scratch copy** and its result recorded.
3. §4's redirect table authored at sub-item granularity, with reciprocal forward pointers in v0.6
   §2.8/§2.9 — **pointer-only edits, which is all v0.6 admits.**
4. `claim_cite_check --check` green; every new citation digest-pinned in the same commit; **and
   every new citation carries its one-line aptness note (§8, R2) — the green tick does not discharge
   this and will read as though it does.**
5. `audits/<date>_<slug>/WRITEUP.md` with its `**Fired:**` bit.
6. ISSUES OQ-287: Limb 1 and Limb 2 closed, or the residue named. `mitigated → resolved` only when
   **both** limbs are discharged.
7. `omega_resolver index` regenerated; `./scripts/gate.sh` GREEN.

---

*Pins in this file are as-of 2026-08-14 and are illustrative. Re-derive every digest with
`claim_digest.sh` before writing it into a document.*
