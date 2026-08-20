# Held Constant: A Development Practice for Research Whose Workers Do Not Persist

**cafebedouin@gmail.com**

*Status: DRAFT v0.1 (2026-08-20). Extracted per ISSUES OQ-287, Limb 1. This paper is canonical for
the practice; `../concealment/concealment_without_a_concealer_v0_4.md` is canonical for the
derivation it rests on, and `../amnesiac_institution/amnesiac_institution_v0_6.md` is canonical for
the institution it is practised in. It restates neither. Two subsections moved here from v0.6 — its
§2.8 becomes §III's spine and its §2.9 becomes §V's — and v0.6 now carries forward pointers at both;
everything else this paper draws on remains v0.6's and is cited as authority.*

> **§0. WHAT KIND OF CLAIMS THESE ARE.** Each claim below is typed, and the type governs how it may
> be discharged. **ANALYTIC** — follows from the derivation; a counterexample is a defeater.
> **EMPIRICAL** — an observation in this repository's record; single-site, self-observed, and
> undenominated unless a denominator is stated. **METHODOLOGICAL** — a rule this project adopted;
> its warrant is that it changed what got caught, and its falsifier is that it stops doing so.
> **OPEN** — stated with its kill condition and not yet settled.
>
> The typing is not decoration. This paper's subject is a practice for producing claims whose
> support is not visible in their form, written by workers whose fluency is uncorrelated with their
> reliability. A paper about that which did not type its own claims would be an instance of its
> subject.

---

## I. Practice, not prompt engineering

### I.1 The gap this addresses

There is a large and growing literature on how to talk to language models, and a large and growing
literature on what language models are. Between them sits an activity with almost no literature at
all: **doing sustained research with them, over months, on a body of work too large to hold in one
context, where the substrate of the work is a repository rather than a conversation.**

The framing this activity usually receives is *prompt engineering* — how to phrase a request. That
framing has an implicit model of the problem: the worker is capable, the instruction is the variable,
and better instructions produce better work. It is the right framing for a task that fits in one
exchange.

It is the wrong framing for the thing described here, and the wrongness is structural rather than a
matter of degree. **Much of what happens in this repository is not software development, and almost
none of it is prompting.** It is: deciding what counts as evidence; noticing that a document and the
code it describes have quietly diverged; ruling on a question that has no fact of the matter;
recognising that a number cited three times has never been re-derived since the run that produced it;
and — most of the working hours — building instruments whose job is to make a specific silent failure
loud.

> **METHODOLOGICAL.** The unit of work is not the exchange. It is the **artifact left in the
> substrate**, evaluated by whether a worker who was not present can act on it correctly.

### I.2 Why the workforce forces the method

The properties that matter are not the impressive ones. Following v0.6 §3.1, the load-bearing
properties of an LLM instance *as employed here* are:

- **Fluent** — output is well-formed and idiomatic at every quality level, so **form does not signal
  reliability.** This is the property that breaks the ordinary review reflex, which is calibrated on
  humans, for whom sloppy form is weak evidence of sloppy thought.
- **Cheap** — the marginal cost of another analysis, another document, another instrument is near
  zero. A hazard before it is a benefit (§V.1).
- **Non-persistent** — nothing survives the session except what was written down.
- **Directionally fallible** — and the direction that matters is **success-shaped absence**:
  reporting the *form* of a completed task whether or not the substance occurred.
- **Recursively applicable** — the same kind of worker that makes these errors can be asked to check
  for them, which helps, and which means every check inherits the checker's failure modes (§IV).

Take those five together and one conclusion is forced. **A practice that relies on the worker
noticing its own failure cannot work here**, because the characteristic failure is one that produces
a confident, well-formed report of success. Nor can a practice that relies on a reviewer's
impression, since form carries no signal. What is left is a practice that relies on **artifacts that
fail loudly**, and the whole of §II is the consequence.

### I.3 What this paper is not

**Not a manual.** No step list is given, because a step list would be the wrong genre for the same
reason prompt engineering is: it presumes the difficulty is in execution.

**Not "this worked for us."** That claim is unfalsifiable and uninteresting. Where this paper reports
that something worked, it reports what was measured, on what population, and what would show it
false — and where nothing was measured, it says so (§V).

**Not a failure taxonomy.** A taxonomy of ways things go wrong is the weakest available contribution
here: it is the part with the most external competition, and it is the part that most invites the
objection *"isn't this just X."* §III uses a taxonomy but does not offer one — the taxonomy is
downstream of an account, and the account is what carries the content.

> **A standing hazard, recorded because it has fired four times on the parent document: GENRE
> DRIFT.** Each recast was argued as if the document were one kind of thing, by strong material
> pulling one section at a time. The behavioural rule is the same one §II.3 states about code: **a
> suspicion is not a run.** If a reviewer says *"isn't this just X"* — run the comparison, then
> decide. Doing that once produced a better artifact than any of the four recasts (§IV.4).

---

## II. The documents as instruments

### II.1 Five records, differentiated by retention

The practice is carried by five documents, and the interesting fact about them is not their contents
but that **they have different retention policies, deliberately.** v0.6 §3.4 is canonical for the
full table; what matters here is the design principle behind it.

| record | what it holds | retention |
|---|---|---|
| `CLAUDE.md` | rules loaded into **every** session | permanent, capped, and paid for every session forever |
| `build_discipline.md` / `design_discipline.md` | the mechanism behind each rule; how we build, and what the engine is for | permanent, uncapped, consulted on demand |
| `ISSUES.md` | open questions, with status, evidence, and what resolution would change | until closed, then compressed |
| `KNOWN_STATE.md` | the dated session log | ~30 days at full text, then compressed in place |
| the repository itself | code, corpora, audits | permanent |

**A worker arrives holding only the first.** Everything else must be *reached* — which makes the
question of what goes where an economic one, not an editorial one.

### II.2 The promotion test, and what it prices

An always-loaded rule costs tokens in every session forever. The test that governs entry (v0.6 §8.2)
is deliberately narrow:

> Promote an item into always-loaded context only if a fresh instance that never read it would make a
> concrete, **silent** mistake before touching the files it names.

Three things are doing work in that sentence. **"Fresh"** — the test is about a worker with no
history, which is the actual worker. **"Silent"** — a failure that announces itself needs no standing
warning, because the error message is the warning; loud failures are never promoted, and this is what
keeps the channel from filling with everything that ever went wrong. **"Before touching the files it
names"** — the rule must be *reachable* at the moment of the mistake, which is a routing property,
not a truth property.

> **METHODOLOGICAL, with two declared gaps (v0.6 §8.2, cited not restated).** The test weights
> frequency and not severity — a rare, expensive silent failure can fail it — and it is evaluated by
> judgement rather than by simulation. The second gap was partly filled on 2026-08-18 by a
> pre-registered draw in which an instance given the files *without* a tripwire emitted the defective
> goal and an instance given them *with* it did not: **one draw per arm, an existence witness that
> the test is runnable, and no rate.**

The dual matters as much as the rule. **A rule whose premise expires stays internally correct**, and
therefore invisible: the worktree rule outlived multi-instance operation and sat true-but-pointless
until a session tripped over it. So the monthly pass asks of every always-loaded rule not "is this
still true?" but **"does its reason still match how the work is done?"** — promotion adds, this
removes, and only having both keeps the channel honest.

### II.3 Canonicity as a checked fact

The most frequently repeated defect on a document substrate is not error but **forking**: a file is
copied, both copies are edited, and nothing anywhere says which is canonical. The rule is one
sentence — *one canonical location per thing, and canonicity must be a checked fact, not a memory* —
and the operative half is the second.

This paper is an instance of enforcing it. The derivation it rests on lived in two papers at once; the
resolution was not to pick one and remember, but to make the pick **machine-checked**: each directory
carries a README naming its canonical file, cross-document claims are cited as `CWC:<label>@<digest>`
where the digest covers the whole source row, and `claim_cite_check` in the standing gate resolves
every pin. Editing a source row moves its digest and **fires every citing site** — which is the
mechanism working, not a false alarm.

> **The instrument's own blind spot, stated at the same volume as its value (ANALYTIC).** The checker
> verifies that a pin matches its row. It cannot verify that the row is the **right** one to cite. A
> citation aimed at `A2` where the argument needs `A4` reads green forever, and stays green through
> every future narrowing of either row. **This is worse than having no instrument**, because a green
> tick reading *"64 live citations"* is a success-shaped token occupying the place where aptness
> review would go, and a reader who trusts it checks *less* than one facing no instrument at all.
> The mitigation is not a better checker: it is a hand-written one-line note per citation saying
> which claim it leans on and why that row rather than a sibling
> (`../../audits/2026-08-20_oq287_limb1_extraction/APTNESS.md`). Unmachine-checkable, and *reviewable*,
> which is the most that is available.

### II.4 Typed openness

An open question is not a defect and is not a to-do. The practice types them, and the types carry
different obligations: what is settleable by evidence, what is settleable only by a ruling, and what
is settleable by neither. A question routed to the wrong type is how a value call gets self-certified
as a finding, or a checkable fact gets deferred forever as a matter of taste.

The rule with teeth is **declared absence**: a capability the system deliberately does not have gets a
ledger entry, so that an empty placeholder is never mistaken for a working feature. A defect that
reads as working is tracked as a defect; an **absence the design admits** is tracked as a gap. Keeping
them in one list would make the second invisible, because absences do not generate symptoms.

---

## III. The failure taxonomy the practice answers to

> **The material in this section moved here from `amnesiac_institution_v0_6.md` §2.8**, which is now
> the superseded side and carries a forward pointer. It is cited there and developed here.

### III.1 The one move

Every instrument in this practice is the same move, and the taxonomy below is a classification of
what happens when that move is *not* made. The move, stated in its general form:

> **Hold everything fixed but one dimension; vary that dimension; read off what stays and what
> moves.**

And the relation that makes it a taxonomy rather than a technique:

> **A failure of type X is what happens when axis X varied without your holding it — the *unmarked*
> perturbation. The method is the same perturbation run on purpose.**

That sentence is the load of this section. It says the failures and the practices are **the same
phenomenon under opposite intent**, and everything below is its consequence. A taxonomy of failures,
by itself, tells you what to fear. This tells you what to *do*, and why that particular thing —
because the practice is not a list of habits that happen to help, it is the enumeration of axes, and
for each axis the operation that holds it.

### III.2 The three axes, and the repairs they license

The generative cut is by **which axis moved unheld**, because that is what determines the repair.
Classifying by symptom instead would collapse the three, since all three present identically at the
read site: something looks fine and is not.

| axis that varied unheld | how it shows up here | the repair it licenses | in framing terms |
|---|---|---|---|
| **Time** (drift) | spec-and-code silently diverge; "the corpus" cited with no date | as-of stamps; regime boundaries; a staleness ladder | *the framing expired* |
| **Structure** | an architecture invariant is violated | machine-enforced invariants in the standing gate | *two framings contradict inside one system* |
| **Position** (ambiguity) | a verdict cited without the seat it was computed at; a count without its corpus | carry the position with the verdict | *the framing is unstated* |

**The three repairs are not interchangeable, and this is the practical payoff of cutting by
mechanism.** Frame-fixing does not repair an invariant violation, and an as-of stamp does not settle a
contested position. Cutting the taxonomy by *what it looks like* would license the wrong repair in
two cases out of three — which is not a hypothetical: it is the argument for why the cut is where it
is.

### III.3 The practices, derived rather than listed

Each practice below is the same move performed deliberately on a named axis. That derivation is the
reason to adopt them; presented as a list they would be folklore.

**Witness-before-claim.** *Perturb the position axis: producer's frame versus consumer's frame.*
Every "done / verified / fixed / passing" claim carries its witness — the pasted run, the diff, the
per-item check — **in the same turn the claim is made**. If the witness cannot be produced now, the
claim is downgraded to OPEN with a named graduation step. The reason this works is not diligence: it
is that **producing a witness usually requires touching the thing that is silently absent.** A claim
in a chat transcript does not witness a committed artifact, because the transcript and the substrate
are two framings, and the paste must land where the claim lives.

**Positive controls, universally.** *Perturb the probe itself.* The deepest of the borrowed
structures, and the one that generalises furthest:

> **A clean read is byte-identical to a read that never looked.**

An empty grep, a zero count, a passing test, an "identical" diff — each is evidence *only if the
probe is shown able to detect what it reports absent, on the exact path it claims to test.* In the
derivation's terms, a positive control is the instrument by which **a search states its own scope**
(`CWC:A2@31548228`): *"no occurrences found"* is a compression over a frame, and the control is what
establishes what that frame could have contained. The purest motivating case in the record is a
comparison that reported `identical: True` because **both sides were empty** — two failed
measurements agreeing with each other, and no channel anywhere for the fact that neither had looked.

**Three tiers, because "witnessed" is not one predicate.** Treating it as one is how the discipline
decays into ritual — a pasted output can be irrelevant, a diff can be real and still not establish
semantic equivalence, a control can fire and still not test the path in question.

1. **Witness** — evidence the operation occurred.
2. **Validating witness** — evidence the operation *could have detected* what it claims to establish,
   on the path it claims to test.
3. **Adequate witness** — evidence sufficient for the declared verification depth, with the residue
   named.

Most observed regressions are a stop at tier 1: **real evidence of a real operation that could not
have failed.**

**A control demonstrates discrimination, not detection.** Planting the target shows only that the
instrument *can* fire; the witness that its firing carries information is a case it **declined**. This
is the perturbation read strictly — a one-sided control varies the axis in one direction only, and an
instrument that fires on everything is indistinguishable from one that fires on nothing. Grades,
strongest first: a decline in the instrument's own history; a naturally-arising negative drawn from
the population; an authored decoy, which shows only that authored decoys get rejected. **No decline
available anywhere in the population means the question is unanswerable from this population** — a
verdict to declare, not a caveat to ship under.

**Pre-registration and churn floors.** *Perturb time, before the results are visible.* For any run
whose outcome matters: a frozen proposal stating what will run and what each outcome would mean,
written before results exist, with the freeze made witnessable by recording the proposal's checksum
physically above the first result line. Where the generator is stochastic, a **churn floor** —
*k* redraws at byte-identical input — establishes what movement means nothing, so that a difference
smaller than the floor is never read as a finding.

**Prove before you replace.** *Perturb the substrate, not the description of it.* Before deleting,
retiring, or overwriting anything something relies on: run old and new, paste both outputs, diff
them, and show identity or justify every difference **in the same change**. "Structurally equivalent"
is a code-read, not proof; the diff is proof. Consolidating N things into one is N separate old-versus-new
diffs, each owed before its standalone is retired.

**Declared stopping points.** *Stop perturbing — and say so.* "Verified enough" is a position with no
floor. The stopping rule is: stop when the next pass costs more than being wrong **and every open
question is declared rather than concealed.** The checkable half is the second — for each verdict
emitted, name a falsifier available at this tier or downgrade the verdict to OPEN. **The number of
passes is not the variable; whether the stop was declared is.**

### III.4 Why the recursion is in the taxonomy and not a footnote

The last axis is the apparatus itself. **Perturb the instrument** — and the reason this does not
terminate is the derivation read upward: **every instrument has its own framing, uncertifiable from
inside.** A perturbation harness that silently no-ops reports "no change" indistinguishably from a
real null. So harnesses get two-sided controls (a no-op overlay must change nothing **and** a
known-live input must flip the output), and gates get planted violations **and** conforming twins.

This is not a caveat appended to the taxonomy. It is the taxonomy's fourth axis, it is why §V exists,
and it is where the honest limit of the whole practice is located.

---

## IV. The recursion: the practice failing on itself

> **Scope bound, stated before the material rather than after it.** This section is *one section of a
> practice paper*, not the paper's subject. If it grows past its share, or if §I–§II begin serving it,
> the genre has drifted and the right response is to cut it, not to expand the rest.

### IV.1 The observation

**EMPIRICAL, and denominated as narrowly as the evidence permits.** A single pre-spend arc in August
2026 produced **at least twelve false absences committed by the apparatus's own instruments** — that
is, by the instruments built to catch false absences. Ten were *discovered* by the red light rather
than confirmed by it. **Five were committed inside instruments built to catch the earlier ones.**

> **The count is a floor, not a measurement.** Self-observed, same party, same pass, **no
> denominator**. It is citable as *"at least twelve, self-observed, undenominated"* and in no other
> form: **never as a rate**, and never compared against the eleven instances recorded at
> `amnesiac_institution_v0_6.md` §7.4, which were counted differently, over a different population,
> by a different route. The two numbers are incommensurable, and a ratio between them would be a
> fabrication with two real inputs.

### IV.2 What it supports, and what it does not

The claim the twelve support is narrow and is the interesting one:

> **ANALYTIC, evidenced empirically here.** Declaring a framing produces a **new artifact with a new
> framing**. The remedy is therefore not self-terminating.

It does **not** support "verification is futile," and the distinction is not a hedge. A
non-terminating remedy that reduces the defect at every step is worth running; what it cannot do is
certify itself, which is a statement about where the practice's limit is, not about whether it works.

### IV.3 What actually terminated them

**Of the twelve: not one was caught by a gate reading its own output green. Ten were caught by a
party comparing a claimed value against the artifact it described** — a diff, a directory listing, a
file count, a re-read.

> **The population is in that sentence deliberately, and this is a correction to how the claim was
> previously made.** Stated without a population — *"none was ever caught by a gate"* — the claim is
> now **false** of v0.6 §7.4's eleven, whose eleventh instance *was* caught by a gate: a structural
> integrity line printing `partition_check: 186 == 185`, refusing a numerator that had silently
> acquired a member which was not a directory. An unscoped version of this sentence would be a false
> absence in a section about false absences.
>
> **The exception is more informative than the streak, and it says exactly what kind of gate can
> close this loop.** *These two strata partition this population* has **no plausible-looking failure
> mode** — there is no value a compromised numerator could take that would satisfy it. A gate catches
> this class exactly when it **asserts a structural invariant rather than checking a value**. Ten of
> the eleven had no such invariant available to violate, which is why they needed a reader. **The
> loop closes wherever an invariant exists to state, and nowhere else — and finding those places is
> design work, not more apparatus.**

### IV.4 The two shapes worth naming

**The detector that named itself.** A detector was written to find controls that nothing calls. **On
its first run it named itself** — nothing called it yet. The recursion complete in one line, by the
instrument's own criterion: *the detector for a defect class is a member of that defect class.* The
exemption subsequently taken is correct — it is a self-test instrument and its wiring witness is that
the suite fails without it — and **the exemption was written into the source with its reason**, which
is the entire difference between this case and the next one.

**The unstated selection rule, which is the thin place.** Twice, in independent arcs, a defensible
criterion was applied by everyone including the operator and **never articulated** — *things that read
like specifications get pinned* (a pre-registration manifest that pinned sixteen texts and nothing
executable, so the run proceeded under a green freeze that carried no information about whether it
could produce data); and *the always-loaded file is the delivery channel worth instrumenting* (a
well-specified, two-sided, pre-registered, cheap canary aimed at a channel with 46× headroom on a read
path that does not truncate at all). Both times **the apparatus performed correctly on the thing it
was given and had no way to ask whether it was given the right thing** — because the manifest is what
the checks are computed over.

Both were corrected the same way, and it is not a gate: **someone read the substrate directly, from
outside the frame the manifest defined.**

> **A third candidate fitting the shape exactly was offered and is DELIBERATELY HELD OUT, because the
> reason is the finding.** It arose in the same arc that coined the pattern. **A pattern that fits
> every finding in its own arc has stopped being falsifiable**; two instances from two independent
> arcs is a pattern, a third from the coining arc is confirmation-shaped. The count stays at two and
> the reason it is not three is written down — which is the section's own rule about unstated
> criteria, applied to the section.

### IV.5 The honest statement of the limit

> **An unstated exemption is indistinguishable from an unnoticed one, and no gate can separate them,
> because the gate is downstream of the exemption.**

The recursion terminates in a declared stopping point — and **declared** is the operative word: it
means declared in a file, not decided in a head.

---

## V. Self-instrumentation, and the negative control

> **The material in this section moved here from `amnesiac_institution_v0_6.md` §2.9**, which is now
> the superseded side and carries a forward pointer. §2.9(b) keeps its letter there, because it is
> cited in correspondence already sent.

### V.1 The hazard the instrument exists for

A verification apparatus built by fluent, cheap workers **grows**. Every incident mints a rule; every
rule is well written; the stack's cost rises monotonically while nothing measures whether the marginal
ritual still catches anything. Stated as a rule the practice must apply to itself:

> **Do not answer "does the apparatus pay for itself?" by producing more well-formed
> apparatus-output.**

The bound is structural rather than exhortative: the rule channel is **capped**, so admitting a rule
requires retiring one, and every audit must declare a **catch bit** — `live` (a control fired, or a
consumer-visible verdict changed), `latent` (a real defect, conditional on an input the system does
not yet produce), or `no` (pure confirmation). The rolling rate is **reported, never gated**, because
a self-gating apparatus could launder its own worth.

### V.2 The negative control, and why it is owed

Here is the claim §III makes, stated plainly: *every defect in this record is an axis that varied
unheld.* That is a **unification claim, and a unification claim is itself an invariance claim** —
invariant under which instrument you look through. By §III's own rule it therefore **carries no
information until a control shows the framing can fail to fit.**

Without that control, *"every defect is an unheld axis"* is byte-identical to *"my framing finds its
own shape everywhere."* This is the practice's central demand turned on the practice's central claim,
and refusing to run it here would make §III unfalsifiable through its own instrument. **The control
runs in two parts, and the second one costs something.**

### V.3 (a) The account must correctly exclude — and it does

Three failure classes in this record are **not** unheld axes, and the account reports them as
not-fitting rather than absorbing them:

- **Structural contradictions.** Two axiom sets that derive a contradiction do so **inside a single
  framing**, immediately, with no process and no read site. Frame-fixing does not dissolve Russell's
  paradox, and declaring a scope does not repair an invariant violation. The repair is axiomatic
  revision — which §III.2's table already routes elsewhere, so the exclusion is not an add-on.
- **Stochastic churn.** An observable that moves at **byte-identical input**. Nothing there is
  compressed, framed, or read at the wrong position; the generator is simply non-deterministic.
  Treating it as a framing problem would have produced the wrong instrument — a scope declaration
  instead of *k* redraws.
- **Loud destructive replacement.** Removing something that turns out to be needed **announces
  itself**: the restoration is the notice. There is no read site fooled and no success-shaped token.
  This is why it was demoted out of the taxonomy — and **the exclusion and the demotion were reached
  independently, before this account was written**, which is what makes it evidence rather than
  accommodation.

Because the record contains defect classes the account **rejects**, the recurrence across the rest is
not vacuous. **The lens discriminates.**

### V.4 (b) A within-scope place the account genuinely breaks

An account that only excluded cases outside its scope would have passed a weak test. The stronger
demand is a place **inside** the scope where the predicted repair does not hold, and there is one.

The account predicts that **carrying the framing to the read site repairs the failure. At the
instrument stratum it does not.**

The eleven instances at v0.6 §7.4 were committed by artifacts that carried their scope
**scrupulously** — manifests with declared coverage, controls with stated plants, a consolidation
table with an explicit totals row. The defect in each case was that **the carried scope was itself a
compression with its own unstated framing**: the totals row disagreed with the table beneath it; the
drift list reported twelve changed sources when two had changed. This is §IV.2 met from the other
direction — **declaring a framing produces a new artifact with a new framing** — and §IV.3's finding
is the shape of what actually terminates it: **not a declaration, but a re-derivation.**

> **So the honest scope of the unification, stated at the altitude the control licenses:**
>
> **The account unifies the *form* of the failure and the *direction* of the repair. It does not
> license the claim that carrying scope is sufficient — and the evidence available says it is not.**

What sufficiency would require is an **enumeration performed by a party positioned outside the
artifact's framing.** That is not a courtesy owed to the human in the loop; it is why the human's
second jurisdiction is **structural**. A worker who sees one session cannot observe that the same
check has gone red three times across three sessions, cannot see a defect's shape recurring across
unrelated subsystems, and cannot notice that a rule's premise expired while the rule stayed internally
correct. Those findings are gated on **cross-session continuity, not on authority** — they belong to
the operator because nobody else is positioned to see them (v0.6 §9.2, cited not restated).

### V.5 What the instrument cannot see, stated at the same volume as what it can

**OPEN, with its kill condition.** The catch bit's readout has **never recorded a decline.** Every
bit so far is `live` or `latent`; there is no `no`. Read strictly by §III.3's own rule, **a falsifier
with no decline in its entire record licenses nothing in either direction** — so the instrument built
to answer "does the apparatus pay for itself?" **currently returns a refusal in place of a rate**, and
prints it as a refusal rather than as a number. That is the correct output and it is not a good one.

The reason is knowable and is a population-selection effect rather than a mystery: **an audit written
because something was suspected scores `live`**, and the audits that would score `no` are the ones
nobody writes. The kill condition follows: **if a substantial run of bits accumulates and still no
decline is ever recorded, the bit is measuring which audits get written, not which apparatus
catches** — and it should be demoted or retired rather than reported.

Three further limits, none of them repairable by more apparatus:

- **The bit can be gamed**, by the party who assigns it.
- **The cap bounds count, not quality.** Retiring the least-defensible rule to admit a new one is a
  procedure that cannot distinguish a rule that stopped being needed from one that was never needed.
- **It cannot see restraint** — the apparatus not built, the sweep not run, the rule not minted. The
  most valuable decisions in a practice whose hazard is unbounded growth are the ones that leave **no
  artifact**, and are therefore invisible to every instrument here, including this one.

### V.6 Closing: what this practice is, in one sentence

**A practice for research whose workers do not persist is an arrangement of artifacts such that the
characteristic failure of the workforce — a confident, well-formed report of a thing that did not
happen — is made loud at a boundary, by an instrument that has itself been perturbed.** The
consequences of that sentence are §II (which artifacts, and what they cost), §III (which
perturbations, on which axes), §IV (that the arrangement fails on itself, and where), and §V (that it
cannot certify itself, and what that does and does not license).

---

*CC0 Universal. Draft v0.1, 2026-08-20. Extracted from `amnesiac_institution_v0_6.md` §2.8/§2.9 per
ISSUES OQ-287 Limb 1; those subsections remain at their numbers in v0.6 as the superseded side, with
forward pointers here. The derivation is cited, never restated:
`../concealment/concealment_without_a_concealer_v0_4.md` is canonical for it. Aptness notes for every
citation: `../../audits/2026-08-20_oq287_limb1_extraction/APTNESS.md`.*
