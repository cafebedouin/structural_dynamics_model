# Concealment Without a Concealer: How Boundaries Turn Locally Valid Artifacts into Unwarranted Claims

**cafebedouin@gmail.com**

*Status: DRAFT v0.4 (2026-08-12). v0.1 was extracted from §2 of `The Amnesiac Institution` v0.6;
this revision incorporates six independent model reviews, narrows the universal claims those reviews
found brittle, adds the historical record (§7), the deep-time limiting case (§8), and a third mode of
concealment (§5.3) that v0.1 could not express. v0.3 adds §7.5 (the institutions built to prevent
this failure, read as carriage technologies) and §8.2–§8.3 (the societal-scale case, and framing
monoculture). v0.4 adds §3.6 (why neutrality is not the alternative to carriage) and §8.4
(condition-indexed markers). The title names a result of the parent program: the no-seat pose is
`deferential_realism_paper_v8.md`'s, and what is added here is its production at boundaries under
amnesia, with nobody posing. Claim types are declared in Appendix A and govern
how each claim may be discharged. Historical cases in §7–§8 were verified against public sources on
2026-08-12; the companion paper is the source of the undated institutional examples.*

> **CANONICITY MARKER (added 2026-08-12; provisional pending an authorial ruling, ISSUES OQ-287).**
> The §2 derivation this paper develops **also** still sits in
> `docs/amnesiac_institution/amnesiac_institution_v0_6.md` §2, whose §13 names it as *that* paper's
> contribution. Two live documents, one contribution, no queryable fact of canonicity — Build
> Discipline Pattern 2 on the paper substrate. **Working disposition pending the ruling:** this
> paper is canonical for the **derivation**; v0.6 is canonical for the **institution** (case study,
> differential amnesia, memory economy, organizational form). Cite accordingly until OQ-287 closes.

---

## Preface: what kind of claims these are

The argument mixes three kinds of claim, and conflating them is how an account of this shape gets
both overclaimed and overcorrected. **Analytic** claims (A1–A4) are derived from stated premises and
are refuted by exhibit, not by data. **Empirical** claims (E1–E3) are generalizations about how
parties behave, and are currently supported unevenly. **Proposed** claims (P1–P3) are design
consequences with falsifiers. The full table, with kill conditions, is Appendix A. A reader who
wants the argument rather than the bookkeeping can proceed; a reader who wants to attack it should
start there, because the table says which attacks would land.

---

## 1. Two cases, thirty years apart

### 1.1 Fogbank

Between 1975 and 1989, the Y-12 National Security Complex at Oak Ridge manufactured a classified
material, codenamed Fogbank, for the W76 warhead. When the last warheads were built the production
facility was deactivated and eventually decommissioned. Around 2000, the W76 life-extension program
required more of it.

<cite index="11-1">By then the agency had lost nearly all of its institutional knowledge of how to make the material: most personnel involved in the original production were no longer available, and the process had to be reconstructed from historical records.</cite> <cite index="12-1">Few records of the process had been kept, and almost all staff with production expertise had retired or left.</cite> A new facility was built to modern health and safety standards, the reconstructed process was run — and the product did not work.

The root cause is the most instructive part, and it is public. <cite index="18-1">Investigation traced the failure to a particular impurity that had remained after the original purification process and subtly altered the material's structure. The structural change had been implicitly relied upon in a downstream process, even though it had never been explicitly documented, tested, or controlled. The new, cleaner purification removed more of the impurity, degrading the final product; once its role was understood, the required concentration was established, added in a separate step, and monitored explicitly.</cite>

Note what did *not* happen. Nobody lied. Nobody was careless in a way an audit would have caught. The
records were not falsified; they were *accurate about what their authors were describing*. The
impurity was not part of the process as anyone understood the process — it was contamination, which
is to say it fell outside the selection rule of "what the process consists of." A specification
cannot be searched for the ingredient it does not mention.

Note also what recovered it: not a better document. <cite index="11-1">Recertification of the production process took the better part of a decade</cite> and proceeded by re-running the process, observing the same failures the original scientists had seen, and doing forensic work against surviving material. The published cost figures disagree — <cite index="13-1">$69 million</cite>, <cite index="14-1">$92 million</cite>, <cite index="16-1">approaching $100 million</cite> — which is itself a small instance of this paper's subject: three compressions of one program's accounting, each formed at a framing none of them carries.

### 1.2 The `0.0` fallback

The same shape, thirty years later, in a research pipeline, at a scale where the whole chain is
visible in a paragraph.

A computation returns nothing when it fails. A fallback emits `0.0`. Downstream, an aggregate
reports the quantity as flat — and a flat gradient is a substantive, publishable null, more
interesting than the truth. When the pipeline was finally traced, every such computation ever made
had failed; the construct had no interval of valid measurement in its entire life.

Every component was correct. The computation correctly reported failure by returning empty. The
fallback correctly supplied its default. The aggregate correctly averaged what it received. The
report correctly stated what the aggregate said.

### 1.3 The triad

Both cases share three properties, and together they are why ordinary error vocabulary does not fit.

- **Nothing was false.** Each artifact was accurate at the framing that produced it.
- **Nothing was withheld.** No party possessed the missing information at the moment it mattered and
  declined to pass it on.
- **There was no signal.** A wrong number that looks wrong recruits attention. These looked exactly
  like the thing they stood in for, because they *were* that thing, minus one dimension for which no
  channel existed.

The rest of this paper argues that the triad is not a coincidence of two anecdotes but the expected
output of any boundary between parties that must compress.

---

## 2. The phenomenon, stated

**Thesis.** A representation can be perfectly faithful relative to the selection rule that produced
it and misleading when consumed under a different one. The boundary between those two operations
does not, in general, preserve the selection rule.

Three definitions do the work.

**Compression.** Used functionally, not information-theoretically. An aggregate, summary, inventory,
manifest, index, default value, filtered query, status flag, or monthly prune is a compression
whenever it stands in operationally for a richer upstream state under some selection.

**Framing (Π).** What fixed the selection: the range, the criterion, the interval, the unit, the
purpose, the residue.

**Boundary.** A transition at which an artifact produced under one working set becomes an input to an
operation governed by another working set, selection rule, or decision criterion. Function calls,
handoffs, shift changes, publications, database writes, and sessions are all instances; what makes
them one kind of thing is that the producing operation is not available to the consuming one.

Two distinct things get lost at a boundary, and it is worth separating them because they have
different repairs:

| Lost dimension | The question it answers | Failure when absent |
|---|---|---|
| **Status** | Did this happen? Was it measured, generated, verified — or absent, not run, not found? | `0.0` reads as a measurement; a green check reads as a passed test |
| **Framing** | What population, interval, and criterion does this concern? | 42% of *what*, over *which* directories, found by *which* filter |

Fogbank lost framing (the process spec's criterion for "the process" excluded uncontrolled inputs).
The fallback lost status (absence and measurement became extensionally identical at the receiving
interface). Most real failures lose some of both, and v0.1 of this paper treated them as one problem;
they are two, sharing a mechanism.

---

## 3. The argument

### 3.1 Premise 1 — working-set finitude (A1)

Any party that must produce an action, judgment, or handoff over a corpus not jointly available
within its operative working set must rely on a representation that does not preserve all
decision-relevant relations in that corpus.

This is a claim about ratios, not about cognition, and it holds for people, models, teams, and files
someone has to actually read. Paging, indexing, and external memory do not evade it; they relocate
the selection to the retrieval step, which is itself a selection under a rule.

A corollary the design sections depend on: **unbounded retention is not memory; it is a pile.** A
record no reader can hold has not retained the knowledge in it.

### 3.2 Premise 2 — selections are standpoints, in a strong and a weak form (A2)

The **strong form** is the one this paper's parent program derives: a parameter that co-determines a
verdict without being fixed by the situation is not a feature of the situation but an index of
evaluation, so a contentful verdict is never standpoint-free; and the partition of a field into
background and free parameters is itself such a parameter, so there is no framing-free
situation-description beneath a verdict for neutrality to retreat to (`seat-theorem-v1.md`).

The strong form is contestable — a direct realist holds that some framings carve at joints — and this
paper **does not need it**. The weak form is sufficient for everything below:

> The artifacts at issue — summaries, inventories, manifests, defaults, aggregates, specifications —
> are produced under finite and potentially differing selection rules, by parties with budgets.

A reader who accepts the strong form gets a deeper account of why the regress in §9 does not
terminate. A reader who rejects it loses nothing operational. Readers of the parent program should
note that this is a deliberate retreat from v0.1, which made the strong form a premise and thereby
handed a skeptic an attack surface the argument never required.

### 3.3 Framing non-identifiability (A2, the proposition)

v0.1 said "the framing is not recoverable from the artifact's form," which is false as stated: a
report may say *"all transactions 1–31 January exceeding $10,000."* The correct proposition is
narrower and more useful:

> **Framing non-identifiability.** The framing is not *entailed* by the compressed content. Where
> framing is present, it is present as additional authored content — a denominator, an as-of date, a
> coverage fraction, a declared residue — not as something recoverable from the compression itself.

This matters because it turns the repair from a paradox into a consequence: **carriage works
precisely because framing is additional information.** Someone has to author it.

And there is a reason it usually goes unauthored that is more specific than "there wasn't room":

> **The producer did not need the framing, because the producer *was* the framing.** Within the act
> of producing, the selection rule is not information — it is the operation being performed. A grep
> does not record which file types it opened; it opened them. A reviewer writing "all addressed" does
> not record that the scope was the items they looked at; those were the items. A process
> specification does not record that "the process" excludes uncontrolled inputs; that exclusion is
> what makes it a specification.

Rules do not record themselves unless someone steps outside them. That is why the repair is
expensive, and why exhortation does not produce it.

An important qualification the reviews of v0.1 forced, and the case in §1.1 illustrates: this is
**not** the claim that nobody knows. Sometimes nobody knows (Fogbank's impurity). Sometimes the
producer knows perfectly well and the artifact does not (`0.0`). The general statement covers both:

> **Knowledge possessed at production time is not necessarily information carried across the
> boundary.**

### 3.4 What a holder of a compression can do (A3)

Narrowed from v0.1:

> No procedure operating only on the compressed content can enumerate omissions not positively
> represented by the artifact or its declared provenance.

An omission has no positive form. It is not a feature with a null value; it is the absence of a
feature, which is not a location a procedure can address. The near-misses — coverage fractions,
residue lists, denominators — are exactly the *authored additions* of §3.3, and their existence is
the repair rather than a counterexample.

### 3.5 Warrant transfer (A4)

The most contestable move in v0.1 was from "the framing is absent" to "the artifact asserts content
while carrying no standpoint." A critic reasonably objects that an omitted framing makes a claim
*underdescribed*, not *neutrality-claiming*. Three cases separate:

1. **Explicit universality** — "this inventory is complete."
2. **Scoped claim** — "this inventory is complete for files matching C, as of D."
3. **Unscoped operational uptake** — an artifact is *treated* by a receiver as sufficient for deciding
   what is complete, measured, tested, or present.

The argument is about the third, and the reformulation is stronger for being narrower:

> The defect is not that an artifact denies its standpoint. It is that, when accepted as sufficient
> warrant at a new read site, it receives the operational authority of a claim whose scope has been
> settled for the reader — although the scope was settled elsewhere, and is no longer available for
> inspection.

The failure is in **warrant transfer**, not in an artifact's semantic form. Formally, with corpus *C*,
framing Π, representation *R*, and interpretation *I*: the problem is not that *R*(Π, *C*) ≠ *C*,
which is what compression means, but that

> *I*(Π′, *R*(Π, *C*)) ≠ *I*(Π, *R*(Π, *C*)) — while the artifact does not identify Π,

so the consumer cannot tell which interpretation it is licensed to make.

### 3.6 Why neutrality is not the alternative to carriage (A5)

An obvious objection to everything above is that the repair is unnecessary: rather than carrying the
framing, state only what is true independent of framing, and the artifact will travel safely. This
does not work, and the reason is worth stating because it is the deepest structural fact in the
paper.

Take the content of an artifact that survives every framing — what glues across all positions, what
stays under every perturbation of seat, interval, and criterion. Call it the artifact's **global
section**. Global sections exist. Deductive consequence is one; conservation constraints are
another; tautologies are the trivial case. But they share a property:

> **Invariants are prohibitive, not selective.** A claim that holds from every position tells you
> what cannot be the case from any position. It does not tell you which of the locally available
> actions to take, because selecting among them requires exactly the parameters that vary between
> positions.

Deduction is the clean illustration: it preserves truth across every framing and adds no content.
That is not a defect of logic; it is what framing-independence costs.

Two corollaries follow, and the second is a diagnostic.

**Utility and transportability are inversely related.** The more decision-relevant a claim is at a
site, the more tightly it is bound to that site's framing, and the worse it transports. Institutions
therefore transport, of necessity, exactly the artifacts that transport worst — because the ones
that transport perfectly are the ones nobody needed to send. This is why the failure is structural
rather than a symptom of carelessness: it is a consequence of what makes an artifact worth moving.

**A global section is not visible from any single position.** Seeing an invariant requires varying
the parameter and observing that nothing moves, which is an operation across positions, not within
one. There is no seat from which the framing-free content is legible, because legibility at a seat
is what framing-dependence means. The view *sub specie aeternitatis* is not a standpoint anyone can
occupy; the finite substitute is the view from many positions held at once, which is an expensive
practice rather than a vantage.

And the diagnostic, which retroactively explains §7.2:

> **A local section masquerades as a global one whenever the positions have not varied enough.** A
> claim that has never been perturbed is indistinguishable from an invariant.

"Citrus cures scurvy" glued across every position the Royal Navy occupied for a century and looked
like a fact about the world. It was a local section over a framing that held fixed the fruit, the
processing, the vessel, and the voyage length. When the positions varied — West Indian limes, copper
vessels, steam-shortened voyages, polar conditions — the gluing failed, and the failure was read not
as *we have discovered the boundary of a local section* but as *the claim was never true*, which is
how the cure was lost rather than refined. Apparent universality is evidence about the range of
positions sampled, not about invariance, and no artifact reports which of the two it is.

This is the same result as A2 stated on the other axis. A2 says the framing is not in the artifact.
A5 says you cannot avoid the problem by removing the framing, because what is left is either
prohibitive or empty. **Carriage is not the second-best repair after neutrality. It is the only
repair, because neutrality is not a place where content lives.**

---

## 4. Recognition and enumeration (E1)

An operational signature, and the most testable thing in the paper:

> **Re-reading exercises recognition. Writing the instruction that would execute the thing exercises
> enumeration.** Recognition confirms that the artifact still says what the holder remembers, and an
> artifact correct in prose passes every time. Enumeration requires producing the operational form —
> every input a receiver needs, every artifact they must produce, every decision they would otherwise
> make on the author's behalf.

Applied to §1.2: the recognition check reads "the pipeline reports the gradient" and confirms that
it does. The enumeration check writes out what the pipeline must do to report a gradient — compute,
return, aggregate, handle the empty case — and discovers on the fourth item that "handle the empty
case" was answered by a constant.

Recognition is *closed over the artifact*; enumeration reopens the boundary. That is why enumeration
finds framing failures and recognition cannot: recognition operates on exactly the representation
that omitted the thing.

Evidence, at two different strengths, and the distinction matters.

**For model workers, the evidence is direct but bounded to tested conditions.** AbsenceBench (Fu et
al., 2025) finds that language models detect *omitted* content poorly even at modest context lengths,
and offers a mechanical account: attention cannot easily attend to gaps, because absences correspond
to no key. Advani (2026) finds that model judges cannot reliably detect completion claims
contradicted by environment state, with no judge configuration exceeding 0.65 AUROC, and with judges
anchoring on confident closing language rather than verified state change. These establish that
*models have difficulty detecting omission and false completion under tested conditions*. That
recognition *cannot in general* find gaps is this paper's theoretical interpretation, and the two
should not be allowed to slide together.

**For human parties, the evidence here is an existence proof.** Four rulings in one institution,
correct in prose and defective in execution, each caught by a receiving party who had to write out
what the rule actually required. That is embedded case material, not a rate. Prediction 1 (§11) is
the measurement that would convert it.

---

## 5. Concealment without a concealer

### 5.1 The result

Combine. A compression crosses a boundary. By §3.3 it does not carry its framing; by §3.4 the
receiver cannot recover the framing from it; by §3.5 the receiver nonetheless takes it as sufficient
warrant for a contentful decision. So the institution acts on a claim whose scope was settled
somewhere the deciding party cannot inspect — and it does so **without requiring any agent who
withheld the missing information.**

The term is used phenomenologically, and the definition should be stated before anyone objects that
nothing was concealed:

> **Concealment without a concealer:** a condition in which information necessary to interpret an
> artifact is unavailable at the read site although no agent withheld it. It describes the receiver's
> epistemic position, not the producer's intention.

Once that is explicit, the apparent paradox dissolves and the useful part remains.

### 5.2 Responsibility is not exhausted by intention

"Nobody concealed anything" is not exculpatory, and v0.1 risked reading that way. Three propositions
separate cleanly:

- No individual need **intend** to hide relevant information.
- No individual need **know**, at production time, which framing will later become decision-relevant.
- An institution can nonetheless be responsible for **building boundaries that permit an upstream
  distinction to become unrepresentable downstream.**

The `0.0` fallback is a design decision: a tagged union or an out-of-band token would have preserved
the distinction at negligible cost. Fogbank's records were kept under a classification regime that
made recording process detail expensive and diffusion of expertise deliberate. Neither is an
accident of nature. The choice this account forces is not "honesty norms versus no accountability" —
it is **intention-centred accountability versus infrastructure-centred accountability.**

### 5.3 Three modes, because the repairs differ

v0.1 recognized two things called concealment and could not express a third. All three are real and
they route differently.

| Mode | What is missing | Who could state it | Repair |
|---|---|---|---|
| **1. Withholding** | An agent holds the framing and does not pass it on — classification, compartmentalization, fraud, editorial privilege | The holder | Disclosure norms, audit, access |
| **2. Non-carriage** | The framing was the operation, not a datum; no channel existed for it at the boundary | Anyone, if they stepped outside the operation | Carriage at the boundary + re-derivation (§9) |
| **3. Positional blindness** | The framing is **inexpressible from the position**: no field to author it, no category to file it under, no predicate that could fire on it | *Nobody*, in the current vocabulary | Inbound perturbation: run a foreign vocabulary against your own and see what it has fields for that you lack; then extend the schema |

Mode 3 is the one this paper did not previously have, and it matters because **carriage presupposes
expressibility.** A carriage rule says: state your denominator, your interval, your criterion. It
cannot be discharged for a dimension the producing vocabulary has no slot for. Where a framing is
positionally invisible, the artifact is unframed *and* the discipline reports compliance — which is
the paper's own failure mode, one level up, committed by the repair.

And Mode 3 has a structural consequence worth stating separately: **a schema cannot enumerate what it
has no slot for.** This is §3.4 applied to a vocabulary rather than to an artifact. It follows that
the blind cells of a representational system are not discoverable from inside it by diligence, only
by contact with a system that carves differently. That is an argument for maintaining foreign
sources not as inspiration but as *instruments* — the only available probe for a class of defect the
system is constitutionally unable to detect in itself.

Mode 1 interacts with Mode 2 in a way §7 makes concrete: a secrecy regime is institutionalized
destruction of the conditions under which non-carriage is survivable.

> **[EDITORIAL NOTE, added 2026-08-12 — an inconsistency in the table above, found by an external
> control and NOT resolved here.]** The table sorts by *who could state it*, which is a claim about
> **datum-possession**. Mode 2's cell then defines itself by *"no channel existed for it at the
> boundary"*, which is a claim about **channel-existence**. **These come apart, and §1.2's own
> worked case is where they do.** For the `0.0` fallback, someone *did* hold the framing as a datum
> — whoever wrote the fallback held *"this is a default, not a measurement"* — so on possession it
> is Mode 1, while on channel it is the Mode 2 the paper files it under. §5.2 half-concedes this
> already (*"a tagged union or an out-of-band token would have preserved the distinction at
> negligible cost"*): a repair that cheap is available precisely because someone held the thing.
>
> The distinction is not cosmetic, because **the repairs differ**. *Held by a party, no channel at
> the boundary* is fixed by widening the channel — a tagged union, a status token, an out-of-band
> field — and needs nobody to step outside their operation. *Held by nobody* (the Fogbank impurity;
> the unstated genre rule) cannot be fixed by widening a channel, because there is no datum to put
> in it; it needs someone to step outside the operation, which is the expensive move §3.3 says
> exhortation does not produce. A three-mode table that routes both to one cell routes two prices
> to one budget.
>
> Candidate dispositions, neither adopted: split Mode 2 into **2a (held, no channel)** and **2b
> (held by nobody)**, or restate the sort column as *channel* throughout and let possession fall
> out as a sub-property. The second is cheaper and loses the repair distinction; the first keeps it
> and makes the table four rows.
>
> Provenance: `audits/2026-08-12_oq283_framing_boundary_discrimination/WRITEUP.md` §4, a
> discriminating control run against `seat-theorem-v1.md` Corollary 2a on a six-instance in-repo
> population. The finding is the control's only correction to this paper. **Recorded here rather
> than in the tracker alone, because a reader of this section would otherwise have no way to know
> the table has a seam — which is this paper's own subject applied to this paper.** Disposition is
> the author's; ISSUES OQ-283 carries the evidence.

### 5.4 The surface signature

The way this is usually first met, in any of the three modes:

> **An inventory built under an unstated selection criterion does not read as visibly partial. It
> reads as plausibly total, because every item in it belongs.**

Three instances of the shape from one institution, expanded here because in v0.1 they were opaque to
anyone without the companion paper:

**A pre-registration's pin manifest.** Before an experiment, sixteen artifacts were frozen and
checksummed so the run could not be silently altered mid-flight. Every pinned artifact was a
*document*. Nothing executable was pinned. The unstated criterion — *things that read like
specifications get pinned* — was defensible and was applied by everyone including the operator. The
freeze check then passed, green, carrying no information about whether the run could produce data at
all, because the instrument that would produce it was outside the manifest. It could not.

**An experiment's design.** The design specified every stage up to the point where data lands, and no
stage after. It ran. It produced 219 answers that nothing in the system could score, because the
scoring stage had never been specified — a producer with no consumer, one step downstream of the
stage that failed.

**A completeness manifest.** A check confirmed that all flagged items had been carried forward. Its
criterion was *items flagged in files*. Seven findings had been produced in conversation and never
written to a file, so they were not flagged, so they were not missing, so the check reported
complete.

Each read complete because every item in it belonged.

---

## 6. Reconstructability (E2)

### 6.1 Why the account does not prove too much

Human institutions have run on framed compressions for millennia without collapsing, and the reason
is not that their compressions carried framings. It is that consumers could often **reconstruct**
the framing they never received: knowing the producer, having been in the room, knowing what the
department counts, being able to walk down the hall and ask.

So the exposure at a boundary is governed by three factors, stated as a qualitative law rather than
an equation, because none of the three is currently measured:

> **Exposure to silent framing failure increases with compression ratio and with framing distance,
> and decreases with reconstructability** — where reconstructability is the probability that the
> consumer restores the producer's framing from shared context without being told.

This is a heuristic for locating intervention points and generating comparative predictions, not a
quantitative law. Framing distance and reconstructability would need to be elicited, coded, or
experimentally manipulated before the relation could be tested as more than an ordering.

### 6.2 Reconstructability is invisible to the institution that depends on it

This is the sharpest thing in the section and it is what Fogbank demonstrates.

Reconstructability decays continuously and *silently*, and it decays in steps that nobody is notified
of. No alarm sounds when the last person who knew a process retires. No document changes when the
facility that could be walked through is demolished. The institution's **belief** in its own
reconstructability decays far more slowly than the quantity itself, because that belief is
maintained by a record whose apparent completeness is unaffected — the specifications on file in 1999
looked exactly as complete as they had in 1985.

So the dangerous condition is not low reconstructability. It is a **gap between actual and believed
reconstructability**, and the gap is unobservable by construction: the only instrument that would
measure it is an attempted re-derivation, which is the expensive thing nobody does until they must.
Fogbank's re-derivation cost the better part of a decade and something between $69M and $100M. The
measurement and the failure were the same event.

Two consequences follow, and the second is counterintuitive.

**Reconstruction is itself a compression and can be confidently wrong.** A receiver who supplies a
framing by pattern-match produces a plausible completion, not a question.

**For boundary-crossing artifacts, an explicit absence can be safer than a plausible but
scope-incomplete positive representation.** A question preserves uncertainty; a completion erases
it. This is not a general claim that losing everything is better than losing something — losing a
patient's whole record is obviously worse than keeping a partial one. It is narrower and it has a
condition: *when reconstruction is performed by pattern completion rather than verification,
intermediate familiarity may be more dangerous than complete unfamiliarity.* Stated that way it is a
secondary prediction (§11) rather than an institutional principle.

---

## 7. The historical record

The account claims to describe an old failure. It should therefore explain historical cases it did
not have in hand, and it should be able to say which cases it does *not* explain.

### 7.1 Fogbank as a Mode-2 case with a Mode-1 accelerant

The impurity was not omitted from the specification by an author who knew and declined to say. It was
outside the framing of "the process" — an uncontrolled input, which is exactly the category a process
specification is built to exclude. <cite index="18-1">It had never been explicitly documented, tested, or controlled, and a downstream step had come to rely on it implicitly.</cite> That is Mode 2 in its purest form: the framing was the operation, and the operation's own definition of itself excluded the load-bearing term.

Mode 1 supplied the accelerant. <cite index="17-1">Because the material was classified, detailed process documentation was either absent or overly restricted</cite>, and <cite index="16-1">there existed very few written records, perhaps due to the significant secrecy that veiled the programme.</cite> This is the general point about secrecy regimes: **compartmentalization is deliberate reconstructability destruction.** Each compartment receives a compression of the whole formed at a framing chosen elsewhere, and is structurally unable to see what it was not given. A regime that manufactures blind cells on purpose, for good reasons, should expect to lose knowledge at a rate proportional to its compartmentalization — and should therefore budget for re-derivation as a standing cost rather than as an emergency.

The recovery pattern is the one this paper predicts: not a better document, but re-running the
process, reproducing the original failure, and doing forensic work against surviving physical
material. **The corpus, not the compression, was what could be interrogated.**

### 7.2 Scurvy as a Mode-3 case

The Royal Navy solved scurvy with citrus in the eighteenth century and then lost the solution over the
nineteenth, with lethal consequences for polar expeditions into the twentieth. The standard account
assembles several causes: <cite index="23-1">a switch from Mediterranean lemons to West Indian limes, which have much lower vitamin C, with the extraction method reducing it further</cite>; <cite index="22-1">juice that came into contact with copper, which oxidizes vitamin C, so that the issued lime juice tested almost totally ineffective in 1918</cite>; and <cite index="24-1">steam shipping shortening voyages, so the reduced potency produced no immediate epidemic and the degradation went unnoticed.</cite>

What makes this Mode 3 rather than Mode 2 is the crucial part. The carried framing was *"citrus cures
scurvy."* The operative variable was ascorbic acid content, and **there was no field in which to
author it**: the concept did not exist. Carriage could not have been discharged, however diligent the
producer, because the dimension that needed carrying was not expressible in the receiving vocabulary.

The consequence is the signature of positional blindness rather than of missing data. As one recent
treatment of the episode puts it, <cite index="25-1">the evidence by itself did not point clearly to the truth; without the concept of a vitamin, the same observations could support multiple theories, and knowing which results to trust and which to explain away required concepts that did not yet exist.</cite> <cite index="25-1">The ptomaine theory made correct predictions — fresh meat prevents scurvy — even though it was completely wrong.</cite> A framework with no slot for a trace nutrient does not report an empty slot. It reports a coherent alternative theory.

Note what this implies for the repair. No amount of Mode-2 discipline recovers a Mode-3 loss. What
recovered it was contact with a vocabulary that carved differently — the isolation of vitamins as a
class, from outside naval medicine entirely.

### 7.3 The recurring shape: the load-bearing background variable

Fogbank's impurity and scurvy's ascorbic acid are the same object seen twice: **a variable that was
load-bearing and, from the producing position, was not a variable at all.** It was background, the
condition under which the work happened rather than a term in the work.

The pattern is common enough in the history of lost technique to be worth naming. Recovered Roman
concrete formulations turned on process detail that surviving texts do not record because it was
simply how the work was done; recovered wootz steel is associated with trace elements present in
particular ore sources, which were not a choice anyone recorded making. In each case the recovery ran
through the physical corpus rather than the documentary compression. *(These two are stated at lower
confidence than §7.1–§7.2 and are offered as pattern, not evidence.)*

Design consequence, and it is actionable: **the carriage rule should extend to uncontrolled inputs,
not only to controlled ones.** A specification that lists what it controls has, by construction, not
listed what it depends on. Fogbank's repair was precisely this — the impurity was moved from
background to foreground, added deliberately, and monitored explicitly.

### 7.4 The positive control: a discipline that does not have this failure

An account of this kind must be able to *fail to fit*, and it should also be able to identify where
the failure is absent, or it explains nothing.

Mathematics is the case. A proof carries its framing by construction: the premises are stated, and
the artifact is not the conclusion but the derivation from declared assumptions. Consequently a proof
from 1850 can be re-derived today by a reader who shares no institution, no employer, and no living
informant with its author — reconstructability is irrelevant because carriage is total. This is what
a maximally carried framing looks like, and it is also a measure of the cost: mathematics achieves it
by making the authoring of framing the *entire content* of the artifact.

And the exceptions prove the mechanism. Where nineteenth-century analysis left conditions implicit
("obviously continuous"), and where the Italian school of algebraic geometry relied on shared
intuitions never written down, results did become unreliable and required later re-derivation under
explicit hypotheses. The failures cluster exactly where framing was left to the reader's
reconstruction.

### 7.5 The institutions that exist to prevent this

The failure is old enough that societies have built institutions against it, and those institutions
are more legible under this account than under the ones usually offered. Each is a **carriage
technology**: a rule about what must accompany a thing when it crosses a boundary, backed by
something other than the goodwill of the producer. And each embeds, explicitly or otherwise, a bet
about the future consumer's reconstructability.

**The patent bargain is a carriage rule with a legal enforcement mechanism.** In exchange for a
time-limited monopoly, the applicant must publish a description sufficient to enable a practitioner
to make and use the invention. That is precisely §9.1's second clause, made a condition of the
grant. What makes it remarkable for this paper is the standard of sufficiency: enablement is
assessed against a *person having ordinary skill in the art*, which is to say **patent law contains
an explicit, declared parameter for reconstructability.** The specification need not carry what a
competent practitioner of the period could reconstruct; it must carry the rest.

Two consequences follow, and both are predictions this account makes and patent practice confirms.
First, the standard is *time-indexed and drifts*: a specification that was enabling when filed can
become non-enabling as the surrounding practice it relied on decays, because the reconstruction it
assumed is no longer available. The document does not change; the reader does. Second, the
enablement requirement is systematically weakest exactly where §7.3 says the danger is — process
knowledge, tacit skill, and uncontrolled inputs. A patent lists what the inventor controlled. It
does not list the impurity.

**Trade secrecy is the opposite bet, and it is the Fogbank bet.** Retain Mode 1, decline carriage,
and rely on the corpus — continuous production, living practitioners — to preserve what the document
does not. It works indefinitely while the practice runs continuously and fails catastrophically at
the first discontinuity. Formulations that have survived centuries under secrecy have survived
because they were *never* discontinued; Fogbank was discontinued for eleven years.

**Open source publishes the corpus rather than a compression**, which is a different move from
carriage and stronger where it applies: re-derivation becomes available to any party, without
permission, which is §9.2's requirement made a property of the artifact rather than of the
institution. But source availability is not framing carriage, and the gap shows up in exactly the
predicted places. Reproducible-build efforts are a re-derivation discipline in the strict sense —
recompute the binary from the source under an independently specified procedure and compare —
existing because "the source is public" does not establish that the shipped artifact came from it.
And abandoned projects preserve corpus while losing framing: the code survives, and why *this*
constant, and what depends on it, does not.

Software has independently derived §7.3's law, under a different name. **Hyrum's Law** — with a
sufficient number of users, every observable behaviour of a system will be depended upon by
somebody, regardless of what was promised — is the load-bearing background variable stated as an
engineering regularity. Fogbank's impurity is Hyrum's Law in a purification plant: a property nobody
specified, nobody controlled, and something downstream relied on.

**Metrology is the case where the repair was actually completed, and it took 130 years.** Until 2019
the kilogram was defined by a platinum–iridium artifact in a vault near Paris. Its official copies
are called *témoins* — **witnesses** — which is either a coincidence or evidence that the vocabulary
is forced. The system had the defect this paper describes, in its purest possible form: <cite index="44-1">the prototype "has not so far changed relative to itself"</cite>, so no measurement made within the system could detect its drift. Drift was visible only as **divergence among the copies**, <cite index="42-1">averaging on the order of 50 μg over a century</cite>, and even then the direction was undecidable from inside — one cannot say whether the prototype lost mass or the witnesses gained it, because the framing under which "one kilogram" was defined was the artifact itself.

<cite index="41-1">The 2019 revision replaced it with a definition fixing the numerical value of the Planck constant, taking effect on 20 May 2019</cite>, and the significance for this paper is not precision. It is that a **corpus-anchored standard was replaced with a fully carried framing**: the definition now travels complete, and any sufficiently equipped laboratory can re-derive the unit without consulting an artifact or trusting a custodian. That is the §7.4 mathematics repair applied to measurement, and the cost of achieving it — a century of divergence, decades of preparatory work, an international apparatus — is the honest price of moving a standard from corpus to carriage.

**Archives, seed banks, type specimens, and version control are the corpus half of the same
strategy**, and their rationale is precisely §3.4: because a lost capability has no positive form,
you cannot enumerate what you will need. The type specimen exists so a classification can be
re-derived rather than trusted; the seed vault exists because nobody can list the cultivars a future
climate will require. These institutions do not carry framing. They preserve the substrate against
which framing can later be reconstructed, which is the only available hedge against Mode 3.

Read together, the family sorts cleanly:

| Institution | Strategy | Bet about the consumer | Fails when |
|---|---|---|---|
| Patent | Carriage, legally enforced, against a declared reconstructability standard | A skilled practitioner of roughly this period | The practice the standard assumed decays; the load-bearing term was uncontrolled |
| Trade secret | No carriage; corpus preserved by continuous practice | There will be no discontinuity | There is a discontinuity |
| Open source | Corpus published; carriage optional | Anyone, given the substrate, can re-derive | The framing (why, and what depends on it) was never written |
| Metrological constants | Carriage total; corpus eliminated | Any competent lab, indefinitely | The constants themselves are misdetermined |
| Archive, seed bank, type specimen | Corpus preserved without carriage | Someone later will know what to ask | The substrate degrades, or nobody knows it exists |
| Classification | Deliberate reconstructability destruction | The knowledge will be needed only while the programme runs | The programme pauses (§7.1) |

The general statement: **an institution of this kind is a bet about the framing distance and
reconstructability of a consumer who does not yet exist.** Where the bet is stated — the PHOSITA
standard, the enablement requirement, the definition of a unit — the institution is auditable. Where
it is implicit, the institution is running §5.4's manifest at civilizational scale.

---

## 8. The limiting case: long horizons

Deep-time problems are this paper's parameters taken to their limits, and two literatures — one
about messages, one about societies — supply independent tests of the repair.

### 8.1 Carriage at zero reconstructability

When the receiver is separated from the producer by ten thousand years, reconstructability is not low
but **zero**, and framing distance is unbounded: the receiver shares no language, no institutions, no
units, and possibly no species-typical cognition. Carriage — the repair of §9 — is the whole problem,
and the field has been at it since 1980.

<cite index="28-1">Nuclear semiotics was established by the American Human Interference Task Force in 1981 to design warning messages for nuclear waste repositories on the order of ten thousand years, and a 1993 Sandia report recommended that such messages be constructed at several levels of complexity.</cite> The proposals are instructive precisely because they abandon message content as the primary channel. <cite index="34-1">The task force concluded that any successful attempt to communicate across deep time will have to rely on monumental architecture and markers: structures durable enough to require no maintenance for ten thousand years, and disturbing enough to inspire people to transmit knowledge about them across generations.</cite> <cite index="29-1">Designs explored physical forms conveying dangerous emanations, shapes evoking bodily harm, and the concept of shunned land.</cite> Others proposed institutional carriage rather than artifact carriage — <cite index="33-1">an "atomic priesthood" transmitting the warning through myth and ritual, modelled on religious institutions that have carried messages for two thousand years</cite> — or biological carriage, in the ray-cat proposal.

Two findings from that literature bear directly on this paper.

**Carriage at zero reconstructability degrades into designing for re-derivation.** The markers are not
really messages; they are *provocations to investigate*, aimed at a future archaeologist who will dig
regardless. The field's own critics make the point sharply: <cite index="36-1">even menacing designs risk reinterpretation as indicators of treasure or ritual significance, potentially inviting rather than discouraging exploration, as with Egyptian tombs explicitly marked against intrusion that nonetheless attracted looters.</cite> A carried framing that the receiver reads under a different framing is exactly the failure this paper describes — committed by the repair, at civilizational scale.

**The stakeholder problem is structural, not moral.** In deep-time cases the party who bears the cost
is non-contemporaneous. There is no occupant of the affected position to confront the claim when it
comes due, to price the retreat, or to say *your framing excluded me*. Every mechanism in §9 assumes
a receiver who can complain. Where the receiver does not yet exist, no boundary discipline is
enforced by anyone with standing, and the only remaining check is a party in the present who has
deliberately taken the absent seat — which is a choice, not a control.

This is the honest terminus of the message case, and it generalizes downward: **the institution
must maintain a position from which the not-yet-existing consumer's framing can be represented, or
that consumer's framing is simply absent from every artifact.** Long-term thinking, on this reading,
is not a virtue of temperament. It is the practice of occupying a seat nobody is standing in.

### 8.2 Societies compress their capability portfolios, and cannot enumerate the loss

The second literature is about what societies stop being able to do.

Specialization is compression. A society's portfolio of capabilities — what its members can make,
grow, repair, navigate, and survive — is selected under a framing, and the framing is *the current
environment*: what is worth knowing given present climate, present trade, present energy. The
capabilities that fall outside the selection are not deleted by decision. They lapse, because nobody
is doing them, in the same way and for the same reason that a specification omits an uncontrolled
input.

The archaeology of the American Southwest supplies the mechanism in one sentence, from the work on
the Ancestral Puebloan abandonments: as the society aggregated and grew more dependent on
agriculture and on itself, *"these parts are no longer redundant. They are part of an integrated
whole"* — one could no longer peel off a lineage here and a lineage there and have them go their own
way. Redundancy is the uncompressed corpus. Integration is the compression ratio rising.

And here the account makes a prediction that the historical record repeatedly confirms: **a society
has no register of the capabilities it has lost.** A lost capability has no positive form (§3.4).
There is no inventory of skills nobody practises, because an inventory is built from what is
present, and what is absent supplies no entry. So capability loss is exactly the failure this paper
describes at civilizational scale: frame-complete reads as complete, the portfolio looks adequate
because every item in it belongs, and the omission is discovered only when the environment moves and
something is required that nobody can do.

Which means §6.2 holds here in its strongest form. The measurement and the failure are the same
event. Drought is the re-derivation run against the agricultural portfolio; the collapse is the
result being returned. Nobody was notified when the last generation that could live off the
periphery stopped doing it, because nothing changed in any record on the day it happened.

The contemporary version, from a case in which the compression is recent enough to watch: Bedouin
who lived at the edge of the Rub' al Khali for millennia now largely live in state-built settlements
and hire others to herd. The skills required to survive there are the corpus; the settled portfolio
is the compression, framed by a subsidy that is finite. Whether those skills can be re-derived when
the framing changes is an open empirical question of exactly the Fogbank kind — recoverable
expensively if the practice or its practitioners survive somewhere, and not at all if they do not.
The essay that prompted this section ends on the reconstructability statement, correctly: *the
difference between us and the Bedouin is that we do not know how to survive in that climate.*

### 8.3 Framing monoculture removes the position from which re-derivation is possible

There is a second, less obvious systemic result, and it is the one this account contributes that
resilience arguments usually do not state.

Interconnection reduces **framing distance**. A globally integrated system converges on shared
units, shared standards, shared practices, shared models of what matters — and by §6.1 that should
*reduce* exposure, since producer and consumer increasingly carve the world the same way. Handoffs
get easier. Translation costs fall. This is real and it is why integration is chosen.

But re-derivation requires a party positioned **outside** the artifact's framing (§9.2), and Mode-3
blind cells are discoverable only by contact with a vocabulary that carves differently (§5.3). So
convergence buys smoother handoffs at the cost of the only instrument that detects framing failure:

> **A monoculture of framings has no outside. It reduces the cost of every handoff and eliminates
> the position from which any handoff could be checked.**

This reframes what is lost when an alternative way of living, measuring, or knowing disappears.
Under a diversity argument, the loss is of options. Under this account it is of **instrumentation**:
each surviving alternative framing is a probe for a class of defect the dominant framing is
constitutionally unable to detect in itself. The Bedouin, on this reading, are not only a
population; they are an occupied position from which the settled portfolio's omissions are visible,
and their assimilation removes both the way of life and the only party who could have re-derived it.

The design consequence is uncomfortable and, I think, correct: **maintaining alternative framings is
not sentimentality or hedging against taste. It is maintaining verification capacity**, and it
should be budgeted as such — which is the same argument as §7.5's for seed banks and type specimens,
extended from substrate to standpoint.

### 8.4 Condition-indexed markers, and what receding water reveals

There is a marker design that solves part of the deep-time problem, and it is worth studying because
it fails in a way the account predicts precisely.

Embedded in the beds of central European rivers — the Elbe above all, with examples on the Rhine,
Danube and Moselle — are **hunger stones**: boulders carved with the years of historic droughts and
with admonitions to the future. <cite index="50-1">The best known, at Děčín, reads *Wenn du mich siehst, dann weine* — "if you see me, then weep" — and is one of dozens engraved to mark river levels during historic droughts and to warn later generations of the famine and hardship that followed each time they became visible.</cite> <cite index="55-1">The Děčín stone commemorates, before 1900, the droughts of 1417, 1616, 1707, 1746, 1790, 1800, 1811, 1830, 1842, 1868, 1892 and 1893.</cite> <cite index="52-1">Receding waters in 2022 exposed these alongside Roman camps, ghost villages and second world war shipwrecks.</cite>

**The design move is that the artifact is indexed to its own condition.** Unlike a sign, a hunger
stone is invisible under normal conditions and becomes legible exactly when the situation it
describes obtains. It cannot be misread as treasure or as ritual architecture, because it is not
available to be read at all except in the state it is about. Compare the nuclear markers of §8.1,
which must be permanently visible and are therefore permanently subject to reinterpretation. The
hunger stone solves that by making the environment the selection rule for the read site.

**What it actually carries is a denominator, not a warning.** By the time the stone is legible the
drought has happened; there is no lead time and nothing to prevent. What it delivers instead is the
interval: living memory covers two or three generations, and the stone extends the record to six
centuries. It says *your sample is too short.* That is framing carriage across time, cut with a
chisel — as-of dating for a rare-event distribution, aimed at the specific error of treating an
unprecedented-in-memory event as unprecedented.

**And it has this paper's failure, in its own design, on the very axis it was built to serve.**
<cite index="52-1">The Elbe stones have appeared more regularly, notably during the 2018 drought, since a dam was built in the 1920s.</cite> The stones encode a threshold — *water this low* — framed by a pre-dam river. The hydrology moved; the artifact cannot report that it moved; and a visible stone today does not mean what a visible stone meant in 1616. The framing drifted underneath the best-designed long-horizon marker in Europe, and the marker went on emitting its original claim. Nothing about the stone distinguishes "the drought of record has returned" from "the river was re-plumbed."

The repair, when it came, was the one §9.2 predicts: <cite index="53-1">a 2020 study verified 40 of 47 drought marks across twelve Elbe hunger stones by cross-referencing the inscriptions against chronicles, parish registers and early hydrological observations.</cite> The marker was made checkable only by re-derivation against an independent substrate — not by reading it more carefully.

A small instance of the same thing sits in the sourcing of this section. Some accounts attribute the
famous sentence to the 1616 inscription; others report that <cite index="56-1">a boatman and riverside innkeeper, Franz Mayer, etched those words during low water in 1904</cite> on a stone whose oldest legible date is 1616. The two are compressions of one object formed at different framings — the stone's, and the inscription's — and neither carries which it is. I record the disagreement rather than resolving it.

**Finally, the wrecks.** The shipwrecks, bones, Roman camps and drowned villages that surface in a
drought are not carriage at all; they are **corpus revealed**. Nothing was transmitted and nothing
was designed. A parameter moved — the water level — and the substrate that the normal framing
occluded became available. In the vocabulary of the parent program this is a perturbation: hold
everything fixed, vary one dimension, read off what appears. The reason such events feel like
revelations is that they are: the world did not change, the selection rule did, and everything that
surfaces had been continuously present and continuously unaddressable.

Which is the whole account in one image. A drought is a re-derivation run against a landscape. The
hunger stone is what a previous generation deliberately placed in the path of that re-derivation,
knowing they could not reach us any other way.

---

## 9. The repair: boundary carriage and external re-derivation

### 9.1 Two rules

**The remedy cannot be an honesty norm** (P1). Nobody was dishonest. A discipline addressed to
withholding reaches Mode 1 and does not reach Modes 2 and 3, where the missing information was never
a datum anyone held or was not expressible at all. What reaches those is an **artifact discipline**:
a rule about what a value must carry when it crosses a boundary, enforced at the boundary,
independent of intent.

> **No epistemically meaningful value crosses a boundary without its status** — enough to distinguish
> measured / generated / verified from absent / not-run / not-found.
>
> **No compression crosses a boundary without the framing it was formed at** — its range,
> denominator, unit, interval, selection rule, and residue.

Status carriage would have caught the `0.0`. Framing carriage would have caught the pin manifest.
Neither would have caught Fogbank, whose missing term was outside the framing of the framing — which
is why §7.3's extension (carry uncontrolled inputs, not only controlled ones) is the specifically
Fogbank-shaped rule, and why Mode 3 needs §5.3's foreign-vocabulary probe instead.

A practical caution the reviews raised and this paper endorses: the full framing list is expensive at
every boundary, and partial adoption may land the institution in the moderate-reconstructability
danger zone of §6.2 — carrying enough to look framed and not enough to be. A **minimal sufficient
carriage rule** is worth identifying empirically; the current best guess is *status, denominator,
interval*, with residue on any claim that will be cited.

### 9.2 Carriage makes an artifact interpretable; re-derivation makes it checkable

Declaring a framing produces a new artifact, which is a compression formed at a framing. The
regress is real, and the useful way to state it is not as an unsolvable philosophical problem but as
a **division of labour**:

| Control | What it prevents | What it cannot prevent |
|---|---|---|
| Status carriage | Conflation of absent, failed, generated, measured, verified | Misstated scope of the reported status |
| Framing carriage | Unmarked change of range, interval, denominator, unit, criterion, residue | Errors in the framing record itself |
| Re-derivation | Mismatch between an artifact's claim and an independently accessed substrate | Errors shared by the source and the derivation |
| Orthogonal or adversarial check | Shared-mode errors, checker defects, scope capture | Unlimited regress; remains costly and partial |

The sharper statement of what re-derivation buys:

> **An artifact cannot certify the completeness of the scope that produced it, because the scope is
> part of what would have to be certified. Completeness therefore requires an operation that is not
> closed over the artifact.**

That is a precise reason external verification is necessary, and it is stronger than "there is a
regress." It also specifies what "outside" must mean, since every verifier has a framing too.
"Outside the artifact's framing" is satisfied by at least one of: **accessing a substrate the
artifact's criterion did not select; executing an independently specified procedure; using a
different instrumentation path; or holding authority to challenge the artifact's implicit
denominator.** Re-derivation does not end regress in the philosophical sense; it **terminates an
operational verification loop at a chosen institutional boundary**, and the choice of boundary is
itself declared.

Evidence for the necessity, from the companion institution: in one dense working period, nine defects
were committed *inside* the repairs meant to address this class — a manifest whose totals row
disagreed with the table beneath it, a drift list reporting twelve changed sources when two had
changed, a self-test that aborted rather than reporting failure. Every one of those artifacts carried
its framing scrupulously. **Not one of the nine was caught by a gate.** Every one was caught by a
person or script comparing a claimed number against the artifact it described.

### 9.3 Improving observability moves the frontier rather than closing it

The consequence worth promoting, because it is the least obvious and the most operational:

> **Every verification layer is another compression boundary.** A manifest, a gate, a checker, and a
> coverage report are all artifacts formed at framings, subject to the same failure as the substrate
> they verify. Mature carriage discipline therefore does not eliminate silent failure; it **relocates
> the silent-failure frontier upward**, from the substrate into the instruments.

This predicts where to look in a well-instrumented system (§11, Prediction 4) and it explains why
mature systems can feel *more* rather than less exposed: the residual failures are in the parts
everyone trusts most, and they are the parts nobody re-derives, because re-deriving the checker feels
like a waste of a working checker.

The structural corollary, which is the same claim at the level of a single check:

> **An unstated exemption is indistinguishable from an unnoticed one, and no gate separates them,
> because the gate is downstream of the exemption.** The frame a check operates within is chosen
> before the check runs; a check cannot report what its own scope excluded.

---

## 10. What this account excludes

Four failure families, of which this paper concerns one:

1. **Wrong content** — ordinary error. The artifact misstates what its own framing covers.
2. **Missing content** — omission within a framing that had a slot for it.
3. **Scope mismatch** — the subject of this paper, and particularly dangerous when it masquerades as
   (1) or (2).
4. **Unstable content** — stochastic or measurement variation, where the same input yields different
   outputs.

Three explicit exclusions and one genuine break.

**Structural contradiction is not a framing mismatch.** Two axiom sets that derive a contradiction do
so inside a single framing, immediately, with no read site. Declaring a scope does not repair an
inconsistency; the repair is revision.

**Stochastic variation is not a framing mismatch.** Treating it as one produces the wrong instrument:
a declaration rather than repeated draws and a noise floor.

**Loud failures are out of scope.** A destructive replacement that turns out to be needed announces
itself. There is no read site fooled and no success-shaped token — which is why "prove before you
replace" is a discipline about warranting a claim rather than a description of quiet failure.

**The break.** The account predicts that carrying the framing repairs the failure, and §9.2–§9.3 show
it does not, because the carried framing is itself framed. The account therefore unifies the *form*
of the failure and the *direction* of the repair, and does **not** establish sufficiency. Mode 3
sharpens the same limit: where the framing is inexpressible, carriage cannot be discharged at all,
and the discipline reports compliance.

---

## 11. Predictions

**Prediction 1 (tests E1).** Give matched reviewers the same artifact under two protocols —
*recognition* (read it and confirm it is complete and correct) and *enumeration* (write the
instruction a receiver would need to execute it). Omission-detection rates should separate sharply;
detection of *present but wrong* content should not. *Falsifier: no separation, or separation on
both.*

> **[ATTEMPTED 2026-08-13; the attempt does not count.]** Run at 60 pre-registered units
> (`audits/2026-08-13_rqd_recognition_vs_enumeration/`, ISSUES OQ-293). Frozen rule returned no
> separation on omissions (Δ −0.07 pooled, +0.08 informative, floor +0.20). **Withdrawn as a test
> of E1:** subjects named ~8 gaps per response (488 across 60) while scoring credited the single
> authored omission, so the measure was selection among many real gaps rather than detection.
> **The prediction as written above is what permitted that** — it says rates "should separate" and
> never says against what inventory detection is scored, which is this paper's own §2 defect
> (an artifact faithful under its producing selection rule, consumed under a different one) landing
> in the paper's own falsifier. **Repair the prediction, not just the experiment:** score each
> arm's *recall over a frozen exhaustive gap inventory per artifact*, so both arms share a
> denominator. What survived: enumeration produced 27% more text and found fewer of **both**
> defect kinds, and omissions were harder than present-but-wrong content in **both** arms — the
> §4 asymmetry, observed but not attributable to the protocol.

**Prediction 2 (tests E2).** Hold compression constant and vary shared history between producer and
consumer — same team, adjacent team, stranger, non-persistent worker. Silent-failure incidence,
measured per handoff, should track reconstructability rather than compression ratio alone.
*Falsifier: incidence flat in shared history.* Secondary prediction, from §6.2: incidence should be
**non-monotonic**, peaking at intermediate familiarity, where pattern-completion is available and
verification is not felt to be necessary.

**Prediction 3 (tests P1).** Compare an honesty-norm intervention ("state your assumptions") against
a carriage rule ("no summary without denominator, interval and residue"). The mechanistic prediction
is specific rather than competitive: **carriage should reduce the class of failures honesty norms
leave untouched — those in which the producer could not report the framing because the framing was
never represented at the production site — and should show little advantage on failures where the
producer knew and did not say.** *Falsifier: equal effect across both classes.*

**Prediction 4 (tests P2, §9.3).** In systems with mature carriage discipline, residual silent
failures should concentrate in the instruments — manifests, gates, checkers, controls — rather than in
the substrate, and should be caught predominantly by re-derivation rather than by gates. *Falsifier:
residual failures distributed evenly across strata, or caught predominantly by gates.* This is the
cheapest of the four to attempt in an existing evidence-gated system.

**Prediction 5 (tests Mode 3, §5.3).** Blind cells are not discoverable from inside a representational
system by diligence. Method: have a system's own maintainers enumerate its representational gaps;
separately, run a foreign vocabulary against it and route what it has fields for that the system
lacks. The overlap should be low, and the foreign-probe set should contain gaps the maintainers'
list does not. *Falsifier: high overlap, which would mean introspection suffices and Mode 3 collapses
into Mode 2.*

**Prediction 6 (tests §7.5).** Processes that were disclosed under an enablement requirement should
be cheaper to re-derive after a production discontinuity than otherwise-comparable processes held as
trade secrets, and open-corpus processes cheaper still — with the advantage concentrated in cases
where the discontinuity exceeded the working life of the original practitioners. *Falsifier:
re-derivation cost independent of disclosure regime once discontinuity length is controlled.* This
is a historical-record study, not an experiment, and the confound to beat is that firms choose the
regime partly on how tacit the process already is.

**Prediction 7 (tests §8.4).** Condition-indexed markers and always-visible markers should trade off
in opposite directions: condition-indexed markers should show lower misinterpretation rates and near-zero
lead time, always-visible markers the reverse — the Aneyoshi tsunami stones, permanently legible and
widely disregarded, against the hunger stones, unreadable until the event and reliably noticed when
they appear. *Falsifier: no systematic difference in either misinterpretation or lead time once
hazard frequency is controlled.* The design consequence, if it holds, is that the two are complements
rather than rivals, and that a hazard regime carrying only one of them has an unstated framing about
which failure it can afford.

---

## 12. Related work

This paper differs from the accounts below by treating the mismatch as a consequence of framing at
the boundary rather than as a taxonomy of failure manifestations.

**Differential observability.** Gray failure (Huang et al., 2017) identifies the dominant cloud
failure mode as a mismatch between what an application experiences and what its detector observes;
fail-slow studies (Gunawi et al., 2018) supply the incident-report tradition. Differential
observability is the special case where the two framings belong to an application and its detector.

**Silent failure in LLM systems.** Wu (2026) derives a five-class mechanism-oriented taxonomy from 22
production postmortems and names *fail-plausible* — a system converting an internal error into fluent
false output — as the escalation specific to systems that speak. The finding that long-latency
failures live in *seams* between simple correct components is, on this account, expected: a seam is
where two framings meet without either being stated. Advani (2026) measures false success at
benchmark scale with text-independent ground truth.

**Evidence gating.** EviBound (Chen, 2025) and evidence-chain frameworks for autonomous research
implement the status clause of §9.1 as system architecture with measured reduction in unsupported
claims. They provide the strongest available evidence that carriage rules work, and they operate on
status rather than framing.

**Long-context behaviour.** The position, length, multi-turn and absence-detection results cited in
§4 make the working-set premise quantitative for model workers rather than merely plausible.

**Metascience.** Preregistration (Chambers, 2013; Nosek et al., 2018) is a framing-fixing device: it
forces the selection rule to be authored *before* the selection, at the one moment the author still
experiences it as a decision rather than as the operation. Assay controls play the same role for
probes. This account provides one explanation for why such devices generalize beyond their home
fields and why they feel expensive.

**Organizational memory and deep time.** The nuclear-semiotics literature (§8.1) is the extreme case
of carriage design, and the knowledge-loss cases of §7 are its natural history. The account offered
here is that these are one problem at different values of one parameter.

**Intellectual-property and disclosure institutions.** The patent bargain, trade-secret doctrine, and
open-source licensing are usually analysed as incentive mechanisms. §7.5 reads them additionally as
carriage technologies with declared reconstructability standards, which is a complementary rather
than a rival account: the incentive analysis explains why a producer discloses, and this one explains
what disclosure has to contain to be worth anything decades later.

**Sheaf-theoretic and standpoint accounts.** The relation in §3.6 between global sections and local
decision content is the parent program's territory (`deferential_realism_paper_v8.md`,
`the_perturbation_principle.md`): structure invariant, classification seated, and the invariant
legible only by varying a parameter rather than by looking harder from one position. The
contribution here is the boundary consequence — that the inverse relation between utility and
transportability makes the failure structural rather than remediable by aiming at neutrality.

**Hazard markers and historical hydrology.** The hunger-stone literature (§8.4) supplies the only
worked example I have found of a marker indexed to its own triggering condition, and the 2020 Elbe
verification study is a re-derivation discipline applied to a six-century marker series.

**Collapse and resilience.** Work on societal collapse — the Bronze Age horizon, the Ancestral
Puebloan abandonments, and the general efficiency-versus-redundancy tradeoff — supplies §8.2's
material. The contribution offered here is narrow: not a theory of collapse, but the observation that
capability loss is undetectable by self-inventory for the same structural reason an omission is
undetectable in a compression, and that framing monoculture removes the external position on which
re-derivation depends (§8.3).

---

## Appendix A: Claim table

**ANALYTIC — derived; refuted by exhibit, not data.**

| # | Claim | Kill condition |
|---|---|---|
| A1 | Any party producing an action, judgment, or handoff over a corpus not jointly available in its working set must rely on a representation that does not preserve all decision-relevant relations | An institution operating at scale with no selection at any boundary |
| A2 | Framing non-identifiability: the framing is not entailed by the compressed content; where present it is additional authored content | A general artifact form whose framing is recoverable from the compressed content alone |
| A3 | No procedure operating only on the compressed content can enumerate omissions not positively represented by the artifact or its declared provenance | Such a procedure |
| A4 | When accepted as sufficient warrant at a new read site, a compression receives the operational authority of a claim whose scope was settled elsewhere and is not inspectable | A boundary protocol under which unscoped artifacts do not transfer warrant |
| A5 | Framing-invariant content is prohibitive rather than selective; therefore neutrality is not an alternative to carriage, and utility and transportability are inversely related | A framing-invariant claim that selects among locally available actions without any seated premise |

**EMPIRICAL — measured; currently supported unevenly.**

| # | Claim | Support | Owed |
|---|---|---|---|
| E1 | Parties checking an artifact default to recognition and enumerate only when forced | Model evidence under tested conditions; four rulings in one institution (embedded case material) | Prediction 1 |
| E2 | Silence scales inversely with reconstructability, and reconstructability is unobservable to the institution relying on it | Fogbank; the scurvy episode; argued in §6 | Prediction 2 |
| E3 | Mode-3 blind cells are not discoverable from inside a representational system | The scurvy episode; one census against a foreign source | Prediction 5 |
| E5 | A claim that has never been perturbed is indistinguishable from an invariant; apparent universality measures the range of positions sampled | A method for distinguishing local from global sections without varying the position |
| E4 | A society cannot enumerate its lost capabilities; capability loss is measured only by the event that requires them | Historical collapse and knowledge-loss cases (§8.2) | A society maintaining an accurate register of what it can no longer do |

**PROPOSED — design consequences.**

| # | Claim | Falsifier |
|---|---|---|
| P1 | The remedy must be an artifact discipline rather than an honesty norm | Honesty-norm interventions reducing Mode-2 failures comparably |
| P2 | Carriage is necessary and not sufficient; termination requires re-derivation from outside the framing | A carriage discipline terminating without external re-derivation |
| P3 | Mature carriage relocates the silent-failure frontier into the instruments rather than eliminating it | Prediction 4's falsifier |
| P4 | Disclosure institutions (patent, open corpus) lower post-discontinuity re-derivation cost relative to secrecy regimes | Prediction 6's falsifier |
| P5 | Diversity of framings is verification capacity, not merely optionality; monoculture removes the external position re-derivation requires | A demonstrated re-derivation performed entirely from inside a single framing on a Mode-3 defect |

---

## Appendix B: Notation

Corpus *C*; framing Π; representation *R*(Π, *C*); interpretation *I*(Π, ·). Compression means
*R*(Π, *C*) ≠ *C*, which is not the problem. The problem is

> *I*(Π′, *R*(Π, *C*)) ≠ *I*(Π, *R*(Π, *C*))

while *R*(Π, *C*) does not identify Π — so the consumer cannot determine which interpretation it is
licensed to make. Carriage appends Π to the artifact. Re-derivation recomputes some property of *C*
under a Π″ chosen independently of Π. Reconstructability is the probability that a consumer recovers
Π from context without carriage.

---

## References

Advani, L. (2026). From Confident Closing to Silent Failure. arXiv:2606.09863.

Chambers, C. D. (2013). Registered Reports. *Cortex* 49(3), 609–610.

Chen, R. (2025). Evidence-Bound Autonomous Research (EviBound). arXiv:2511.05524.

Du, Y., et al. (2025). Context Length Alone Hurts LLM Performance Despite Perfect Retrieval. EMNLP.

Fu, H. Y., et al. (2025). AbsenceBench: Language Models Can't Tell What's Missing. arXiv:2506.11440.

Gunawi, H. S., et al. (2018). Fail-slow at scale. USENIX FAST.

Hsieh, C.-P., et al. (2024). RULER. arXiv:2404.06654.

Huang, P., et al. (2017). Gray failure: the Achilles' heel of cloud-scale systems. HotOS.

Laban, P., et al. (2025). LLMs Get Lost in Multi-Turn Conversation. arXiv:2505.06120.

Liu, N. F., et al. (2024). Lost in the Middle. *TACL* 12, 157–173.

Modarressi, A., et al. (2025). NoLiMa. ICML.

Nosek, B. A., et al. (2018). The preregistration revolution. *PNAS* 115(11), 2600–2606.

Wu, W. (2026). When Errors Become Narratives. arXiv:2606.14589.

*Compaction as Epistemic Failure*, arXiv:2607.13071 (single-author case study; used as such).

**Historical cases (verified 2026-08-12).** Fogbank: NNSA/LANL and GAO material as reported by the
Federation of American Scientists, *The War Zone*, and the consolidated public account; production
1975–1989 at Y-12, restart decision c. 2000, recertification 2008, published cost figures ranging
$69M–~$100M. Scurvy: contemporary accounts of the lemon-to-lime substitution, copper-vessel
processing, and the 1918 potency trials; Ceglowski's *Scott and Scurvy* for the polar sequence.
Nuclear semiotics: Human Interference Task Force (1981–1984); Sandia National Laboratories marker
studies (1992–1993); WIPP permanent-markers planning. Metrology: International Prototype of the
Kilogram and its *témoins*; divergence on the order of 50 μg per century among prototypes; CIPM
redefinition in terms of the Planck constant, effective 20 May 2019. Southwestern archaeology: the
Ancestral Puebloan aggregation-and-abandonment sequence, Kohler's redundancy remark as quoted in
Johnson, *Vanished: A Pueblo Mystery*, New York Times, 8 April 2008. Rub' al Khali: Webster, *The
Empty Quarter*, National Geographic, February 2005, and cafebedouin, *The Bedouin Way of Life in Rub'
al Khali* (2017), which prompted §8.2–§8.3. Hunger stones: Guardian, *Hunger stones, wrecks and
bones*, 19 August 2022; NPR and AP coverage of the 2018 Elbe exposures; the 2013 Czech historical-drought
study and the 2020 verification of 40 of 47 marks across twelve stones; attribution of the Děčín
inscription is disputed between 1616 and 1904 (§8.4).

`seat-theorem-v1.md` — Premise 2's strong form. `the_perturbation_principle.md` — the control
requirement discharged in §10. `The Amnesiac Institution` v0.6 — the worked institution.

---

*CC0 Universal. Draft v0.2, 2026-08-12. Six independent reviews of v0.1 were taken in part: the
narrowing of A1–A4, the split of A2 into definition and proposition, warrant transfer as the
formulation of A4, the phenomenological definition of concealment, the qualitative form of the
exposure law, the boundary and compression definitions, the expansion of §5.4's instances, the
mechanistic reframing of Prediction 3, and the promotion of the frontier-relocation result. Not
taken: full formalization of the exposure relation; removal of Premise 2 (retained in a strong and a
weak form, with the argument resting on the weak); and the several suggestions to open with the claim
table, which is now Appendix A. v0.3 was produced as targeted edits against a byte-identical copy of
v0.2; the diff is the witness. New in v0.3: §7.5, §8.2, §8.3, Prediction 6, claims E4/P4/P5, and the related-work
paragraphs on disclosure institutions and collapse. New in v0.4: §3.6, §8.4, Prediction 7, claims
A5/E5, and two related-work paragraphs. Everything else is unchanged by construction.*
