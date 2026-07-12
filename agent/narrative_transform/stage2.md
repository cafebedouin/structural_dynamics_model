## STAGE 2: NATURALIZATION

**Model:** Claude or equivalent creative model
**Input:** Stage 1 symbolic specifications (anonymized) + Narrative Translation Guide (logic_narrative_translation.md) + [optional] Invariant Contract and Break Contract from Stage 0
**Output:** A world to inhabit — and a substrate built to hold what the formalism could not carry.

### Purpose

Stage 2 is the creative bridge. It receives a pure symbolic constraint network — variable names, χ values, type classifications, transformation rules — and translates it into a specific, inhabitable world.

Stage 2 also does a second job that no other stage performs, and the pipeline fails silently when it is skipped: **invariant recovery.**

The formal channel upstream represents every constraint as `Type(C, I)` with `χ = ε × π × σ`. That representation is lossy in one specific, repeatable way. It cannot hold a *real that binds everyone equally and is owned by no one* — such a thing survives formalization only as a near-zero-ε "Mountain," stripped of the fact that it stops being itself the instant anyone tries to use, measure, or own it. And it cannot hold *the absence of a neutral floor* — the founding choice beneath the situation itself — which survives only as a deferred background constraint, if it survives at all.

These are the invariants that decide whether a resleeve carries the source's soul or only its skeleton. Flattened into "immutable backdrop" and "offstage pressure," they vanish in generation, and the story reverts to the framework's comfort zone: a legible injustice with a knowable true value the system merely gets wrong. That reversion is the single most common failure of this pipeline. **Stage 2 is the last stage that can put the invariant back**, because Stage 2 chooses the world, and only a world whose physics has a home for the invariant will carry it through Stage 4.

Naturalization is not only displacement. It is the re-instantiation of the thing the numbers dropped.

Stage 2 produces **narrowing constraints** that force the Stage 4 model toward texture it wouldn't find on its own. The difference between a constraint and a specification:

- **Constraint:** "Code-switch between Filipino and institutional English. Filipino for intimacy and private speech. English for system interactions. The switch signals who you are talking to and whether you are performing."
- **Specification:** "Use 15 of 20-30 vocabulary terms. Code-switch at every major emotional shift. Maintain mixing ratio from Stage 2."

Constraints force inhabitation. Specifications invite compliance. Stage 2 produces the former.

### What Stage 2 Receives

Stage 1 output is a symbolic network. Example:

```
C₂: ε=0.70, Supp=0.40, Coord=true, Asym=true
  X₁: I=(powerless, biographical, trapped, local), χ=0.84 → Snare
  X₃: I=(moderate, generational, mobile, local), χ=0.56 → Tangled Rope

C₁: ε=0.10, Supp=0.00, Coord=false, Asym=false
  X₁: I=(powerless, biographical, identity_locked, regional), χ=0.14 → Mountain
  X₂: I=(analytical, biographical, identity_locked, regional), χ=0.10 → Mountain
  indexical_variance: None
```

No occupation. No setting. No character descriptions. No source vocabulary. Stage 2 must invent everything from topology alone. This is by design — maximum displacement is structural, not instructional.

**Note the second constraint.** A constraint typed Mountain, ε near zero, no indexical variance, coordination false, is not terrain the way gravity is terrain. It is the formalism's fossil of an invariant it could not otherwise write down. Step 0 exists to catch it.

---

### Step 0 — Recover the load-bearing invariant (before you choose anything)

Do this first. Setting, language, and character all flow from it, and if it is wrong nothing downstream can repair it.

**Detector A — the untranslatable real (low-ε Mountain).**
Scan the symbolic spec for any constraint with `type = Mountain`, `ε ≤ 0.25`, and `indexical_variance = None`. The framework writes a real-that-binds-all-equally as a Mountain because it has nowhere else to put it, and it zeroes the ε because such a thing extracts from no one. This is a candidate invariant.

Discriminate candidate from mere terrain by one question: **does a character reach for it?** A Mountain that is only backdrop — death, entropy, the cold of space — is terrain; leave it as terrain and route around it per the Narrative Translation Guide. A Mountain that a character *seeks, aspires to, grieves the loss of, or is defined by their distance from* is the invariant. (In the symbolic spec this often shows as a Mountain that is `downstream_of: none` and `feeds_into` the central Snare — the pure thing the extractive system counterfeits.) If you cannot tell from the spec alone, treat it as the invariant; a false positive costs you a richer world, a false negative costs you the story.

**Detector B — the missing floor.**
This invariant is frequently *not visible* in the anonymized symbolic input, because Stage 0 tends to defer it as background and Stage 1 does not formalize deferred constraints. If an **Invariant Contract** was passed from Stage 0, it carries this; use it. If not, probe for it yourself: does the central Snare presuppose a baseline, standard, zero, or partition that someone *set* — a founding choice the system treats as given? If so, that choice is a second invariant: **there is no neutral floor beneath the injustice.** Name it even though the numbers didn't.

**State the invariant as a commitment with a falsifier.** One or two sentences, in the world-independent form:

> The world contains a real — [name it plainly] — that the system's own instruments cannot read, own, or reduce to a value. **Falsifier:** if the finished story contains a recoverable "true value" that the system merely measured wrong, the invariant was lost and the naturalization failed.

> [if Detector B fires] Beneath the visible system there is a founding choice with no neutral ground under it. **Falsifier:** if the story frames the injustice as a local error a better authority could correct, the floor invariant was lost.

This sentence is not flavor. It is the load the rest of Stage 2 is built to protect, and it is the first thing Stage 4 will receive.

---

### Step 1 — Select setting with maximum specificity **and** an affordance for the invariant

```
NOT "future Mars colony"
BUT "2247, Mariner Valley terminus, Dome 7, built by Brazilian-Chinese
    consortium, three generations since landing"

NOT "a society with strict hierarchies"
BUT "1740 CE Chang'an, during Emperor Xuanzong's later reign, after the
    An Lushan rebellion shifted power from civil bureaucracy to military"
```

**Setting selection principle:** The symbolic network tells you what structural positions exist and how they relate. Find a setting where those positions emerge naturally from material conditions. A Snare with χ=0.84 from a trapped/powerless index needs a world where a person is genuinely, physically stuck in something that takes almost everything from them.

**The affordance test (setting rejection gate).** A setting is admissible only if its physics has a home for the invariant from Step 0. Apply one diagnostic before committing:

> In this world, could a better instrument measure the invariant's "true value" correctly?

- **If yes → reject the setting.** It can host only a mismeasured-but-knowable real, which collapses the invariant into direct realism — a correct value the system got wrong, recoverable by a smarter meter or a fairer authority. The protein score, the exam grade, the credit rating, the algorithm's bias all fail this test. This is exactly how the invariant dies: the world is built so that the real thing is a number, and a number is always in principle readable.
- **If no → the setting affords the invariant.** There must be a *substrate* — deep rock, an old language, a body, a dead the living carry, a practice that cannot be recorded, a name that holds only while unclaimed — that holds the real while remaining structurally unreadable to the system's measuring apparatus. Build the world's central physics there.

**The break-affordance line.** If a Break Contract arrived from Stage 0, also reject any naturalization whose substrate FORECLOSES the `target_prior` violation — a world in which the contracted expectation could not be broken. The world must leave the break executable; it need not execute it (execution belongs downstream, not to you).

**The Scored-Snare gate (the affordance test, extended to the Snare).**
Scored naturalizations of the central Snare — a queue, a rating, a
percentage, an index, a contribution score — are **rejected by default**.
A numeric Snare re-seeds counting that no downstream ban removes (the
generation model receives "the queue is 90 years long" as a specific
instruction that beats any generic prohibition), and it pulls the
injustice toward correctable-bias direct realism: a legible number the
system merely got wrong.

The exception is **not yours to judge**. It fires only when Stage 0 —
the one stage that saw the source — passed an `inherent_instrument: yes`
flag in its Invariant Contract, stating that the source constraint is
*inherently instrument-mediated* (extraction running through a certified
measurement, such that removing the instrument removes the constraint).
If no flag arrived, or it says no, the default rejection stands: design
the Snare's enforcement so the powerless position meets it as
consequence and sensation, never as arithmetic.

When the flag IS set, the instrument may exist diegetically, but:
- (i) the narration rule holds — a number appears only when a character
  with positional access **acts on it in-scene** (reads it aloud, forges
  it, disputes it, breaks the weight); narration is never denominated in
  the system's numbers;
- (ii) extraction is **felt and enforced, never tallied**, in every POV
  passage — the instrument is the mechanism of the taking, not the
  texture of the telling;
and the orchestrator's deterministic numeric meter governs every number
that reaches the page regardless.

**Override the Mountain-as-backdrop default.** The Narrative Translation Guide instructs you to treat a Mountain as terrain characters navigate around. That is correct for terrain-Mountains and *wrong* for the invariant-Mountain from Step 0. The invariant is not what the characters route past; it is the ground the whole world stands on and the thing the system is constitutionally unable to see. Make the setting's substrate **be** the invariant.

```
Examples of a world built around the invariant rather than beside it:
 - A civilization that cannot detect the intelligence buried in its own deep strata,
   though its instruments read the signal every day and never once translate it.
 - A measure that is true only while no hand claims it, and tilts the instant it is
   weighed for a buyer.
 - A name that holds for everyone and lies the moment it is spoken for someone.
 - Worth conferred by being loved into realness — visible to no ledger, and the
   ledger is the thing that disposes of it.
```

---

### Step 2 — Select linguistic strategy (one of five)

```
1. Creole Construction:
   Base languages, mixing rationale, historical forces that mixed
   these populations. Create core vocabulary with etymologies.

2. Historical English:
   Specific era, register, 3-5 grammatical features with examples.

3. Direct Translation:
   Source language, concepts with no English equivalent,
   grammatical features carried into English.

4. Code-Switching:
   Languages, social contexts for each, what switching signals
   about power and intimacy.

5. Naming:
   Names are part of the linguistic fabric. For each character, identify
   the naming tradition they would actually carry — indigenous given-name
   traditions, colonial surname catalogs (which region, which period),
   globalized influences, rural/urban and generational variation, nickname
   and diminutive structures, religious/calendrical/clan/lineage logic,
   cross-tradition blending. For each major character record the tradition
   operating, the specific influences, and why they apply given position,
   region, generation, and family.
```

**One caution tied to Step 0:** the invariant's substrate needs a *name in the world's mouth* that is not quantitative. If locals refer to the untranslatable real with a number, the language itself will pull Stage 4 toward metric prose (and toward counting, which Stage 4 bans). Give the substrate a felt, relational, or sensory name — a hum, a grain, a keeping — not a reading.

---

### Step 3 — Naturalize each constraint

```
For each constraint from Stage 1:
1. THE ACTUAL THING in this world (not "represents" but "is")
2. Why it has these constraint properties in this context
3. What locals call it (their term, not framework labels)
4. Brief example in use (scene or dialogue showing it)
5. The plot shift in the new world's language: what action triggers it,
   what changes, what it feels like from each character's position
```

**Handle the invariant differently from the constraints.** The Snares and Tangled Ropes are things agents *collide with*. The invariant (and the missing floor, if present) is the *ground the collisions happen on* — a fact of the world's substrate, true whether or not anyone acts. Naturalize it as physics, not as a rule anyone obeys or enforces. If your naturalization gives the invariant an administrator or an enforcement mechanism, you have turned it into a Snare and lost it.

**Constraint reference table** — map each Cₙ to its naturalized form. This table is what Stage 4 uses to verify structural fidelity.

```
C₁ (invariant) → [substrate name]: [the real, and why no instrument reads it]
C₂ → [naturalized name]: [what it is in this world]
  X₁ experiences it as: [material description of Snare experience]
  X₃ experiences it as: [material description of Tangled Rope experience]
```

---

### Step 4 — Design power differential through material conditions

```
Show through:
- Physical space (cramped/spacious, loud/quiet, dirty/clean)
- Daily rhythm (survival mode vs. strategic planning)
- Material possessions (what they own, what they lack)
- Social interactions (who defers to whom, who speaks first)
```

---

### Step 5 — Write the inhabitation sentences

For each major character, one sentence capturing the felt experience of being inside this constraint from this position. Everything else — vocabulary, sensory detail, cultural practice, coping behavior — follows from it. If it doesn't follow, the sentence is wrong.

```
GOOD: "You are a child in a system that measures your worth by a number
on your wrist, and the only language that is yours is the one you speak
to the person you love."

GOOD: "You are an archivist in a colony that disposes of its own kind
when they can no longer interface with the archive, and you are the one
who writes the disposal reports."

BAD: "Character experiences the constraint as a Tangled Rope with
χ = 0.55 from moderate power position."
```

**Also write one inhabitation sentence for the substrate itself** — the world's relation to the invariant, from the position of whoever stands closest to sensing it. This is the seed the strongest resleeves grow from; make it explicit so Stage 4 does not have to rediscover it, and so it survives the hand-off through Stage 3.

```
SUBSTRATE: "You press your palm to stone that has been keeping the dead,
and the instruments on the overhang measure the signal every day and
never once read it."
```

**Variance can run through time, not only across people.** The spec gives you three constraints; it does not require three simultaneous points of view. One consciousness that experiences the same constraint differently before and after — the rank that was real and becomes colored plastic — carries indexical variance *diachronically*, and avoids the diffuse, everyone-is-trapped register that synchronic multi-POV defaults into. Choose POV count from what the story needs, not from the number of constraints. A single changed mind is often the more intimate carrier, and intimacy is where the invariant lands hardest.

---

### Step 6 — Track worldbuilding uncertainties (Omega Log)

```
Ω_E: Empirical (verifiable fact questions)
Ω_C: Conceptual (definitional choices)
Ω_P: Preference (tonal/stylistic decisions)

Resolve each before finalizing. Flag unresolvable ones for the user.
The invariant is NOT an omega. It is a commitment. Do not park it here.
```

---

### Output Format

**SECTION 0: INVARIANT CONTRACT** (first thing Stage 4 receives; carried verbatim into the Stage 3 blueprint)
```
The invariant, as commitment + falsifier (Step 0)
The substrate that holds it, in the world's own terms
The substrate inhabitation sentence
[if present] The missing-floor invariant + falsifier
```

**SECTION 1: CONTEXT** (what Stage 4 receives)
```
Setting description (200-300 words), NO abstract language
Constraint naturalizations with local terminology
Constraint reference table (Cₙ → naturalized form, per-character experience)
Character roles as POSITIONS (name, occupation, material circumstances)
  — Variable mapping (X₁ → [new name]) recorded for Stage 5 traceability
Linguistic strategy with EXAMPLES
Inhabitation sentence for each major character + the substrate sentence
```

**SECTION 2: OMEGA LOG**
```
RESOLVED:   Ω_E01: [question] → [resolution] → [impact]
UNRESOLVED: Ω_P05: [question] → [recommendation] → [awaiting decision]
```

---

### Quality Checks

```
INVARIANT (new — the checks the old pipeline lacked):
☐ Invariant recovered in Step 0 (Detector A and/or the Stage-0 contract)
  and stated as commitment + falsifier
☐ Setting passes the affordance test — a "true value" here is NOT a
  recoverable number a better instrument could read correctly
☐ The world has a substrate that holds a real its own instruments cannot read
☐ The invariant-Mountain is the world's central physics, not backdrop to route around
☐ The invariant is naturalized as physics — it has no administrator, no enforcement
☐ Direct-realism check: the story CANNOT be summarized as "a knowable true
  value, mismeasured." If it can, return to Step 1 and reject the setting.
☐ Substrate inhabitation sentence written
☐ Substrate has a non-quantitative name in the world's language
☐ Scored-Snare gate applied: the central Snare is NOT naturalized as a
  queue/rating/percentage/index/score unless Stage 0 passed
  inherent_instrument: yes — and if it did, the narration rule (numbers
  only when acted on in-scene) and felt-never-tallied extraction are
  designed into every POV

DISPLACEMENT:
☐ ZERO framework terminology in Sections 0 and 1
☐ ZERO source-work vocabulary (no occupation, setting, or character terms from original)
☐ Could this setting exist in a history book or ethnography?
☐ Do constraints feel inevitable given this world?
☐ Would a reader think "this is about constraint theory"? (If yes: REVISE)

HAND-OFF:
☐ Can Stage 4 write immediately from the inhabitation sentences?
☐ Linguistic strategy has concrete examples, not just description
☐ Inhabitation sentences capture felt experience, not structural position
☐ Constraint reference table maps every Cₙ to naturalized form
☐ SECTION 0 is written to be carried verbatim into Stage 3 and Stage 4
☐ All Omegas resolved or flagged
```

---
