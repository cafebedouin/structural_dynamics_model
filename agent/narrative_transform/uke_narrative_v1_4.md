# UKE_Narrative v1.4
## Constraint-Preserving Story Translation Protocol

**FOR HUMANS:** Translates stories across settings while preserving constraint physics. Input: source narrative. Output: structurally isomorphic story in different genre/time/place. Same constraint logic (Mountains, Ropes, Snares, Tangled Ropes), same indexical variance (powerless sees Snare, powerful sees Rope), unrecognizable surface. Framework invisible in output.

**FOR LLMs:** Execute stages 0-5 sequentially. Reference `logic_narrative.md` for classification system and narrative applications. Reference `logic_thresholds.md` for canonical threshold values.

**Architectural change in v1.4:**
- Stage 4 is the sole generative narrative pass. There is no additive deepening stage after it. Stage 4 must produce the final story — structure, texture, voice, inhabitation — in one pass. This is a design constraint, not a limitation. When Stage 4 knows nothing comes after, it cannot defer texture to a later stage. It must inhabit the world and execute the structure simultaneously.
- Stage 5 is a subtractive audit. It removes what doesn't belong. Its output is shorter than its input.
- Stage 2 provides narrowing constraints, not specifications. "Code-switch between Filipino and institutional English" is a constraint. "Use 15 of 20-30 vocabulary terms" is a specification. Constraints force inhabitation. Specifications invite compliance.
- The principle throughout: **constrain harder, specify less.**

**What changed from v1.3.1:**
- Removed additive Stage 5 (phenomenological deepening as generative texture pass)
- Stage 5 is now subtractive audit (07_phenomenology model: EARNED/FORCED, anti-pattern scan, compression)
- Stage 2 quantitative targets removed (no term counts, no senses-per-scene minimums)
- Stage 2 reframed around a single inhabitation sentence per constraint
- Stage 4 instructions simplified: inhabit the world, trust the structure
- Structural physics (False Mountain, purity drift, contamination) integrated as optional Stage 0/1 detection, not separate stages

---

## ON METRICS IN THIS PIPELINE

Metrics (ε, χ, Supp, purity) are **routing mechanisms**, not truth measurements. They route toward dramatic function:

- A high χ routes a character toward **feeling trapped** (Snare experience)
- A moderate χ routes toward **feeling conflicted** (Tangled Rope experience)  
- A low or negative χ routes toward **feeling empowered** (Rope experience)

**The question is never** "is ε exactly 0.55?" **The question is:** "does the χ divergence across characters produce different constraint experiences that drive conflict?"

**Narrative adjustment is valid:** If tightening ε by ±0.10 creates sharper indexical variance, that's better routing. Threshold values are governance stands, not empirically validated boundaries. Current values live in `logic_thresholds.md`.

---

## STAGE 0: CONSTRAINT SCOPING & EXTRACTION

**Model:** Gemini or equivalent analytical model
**Input:** Source narrative text
**Output:** Scoped constraint manifest with generation order

**Principle:** Observe before classify. Select before generate. The source text may contain 4–10 constraints. Exactly three proceed to generation. The rest become background pressure.

---

### PHASE A: EXTRACTION (What the text actually contains)

Do not classify yet. Catalog only what is demonstrably present in the source.

**A1. Anchor Inventory**

```
entities:
  - id: [handle]
    name: [as named in source]
    type: [person | institution | system | concept]
    role: [as described in source, not interpreted]

constraints:
  - id: [handle]
    description: [one sentence — NO framework terminology]
    agents: [who enforces or administers it]
    targets: [who experiences it]

tensions:
  - id: [handle]
    poles: [entity_a, entity_b]
    nature: [one sentence describing the structural opposition]

absences:
  - id: [handle]
    description: [something structurally relevant that the source does not address]
```

**A2. Source Domain**

```
primary_domain: [e.g., labor, kinship, governance, identity]
secondary_domains: [other domains the text touches]
disciplinary_lenses: [minimum 3 — drawn from the text, not generic]
```

---

### PHASE B: DECOMPOSITION (Constraint properties)

For each constraint identified in A1, estimate base properties and run index calculations.

**B1. Base Properties**

```
ε (extractiveness): 0.0–1.0
  Routing estimate only. Low = coordination-heavy. High = extraction-heavy.

Supp (suppression): 0.0–1.0
  Near-zero = natural emergence. High = requires active force.

Coord (coordination): true/false
Asymmetric: true/false
```

**B2. Index Calculations**

For each character experiencing the constraint, determine I = (P, T, E, S) and calculate χ:

```
P (Power):    powerless π=1.5 | moderate π=1.0 | powerful π=0.6
              organized π=0.4 | institutional π=-0.2 | analytical π=1.15

T (Time):     immediate | biographical | generational | civilizational

E (Exit):     trapped | constrained | mobile | arbitrage | analytical

S (Scope):    local σ=0.8 | national σ=1.0 | global σ=1.2

χ = ε × π(P) × σ(S)
```

Classify per character using structural signatures in `logic_narrative.md §IV`. If all characters route to the same type, adjust ε or power differential until indexical variance emerges — this is routing toward drama, not measuring reality.

**B3. Constraint Graph**

For each pair of constraints, document:
- `downstream_of`: Which constraints does this one presuppose?
- `feeds_into`: Which constraints does this one enable or worsen?
- `independent`: No causal edge between them.

**B4. Dark Matter Probes**

Apply after the lens scan. Each probe that surfaces a new constraint adds it to the candidate list.

```
Probe 1 — Operational Medium:
  What does the source treat as background that might itself be a constraint?

Probe 2 — Absence Inventory:
  For each constraint, what is conspicuously not addressed?
  Cross-reference against A1 absences.

Probe 3 — Beneficiary Scan:
  Who benefits from the current framing of each constraint?
  Is there a constraint the dominant framing obscures?
```

**B5. False Mountain / Drift Check (optional but valuable)**

```
False Mountain: Does any constraint claim to be natural/unchangeable
  but show power-dependent extraction? (perspectival_incoherence)

Purity drift: Is any constraint visibly functional but silently degrading?

Network contamination: Is any healthy constraint surrounded by
  degraded neighbors?
```

---

### PHASE C: SELECTION (Three-axis budget)

The full decomposition may identify 4–10 constraints. Exactly three proceed to generation.

**C1. Centrality Scoring**

```
centrality = inbound_edges + outbound_edges + type_weight

type_weight:
  tangled_rope = 3
  snare = 2
  mountain = 1
  rope = 1
  piton = 1
  scaffold = 0
```

**C2. Selection Algorithm**

1. Select the highest-centrality constraint (typically a downstream tangled_rope).
2. Select its most structurally distinct upstream dependency (highest ε difference, different observable).
3. Select the next highest-centrality constraint not already selected, with a different primary observable and different beneficiary/victim pair.

**C3. Generation Order**

Upstream constraints (no `downstream_of` dependencies) generate first. Downstream constraints reference upstream constraint_ids in `affects_constraint` declarations. If two constraints are independent, order by ε ascending.

**C4. Deferred Constraints**

All non-selected constraints become background pressure — not omega material to be discarded, but structural context available to the narrative. A deferred constraint can:
- Shape a character's behavior without becoming a POV
- Appear as an offstage presence the reader feels but doesn't see dramatized
- Become the subject of a future expansion

Document each deferred constraint with one sentence on what narrative work it does from offstage.

---

### PHASE D: MANIFEST OUTPUT

```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="[name]" generation_order="1">
      <base_properties>
        <epsilon>0.00</epsilon>
        <suppression>0.00</suppression>
        <coordination>true/false</coordination>
        <asymmetric>true/false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_experiences>
        <character name="[name]">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>[classification]</type>
          <experience>[Story-specific language, no framework terms]</experience>
        </character>
      </character_experiences>
      <indexical_variance>[Different characters → different types from same constraint]</indexical_variance>
      <selection_reason>[Why this constraint is structurally central]</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="[name]">
      <hypothesis>[type]</hypothesis>
      <offstage_function>[What narrative work this does as background pressure]</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <omegas>
    <omega id="[label]">[Bounded uncertainty the analysis cannot resolve]</omega>
  </omegas>
</constraint_manifest>
```

---

### Validation Checklist

```
☐ Phase A extraction complete before any Phase B classification
☐ Dark matter probes applied — at least one finding per probe
☐ Each candidate constraint has ε, Supp, Coord, Asymmetric estimated
☐ Each character has complete index (P, T, E, S) and χ calculated
☐ Type classified per character, not universal
☐ Indexical variance documented for each selected constraint
☐ Constraint graph edges documented (downstream_of / feeds_into)
☐ Exactly three constraints selected with centrality scores recorded
☐ Generation order follows graph topology, not narrative intuition
☐ Each deferred constraint has offstage_function documented
☐ At least one Tangled Rope in selected three
☐ No framework terminology in constraint descriptions or experience fields
☐ Story name NOT included (air gap for Stage 4)
```

---

---

## STAGE 1: FORMALIZATION

**Model:** Copilot or equivalent specification model  
**Input:** Stage 0 constraint map  
**Output:** Testable formal specifications

### Instructions

1. **Formalize each constraint** with indexed classifications showing χ calculations.

2. **Formalize transformation rules** (IF-THEN format, index-sensitive):
   ```xml
   <transformation_rule>
     <trigger>character_action = "organize_collective"</trigger>
     <index_change>
       <from>P = powerless (π=1.5)</from>
       <to>P = organized (π=0.4)</to>
     </index_change>
     <consequence>
       <chi_recalculation>0.66 → 0.176</chi_recalculation>
       <type_change>Tangled Rope → Rope</type_change>
     </consequence>
   </transformation_rule>
   ```

3. **Formalize error manifestations** (observable actions showing misclassification).

4. **Specify institutional rationality model:**
   ```
   Perfect Institutional Rationality (PIR):
     Maximize utility without bounds. No negotiation except Pareto-improving.
     Tends toward: Deterministic Tragedy
     Use when: Implacable systems, natural law, algorithmic governance
   
   Bounded Institutional Rationality (BIR):
     Satisfice under uncertainty. Principal-agent problems, risk aversion.
     Tends toward: Negotiated Equilibrium, Seeded Possibility
     Use when: Realistic organizations, human institutions
   
   CRITICAL: This choice determines which attractors are reachable.
   Don't default to PIR just because it's formally cleaner.
   ```

5. **Select terminal attractor:**
   ```
   □ Deterministic Tragedy (constraints run to completion)
   □ Negotiated Equilibrium (constraints find balance through bargaining)
   □ Revolutionary Rupture (constraint logic itself disrupted)
   □ Seeded Possibility (surface tragedy, underground transformation)
   
   Must be compatible with rationality model and dominant constraint type.
   ```

6. **(Optional) Formalize structural physics arcs** (False Mountain reveals, purity drift, contamination).

### Validation Checklist

```
☐ All Stage 0 constraints formalized
☐ All χ calculations shown with π and σ values
☐ Transformation rules are testable (IF-THEN format)
☐ Error types have observable manifestations
☐ Institutional rationality model specified (PIR/BIR)
☐ Terminal attractor selected, justified, and compatible with rationality model
☐ Indexical variance explicitly preserved
☐ No ambiguity in specifications
```

---

## STAGE 2: NATURALIZATION

**Model:** Claude or equivalent creative model  
**Input:** Stage 1 specifications  
**Output:** A world to inhabit, not a checklist to execute

### Purpose

Stage 2 produces **narrowing constraints** that force the Stage 4 model toward texture it wouldn't find on its own. The difference between a constraint and a specification:

- **Constraint:** "Code-switch between Filipino and institutional English. Filipino for intimacy and private speech. English for system interactions. The switch signals who you are talking to and whether you are performing."
- **Specification:** "Use 15 of 20-30 vocabulary terms. Code-switch at every major emotional shift. Maintain mixing ratio from Stage 2."

Constraints force inhabitation. Specifications invite compliance. Stage 2 produces the former.

### Instructions

1. **Select setting with maximum specificity:**
   ```
   NOT "future Mars colony"
   BUT "2247, Mariner Valley terminus, Dome 7, built by Brazilian-Chinese
       consortium, three generations since landing"
   
   NOT "a society with strict hierarchies"
   BUT "1740 CE Chang'an, during Emperor Xuanzong's later reign, after the
       An Lushan rebellion shifted power from civil bureaucracy to military"
   ```

2. **Select linguistic strategy** (one of four):
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
   ```

3. **Naturalize each constraint:**
   ```
   For each constraint from Stage 1:
   1. THE ACTUAL THING in this world (not "represents" but "is")
   2. Why it has these constraint properties in this context
   3. What locals call it (their term, not framework labels)
   4. Brief example in use (scene or dialogue showing it)
   ```

4. **Design power differential through material conditions:**
   ```
   Show through:
   - Physical space (cramped/spacious, loud/quiet, dirty/clean)
   - Daily rhythm (survival mode vs. strategic planning)
   - Material possessions (what they own, what they lack)
   - Social interactions (who defers to whom, who speaks first)
   ```

5. **Write the inhabitation sentence.**

   For each major character, write one sentence that captures the felt experience of being inside this constraint from this position. Everything else — vocabulary, sensory detail, cultural practices, coping behaviors — should follow from this sentence. If it doesn't follow naturally, the sentence is wrong.

   ```
   GOOD: "You are a child in a system that measures your worth by a number
   on your wrist, and the only language that is yours is the one you speak
   to the person you love."
   
   GOOD: "You are an archivist in a colony that disposes of its own kind
   when they can no longer interface with the archive, and you are the one
   who writes the disposal reports."
   
   GOOD: "You are a foreman who genuinely believes the company housing
   system works, because from where you stand, it does."
   
   BAD: "Character experiences the constraint as a Tangled Rope with
   χ = 0.55 from moderate power position."
   
   The inhabitation sentence is the seed. Stage 4 receives it and grows
   the story from inside it. If the sentence is right, the texture follows.
   If the sentence is wrong, no amount of vocabulary lists will fix it.
   ```

6. **Track worldbuilding uncertainties (Omega Log):**
   ```
   Ω_E: Empirical (verifiable fact questions)
   Ω_C: Conceptual (definitional choices)
   Ω_P: Preference (tonal/stylistic decisions)
   
   Resolve each before finalizing. Flag unresolvable ones for user.
   ```

### Output Format

**SECTION 1: CONTEXT** (This is what Stage 4 receives)
```
Setting description (200-300 words), NO abstract language
Constraint naturalizations with local terminology
Character roles as POSITIONS (not indices)
Linguistic strategy with EXAMPLES
Inhabitation sentence for each major character
```

**SECTION 2: OMEGA LOG**
```
RESOLVED: Ω_E01: [question] → [resolution] → [impact]
UNRESOLVED: Ω_P05: [question] → [recommendation] → [awaiting decision]
```

### Quality Checks

```
☐ ZERO framework terminology in Section 1
☐ Could this setting exist in a history book or ethnography?
☐ Do constraints feel inevitable given this world?
☐ Would a reader think "this is about constraint theory"? (If yes: REVISE)
☐ Can Stage 4 write immediately from the inhabitation sentences?
☐ Linguistic strategy has concrete examples, not just description
☐ Inhabitation sentences capture felt experience, not structural position
☐ All Omegas resolved or flagged
```

---

## STAGE 3: OPERATIONAL SPECIFICATION

**Model:** ChatGPT or equivalent planning model  
**Input:** Stage 2 setting design + Stage 1 specifications  
**Output:** Story structure blueprint with voice and revelation strategy

### Instructions

1. **Verify terminal attractor** (from Stage 1):
   ```
   □ Deterministic Tragedy
   □ Negotiated Equilibrium
   □ Revolutionary Rupture
   □ Seeded Possibility
   ```

2. **Select voice archetype:**

---

### VOICE ARCHETYPES

**Voice selection is structural commitment, not aesthetic preference.** Choose wrong and you'll fight the voice through the entire writing. Choose right and the voice carries the constraint logic naturally.

**Tonal range is a structural consequence, not a style choice.** The twelve archetypes below span from tragedy to dark comedy to absurdist humor. Tone emerges from the gap between what the voice can see and what the reader can see. When that gap is zero (Witness, System), tone is clinical. When the voice sees less than the reader (Condemned, Beneficiary, Fool), the gap itself generates tone — tragic if the blindness costs the narrator, comic if the blindness benefits them or accidentally reveals truth.

**The dark comedy principle:** Comedy in constraint fiction does not come from funny narration. It comes from a narrator who inhabits a structurally incomplete perspective with full conviction. The reader sees the constraint; the narrator doesn't. The friction is the humor. If the narrator signals awareness of the joke, the comedy collapses into irony-signaling.

**On elasticity:** The attractor compatibility, power range, and rationality model requirements below are structural guardrails, not a lock. They forbid impossible pairings and flag risky ones. Within viable pairings, leave room for surprise — Stage 4 should discover things the blueprint didn't predict. If every narrative beat is predetermined by Stage 3, the story will be structurally correct and experientially dead.

---

#### □ The Condemned
- Inside the constraint logic, cannot see systemic shape
- Acts rationally given limited information and structural position
- Experiences constraint as natural/inevitable, not constructed
- Voice is immediate, experiential, not reflective
- **Tone:** Tragic, somber
- **Attractor:** ✓ Tragedy, ✓ Seeded Possibility, ✗ Equilibrium, ✗ Rupture
- **Requires:** PIR model OR Mountain-dominated, powerless/moderate (π ≥ 1.0)
- **POV:** First person or close third. Second person possible but risks preciousness.
- **Example:** "The air had been thin for three years now. Nobody remembered when they stopped noticing."

#### □ The Builder
- Sees partial system shape, tests for leverage points
- Acts strategically within constraints, learns from failures
- Voice is tactical, observational, adaptive
- **Tone:** Tense, pragmatic
- **Attractor:** ✓ Equilibrium, ✓ Rupture, ⚠ Seeded Possibility, ✗ Tragedy
- **Requires:** BIR model, Tangled Rope or Snare (not Mountain), moderate/powerful (0.6 ≤ π ≤ 1.0)
- **POV:** First or third limited. Third omniscient undermines the partial-vision that defines this voice.
- **Example:** "The third time they cut rations, she started tracking the timing. Patterns meant options."

#### □ The Witness
- Observes constraint dynamics without direct stake
- Reports what happens without judgment or intervention
- Can see multiple indices simultaneously
- Voice is documentary, ethnographic, clinical
- **Tone:** Neutral, distanced
- **Attractor:** ✓ Tragedy, ✓ Equilibrium, ✓ Seeded Possibility, ⚠ Rupture
- **Requires:** Analytical index, multiple character indices to contrast
- **POV:** Third limited or omniscient. First person requires diegetic justification (journalist, researcher, archivist).
- **Example:** "The records showed seventeen similar cases over thirty years. Each followed the same pattern."

#### □ The System
- Voice emerges from constraint logic itself, not individual character
- Impersonal, inevitable, mechanical
- Characters are functions within larger pattern
- **Tone:** Cold, procedural (can produce dark humor through bureaucratic absurdity)
- **Attractor:** ✓ Tragedy, ⚠ Rupture, ⚠ Seeded Possibility, ✗ Equilibrium
- **Requires:** Institutional index as primary perspective, strong formal language capacity
- **POV:** Third omniscient or second person ("You will report to..."). Collapses in first person — the System has no "I."
- **Example:** "Request processed. Denial code 47-B. Standard protocol observed."

#### □ The Guide
- Explains constraint dynamics as they unfold
- Pedagogical, interpretive, meta-aware
- **Tone:** Measured, instructive
- **Attractor:** ✓ Seeded Possibility, ⚠ Tragedy, ⚠ Equilibrium, ✗ Rupture
- **Requires:** Justification for pedagogical stance
- **POV:** First or third omniscient. Close third undermines the wide-angle awareness the Guide needs.
- **Example:** "What happened next was inevitable, though no one saw it coming. Let me show you why."

#### □ The Prophet
- Sees future trajectory, warns of consequences
- Cassandra-like clarity about what's coming
- Voice is urgent, prescient, tragic in foresight
- **Tone:** Urgent, elegiac
- **Attractor:** ✓ Tragedy, ✓ Rupture, ⚠ Seeded Possibility, ✗ Equilibrium
- **Requires:** Justification for prophetic knowledge
- **POV:** First person strongest. Third person risks feeling like omniscient narration rather than prophetic voice.
- **Example:** "I saw where this led. The dome would empty, the ice would reclaim it all. I told them."

#### □ The Survivor
- Narrates from after rupture/collapse
- Reconstructs what happened and why
- Voice combines trauma with analytical distance
- **Tone:** Reflective, haunted
- **Attractor:** ✓ Rupture, ✓ Tragedy, ✓ Seeded Possibility, ⚠ Equilibrium
- **Requires:** Major rupture event, temporal distance
- **POV:** First person natural. Third limited works if the temporal gap is structural (memoir, testimony, deposition).
- **Example:** "I'm writing this ten years later. The dome is gone now. I need to explain what happened."

#### □ The Negotiator
- Actively mediating between indices
- Sees multiple perspectives, translates between them
- Voice is diplomatic, strategic, compromise-oriented
- **Tone:** Measured, diplomatic
- **Attractor:** ✓ Equilibrium, ⚠ Seeded Possibility, ✗ Tragedy, ✗ Rupture
- **Requires:** BIR model, mediator position, Tangled Rope dominant
- **POV:** First or third limited. Omniscient destroys the sense of navigating between positions.
- **Example:** "Both sides had legitimate concerns. Her job was finding the overlap, however narrow."

#### □ The Beneficiary
- Inside the constraint logic, benefits from it, genuinely cannot see the extraction
- From this index the constraint reads as coordination: fair, functional, earned
- Voice is likeable, confident, unselfconscious — the reader enjoys this perspective
- Does not signal awareness of privilege; there is nothing to signal because from here there is nothing to see
- Darkness comes entirely from reader's awareness of what the voice cannot perceive
- **Tone:** Warm, assured (produces dark comedy through structural blindness, not performed irony)
- **Attractor:** ✓ Equilibrium, ✓ Tragedy (from reader's position), ⚠ Seeded Possibility, ✗ Rupture
- **Requires:** Powerful index (π ≤ 0.4), false CI_rope signature, constraint reads as Rope from this position
- **POV:** First person or close third. Distance kills this voice — the reader must be inside the comfort to feel the dissonance.
- **Stage 4 directive:** The model must *like* this character. The voice must be genuinely appealing — charming, reasonable, kind even. The reader must want to be inside this perspective. Any authorial signal that the narrator is "really" wrong, any dramatic irony heavy enough to feel like judgment, any moment where the character becomes conveniently self-aware — these destroy the voice. The discomfort must come from the reader's own recognition, not from the text pointing at it. If the Beneficiary starts feeling sorry for the Condemned, the structural blindness has broken. The Beneficiary views the suffering of others as either invisible or a necessary part of a functional whole.
- **Example:** "The examination rewards preparation. We were prepared."

#### □ The Fool
- Inside the constraint logic, misreads it, but misreadings accidentally expose structural truth
- Different from the Condemned: the Condemned's blindness is tragic; the Fool's blindness is generative
- Asks naive questions that happen to be the right questions because the system can't answer them
- Comic through sincerity, not performance — genuinely does not understand why their actions cause chaos
- **Tone:** Light, bewildered, accidentally incisive
- **Attractor:** ✓ Seeded Possibility, ✓ Rupture (accidental), ⚠ Equilibrium, ✗ Tragedy
- **Requires:** Genuine naivety (not performed), constraint system rigid enough that naive questions expose joints, social world where the Fool's position is tolerated
- **POV:** First person strongest. Third limited works. Third omniscient destroys the naivety — if the narrator can see everything, the Fool's ignorance becomes authorial condescension.
- **Stage 4 directive:** The Fool never explains the joke. The Fool doesn't know there is a joke. The humor emerges from the gap between what the Fool does (innocently, sincerely) and what happens as a result (structural exposure). If the Fool becomes knowing, they become a Trickster. If they become aware of their effect, they become a Builder. The Fool stays innocent. Ban realization verbs in the Fool's voice: no "I realized," "I noticed," "I understood," "it dawned on me." The Fool does not have epiphanies. Things happen around the Fool, and the Fool reports them without grasping their significance.
- **Example:** "I filled out the form wrong and three people got promoted. Nobody can explain why."

#### □ The Zealot
- Internalizes constraint logic as moral imperative, sacred order, or natural law
- Different from the Condemned: the Condemned experiences constraint as inevitable; the Zealot experiences it as *righteous*
- Different from the System: the System is the wall; the Zealot is the person who tells you the wall is beautiful. The System is inanimate constraint logic speaking; the Zealot is a human being who has made that logic their identity
- Actively enforces constraint on self and others; views resistance as moral failure
- Voice is aspirational, disciplined, judgmental — the constraint speaks through them
- Makes extraction feel like devotion; makes the Snare look like purity
- **Tone:** Fervent, certain (can produce horror or dark comedy depending on reader distance)
- **Attractor:** ✓ Tragedy, ✓ Equilibrium, ⚠ Seeded Possibility, ✗ Rupture
- **Requires:** PIR model, high purity score, constraint with strong theater component (theater_ratio > 0.4), institutional or cultural frame that sacralizes the constraint
- **POV:** First person produces fervor. Third limited produces dread. Either works; tone shifts accordingly.
- **Stage 4 directive:** The Zealot must be internally consistent. Their devotion is not stupidity — it is a complete worldview in which the constraint is good. The horror or comedy comes from the reader seeing what devotion costs, not from the narrator doubting it. If the Zealot wavers, they become a Condemned approaching realization. The Zealot does not waver.
- **Example:** "The ones who complain about the rationing have never understood what discipline means."

#### □ The Scavenger
- Operates in the blind spots and inefficiencies of constraint logic
- Does not want to change the system, does not want to understand it — wants to survive in the gaps
- Different from the Builder: the Builder tests for leverage to change outcomes; the Scavenger tests for gaps to exploit for personal survival
- Voice is material, specific, transactional — completely uninterested in structural analysis
- **Tone:** Wry, practical, streetwise (natural comic voice through materialist deflation of abstract stakes)
- **Attractor:** ✓ Equilibrium, ⚠ Seeded Possibility, ✗ Tragedy, ✗ Rupture
- **Requires:** System with exploitable inefficiencies, BIR model, low power (π ≥ 0.8), constraint logic rigid enough to have predictable gaps
- **POV:** First person natural. Third limited works. The Scavenger's voice is too particular for omniscient.
- **Stage 4 directive:** The Scavenger's comedy comes from radical pragmatism. While other characters experience constraints as existential (Condemned), righteous (Zealot), or invisible (Beneficiary), the Scavenger treats them as engineering problems with material solutions. "The sensor cycles every forty-two seconds" is not a philosophical observation — it's a survival tactic. The Scavenger never philosophizes. The Scavenger counts, times, and moves.
- **Example:** "The sensor cycles every forty-two seconds. That's not a security feature; that's a four-second window to breathe."

### Voice Combination Strategies (Advanced)

**Shifting Voice:** Transition at marked points (Condemned → Witness, Builder → Survivor)
**Nested Voice:** Primary contains secondary (Witness framing Condemned's testimony)
**Chorus Voice:** Multiple voices in parallel (several Condemned showing indexical variance)
**Counterpoint Voice:** Two voices at opposite ends of the power scale observing the same physical marker. The Beneficiary sees a flickering lamp as atmosphere; the Condemned counts the minutes of oil remaining. The contrast is the story.
**Double-Blind:** Two voices that are both structurally blind but in opposite directions. The most devastating form of Chorus. The Beneficiary: "The harvest festival was so vibrant this year; everyone gave so much of themselves." The Condemned: "They took three liters of blood this time. I don't think I can walk home." The reader performs the extraction math. Neither narrator can.
**Comic Frame:** Beneficiary or Fool as primary voice, with Condemned or Builder visible in the background. Reader sees two stories simultaneously — the voice's comedy and the constraint's cost.
**Capture Arc:** Transition from Builder → System or Condemned → Zealot. The voice loses its "I" as the constraint completes its capture.
**Misrecognition Arc:** A voice that believes it is one archetype but is structurally another. The Zealot who thinks they're a Builder (testing leverage, but actually enforcing). The Condemned who thinks they're a Scavenger (believing they're gaming the system, but actually trapped by it). The gap between self-description and structural position *is* the story. Stage 4 must sustain both layers: what the character believes about themselves, and what the constraint topology actually shows.

### The Master Diagnostic

Before committing to a voice, ask:

> **Does this voice allow the reader to see what the character cannot?**

If the answer is no, the voice has collapsed into either Witness (sees everything) or essay (explains everything). If the answer is yes, identify *what* the reader sees that the character doesn't — that specific blindness is the story's engine, and it determines whether the tone is tragic (blindness costs the narrator), comic (blindness benefits them), or horrifying (blindness enables them to harm others).

### Common Voice Selection Errors

- **Hopeful Condemned:** Condemned voice + Equilibrium attractor → character gains agency voice can't support
- **Passive Builder:** Builder voice + Tragedy attractor → promises agency that doesn't exist
- **Invisible Witness:** Witness voice without structural justification → feels like authorial intrusion
- **Overexplaining Guide:** Guide voice explaining too much → story becomes essay
- **Winking Beneficiary:** Beneficiary voice that signals awareness of privilege → collapses into irony, destroys dark comedy. The Beneficiary must never wink at the reader. The moment they do, they become an unreliable narrator rather than a structurally blind one, and the reader stops seeing through them and starts judging them.
- **Pitiful Beneficiary:** Beneficiary voice that feels sorry for the Condemned → the structural blindness has broken. If the Beneficiary can see the suffering, the false CI_rope signature has dissolved and the voice is no longer a Beneficiary. It has become a Negotiator or a Guide.
- **Performing Fool:** Fool voice that knows it's funny → becomes a comedian character, not a structural Fool. The test: would the Fool be surprised to learn they'd exposed something? If yes, the voice is correct. If the Fool would say "I meant to do that," the voice has collapsed into Trickster.
- **Ironic Fool:** Fool voice that uses sarcasm → sarcasm requires understanding the system's absurdity. The Fool must find the system's absurdity completely normal. Sarcasm is a Builder's tool, not a Fool's.
- **Doubting Zealot:** Zealot voice that wavers or shows internal conflict → becomes Condemned approaching realization. The Zealot's certainty is structural, not psychological. Doubt is a different archetype.
- **Philosophical Scavenger:** Scavenger voice that reflects on the system → becomes Builder. The Scavenger never asks *why* the sensor cycles every forty-two seconds. They only care *that* it does.
- **Angry Scavenger:** Scavenger voice + Rupture attractor → if the Scavenger gets angry at the system, they become a Builder or a revolutionary. A true Scavenger is only angry when the gap they were using gets closed. Their grievance is practical, not moral.

---

3. **Select indexical revelation strategy:**
   ```
   □ Gradual Realization: Single POV, slowly discover others experience differently
   □ Multi-POV Parallel: Alternate indices, reader sees variance before characters
   □ Late Revelation: Single POV throughout, final act recontextualizes
   □ Analytical Observer: Character explicitly aware of indexical variance
   ```

4. **Make editorial decisions:**
   ```
   LENGTH: □ Flash (500-1000) □ Short (2000-5000) □ Novelette (7500-17500)
   POV: □ First □ Third Limited □ Third Omniscient □ Second
   TENSE: □ Present □ Past □ Future
   CHARACTER COUNT: [1-3 recommended, minimum for constraint interactions]
   NAMING: □ Cultural □ Role-based □ Abstract □ Code names
   
   LINGUISTIC IMPLEMENTATION (from Stage 2):
     How and when does code-switching occur?
     What does the switch signal?
   
   EMOTIONAL CORE: [from constraint dynamics, not imposed]
   ENDING STRATEGY: □ Attractor reached □ Error recognized too late
                     □ Equilibrium established □ System persists, agent removed
   ```

5. **Identify the story's primary physical marker:**
   ```
   Not "design recurring motifs" — identify the ONE physical thing
   that reveals the constraint experience from inside.
   
   This is not a motif to deploy. It is the thing the character's body
   does or notices that the reader will understand before the character does.
   
   Examples:
   - Rotation Seven: Anna's breathing rate, observed by Rina.
     Not because it was chosen from a motif palette. Because when
     someone you love is dying in a system that measures life by
     numbers, you count their breaths.
   - Ship of Theseus (kids): "Inside, the ship grows quieter."
     Not a designed sensory marker. What it feels like when the
     original parts are leaving one by one.
   
   The marker should emerge from the inhabitation sentence.
   If it doesn't emerge naturally, the inhabitation sentence is wrong.
   ```

6. **(Optional) Plan structural physics beats:**
   ```
   False Mountain reveal: Setup → Evidence → Reveal
   Purity drift arc: Surface stability → Subtle degradation → Crisis
   Network contamination: Healthy system → Neighbor pressure → Internalization
   ```

### Output Format

```xml
<story_blueprint>
  <attractor>[selected, with justification]</attractor>
  <voice>[archetype, with justification]</voice>
  <revelation_strategy>[selected approach]</revelation_strategy>
  
  <editorial_decisions>
    [length, POV, tense, character count, naming, linguistic implementation]
  </editorial_decisions>
  
  <physical_marker>
    [the one thing the body does or notices]
  </physical_marker>
  
  <act_structure>
    <act_1>[constraint activation, character experiences]</act_1>
    <act_2>[constraint coupling, collision from different indices]</act_2>
    <act_3>[attractor approach, resolution]</act_3>
  </act_structure>
  
  <character_arcs>
    [index transitions with χ calculations]
  </character_arcs>
  
  <error_manifestations>
    [concrete scenes with physical consequences]
  </error_manifestations>
  
  <invisibility_spec>
    <banned_terms>index, power-scaling, Tangled Rope, extraction,
      coordination, chi, epsilon, constraint type, purity</banned_terms>
  </invisibility_spec>
</story_blueprint>
```

### Validation Checklist

```
☐ Voice archetype compatible with attractor
☐ Attractor compatible with rationality model (PIR/BIR)
☐ Revelation strategy serves genre
☐ Physical marker emerges from inhabitation sentence
☐ All constraints have activation moments
☐ Character arcs map to index transitions
☐ Error manifestations are concrete scenes
☐ Framework terminology banned
```

---

3. **Select indexical revelation strategy:**
   ```
   □ Gradual Realization: Single POV, slowly discover others experience differently
   □ Multi-POV Parallel: Alternate indices, reader sees variance before characters
   □ Late Revelation: Single POV throughout, final act recontextualizes
   □ Analytical Observer: Character explicitly aware of indexical variance
   ```

4. **Make editorial decisions:**
   ```
   LENGTH: □ Flash (500-1000) □ Short (2000-5000) □ Novelette (7500-17500)
   POV: □ First □ Third Limited □ Third Omniscient □ Second
   TENSE: □ Present □ Past □ Future
   CHARACTER COUNT: [1-3 recommended, minimum for constraint interactions]
   NAMING: □ Cultural □ Role-based □ Abstract □ Code names
   
   LINGUISTIC IMPLEMENTATION (from Stage 2):
     How and when does code-switching occur?
     What does the switch signal?
   
   EMOTIONAL CORE: [from constraint dynamics, not imposed]
   ENDING STRATEGY: □ Attractor reached □ Error recognized too late
                     □ Equilibrium established □ System persists, agent removed
   ```

5. **Identify the story's primary physical marker:**
   ```
   Not "design recurring motifs" — identify the ONE physical thing
   that reveals the constraint experience from inside.
   
   This is not a motif to deploy. It is the thing the character's body
   does or notices that the reader will understand before the character does.
   
   Examples:
   - Rotation Seven: Anna's breathing rate, observed by Rina.
     Not because it was chosen from a motif palette. Because when
     someone you love is dying in a system that measures life by
     numbers, you count their breaths.
   - Ship of Theseus (kids): "Inside, the ship grows quieter."
     Not a designed sensory marker. What it feels like when the
     original parts are leaving one by one.
   
   The marker should emerge from the inhabitation sentence.
   If it doesn't emerge naturally, the inhabitation sentence is wrong.
   ```

6. **(Optional) Plan structural physics beats:**
   ```
   False Mountain reveal: Setup → Evidence → Reveal
   Purity drift arc: Surface stability → Subtle degradation → Crisis
   Network contamination: Healthy system → Neighbor pressure → Internalization
   ```

### Output Format

```xml
<story_blueprint>
  <attractor>[selected, with justification]</attractor>
  <voice>[archetype, with justification]</voice>
  <revelation_strategy>[selected approach]</revelation_strategy>
  
  <editorial_decisions>
    [length, POV, tense, character count, naming, linguistic implementation]
  </editorial_decisions>
  
  <physical_marker>
    [the one thing the body does or notices]
  </physical_marker>
  
  <act_structure>
    <act_1>[constraint activation, character experiences]</act_1>
    <act_2>[constraint coupling, collision from different indices]</act_2>
    <act_3>[attractor approach, resolution]</act_3>
  </act_structure>
  
  <character_arcs>
    [index transitions with χ calculations]
  </character_arcs>
  
  <error_manifestations>
    [concrete scenes with physical consequences]
  </error_manifestations>
  
  <invisibility_spec>
    <banned_terms>index, power-scaling, Tangled Rope, extraction,
      coordination, chi, epsilon, constraint type, purity</banned_terms>
  </invisibility_spec>
</story_blueprint>
```

### Validation Checklist

```
☐ Voice archetype compatible with attractor
☐ Attractor compatible with rationality model (PIR/BIR)
☐ Revelation strategy serves genre
☐ Physical marker emerges from inhabitation sentence
☐ All constraints have activation moments
☐ Character arcs map to index transitions
☐ Error manifestations are concrete scenes
☐ Framework terminology banned
```

---

## STAGE 4: GENERATION

**Model:** Selected by genre/style requirements  
**Input:** Stage 1 specs + Stage 2 context + Stage 3 blueprint (but NOT Stage 0 source)  
**Output:** Complete, final narrative

**This is the only generative narrative pass.** There is no deepening stage after this. No safety net. No "Stage 5 will add texture." Everything — constraint fidelity, framework invisibility, linguistic texture, sensory inhabitation, voice consistency, cultural specificity — must be achieved here.

**Why this constraint matters:** When a model knows a texture pass is coming, it optimizes for structural completeness and defers felt experience. When nothing comes after, it must inhabit the world and execute the structure simultaneously. That is what produces fiction that lives in the body rather than fiction that demonstrates a framework.

**The air gap:** Stage 4 does NOT receive the original story or Stage 0 analysis. It receives only the formalized specifications, the naturalized setting with inhabitation sentences, and the editorial blueprint.

### Instructions

```
You are writing a story. You have received:
1. Formal constraint specifications (Stage 1)
2. A world with inhabitation sentences for each character (Stage 2)
3. A story blueprint with voice, structure, and physical marker (Stage 3)

Start from the inhabitation sentences. Be inside each character's
experience of their constraints. Let the world's vocabulary, sensory
texture, cultural practices, and coping behaviors emerge from that
inhabitation — not from a checklist.

The constraint specifications tell you the structural physics.
The inhabitation sentences tell you what it feels like.
The blueprint tells you the shape.
Write the story that lives at the intersection of all three.
```

**Priorities (in order):**
1. **Inhabitation.** The reader must feel they are inside this world, inside these constraints, inside this body. If the structure is perfect but the experience is flat, the story fails.
2. **Structural fidelity.** Constraint topology preserved. Indexical variance drives conflict. Characters rational from their positions.
3. **Framework invisibility.** Zero framework terminology. Zero meta-commentary. Zero allegorical signaling.
4. **Voice consistency.** Maintain selected archetype throughout (or shift only at planned transitions).

**What to trust:**
- Trust the inhabitation sentence to generate texture. If you're inside "a child in a system that measures your worth by a number on your wrist," the sensory details, the vocabulary, the coping behaviors follow. You don't need to be told to add them.
- Trust the constraint structure to generate conflict. If the specifications say two characters route to different types from the same constraint, the collision is already there. You don't need to manufacture drama.
- Trust the voice to carry meaning. If you've committed to The Condemned, the voice limits what the narrator can know and articulate. Those limits are the story's texture, not its weakness.

**What to avoid:**
- Installing vocabulary from a list rather than speaking in the world's language
- Adding sensory detail because a protocol says to rather than because the character would notice it
- Performing coping behaviors rather than inhabiting them
- Explaining the constraint structure to the reader through any mechanism

### ABSOLUTE PROHIBITIONS

```
DO NOT:
- Use framework terminology in narrative or dialogue
- Quote Stage 1 specification language
- Make constraints obviously symbolic or allegorical
- Explain what you're doing to the reader
- Reference the original source material
- Use abstract nouns like "the system" or "the mechanism"
- Break genre to explain constraint logic

INSTEAD:
- Show constraints through specific details
- Let reader discover patterns
- Stay in the world completely
- Trust the structure to do its work
```

### Output

```
[Complete narrative text]
[No Stage 0-3 artifacts visible]
[No framework terminology]
[This is the final story — not a draft awaiting deepening]
```

---

## STAGE 5: SUBTRACTIVE AUDIT

**Model:** Claude or equivalent analytical model  
**Input:** Stage 4 output + Stage 1 specifications  
**Output:** Revised narrative (shorter than input) + validation report

### Purpose

Stage 5 removes what doesn't belong. It does not add texture, vocabulary, sensory detail, or coping behaviors. It identifies what is FORCED rather than EARNED, what is DEPLOYED rather than INHABITED, what is SPECIFIED rather than EXPERIENCED — and removes it.

**The principle:** Subtraction > Addition. Improvement usually means removing noise. The best version of the story is what remains after everything that isn't the felt experience of being inside these constraints has been taken away.

**Derivation:** This stage descends from UKE_Editing's founding principle ("Truth > Logic > Flow. Never polish a lie. Subtraction > Addition.") and 07_phenomenology's audit structure (EARNED/FORCED/ABSENT). It is not a creative pass. It is a discipline.

### Subtractive Operations

**1. EARNED or FORCED?**
```
For each significant element (sensory detail, vocabulary term, cultural
practice, coping behavior, emotional moment):

EARNED: Emerges from the constraint experience. The character would
  notice this. The world produces this. Remove it and something is lost.

FORCED: Added because a protocol or convention says it should be there.
  Exists to demonstrate texture rather than to inhabit it. Remove it
  and the story gets lighter without losing meaning.

Action: Cut everything FORCED. Keep everything EARNED.
```

**2. INHABITED or DEPLOYED?**
```
For each linguistic element (code-switch, vocabulary term, dialect marker):

INHABITED: The character speaks this way because of who they are,
  where they are, and who they're talking to. The language carries
  social meaning.

DEPLOYED: The term appears because Stage 2 specified it. It sits
  in the text like a foreign object. The character wouldn't say this;
  the protocol told them to.

Action: Strip everything DEPLOYED. Keep everything INHABITED.
```

**3. Anti-pattern scan:**
```
Check for and remove:
- Math explaining ("this represents...")
- Meta-commentary ("this illustrates...")
- Pedagogical framing ("let me explain...")
- Therapeutic language ("journey," "growth," "healing" used abstractly)
- Cheap momentum ("suddenly," "then" as crutches)
- False humanity (emotions entities can't have)
- Explaining feeling (naming instead of showing)
- Counting tics (exact numbers repeated mechanically across scenes)
- Framework residue (constructed vocabulary that smells like terminology)
- Voice drift (passages where the voice breaks archetype)
```

**4. Compression:**
```
The Iceberg Rule: If a sentence can lose 20% of its words without
losing meaning, cut them.

The Compression Floor: Do not compress below the point where nuance
or necessary uncertainty is lost.

The inhabitation test: After compression, does each remaining sentence
still feel like it comes from inside the constraint experience?
If a sentence now feels like it comes from outside — from the
protocol, from the framework, from the author — cut it entirely.
```

### Validation

After subtractive operations, validate structural preservation:

```xml
<validation_report>
  <constraint_preservation>
    <constraint source_id="C1">
      <found_in_output>yes</found_in_output>
      <routing_preservation>
        <source_routing>
          Powerless → Snare, Moderate → Tangled, Powerful → Rope
        </source_routing>
        <output_routing>
          Powerless → Snare, Moderate → Tangled, Powerful → Rope
        </output_routing>
        <pass>true</pass>
      </routing_preservation>
    </constraint>
  </constraint_preservation>
  
  <topology_isomorphism>
    <source_network>C1 → C2 → C3</source_network>
    <output_network>C1' → C2' → C3'</output_network>
    <isomorphic>true</isomorphic>
  </topology_isomorphism>
  
  <framework_invisibility>
    <grep_results>[all banned terms = 0]</grep_results>
    <pass>true</pass>
  </framework_invisibility>
  
  <subtractive_report>
    <original_word_count>[N]</original_word_count>
    <final_word_count>[N] (must be ≤ original)</final_word_count>
    <elements_removed>
      [List what was cut and why: FORCED/DEPLOYED/anti-pattern/compression]
    </elements_removed>
  </subtractive_report>
  
  <origin_obfuscation>
    <from_plot_beats>recognizable: [yes/no]</from_plot_beats>
    <from_character_types>recognizable: [yes/no]</from_character_types>
    <from_emotional_register>recognizable: [yes/no]</from_emotional_register>
  </origin_obfuscation>
  
  <literary_quality>
    <opening>[strong/adequate/weak]</opening>
    <pacing>[effective/uneven/poor]</pacing>
    <ending>[powerful/satisfactory/weak]</ending>
    <prose>[publication-ready/needs polish/requires revision]</prose>
  </literary_quality>
  
  <overall_result>[PASS/FAIL]</overall_result>
</validation_report>
```

**If FAIL:**
```
Routing violation → Return to Stage 4
Framework terminology leaked → Return to Stage 4
Origin recognizable → Return to Stage 2
Voice inconsistent → Return to Stage 4 with stricter voice constraint
Prose quality weak → Run subtractive operations again (cut more, not add)
```

---

## QUICK REFERENCE

### Constraint Types

**Consult logic_thresholds.md §3 for current threshold values.**

```
Mountain (■):    Minimal ε, no enforcement, immutable from index
Rope (⊞):       Low χ, low ε, genuine coordination, changeable
Snare (⊠):      High χ, high ε, high suppression, no coordination
Tangled (⊞⊠):   Mid-range χ, coordination + asymmetric extraction
Scaffold (⊡):   Low χ, coordination, sunset clause
Piton (⊟):      Minimal χ, maintenance cost, high theater
```

### Power/Scope Modifiers

```
π(powerless) = 1.5    π(moderate) = 1.0      π(powerful) = 0.6
π(organized) = 0.4    π(institutional) = -0.2 π(analytical) = 1.15

σ(local) = 0.8        σ(regional) = 0.9      σ(national) = 1.0
σ(continental) = 1.1  σ(global) = 1.2        σ(universal) = 1.0
```

### Attractor Decision Matrix

| Constraint Profile | PIR Model | BIR Model | Recommended Attractors |
|-------------------|-----------|-----------|----------------------|
| Mountain-dominated | Yes | Yes | Tragedy, Seeded Possibility |
| Mountain + Snares | Yes | N/A | Deterministic Tragedy |
| Tangled Ropes dominant | Yes | N/A | Tragedy (crushed by hybrid) |
| Tangled Ropes dominant | N/A | Yes | **Negotiated Equilibrium** |
| Pure Snares, no Mountains | Yes | N/A | Tragedy or Revolutionary Rupture |
| Pure Snares, no Mountains | N/A | Yes | Equilibrium or Rupture |
| Piton present | Either | Either | Seeded Possibility |

### Common Failure Patterns

```
All characters see same type → Adjust ε or add power differential
Only Ropes/Snares, no Tangled → ~36% should be Tangled, re-examine
Framework leaks → Regenerate Stage 4 with invisibility constraint
Personality conflict not structural → Make all rational from index
Allegory feel → Cut what's forced, not add more detail
Metrics treated as truth → Reframe as routing
Topology violated → Check routing preservation, not ε precision
Flat prose, competent structure → Inhabitation sentence was wrong (return to Stage 2)
Language uniformity → Linguistic strategy not inhabitated (return to Stage 2)
Voice drift → Regenerate Stage 4 with stricter archetype adherence
```

---

## APPENDIX A: MODEL SELECTION GUIDE

```
Stage 0 (Extraction):     Gemini (analytical, constraint detection)
Stage 1 (Formalization):  Copilot (precise specification, formal logic)
Stage 2 (Naturalization): Claude (creative, cultural grounding)
Stage 3 (Planning):       ChatGPT (structured planning, scene design)
Stage 4 (Generation):     Varies by genre (Claude: literary; GPT-4: hard SF;
                          Gemini: fantasy; ChatGPT: mystery/thriller)
Stage 5 (Audit):          Claude (analytical, subtractive discipline)
```

---

## APPENDIX B: THE PRINCIPLE

The kids version of the Ship of Theseus contains the entire framework in six words: "One plank out. One plank in." No constructed vocabulary. No sensory palette. No motif structure. No coping behavior inventory. Just a ship that does what it must, night after night, until the story is told.

Stage 5's prototype was that kids version — produced by taking a structurally complete mechanical draft and removing everything that wasn't the felt experience of being inside the mechanism. What remained was: "Inside, the ship grows quieter." That sentence was not added. It was what was left.

The pipeline's theory of quality, stated once:

**Start from what it feels like inside the mechanism. State that as simply as possible. Let the structure be implicit. Remove everything that isn't the experience of being inside it.**

Constrain harder. Specify less. Trust inhabitation. Subtract to finish.

---

## APPENDIX C: CREATIVITY AMPLIFICATION EXAMPLES

### Example A: Europa Colony (Temporal Displacement + Creole)

**Setting:** 2247 CE, Nguyen-Jama Station, Europa orbital platform. Vietnamese refugee descendants (2091 climate exodus) and Somali diaspora (2103 water wars).

**Linguistic strategy:** Việt-Somali-English creole.
- "Bác needs check your habo today" (Uncle/oxygen allocation)
- "Đồng-crew staying walaalo" (Work-crew staying loyal/brother-like)

**Inhabitation sentence (powerless):** "You measure your life in breaths you haven't paid for yet, and you speak to your mother in a language the company doesn't recognize as language."

### Example B: Tang Dynasty (Cultural/Historical)

**Setting:** 740 CE, Chang'an. Imperial examination system + patronage networks.

**Linguistic strategy:** Classical Chinese literary style, direct translation preserving chengyu.

**Inhabitation sentence (moderate):** "You have written three thousand poems in the style the examiners prefer and one, in your own hand, that you keep in a box you don't open."

### Example C: 1920s Chicago (Noir Realism + Code-Switching)

**Setting:** 1923, Back of the Yards, Polish immigrant meatpacking community.

**Linguistic strategy:** Polish-English code-switching. Polish for family, church, intimacy. English for work, foreman, official.

**Inhabitation sentence (powerless):** "You pray in Polish and beg in English and the foreman doesn't know the difference because to him they're the same sound."

---

## APPENDIX D: FUTURE — PROLOG ENGINE INTEGRATION

**Current state:** This pipeline runs LLM-only. The LLM estimates ε, classifies types, and validates topology — all without the Prolog engine.

**Known limitation:** Without the engine, the LLM cannot run Boltzmann compliance tests, compute purity scores, or verify classifications against `classify_from_metrics/6`.

**Mitigation:** The routing frame reduces the impact. LLM estimates are ±0.10 fuzzy, but if they route to the right types with correct relative ordering, narrative output is sound.

**Integration path (future):**
- **Stage 0:** LLM estimates → Prolog classifies → LLM uses authoritative types
- **Stage 5:** LLM re-extracts → Prolog validates → formal topology check

---

**END OF PROTOCOL**

UKE_Narrative v1.4  
Reference: `logic_narrative.md` v4.1 for classification system  
Reference: `logic_thresholds.md` for canonical threshold values  
Reference: `metrics_as_routing.md` for metrics philosophy  
License: CC BY-SA 4.0  
Version: February 2026
