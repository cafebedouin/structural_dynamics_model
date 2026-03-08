# STAGE 1: FORMAL SPECIFICATION
## The Old Man and the Sea - Operational Constraint System

---

## I. CONSTRAINT FORMALIZATIONS

### C1: The Fisherman's Code

**Base Properties:**
```
ε(C1) = 0.30
Supp(C1) = 0.80
Coordination = TRUE
Asymmetric = FALSE
```

**Indexed Classifications:**

**Agent_A (moderate, biographical, trapped, local):**
```
I_Agent_A = (P=moderate, T=biographical, E=trapped, S=local)
π(moderate) = 1.0
σ(local) = 0.8

χ(C1, Agent_A) = ε × π × σ
                = 0.30 × 1.0 × 0.8
                = 0.24

Classification: ROPE (⊞)
Threshold check: χ = 0.24 < 0.46 (Rope/Tangled boundary)
```

**Structural signature verification:**
- ✓ Low extraction (χ = 0.24)
- ✓ Provides coordination (professional standards)
- ✓ Minimal enforcement needed (self-imposed)
- ✓ Changeable in principle (could abandon code)

**Experience:** Self-imposed discipline that structures identity. The code transforms suffering into meaning through precision and endurance. Not externally enforced—Agent_A maintains it even when alone at sea.

---

**Agent_B (powerful, biographical, mobile, local):**
```
I_Young = (P=powerful, T=biographical, E=mobile, S=local)
π(powerful) = 0.6
σ(local) = 0.8

χ(C1, Young) = 0.30 × 0.6 × 0.8
              = 0.144

Classification: ROPE (⊞)
Threshold check: χ = 0.144 < 0.46
```

**Structural signature verification:**
- ✓ Very low extraction (χ = 0.144)
- ✓ Provides coordination (professional standards)
- ✓ Technology mediates burden
- ✓ Exit options available (can change methods)

**Experience:** Professional best practices mediated by technology. The code is instrumental—a means to economic efficiency rather than existential validation. Can be modified or abandoned based on results.

**Indexical Variance:**
```
Δχ = 0.24 - 0.144 = 0.096
Both classify as Rope, but Agent_A experiences 67% more extraction.
Variance source: Power differential (1.0 vs 0.6)
```

The same professional code is existential discipline for Agent_A (trapped in traditional methods, no alternatives) versus instrumental technique for younger fishermen (mobile, technology-mediated, multiple approaches available).

---

### C2: The Law of Luck

**Base Properties:**
```
ε(C2) = 0.60
Supp(C2) = 0.50
Coordination = TRUE (social coordination through reputation)
Asymmetric = TRUE (burden falls on "unlucky")
```

**Indexed Classifications:**

**Agent_A (powerless, biographical, trapped, local):**
```
I_Agent_A = (P=powerless, T=biographical, E=trapped, S=local)
π(powerless) = 1.5
σ(local) = 0.8

χ(C2, Agent_A) = 0.60 × 1.5 × 0.8
                = 0.72

Classification: SNARE (⊠)
Threshold check: χ = 0.72 > 0.66 (Tangled/Snare boundary)
```

**Structural signature verification:**
- ✓ High extraction (χ = 0.72)
- ✓ Requires enforcement (social pressure, economic exclusion)
- ✓ Asymmetric burden (only "unlucky" suffer)
- ✓ No genuine exit (E=trapped, cannot escape reputation)

**Experience:** Inescapable trap of social reputation. The label "salao" (definitively unlucky) operates as social fact with material consequences—loss of apprentice, isolation, economic marginalization. Cannot be escaped through skill or effort.

---

**Agent_D_guardians (moderate, biographical, mobile, local):**
```
I_Parents = (P=moderate, T=biographical, E=mobile, S=local)
π(moderate) = 1.0
σ(local) = 0.8

χ(C2, Parents) = 0.60 × 1.0 × 0.8
                = 0.48

Classification: TANGLED ROPE (⊞⊠)
Threshold check: 0.46 < χ = 0.48 < 0.66
```

**Structural signature verification:**
- ✓ Moderate extraction (χ = 0.48)
- ✓ Has coordination value (reputation system allocates resources)
- ✓ Has extraction component (excludes based on luck, not skill)
- ✓ Requires active enforcement (social pressure)

**Experience:** Pragmatic management tool. The reputation system provides useful information for economic decisions (which boats catch fish) while also creating social exclusion. Parents navigate this by moving their son to a "lucky" boat—using the system's coordination function while deflecting its extraction.

**Indexical Variance:**
```
Δχ = 0.72 - 0.48 = 0.24
Agent_A: SNARE (trapped by reputation)
Parents: TANGLED ROPE (can navigate system)
Variance source: Power (1.5 vs 1.0) + Exit (trapped vs mobile)
```

**Critical structural insight:** Same belief system functions as:
- **Oppression** for Agent_A (powerless, trapped—cannot escape label)
- **Coordination mechanism** for parents (moderate power, mobile—can move son between boats)

This is **not** disagreement about facts. Both classifications are objectively true from their respective indices.

---

### C3: A Son's Duty

**Base Properties:**
```
ε(C3) = 0.50
Supp(C3) = 0.70
Coordination = TRUE (family structure, intergenerational support)
Asymmetric = TRUE (burden on child)
```

**Indexed Classifications:**

**Agent_D (powerless, biographical, trapped, local):**
```
I_Agent_D = (P=powerless, T=biographical, E=trapped, S=local)
π(powerless) = 1.5
σ(local) = 0.8

χ(C3, Agent_D) = 0.50 × 1.5 × 0.8
                = 0.60

Classification: TANGLED ROPE (⊞⊠)
Threshold check: 0.46 < χ = 0.60 < 0.66
```

**Structural signature verification:**
- ✓ Moderate-high extraction (χ = 0.60, upper Tangled range)
- ✓ Has coordination value (family support structure)
- ✓ Has extraction component (emotional suffering, agency restriction)
- ✓ Requires enforcement (parental authority, social expectation)

**Experience:** Painful conflict between love and duty. The obligation to obey provides family structure and economic support (coordination) while forcing separation from Agent_A and restricting agency (extraction). Cannot simply accept or reject—must navigate genuine benefits entangled with genuine costs.

---

**Agent_A (moderate, biographical, analytical, local):**
```
I_Agent_A = (P=moderate, T=biographical, E=analytical, S=local)
π(moderate) = 1.0
σ(local) = 0.8

χ(C3, Agent_A) = 0.50 × 1.0 × 0.8
                = 0.40

Classification: ROPE (⊞)
Threshold check: χ = 0.40 < 0.46
```

**Structural signature verification:**
- ✓ Low extraction (χ = 0.40)
- ✓ Provides coordination (family structure, social order)
- ✓ Minimal burden from outside (not his duty to enforce)
- ✓ Analytical exit (can observe without being subject)

**Experience:** Normal and correct social structure. Agent_A sees the boy's obedience as proper functioning of family hierarchy—the way things should be. No resentment because he's not subject to the constraint (analytical position) and recognizes its coordination value.

**Indexical Variance:**
```
Δχ = 0.60 - 0.40 = 0.20
Agent_D: TANGLED ROPE (painful conflict, trapped in duty)
Agent_A: ROPE (functional social order, analytical observer)
Variance source: Power (1.5 vs 1.0) + Exit (trapped vs analytical)
```

**Critical structural insight:** The boy experiences the constraint as **irreducible complexity** (genuine love for Agent_A + genuine duty to parents = impossible choice). Agent_A experiences it as **functional coordination** (proper family structure). Both correct from their indices.

---

## II. TRANSFORMATION RULES

### TR1: Collective Organization (C2 Transformation)

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>character_action = "organize_collective"</condition>
    <condition>target_constraint = "C2"</condition>
    <condition>collective_size ≥ threshold</condition>
  </trigger>
  
  <index_change>
    <character>Agent_A</character>
    <from>
      <power>powerless (π=1.5)</power>
      <exit>trapped</exit>
    </from>
    <to>
      <power>organized (π=0.4)</power>
      <exit>constrained</exit>
    </to>
  </index_change>
  
  <consequence>
    <chi_recalculation>
      <before>χ = 0.60 × 1.5 × 0.8 = 0.72</before>
      <after>χ = 0.60 × 0.4 × 0.8 = 0.192</after>
    </chi_recalculation>
    <type_change>
      <from>SNARE (⊠)</from>
      <to>ROPE (⊞)</to>
    </type_change>
    <mechanism>
      Collective action shares burden of reputation system.
      "Unlucky" fishermen form cooperative, pool catches,
      distribute based on need rather than individual luck.
      Reputation system loses power when collective can
      buffer individual variance.
    </mechanism>
  </consequence>
  
  <narrative_manifestation>
    Agent_A and other "salao" fishermen form cooperative.
    Share equipment, pool catches, support each other during
    dry spells. Parents can no longer use "unluckiness" to
    justify moving Agent_D—cooperative demonstrates stable
    income despite individual variance.
  </narrative_manifestation>
  
  <preconditions>
    <required>Sufficient "unlucky" fishermen exist</required>
    <required>Communication possible (local scope enables)</required>
    <required>No institutional suppression of organizing</required>
  </preconditions>
</transformation_rule>
```

---

### TR2: Reputation Redemption (C2 Transformation)

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>character_action = "exceptional_catch"</condition>
    <condition>target_constraint = "C2"</condition>
    <condition>catch_size > community_threshold</condition>
  </trigger>
  
  <index_change>
    <character>Agent_A</character>
    <from>
      <power>powerless (π=1.5)</power>
    </from>
    <to>
      <power>moderate (π=1.0)</power>
    </to>
  </index_change>
  
  <consequence>
    <chi_recalculation>
      <before>χ = 0.60 × 1.5 × 0.8 = 0.72</before>
      <after>χ = 0.60 × 1.0 × 0.8 = 0.48</after>
    </chi_recalculation>
    <type_change>
      <from>SNARE (⊠)</from>
      <to>TANGLED ROPE (⊞⊠)</to>
    </type_change>
    <mechanism>
      Exceptional catch proves skill, disrupts "salao" label.
      Reputation system recalibrates—Agent_A no longer
      definitively unlucky. Gains moderate power to negotiate
      within system (can attract apprentice, get better prices).
      But system itself unchanged—still coordinates through
      reputation, still extracts from those labeled unlucky.
    </mechanism>
  </consequence>
  
  <narrative_manifestation>
    Agent_A returns with marlin skeleton. Despite sharks
    destroying meat, size proves his skill. Community
    re-evaluates: "He went out too far" (respect for
    ambition) rather than "He is unlucky" (dismissal).
    Agent_D's parents might reconsider. But reputation
    system still operates—just Agent_A's position within
    it has changed.
  </narrative_manifestation>
  
  <limitations>
    <note>Does NOT eliminate C2 (system persists)</note>
    <note>Only changes Agent_A's position within system</note>
    <note>Other "unlucky" fishermen still trapped</note>
    <note>Temporary—new dry spell could restore "salao" label</note>
  </limitations>
</transformation_rule>
```

---

### TR3: Filial Emancipation (C3 Transformation)

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>character_action = "assert_independence"</condition>
    <condition>target_constraint = "C3"</condition>
    <condition>economic_independence = TRUE</condition>
  </trigger>
  
  <index_change>
    <character>Agent_D</character>
    <from>
      <power>powerless (π=1.5)</power>
      <exit>trapped</exit>
    </from>
    <to>
      <power>moderate (π=1.0)</power>
      <exit>constrained</exit>
    </to>
  </index_change>
  
  <consequence>
    <chi_recalculation>
      <before>χ = 0.50 × 1.5 × 0.8 = 0.60</before>
      <after>χ = 0.50 × 1.0 × 0.8 = 0.40</after>
    </chi_recalculation>
    <type_change>
      <from>TANGLED ROPE (⊞⊠)</from>
      <to>ROPE (⊞)</to>
    </type_change>
    <mechanism>
      Economic independence enables negotiation with parents.
      Duty remains (coordination value of family structure)
      but extraction reduced (can choose fishing partner
      while maintaining family ties). Exit changes from
      trapped (must obey) to constrained (can negotiate
      at cost of family tension).
    </mechanism>
  </consequence>
  
  <narrative_manifestation>
    Agent_D earns enough to support himself. Tells parents:
    "I will fish with the old man. I will still help the
    family, but this is my choice." Parents object but
    cannot force compliance—economic leverage gone.
    Agent_D experiences duty as chosen commitment rather
    than imposed obligation.
  </narrative_manifestation>
  
  <preconditions>
    <required>Economic independence achievable</required>
    <required>Family structure allows negotiation</required>
    <required>Social norms permit adult autonomy</required>
  </preconditions>
</transformation_rule>
```

---

### TR4: Code Abandonment (C1 Transformation)

```xml
<transformation_rule id="TR4">
  <trigger>
    <condition>character_action = "abandon_code"</condition>
    <condition>target_constraint = "C1"</condition>
    <condition>survival_pressure > threshold</condition>
  </trigger>
  
  <index_change>
    <character>Agent_A</character>
    <from>
      <time>biographical</time>
    </from>
    <to>
      <time>immediate</time>
    </to>
  </index_change>
  
  <consequence>
    <chi_recalculation>
      <note>χ unchanged (0.24) but meaning transforms</note>
    </chi_recalculation>
    <type_change>
      <from>ROPE (⊞) - existential discipline</from>
      <to>ROPE (⊞) - instrumental technique</to>
    </type_change>
    <mechanism>
      Time horizon collapse (biographical → immediate)
      transforms code from identity-constituting discipline
      to survival technique. Still coordinates (professional
      standards work), but no longer provides existential
      meaning. Agent_A becomes like younger fishermen—
      code as tool, not essence.
    </mechanism>
  </consequence>
  
  <narrative_manifestation>
    Facing starvation, Agent_A cuts corners: imprecise
    knots, shortcuts in preparation, focus on results
    over process. "A man can be destroyed but not defeated"
    becomes "A man must eat." Code persists as technique
    but loses spiritual dimension. Precision serves
    efficiency, not identity.
  </narrative_manifestation>
  
  <cost>
    <identity_loss>
      Source of dignity and meaning evaporates.
      Agent_A survives but becomes indistinguishable
      from younger fishermen he once pitied.
    </identity_loss>
  </cost>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### E1: False Mountain (Type I Error)

**Pattern:** Treating C2 (Law of Luck) as unchangeable natural law.

**Manifestation in Agent_A:**
```
Observable actions:
- Accepts "salao" label without resistance
- Does not organize with other unlucky fishermen
- Treats reputation as fate rather than social construction
- Says: "I am unlucky" (ontological claim) not "They call me unlucky" (social fact)

Internal experience:
- Experiences social exclusion as natural consequence
- Does not imagine collective action as possibility
- Attributes isolation to personal failing, not system design

Index explanation:
I = (powerless, biographical, trapped, local)
- Powerless: Cannot see beyond immediate power relations
- Trapped: No exit options make system appear immutable
- Biographical: Cannot imagine generational change
- Local: Face-to-face enforcement feels natural, not constructed

Why error is rational from this index:
From Agent_A's position, C2 genuinely appears as Mountain.
No visible enforcement mechanism (operates through distributed
social pressure). No examples of successful resistance.
Time horizon too short to imagine systemic change.
```

**Consequence:**
- Wasted agency (could organize but doesn't)
- Unnecessary suffering (accepts isolation as inevitable)
- Premature surrender (doesn't attempt reputation repair)

**Correction trigger:**
- Analytical character reveals enforcement mechanism
- Collective action example demonstrates changeability
- Index shift (powerless → organized) makes alternatives visible

---

### E2: Snare-as-Rope (Type III Error)

**Pattern:** Treating C2 (Law of Luck) as pure coordination, missing extraction.

**Manifestation in Agent_D_guardians:**
```
Observable actions:
- Use reputation system to make economic decisions
- Move son to "lucky" boat without questioning system
- Treat Agent_A's exclusion as unfortunate but necessary
- Say: "The system works—lucky boats catch more fish"

Internal experience:
- See reputation as useful information, not oppression
- Experience coordination benefits (resource allocation)
- Don't feel extraction (not subject to "unlucky" label)

Index explanation:
I = (moderate, biographical, mobile, local)
- Moderate power: Can navigate system, deflect costs
- Mobile: Can move son between boats (exit option)
- Local: See coordination function (who catches fish)
- Don't experience: Powerless position where system is trap

Why error is rational from this index:
From parents' position, C2 genuinely functions as Tangled Rope
(coordination + extraction). They experience coordination
benefits and can navigate extraction. Don't see Snare
experience because they're not subject to it.
```

**Consequence:**
- Voluntary participation in Agent_A's oppression
- Perpetuation of extractive system
- Moral blindness to asymmetric burden

**Correction trigger:**
- Agent_A's perspective revealed (shows Snare experience)
- Collective action demonstrates system's extractive nature
- Empathy bridge (imagine being labeled "salao")

---

### E3: Tangled Rope Mishandling (Type V.b Error)

**Pattern:** Treating C3 (Son's Duty) as pure Snare, missing coordination value.

**Manifestation in Hypothetical Rebellious Agent_D:**
```
Observable actions:
- Rejects all parental authority
- Abandons family obligations entirely
- Treats duty as pure oppression
- Says: "Family structure is just control—I owe them nothing"

Internal experience:
- Experiences only extraction (agency restriction, emotional pain)
- Cannot see coordination value (economic support, social structure)
- Frames duty as pure imposition, not reciprocal relationship

Index explanation:
I = (powerless, biographical, trapped, local)
- Powerless: Feels full weight of obligation
- Trapped: Cannot negotiate, only obey or rebel
- Biographical: Cannot see long-term reciprocity
- Focuses on: Immediate extraction (separation from Agent_A)

Why error is rational from this index:
From powerless position, extraction is most salient.
Coordination benefits (family support) feel like
compensation for oppression, not genuine mutual benefit.
```

**Consequence:**
- Destroys functional family structure
- Loses economic support system
- Isolation (no Agent_A, no family)

**Correction trigger:**
- Economic independence enables seeing coordination value
- Time horizon extension (biographical → generational)
- Recognition of reciprocity (parents also constrained)

---

### E4: Mountain Denial (Type II Error)

**Pattern:** Treating C4 (Bodily Decline) as changeable through will.

**Manifestation in Agent_A:**
```
Observable actions:
- Pushes body beyond physical limits
- Ignores pain signals, exhaustion warnings
- Says: "I can be destroyed but not defeated"
- Treats aging as challenge to overcome, not reality to accept

Internal experience:
- Experiences physical limits as tests of character
- Believes willpower can transcend biology
- Frames decline as moral failing, not natural process

Index explanation:
I = (moderate, biographical, trapped, local)
- Biographical: Sees lifetime as arena for proving worth
- Trapped: No exit from aging, so must fight it
- Moderate: Has agency in other domains, expects it here too

Why error is rational from this index:
Professional code (C1) teaches that precision and endurance
overcome obstacles. Agent_A applies same logic to biology.
No analytical distance to recognize category difference
between social constraints (changeable) and natural laws
(unchangeable).
```

**Consequence:**
- Energy depletion fighting entropy
- Physical damage from ignoring limits
- Burnout (exhaustion without acceptance)

**Correction trigger:**
- Catastrophic failure (body gives out)
- Analytical perspective (someone explains natural limits)
- Time horizon extension (accepts decline as part of life cycle)

---

## IV. INSTITUTIONAL RATIONALITY MODEL

**Selected Model: BOUNDED INSTITUTIONAL RATIONALITY (BIR)**

### Justification

The Old Man and the Sea depicts **realistic human institutions** operating under uncertainty with principal-agent problems:

1. **Fishing community** (not algorithmic system):
   - Reputation system based on incomplete information
   - Social pressure, not deterministic enforcement
   - Negotiation possible (parents can change minds)
   - Risk aversion (parents move son to "safe" boat)

2. **Family structure** (not implacable law):
   - Parental authority bounded by economic reality
   - Agent_D can negotiate (brings food, maintains relationship)
   - Satisficing behavior (parents accept "good enough" boat)
   - Principal-agent problem (parents' goals ≠ Agent_D's goals)

3. **Professional code** (not natural law):
   - Self-imposed, can be abandoned
   - Satisficing (Agent_A doesn't achieve perfection, just "good enough")
   - Uncertainty (luck matters despite skill)

**PIR would be inappropriate** because:
- No perfect information (reputation system is noisy)
- No unbounded utility maximization (characters satisfice)
- Negotiation is central (not just Pareto-improving trades)
- Institutions are human-scale, not algorithmic

### Implications for Attractor Selection

**BIR enables:**
- Negotiated Equilibrium (characters bargain within constraints)
- Seeded Possibility (underground transformation while surface persists)

**BIR excludes:**
- Deterministic Tragedy (requires PIR—implacable logic running to completion)

---

## V. TERMINAL ATTRACTOR SELECTION

**Selected Attractor: SEEDED POSSIBILITY**

### Structural Justification

**Constraint topology:**
```
C1 (Fisherman's Code): ROPE for both characters
  → Stable, self-maintaining
  → No transformation pressure

C2 (Law of Luck): SNARE (Agent_A) / TANGLED ROPE (Parents)
  → High extraction for powerless
  → Transformation possible but not inevitable
  → Surface persistence likely

C3 (Son's Duty): TANGLED ROPE (Agent_D) / ROPE (Agent_A)
  → Irreducible complexity
  → Surgical reform possible
  → Underground transformation viable
```

**Why not Deterministic Tragedy:**
- BIR model (not PIR) → negotiation possible
- No load-bearing Snare (cutting C2 doesn't collapse system)
- Characters have agency (not implacable logic)
- Transformation rules exist (TR1, TR2, TR3)

**Why not Negotiated Equilibrium:**
- Power asymmetry too severe (Agent_A powerless, trapped)
- No bargaining leverage for Agent_A
- Parents have no incentive to negotiate (system works for them)
- Would require index shift (powerless → organized) first

**Why not Revolutionary Rupture:**
- No constraint logic disruption
- No fundamental category breakdown
- Institutions remain coherent (fishing, family, reputation)
- Would require external shock (not present in story)

**Why Seeded Possibility:**

1. **Surface tragedy:**
   - Agent_A remains "salao" (C2 persists as Snare)
   - Dies without catching fish that matters economically
   - Reputation system unchanged
   - Parents' decision stands

2. **Underground transformation:**
   - Agent_D's loyalty seeds future change (TR3 precondition)
   - Agent_A's endurance demonstrates code's value (C1 transmission)
   - Marlin skeleton proves skill (TR2 partial activation)
   - Next generation positioned differently (Agent_D has analytical distance)

3. **Structural compatibility:**
   - BIR allows underground organizing
   - Tangled Ropes enable surgical reform
   - Local scope enables face-to-face transmission
   - Biographical time horizon allows generational shift

### Narrative Manifestation

**Surface (visible to community):**
- Old man dies poor and alone
- Reputation system vindicated ("He was unlucky")
- Parents' decision appears correct
- Traditional order maintained

**Underground (visible to Agent_D and reader):**
- Code transmitted to next generation
- Loyalty demonstrates alternative values
- Skill proven despite economic failure
- Seeds planted for future transformation

**Attractor mechanics:**
```
t=0: Agent_A trapped in C2 (Snare)
t=1: Exceptional catch (TR2 partial activation)
t=2: Sharks destroy catch (surface tragedy)
t=3: Agent_D witnesses endurance (underground transmission)
t=4: Agent_A dies (surface tragedy complete)
t=5: Agent_D carries code forward (seeded possibility)
t=∞: Next generation positioned for TR1 or TR3
```

---

## VI. STRUCTURAL PHYSICS ARCS (OPTIONAL)

### Arc 1: False Mountain Reveal (C2)

**Structure:**
```
Stage 1: Naturalization
- Community treats reputation system as natural law
- "Some men are lucky, some are not—that's just how it is"
- No visible enforcement (distributed social pressure)
- Agent_A accepts label as ontological fact

Stage 2: Evidence Accumulation
- Reputation varies by observer (parents see coordination, Agent_A sees trap)
- System requires active maintenance (gossip, social pressure, economic exclusion)
- Burden varies by power position (asymmetric extraction)
- Scope-dependent (works at local scale, would fail at global)

Stage 3: Reveal
- Analytical character (Agent_D?) recognizes construction
- "They call you unlucky—but you're the most skilled fisherman here"
- Enforcement mechanism visible (parents' decision based on reputation, not skill)
- Natural law doesn't need police; this system does

Stage 4: Character Choice
- Agent_A: Continue accepting (Type I error) or organize (TR1)
- Agent_D: Recognize system as changeable, position for future transformation
- Parents: Defend system (benefits them) or acknowledge extraction
```

**Boltzmann Test Application:**
```
Natural law check:
- Does C2 bind everyone equally? NO (powerless trapped, moderate navigate)
- Does C2 operate same at all scales? NO (local coordination, global would fail)
- Does C2 require enforcement? YES (social pressure, economic exclusion)

Conclusion: C2 is constructed constraint masquerading as natural law.
It couples Power × Scope (independent dimensions in natural laws).
```

**Narrative Impact:**
- Transforms Agent_A's tragedy from inevitable to constructed
- Creates moral stakes (system is changeable but unchanged)
- Positions Agent_D for future action (knows it's not natural)

---

### Arc 2: Purity Drift (C1)

**Structure:**
```
Stage 1: High Purity
- Code genuinely coordinates (professional standards work)
- Minimal theater (precision serves function)
- Agent_A's generation: Code = identity + effectiveness

Stage 2: Subtle Degradation
- Younger generation: Code = technique only
- Technology mediates (GPS, sonar replace skill-based judgment)
- Theater increases (maintain appearance of code without substance)
- Coordination function hollows out (technology does the work)

Stage 3: Pre-Symptomatic Decay
- Surface metrics stable (boats still catch fish)
- Underlying purity dropping (code becoming ritual)
- Agent_A notices ("They don't know the sea like we did")
- Others dismiss ("Old man is stuck in the past")

Stage 4: Visible Crisis
- Technology fails (GPS breaks, sonar malfunctions)
- Younger fishermen helpless (code was theater, not skill)
- Agent_A's knowledge suddenly valuable (purity was real)
- But too late—transmission interrupted (Agent_D moved to other boat)
```

**Purity Calculation:**
```
Agent_A's generation:
  Coordination value: HIGH (code enables skill)
  Extraction: LOW (self-imposed discipline)
  Theater: LOW (precision serves function)
  Purity: HIGH (~0.85)

Younger generation:
  Coordination value: MEDIUM (technology does work)
  Extraction: LOW (still self-imposed)
  Theater: MEDIUM (maintain appearance)
  Purity: MEDIUM (~0.60)

Future generation (without transmission):
  Coordination value: LOW (technology dependency)
  Extraction: LOW (abandoned when inconvenient)
  Theater: HIGH (ritual without understanding)
  Purity: LOW (~0.35)
```

**Narrative Impact:**
- Creates urgency (transmission must happen now)
- Explains Agent_A's desperation (knows code is dying)
- Positions Agent_D as critical (last chance for transmission)
- Foreshadows future crisis (when technology fails)

---

### Arc 3: Network Contamination (C3 → C2)

**Structure:**
```
Stage 1: Clean Constraint
- C3 (Son's Duty) has high intrinsic purity
- Genuine reciprocity (family support ↔ filial obligation)
- Coordination value clear (economic security, social structure)
- Extraction balanced (both generations constrained)

Stage 2: Contamination Source
- C2 (Law of Luck) is degraded (Snare for powerless)
- Imposes costs on C3 (forces separation based on reputation)
- Creates workarounds (Agent_D lies about fishing with Agent_A)
- Normalizes extraction (parents use C2 to justify C3 enforcement)

Stage 3: Effective Purity Drop
- C3's intrinsic purity unchanged (family structure still functional)
- But effective purity drops (contaminated by C2's extraction)
- Agent_D experiences C3 as more extractive (separation is unjust)
- Parents experience C3 as more necessary (must protect son from "salao")

Stage 4: Internalization
- C3 starts absorbing C2's logic
- Duty becomes about economic calculation, not reciprocity
- Parents: "We must move him because old man is unlucky" (C2 logic)
- Not: "We must move him because family needs income" (C3 logic)
- Contamination complete: C3 now serves C2's extraction
```

**Contamination Mechanics:**
```
C3 intrinsic

<!-- ANONYMIZATION: 4 character names replaced: Santiago -> Agent_A, Younger Fishermen -> Agent_B, Manolin's Parents -> Agent_C, Manolin -> Agent_D -->
