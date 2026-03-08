# STAGE 1: FORMAL SPECIFICATION
## The Old Man and the Sea - Operational Constraint Model

---

## I. FORMALIZED CONSTRAINTS WITH INDEXED CLASSIFICATIONS

### C1: The Physical Contest (Natural Law)

**Base Properties:**
```
ε(C1) = 0.0
Supp(C1) = 0.0
Coord(C1) = false
Asym(C1) = true (affects biological entities differently by capability)
```

**Character: Santiago**
```
Index: I_Santiago = (moderate, immediate, trapped, universal)
  P = moderate (π = 1.0) — Has skill/experience but not institutional power
  T = immediate — Survival timeframe (days at sea)
  E = trapped — Cannot escape the physical contest once engaged
  S = universal (σ = 1.0) — Natural law applies equally

χ(C1, I_Santiago) = 0.0 × 1.0 × 1.0 = 0.0

Classification: Mountain (■)
Verification: ε = 0.0 < 0.15 (Mountain threshold)
              No enforcement needed (natural law)
              Immutable from biographical timeframe
```

**Character: The Marlin**
```
Index: I_Marlin = (powerful, immediate, trapped, universal)
  P = powerful (π = 0.6) — Immense physical strength
  T = immediate — Survival struggle
  E = trapped — Hook prevents escape
  S = universal (σ = 1.0) — Natural law

χ(C1, I_Marlin) = 0.0 × 0.6 × 1.0 = 0.0

Classification: Mountain (■)
Verification: Same natural law, same classification despite power difference
              True Mountain property: power-invariant
```

**Indexical Variance:** None (0.0)
- Both characters experience identical constraint type
- Natural law property: classification independent of power position
- This is the unchangeable terrain upon which social drama unfolds

---

### C2: The Label of Unluck (Social Stigma)

**Base Properties:**
```
ε(C2) = 0.70
Supp(C2) = 0.50
Coord(C2) = true (risk management function for community)
Asym(C2) = true (burden falls on labeled individual)
```

**Character: Santiago**
```
Index: I_Santiago = (powerless, immediate, trapped, local)
  P = powerless (π = 1.5) — Cannot contest community judgment
  T = immediate — Needs catch now to survive
  E = trapped — Cannot leave fishing community
  S = local (σ = 0.8) — Face-to-face community

χ(C2, I_Santiago) = 0.70 × 1.5 × 0.8 = 0.84

Classification: Snare (⊠)
Verification: χ = 0.84 > 0.66 (Snare threshold)
              High suppression (0.50) — social enforcement
              Cannot exit without catch (trapped)
              Extraction dominates despite coordination function
```

**Character: Manolin's Parents**
```
Index: I_Parents = (powerful, biographical, mobile, local)
  P = powerful (π = 0.6) — Control son's labor allocation
  T = biographical — Planning son's career/future
  E = mobile — Can choose any boat for son
  S = local (σ = 0.8) — Same community

χ(C2, I_Parents) = 0.70 × 0.6 × 0.8 = 0.336

Classification: Rope (⊞)
Verification: χ = 0.336 < 0.46 (Rope/Tangled boundary)
              Genuine coordination function (risk management)
              Low effective extraction from their position
              Rational labor allocation tool
```

**Indexical Variance:** High (0.504 difference in χ)
- Same social norm experienced as:
  - Oppressive trap (Santiago: Snare)
  - Rational coordination (Parents: Rope)
- Both classifications objectively true from respective indices
- Power differential (1.5 vs 0.6) creates 2.5× extraction multiplier
- **Dramatic engine:** Santiago cannot disprove unluck without catch, but needs catch to disprove unluck

---

### C3: A Boy's Duty (Filial Obligation)

**Base Properties:**
```
ε(C3) = 0.60
Supp(C3) = 0.40
Coord(C3) = true (family cohesion, intergenerational care)
Asym(C3) = true (burden on child, benefit to parents)
```

**Character: Manolin**
```
Index: I_Manolin = (powerless, biographical, constrained, local)
  P = powerless (π = 1.5) — Child under parental authority
  T = biographical — Lifetime relationship with father/Santiago
  E = constrained — Can resist but at high social cost
  S = local (σ = 0.8) — Family/village scale

χ(C3, I_Manolin) = 0.60 × 1.5 × 0.8 = 0.72

Classification: Tangled Rope (⊞⊠)
Verification: 0.46 < χ = 0.72 < 0.76 (Tangled Rope range)
              Genuine coordination (family bonds, reciprocity)
              Significant extraction (agency restriction, emotional pain)
              Both elements irreducible
              Moderate suppression (0.40) — social enforcement present
```

**Character: Santiago**
```
Index: I_Santiago_C3 = (powerless, biographical, trapped, local)
  P = powerless (π = 1.5) — Cannot contest parental authority
  T = biographical — Loss of companionship for remaining years
  E = trapped — No alternative to accept boy's absence
  S = local (σ = 0.8) — Same community

χ(C3, I_Santiago_C3) = 0.60 × 1.5 × 0.8 = 0.72

Classification: Snare (⊠)
Verification: χ = 0.72 > 0.66 (Snare threshold)
              For Santiago: no coordination benefit (only loss)
              Pure extraction (loses helper, companion)
              Cannot exit (E = trapped)
              
Note: Same χ value, different classification
Reason: Manolin experiences BOTH coordination (family duty) AND extraction (torn loyalty)
        Santiago experiences ONLY extraction (social rule that isolates him)
        Classification depends on whether coordination function is present FOR THAT CHARACTER
```

**Indexical Variance:** Moderate (same χ, different experience)
- Manolin: Tangled Rope (painful but legitimate conflict)
- Santiago: Snare (pure trap enforcing isolation)
- **Dramatic function:** Irresolvable tension between love and social structure
- Boy accepts constraint as normal; old man only feels its cost

---

## II. TRANSFORMATION RULES (Index-Sensitive)

### TR1: Catch Breaks Unluck Label

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>Santiago.action = "return_with_marlin"</condition>
    <condition>marlin.state = "intact" OR "recognizable"</condition>
  </trigger>
  
  <index_change>
    <character>Santiago</character>
    <constraint>C2</constraint>
    <from>
      <index>(powerless, immediate, trapped, local)</index>
      <chi>0.84</chi>
      <type>Snare</type>
    </from>
    <to>
      <index>(moderate, biographical, constrained, local)</index>
      <chi>0.56</chi>
      <type>Tangled Rope</type>
    </to>
  </index_change>
  
  <mechanism>
    <power_shift>powerless → moderate (π: 1.5 → 1.0)</power_shift>
    <time_shift>immediate → biographical (planning horizon extends)</time_shift>
    <exit_shift>trapped → constrained (can now choose boats)</exit_shift>
    <reason>Proof of skill restores agency and social standing</reason>
  </mechanism>
  
  <consequence>
    <chi_recalculation>
      χ_new = 0.70 × 1.0 × 0.8 = 0.56
    </chi_recalculation>
    <type_change>Snare → Tangled Rope</type_change>
    <narrative_effect>
      Santiago regains social standing but stigma memory persists.
      Community coordination function now visible to him (not pure trap).
    </narrative_effect>
  </consequence>
  
  <blocked_by>
    <condition>marlin.state = "skeleton"</condition>
    <result>Transformation fails — no proof, label persists</result>
    <chi_remains>0.84 (Snare)</chi_remains>
  </blocked_by>
</transformation_rule>
```

### TR2: Collective Action (Counterfactual)

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>fishermen.action = "organize_mutual_aid"</condition>
    <condition>fishermen.count ≥ threshold_collective</condition>
  </trigger>
  
  <index_change>
    <character>Santiago (and peers)</character>
    <constraint>C2</constraint>
    <from>
      <index>(powerless, immediate, trapped, local)</index>
      <chi>0.84</chi>
      <type>Snare</type>
    </from>
    <to>
      <index>(organized, biographical, constrained, local)</index>
      <chi>0.224</chi>
      <type>Rope</type>
    </to>
  </index_change>
  
  <mechanism>
    <power_shift>powerless → organized (π: 1.5 → 0.4)</power_shift>
    <time_shift>immediate → biographical (collective planning)</time_shift>
    <exit_shift>trapped → constrained (mutual support creates options)</exit_shift>
    <reason>Shared burden, collective bargaining, mutual insurance</reason>
  </mechanism>
  
  <consequence>
    <chi_recalculation>
      χ_new = 0.70 × 0.4 × 0.8 = 0.224
    </chi_recalculation>
    <type_change>Snare → Rope</type_change>
    <narrative_effect>
      Risk-sharing transforms stigma into manageable coordination.
      "Unluck" becomes statistical variance, not personal failure.
    </narrative_effect>
  </consequence>
  
  <not_taken>
    <reason>Santiago's pride, individualist culture, isolation</reason>
    <result>Remains in Snare (χ = 0.84)</result>
  </not_taken>
</transformation_rule>
```

### TR3: Boy's Duty Intensifies (Degradation)

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>Santiago.state = "injured" OR "failing"</condition>
    <condition>time_passes AND no_catch</condition>
  </trigger>
  
  <index_change>
    <character>Manolin</character>
    <constraint>C3</constraint>
    <from>
      <index>(powerless, biographical, constrained, local)</index>
      <chi>0.72</chi>
      <type>Tangled Rope</type>
    </from>
    <to>
      <index>(powerless, immediate, trapped, local)</index>
      <chi>0.72</chi>
      <type>Snare</type>
    </to>
  </index_change>
  
  <mechanism>
    <time_shift>biographical → immediate (crisis urgency)</time_shift>
    <exit_shift>constrained → trapped (moral obligation intensifies)</exit_shift>
    <extraction_increase>Coordination value diminishes as Santiago weakens</extraction_increase>
    <reason>Duty becomes pure burden when reciprocity impossible</reason>
  </mechanism>
  
  <consequence>
    <chi_unchanged>0.72 (same value)</chi_unchanged>
    <type_change>Tangled Rope → Snare</type_change>
    <narrative_effect>
      Boy's conflict resolves into pure obligation.
      Love remains but coordination function (learning, partnership) lost.
      Becomes caretaking burden without reciprocal benefit.
    </narrative_effect>
  </consequence>
  
  <trajectory>
    <stage_1>Tangled Rope (balanced conflict)</stage_1>
    <stage_2>Tangled Rope (heavy) (extraction increases)</stage_2>
    <stage_3>Snare (coordination lost, pure duty)</stage_3>
    <mechanism>Natural degradation: Rope → Tangled → Snare</mechanism>
  </trajectory>
</transformation_rule>
```

### TR4: Physical Contest Escalation (Mountain Remains)

```xml
<transformation_rule id="TR4">
  <trigger>
    <condition>Santiago.action = "fight_marlin"</condition>
    <condition>duration > threshold_endurance</condition>
  </trigger>
  
  <index_change>
    <character>Santiago</character>
    <constraint>C1</constraint>
    <from>
      <index>(moderate, immediate, trapped, universal)</index>
      <chi>0.0</chi>
      <type>Mountain</type>
    </from>
    <to>
      <index>(moderate, immediate, trapped, universal)</index>
      <chi>0.0</chi>
      <type>Mountain</type>
    </to>
  </index_change>
  
  <mechanism>
    <no_change>Natural law unchanged by effort</no_change>
    <cost_increase>Physical toll increases (hands, back, exhaustion)</cost_increase>
    <reason>Cannot transform Mountain — only navigate it</reason>
  </mechanism>
  
  <consequence>
    <chi_unchanged>0.0</chi_unchanged>
    <type_unchanged>Mountain</type_unchanged>
    <narrative_effect>
      Heroic effort does not change physical reality.
      Santiago's skill allows navigation, not transformation.
      Dignity comes from acceptance, not victory over nature.
    </narrative_effect>
  </consequence>
  
  <critical_insight>
    Mountains do not transform through character action.
    They provide unchangeable terrain for social drama.
    Santiago's struggle is WITH the Mountain, not AGAINST it.
  </critical_insight>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS (Observable Actions)

### E1: Type I Error — False Mountain (Santiago)

**Pattern:** Treating C2 (social stigma) as unchangeable natural law

**Observable Manifestations:**
```
1. Verbal acceptance:
   - "I am a strange old man" (naturalizes social judgment)
   - "Perhaps I should not have been a fisherman" (internalizes label)
   
2. Behavioral resignation:
   - Fishes alone despite danger
   - Does not contest parents' decision about Manolin
   - Accepts isolation as inevitable
   
3. Cognitive framing:
   - Treats 84 days without fish as personal failure, not variance
   - Sees unluck as property of self, not social construction
   
4. Missed agency:
   - Does not organize with other fishermen
   - Does not challenge community judgment directly
   - Routes around constraint instead of transforming it
```

**Index Explanation:**
```
From I_Santiago = (powerless, immediate, trapped, local):
  - Powerless: Cannot see path to contesting judgment
  - Immediate: Survival pressure prevents long-term organizing
  - Trapped: No exit from fishing community
  - Local: Face-to-face enforcement makes stigma feel natural

Objectively appears as Mountain from this index.
Error is tragic, not irrational.
```

**Consequence:**
- Wasted agency (could organize, doesn't)
- Unnecessary suffering (accepts isolation)
- Heroic effort directed at symptom (catching fish) not cause (social structure)

---

### E2: Type III Error — Snare-as-Rope (Manolin's Parents)

**Pattern:** Missing extraction in C2, treating stigma as pure coordination

**Observable Manifestations:**
```
1. Verbal framing:
   - "The boy must fish with a lucky boat" (coordination language)
   - "It is for his own good" (genuine belief in benefit)
   
2. Behavioral confidence:
   - No hesitation in removing boy from Santiago
   - No acknowledgment of harm to Santiago
   - Treat decision as obvious, natural
   
3. Cognitive blindness:
   - Do not see Santiago's isolation as cost
   - Do not see stigma as extractive mechanism
   - Focus only on risk management function
   
4. Structural position enables error:
   - Powerful (π = 0.6) — don't feel extraction
   - Mobile — have alternatives, don't see trap
   - Biographical horizon — can afford to wait for results
```

**Index Explanation:**
```
From I_Parents = (powerful, biographical, mobile, local):
  χ = 0.336 (Rope range)
  
Genuinely experience C2 as coordination mechanism.
Not lying or rationalizing — structurally cannot see extraction.
Their classification (Rope) is objectively true from their index.
```

**Consequence:**
- Voluntary participation in Santiago's oppression
- Sincere belief they are acting rationally
- No malice, but structural harm nonetheless

**Dramatic Function:**
- Shows how same system can be Rope for powerful, Snare for powerless
- Both sides correct from their indices
- Conflict emerges from structure, not character flaw

---

### E3: Type V.a Error — Tangled-as-Rope (Manolin, Early)

**Pattern:** Ignoring extraction in C3, treating duty as pure coordination

**Observable Manifestations:**
```
1. Verbal minimization:
   - "I can come back" (downplays permanence)
   - "My father will understand" (optimistic about negotiation)
   
2. Behavioral compliance:
   - Obeys father without sustained resistance
   - Accepts separation as temporary
   - Does not recognize structural trap
   
3. Cognitive framing:
   - Sees duty as reciprocal (will care for father when old)
   - Focuses on coordination benefits (family harmony)
   - Minimizes extraction (emotional pain, lost mentorship)
   
4. Developmental stage:
   - Youth prevents seeing biographical timeframe clearly
   - Has not yet experienced irreversible loss
   - Trusts that love will find a way
```

**Index Explanation:**
```
From I_Manolin = (powerless, biographical, constrained, local):
  χ = 0.72 (Tangled Rope)
  
Should recognize BOTH coordination AND extraction.
Instead, focuses on coordination, minimizes extraction.
Age and hope create cognitive bias toward Rope classification.
```

**Consequence:**
- Underestimates cost of separation
- Delayed recognition of irreversibility
- When extraction becomes undeniable (Santiago injured/failing), sudden shift to Snare experience

**Transformation:**
```
Early: Tangled-as-Rope (Type V.a error)
  ↓
Crisis: Tangled Rope (correct recognition)
  ↓
Late: Tangled-as-Snare (Type V.b error, overcorrection)
```

---

### E4: Type II Error — Mountain Denial (Counterfactual)

**Pattern:** Treating C1 (physical contest) as changeable through will

**Observable Manifestations (if present):**
```
1. Verbal defiance:
   - "I will defeat the sea" (not "navigate" or "work with")
   - "Nature will bend to my will"
   
2. Behavioral overextension:
   - Ignoring physical limits (hands, back, age)
   - Refusing to accept when contest is lost
   - Fighting entropy instead of routing around it
   
3. Cognitive framing:
   - Sees natural law as opponent to be conquered
   - Treats biological limits as moral failures
   - Believes sufficient effort can change physics
   
4. Energy depletion:
   - Exhaustion from fighting unchangeable
   - Burnout from refusing to accept limits
   - Collapse when reality asserts itself
```

**Why Santiago Avoids This Error:**
```
Santiago DOES NOT make Type II error with C1.
He accepts the Mountain:
  - "I wish I was the fish" (identification, not conquest)
  - Works WITH the marlin's strength, not against it
  - Respects physical limits even while pushing them
  
This is why his struggle has dignity.
He navigates the Mountain; he does not deny it.
```

**Contrast:**
```
Type I error (False Mountain): Treats changeable as unchangeable
  → Santiago with C2 (social stigma)
  
Type II error (Mountain Denial): Treats unchangeable as changeable
  → Santiago does NOT make this error with C1 (physical contest)
  
This asymmetry is structurally important:
  - Accepts natural law (wisdom)
  - Misclassifies social construction (tragedy)
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**
```
The Old Man and the Sea depicts human institutions (fishing community, family structure)
operating under realistic constraints:

1. Uncertainty: Cannot predict fish behavior, weather, individual luck
2. Information asymmetry: Parents don't see Santiago's skill, only results
3. Risk aversion: Community protects against variance through stigma
4. Satisficing: "Good enough" solutions (remove boy from unlucky boat)
5. Principal-agent problems: Parents optimize for family, not Santiago

NOT Perfect Institutional Rationality (PIR):
  - No implacable optimization
  - No perfect information
  - Negotiation possible (boy sneaks back, brings food)
  - Human institutions, not algorithmic governance
```

**Implications for Attractor Space:**

```
BIR enables:
  ✓ Negotiated Equilibrium (possible)
  ✓ Seeded Possibility (possible)
  ✗ Deterministic Tragedy (requires PIR)
  ? Revolutionary Rupture (requires external shock)

Under BIR:
  - Constraints can be negotiated (boy brings food despite prohibition)
  - Satisficing creates slack (parents don't enforce perfectly)
  - Risk aversion can be managed (mutual aid could reduce stigma)
  - Information can change minds (proof of skill could restore standing)
```

**Behavioral Predictions:**

```
1. Stigma enforcement is imperfect:
   - Boy can help Santiago covertly
   - Community members show sympathy
   - Not totalitarian control

2. Parental authority is negotiable:
   - Boy can resist at margins
   - Love creates exceptions
   - Not absolute obedience

3. Social norms can shift:
   - Proof of skill could restore Santiago
   - Community could organize mutual aid
   - Not locked into deterministic tragedy

4. Satisficing creates hope:
   - "Good enough" catch might suffice
   - Partial proof might restore some standing
   - Not all-or-nothing
```

**Critical Constraint:**
```
BIR does NOT guarantee happy ending.
It means:
  - Tragedy is not inevitable (negotiation possible)
  - But success is not guaranteed (uncertainty remains)
  - Outcomes depend on actions + luck + structural position
  
Santiago's tragedy is:
  - Not deterministic (could have organized, could have caught fish intact)
  - But highly probable (structural position + isolation + age)
  - Bounded rationality creates possibility, not certainty
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected: Seeded Possibility

**Formal Specification:**
```
Terminal State: Seeded Possibility
  Surface: Deterministic Tragedy (Santiago dies, stigma persists)
  Underground: Transformation seeded (Manolin inherits wisdom, will teach others)
  
Compatibility:
  ✓ BIR (bounded rationality allows underground transformation)
  ✓ Dominant constraint types (Snare + Tangled Rope allow hidden resistance)
  ✓ Indexical variance (different characters see different futures)
```

**Structural Justification:**

```
1. Surface Tragedy (Visible):
   - Santiago returns with skeleton (no proof)
   - Stigma persists (C2 remains Snare)
   - Physical decline (C1 Mountain takes toll)
   - Isolation continues (C3 enforces separation)
   - Death implied (exhaustion, age, injury)

2. Underground Transformation (Hidden):
   - Manolin witnesses dignity in defeat
   - Learns: worth ≠ market value
   - Inherits: skill, wisdom, relationship to sea
   - Will teach: next generation, different values
   - Seeds: alternative to stigma-based coordination

3. Indexical Divergence:
   - Santiago (immediate, trapped): Experiences pure tragedy
   - Manolin (biographical, constrained): Sees tragedy + possibility
   - Community (powerful, mobile): Sees confirmation of stigma
   - Reader (analytical, civilizational): Sees critique of system
```

**Why Not Other Attractors:**

```
❌ Deterministic Tragedy:
   - Requires PIR (implacable optimization)
   - But we have BIR (negotiation possible)
   - Too absolute for human institutions
   - Misses underground transformation

❌ Negotiated Equilibrium:
   - Requires successful bargaining
   - But Santiago lacks power to negotiate
   - Stigma too entrenched
   - Would require collective action (not taken)

❌ Revolutionary Rupture:
   - Requires constraint logic itself disrupted
   - But no external shock present
   - No mass uprising
   - System continues functioning
```

**Attractor Mechanics:**

```
Phase 1: Constraint Tightening
  - C2 (stigma) intensifies with each failed day
  - C3 (duty) separates boy from old man
  - C1 (physical contest) takes increasing toll
  - χ values rise for powerless characters

Phase 2: Heroic Effort (Surface)
  - Santiago catches marlin (temporary hope)
  - Sharks attack (hope destroyed)
  - Returns with skeleton (no proof)
  - Stigma confirmed (surface tragedy)

Phase 3: Underground Seeding
  - Manolin sees skeleton, understands scale
  - Witnesses dignity despite defeat
  - Learns alternative value system
  - Commits to care for Santiago
  - Will teach others (future transformation)

Phase 4: Terminal State
  - Surface: Tragedy (Santiago broken, stigma persists)
  - Underground: Possibility (Manolin transformed, seeds planted)
  - Indexical split: Different characters see different endings
```

**Compatibility with Constraint Types:**

```
C1 (Mountain): Unchangeable
  → Surface: Physical limits cannot be overcome
  → Underground: Wisdom comes from acceptance, not conquest

C2 (Snare for Santiago, Rope for Parents): Indexical
  → Surface: Stigma persists (parents' view confirmed)
  → Underground: Manolin sees extraction (will resist in future)

C3 (Tangled Rope → Snare): Degrading
  → Surface: Duty enforces separation (tragedy)
  → Underground: Love persists despite structure (seeds resistance)

All three constraints compatible with Seeded Possibility:
  - Surface tragedy driven by Snare + Mountain
  - Underground transformation enabled by Tangled Rope recognition
  - Indexical variance allows simultaneous truth of both
```

**Narrative Function:**

```
Seeded Possibility resolves the paradox:
  - Hemingway's pessimism (surface tragedy)
  - Hemingway's humanism (underground dignity)
  - Both true simultaneously

Reader experiences:
  - Immediate: Tragedy (Santiago's defeat)
  - Biographical: Possibility (Manolin's transformation)
  - Civilizational: Critique (system that wastes wisdom)

This is why the ending resonates:
  - Not pure tragedy (too nihilistic)
  - Not happy ending (too false)
  - Tragedy + seeded transformation (complex truth)
```

---

## VI. STRUCTURAL PHYSICS ARCS (Optional)

### Arc 1: False Mountain Reveal (Partial)

**Structure:**
```
Stage 1: Naturalization
  - Santiago treats stigma as natural law
  - "I am unlucky" (internalizes social judgment)
  - Community treats it as obvious truth
  
Stage 2: Evidence Accumulation
  - Boy's love persists despite stigma
  - Santiago's skill evident in marlin catch
  - Skeleton proves scale of achievement
  
Stage 3: Partial Reveal
  - Manolin sees: stigma ≠ natural law
  - Recognizes: social construction, not physics
  - But: Community does not see (reveal incomplete)
  
Stage 4: Indexical Split
  - Manolin: Reclassifies C2 as Snare (sees extraction)
  - Community: Maintains C2 as Rope (sees coordination)
  - Santiago: Dies believing it was Mountain (tragic)
```

**Narrative Function:**
- Reader sees False Mountain before characters do
- Manolin's awakening is the underground transformation
- Community's blindness is the surface tragedy
- Incomplete reveal maintains tension

---

### Arc 2: Purity Drift (C3: Boy's Duty)

**Structure:**
```
Stage 1: High Purity (Early)
  - Genuine reciprocity (Santiago teaches, boy learns)
  - Mutual benefit (companionship, skill transfer)
  - Coordination dominates extraction
  - Purity ≈ 0.75 (healthy Tangled Rope)

Stage 2: Drift Begins (Separation)
  - Coordination function weakens (no more fishing together)
  - Extraction persists (duty remains, benefit declines)
  - Boy brings food (theater increases, substance decreases)
  - Purity ≈ 0.55 (degrading)

Stage 3: Crisis (Santiago Injured)
  - Coordination function nearly gone (cannot teach, cannot fish)
  - Extraction dominates (pure caretaking burden)
  - Duty becomes obligation without reciprocity
  - Purity ≈ 0.30 (approaching Snare)

Stage 4: Terminal State
  - If Santiago dies: Duty ends, memory remains
  - If Santiago lives: Becomes pure caretaking (Snare)
  - Purity drift complete: Tangled Rope → Snare
  - Natural entropy: coordination → extraction
```

**Metrics:**
```
Purity(C3, t) = Coordination_value(t) / (Coordination_value(t) + Extraction_cost(t))

t=0 (Early): P = 0.75 (high coordination, moderate extraction)
t=1 (Separation): P = 0.55 (coordination declining)
t=2 (Crisis): P = 0.30 (extraction dominates)
t=3 (Terminal): P → 0.15 (approaching pure Snare)

Threshold: P < 0.40 → Tangled Rope becomes Snare
```

**Narrative Function:**
- Shows natural degradation (entropy principle)
- Boy's love persists but structure decays
- Explains why duty becomes burden
- Tragic inevitability without villain

---

### Arc 3: Network Contamination (Counterfactual)

**Structure:**
```
Hypothetical: Santiago organizes mutual aid (TR2)

Stage 1: Focal System (Mutual Aid)
  - Fishermen pool resources
  - Share risk, share reward
  - High intrinsic purity (genuine coordination)
  - χ = 0.224 (Rope)

Stage 2: Contamination from C2 (Stigma)
  - Community stigmatizes "socialist" fishermen
  - Buyers pay less for "collective" fish
  - Enforcement costs rise (social pressure)
  - Effective purity drops despite intrinsic health

Stage 3: Internalization
  - Mutual aid group starts excluding "unlucky" members
  - Reproduces stigma internally to reduce external pressure
  - Intrinsic purity now falling
  - Contamination complete

Stage 4: Collapse
  - Mutual aid becomes another hierarchy
  - Original coordination function lost
  - Network contamination successful
  - C2 (Snare) has infected C_mutual_aid (Rope)
```

**Why Not Taken:**
- Santiago's pride (individualist)
- Community's stigma (prevents organizing)
- Isolation (no critical mass)
- Age (biographical horizon too short)

**Narrative Function:**
- Shows why isolated reform fails
- Explains structural persistence of stigma
- Demonstrates network effects
- Justifies tragic outcome (not just individual failure)

---

## VII. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized
  - C1: The Physical Contest 