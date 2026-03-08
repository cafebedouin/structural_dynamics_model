# STAGE 1: FORMAL SPECIFICATION
## The Old Man and the Sea - Operational Constraint Model

---

## I. CONSTRAINT FORMALIZATIONS

### C1: The Body's Limits

**Base Properties:**
```
ε(C1) = 0.0
Supp(C1) = 0.0
Coord(C1) = false
Asym(C1) = false
```

**Indexed Classification:**

```xml
<character name="Santiago">
  <index>
    <power>powerless</power>        <!-- π = 1.5 -->
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>local</scope>            <!-- σ = 0.8 -->
  </index>
  
  <chi_calculation>
    χ(C1, Santiago) = ε × π(P) × σ(S)
                    = 0.0 × 1.5 × 0.8
                    = 0.0
  </chi_calculation>
  
  <classification>
    <type>Mountain (■)</type>
    <rationale>
      Zero extraction (natural law).
      No enforcement needed (biological inevitability).
      Immutable from biographical time horizon.
      Power-invariant (affects all equally).
    </rationale>
  </classification>
  
  <experience>
    Physical aging: cramping hands, weakening back, finite endurance.
    Cannot be negotiated with or escaped.
    Must be routed around through skill, tricks, resolution.
  </experience>
</character>
```

**Structural Certification:**
```
Boltzmann Test: PASS
  - Classification invariant across power positions
  - No scope-dependence (biological law)
  - No enforcement mechanism required
  → Genuine Mountain, not physics-washing
```

---

### C2: The Tally of Luck

**Base Properties:**
```
ε(C2) = 0.70
Supp(C2) = 0.40
Coord(C2) = true (resource allocation mechanism)
Asym(C2) = true (burden falls on "unlucky")
```

**Indexed Classifications:**

```xml
<character name="Santiago">
  <index>
    <power>powerless</power>        <!-- π = 1.5 -->
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>local</scope>            <!-- σ = 0.8 -->
  </index>
  
  <chi_calculation>
    χ(C2, Santiago) = 0.70 × 1.5 × 0.8
                    = 0.84
  </chi_calculation>
  
  <classification>
    <type>Snare (⊠)</type>
    <threshold_check>
      χ = 0.84 > 0.70 (Snare threshold)
      High extraction dominates minimal coordination value
    </threshold_check>
    <rationale>
      High power-scaled extraction (χ = 0.84).
      Requires social enforcement (Supp = 0.40).
      No exit (trapped in community, no alternative livelihood).
      Asymmetric burden (labeled "salao" strips him of resources).
    </rationale>
  </classification>
  
  <experience>
    Social isolation: loses apprentice, community standing.
    Label becomes self-fulfilling (no crew → no catch → more "unlucky").
    Trapped by belief system he cannot escape or disprove.
  </experience>
</character>

<character name="Manolin">
  <index>
    <power>powerless</power>        <!-- π = 1.5 -->
    <time>biographical</time>
    <exit>constrained</exit>        <!-- slightly better than trapped -->
    <scope>local</scope>            <!-- σ = 0.8 -->
  </index>
  
  <chi_calculation>
    χ(C2, Manolin) = 0.70 × 1.5 × 0.8
                   = 0.84
  </chi_calculation>
  
  <classification>
    <type>Snare (⊠)</type>
    <threshold_check>
      χ = 0.84 > 0.70 (Snare threshold)
    </threshold_check>
    <rationale>
      Same high extraction as Santiago.
      Forced to abandon relationship he values.
      Exit slightly less constrained (could theoretically refuse parents).
      But social/economic pressure makes exit prohibitively costly.
    </rationale>
  </classification>
  
  <experience>
    Emotional extraction: guilt, sadness, divided loyalty.
    Forced choice between love (Santiago) and survival (lucky boat).
    Belief system extracts his agency over his own relationships.
  </experience>
</character>

<character name="Boy's Parents">
  <index>
    <power>moderate</power>         <!-- π = 1.0 -->
    <time>generational</time>       <!-- planning for son's future -->
    <exit>mobile</exit>             <!-- can choose boats -->
    <scope>local</scope>            <!-- σ = 0.8 -->
  </index>
  
  <chi_calculation>
    χ(C2, Parents) = 0.70 × 1.0 × 0.8
                   = 0.56
  </chi_calculation>
  
  <classification>
    <type>Tangled Rope (⊞⊠)</type>
    <threshold_check>
      0.46 < χ = 0.56 < 0.70
      Falls in Tangled Rope range
    </threshold_check>
    <rationale>
      Moderate extraction (χ = 0.56).
      Genuine coordination value: practical resource allocation.
      Asymmetric burden: they impose cost on Santiago/Manolin.
      But from their index: functional tool for son's economic security.
    </rationale>
  </classification>
  
  <experience>
    Practical decision-making: use community belief to guide son to productive boat.
    Coordination benefit: ensures son earns income.
    Extraction cost: emotional harm to son, abandonment of old man.
    Navigate hybrid: accept extraction as price of coordination.
  </experience>
</character>
```

**Indexical Variance Summary:**
```
Same constraint (C2: Tally of Luck):
  Santiago:  χ = 0.84 → Snare (oppressive trap)
  Manolin:   χ = 0.84 → Snare (forced abandonment)
  Parents:   χ = 0.56 → Tangled Rope (functional but costly tool)

All three classifications objectively true from their indices.
Variance driven by: Power (powerless vs moderate) and Exit (trapped vs mobile).
```

**Structural Certification:**
```
Boltzmann Test: FAIL (correctly)
  - Classification varies by power position
  - Requires social enforcement (Supp = 0.40)
  - Asymmetric burden distribution
  → Not a natural law. Constructed social constraint.
  → Physics-washing if presented as inevitable.
```

---

### C3: The Price of the Catch

**Base Properties:**
```
ε(C3) = 0.80
Supp(C3) = 0.20
Coord(C3) = true (market pricing mechanism)
Asym(C3) = true (risk borne by producer)
```

**Indexed Classifications:**

```xml
<character name="Santiago">
  <index>
    <power>powerless</power>        <!-- π = 1.5 -->
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>local</scope>            <!-- σ = 0.8 -->
  </index>
  
  <chi_calculation>
    χ(C3, Santiago) = 0.80 × 1.5 × 0.8
                    = 0.96
  </chi_calculation>
  
  <classification>
    <type>Snare (⊠)</type>
    <threshold_check>
      χ = 0.96 > 0.70 (well above Snare threshold)
      Extreme extraction
    </threshold_check>
    <rationale>
      Very high power-scaled extraction (χ = 0.96).
      All risk borne by producer (asymmetric).
      No exit (must sell to market or starve).
      Minimal coordination value from his position.
      Low suppression (market "naturally" enforces through necessity).
    </rationale>
  </classification>
  
  <experience>
    Total risk exposure: heroic labor worthless if fish destroyed.
    No insurance, no safety net, no alternative buyers.
    Market indifferent to effort, suffering, skill.
    Value contingent on delivery of intact product.
    Sharks destroy fish → labor value = 0.
  </experience>
</character>

<character name="Havana Market (institutional)">
  <index>
    <power>institutional</power>    <!-- π = -0.2 (net beneficiary) -->
    <time>generational</time>
    <exit>analytical</exit>         <!-- can observe without participating -->
    <scope>regional</scope>         <!-- σ = 0.9 -->
  </index>
  
  <chi_calculation>
    χ(C3, Market) = 0.80 × -0.2 × 0.9
                  = -0.144
  </chi_calculation>
  
  <classification>
    <type>Rope (⊞)</type>
    <threshold_check>
      χ = -0.144 < 0.46 (negative = net benefit)
      Coordination mechanism from institutional view
    </threshold_check>
    <rationale>
      Negative χ (net beneficiary position).
      Provides genuine coordination: price discovery, distribution.
      Extraction invisible from this index (borne by producers).
      Low enforcement cost (necessity does the work).
    </rationale>
  </classification>
  
  <experience>
    Efficient pricing mechanism.
    Quality control (spoiled fish have no value).
    Resource allocation across buyers.
    Risk externalized to producers (not visible as extraction).
  </experience>
</character>
```

**Indexical Variance Summary:**
```
Same constraint (C3: Price of the Catch):
  Santiago: χ = 0.96 → Snare (brutal trap, total risk exposure)
  Market:   χ = -0.144 → Rope (efficient coordination mechanism)

Extreme indexical divergence.
Market sees coordination, Santiago experiences extraction.
Both objectively correct from their structural positions.
```

**Structural Certification:**
```
Boltzmann Test: FAIL (correctly)
  - Classification varies dramatically by power position
  - Asymmetric risk distribution (producer bears all)
  - Requires economic necessity as enforcement
  → Not a natural law. Constructed market constraint.
  → Appears as "natural" market forces but is institutional design.
```

---

## II. TRANSFORMATION RULES

### TR1: Collective Organization (C2 Transformation)

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>character_action = "organize_collective"</condition>
    <condition>target_constraint = C2</condition>
    <condition>initial_power = "powerless"</condition>
  </trigger>
  
  <index_change>
    <from>
      <power>powerless</power>      <!-- π = 1.5 -->
      <exit>trapped</exit>
    </from>
    <to>
      <power>organized</power>      <!-- π = 0.4 -->
      <exit>constrained</exit>
    </to>
  </index_change>
  
  <chi_recalculation>
    <before>
      χ(C2, powerless) = 0.70 × 1.5 × 0.8 = 0.84 (Snare)
    </before>
    <after>
      χ(C2, organized) = 0.70 × 0.4 × 0.8 = 0.224 (Rope)
    </after>
  </chi_recalculation>
  
  <type_change>
    <from>Snare (⊠)</from>
    <to>Rope (⊞)</to>
  </type_change>
  
  <consequence>
    <narrative>
      Fishermen collectively reject "luck" as resource allocation.
      Establish mutual aid, shared equipment, collective bargaining.
      Belief system loses enforcement power.
      Santiago regains apprentice, community standing.
    </narrative>
    <structural>
      Coordination value preserved (resource allocation still needed).
      Extraction eliminated (burden shared, not concentrated).
      Suppression drops (no longer need to enforce belief).
    </structural>
  </consequence>
  
  <preconditions>
    <required>Multiple fishermen experience C2 as Snare</required>
    <required>Communication possible (local scope enables coordination)</required>
    <required>Sufficient desperation to overcome collective action problem</required>
  </preconditions>
  
  <blocked_by>
    <constraint>C1 (Body's Limits) - still must fish despite organization</constraint>
    <constraint>C3 (Price of Catch) - market still extracts from collective</constraint>
  </blocked_by>
</transformation_rule>
```

### TR2: Market Reform (C3 Transformation)

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>institutional_action = "establish_cooperative"</condition>
    <condition>target_constraint = C3</condition>
  </trigger>
  
  <constraint_modification>
    <parameter>ε(C3)</parameter>
    <from>0.80</from>
    <to>0.50</to>
    <mechanism>
      Fishing cooperative: shared storage, collective bargaining, risk pooling.
      Reduces asymmetric risk burden on individual producers.
    </mechanism>
  </constraint_modification>
  
  <chi_recalculation>
    <before>
      χ(C3, powerless) = 0.80 × 1.5 × 0.8 = 0.96 (Snare)
    </before>
    <after>
      χ(C3, organized) = 0.50 × 0.4 × 0.8 = 0.16 (Rope)
    </after>
  </chi_recalculation>
  
  <type_change>
    <from>Snare (⊠)</from>
    <to>Rope (⊞)</to>
  </type_change>
  
  <consequence>
    <narrative>
      Fishermen share risk of spoilage, shark attacks.
      Collective negotiates better prices with buyers.
      Individual catastrophic loss becomes shared manageable loss.
      Santiago's heroic catch would have value even if partially damaged.
    </narrative>
    <structural>
      Base extraction reduced (ε: 0.80 → 0.50).
      Power position improved (powerless → organized).
      Coordination value increased (risk pooling, collective bargaining).
    </structural>
  </consequence>
  
  <preconditions>
    <required>TR1 completed (collective organization exists)</required>
    <required>Sufficient capital for shared infrastructure</required>
    <required>Trust among fishermen (local scope enables)</required>
  </preconditions>
  
  <blocked_by>
    <constraint>C1 (Body's Limits) - still must physically fish</constraint>
    <external>Market power of buyers (may resist collective bargaining)</external>
  </blocked_by>
</transformation_rule>
```

### TR3: Purity Drift (C2 Degradation)

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>time_elapsed = "generational"</condition>
    <condition>target_constraint = C2</condition>
    <condition>no_active_maintenance</condition>
  </trigger>
  
  <degradation_mechanism>
    <type>Purity Drift</type>
    <description>
      "Luck" belief initially had genuine coordination value:
      - Heuristic for identifying skilled vs unskilled fishermen
      - Social mechanism for resource allocation
      
      Over time, coordination function hollows out:
      - Becomes post-hoc rationalization for random outcomes
      - Used to justify excluding disfavored individuals
      - Theater ratio increases (ritual > substance)
    </description>
  </degradation_mechanism>
  
  <parameter_changes>
    <coordination_value>
      <from>genuine (practical heuristic)</from>
      <to>theater (empty ritual)</to>
    </coordination_value>
    <extraction>
      <from>0.70 (moderate)</from>
      <to>0.85 (high)</to>
      <mechanism>Extraction increases as coordination value drops</mechanism>
    </extraction>
  </parameter_changes>
  
  <chi_recalculation>
    <before>
      χ(C2, powerless) = 0.70 × 1.5 × 0.8 = 0.84 (Snare)
    </before>
    <after>
      χ(C2, powerless) = 0.85 × 1.5 × 0.8 = 1.02 (deeper Snare)
    </after>
  </chi_recalculation>
  
  <consequence>
    <narrative>
      Belief system becomes more oppressive over time.
      Santiago's isolation deepens as "salao" label hardens.
      Community less willing to question belief (sunk cost in ritual).
      Younger generation may reject entirely (revolutionary rupture).
    </narrative>
    <structural>
      Snare tightens (higher χ).
      Suppression may increase (more enforcement needed as legitimacy drops).
      Vulnerable to sudden collapse (low purity = brittle).
    </structural>
  </consequence>
  
  <reversal_difficulty>
    <energy_required>HIGH</energy_required>
    <rationale>
      Must rebuild coordination function while dismantling extraction.
      Requires collective recognition that belief is degraded.
      Entropy fights reversal (natural drift is toward extraction).
    </rationale>
  </reversal_difficulty>
</transformation_rule>
```

### TR4: False Mountain Reveal (C2 Reclassification)

```xml
<transformation_rule id="TR4">
  <trigger>
    <condition>character_gains_analytical_index</condition>
    <condition>target_constraint = C2</condition>
  </trigger>
  
  <index_change>
    <from>
      <power>powerless</power>
      <time>biographical</time>
      <exit>trapped</exit>
    </from>
    <to>
      <power>analytical</power>      <!-- π = 1.15 -->
      <time>generational</time>
      <exit>analytical</exit>
    </to>
  </index_change>
  
  <reclassification>
    <initial_belief>
      "Luck is natural law (Mountain). Cannot be changed, only endured."
    </initial_belief>
    <revelation>
      "Luck is social construct (Snare). Requires enforcement, varies by power."
    </revelation>
    <evidence>
      - Belief requires active social enforcement (parents forcing Manolin)
      - Burden falls asymmetrically (powerless labeled "unlucky")
      - Classification varies by power position (Snare vs Tangled Rope)
      - Fails Boltzmann test (not power-invariant)
    </evidence>
  </reclassification>
  
  <chi_recalculation>
    <analytical_index>
      χ(C2, analytical) = 0.70 × 1.15 × 0.8 = 0.644 (Tangled Rope)
    </analytical_index>
    <note>
      Analytical index sees both coordination (resource allocation) 
      and extraction (asymmetric burden). Recognizes hybrid nature.
    </note>
  </chi_recalculation>
  
  <consequence>
    <narrative>
      Character (Santiago or Manolin) realizes "salao" is not fate.
      Sees enforcement mechanisms (social pressure, economic necessity).
      Recognizes possibility of change (not Mountain, therefore changeable).
      May attempt TR1 (collective organization) or individual resistance.
    </narrative>
    <structural>
      Type I error corrected (False Mountain → Snare).
      Opens action space (if changeable, can be changed).
      May trigger conflict (others still believe it's natural).
    </structural>
  </consequence>
  
  <dramatic_function>
    <type>Revelation</type>
    <description>
      Classic "scales fall from eyes" moment.
      What seemed inevitable revealed as constructed.
      Empowering (can act) but also enraging (was preventable).
    </description>
  </dramatic_function>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### E1: Type I Error - False Mountain (Santiago)

```xml
<error_manifestation id="E1">
  <type>Type I: False Mountain</type>
  <character>Santiago</character>
  <constraint>C2 (The Tally of Luck)</constraint>
  
  <misclassification>
    <actual_type>Snare (⊠)</actual_type>
    <perceived_type>Mountain (■)</perceived_type>
    <chi_actual>0.84</chi_actual>
    <chi_perceived>0.0 (treats as unchangeable)</chi_perceived>
  </misclassification>
  
  <observable_actions>
    <action>Accepts "salao" label without resistance</action>
    <action>Does not attempt to organize other fishermen</action>
    <action>Routes around constraint (goes far out alone) rather than challenging it</action>
    <action>Internalizes belief ("Perhaps I truly am unlucky")</action>
  </observable_actions>
  
  <dialogue_markers>
    <quote>"I am a strange old man."</quote>
    <quote>"But I will show them what a man can do and what a man endures."</quote>
    <interpretation>
      Accepts constraint as given. Focuses on individual heroism within constraint
      rather than collective action to change constraint.
    </interpretation>
  </dialogue_markers>
  
  <consequence>
    <immediate>Wasted agency (could organize, doesn't)</immediate>
    <immediate>Unnecessary suffering (accepts isolation)</immediate>
    <long_term>Enables constraint persistence (no resistance)</long_term>
    <long_term>Tragic outcome (heroic effort within Snare still fails)</long_term>
  </consequence>
  
  <index_explanation>
    <why_error_occurs>
      From Santiago's index (powerless, biographical, trapped, local):
      - No visibility into other power positions (can't see it's Tangled Rope for parents)
      - Immediate time horizon (can't see generational changeability)
      - Trapped exit (no experience of mobility that would reveal constructedness)
      - Local scope (can't see how belief varies across communities)
      
      Objectively appears as Mountain from his structural position.
      Error is rational given limited information access.
    </why_error_occurs>
  </index_explanation>
  
  <correction_path>
    <trigger>TR4 (False Mountain Reveal)</trigger>
    <mechanism>
      Gains analytical perspective (sees enforcement, power-variance).
      Recognizes Snare nature (changeable, constructed).
      Opens possibility of TR1 (collective organization).
    </mechanism>
  </correction_path>
</error_manifestation>
```

### E2: Type III Error - Snare-as-Rope (Boy's Parents)

```xml
<error_manifestation id="E2">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <character>Boy's Parents</character>
  <constraint>C2 (The Tally of Luck)</constraint>
  
  <misclassification>
    <actual_type>Tangled Rope (⊞⊠) from their index, Snare (⊠) from Santiago's</actual_type>
    <perceived_type>Rope (⊞) - pure coordination</perceived_type>
    <chi_actual>0.56 (Tangled Rope)</chi_actual>
    <chi_perceived>~0.30 (treats as pure coordination)</chi_perceived>
  </misclassification>
  
  <observable_actions>
    <action>Enforce belief without acknowledging harm to Santiago/Manolin</action>
    <action>Frame decision as purely practical ("lucky boat earns more")</action>
    <action>Dismiss emotional costs as irrelevant</action>
    <action>No attempt to mitigate extraction (e.g., support Santiago while moving son)</action>
  </observable_actions>
  
  <dialogue_markers>
    <quote>"The boy must fish with a lucky boat."</quote>
    <interpretation>
      Presents as neutral resource allocation.
      Invisible: Santiago's isolation, Manolin's guilt, asymmetric burden.
      Coordination value visible, extraction invisible.
    </interpretation>
  </dialogue_markers>
  
  <consequence>
    <immediate>Voluntary participation in exploitation</immediate>
    <immediate>Moral blindness (don't see harm caused)</immediate>
    <long_term>Enables Snare persistence (no pressure to reform)</long_term>
    <long_term>Intergenerational transmission (teach son to accept extraction)</long_term>
  </consequence>
  
  <index_explanation>
    <why_error_occurs>
      From parents' index (moderate, generational, mobile, local):
      - Don't experience extraction directly (moderate power deflects cost)
      - Mobile exit (have alternatives, don't feel trapped)
      - Generational time (long-term practical focus obscures immediate suffering)
      - Impose extraction on others (powerless) while capturing coordination benefit
      
      Genuinely experience as Tangled Rope (coordination + some cost).
      But miss that it's Snare for those with less power.
      Error is structural invisibility, not malice.
    </why_error_occurs>
  </index_explanation>
  
  <correction_path>
    <trigger>Forced perspective-taking (see Santiago's experience)</trigger>
    <mechanism>
      Recognize asymmetric burden (they benefit, Santiago suffers).
      Acknowledge extraction component (not pure coordination).
      Reclassify as Tangled Rope (hybrid requiring navigation).
      Possible mitigation: support Santiago while enforcing practical decision.
    </mechanism>
  </correction_path>
</error_manifestation>
```

### E3: Type V.b Error - Tangled-as-Snare (Hypothetical Revolutionary)

```xml
<error_manifestation id="E3">
  <type>Type V.b: Tangled-as-Snare (Missing Coordination)</type>
  <character>Hypothetical Young Revolutionary Fisherman</character>
  <constraint>C2 (The Tally of Luck)</constraint>
  
  <misclassification>
    <actual_type>Tangled Rope (⊞⊠) - hybrid coordination-extraction</actual_type>
    <perceived_type>Snare (⊠) - pure extraction</perceived_type>
    <chi_actual>0.56-0.84 (varies by index)</chi_actual>
    <chi_perceived>0.90+ (sees only extraction)</chi_perceived>
  </misclassification>
  
  <observable_actions>
    <action>Advocates complete rejection of "luck" belief</action>
    <action>Proposes no alternative resource allocation mechanism</action>
    <action>Dismisses practical coordination value</action>
    <action>"Burn it all down" rhetoric without reconstruction plan</action>
  </observable_actions>
  
  <dialogue_markers>
    <quote>"Luck is a lie to keep us divided!"</quote>
    <quote>"We must reject all superstition!"</quote>
    <interpretation>
      Sees extraction clearly (correct).
      Misses coordination function (resource allocation still needed).
      No plan for what replaces belief system.
    </interpretation>
  </dialogue_markers>
  
  <consequence>
    <immediate>Destroys functional coordination</immediate>
    <immediate>Creates power vacuum (no alternative allocation mechanism)</immediate>
    <long_term>Possible worse outcome (chaos, or new Snare emerges)</long_term>
    <long_term>Discredits reform (associates change with destruction)</long_term>
  </consequence>
  
  <index_explanation>
    <why_error_occurs>
      From revolutionary's index (powerless, biographical, trapped, local):
      - Experiences high extraction (χ = 0.84, Snare territory)
      - Immediate suffering obscures coordination function
      - Trapped position creates urgency (no time for nuance)
      - Analytical insight (sees through normalization) but incomplete
      
      Correctly identifies extraction.
      Fails to recognize genuine coordination value.
      Error is incomplete analysis, not wrong analysis.
    </why_error_occurs>
  </index_explanation>
  
  <correction_path>
    <trigger>Forced to propose alternative system</trigger>
    <mechanism>
      Recognize coordination function (resource allocation needed).
      Distinguish extraction from coordination (surgical reform possible).
      Propose TR1 (collective organization) instead of pure rejection.
      Build Scaffold (alternative mechanism) before cutting Tangled Rope.
    </mechanism>
  </correction_path>
</error_manifestation>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

```xml
<rationality_model>
  <selected>Bounded Institutional Rationality (BIR)</selected>
  
  <justification>
    <reason>
      The Old Man and the Sea depicts realistic human institutions:
      - Fishing community with social norms (not algorithmic)
      - Market with human buyers/sellers (not perfect competition)
      - Family structures with emotional bonds (not utility maximizers)
    </reason>
    <reason>
      Characters satisfice under uncertainty:
      - Parents choose "lucky boat" (good enough, not optimal)
      - Santiago uses heuristics (go far out, not calculate expected value)
      - Market prices are negotiated (not Pareto-efficient)
    </reason>
    <reason>
      Principal-agent problems visible:
      - Parents (principals) vs Manolin (agent) - conflicting interests
      - Santiago (producer) vs Market (buyer) - information asymmetry
    </reason>
    <reason>
      Risk aversion drives behavior:
      - Parents avoid risk of son on "unlucky" boat
      - Santiago takes extreme risk (desperation, not rationality)
      - Market discounts risk (won't pay for potential catch)
    </reason>
  </justification>
  
  <implications>
    <attractor_compatibility>
      BIR enables:
      - Negotiated Equilibrium (bargaining possible)
      - Seeded Possibility (underground transformation)
      
      BIR makes difficult:
      - Deterministic Tragedy (requires PIR's implacable logic)
      - Revolutionary Rupture (requires more brittleness than BIR produces)
    </attractor_compatibility>
    
    <transformation_feasibility>
      TR1 (Collective Organization): FEASIBLE
        - BIR allows coordination under uncertainty
        - Satisficing enables "good enough" collective action
        - Risk aversion motivates mutual aid
      
      TR2 (Market Reform): FEASIBLE
        - BIR allows negotiation, bargaining
        - Imperfect information enables cooperative advantage
        - Principal-agent problems create reform pressure
      
      TR3 (Purity Drift): NATURAL
        - BIR's bounded rationality enables normalization
        - Satisficing allows "good enough" degraded systems
        - Entropy toward extraction is default
      
      TR4 (False Mountain Reveal): POSSIBLE
        - BIR allows learning, perspective-taking
        - Bounded rationality can be expanded (analytical index)
        - Not locked into fixed beliefs
    </transformation_feasibility>
  </implications>
  
  <contrast_with_PIR>
    <if_PIR_selected>
      Perfect Institutional Rationality would imply:
      - Market is Pareto-efficient (no reform possible)
      - "Luck" belief is optimal information aggregation
      - Santiago's tragedy is inevitable (no collective action)
      - Deterministic Tragedy is only reachable attractor
      
      This contradicts narrative evidence:
      - Market is clearly inefficient (risk borne by producer)
      - "Luck" belief is post-hoc rationalization, not information
      - Collective action is possible (just not taken)
      - Story suggests possibility of different outcome
    </if_