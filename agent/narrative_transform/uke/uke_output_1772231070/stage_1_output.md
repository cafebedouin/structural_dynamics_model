# STAGE 1: FORMAL SPECIFICATION
## "The Eighty-Yard Run" by Irwin Shaw

---

## I. FORMALIZED CONSTRAINTS

### C1: The New Language (Cultural Capital Barrier)

**Base Properties:**
- ε = 0.40 (moderate extraction, coordination-heavy)
- Suppression = 0.30 (soft enforcement through social exclusion)
- Coordination = TRUE (genuine network effects, shared cultural reference)
- Asymmetric = TRUE (benefits vary by cultural background)

**Indexed Classifications:**

#### Louise Darling
```xml
<character name="Louise Darling">
  <index>
    <power>powerful (π = 0.6)</power>
    <time>biographical</time>
    <exit>mobile</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.40 × 0.6 × 0.8
    χ = 0.192
  </calculation>
  <classification>Rope (⊞)</classification>
  <threshold_check>χ = 0.192 &lt; 0.46 (Rope/Tangled boundary)</threshold_check>
  <experience>Cultural knowledge as social capital. Opens doors to interesting people, career advancement, romantic connection. Minimal cost (already fluent), high coordination benefit.</experience>
</character>
```

#### Christian Darling
```xml
<character name="Christian Darling">
  <index>
    <power>powerless (π = 1.5)</power>
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.40 × 1.5 × 0.8
    χ = 0.48
  </calculation>
  <classification>Tangled Rope (⊞⊠)</classification>
  <threshold_check>χ = 0.48 ∈ [0.46, 0.70] (Tangled Rope range)</threshold_check>
  <experience>Barrier to intimacy with wife. Must participate to maintain relationship (coordination value) but constant reminder of inadequacy. Learning cost high, always behind, never fluent. Extraction: emotional labor, status anxiety, alienation.</experience>
</character>
```

**Indexical Variance:**
```
Δχ = 0.48 - 0.192 = 0.288
Type divergence: Rope → Tangled Rope
Mechanism: Same cultural system experienced as empowerment tool (Louise) vs. exclusion mechanism (Christian)
```

---

### C2: The Public Arena (Athletic Performance Economy)

**Base Properties:**
- ε = 0.70 (high extraction, coordination present but asymmetric)
- Suppression = 0.50 (moderate enforcement through institutional gatekeeping)
- Coordination = TRUE (genuine meritocratic sorting, spectacle value)
- Asymmetric = TRUE (extreme winner-take-all dynamics)

**Indexed Classifications:**

#### Christian Darling (Young, Peak Performance)
```xml
<character name="Christian Darling" phase="youth">
  <index>
    <power>powerful (π = 0.6)</power>
    <time>immediate</time>
    <exit>mobile</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.70 × 0.6 × 0.8
    χ = 0.336
  </calculation>
  <classification>Rope (⊞)</classification>
  <threshold_check>χ = 0.336 &lt; 0.46 (Rope/Tangled boundary)</threshold_check>
  <experience>Clear meritocracy. Physical performance directly converts to status, romantic success, future prospects. Body as reliable asset. Coordination benefit: spectacle creates value for all participants. Minimal extraction felt at peak.</experience>
</character>
```

#### Diederich (Post-Career, Broken)
```xml
<character name="Diederich" phase="post_career">
  <index>
    <power>powerless (π = 1.5)</power>
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>national (σ = 1.0)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.70 × 1.5 × 1.0
    χ = 1.05
  </calculation>
  <classification>Snare (⊠)</classification>
  <threshold_check>χ = 1.05 &gt; 0.70 (Snare threshold)</threshold_check>
  <experience>System extracted physical health permanently. "They broke my neck for me." Iron brace for life. No compensation, no alternative career path. Coordination value (spectacle) captured by institution, extraction (injury) borne by individual. Trapped in consequences with no exit.</experience>
</character>
```

#### Christian Darling (Post-Career, Declining)
```xml
<character name="Christian Darling" phase="decline">
  <index>
    <power>powerless (π = 1.5)</power>
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.70 × 1.5 × 0.8
    χ = 0.84
  </calculation>
  <classification>Snare (⊠)</classification>
  <threshold_check>χ = 0.84 &gt; 0.70 (Snare threshold)</threshold_check>
  <experience>Skills obsolete. Identity built on performance now inaccessible. System promised future but delivered only temporary status. No transferable skills. Extraction: wasted youth, identity crisis, economic precarity. Trapped by having built entire self-concept on ephemeral advantage.</experience>
</character>
```

**Indexical Variance:**
```
Peak performance: χ = 0.336 (Rope)
Post-career: χ = 0.84 (Snare)
Δχ = 0.504 (massive divergence)
Type flip: Rope → Snare
Mechanism: Time-dependent extraction. System appears as Rope during brief window of peak performance, reveals as Snare across biographical time horizon.
```

---

### C3: The Marriage Contract (Asymmetric Intimacy Economy)

**Base Properties:**
- ε = 0.55 (moderate-high extraction, genuine coordination present)
- Suppression = 0.40 (moderate enforcement through social/economic pressure)
- Coordination = TRUE (genuine partnership benefits, resource pooling)
- Asymmetric = TRUE (benefits and costs distributed by power within relationship)

**Graph Position:**
- Downstream of: C1 (New Language), C2 (Public Arena)
- Feeds into: Terminal state

**Indexed Classifications:**

#### Louise Darling
```xml
<character name="Louise Darling">
  <index>
    <power>powerful (π = 0.6)</power>
    <time>biographical</time>
    <exit>mobile</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.55 × 0.6 × 0.8
    χ = 0.264
  </calculation>
  <classification>Rope (⊞)</classification>
  <threshold_check>χ = 0.264 &lt; 0.46 (Rope/Tangled boundary)</threshold_check>
  <experience>Marriage provides companionship, social status, economic security. Can maintain independent social life, career trajectory. Exit options present (financially independent, socially connected). Coordination benefits high, extraction low from her position.</experience>
</character>
```

#### Christian Darling (Early Marriage)
```xml
<character name="Christian Darling" phase="early_marriage">
  <index>
    <power>moderate (π = 1.0)</power>
    <time>biographical</time>
    <exit>constrained</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.55 × 1.0 × 0.8
    χ = 0.44
  </calculation>
  <classification>Rope (⊞) [borderline Tangled]</classification>
  <threshold_check>χ = 0.44 &lt; 0.46 (just below Tangled threshold)</threshold_check>
  <experience>Marriage provides love, status through wife's social position, motivation. Still has athletic identity, some economic prospects. Coordination benefits visible. Extraction present but manageable.</experience>
</character>
```

#### Christian Darling (Late Marriage)
```xml
<character name="Christian Darling" phase="late_marriage">
  <index>
    <power>powerless (π = 1.5)</power>
    <time>biographical</time>
    <exit>trapped</exit>
    <scope>local (σ = 0.8)</scope>
  </index>
  <calculation>
    χ = ε × π × σ
    χ = 0.55 × 1.5 × 0.8
    χ = 0.66
  </calculation>
  <classification>Tangled Rope (⊞⊠) [heavy, trending Snare]</classification>
  <threshold_check>χ = 0.66 ∈ [0.46, 0.70] (upper Tangled Rope range)</threshold_check>
  <experience>Marriage now site of humiliation. Wife's success highlights his failure. Economically dependent. Social life entirely her world where he's inadequate. Coordination benefits (companionship, household) still present but extraction dominates: status anxiety, economic dependence, identity erosion. Trapped by economic necessity and social expectation.</experience>
</character>
```

**Indexical Variance:**
```
Louise: χ = 0.264 (Rope)
Christian (early): χ = 0.44 (Rope, borderline)
Christian (late): χ = 0.66 (Tangled Rope, heavy)
Maximum divergence: Δχ = 0.396
Type divergence: Rope (Louise) vs. Tangled Rope trending Snare (Christian late)
Mechanism: Power differential within marriage amplifies over time as external constraints (C1, C2) shift Christian's position from moderate to powerless.
```

---

## II. TRANSFORMATION RULES

### TR1: Athletic Obsolescence (C2 Lifecycle)

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>time_passage AND physical_decline</condition>
    <threshold>biographical_time &gt; peak_performance_window</threshold>
  </trigger>
  <index_change>
    <character>Christian Darling</character>
    <from>
      <power>powerful (π = 0.6)</power>
      <time>immediate</time>
      <exit>mobile</exit>
    </from>
    <to>
      <power>powerless (π = 1.5)</power>
      <time>biographical</time>
      <exit>trapped</exit>
    </to>
  </index_change>
  <constraint_effect constraint="C2">
    <chi_recalculation>
      <before>0.336 (Rope)</before>
      <after>0.84 (Snare)</after>
    </chi_recalculation>
    <type_change>Rope (⊞) → Snare (⊠)</type_change>
    <mechanism>Time-dependent extraction reveals. Skills non-transferable, identity built on ephemeral advantage, no alternative path constructed during peak window.</mechanism>
  </constraint_effect>
  <cascade_effects>
    <effect constraint="C3">
      <description>Power position within marriage degrades as economic/status foundation erodes</description>
      <chi_shift>0.44 → 0.66 (Rope → Tangled Rope)</chi_shift>
    </effect>
    <effect constraint="C1">
      <description>Cultural capital gap becomes more salient as athletic capital vanishes</description>
      <chi_remains>0.48 (Tangled Rope, but subjectively heavier)</chi_remains>
    </effect>
  </cascade_effects>
  <irreversibility>TRUE</irreversibility>
  <narrative_marker>"He had practiced the wrong thing, perhaps."</narrative_marker>
</transformation_rule>
```

### TR2: Cultural Capital Accumulation (C1 Lifecycle)

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>sustained_engagement AND social_network_expansion</condition>
    <character>Louise Darling</character>
  </trigger>
  <index_change>
    <from>
      <power>moderate (π = 1.0)</power>
    </from>
    <to>
      <power>powerful (π = 0.6)</power>
    </to>
  </index_change>
  <constraint_effect constraint="C1">
    <chi_recalculation>
      <before>0.32 (Rope)</before>
      <after>0.192 (Rope, strengthened)</after>
    </chi_recalculation>
    <type_stability>Rope (⊞) → Rope (⊞)</type_stability>
    <mechanism>Positive feedback loop. Cultural fluency → social connections → more cultural exposure → greater fluency. Coordination benefits compound.</mechanism>
  </constraint_effect>
  <contrast>
    <character>Christian Darling</character>
    <effect>Relative gap widens. Louise's increasing fluency makes Christian's stagnation more visible.</effect>
    <chi_divergence_increase>Δχ grows from 0.16 to 0.288</chi_divergence_increase>
  </contrast>
  <narrative_marker>"Louise was getting a divorce because she had grown away from him, not because he had become a bad husband."</narrative_marker>
</transformation_rule>
```

### TR3: Marriage Power Inversion (C3 Transformation)

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>TR1.complete AND TR2.complete</condition>
    <description>Athletic capital depleted AND cultural capital gap widened</description>
  </trigger>
  <index_change>
    <character>Christian Darling</character>
    <constraint>C3</constraint>
    <from>
      <power>moderate (π = 1.0)</power>
      <exit>constrained</exit>
    </from>
    <to>
      <power>powerless (π = 1.5)</power>
      <exit>trapped</exit>
    </to>
  </index_change>
  <constraint_effect constraint="C3">
    <chi_recalculation>
      <before>0.44 (Rope, borderline)</before>
      <after>0.66 (Tangled Rope, heavy)</after>
    </chi_recalculation>
    <type_change>Rope (⊞) → Tangled Rope (⊞⊠)</type_change>
    <mechanism>
      <coordination_persists>Household partnership, companionship, social expectation</coordination_persists>
      <extraction_emerges>Economic dependence, status humiliation, identity erosion, social alienation in wife's world</extraction_emerges>
    </mechanism>
  </constraint_effect>
  <asymmetry>
    <louise_experience>χ remains 0.264 (Rope). Marriage still functional from her position.</louise_experience>
    <christian_experience>χ = 0.66 (Tangled Rope). Marriage now site of extraction.</christian_experience>
    <divergence>Δχ = 0.396. Same relationship, radically different structural experiences.</divergence>
  </asymmetry>
  <exit_dynamics>
    <louise>Mobile. Can leave without economic hardship.</louise>
    <christian>Trapped. Economic dependence, no alternative identity/career, social isolation outside marriage.</christian>
  </exit_dynamics>
  <narrative_marker>"He had been a hero and now he was not."</narrative_marker>
</transformation_rule>
```

### TR4: Nostalgia Trap (False Mountain Formation)

```xml
<transformation_rule id="TR4">
  <trigger>
    <condition>TR1.complete AND present_circumstances_intolerable</condition>
    <psychological_mechanism>Cognitive escape through memory</psychological_mechanism>
  </trigger>
  <error_formation>
    <type>Type I: False Mountain</type>
    <character>Christian Darling</character>
    <misclassification>
      <treats_as>Mountain (■) - "The past was perfect and unchangeable"</treats_as>
      <actually_is>Piton (⊟) - Obsolete constraint, function dried up</actually_is>
    </misclassification>
  </error_formation>
  <mechanism>
    <step1>Present constraints (C1, C2, C3) all experienced as Snare/Tangled Rope</step1>
    <step2>Memory of past when C2 was Rope provides psychological refuge</step2>
    <step3>Repeated mental rehearsal of "eighty-yard run" moment</step3>
    <step4>Past crystallizes as unchangeable perfection (False Mountain)</step4>
    <step5>Agency directed toward impossible restoration rather than present navigation</step5>
  </mechanism>
  <consequence>
    <wasted_agency>Energy spent on nostalgia rather than adaptation</wasted_agency>
    <present_deterioration>Current constraints worsen while attention focused on past</present_deterioration>
    <relationship_damage>Louise experiences his nostalgia as refusal to engage present</relationship_damage>
  </consequence>
  <indexical_note>
    From Christian's index (powerless, trapped, immediate→biographical time shift), the past genuinely appears as Mountain - a time when constraints were navigable. Error is treating this as recoverable rather than as evidence that C2 was always time-limited Rope, not eternal Mountain.
  </indexical_note>
  <narrative_marker>"He wished gently that he could go back fifteen years."</narrative_marker>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### E1: Type I Error - False Mountain (Christian's Nostalgia)

```xml
<error_manifestation id="E1">
  <type>Type I: False Mountain</type>
  <character>Christian Darling</character>
  <misclassification>
    <treats_constraint>C2 (Athletic Performance Economy) in past</treats_constraint>
    <as_type>Mountain (■) - Unchangeable perfection</as_type>
    <actual_type>Rope (⊞) with time-limited window, now Piton (⊟)</actual_type>
  </misclassification>
  <observable_actions>
    <action>Repeated mental rehearsal of "eighty-yard run" moment</action>
    <action>Physical re-enactment: runs the same route on practice field</action>
    <action>Emotional investment in memory rather than present</action>
    <action>Treats past as recoverable rather than as evidence of constraint's time-dependence</action>
  </observable_actions>
  <dialogue_markers>
    <quote>"He wished gently that he could go back fifteen years."</quote>
    <quote>Memory of "the high point" - implies everything since is decline from unchangeable peak</quote>
  </dialogue_markers>
  <structural_cause>
    <present_state>All current constraints experienced as Snare/Tangled Rope (high χ)</present_state>
    <past_state>C2 was Rope (low χ) during brief peak performance window</past_state>
    <cognitive_escape>Memory provides refuge from intolerable present</cognitive_escape>
    <error_formation>Refuge crystallizes into False Mountain - treats past as unchangeable perfection rather than time-limited Rope</error_formation>
  </structural_cause>
  <consequence>
    <wasted_agency>Energy directed toward impossible restoration</wasted_agency>
    <present_neglect>Current constraints (C1, C3) worsen while attention backward-focused</present_neglect>
    <relationship_damage>Louise experiences nostalgia as refusal to engage with her/present reality</relationship_damage>
  </consequence>
  <indexical_validity>
    From Christian's index (powerless, trapped, biographical time), past genuinely appears as Mountain - constraints were navigable then. Error is not in perception but in treating this as recoverable state rather than as evidence that C2 was always time-dependent.
  </indexical_validity>
</error_manifestation>
```

### E2: Type III Error - Snare-as-Rope (Christian's Early Marriage)

```xml
<error_manifestation id="E2">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <character>Christian Darling</character>
  <phase>Early marriage, peak athletic performance</phase>
  <misclassification>
    <treats_constraint>C2 (Athletic Performance Economy)</treats_constraint>
    <as_type>Rope (⊞) - Pure coordination, reliable path to success</as_type>
    <actual_type>Tangled Rope (⊞⊠) with time-dependent extraction, trending Snare</actual_type>
  </misclassification>
  <observable_actions>
    <action>Invests entire identity in athletic performance</action>
    <action>Neglects development of transferable skills</action>
    <action>Builds no alternative career foundation</action>
    <action>Assumes athletic capital will remain valuable across biographical time</action>
  </observable_actions>
  <structural_cause>
    <index_position>Powerful (π = 0.6), immediate time horizon, mobile exit options</index_position>
    <genuine_coordination>System does provide real benefits at peak: status, romantic success, clear meritocracy</genuine_coordination>
    <hidden_extraction>Time-limited window, physical toll, skill non-transferability, identity trap</hidden_extraction>
    <normalization>Success at peak normalizes the system, makes extraction invisible</normalization>
  </structural_cause>
  <consequence>
    <no_preparation>Fails to build alternative skills/identity during peak window</no_preparation>
    <identity_trap>Entire self-concept built on ephemeral advantage</identity_trap>
    <extraction_reveals>When time window closes, full extraction becomes visible (TR1)</extraction_reveals>
    <catastrophic_transition>Rope → Snare flip experienced as betrayal rather than predictable lifecycle</catastrophic_transition>
  </consequence>
  <indexical_note>
    Error is index-sensitive. From immediate time horizon and powerful position, C2 genuinely appears as Rope. Extraction only visible from biographical time horizon or from position of those already broken (Diederich). Christian's error is not stupidity but structural blindness - his index prevents seeing what's visible from other positions.
  </indexical_note>
  <narrative_marker>"He had practiced the wrong thing, perhaps."</narrative_marker>
</error_manifestation>
```

### E3: Type V.a Error - Tangled-as-Rope (Louise's Marriage View)

```xml
<error_manifestation id="E3">
  <type>Type V.a: Tangled-as-Rope (Ignoring Extraction)</type>
  <character>Louise Darling</character>
  <misclassification>
    <treats_constraint>C3 (Marriage Contract)</treats_constraint>
    <as_type>Rope (⊞) - Functional partnership</as_type>
    <actual_type>Tangled Rope (⊞⊠) - Coordination + asymmetric extraction</actual_type>
  </misclassification>
  <observable_actions>
    <action>Maintains independent social life without recognizing Christian's exclusion</action>
    <action>Pursues career/cultural advancement without seeing impact on power balance</action>
    <action>Experiences marriage as functional while Christian experiences extraction</action>
    <action>Surprised by divorce request - "he had become a bad husband" misdiagnosis</action>
  </observable_actions>
  <structural_cause>
    <index_position>Powerful (π = 0.6), mobile exit, local scope</index_position>
    <genuine_coordination>Marriage does provide real benefits: companionship, household partnership, social legitimacy</genuine_coordination>
    <invisible_extraction>From her position (χ = 0.264), extraction is minimal. From Christian's position (χ = 0.66), extraction dominates.</invisible_extraction>
    <power_blindness>Powerful position prevents seeing extraction borne by powerless partner</power_blindness>
  </structural_cause>
  <consequence>
    <relationship_deterioration>Christian's experience of extraction intensifies while Louise sees functional partnership</relationship_deterioration>
    <communication_failure>Incompatible structural experiences prevent mutual understanding</communication_failure>
    <divorce_as_surprise>Louise experiences Christian's unhappiness as character flaw rather than structural position</divorce_as_surprise>
  </consequence>
  <indexical_validity>
    Louise is not wrong from her index. Marriage genuinely is Rope (χ = 0.264) from her position. Error is not in her classification but in assuming her experience is universal - failing to recognize that same constraint is Tangled Rope (χ = 0.66) from Christian's position.
  </indexical_validity>
  <narrative_marker>"Louise was getting a divorce because she had grown away from him, not because he had become a bad husband."</narrative_marker>
</error_manifestation>
```

### E4: Type III Error - Snare-as-Rope (Diederich's Retrospective)

```xml
<error_manifestation id="E4">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <character>Diederich</character>
  <phase>During professional career</phase>
  <misclassification>
    <treats_constraint>C2 (Athletic Performance Economy)</treats_constraint>
    <as_type>Rope (⊞) - Meritocratic path to glory</as_type>
    <actual_type>Snare (⊠) - Extraction trap with coordination facade</actual_type>
  </misclassification>
  <observable_actions>
    <action>Played professionally until catastrophic injury</action>
    <action>Accepted physical risk as part of meritocratic bargain</action>
    <action>No alternative career developed</action>
    <action>Now permanently disabled: "They broke my neck for me"</action>
  </observable_actions>
  <structural_cause>
    <peak_experience>At peak performance, system appeared as Rope (genuine meritocracy, clear rewards)</peak_experience>
    <hidden_extraction>Physical toll, injury risk, skill non-transferability, winner-take-all dynamics</hidden_extraction>
    <normalization>Success normalizes risk. "All-American" status makes extraction invisible.</normalization>
    <catastrophic_reveal>Extraction reveals suddenly and permanently through injury</catastrophic_reveal>
  </structural_cause>
  <consequence>
    <permanent_disability>Iron brace for life, physical capacity destroyed</permanent_disability>
    <no_compensation>System extracted health, provided no alternative path</no_compensation>
    <retrospective_clarity>Now sees system as Snare (χ = 1.05) but too late to exit</retrospective_clarity>
    <warning_function>Serves as narrative warning to Christian of C2's true nature</warning_function>
  </consequence>
  <indexical_note>
    Diederich's error is identical to Christian's (E2) but with more catastrophic outcome. Both treated C2 as Rope during peak performance window. Diederich's broken body is physical evidence of the extraction Christian hasn't yet fully experienced. From biographical time horizon and powerless post-injury position, C2 reveals as Snare.
  </indexical_note>
  <narrative_function>
    <foreshadowing>Diederich's fate previews Christian's trajectory</foreshadowing>
    <structural_proof>Physical evidence that C2 extracts permanently, not just temporarily</structural_proof>
    <indexical_contrast>Same system, different outcomes based on when extraction reveals (injury vs. aging)</indexical_contrast>
  </narrative_function>
</error_manifestation>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>Bounded Institutional Rationality (BIR)</type>
  <justification>
    <reason>Story involves human institutions (marriage, social networks, athletic systems) not implacable natural forces</reason>
    <reason>Characters satisfice under uncertainty rather than optimize perfectly</reason>
    <reason>Principal-agent problems present (athletic institutions vs. athletes, marriage partners with asymmetric information)</reason>
    <reason>Risk aversion and bounded information drive decisions (Christian doesn't see C2's time-dependence, Louise doesn't see Christian's extraction experience)</reason>
    <reason>Negotiation and compromise possible but constrained by power differentials and information asymmetry</reason>
  </justification>
  <characteristics>
    <satisficing>Characters seek "good enough" rather than optimal outcomes</satisficing>
    <bounded_information>Christian doesn't see C2's lifecycle, Louise doesn't see Christian's χ values</bounded_information>
    <risk_aversion>Christian clings to marriage despite unhappiness (exit risk), Louise maintains status quo until divorce</risk_aversion>
    <principal_agent>Athletic institutions extract from athletes, marriage partners have asymmetric power/information</principal_agent>
    <negotiation_possible>Divorce represents negotiated exit rather than catastrophic rupture</negotiation_possible>
  </characteristics>
  <attractor_compatibility>
    <enables>Negotiated Equilibrium (divorce as negotiated exit)</enables>
    <enables>Seeded Possibility (Christian's potential transformation post-divorce)</enables>
    <constrains>Deterministic Tragedy (BIR allows negotiation, not inevitable doom)</constrains>
    <constrains>Revolutionary Rupture (no disruption of constraint logic itself)</constrains>
  </attractor_compatibility>
  <contrast_with_PIR>
    <pir_would_predict>Deterministic Tragedy (constraints run to completion, no negotiation)</pir_would_predict>
    <bir_predicts>Negotiated Equilibrium (divorce as satisficing solution to incompatible χ experiences)</bir_predicts>
    <story_evidence>Louise initiates divorce (negotiated exit) rather than marriage collapsing catastrophically. Christian survives rather than being crushed. Both satisfice rather than optimize.</story_evidence>
  </contrast_with_PIR>
</rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Negotiated Equilibrium

```xml
<terminal_attractor>
  <type>Negotiated Equilibrium</type>
  <definition>Constraints find balance through bargaining and satisficing. Characters reach stable (if suboptimal) arrangements through negotiation rather than catastrophic rupture or complete victory.</definition>
  
  <justification>
    <narrative_evidence>
      <evidence>Louise initiates divorce - negotiated exit rather than catastrophic collapse</evidence>
      <evidence>Christian survives the dissolution - not crushed, but diminished</evidence>
      <evidence>Story ends with Christian alone but functional, not destroyed</evidence>
      <evidence>No revolutionary transformation of constraint logic itself</evidence>
      <evidence>No deterministic doom - characters make choices within constraints</evidence>
    </narrative_evidence>
    
    <structural_evidence>
      <evidence>BIR model enables negotiation and satisficing</evidence>
      <evidence>Power differentials allow Louise to exit (mobile) while Christian remains trapped but not destroyed</evidence>
      <evidence>Constraints persist but characters find new equilibria within them</evidence>
      <evidence>C1, C2, C3 all remain operative - no constraint elimination</evidence>
    </structural_evidence>
    
    <character_trajectories>
      <louise>Exits marriage (mobile, powerful position allows negotiated exit). Continues in cultural world (C1 remains Rope for her). Finds new equilibrium.</louise>
      <christian>Remains trapped in diminished state but survives. C2 remains Snare (obsolete skills), C1 remains Tangled Rope (