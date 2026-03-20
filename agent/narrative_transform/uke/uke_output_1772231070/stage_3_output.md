# STAGE 3: OPERATIONAL SPECIFICATION

## 1. TERMINAL ATTRACTOR VERIFICATION

**Selected Attractor:** Negotiated Equilibrium

**Verification:**
```xml
<attractor_verification>
  <attractor>Negotiated Equilibrium</attractor>
  
  <compatibility_check>
    <rationality_model>BIR ✓</rationality_model>
    <reasoning>BIR enables satisficing and negotiated exits. Characters can bargain within constraints rather than being crushed by them.</reasoning>
  </compatibility_check>
  
  <constraint_logic_alignment>
    <c1_new_language>
      Persists post-divorce. Suyin remains in cosmopolitan world (Rope).
      Chen Mingzhe remains excluded (Tangled Rope). No resolution of indexical variance.
    </c1_new_language>
    
    <c2_athletic_economy>
      Irreversible transition complete (Rope → Snare). Chen Mingzhe's athletic capital exhausted.
      System persists (other young athletes still entering), but he's aged out.
    </c2_athletic_economy>
    
    <c3_marriage>
      Negotiated dissolution. Suyin exits (mobile position allows). Chen Mingzhe survives but diminished.
      Neither destroyed, both find new (suboptimal) equilibria.
    </c3_marriage>
  </constraint_logic_alignment>
  
  <narrative_evidence>
    <evidence>Divorce as negotiated exit, not catastrophic collapse</evidence>
    <evidence>Chen Mingzhe survives alone but functional</evidence>
    <evidence>No revolutionary transformation of constraint systems</evidence>
    <evidence>Characters satisfice rather than optimize</evidence>
  </narrative_evidence>
  
  <status>VERIFIED ✓</status>
</attractor_verification>
```

---

## 2. VOICE ARCHETYPE SELECTION

**Selected Voice:** The Condemned

**Justification:**
```xml
<voice_selection>
  <archetype>The Condemned</archetype>
  
  <structural_fit>
    <attractor_compatibility>
      ✓ Negotiated Equilibrium (Condemned can survive, diminished)
      ✓ Seeded Possibility (awareness grows too late)
      ⚠ Tragedy (compatible but we're not going full tragic)
    </attractor_compatibility>
    
    <constraint_requirements>
      ✓ PIR/BIR model (Chen Mingzhe acts rationally given limited information)
      ✓ Powerless position (π = 1.5 in C1, C2 post-decline, C3 late)
      ✓ Trapped exit (cannot leave marriage without economic destruction)
      ✓ Experiences constraints as natural/inevitable, not constructed
    </constraint_requirements>
  </structural_fit>
  
  <character_alignment>
    <chen_mingzhe>
      Inside the constraint logic. Cannot see systemic shape.
      Experiences athletic obsolescence as personal failure, not lifecycle.
      Experiences cultural exclusion as inadequacy, not structural barrier.
      Experiences marriage trap as his fault, not power differential.
      Voice is immediate, experiential, not analytical.
    </chen_mingzhe>
  </character_alignment>
  
  <why_not_others>
    <builder>Would require seeing leverage points, testing strategies. Chen Mingzhe doesn't—he's reactive, not strategic.</builder>
    <witness>Would require observational distance. Chen Mingzhe is too embedded in his own experience.</witness>
    <survivor>Would require temporal distance post-rupture. We're ending at the moment of negotiated exit, not years later.</survivor>
    <guide>Would require pedagogical stance. Chen Mingzhe doesn't understand what's happening to him.</guide>
  </why_not_others>
  
  <voice_characteristics>
    <perspective>First person (Chen Mingzhe's immediate experience)</perspective>
    <temporal_mode>Present tense (trapped in the now)</temporal_mode>
    <awareness_level>Experiences constraint as natural, not constructed</awareness_level>
    <emotional_register>Melancholic resignation, not rage or analysis</emotional_register>
  </voice_characteristics>
</voice_selection>
```

---

## 3. INDEXICAL REVELATION STRATEGY

**Selected Strategy:** Gradual Realization (Single POV, slowly discovers others experience differently)

**Implementation:**
```xml
<revelation_strategy>
  <approach>Gradual Realization</approach>
  
  <mechanism>
    Chen Mingzhe's POV throughout. He slowly becomes aware that:
    - Suyin experiences the marriage differently (functional for her, trap for him)
    - The International Club world is accessible to her, barrier to him
    - His athletic past was time-limited Rope, not eternal Mountain
    - Wei Guozhang's broken body previews his own trajectory
  </mechanism>
  
  <revelation_sequence>
    <act_1>
      Chen Mingzhe experiences constraints as personal inadequacy.
      "I should try harder to understand the conversations."
      "I should have planned better after football."
      Indexical variance invisible—assumes his experience is universal.
    </act_1>
    
    <act_2>
      Accumulating evidence that others experience differently:
      - Suyin's ease in spaces where he's silent
      - Wei's bitterness about the athletic system
      - Younger players at the bank still believing in football's promise
      Begins to suspect structural pattern, but can't articulate it.
    </act_2>
    
    <act_3>
      Divorce conversation forces recognition:
      - Suyin says marriage is "not working" (her Rope experience)
      - Chen Mingzhe realizes she doesn't see his trap (her blindness from powerful position)
      - Final scene: alone, running the old route, understanding comes too late
      Recognition without escape—Condemned archetype fulfilled.
    </act_3>
  </revelation_sequence>
  
  <reader_experience>
    Reader sees indexical variance before Chen Mingzhe does.
    Dramatic irony: we understand the structural trap while he's still blaming himself.
    Revelation is tragic because it comes after negotiated exit—understanding without agency.
  </reader_experience>
</revelation_strategy>
```

---

## 4. EDITORIAL DECISIONS

```xml
<editorial_decisions>
  <length>
    <selection>Short Story (4,000-5,000 words)</selection>
    <justification>
      Three constraint systems + lifecycle transitions + indexical variance.
      Needs space for gradual realization arc.
      Too complex for flash, not enough plot for novelette.
    </justification>
  </length>
  
  <pov>
    <selection>First Person (Chen Mingzhe)</selection>
    <justification>
      Condemned voice requires immediate, embedded perspective.
      Cannot see systemic shape from inside.
      First person naturalizes limited awareness.
    </justification>
  </pov>
  
  <tense>
    <selection>Present Tense</selection>
    <justification>
      Trapped in immediate experience (time horizon = immediate).
      Present tense reinforces inability to see biographical arc.
      Creates urgency, prevents retrospective distance.
    </justification>
  </tense>
  
  <character_count>
    <primary>1 (Chen Mingzhe - POV)</primary>
    <secondary>2 (Xu Suyin, Wei Guozhang)</secondary>
    <justification>
      Minimum for constraint interactions:
      - Suyin: indexical contrast (powerful vs powerless in C1, C3)
      - Wei: foreshadowing (C2 extraction already complete)
      Additional characters would dilute focus.
    </justification>
  </character_count>
  
  <naming>
    <selection>Cultural (Chinese names)</selection>
    <justification>
      Setting is 1932 Tianjin—cultural names naturalize context.
      Chen Mingzhe (陈明哲): "bright wisdom" (ironic—he lacks insight)
      Xu Suyin (徐素音): "pure sound" (her voice in multiple languages)
      Wei Guozhang (魏国章): "national glory" (ironic—broken by the system)
    </justification>
  </naming>
  
  <linguistic_implementation>
    <code_switching>
      Chen Mingzhe's internal monologue in English (story language).
      Dialogue with Suyin: mostly Chinese (indicated by context, not tags).
      International Club scenes: English dialogue (Chen Mingzhe silent or struggling).
      The switch signals: inclusion (Chinese) vs exclusion (English).
    </code_switching>
    
    <technique>
      Don't use italics or language tags.
      Show through Chen Mingzhe's comprehension:
      - Chinese dialogue: he understands, responds naturally
      - English dialogue: he catches fragments, misses references, stays silent
      Reader infers language from his experience of it.
    </technique>
  </linguistic_implementation>
  
  <emotional_core>
    <source>Constraint dynamics, not imposed</source>
    <primary_emotion>Melancholic resignation</primary_emotion>
    <mechanism>
      Emerges from:
      - C2 lifecycle (glory → obsolescence)
      - C3 power inversion (partnership → dependency)
      - C1 permanent exclusion (cannot access Suyin's world)
      Not "sad story"—structural inevitability produces melancholy.
    </mechanism>
  </emotional_core>
  
  <ending_strategy>
    <selection>Equilibrium established, agent removed</selection>
    <specification>
      Divorce negotiated (Suyin exits, Chen Mingzhe survives).
      Final scene: Chen Mingzhe alone on old Nankai pitch, running the championship route.
      Understanding comes (recognizes the trap) but too late (already removed from marriage).
      Constraints persist (C1, C2, C3 all still operative for others).
      He finds new equilibrium (alone, diminished, but functional).
    </specification>
  </ending_strategy>
</editorial_decisions>
```

---

## 5. PRIMARY PHYSICAL MARKER

```xml
<physical_marker>
  <marker>Chen Mingzhe's breathing during the championship run memory vs. present</marker>
  
  <emergence_from_inhabitation>
    Inhabitation sentence (from Stage 1): "Chen Mingzhe experiences C2 as Rope at peak (body as reliable asset, clear meritocracy) then Snare post-decline (skills obsolete, identity crisis, no transferable value)."
    
    When someone's entire identity is built on physical performance, and that performance is time-limited, the body becomes both asset and betrayer. The breath—automatic, unconscious at peak—becomes labored, conscious, a reminder of decline.
  </emergence_from_inhabitation>
  
  <function>
    <past_memory>
      "In the memory, I'm not breathing. I'm just running. The ball at my feet, the goal ahead, my body knowing exactly what to do. No thought, no effort, just perfect motion."
    </past_memory>
    
    <present_reality>
      "Now, on the Sunday pitch, I count my breaths. In-two-three-four, out-two-three-four. My knees ache. The younger men from the bank are faster. I'm aware of my body in a way I never was at eighteen."
    </present_reality>
    
    <revelation_moment>
      "When Suyin tells me about the divorce, I realize I've been holding my breath. In her world—the salons, the conversations in English, the gatherings where everyone laughs at references I don't understand—I've been holding my breath for three years. Waiting for the moment to pass. Waiting to breathe again."
    </revelation_moment>
  </function>
  
  <reader_understanding>
    Reader will notice the breathing pattern before Chen Mingzhe articulates it:
    - Past: effortless, unconscious
    - Present athletic: labored, counted
    - Present social: held, suspended
    The physical marker reveals the constraint experience from inside.
  </reader_understanding>
  
  <why_this_marker>
    Not chosen from motif palette. Emerges from the structural reality:
    When your body was your power, and your body is failing, you notice every breath.
    When you're trapped in spaces where you don't belong, you hold your breath.
    The marker is what the constraint feels like in the body.
  </why_this_marker>
</physical_marker>
```

---

## 6. STORY BLUEPRINT

```xml
<story_blueprint>
  <title>The Championship Run</title>
  
  <attractor>
    Negotiated Equilibrium
    <justification>
      BIR model enables satisficing. Divorce as negotiated exit.
      Chen Mingzhe survives diminished. Constraints persist.
      No revolutionary transformation, no deterministic doom.
    </justification>
  </attractor>
  
  <voice>
    The Condemned
    <justification>
      Inside constraint logic, cannot see systemic shape.
      Powerless position (π = 1.5), trapped exit.
      Experiences constraints as natural/inevitable.
      First person present tense, immediate experience.
    </justification>
  </voice>
  
  <revelation_strategy>
    Gradual Realization (Single POV)
    <mechanism>
      Chen Mingzhe slowly discovers others experience same constraints differently.
      Indexical variance becomes visible through accumulated evidence.
      Recognition comes too late—after negotiated exit already initiated.
    </mechanism>
  </revelation_strategy>
  
  <editorial_decisions>
    <length>4,000-5,000 words (Short Story)</length>
    <pov>First Person (Chen Mingzhe)</pov>
    <tense>Present Tense</tense>
    <character_count>3 (Chen Mingzhe, Xu Suyin, Wei Guozhang)</character_count>
    <naming>Cultural (Chinese names, 1932 Tianjin context)</naming>
    <linguistic_implementation>
      Code-switching shown through comprehension, not tags.
      Chinese dialogue: natural understanding.
      English dialogue: fragments, silence, exclusion.
    </linguistic_implementation>
  </editorial_decisions>
  
  <physical_marker>
    Chen Mingzhe's breathing:
    - Past (championship): effortless, unconscious
    - Present (athletic): labored, counted
    - Present (social): held, suspended
    Reveals constraint experience from inside the body.
  </physical_marker>
  
  <act_structure>
    <act_1>
      <title>The Memory and the Present</title>
      <constraint_activation>
        - C2 (Athletic Economy): Chen Mingzhe at Sunday game, body failing
        - C1 (New Language): International Club gathering, silent while Suyin shines
        - C3 (Marriage): Domestic scene, awareness of dependency
      </constraint_activation>
      <character_experience>
        Chen Mingzhe experiences all three as personal inadequacy.
        "I should try harder." "I should have planned better."
        Indexical variance invisible—assumes his experience is universal.
      </character_experience>
      <physical_marker_introduction>
        Championship memory: effortless breathing, perfect motion.
        Sunday game: counted breaths, aching knees.
        Contrast established but not yet articulated.
      </physical_marker_introduction>
    </act_1>
    
    <act_2>
      <title>The Accumulation</title>
      <constraint_coupling>
        - C2 → C3: Athletic obsolescence erodes marriage power balance
        - C1 → C3: Cultural capital gap widens, intimacy erodes
        - Wei Guozhang encounter: C2 extraction already complete (broken body)
      </constraint_coupling>
      <collision_from_different_indices>
        - Suyin's ease vs Chen Mingzhe's silence (C1 indexical variance)
        - Wei's bitterness vs younger players' hope (C2 time-dependent experience)
        - Suyin's "functional marriage" vs Chen Mingzhe's trap (C3 power differential)
      </collision_from_different_indices>
      <gradual_realization>
        Chen Mingzhe begins to suspect pattern:
        "Suyin doesn't notice when I'm silent. She's more alive with her friends."
        "Wei said they used up his body. I thought he was bitter. Now I wonder."
        Still can't articulate systemic shape—just accumulating evidence.
      </gradual_realization>