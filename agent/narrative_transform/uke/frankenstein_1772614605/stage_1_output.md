# STAGE 1: FORMALIZATION — FRANKENSTEIN CONSTRAINT NETWORK

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Victor Frankenstein
X₂ ← The Creature
X₃ ← Robert Walton
X₄ ← Elizabeth Lavenza
X₅ ← Henry Clerval
X₆ ← Justine Moritz
X₇ ← Alphonse Frankenstein
G₁ ← De Lacey Family
G₂ ← Society (general enforcement body)
I₁ ← Scientific Community (implicit institution)
I₂ ← Legal System
C₁ ← Ambition constraint
C₂ ← Creation duty constraint
C₃ ← Appearance-based exclusion constraint
C₄ ← Reputation preservation constraint
C₅ ← Revenge cycle constraint
```

---

## I. CONSTRAINT FORMALIZATIONS

### C₁: Ambition Constraint

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.20</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>              <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>national</scope>              <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 1.0 = 0.80</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.80 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      High extraction from personal resources (health, relationships, safety).
      Identity-locked exit: cognitive fusion between self-concept and pursuit.
      No genuine coordination value — purely extractive drive.
      Asymmetric: benefits accrue to abstract "glory," costs borne by agent.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>              <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>continental</scope>           <!-- σ = 1.1 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 1.1 = 0.88</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.88 > 0.70 → PASS</threshold_check>
    <rationale>
      Scope amplification: pursuit operates at continental scale (Arctic expedition).
      Verification difficulty increases extraction (fewer witnesses to consequences).
      Same identity-lock mechanism as X₁.
    </rationale>
  </agent>

  <agent ref="X₇">
    <index>
      <power>powerful</power>              <!-- π = 0.6 -->
      <time>generational</time>
      <exit>mobile</exit>
      <scope>regional</scope>              <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 0.6 × 0.9 = 0.43</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ = 0.43 ≤ 0.70 → FAIL (below tangled_chi_floor)</threshold_check>
    <type_correction>Rope (χ ≤ 0.35 ceiling violated, but ε = 0.80 > 0.45 → Naturalized)</type_correction>
    <rationale>
      Power position deflects extraction: can encourage pursuit without bearing costs.
      Generational horizon: sees ambition as family legacy mechanism.
      Mobile exit: not identity-locked to specific achievement.
      Classification ambiguity signals power-scaling hiding extraction.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.80 → Snare, X₃ χ=0.88 → Snare, X₇ χ=0.43 → Naturalized
    Divergence: Power position transforms experience of same constraint.
    X₁ and X₃ bear full extraction; X₇ benefits from their pursuit while avoiding costs.
    Naturalized classification for X₇ indicates extraction absorption by privilege.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by power position (π).
    X₁/X₃ experience Snare; X₇ experiences dampened version.
    Constraint is constructed (social value system), not natural law.
  </boltzmann_test>
</constraint>
```

---

### C₂: Creation Duty Constraint

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.80</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>              <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.0 × 0.8 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.72 > 0.70 → PASS</threshold_check>
    <rationale>
      Extreme extraction: duty demands total sacrifice of well-being.
      Trapped exit: cannot undo creation, cannot escape consequences.
      High suppression: enforced through guilt, social expectation, direct threat.
      Local scope dampens slightly (immediate witnesses verify duty).
      No coordination value — purely extractive obligation.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerless</power>             <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 → PASS (extreme)</threshold_check>
    <rationale>
      Powerless position amplifies extraction: no capacity to compel duty fulfillment.
      Trapped by existence itself — cannot exit constraint of being created.
      Experiences constraint as total abandonment combined with social rejection.
      Extraction exceeds 1.0 (bears costs beyond baseline).
    </rationale>
  </agent>

  <agent ref="I₁">
    <index>
      <power>analytical</power>            <!-- π = 1.15 -->
      <time>historical</time>
      <exit>analytical</exit>
      <scope>global</scope>                <!-- σ = 1.2 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.15 × 1.2 = 1.24</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.24 > 0.70 → PASS</threshold_check>
    <rationale>
      Analytical position detects constraint despite not being subject to it.
      Historical horizon reveals pattern: creation without responsibility is extractive.
      Global scope: verification difficulty amplifies (many creators, few consequences visible).
      π = 1.15 breaks degeneracy with moderate position — analyst sees Snare where X₁ might normalize.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.72 → Snare, X₂ χ=1.08 → Snare (extreme), I₁ χ=1.24 → Snare (detected)
    Divergence: All indices classify as Snare, but χ magnitude varies dramatically.
    X₂ bears amplified extraction (powerless position).
    I₁ detects extraction that might be normalized by participants.
    Constraint is universally extractive but asymmetrically distributed.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification stable (Snare across indices), but χ varies by power.
    X₂ experiences 1.5× extraction of X₁ due to powerless position.
    Constraint is constructed (social/ethical obligation), not natural law.
  </boltzmann_test>
</constraint>
```

---

### C₃: Appearance-Based Exclusion Constraint

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.85</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₂">
    <index>
      <power>powerless</power>             <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>              <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.5 × 0.9 = 1.15</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.15 > 0.70 → PASS (extreme)</threshold_check>
    <rationale>
      Extreme extraction: total social exclusion, violent rejection, denial of basic needs.
      Powerless position amplifies: no capacity to resist or escape judgment.
      Identity-locked: cannot change appearance, cannot exit constraint.
      High suppression: enforced through violence, expulsion, denial of resources.
      Coordination value exists for in-group (social cohesion through exclusion) but not for target.
    </rationale>
  </agent>

  <agent ref="X₆">
    <index>
      <power>powerless</power>             <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.5 × 0.8 = 1.02</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.02 > 0.70 → PASS (extreme)</threshold_check>
    <rationale>
      Powerless position: no capacity to defend against false accusation.
      Trapped exit: legal system provides no escape mechanism.
      Immediate horizon: no time to gather evidence or mount defense.
      Local scope slightly dampens (witnesses present) but extraction remains extreme.
      Coordination serves in-group (scapegoat mechanism) at total cost to target.
    </rationale>
  </agent>

  <agent ref="G₂">
    <index>
      <power>institutional</power>         <!-- π = -0.2 -->
      <time>generational</time>
      <exit>mobile</exit>
      <scope>national</scope>              <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.85 × (-0.2) × 1.0 = -0.17</chi>
    <type>Rope</type>
    <threshold_check>χ = -0.17 ≤ 0 (net beneficiary) → ε ceiling bypassed → Rope</threshold_check>
    <rationale>
      Institutional position: net beneficiary of exclusion mechanism.
      Negative χ indicates extraction FROM constraint (social cohesion benefit).
      Generational horizon: sees exclusion as stable coordination mechanism.
      Mobile exit: can adjust enforcement without being subject to it.
      Coordination value genuine from this index (maintains group boundaries).
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₂ χ=1.15 → Snare, X₆ χ=1.02 → Snare, G₂ χ=-0.17 → Rope
    Divergence: EXTREME indexical variance — same constraint is Snare for targets, Rope for enforcers.
    G₂ benefits from mechanism that destroys X₂ and X₆.
    This is the signature of asymmetric coordination: genuine value for in-group, total extraction for out-group.
    Constraint cannot be reformed — it IS the asymmetry.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies dramatically by power position.
    Targets experience Snare (χ > 1.0); enforcers experience Rope (χ < 0).
    Constraint is constructed (social prejudice system), not natural law.
  </boltzmann_test>
</constraint>
```

---

### C₄: Reputation Preservation Constraint

```xml
<constraint id="C₄">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.95</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>              <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>regional</scope>              <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.0 × 0.9 = 0.81</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.81 > 0.70 → PASS</threshold_check>
    <rationale>
      Extreme extraction: silence costs innocent lives (X₆, X₄).
      High suppression: enforced through shame, social death threat, family honor.
      Constrained exit: could confess at high cost (social annihilation).
      Coordination value exists (reputation system maintains social order) but asymmetrically distributed.
      From X₁'s index: extraction dominates coordination value.
    </rationale>
  </agent>

  <agent ref="X₆">
    <index>
      <power>powerless</power>             <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 → PASS (extreme)</threshold_check>
    <rationale>
      Powerless position: no capacity to defend reputation or reveal truth.
      Trapped exit: legal system offers no mechanism for exoneration.
      Immediate horizon: no time to gather evidence.
      Bears total cost of X₁'s reputation preservation (execution).
      Coordination value serves others at total extraction from target.
    </rationale>
  </agent>

  <agent ref="G₂">
    <index>
      <power>institutional</power>         <!-- π = -0.2 -->
      <time>generational</time>
      <exit>mobile</exit>
      <scope>national</scope>              <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.90 × (-0.2) × 1.0 = -0.18</chi>
    <type>Rope</type>
    <threshold_check>χ = -0.18 ≤ 0 → ε ceiling bypassed → Rope</threshold_check>
    <rationale>
      Institutional position: benefits from reputation system (social order maintenance).
      Negative χ: extracts coordination value without bearing costs.
      Generational horizon: sees reputation as stable governance mechanism.
      Mobile exit: can adjust enforcement without being subject to it.
      Genuine coordination from this index (prevents chaos through accountability).
    </rationale>
  </agent>

  <indexical_variance>
    C₄: X₁ χ=0.81 → Snare, X₆ χ=1.08 → Snare (extreme), G₂ χ=-0.18 → Rope
    Divergence: Extreme asymmetry — targets bear total extraction, institution benefits.
    X₁ trapped by constraint he could theoretically exit (confession).
    X₆ bears ultimate cost (death) of X₁'s constraint.
    G₂ benefits from mechanism that kills X₆.
    Coordination value is real but asymmetrically distributed — classic Tangled Rope structure, but χ pushes targets into Snare range.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by power position.
    Targets experience Snare; institution experiences Rope.
    Constraint is constructed (reputation system), not natural law.
  </boltzmann_test>
</constraint>
```

---

### C₅: Revenge Cycle Constraint

```xml
<constraint id="C₅">
  <properties>
    <epsilon>1.00</epsilon>
    <suppression>0.10</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>              <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.0 × 0.8 = 0.80</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.80 > 0.70 → PASS</threshold_check>
    <rationale>
      Maximum extraction: total consumption of remaining life, relationships, purpose.
      Identity-locked: self-concept fused with pursuit of revenge.
      Low suppression: self-enforcing (each act of revenge triggers counter-revenge).
      Local scope dampens slightly (immediate witnesses to cycle).
      Zero coordination value — purely destructive feedback loop.
      Asymmetric: each party believes they are responding to aggression, not initiating.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerless</power>             <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.20 > 0.70 → PASS (extreme)</threshold_check>
    <rationale>
      Powerless position amplifies extraction: no alternative means of redress.
      Identity-locked: revenge becomes sole remaining purpose after total social rejection.
      Maximum base extraction: consumes all remaining agency.
      Local scope: witnesses present but powerless to intervene.
      Self-enforcing cycle with no exit mechanism.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>              <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>continental</scope>           <!-- σ = 1.1 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.0 × 1.1 = 1.10</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.10 > 0.70 → PASS (extreme)</threshold_check>
    <rationale>
      Mobile exit: could theoretically abandon pursuit, but chooses not to.
      Continental scope amplifies: pursuit operates across vast distances.
      Verification difficulty increases (fewer witnesses to consequences).
      Maximum extraction despite mobile position — indicates identity-lock overriding exit capacity.
      Observes X₁'s cycle, risks replicating pattern.
    </rationale>
  </agent>

  <indexical_variance>
    C₅: X₁ χ=0.80 → Snare, X₂ χ=1.20 → Snare (extreme), X₃ χ=1.10 → Snare (extreme)
    Divergence: All indices classify as Snare, but χ varies by power and scope.
    X₂ bears maximum extraction (powerless + identity-locked).
    X₃ experiences amplified extraction despite mobile exit (scope effect).
    Constraint is universally destructive — no index reveals coordination value.
    Self-reinforcing cycle with no natural termination point.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification stable (Snare), but χ varies by power position.
    X₂ experiences 1.5× extraction of X₁ due to powerless position.
    Constraint is constructed (social revenge logic), not natural law.
    Low suppression indicates self-enforcement, not natural emergence.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Ambition Pursuit → Creation Act

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>pursue_forbidden_knowledge</condition>
    <target>C₁</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), E = identity_locked</from>
    <to>P = moderate (π=1.0), E = trapped</to>
  </index_change>
  <chi_recalculation>
    <before>C₁: χ = 0.80 → Snare</before>
    <after>C₂: χ = 0.72 → Snare (new constraint activated)</after>
  </chi_recalculation>
  <type_change>Snare (C₁) → Snare (C₂), exit degradation (identity_locked → trapped)</type_change>
  <preconditions>
    - C₁ active (ambition constraint)
    - No institutional oversight (absence: a_ethical_oversight)
    - Technical capacity present
  </preconditions>
  <blocked_by>None — absence of ethical framework means no structural barrier</blocked_by>
  <consequence>
    Activates C₂ (creation duty constraint).
    Exit option degrades: identity_locked → trapped (cannot undo creation).
    Constraint cascade: C₁ feeds into C₂, C₂ feeds into C₃, C₄, C₅.
  </consequence>
</transformation_rule>
```

---

### TR₂: Creation Abandonment → Appearance Constraint Activation

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>abandon_created_entity</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>X₂: P = powerless (π=1.5), E = trapped, S = local (σ=0.8)</from>
    <to>X₂: P = powerless (π=1.5), E = identity_locked, S = regional (σ=0.9)</to>
  </index_change>
  <chi_recalculation>
    <before>C₂ (X₂): χ = 1.08 → Snare</before>
    <after>C₃ (X₂): χ = 1.15 → Snare (new constraint activated)</after>
  </chi_recalculation>
  <type_change>Snare (C₂) → Snare (C₃), scope expansion (local → regional)</type_change>
  <preconditions>
    - C₂ active (creation duty constraint)
    - X₁ has power to abandon (moderate position)
    - X₂ has no alternative support structure
  </preconditions>
  <blocked_by>None — no institutional mechanism to prevent abandonment</blocked_by>
  <consequence>
    Activates C₃ (appearance-based exclusion) for X₂.
    Scope expands: X₂ must navigate broader social terrain.
    Exit degrades: trapped → identity_locked (cannot change appearance).
    χ increases from 1.08 to 1.15 (extraction amplification).
    Feeds into C₅ (revenge cycle) — abandonment creates grievance.
  </consequence>
</transformation_rule>
```

---

### TR₃: Silence Under Reputation Constraint → Innocent Execution

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>maintain_silence_despite_knowledge</condition>
    <target>C₄</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>X₆: P = powerless (π=1.5), E = trapped, T = immediate</from>
    <to>X₆: [constraint terminal — agent eliminated]</to>
  </index_change>
  <chi_recalculation>
    <before>C₄ (X₁): χ = 0.81 → Snare</before>
    <after>C₄ (X₁): χ = 0.81 → Snare (unchanged, but consequence realized)</after>
    <before>C₄ (X₆): χ = 1.08 → Snare</before>
    <after>C₄ (X₆): [constraint runs to completion]</after>
  </chi_recalculation>
  <type_change>Snare (maintained) → Deterministic Tragedy (terminal state)</type_change>
  <preconditions>
    - C₄ active (reputation preservation constraint)
    - X₁ has knowledge that would exonerate X₆
    - X₁ values reputation above X₆'s life (χ calculation confirms extraction dominates)
    - I₂ (legal system) has no mechanism for truth discovery
  </preconditions>
  <blocked_by>None — no structural mechanism to compel confession or discover truth</blocked_by>
  <consequence>
    X₆ eliminated (execution).
    C₄ persists for X₁ (reputation preserved at cost of innocent life).
    Feeds into C₅ (revenge cycle) — X₂ witnesses injustice, adds to grievance.
    Demonstrates asymmetric coordination: G₂ benefits (social order maintained), X₆ bears total cost.
    No transformation of constraint itself — runs to completion.
  </consequence>
</transformation_rule>
```

---

### TR₄: Revenge Act → Cycle Escalation

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>inflict_harm_in_retaliation</condition>
    <target>C₅</target>
    <agent>X₂</agent>
  </trigger>
  <index_change>
    <from>X₁: P = moderate (π=1.0), E = identity_locked</from>
    <to>X₁: P = moderate (π=1.0), E = identity_locked (no change, but cycle reinforced)</to>
  </index_change>
  <chi_recalculation>
    <before>C₅ (X₁): χ = 0.80 → Snare</before>
    <after>C₅ (X₁): χ = 0.80 → Snare (unchanged, but intensity increased)</after>
    <before>C₅ (X₂): χ = 1.20 → Snare</before>
    <after>C₅ (X₂): χ = 1.20 → Snare (unchanged, but cycle reinforced)</after>
  </chi_recalculation>
  <type_change>Snare → Snare (self-reinforcing, no type change)</type_change>
  <preconditions>
    - C₅ active (revenge cycle constraint)
    - Prior harm inflicted by X₁ (trigger for retaliation)
    - No alternative redress mechanism available
  </preconditions>
  <blocked_by>None — low suppression means self-enforcing</blocked_by>
  <consequence>
    Cycle reinforces: each act of revenge triggers counter-revenge.
    No χ change (already at maximum extraction for both agents).
    No exit mechanism emerges — identity-lock persists.
    Feeds back into itself: C₅ → C₅ (recursive).
    Tends toward mutual destruction (Deterministic Tragedy).
    Each agent perceives self as responding, not initiating (asymmetry in perception).
  </consequence>
</transformation_rule>
```

---

### TR₅: Witness Observation → Potential Cycle Replication

```xml
<transformation_rule id="TR₅">
  <trigger>
    <condition>observe_destructive_cycle</condition>
    <target>C₅</target>
    <agent>X₃</agent>
  </trigger>
  <index_change>
    <from>X₃: P = moderate (π=1.0), E = mobile, T = biographical</from>
    <to>X₃: P = moderate (π=1.0), E = constrained, T = biographical</to>
  </index_change>
  <chi_recalculation>
    <before>C₅ (X₃): χ = 1.10 → Snare (potential)</before>
    <after>C₅ (X₃): χ = 1.10 → Snare (activated if pursuit continues)</after>
  </chi_recalculation>
  <type_change>Snare (potential) → Snare (activated), exit degradation (mobile → constrained)</type_change>
  <preconditions>
    - X₃ observes X₁-X₂ cycle
    - X₃ shares C₁ (ambition constraint) with X₁
    - X₃ has mobile exit (can abandon pursuit)
  </preconditions>
  <blocked_by>
    - Recognition of pattern (requires analytical capacity)
    - Alternative purpose available (exit mechanism)
  </blocked_by>
  <consequence>
    Exit option degrades: mobile → constrained (psychological commitment to pursuit).
    If X₃ continues pursuit despite observation, replicates X₁'s trajectory.
    C₁ (ambition) → C₅ (revenge) pathway becomes visible.
    Demonstrates constraint contagion: observation of cycle risks replication.
    Seeded Possibility: X₃ could break pattern if recognition occurs before identity-lock.
  </consequence>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### E₁: False Mountain (Creation Duty)

```xml
<error id="E₁">
  <type>Type I — False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (χ = 0.72, changeable through confession/responsibility)</actual_type>
  <perceived_type>Mountain (unchangeable obligation)</perceived_type>
  <observable>
    Does not attempt to fulfill duty despite capacity to do so.
    Treats abandonment as inevitable rather than chosen