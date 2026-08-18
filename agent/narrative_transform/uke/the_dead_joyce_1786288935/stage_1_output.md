# STAGE 1: FORMALIZATION OUTPUT

## Variable Mapping (header only — stripped upon anonymization)

```
X₁ ← Gabriel Conroy
X₂ ← Gretta Conroy
X₃ ← Lily
X₄ ← Miss Kate & Miss Julia Morkan (aggregated as G₁)
C₁ ← "The Unalterable Past"
C₂ ← "Compulsory Social Performance"
C₃ ← "Presumed Marital Dominion"
C₄ ← "Political Identity Test" (deferred)
C₅ ← "Polite Insolvency" (deferred)
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.10</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>universal</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.0 × 1.0 = 0.10</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 (PASS: 0.10) ∧ Supp ≤ 0.05 (PASS: 0.00)</threshold_check>
    <rationale>Zero suppression, minimal extraction, exit option trapped by time horizon (biographical scale cannot reach the fact backward). No enforcement mechanism required for this constraint to bind.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>universal</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.5 × 1.0 = 0.15</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 (PASS: 0.10) ∧ Supp ≤ 0.05 (PASS: 0.00)</threshold_check>
    <rationale>Mountain type does not gate on χ. Higher χ than X1 reflects greater power-scaled salience but does not alter type — classification depends on ε/Supp/immutability, not on power-weighted experience.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=0.10 → Mountain, X2 χ=0.15 → Mountain
    No divergence in type. Both indices independent of power position for classification purposes — confirms candidate Mountain status pending Boltzmann test.
  </indexical_variance>

  <boltzmann_test>
    PASS. Type(C1, I) invariant under variation of P (moderate → powerless: 1.0 → 1.5 π) and S (only universal tested; scope-invariance is a defining property of this constraint by construction — natural fact does not compress or expand with observer scale). No non-factorizable dependency detected. Classifies as structurally natural, not constructed.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.70</epsilon>
    <suppression>0.65</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.0 × 0.8 = 0.56</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 < χ ≤ 0.90 (PASS: 0.56) ∧ ε ≥ 0.30 (PASS) ∧ Supp ≥ 0.40 (PASS: 0.65) ∧ Coord=true ∧ Asym=true</threshold_check>
    <rationale>Genuine coordination function (assembles the collective) combined with asymmetric enforcement cost. Active enforcement present (social monitoring, role expectation). Sits within strict Tangled band, not at Rope ceiling.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.5 × 0.8 = 0.84</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 (PASS: 0.84) ∧ ε ≥ 0.46 (PASS: 0.70) ∧ Supp ≥ 0.60 (PASS: 0.65) ∧ SnareImmutability (PASS: moderate-index sees Rope/Tangled)</threshold_check>
    <rationale>Same base constraint, powerless index amplifies χ past Snare floor. Exit option trapped (economic dependency), no alternative structural position from which to renegotiate role within this event-frame.</rationale>
  </agent>

  <agent ref="G1">
    <index>
      <power>moderate</power>            <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.0 × 0.8 = 0.56</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 < χ ≤ 0.90 (PASS: 0.56) ∧ ε ≥ 0.30 (PASS) ∧ Supp ≥ 0.40 (PASS)</threshold_check>
    <rationale>As institutional hosts, coordination benefit (successful gathering) offsets extraction cost, but role rigidity (identity_locked exit) prevents pure Rope classification.</rationale>
  </agent>

  <indexical_variance>
    C2: X1 χ=0.56 → Tangled Rope, X3 χ=0.84 → Snare, G1 χ=0.56 → Tangled Rope
    Divergence driven entirely by power position (π: 1.0 vs 1.5). Same base ε/Supp/Coord/Asym, different lived extraction. Textbook Type III risk zone (Snare-as-Rope) if evaluated only from X1/G1 index.
  </indexical_variance>

  <boltzmann_test>N/A — constraint fails Mountain candidacy on ε and Supp thresholds alone (ε=0.70 > 0.25 ceiling). Not tested for naturalness; classified as constructed by definition.</boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.95</epsilon>
    <suppression>0.75</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>institutional</power>       <!-- π = −0.2 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.95 × (−0.2) × 0.8 = −0.152 ≈ −0.15</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0 → ε-ceiling bypassed (χ negative, net beneficiary) ∧ χ ≤ 0.35 (PASS) ∧ Changeable(C, biographical, identity_locked) (PASS — social structure, not natural law)</threshold_check>
    <rationale>Institutional power position yields negative χ: net extraction flows toward this agent, not from. Classifies as coordination mechanism from this index despite high base ε — the ε ceiling is bypassed precisely because this agent is a beneficiary rather than a bearer of the constraint's cost.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                 <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.95 × 1.5 × 0.8 = 1.14</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 (PASS: 1.14) ∧ ε ≥ 0.46 (PASS: 0.95) ∧ Supp ≥ 0.60 (PASS: 0.75) ∧ SnareImmutability (PASS: institutional-index perceives Rope)</threshold_check>
    <rationale>Maximal divergence in the network. High base extraction combined with powerless-index amplification produces χ far past Snare floor. Exit option trapped: no structural alternative available within the marital frame as authored. SnareImmutability gate satisfied — X1's index explicitly perceives Rope for the same constraint.</rationale>
  </agent>

  <indexical_variance>
    C3: X1 χ=−0.15 → Rope, X2 χ=1.14 → Snare
    Maximal indexical divergence in the network — same constraint, opposite sign of χ. This is the canonical Rope/Snare split predicted by institutional vs. powerless power positions (π = −0.2 vs 1.5), demonstrating that a coordination mechanism from one structural position is an extraction trap from another.
  </indexical_variance>

  <boltzmann_test>N/A — ε=0.95, Supp=0.75 both far exceed Mountain ceilings. Excluded from Mountain candidacy without further test; classified as constructed.</boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>agent_discloses_untranslatable_prior_claim</condition>
    <target>C3</target>
    <agent>X2</agent>
  </trigger>
  <index_change>
    <from>E = identity_locked (as perceived by X1)</from>
    <to>E = analytical (X2's interior state revealed as autonomous, pre-existing, non-transactional)</to>
  </index_change>
  <chi_recalculation>
    <before>χ(X2) = 0.95 × 1.5 × 0.8 = 1.14 → Snare</before>
    <after>χ(X2, post-disclosure) = 1.14 [unchanged for X2 — X2's structural position does not shift]</after>
  </chi_recalculation>
  <type_change>No type change for X2. Effect is entirely on X1's index.</type_change>
  <preconditions>C1 (Mountain) must irrupt into frame; disclosure must be involuntary/unprompted by C2's social performance logic.</preconditions>
  <blocked_by>C2 (Compulsory Social Performance) suppresses this disclosure as long as active — TR1 can only fire once C2's local enforcement window closes (post-event, private frame).</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>agent_confronts_scope_of_own_index</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = institutional (π = −0.2), S = local (σ = 0.8)</from>
    <to>P = analytical (π = 1.15), S = universal (σ = 1.0) [candidate — see Omega]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.95 × (−0.2) × 0.8 = −0.15 → Rope</before>
    <after>χ = 0.95 × 1.15 × 1.0 = 1.09 → Snare</after>
  </chi_recalculation>
  <type_change>Rope → Snare (from X1's own re-indexed position)</type_change>
  <preconditions>TR1 must fire first. X1 must abandon institutional-power self-model and adopt analytical exit option — i.e., recognize prior "coordination mechanism" framing as extraction that was merely unfelt from his own index.</preconditions>
  <blocked_by>Ego-inflation sustained by C2's success (undamaged institutional self-model). If C2 remains classified by X1 as pure Tangled Rope without residue, TR2 cannot trigger — X1 lacks motive to re-index.</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>constraint_network_irrupted_by_mountain</condition>
    <target>C2, C3 (jointly)</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>T = biographical (local social frame treated as primary reality)</from>
    <to>T = universal (C1's time horizon absorbed into X1's frame)</to>
  </index_change>
  <chi_recalculation>
    <before>χ(C2, X1) = 0.56 → Tangled Rope; χ(C3, X1) = −0.15 → Rope</before>
    <after>Both recalculated at T=universal: extractiveness ε unchanged (structural property), but Changeable(C, T=universal, E) → FALSE for both — social performance and marital dominion become visibly contingent/trivial against C1's scope, but do NOT reclassify as Mountain (still fail ε/Supp gates; only their *significance*, not their formal type, is altered).</after>
  </chi_recalculation>
  <type_change>No forced type change under the six-type taxonomy — this transformation operates on significance/salience, not on ε, Supp, Coord, Asym. Flagged as an unresolved zone (see Omega below): whether X1's recognition constitutes genuine re-classification (TR2) or merely aestheticized absorption without index change.</type_change>
  <preconditions>C1 must be foregrounded from background (generation_order 1 → active). Requires narrative event exposing X2's untranslatable interior claim (TR1).</preconditions>
  <blocked_by>Nothing within the network itself — C1 as Mountain is asserted as unconditionally accessible once TR1 fires. The open question is whether X1's P and E actually shift (TR2) or whether T shifts while P/E remain frozen (producing the Omega ambiguity).</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X1</agent>
  <constraint>C3</constraint>
  <actual_type>Snare (at X2's index: χ=1.14)</actual_type>
  <perceived_type>Rope (at X1's own index: χ=−0.15)</perceived_type>
  <observable>X1 acts on the assumption of unrestricted access to X2's interior/affective state without verifying χ at X2's index. Does not check whether X2's compliance reflects coordination-consent or extraction-compulsion.</observable>
  <correction_trigger>Direct exposure to X2's index-specific χ calculation — i.e., disclosure event (TR1) forcing X1 to recompute χ from powerless/trapped position rather than institutional position.</correction_trigger>
</error>

<error id="E2">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X1</agent>
  <constraint>C2</constraint>
  <actual_type>Snare (at X3's index: χ=0.84)</actual_type>
  <perceived_type>Tangled Rope, treated as pure Rope in practice (extraction component unattended)</perceived_type>
  <observable>Does not attempt to renegotiate or flag asymmetric burden on service-position agent despite social proximity and opportunity for observation; treats own χ=0.56 experience as representative of the constraint's overall character.</observable>
  <correction_trigger>Would require X1 to compute χ at X3's index (powerless, trapped, local) rather than generalizing from own moderate-index experience.</correction_trigger>
</error>

<error id="E3">
  <type>Type V.a — Tangled-as-Rope (ignoring extraction component)</type>
  <agent>G1</agent>
  <constraint>C2</constraint>
  <actual_type>Tangled Rope (χ=0.56, both Coord and extraction present)</actual_type>
  <perceived_type>Rope (pure coordination good, extraction cost unacknowledged)</perceived_type>
  <observable>Continues to intensify enforcement of C2 (role performance, hospitality obligation) without adjustment despite Supp=0.65 indicating active, costly maintenance rather than self-sustaining coordination.</observable>
  <correction_trigger>Would require explicit accounting of enforcement cost (Supp) as a debit against the coordination benefit — currently absent from agent's operative model.</correction_trigger>
</error>

<error id="E4" status="unresolved — see Omega">
  <type>Ambiguous: Type II (Mountain Denial) vs. correctly-triggered TR3</type>
  <agent>X1</agent>
  <constraint>C1</constraint>
  <actual_type>Mountain (χ=0.10, confirmed Boltzmann PASS)</actual_type>
  <perceived_type>Underdetermined — either (a) X1 correctly registers C1 as Mountain and reorganizes P/E accordingly [TR2 fires], producing analytical humility; or (b) X1 aestheticizes C1's Mountain-status into new material for identity-performance under C2's logic, treating the unchangeable as a occasion for further self-regarding narrative [Mountain acknowledged in content but re-absorbed into Tangled Rope process].</perceived_type>
  <observable>Terminal state of X1's index (P, E) after TR3 is not determinable from the constraint network alone — the network is compatible with both a genuine π→analytical shift and a merely T→universal shift with P/E unchanged (frozen institutional/identity_locked position redescribing Mountain-content in old structural terms).</observable>
  <correction_trigger>Would require additional index evidence beyond generation_order and χ values — specifically, evidence of whether X1's subsequent actions exhibit E=analytical (capacity to hold C1 without transactional return) or E=identity_locked (incorporating C1 into the same self-regarding performance C2 always demanded). This is the formal locus of the declared Omega.</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Model selected: BOUNDED INSTITUTIONAL RATIONALITY (BIR)

Justification:
- C2 and C3 both exhibit Coord=true with Asym=true — hybrid Tangled Rope
  signatures dominate the active network (2 of 3 constraints), not pure
  Snare-without-Mountain or algorithmic/implacable-system profiles.
- Agents operate under uncertainty about each other's index (X1 does not
  know X2's χ at C3 until TR1 fires) — classic principal-agent /
  information-asymmetry structure, not perfect-information optimization.
- Enforcement (Supp) on C2/C3 is socially/normatively maintained (internalized
  roles, diffuse performance pressure), not algorithmically or
  physically implacable — satisficing agents, not utility-maximizing
  institutions immune to renegotiation.
- C1 alone would suggest PIR-compatible Mountain-dominated determinism,
  but C1 does not act as an enforcement mechanism on the other two
  constraints — it irrupts rather than governs. The active constraint
  network (C2, C3) is what agents actually negotiate within, and both
  are Tangled Rope / Rope-Snare hybrids, which under the Attractor
  Compatibility Matrix require BIR for reachability of non-Tragedy outcomes.

Tendency: Negotiated Equilibrium, Seeded Possibility
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
Constraint profile summary:
  C1: Mountain (background, non-enforcing, universal scope)
  C2: Tangled Rope (X1, G1) / Snare (X3) — mixed hybrid + index-dependent Snare
  C3: Rope (X1) / Snare (X2) — maximal indexical divergence, hybrid-dominant network

Profile classification: Tangled Ropes dominant, with one Mountain in background
  and one embedded Snare (X2 at C3; X3 at C2) not eliminated by network
  resolution.

Attractor Compatibility Matrix lookup:
  "Tangled Ropes dominant" + BIR → Negotiated Equilibrium
  "Piton present" → N/A (no Piton classified in this network)
  "Mountain-dominated" → N/A (C1 is present but not dominant; C2/C3 dominate
    the active agent-experienced network)

Selected attractor: ☑ SEEDED POSSIBILITY

Justification:
- Pure Negotiated Equilibrium is disqualified: C3's Snare-classification at
  X2's index (χ=1.14) does not resolve through bargaining within the
  network as formalized — TR1 discloses but does not renegotiate the
  underlying χ; X2's structural position (trapped, powerless) is
  unchanged after disclosure (see TR1 chi_recalculation: χ unchanged for X2).
- Pure Deterministic Tragedy is disqualified: the profile is not
  Mountain+Snares-without-agency (PIR profile) — BIR is justified above,
  and BIR is incompatible with pure Tragedy per the Matrix.
- Seeded Possibility fits the declared Omega precisely: the surface
  resolution (X1's encounter with C1-as-Mountain) is compatible with
  either continued entrapment in C2's performance-logic (tragic surface)
  or genuine underground re-indexing via TR2 (analytical transformation)
  — and the network as formalized does NOT resolve which occurs. This
  is definitionally the Seeded Possibility signature: tragedy on the
  surface, transformation-potential seeded but unconfirmed underneath.
- The unresolved Error E4 (Mountain Denial vs. correctly-triggered TR3)
  is the formal marker of seeded-but-unconfirmed transformation.

Compatibility with BIR: CONFIRMED (Seeded Possibility is BIR-reachable
per Matrix row "Piton present" generalizes to any hybrid-dominant profile
with unresolved agent-level transformation; more directly, BIR → 
{Negotiated Equilibrium, Seeded Possibility} per §VII, and Negotiated
Equilibrium is excluded above, leaving Seeded Possibility as the
uniquely compatible attractor.)
```

---

## 6. VALIDATION CHECKLIST

```
☑ All constraints (C1, C2, C3) formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with explicit π and σ values
☑ All characters use variable names (X1, X2, X3, G1) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules (TR1–TR3) use abstract trigger descriptions, mechanical χ recalculation
☑ Error observables (E1–E4) are testable structural conditions, not source-specific actions
☑ Institutional rationality model specified (BIR) with explicit justification
☑ Terminal attractor selected (Seeded Possibility), justified, and confirmed compatible with BIR
☑ Indexical variance explicitly preserved across agents for all three constraints
☑ Boltzmann test run for C1 (PASS — Mountain confirmed); N/A correctly declared for C2, C3
☑ Deferred constraints (C4, C5) excluded from formalization — correctly left out of scope
☑ No ambiguity in specifications except the single declared Omega (E4), which is formally
   marked as unresolved rather than silently assumed
☑ Output is pure symbolic network — no reader-identifiable source content present
```