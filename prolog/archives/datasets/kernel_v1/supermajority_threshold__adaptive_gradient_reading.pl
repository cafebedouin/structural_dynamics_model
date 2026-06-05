% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold as Adaptive Institutional Gradient
 *   domain: constitutional_theory/institutional_design/political_economy
 *
 * SUMMARY:
 *   The supermajority threshold is presented in constitutional law as a
 *   structural device for ensuring that fundamental change reflects genuine
 *   consensus. This narrative instantiates the ADAPTIVE GRADIENT READING: the
 *   legitimacy of the threshold depends not on intrinsic value (e.g.,
 *   'two-thirds reflects natural unanimity') but on empirical calibration to
 *   the rate at which actual consensus forms in the target polity and the
 *   reversibility costs of lock-out vs. implementation. Under this reading, a
 *   threshold set at 67% is legitimate only if consensus on major issues
 *   empirically forms at or above 67%; if consensus typically crystallizes at
 *   55-60%, the threshold becomes ossifying (pushing from tangled_rope into
 *   snare territory). Conversely, if consensus rarely forms below 75%, a 67%
 *   threshold is under-protective. The reading treats supermajority as a
 *   functional institutional gradient requiring evidence-based tuning, not as
 *   a fixed principle. This differs fundamentally from the
 *   consensus_safeguard_reading (threshold as inherent protection against
 *   hasty majorities, legitimacy intrinsic) and the minoritarian_veto_reading
 *   (threshold as inherent protection against majoritarian excess, legitimacy
 *   intrinsic). The adaptive reading grounds legitimacy in measured
 *   performance — an empirical, revisable foundation — rather than in
 *   constitutional principle or philosophical necessity. The constraint
 *   exhibits extraction because constitutional designers who establish
 *   thresholds extract value from their authority to set institutional rules;
 *   rapid-adapting coalitions bear extraction costs when their majority
 *   support cannot overcome the gate.
 *
 * KEY AGENTS:
 *   - Constitutional Designers / Legitimacy Engineers: Institutional actors (institutional/arbitrage) — extract institutional authority to set threshold rules; benefit from stability mechanism; experience constraint as pure coordination
 *   - Locked-Out Majority: Policy coalitions commanding 55-65% support (powerless/trapped) — cannot implement policy when supermajority required; bear maximum extraction; have no exit
 *   - Swing Factions: Coalitions at 55-67% support (moderate/constrained) — experience mixed coordination (threshold slows flip-flop) and extraction (gatekeeping by minorities); constrained by electoral lock-in
 *   - Reform Coalition: Organized groups (organized/constrained) — propose dynamic threshold adjustment based on evidence; see current threshold as temporary coordination failure with exit path
 *   - Constitutional Inertia Mechanism: Institutional persistence (institutional/arbitrage) — maintains threshold through tradition despite minimal empirical justification; institutional legitimacy decoupled from functional verification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit naturalization unless the constraint is understood as calibrable tool rather than law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.52).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.48).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold as Adaptive Institutional Gradient").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/institutional_design/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, 'c947e1cb-4667-4572-b24a-a75a1efda65e').
narrative_ontology:cs_kernel_codification('c947e1cb-4667-4572-b24a-a75a1efda65e', formalized).
narrative_ontology:cs_authority_grounding('c947e1cb-4667-4572-b24a-a75a1efda65e', lineage).
narrative_ontology:cs_interpretation_layer_present('c947e1cb-4667-4572-b24a-a75a1efda65e').
narrative_ontology:cs_reading_relation('c947e1cb-4667-4572-b24a-a75a1efda65e', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('c947e1cb-4667-4572-b24a-a75a1efda65e', supermajority_threshold__minoritarian_veto_reading, influences).
narrative_ontology:cs_axiom('c947e1cb-4667-4572-b24a-a75a1efda65e', foundational, threshold_legitimacy_empirically_contingent).
narrative_ontology:cs_axiom_status(threshold_legitimacy_empirically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('c947e1cb-4667-4572-b24a-a75a1efda65e', threshold_legitimacy_empirically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('c947e1cb-4667-4572-b24a-a75a1efda65e', foundational, reversibility_cost_asymmetry_matters).
narrative_ontology:cs_axiom_status(reversibility_cost_asymmetry_matters, holdable).
narrative_ontology:cs_axiom_grounding('c947e1cb-4667-4572-b24a-a75a1efda65e', reversibility_cost_asymmetry_matters, empirically_contingent).
narrative_ontology:cs_reference_frame('c947e1cb-4667-4572-b24a-a75a1efda65e', empirically_calibrated_institutional_gradient).
narrative_ontology:cs_drift_state('c947e1cb-4667-4572-b24a-a75a1efda65e', contemporary_polarization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c947e1cb-4667-4572-b24a-a75a1efda65e', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, institutional_designers).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, constraint_stabilizers).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, rapid_adapters).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, majoritarian_coalitions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-OUT MAJORITY (SNARE) — A coalition commanding 55-65% support cannot implement policy when supermajority (67%+) is required. No exit: they remain citizens subject to the threshold. Cannot organize above threshold without external conversion. Maximum experienced extraction from structural position.
constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SWING FACTION (TANGLED ROPE) — Groups at 55-67% support occupy mixed position: they benefit from threshold serving as coordination anchor (slowing flip-flop policies), but pay extraction cost of gate-keeping by minority coalitions. Exit constrained by political geography and electoral lock-in. Mixed classification reflects genuine coordination function plus asymmetric extraction.
constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL DESIGNER (ROPE) — Benefits from threshold serving as stability mechanism. Extracts value from institutional authority to set thresholds. Sees constraint as pure coordination: threshold solves the consensus-signaling problem. High arbitrage (can write the threshold rule itself). Low experienced extraction.
constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents proposing dynamic threshold adjustment (e.g., declining supermajority as issue changes) see the current threshold as a temporary coordination failure. Exit path: evidence-based threshold tuning that adjusts cost-benefit trade-off over time. Has agency through policy design but constrained by entrenched constitutional processes.
constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL INERTIA MECHANISM (PITON) — Viewed from civilizational timescale, supermajority thresholds are largely theatrical expressions of 'we take fundamental change seriously,' divorced from actual calibration to consensus formation rates in the polity. The mechanism persists through institutional legitimacy (framers' intent, constitutional tradition) despite minimal functional verification. Theater ratio reflects that threshold level is rarely revisited or empirically justified.
constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FALSE SUMMIT (MOUNTAIN REJECTED) — A competing naturalized framing claims supermajority emerges inevitably from the logic of collective decision-making: 'any change affecting the whole must reflect overwhelming consensus.' This reading treats threshold as immutable natural law rather than contingent design choice. This story rejects that framing — empirical instantiation of adaptive_gradient_reading reveals the threshold as a calibrable institutional tool, not a natural limit. The engine's false summit detector identifies this.
constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supermajority_threshold__adaptive_gradient_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, TR),
    TR >= 0.70.

:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The threshold extracts from rapid-adapting majorities by imposing a gate; constitutional designers extract authority value by setting rules; but significant coordination function exists (slowing policy flip-flop, signaling consensus requirement). The measurement trajectory shows rising extractiveness over 30 years, reflecting institutional capture: initial threshold (0.38) was calibrated to historical consensus rates; as consensus formation rates changed (polarization, demographic shifts), the threshold became increasingly misaligned, pushing toward ossification. Suppression (0.48): Moderate. Structural barriers include constitutional entrenchment (high friction for revision), political geography lock-in, and gate-keeper power of minority coalitions. But suppression is not total — coalition-building remains possible, and reform paths exist (constitutional amendment, judicial reinterpretation). Theater ratio (0.38): Low-moderate. The threshold is substantively enforced (real gate), but rhetoric around it is often disconnected from empirical justification. Much constitutional discourse treats the threshold as intrinsically legitimate rather than instrumentally calibrated, producing performative justification (appeals to 'framers' intent,' 'consensus requirement,' 'fundamental change') without empirical grounding.
 *
 * PERSPECTIVAL GAP:
 *   The core gap: constitutional designers see pure coordination (Rope) because the threshold solves their legitimacy problem; locked-out majorities see pure extraction (Snare) because they bear a gate with no exit; reform coalitions see a temporary coordination failure with a sunset (Scaffold) because empirical calibration is technically feasible but institutionally blocked; the civilizational view risks false summit (Mountain) by naturalizing the threshold as immutable. The gap reveals that the threshold's classification depends entirely on whether one treats the gate-keeping function as coordinating (slowing policy change) or extracting (blocking majority will). Adaptive gradient reading commits to measuring performance to resolve this gap: if consensus forms at 55%, the gate-keeping function is extraction (snare); if at 75%, it is coordination (rope). The reading operationalizes the gap as an empirical question, not a philosophical one.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from their structural position relative to the gate-keeping function. Constitutional designers benefit from the authority to set rules (d ≈ 0.15, beneficiary + arbitrage); locked-out majorities experience maximum extraction (d ≈ 0.95, victim + trapped); swing factions occupy intermediate position (d ≈ 0.60, both victim and beneficiary depending on issue, constrained exit). The piton classification reflects high institutional inertia (theater ≥ 0.70 regime boundary approached as performative justification dominates). The snare classification from the locked-out majority perspective reflects maximum structural entrapment. The scaffold classification from reform coalition reflects a real exit path through evidence-based recalibration.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by rejecting both the false summit (mountain) and the false coordination (pure rope) framings. The constraint is genuinely tangled rope: it coordinates (slows policy flip-flop, signals consensus requirement) AND extracts (blocks majority will when consensus fails to reach the set threshold). The measured extractiveness (0.52) reflects this hybrid function. Mandatrophy is resolved by the commitment to empirical calibration: if the threshold is misaligned to actual consensus formation, the tangled rope becomes increasingly snare-like (extractiveness rises in measurements as polarization increases, pushing from 0.38 to 0.52). The resolution path is recalibration, not reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_formation_rate_measurement,
    'What rate of actual consensus formation (measured by longitudinal polling, legislative voting patterns, or citizen deliberation) should trigger threshold recalibration? At what speed does consensus form in THIS polity?',
    'Empirical longitudinal study: track consensus formation trajectories for major policy shifts (civil rights, environmental, healthcare, fiscal policy); correlate with supermajority requirement success/failure; identify empirical consensus floor where threshold becomes functional vs. ossifying.',
    'If consensus typically forms at 55-62%: current threshold (67%+) is extractive ossification, snare. If at 75%+: current threshold is reasonable coordination tool, rope. Threshold''s legitimacy hinges on calibration to actual rate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_formation_rate_measurement, empirical, 'Measured consensus formation rate in target polity').

omega_variable(
    reversibility_cost_asymmetry,
    'Is the cost of reversing a majoritarian policy (when threshold not met) symmetric to the cost of achieving it? Or does the gate-keeping function impose asymmetric reversal costs?',
    'Comparative analysis: cost to implement policy with majority support vs. cost to reverse policy implemented with same threshold. Includes institutional friction, political capital, agenda prioritization, irreversibility of effects.',
    'If symmetric: threshold is pure coordination (rope from institutional perspective, snare from locked-out majority). If asymmetric (reversal more costly): threshold extracts from majority by imposing reversibility barrier, elevating to snare territory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_cost_asymmetry, empirical, 'Asymmetry between forward and reverse implementation costs').

omega_variable(
    competing_readings_simultaneity,
    'Can consensus_safeguard_reading and minoritarian_veto_reading coexist in the same constitutional framework, or does adaptive_gradient_reading''s commitment to empirical calibration foreclose at least one sibling?',
    'Examine whether a polity can simultaneously treat threshold as a safeguard against hasty majorities (safeguard framing) and as a protection against majoritarian excess (veto framing), while also committing to evidence-based tuning. Map the axiomatic tensions: empirical calibration requires abandoning the claim that any fixed threshold is intrinsically legitimate.',
    'If readings coexist: three live readings in contemporary constitutional practice, framework can hold all. If empirical tuning forecloses one sibling: adaptive_gradient forecloses the static natural-law reading in any single framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_readings_simultaneity, conceptual, 'Whether adaptive_gradient and sibling readings can coexist in single constitutional framework').

omega_variable(
    empirical_calibration_authority,
    'Who has institutional authority to recalibrate the threshold based on empirical consensus data? Constitutional convention, elected majority, independent commission, academic expertise?',
    'Institutional design audit: identify which actor(s) can invoke empirical evidence to change the threshold. Compare to how other constitutional parameters (e.g., voting age, district apportionment) are adjusted. Document resistance patterns and whose interests require high institutional friction for change.',
    'If no recalibration mechanism exists: adaptive_gradient reading is aspirational/false summit (piton). If mechanism exists: reading is structurally viable. If mechanism captured: mechanism itself becomes snare for majority interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_calibration_authority, empirical, 'Authority and accessibility of empirical threshold recalibration mechanism').

omega_variable(
    threshold_as_kernel_reading_contest,
    'This is one reading of the supermajority_threshold kernel. What makes adaptive_gradient_reading distinct from its siblings, and can all three coexist as live commitments in contemporary constitutional democracies?',
    'Comparative constitutional analysis: which democracies instantiate safeguard reading (threshold as protection against hasty change)? Which instantiate veto reading (threshold as protection for minorities)? Which instantiate adaptive reading (threshold as empirically calibrated tool)? Are these three readings segregated across different polities, or do some polities hold multiple readings simultaneously?',
    'If segregated: readings are alternative trajectories, not truly coexisting. If simultaneous: underlying tension may be unresolved in constitutional practice. If adaptive reading is rare: it may be epistemically marginalized or institutionally blocked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_as_kernel_reading_contest, conceptual, 'Kernel reading coexistence and geographical/temporal distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smaj_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(smaj_tr_t15, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(smaj_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(smaj_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(smaj_be_t15, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(smaj_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(smaj_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(smaj_su_t15, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(smaj_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, constitutional_amendment_friction).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% Supermajority threshold is one kernel with three readings: adaptive_gradient_reading (this story), consensus_safeguard_reading, minoritarian_veto_reading. Each reading has different ε (consensus_safeguard emphasizes coordination ~0.30, minoritarian_veto emphasizes extraction ~0.65, adaptive_gradient balances both ~0.52) because each reading measures different structural properties of the same institutional rule. The readings are linked via reading_relations in cs_structure to show their relationship (coexists_with vs forecloses vs influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
