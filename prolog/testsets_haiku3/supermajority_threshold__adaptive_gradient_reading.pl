% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold (Adaptive Gradient Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The supermajority threshold for constitutional amendment is a
 *   foundational institutional rule designed to balance two competing
 *   pathologies: amendment churn (when change is too easy) and constitutional
 *   ossification (when change is too hard). This reading treats the threshold
 *   as a calibrated, performance-dependent constraint whose legitimacy rests
 *   on evidence about actual consensus-formation rates in a specific polity.
 *   The threshold is functional and adaptive: too low a supermajority
 *   requirement enables instability and unprotected transient majorities
 *   imposing changes they cannot defend against later reversal; too high a
 *   requirement ossifies the constitution and blocks necessary reform.
 *   Legitimacy is grounded not in intrinsic democratic principle but in
 *   measurable institutional performance. This reading coexists with two
 *   sibling readings: the consensus-safeguard reading (which treats the
 *   supermajority as intrinsically protective of deep democratic consensus)
 *   and the minoritarian-veto reading (which treats it as entrenching
 *   blocking minorities and converting historical privilege into permanent
 *   veto).
 *
 * KEY AGENTS:
 *   - Consensus coalition builders (organized actors with resources for durable coalition-building; benefit when threshold calibration rewards their work)
 *   - Transient majorities (groups with simple-majority but not supermajority support; bear the blocking cost)
 *   - Reform initiators (political entrepreneurs seeking constitutional change; bear the extra-effort cost of supermajority requirement)
 *   - Institutional incumbents (constitutional court or supermajority bloc; benefit from threshold persistence and control its interpretation)
 *   - Citizens and regions (distributed across all roles; benefit from filtering function, bear costs of delayed reform)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.62).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.58).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '0d1ba0c8-8c7c-4857-be47-549b2d3a21c0').
narrative_ontology:cs_kernel_codification('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', formalized).
narrative_ontology:cs_authority_grounding('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', extraction).
narrative_ontology:cs_interpretation_layer_present('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0').
narrative_ontology:cs_reading_relation('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', foundational, threshold_legitimacy_performance_calibrated).
narrative_ontology:cs_axiom_status(threshold_legitimacy_performance_calibrated, holdable).
narrative_ontology:cs_axiom_grounding('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', threshold_legitimacy_performance_calibrated, empirically_contingent).
narrative_ontology:cs_axiom('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', secondary, evidence_based_amendment_threshold_adjustment).
narrative_ontology:cs_axiom_status(evidence_based_amendment_threshold_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', evidence_based_amendment_threshold_adjustment, instrumental).
narrative_ontology:cs_reference_frame('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', empirically_calibrated_amendment_threshold).
narrative_ontology:cs_drift_state('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', contemporary_ossification_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0d1ba0c8-8c7c-4857-be47-549b2d3a21c0', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, consensus_coalition_builders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, transient_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, reform_initiators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, institutional_incumbents).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, polity_citizens).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, polity_citizens).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, calibrated_institutional_design).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, evidence_based_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actors who benefit from the threshold requirement because they have the time, resources, and social capital to assemble durable coalitions. They benefit from rules that force deliberation and broad agreement. Their interest aligns with the threshold's filtering function when the threshold is calibrated to actual consensus-formation rates. They include political parties with broad constituencies, civil-society coalitions, regional interests that negotiate multi-year amendments.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, consensus_coalition_builders, beneficiary,
    organized, generational, constrained, national).

% Groups that achieve simple majority support for constitutional change but lack the supermajority votes required by the threshold. They must either accept the constraint (no change) or invest years building cross-cutting coalitions. The threshold blocks their preferences even when democratic majorities support the change. They include labor movements seeking constitutional labor rights, environmental movements seeking constitutional protection, regional minorities seeking constitutional recognition when they form simple majorities but not supermajorities.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, transient_majorities, payer,
    moderate, biographical, constrained, national).

% Political entrepreneurs seeking to reshape constitutional rules. The threshold raises the cost of their entry by requiring supermajority agreement. They bear the enforcement cost directly: their proposed changes fail unless they can convince voters across ideological and regional lines. They include constitutional reformers, amendment entrepreneurs, populist movements, and issue-specific coalitions pushing particular constitutional changes.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reform_initiators, payer,
    moderate, biographical, constrained, national).

% The constitutional court or legislative supermajority bloc that interprets and enforces the threshold. They have the power to define what counts as 'supermajority' (simple numerical majority of two-thirds chambers? three-fifths? regional minima?). They benefit from threshold persistence because it limits the rate of constitutional revision and protects their interpretation from rapid override. They include constitutional courts, legislative supermajority blocs, and established constitutional law traditions.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_incumbents, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, institutional_incumbents, beneficiary).

% Benefit from the threshold's filtering function: it prevents constitutional change driven by transient passions and requires deeper deliberation. They also bear costs: when reform is genuinely needed but lacks supermajority support, the threshold delays or blocks improvement. The adaptive reading claims their experience should feedback into recalibration. They are simultaneously beneficiaries (from filtering) and payers (from blocked necessary reforms).
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, polity_citizens, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, polity_citizens, payer).

% Alternative supermajority definitions (simple plurality, consensus governance, delegated expertise review) that would allocate amendment authority differently. Excluded from the formal amendment process because the standing constitutional order selects the current threshold. They remain live proposals in academic and reform discourse but are structurally foreclosed by the standing constitutional commitment to supermajority amendment.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, competing_amendment_frameworks, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__adaptive_gradient_reading, competing_amendment_frameworks).

% The actual observable rate at which citizens and parties form durable agreement on constitutional questions in this specific polity. Neither agent nor stakeholder, but the empirical referent against which threshold legitimacy is measured. This is what the adaptive reading claims the threshold should track. The observable consensus-formation rate is the measuring stick for legitimate threshold calibration.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, consensus_formation_process, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__adaptive_gradient_reading, consensus_formation_process).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, institutional_incumbents).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requires broad coalition-building for constitutional change, ensuring amendments reflect durable consensus rather than temporary factional advantage. Coordinates the pacing of constitutional revision by raising the cost of unilateral change and creating time for secondary deliberation across regions and ideological lines.
% TRANSFER_FUNCTION: Transfers constitutional initiative power from simple majorities to durable supermajorities. Moves the burden of proof from defenders of the status quo (who must block change) to reformers (who must build supermajority support). Extracts time and political capital from reform initiators; distributes delay costs and entrenchment benefits unequally depending on the calibration of the specific threshold.
% ABSENT_VOICES: Voters who form simple majorities but not supermajorities are excluded from amendment efficacy; they lose voice in constitutional amendment while retaining vulnerability to the threshold's blocking effect. Marginalized regions and ideological minorities whose buyoff is required to reach supermajority are included in the formal process but often as veto players rather than equal participants. Alternative threshold designers—those who would argue for lower thresholds (enabling faster reform) or higher ones (enabling stronger blocking minorities)—are excluded by the standing constitutional order.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold disappeared, the amendment process would shift to simple-majority governance. Constitutional change would accelerate. Some reforms that lacked supermajority support would pass; some changes that the current threshold blocks would be adopted. The distribution of constitutional power would flatten toward numerically larger coalitions. The regime's stability equilibrium would change—toward faster institutional drift or entrenchment depending on whether simple majorities consistently align on reform.
% FOUNDING_PROBLEM: Early constitutional orders faced two pathological amendment rates: either amendments were impossibly hard to achieve (constitutional ossification), blocking necessary modernization, or amendments were too easy, enabling destructive constitutional churn. The supermajority threshold was calibrated to thread this needle: raise the amendment bar enough to prevent churn, but not so high as to enable ossification.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative scholars document the empirical equilibria: purely majoritarian systems show measurable churn; purely entrenchment-friendly systems show measurable ossification. The adaptive reading is attested by political scientists studying comparative supermajority thresholds (Lorenz, Negretto, Elkins et al.); independent empirical analysis of amendment rates across jurisdictions with different thresholds supports the claim that legitimacy is performance-calibrated. The standing institutional incumbents attest the founding problem remains live; reform advocates attest it is partially solved but the calibration is mismatched to current consensus-formation rates.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end, rising from 0.48 over the first 20 units then plateauing) because the threshold systematically transfers constitutional initiative power from simple majorities to supermajorities. This is not pure coordination overhead—some filtering is necessary—but the distribution is asymmetric: reform initiators bear concentrated costs, consensus builders and incumbents capture concentrated benefits. Suppression is high (0.58) because the threshold actively excludes simple majorities from amendment efficacy; this exclusion is structural (you need supermajority votes; there is no workaround) rather than internalized. Theater is moderate (0.44 at plateau) because deliberative legitimacy stories circulate about the threshold, but an increasing share of enforcement activity in years 5–20 goes to managing the political cost of blocked reforms rather than the stated filtering function. The time series shows: (1) extractiveness rising as consensus-coalition benefits compound and blocked-reform costs accumulate; (2) plateau at t=25 as political equilibrium stabilizes around the threshold; (3) theater rising as institutional actors invest more effort in rhetorical defense and less in demonstrating filtering value. The measurement series track one shared grid: every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the consensus-builder seat, the threshold is legitimate coordination that prevents churn and rewards serious coalition-work—they see a rope. From the transient-majority seat, the threshold is an unjust veto that blocks their democratic preferences—they see a snare. From the reform-initiator seat, it is a prohibitive cost barrier that they must overcome by assembling redundant coalitions—they see the constraint as asymmetrically extractive. From the institutional-incumbent seat, it is a maintenance object they defend and interpret—they see an agenda they set. The engine computes these divergent seat-level classifications from the structural data (power, exit, beneficiary/victim status); the authored claim (tangled_rope) reflects the reading's own framing that the constraint performs both genuine coordination (filtering) and asymmetric extraction (blocking simple majorities without their consent).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Consensus builders have low directionality (d ≈ 0.25) because they are beneficiaries with substantial power to work with the constraint's logic. Transient majorities have high directionality (d ≈ 0.75) because they are victims with constrained exit and no power to unilaterally change the threshold. Reform initiators have high directionality (d ≈ 0.80) because they bear the enforcement cost directly and are identity-locked into constitutional reform work. Institutional incumbents have low directionality (d ≈ 0.30) because they set the constraint and benefit from its persistence. Citizens have moderate directionality (d ≈ 0.50) because they occupy both beneficiary and payer positions simultaneously. The spatial scope (national) amplifies these values moderately; the engine scales effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptive-gradient reading avoids mandatrophy by grounding legitimacy in measurable performance (evidence-based calibration) rather than in intrinsic democratic principle. The founding problem—balancing churn against ossification—remains live (contested). The constraint's function (filtering vs. blocking) is empirically testable: if consensus-formation rates slow, or reform that lacked supermajority support proves necessary, or ossification increases, these are signals the threshold is miscalibrated and should be adjusted. This reading treats the threshold as a tool with calibration parameters, not as a constitutional absolute. This prevents the legitimacy claim from drifting away from its performance referent: as long as the threshold can be measured against evidence and adjusted, mandatrophy is avoidable. The sibling readings (consensus-safeguard, minoritarian-veto) are more vulnerable to mandatrophy because they ground legitimacy in intrinsic principle (consensus reflection, minority protection) rather than in calibration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_formation_rate_measurement,
    'What is the observable consensus-formation rate in this polity—the actual empirical rate at which durable agreement forms across ideological and regional lines on constitutional questions?',
    'Longitudinal empirical study of amendment proposal trajectories, time-to-passage, coalition composition, and regional/ideological spread. Measure the time and breadth required for sustained support across multiple electoral cycles.',
    'If consensus forms systematically at lower thresholds than the current supermajority requires (e.g., 55% regular broad coalitions), the threshold is miscalibrated high and creates unnecessary ossification. If consensus requires systematically higher thresholds (e.g., 70%+ durable coalitions), the current threshold filters incompletely and risks churn. The adaptive reading claims this measurement determines legitimate threshold calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_formation_rate_measurement, empirical, 'Observable consensus-formation rate in this polity vs. standing threshold.').

omega_variable(
    reversibility_cost_asymmetry,
    'Are some constitutional amendments systematically harder to reverse than others? Do amendments that were easy to pass (because they enjoyed supermajority support) also require supermajority support to repeal, creating path dependence?',
    'Comparative study of amendments passed under supermajority requirements: do repeal attempts face the same supermajority bar? If yes, the threshold creates symmetric reversibility costs; if no (if repeals face lower bars), the threshold is asymmetrically extractive by locking in supermajority-passed changes against simple-majority reversal.',
    'If reversibility is symmetric, the threshold''s filtering function is more defensible: supermajority consent is required both to adopt and to reverse. If reversibility is asymmetric (supermajority to adopt, simple majority to repeal), the threshold is a minority-entrenching device and the adaptive reading''s performance claims weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_cost_asymmetry, empirical, 'Whether constitutional amendments are reversible at the same threshold they required to pass.').

omega_variable(
    kernel_reading_coexistence,
    'Can this reading (adaptive gradient) and the consensus-safeguard reading coexist in a single constitutional framework, or do they logically foreclose each other?',
    'Logical analysis: does treating the threshold as calibrated to consensus-formation rates contradict treating it as intrinsically protective of deep consensus? Or can both claims be true (the threshold is both intrinsically protective AND requires evidence-based calibration)?',
    'If coexistence is possible, this reading influences but does not foreclose the consensus-safeguard reading. If the readings logically foreclose, the threshold permits only one interpretation and the committer framework must resolve which. The current omega records uncertainty about the logical relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether the adaptive-gradient and consensus-safeguard readings can both be true.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression the threshold produces (blocking simple majorities from amendment efficacy) structural (you cannot amend without supermajority votes; no workaround) or partially internalized (the excluded majorities believe the threshold is legitimate and deserve to be blocked)?',
    'Post-threshold-removal thought experiment: if the supermajority requirement were repealed, would the excluded majorities continue to treat amendment decisions with the same deliberative care, or would care decay? Do excluded majorities advocate for threshold retention, or only comply with it?',
    'If suppression is purely structural, the measured suppression value (0.58) is conservative—the constraint''s effective blocking power equals the structural barrier. If suppression is partially internalized, some of the constraint''s force comes from accepted legitimacy rather than coercion, shifting the classification toward rope and away from snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the supermajority threshold''s suppressive force is structural or internalized.').

omega_variable(
    calibration_reversibility,
    'Can the supermajority threshold be recalibrated based on evidence, or does the constitutional order that ratified the threshold also lock the supermajority requirement itself against revision?',
    'Examine whether proposals to adjust the amendment threshold (e.g., from 2/3 to 55%, or from 2/3 to 3/5) face the same supermajority bar they propose to change, creating a logical knot that prevents recalibration.',
    'If the threshold can be recalibrated at a lower bar, the adaptive reading''s performance claim remains meaningful: evidence can drive adjustment. If recalibration itself requires the current supermajority, the threshold is locked against evidence-based adjustment and the adaptive reading''s claim becomes performatively impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_reversibility, empirical, 'Whether the supermajority threshold itself can be recalibrated based on performance evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t5, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(supe_tr_t5, observed).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(supe_tr_t10, observed).
narrative_ontology:measurement(supe_tr_t15, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(supe_tr_t15, observed).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(supe_tr_t20, observed).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(supe_tr_t25, observed).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(supe_tr_t30, observed).
narrative_ontology:measurement(supe_tr_t35, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 35, 0.43).
narrative_ontology:measurement_basis(supe_tr_t35, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(supe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t5, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(supe_be_t5, observed).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(supe_be_t10, observed).
narrative_ontology:measurement(supe_be_t15, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(supe_be_t15, observed).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(supe_be_t20, observed).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(supe_be_t25, observed).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(supe_be_t30, observed).
narrative_ontology:measurement(supe_be_t35, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 35, 0.61).
narrative_ontology:measurement_basis(supe_be_t35, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(supe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t5, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(supe_su_t5, observed).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(supe_su_t10, observed).
narrative_ontology:measurement(supe_su_t15, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(supe_su_t15, observed).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(supe_su_t20, observed).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(supe_su_t25, observed).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(supe_su_t30, observed).
narrative_ontology:measurement(supe_su_t35, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 35, 0.58).
narrative_ontology:measurement_basis(supe_su_t35, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(supe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel admits three readings with structurally distinct ε values and beneficiary/victim distributions: the adaptive_gradient_reading (this file) treats threshold legitimacy as performance-calibrated; the consensus_safeguard_reading treats it as intrinsically protective of deep consensus; the minoritarian_veto_reading treats it as entrenching blocking minorities. All three readings share the same constitutional text but diverge on its structural function. The adaptive reading influences (but does not foreclose) the other two by establishing that empirical performance is a measuring stick for legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
