% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Imperial Legitimation Mechanism (Symbolic Authority + Institutional Incentives)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   A sovereign establishes a new cultural norm by combining two mechanisms:
 *   (1) visible, exemplary adoption of the norm by the sovereign themselves,
 *   investing the sovereign's charisma and prestige in the norm to make it
 *   seem inevitable and culturally superior; (2) material incentives (land
 *   grants, honors, ceremonial privilege) offered to elites who adopt the
 *   norm visibly, creating a cascading legitimacy gradient. This reading
 *   instantiates a HYBRID mechanism: neither the bottom-up grassroots climb
 *   (endogenous_climb_reading) nor the top-down coercive override
 *   (exogenous_override_reading), but a fusion of symbolic authority transfer
 *   and institutional incentive alignment. The claim is tangled_rope because
 *   the mechanism coordinates cultural expectations (genuine function) while
 *   extracting compliance from subordinated populations and tradition-bearers
 *   through a mix of symbolic pressure and enforcement (asymmetric
 *   extraction). The metrics reflect the hybrid character: moderate
 *   extractiveness because the norm is partially legitimated by the
 *   sovereign's example (lower extraction than pure coercion), but extraction
 *   still exists because the norm is ultimately mandatory. Suppression
 *   requirement declines over the interval as the norm normalizes, but
 *   theater ratio remains elevated because the sovereign's ongoing ceremonial
 *   endorsement is essential to the norm's legitimacy maintenance.
 *
 * KEY AGENTS:
 *   - imperial_sovereign: sets and exemplifies the norm through visible, charismatic action; sole center of cultural authority transfer
 *   - imperial_elite: adopt the norm visibly in exchange for material incentives; serve as intermediate broadcasters of the norm to subordinated populations
 *   - institutional_apparatus: operationalize the norm through administered enforcement; benefit from the norm's symbolic legitimacy which reduces active suppression burden
 *   - local_custom_holders: bear identity-locked costs of norm displacement; form the core of identity-transgressive resistance
 *   - subordinated_populations: adopt the norm under a mixture of elite-led social cascading and institutional enforcement; trapped by the norm's perceived inevitability
 *   - competing_sovereigns: structurally excluded from the symbolic authority mechanism because it depends on a single center of charisma
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.48).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.42).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Imperial Legitimation Mechanism (Symbolic Authority + Institutional Incentives)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'ba9b8416-9f7f-4a51-9425-5f6126e78692').
narrative_ontology:cs_kernel_codification('ba9b8416-9f7f-4a51-9425-5f6126e78692', formalized).
narrative_ontology:cs_authority_grounding('ba9b8416-9f7f-4a51-9425-5f6126e78692', lineage).
narrative_ontology:cs_interpretation_layer_present('ba9b8416-9f7f-4a51-9425-5f6126e78692').
narrative_ontology:cs_reading_relation('ba9b8416-9f7f-4a51-9425-5f6126e78692', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba9b8416-9f7f-4a51-9425-5f6126e78692', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('ba9b8416-9f7f-4a51-9425-5f6126e78692', foundational, legitimacy_from_fused_symbolic_and_institutional_alignment).
narrative_ontology:cs_axiom_status(legitimacy_from_fused_symbolic_and_institutional_alignment, holdable).
narrative_ontology:cs_axiom_grounding('ba9b8416-9f7f-4a51-9425-5f6126e78692', legitimacy_from_fused_symbolic_and_institutional_alignment, deontological).
narrative_ontology:cs_axiom('ba9b8416-9f7f-4a51-9425-5f6126e78692', secondary, stratified_adoption_via_elite_cascading).
narrative_ontology:cs_axiom_status(stratified_adoption_via_elite_cascading, holdable).
narrative_ontology:cs_axiom_grounding('ba9b8416-9f7f-4a51-9425-5f6126e78692', stratified_adoption_via_elite_cascading, empirically_contingent).
narrative_ontology:cs_reference_frame('ba9b8416-9f7f-4a51-9425-5f6126e78692', sovereign_charismatic_authority_as_cultural_legitimation_source).
narrative_ontology:cs_drift_state('ba9b8416-9f7f-4a51-9425-5f6126e78692', post_elite_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ba9b8416-9f7f-4a51-9425-5f6126e78692', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, institutional_apparatus).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, subordinated_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, local_custom_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the new norm through visible, exemplary action (ceremonial adoption, public performance). Invests symbolic authority—the prestige and charisma of the office—in the norm to make it seem inevitable and culturally superior. Simultaneously structures institutional incentives (land grants, honors, exemptions) to reward early adopters among local elites. The sovereign's legitimacy derives from the fusion of symbolic demonstration and material incentive alignment, not from coercive capacity alone. The sovereign can withdraw these incentives or the symbolic endorsement, making the mechanism's continuation depend on the sovereign's sustained commitment.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_sovereign, agenda_setter,
    institutional, generational, analytical, continental).

% Adopt the norm visibly and early in exchange for land, titles, and ceremonial privilege. They interpret the sovereign's example as cultural authority—a demonstration that the norm is consonant with imperial prestige and advancement. Their adoption broadcasts the norm to subordinated populations, creating a social cascading effect. They bear compliance costs (demonstrating the norm publicly, maintaining it visibly), but these are offset by material rewards and access to institutional power. They have mobility options—they could resist the norm and lose rewards, or emigrate—but the reward structure makes compliance the dominant strategy.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite, payer).

% Bureaucratic, military, and ecclesiastical structures operationalize the norm through administered practice. They benefit from the norm's legitimacy (reduced resistance, voluntary compliance) which lowers their active enforcement burden relative to pure coercion. They manage the rollout sequentially: elites first (lower resistance cost), masses later (normalized by elite example). The apparatus's survival depends on making the norm seem natural and inevitable rather than imposed. They bear the cost of administering enforcement, but those costs are substantially lower than they would be without the norm's cultural legitimation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, institutional_apparatus, beneficiary,
    organized, generational, constrained, continental).

% Bear the cost of norm displacement: their established practices lose legitimacy and are formally superseded. The sovereign's example creates a cultural authority gradient that makes the old norm seem provincial, backward, or morally inferior. They face sequential pressure—first through elite adoption (social proof from above), then through institutional enforcement (explicit prohibition). Exit from the norm (continuing old practice) becomes identity-transgressive: it marks them as resistant to imperial authority and disloyal to the cultural order the sovereign exemplifies. Their resistance costs rise as the norm spreads, because resistance becomes increasingly isolated and marked as deviant.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_custom_holders, payer,
    moderate, biographical, identity_locked, local).

% Adopt the new norm under a mixture of institutional enforcement and symbolic pressure. They see the norm adopted by visible elites (social cascading) and face explicit prohibition of the old practice through administrative channels. They cannot refuse without being marked as seditious or culturally inferior. The constraint's legitimacy—derived from the sovereign's charisma and the elite-led cascade—makes resistance seem foolish rather than justified. Active suppression (punishment for violations) is necessary but supplementary; the norm's perceived inevitability does much of the enforcement work. Younger cohorts who grow up under the new norm internalize it more readily than those who lived under the old norm.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, subordinated_populations, payer,
    powerless, biographical, trapped, continental).

% Are excluded from the symbolic authority transfer mechanism because the norm is tied to a specific sovereign's charisma and example. If they attempted to impose a different norm through their own symbolic example, they would risk direct conflict with the imperial order. Their exclusion is structural: the mechanism depends on the sovereign's unique claim to cultural authority, which precludes competing centers of legitimacy. They could contest the norm by building competing cultural authority, but doing so would be read as political rebellion and face direct suppression from the apparatus.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, competing_sovereigns, excluded,
    institutional, generational, trapped, continental).

% Reconstruct the norm's adoption trajectory from historical records. They observe the stratified adoption pattern (elites early, masses late), the central role of the sovereign's visible example, the material incentives aligned with symbolic demonstration, and the persistence of identity-locked resistance among subordinated populations. They measure the enforcement cost and the legitimacy gradient to adjudicate whether this was primarily a climb, an override, or a hybrid mechanism. They are outside the mechanism but their interpretations feed back into how later historians and the apparatus itself understand the norm's legitimacy.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historians_and_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_elite).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified cultural framework across a geographically dispersed, hierarchically stratified population. The norm coordinates expectations about proper conduct, status signals, and loyalty to the imperial order. It reduces transaction costs for the apparatus by making the norm seem inevitable and culturally superior rather than imposed, thereby lowering active enforcement overhead. It resolves a genuine coordination problem: without shared cultural norms, the empire cannot maintain integration across diverse local traditions.
% TRANSFER_FUNCTION: Transfers cultural authority from the sovereign's personal charisma to the norm itself, making the norm seem inseparable from imperial legitimacy. Transfers compliance capacity from subordinated populations to the apparatus by leveraging the norm's perceived inevitability. Transfers prestige, land, and material rewards to early-adopting elites. Transfers identity costs to tradition-bearers and subordinated populations, who must either internalize the norm or face suppression and social marking as deviant.
% ABSENT_VOICES: Competing sovereigns and alternative norm-setting authorities are structurally excluded because the constraint depends on a single center of charismatic authority. Traditional practitioners and local custom-holders are also largely absent from the decision process, though they are later forced to comply. Religious authorities who might contest the norm on theological grounds are excluded unless incorporated into the institutional apparatus and made beneficiaries. Subordinated populations are present only as objects of enforcement, not as participants in setting the norm.
% DISAPPEARANCE_RATIONALE: If the norm and the hybrid legitimation mechanism disappeared, the apparatus would lose its primary non-coercive tool for coordinating behavior across populations with different local preferences. The elites' incentive to demonstrate loyalty would evaporate. Subordinated populations would face the choice of reverting to old practices or needing explicit enforcement for every behavior the apparatus wanted to regulate. The institutional apparatus would fragment because it depends on shared cultural expectations to function efficiently. The empire's ability to govern without constant coercion would collapse.
% FOUNDING_PROBLEM: The sovereign seeks to establish a new cultural norm that supersedes strong local attachments to traditional practices across a heterogeneous, geographically dispersed population. Direct coercion alone is expensive and produces resentment; pure bottom-up demand does not exist because local populations are invested in their traditions. The hybrid solution leverages the sovereign's unique charismatic authority (making the norm seem inevitable and culturally superior) combined with material incentives that align elite interests with demonstrating the norm visibly, creating a cascading legitimacy gradient that makes the norm seem culturally inevitable rather than imposed. The mechanism solves the problem by making adoption seem rational and culturally superior, not coerced.
% FOUNDING_PROBLEM_CORROBORATION: The sovereign's own proclamations and imperial historical records assert the problem and solution: the norm was necessary for unified imperial culture and the sovereign's charismatic example was its most effective means of establishing it. However, local historians and anthropologists studying subordinated populations and tradition-bearers describe the adoption as fundamentally coercive—elites were bribed, masses were threatened with punishment or social exclusion. Institutional historians note the apparatus's own efficiency measures and phased-rollout strategy reflect cost-reduction engineering, not natural cultural spread. Literary and oral history from tradition-bearing communities records resistance and identity loss. The founding problem's status is contested between the sovereign's own narrative (the norm was necessary and rationally adopted) and the lived experience of those forced to comply (the norm was imposed and identity-destroying). No corroboration exists outside the beneficiary parties for the 'rational adoption' narrative; independent observers note the coercive mechanisms throughout.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.48 reflects the hybrid mechanism: the sovereign's charismatic demonstration and elite incentives create genuine cultural authority (lowering extraction below pure coercion), but the norm is ultimately mandatory and displacement of local custom is enforced. Suppression starts high (0.65) because the apparatus must initially suppress alternatives directly, but declines to 0.38 by interval end as the norm normalizes and becomes self-reinforcing through internalized cultural authority. Theater ratio is elevated (0.75 at t0, declining to 0.58 at t40) because the sovereign's visible example is the primary legitimacy mechanism—the constraint's persistence depends on ongoing ceremonial and symbolic performance. The ratio declines as the norm institutionalizes, but remains above 0.5 because the apparatus continues to maintain the sovereign's endorsement as performative cultural validation. Accessibility_collapse is stratified: structural level (0.55→0.68) because the system-level arrangement becomes increasingly normalized; organizational level (0.70→0.75) because elite adoption rapidly forecloses alternatives for that tier; class level (0.45→0.62) because collective alternatives erode as the norm cascades; individual level (0.30→0.55) because individual actors retain some exit options through identity-locked resistance, though those options become progressively marked as seditious. Resistance is highest at the individual and class levels (0.65 and 0.58 at t0) because those populations bear the identity costs, declining over time as younger cohorts naturalize the norm. This temporal pattern reflects the mechanism's core dynamic: the sovereign's example and elite incentives achieve rapid adoption among beneficiaries and compliance among subordinated populations, but identity-locked resistance persists as long as the old norm-holders remain alive.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign's and elite's perspectives, the constraint is a legitimate cultural coordination mechanism—a demonstration of superior cultural values that subjects rationally embrace and adopt. From the local_custom_holders' perspective, it is coercive displacement of their cultural practices, legitimated through a charade of symbolic authority. From the institutional apparatus's perspective, it is an efficiency mechanism that reduces enforcement burden by making the norm seem inevitable. These divergent readings all map to the same structural mechanism and metrics, but they compute different classification verdicts from the same data. The engine's per-seat computation captures this divergence: the beneficiary seats (sovereign, elite, apparatus) and the payer seats (custom-holders, subordinated populations) experience the constraint differently, and those differences are structural, not observational.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was the sovereign's need to establish a new cultural norm across a heterogeneous population with strong local attachments to tradition. The hybrid mechanism solves this problem by combining symbolic authority and material incentive alignment. However, the mechanism's legitimacy depends on the sovereign's charisma remaining salient. If the founding problem (heterogeneous local attachments) is superseded by a new generation for whom the norm is already naturalized, the mechanism's coordination function becomes vestigial—the apparatus continues enforcing the norm through institutional inertia, but the symbolic authority and elite incentive components become merely performative. The measurement series shows suppression declining (fewer actual violations as the norm internalizes) and theater rising relative to functional need, which is a sign of potential mandatrophy: the apparatus continues investing in the sovereign's ceremonial endorsement not because it solves the original coordination problem, but because it is the institutionalized method. The tangled_rope classification remains appropriate across the interval because the norm genuinely coordinates cultural expectations (real coordination function) and genuinely extracts compliance from tradition-bearers (real asymmetric extraction), even as the ratio between the two components shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_authority_internalization,
    'To what extent did subordinated populations internalize the norm because the sovereign''s example genuinely shifted their cultural values, versus adopting it because the elite-led cascade and institutional enforcement made resistance costly and identity-transgressive?',
    'Post-transition behavioral data: if subordinated populations continue the norm after the apparatus withdraws enforcement and elite incentives are removed, internalization occurred; if the norm reverts rapidly, adoption was compliance without internalization.',
    'If internalized, the mechanism succeeded in its cultural legitimation function and should decline toward a pure-rope classification (genuine coordination without extraction). If compliance-without-internalization, the mechanism is primarily extractive and sustained by theater and suppression, suggesting piton characteristics in later periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_authority_internalization, empirical, 'Whether symbolic authority achieved genuine cultural shifts or merely behavioral compliance.').

omega_variable(
    elite_capture_vs_coordinated_alignment,
    'Were the institutional incentives aligned with the norm''s cultural value (elites internalized the norm first and were rewarded for demonstrating it), or were they functionally captured (elites were bribed to feign adoption and broadcast a norm they did not themselves believe)?',
    'Comparative analysis of elite behavior post-incentive: if elite adherence persists after material rewards decline, the alignment was genuine; if it collapses, the incentives were purely extractive.',
    'Genuine alignment supports the hybrid mechanism as a functional coordination hybrid (partly rope, partly tangled rope). Capture suggests the mechanism is a snare disguised as rope: the sovereign extracts cultural authority through an elite coalition, and the norm persists only because the apparatus enforces it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_vs_coordinated_alignment, empirical, 'Whether elite adoption was internalized cultural change or captured performance.').

omega_variable(
    kernel_reading_contestation,
    'Is the norm''s legitimacy evidence that the endogenous_climb_reading was right (subordinated populations wanted this norm), evidence that the exogenous_override_reading was right (coercion worked and cultural internalization is post-hoc rationalization), or evidence that this hybrid_legitimation_reading was right (the sovereign''s charisma plus elite cascading were the legitimate mechanisms)?',
    'No single empirical test resolves this—the three readings reframe the same historical record. The resolution is committer-dependent: which reading''s axioms and reference frame the evaluating authority accepts.',
    'Acceptance of the hybrid reading requires accepting that legitimacy can be derived from fused symbolic authority and institutional incentive alignment. Rejection implies endorsing either grassroots demand or coercive imposition as the only legitimate sources of norm change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which kernel reading correctly accounts for the norm''s legitimacy.').

omega_variable(
    identity_locked_resistance_suppression,
    'Is the measured suppression (0.42 at t40) applied against active resistance from tradition-bearers, or is it the internalized psychological suppression that tradition-bearers apply to themselves (belief that resistance is futile, identity-transgressive, or morally wrong)?',
    'Structural exit test: tradition-bearer populations that leave the jurisdiction after norm imposition and revert to old practices exhibit structural suppression (external barriers blocked their exit). Populations that remain and internalize the norm exhibit internalized suppression (they apply it to themselves through identity fusion).',
    'If structural, the constraint''s effective suppression should be re-evaluated and the payer seats'' exits re-categorized (from identity_locked toward trapped). If internalized, the suppression metric is accurate but the identity-locking mechanism is the true extraction lever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_resistance_suppression, empirical, 'Whether suppression is structural enforcement or internalized identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement_basis(impo_tr_t0, observed).
narrative_ontology:measurement(impo_tr_t5, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 5, 0.7).
narrative_ontology:measurement_basis(impo_tr_t5, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.65).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement_basis(impo_tr_t15, observed).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement_basis(impo_tr_t25, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(impo_be_t0, observed).
narrative_ontology:measurement(impo_be_t5, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(impo_be_t5, observed).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(impo_be_t15, observed).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 25, 0.5).
narrative_ontology:measurement_basis(impo_be_t25, observed).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(impo_su_t0, observed).
narrative_ontology:measurement(impo_su_t5, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(impo_su_t5, observed).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(impo_su_t15, observed).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement_basis(impo_su_t25, observed).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(impo_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(impo_grid_01, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement(impo_grid_02, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(class), 40, 0.62).
narrative_ontology:measurement(impo_grid_03, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(individual), 0, 0.3).
narrative_ontology:measurement(impo_grid_04, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(individual), 40, 0.55).
narrative_ontology:measurement(impo_grid_05, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(organizational), 0, 0.7).
narrative_ontology:measurement(impo_grid_06, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(organizational), 40, 0.75).
narrative_ontology:measurement(impo_grid_07, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(impo_grid_08, imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse(structural), 40, 0.68).
narrative_ontology:measurement(impo_grid_09, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(impo_grid_10, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(class), 40, 0.48).
narrative_ontology:measurement(impo_grid_11, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(individual), 0, 0.65).
narrative_ontology:measurement(impo_grid_12, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(individual), 40, 0.52).
narrative_ontology:measurement(impo_grid_13, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(impo_grid_14, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(organizational), 40, 0.28).
narrative_ontology:measurement(impo_grid_15, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(impo_grid_16, imposition_mechanism_kernel__hybrid_legitimation_reading, resistance(structural), 40, 0.38).
narrative_ontology:measurement(impo_grid_17, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(impo_grid_18, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(class), 40, 0.6).
narrative_ontology:measurement(impo_grid_19, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(individual), 0, 0.4).
narrative_ontology:measurement(impo_grid_20, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(individual), 40, 0.5).
narrative_ontology:measurement(impo_grid_21, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(impo_grid_22, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(impo_grid_23, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(structural), 0, 0.7).
narrative_ontology:measurement(impo_grid_24, imposition_mechanism_kernel__hybrid_legitimation_reading, stakes_inflation(structural), 40, 0.75).
narrative_ontology:measurement(impo_grid_25, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(impo_grid_26, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(class), 40, 0.35).
narrative_ontology:measurement(impo_grid_27, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(impo_grid_28, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(individual), 40, 0.42).
narrative_ontology:measurement(impo_grid_29, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(impo_grid_30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(organizational), 40, 0.38).
narrative_ontology:measurement(impo_grid_31, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(impo_grid_32, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression(structural), 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.12).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling readings decompose the contested kernel imposition_mechanism_kernel into three structurally distinct constraint stories. The hybrid_legitimation_reading claims legitimacy derives from fused symbolic authority (the sovereign's charismatic demonstration) and institutional incentive alignment (material rewards for elite adoption), with stratified adoption (elites first, masses later). The endogenous_climb_reading claims the norm was adopted because subordinated populations came to demand it. The exogenous_override_reading claims the norm was imposed by coercive capacity alone. These readings are not alternative observations of a single constraint—they are alternative framings of what legitimacy means in norm establishment. Each has its own beneficiary/victim structure, its own ε, and its own type. The readings coexist: they are held by different parties and different scholarly traditions, and no single empirical test forecloses any of them, though differential success in explaining adoption patterns and post-transition persistence could influence scholarly weight. This constraint (hybrid) influences both siblings by establishing a third mechanism that is neither pure demand nor pure coercion, thereby pressuring the endogenous and exogenous readings to sharpen their claims about what 'demand' and 'coercion' mean.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__hybrid_legitimation_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
