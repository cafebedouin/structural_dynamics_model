% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The constraint typology (mountain/rope/tangled_rope/snare/scaffold/piton)
 *   is itself a constraint on how institutional actors classify other
 *   constraints. The immutable diagnostic reading claims this typology is an
 *   observational instrument: constraint types are fixed categories
 *   (mountains as physical invariants, snares as measurable extraction
 *   mechanisms), epsilon values are discoverable facts, and misclassification
 *   is an error correctable through better observation. This reading
 *   privileges empiricist epistemology and establishes measurement protocols
 *   as the legitimate arbiter of classification disputes. It suppresses
 *   alternative framings—particularly the claim that constraint
 *   classification depends on normative premises about what counts as
 *   legitimate extraction. The measurement series tracks the reading's
 *   ascendance: extractiveness rises as it becomes institutionalized (higher
 *   cost to reject it), suppression intensifies as alternative epistemologies
 *   are excluded from formal deliberation, and theater increases as the
 *   reading's enforcers perform 'neutrality' while suppressing contestation.
 *
 * KEY AGENTS:
 *   - empiricist_epistemology: the core beneficiary — framed as value-neutral but actually capturing classification authority
 *   - institutional_measurement_protocols: the agenda-setter — controls metric standards and enforces the reading by defining 'legitimate input' as metric-based claims
 *   - normative_theoretical_frameworks: the primary target — suppressed by being redefined as 'opinion' rather than 'valid classification input'
 *   - policy_critique_vocabularies: secondary target — pay the cost of suppression while incidentally benefiting when metrics align with normative conclusions
 *   - alternative_epistemologies: excluded — not by rule but by construction; the reading redefines what counts as a valid basis for classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.79).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '0c451e57-0e9a-4b5a-bee8-871385860377').
narrative_ontology:cs_kernel_codification('0c451e57-0e9a-4b5a-bee8-871385860377', distributed).
narrative_ontology:cs_authority_grounding('0c451e57-0e9a-4b5a-bee8-871385860377', extraction).
narrative_ontology:cs_interpretation_layer_present('0c451e57-0e9a-4b5a-bee8-871385860377').
narrative_ontology:cs_reading_relation('0c451e57-0e9a-4b5a-bee8-871385860377', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_reading_relation('0c451e57-0e9a-4b5a-bee8-871385860377', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('0c451e57-0e9a-4b5a-bee8-871385860377', foundational, constraint_type_observationally_determined).
narrative_ontology:cs_axiom_status(constraint_type_observationally_determined, holdable).
narrative_ontology:cs_axiom_grounding('0c451e57-0e9a-4b5a-bee8-871385860377', constraint_type_observationally_determined, empirically_contingent).
narrative_ontology:cs_axiom('0c451e57-0e9a-4b5a-bee8-871385860377', foundational, epsilon_value_discoverable_not_constructed).
narrative_ontology:cs_axiom_status(epsilon_value_discoverable_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('0c451e57-0e9a-4b5a-bee8-871385860377', epsilon_value_discoverable_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('0c451e57-0e9a-4b5a-bee8-871385860377', secondary, metric_based_classification_epistemologically_neutral).
narrative_ontology:cs_axiom_status(metric_based_classification_epistemologically_neutral, holdable).
narrative_ontology:cs_axiom_grounding('0c451e57-0e9a-4b5a-bee8-871385860377', metric_based_classification_epistemologically_neutral, deontological).
narrative_ontology:cs_reference_frame('0c451e57-0e9a-4b5a-bee8-871385860377', metric_observable_classification).
narrative_ontology:cs_drift_state('0c451e57-0e9a-4b5a-bee8-871385860377', contemporary_institutional_saturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c451e57-0e9a-4b5a-bee8-871385860377', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, empiricist_epistemology).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_measurement_protocols).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, normative_theoretical_frameworks).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, policy_critique_vocabularies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, policy_critique_vocabularies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The immutable diagnostic reading privileges empirical observation as the legitimate path to truth about constraints. It grants empiricism authority to say what counts as valid knowledge about constraint classification. This position benefits from perceived objectivity and insulates empirical methodology from critique.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, empiricist_epistemology, beneficiary,
    institutional, civilizational, analytical, universal).

% Sets and enforces standards by which constraints are measured and classified. Controls what counts as 'observable'. Enforces these standards by excluding non-metric-based framings from formal deliberation and treating metric disputes as technical corrections. Collects rents from controlling the classification apparatus.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_measurement_protocols, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the cost of suppression: their claim that constraint classification depends on value premises is excluded from formal deliberation. Must recast claims in metric language or remain outside institutional discourse. Exit is constrained because institutional authority increasingly demands metric justification, making non-empirical argumentation professionally costly.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_theoretical_frameworks, payer,
    moderate, generational, constrained, global).

% Derive legitimacy from normative critique rather than measurement. Their claims are treated as opinions about values rather than discoveries about structure. Pay suppression cost but benefit incidentally when empirical metrics align with their normative conclusions. Carry structural tension: depend on discovering metric evidence for claims held on normative grounds.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_critique_vocabularies, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, policy_critique_vocabularies, beneficiary).

% Conduct systematic investigation of constraints using the immutable diagnostic framing. Appear neutral (pursuing truth) but positioned to enforce the reading's core claim. Career incentives align with producing metric evidence and resolving disputes through better measurement rather than acknowledging irreducible normative disagreement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constraint_researchers, observer,
    institutional, biographical, constrained, global).

% Constructivist, pragmatist, and interpretive epistemologies are barred from the classification conversation. Excluded not by formal rule but by the reading's core mechanism: redefining 'legitimate input' as 'metric-based claims' makes non-empirical epistemology ineligible by construction. Trapped because they cannot engage with the framework on their own terms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_epistemologies, excluded,
    powerful, civilizational, trapped, universal).

% Operate from a hybrid position: use the immutable diagnostic reading's metric tools instrumentally, but recognize that constraint classification ultimately depends on normative commitments. Observe the reading's enforcement machinery but maintain mobility to defect if the metric standard becomes too restrictive for policy work.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, pragmatist_policy_designers, observer,
    organized, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, institutional_measurement_protocols).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared diagnostic vocabulary for identifying and classifying constraints on human action, using observable metrics (extractiveness, suppression, theater_ratio) as the arbitrating evidence. Solves the problem of how institutional agents can reach agreement about what a constraint *is* without being trapped in infinite normative dispute.
% TRANSFER_FUNCTION: Transfers interpretive authority from normative theorists and policy critics to empiricist researchers and measurement protocol designers. Moves the power to say 'this is a snare' from 'this mechanism serves illegitimate ends' (normative judgment) to 'this mechanism exhibits high extractiveness and active suppression, measurably' (empirical claim). The beneficiary is the epistemology and institutional measurement infrastructure; the cost is borne by framings that depend on normative premises.
% ABSENT_VOICES: Constructivist epistemologists, normative philosophers who hold that classification depends on value commitments, and policy advocates who derive their authority from critique rather than measurement. These voices are excluded not by explicit rule but by the constraint's core mechanism: redefining 'legitimate classification basis' to be 'metric-observable' makes their contributions ineligible.
% DISAPPEARANCE_RATIONALE: If the immutable diagnostic reading vanished, institutional deliberation about constraints would splinter into competing normative framings without a shared metric standard. Some would argue this is liberating (truth emerges from contestation); others that it creates gridlock (measurement is necessary for coordination). The constraint itself is contestable: some parties see it as essential infrastructure for rational policy; others see it as suppression of legitimate normative complexity.
% FOUNDING_PROBLEM: How can institutional actors coordinate on identifying extractive constraints without defaulting to naked power struggle over whose normative framework governs the classification? The founding problem assumes that metric-based observation can provide a neutral arbiter.
% FOUNDING_PROBLEM_CORROBORATION: Empiricist researchers and institutional measurement designers attest the founding problem is live and metric-based standards are the solution. Pragmatist policy designers and constructivist epistemologists attest the founding problem is *misdefined*—the real problem is that metric standards falsely claim neutrality while suppressing legitimate normative input. No corroboration from outside the benefiting epistemological faction.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the immutable diagnostic reading concentrates interpretive authority: it declares that constraint classification is essentially an empirical-measurement problem, which transfers power from normative theorists to measurement protocol designers. The transfer is real; the authority is extracted from competing framings. Suppression is higher (0.79) because the constraint's persistence depends on actively excluding non-metric-based classifications from institutional deliberation. This is not passive (alternatives simply fail to convince) but active: institutional gatekeeping enforces the metric-based requirement, making it professionally costly for researchers to publish work that treats constraint classification as normatively dependent. Theater is moderate (0.41) because the reading must perform 'neutrality' and 'objectivity' to justify its suppression of alternative epistemologies — the performance of value-neutrality is part of what holds the constraint in place. The measurement trajectory is flat after t=30, indicating the constraint has reached institutional saturation: extractiveness plateaus because the reading is now institutionalized sufficiently that marginal gains to enforcement produce diminishing returns. Suppression holds steady at 0.79 because the constraint's existence does not require continuous enforcement once it is embedded in institutional norms and career incentives.
 *
 * PERSPECTIVAL GAP:
 *   From the empiricist and institutional-protocol seats, the immutable diagnostic reading is a genuine solution to coordination: it provides a shared standard for what a constraint is, enabling rational deliberation without defaulting to power struggle. From the normative-theory and alternative-epistemology seats, the reading is enforced extraction: it suppresses legitimate epistemological frameworks by redefining 'legitimate input' to exclude them. The two perspectives are not compatible within a single framework. The immutable diagnostic reading, by its own logic, treats this gap as a measurement problem (the normative theorists lack proper empirical evidence), which is itself an enforcement mechanism—it permits no space for the competing framings to register as valid disagreement. The engine computes per-seat type assignments from the structural data; the divergence is exactly the evidence the framework is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Empiricist epistemology is a structural beneficiary (d ≈ 0.1): the immutable diagnostic reading grants it authority to say what counts as 'truth about constraints'. Institutional measurement protocols are the agenda-setter (d ≈ 0.2): they set the standards and enforce them, collecting interpretive rents. Normative theoretical frameworks are targets (d ≈ 0.85): their alternative classification basis is suppressed, and they bear the cost of being redefined as 'opinion' rather than valid knowledge. Policy critique vocabularies are near the target end (d ≈ 0.75) because they depend on normative premises, which the reading treats as illegitimate input. Alternative epistemologies are fully trapped (d = 1.0) because the reading's core mechanism is to exclude them by redefining the admission criteria for valid classification claims. The engine computes these directionalities from the structural data; the spread across agent positions illustrates the reading's asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was: how can institutional actors coordinate on constraint classification without defaulting to naked normative conflict? The immutable diagnostic reading claims to solve this by making classification a measurement problem rather than a normative problem. But the founding problem itself embeds a normative premise: that *coordination on neutral terms* is desirable. The hybrid_pragmatic_reading would argue that some constraints (mountains, ropes) have fixed referents grounded in physical and coordination structure, but the contested periphery (snares, tangled_ropes) cannot be resolved by measurement alone—normative judgment is structurally necessary. The rhetorical_scaffold_reading would argue the founding problem is misdefined entirely: constraint classification is *always* a normative act (it requires saying what counts as legitimate extraction), and the immutable diagnostic reading's claim to neutrality is the rhetorical mechanism that makes the normative choice invisible. Does the immutable diagnostic reading resolve the founding problem or replace it with a different problem (suppression of competing epistemologies)? This is contestable. The measurement series shows extractiveness plateauing, which could indicate (1) the problem is solved and the constraint stabilizes at the solution, or (2) the constraint reaches institutional saturation and further extraction becomes politically expensive, shifting the cost of enforcement upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_sufficiency_for_classification,
    'Are metric-observable properties (extractiveness, suppression, theater_ratio) sufficient to determine constraint type, or do normative premises about legitimate beneficiaries necessarily enter the classification?',
    'Attempt to classify a contested constraint (e.g., a labor norm that some call protective and others extractive) using only metrics, without normative input. If classification remains ambiguous or changes when normative premises shift, the metrics are not sufficient.',
    'If metrics are insufficient, the immutable diagnostic reading''s core claim fails: constraint classification is not a pure observation problem. If metrics are sufficient, the normative theories that claim otherwise are simply wrong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_sufficiency_for_classification, conceptual, 'Whether metric properties alone determine constraint type or normative premises are structurally necessary.').

omega_variable(
    epistemology_neutrality_claim,
    'Is the immutable diagnostic reading''s framing of constraint classification as ''measurement'' genuinely epistemologically neutral, or does it privilege one epistemology (empiricism) over others (constructivism, pragmatism, interpretivism)?',
    'Meta-analysis of which epistemological frameworks are treated as valid input to the classification process. If only empiricist frameworks pass, the reading is not neutral but rather enforces epistemological conformity while claiming to discover neutral facts.',
    'If the reading enforces epistemological conformity while claiming neutrality, it operates as suppression of alternative epistemologies disguised as neutral methodology. If genuine neutrality is maintained, the suppression is justified by the superiority of empirical method.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemology_neutrality_claim, empirical, 'Whether the immutable reading''s epistemological premises are genuinely neutral or mask suppression of alternatives.').

omega_variable(
    institutional_capture_by_measurement,
    'Does the immutable diagnostic reading''s institutionalization lead to the constraint becoming a tool for institutional agenda-setting, where measurement standards are chosen to produce preferred classifications rather than to discover truth?',
    'Examine metric choices over time: are standards changed when they produce unwanted classifications? Are alternative metrics suppressed? Do institutional beneficiaries control the definition of ''observable''?',
    'If measurement standards are bent to serve institutional agendas, the reading''s claim to objective discovery is compromised—it becomes a sophisticated mechanism for disguising power as method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_by_measurement, empirical, 'Whether the immutable reading becomes institutionally captured such that metric standards serve agenda-setting rather than truth-seeking.').

omega_variable(
    alternative_epistemologies_suppression,
    'Is the suppression of alternative epistemologies a necessary feature of the immutable diagnostic reading, or could the reading coexist with non-empiricist frameworks that also claim to classify constraints validly?',
    'Institutional experiment: permit constructivist or pragmatist frameworks to classify constraints alongside empirical metrics. If the immutable reading survives such coexistence, suppression was not necessary; if it collapses, suppression is structural.',
    'If suppression is necessary, the reading is inherently exclusionary and the constraint is a snare (extraction via suppression of epistemic alternatives). If suppression is contingent, the reading might be genuine coordination that happened to exclude alternatives unnecessarily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemologies_suppression, conceptual, 'Whether the suppression of non-empiricist epistemologies is structurally required by the immutable reading or contingent on institutional choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(defe_tr_t5, observed).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(defe_tr_t10, observed).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(defe_tr_t15, observed).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(defe_tr_t20, observed).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(defe_tr_t25, observed).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(defe_tr_t30, observed).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(defe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(defe_be_t5, observed).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(defe_be_t10, observed).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(defe_be_t15, observed).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(defe_be_t20, observed).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(defe_be_t25, observed).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(defe_be_t30, observed).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(defe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(defe_su_t5, observed).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(defe_su_t10, observed).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(defe_su_t15, observed).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(defe_su_t20, observed).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement_basis(defe_su_t25, observed).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(defe_su_t30, observed).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(defe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.18).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'deferential_realism_ontology'. The three readings (immutable_diagnostic, hybrid_pragmatic, rhetorical_scaffold) are structurally distinct constraints with different epsilon values and beneficiary structures. The immutable diagnostic reading claims constraint classification is a measurement problem with discoverable answers (high extraction from suppressing alternative epistemologies); the hybrid reading claims some constraints have fixed referents but others require normative judgment; the rhetorical reading claims all classification is normative and the framework's value is persuasive rather than metaphysical. These are not the same constraint viewed from different angles—they have different ε values and different suppression targets. The family link is: immutable_diagnostic and rhetorical_scaffold foreclose each other (incompatible core premises), while hybrid_pragmatic influences both (claims to mediate but actually depends on distinguishing the fixed-referent core from the normatively-contested periphery).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
