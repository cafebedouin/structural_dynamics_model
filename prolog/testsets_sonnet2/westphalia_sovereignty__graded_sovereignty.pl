% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint instantiates the 'graded sovereignty' reading of the
 *   contested Westphalian sovereignty kernel: territorial authority is
 *   treated not as a binary attribute but as a scalar function of measured
 *   state capacity, with intervention legitimacy calibrated continuously to
 *   capacity deficits rather than triggered categorically by either
 *   territorial breach (absolute_non_intervention) or atrocity threshold
 *   (conditional_responsibility). Under this reading, capacity-evaluation
 *   bodies — fragility indices, governance benchmarking regimes,
 *   donor-conditionality frameworks — become the load-bearing institution:
 *   they do not merely respond to crisis but continuously produce the tiering
 *   that determines who may act against whom. The reading's own beneficiaries
 *   are exactly those positioned to design and administer the metrics; its
 *   victims are states whose scored deficiency (often substantially inherited
 *   from colonial-era institutional and economic structuring) becomes the
 *   doctrinal basis for graduated external control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.61).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '1ebc7db1-0e94-43b0-825c-c3353dff1e2c').
narrative_ontology:cs_kernel_codification('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', distributed).
narrative_ontology:cs_authority_grounding('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', distributed).
narrative_ontology:cs_reading_relation('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', foundational, sovereignty_is_scalar_not_binary).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', sovereignty_is_scalar_not_binary, conventional).
narrative_ontology:cs_axiom('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', foundational, measured_capacity_deficit_licenses_graduated_intervention).
narrative_ontology:cs_axiom_status(measured_capacity_deficit_licenses_graduated_intervention, holdable).
narrative_ontology:cs_axiom_grounding('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', measured_capacity_deficit_licenses_graduated_intervention, empirically_contingent).
narrative_ontology:cs_reference_frame('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', westphalian_absolute_territorial_authority).
narrative_ontology:cs_drift_state('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', post_cold_war_intervention_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ebc7db1-0e94-43b0-825c-c3353dff1e2c', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, dominant_power_bloc_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, post_colonial_fragile_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, populations_of_intervened_states).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, state_capacity_as_legitimacy_metric).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, hierarchical_state_system_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bodies (UN agencies, treaty organs, donor-state coalitions, credit-rating and governance indices) that produce and certify the capacity metrics — fragility indices, governance scores, rule-of-law rankings — which determine where a state sits on the sovereignty spectrum. They set the criteria, administer the assessment process, and revise thresholds. Their own conduct is not scored against the same metrics they apply to others.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% States that score at the 'full capacity' end of the spectrum by construction of the metrics themselves (often the same states that designed the metrics). They gain standing to authorize, fund, or lead interventions against lower-scored states, and are functionally exempt from having their own territorial authority questioned regardless of internal conduct.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, dominant_power_bloc_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, dominant_power_bloc_states, agenda_setter).

% Governments scored as having 'deficient' capacity — often for reasons rooted in colonial-era border drawing, extraction economies, or externally imposed structural adjustment — and thereby rendered subject to graduated external oversight: conditional aid, trusteeship-like administrative arrangements, or armed intervention justified as capacity-substitution. They cannot appeal the metric itself; the only path off the list is compliance with donor/evaluator-set benchmarks.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, low_capacity_states, payer,
    powerless, biographical, trapped, national).

% A subset of low-capacity states whose 'deficiency' is substantially the residue of the same international order now empowered to grade them — border incoherence, extractive economic integration, and weak institutions inherited from colonial administration. They bear a double cost: the historical cause of their scored deficiency and the present consequence of being scored.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, post_colonial_fragile_states, payer,
    powerless, generational, trapped, national).

% Civilians living under a state that has been graded low-capacity and made subject to peacekeeping missions, conditional aid regimes, or armed intervention. They absorb the immediate costs of intervention — displacement, occupation-adjacent governance, disrupted services — while having no seat in either the metric design or the intervention decision.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_of_intervened_states, payer,
    powerless, immediate, trapped, local).

% States scored in the ambiguous middle of the spectrum — neither fully sovereign by the metric's own terms nor formally subject to intervention. They have no formal voice in setting or revising the capacity thresholds that determine their own status, and their position can shift with methodology revisions they did not participate in drafting.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, middle_capacity_states, excluded,
    moderate, biographical, constrained, national).

% Lending and governance-conditionality bodies whose leverage over low-capacity states is legitimated by the same graded-sovereignty logic: conditional lending, structural benchmarks, and governance reform requirements are framed as capacity-building rather than as external control, and the graded framework supplies the doctrinal cover for this leverage.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Scholars and jurists who study how the capacity-spectrum framing emerged from mid-20th-century decolonization anxieties and post-Cold War intervention debates, and who track whether the metrics track genuine institutional weakness or reproduce prior power hierarchies under new vocabulary.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for allocating scarce international attention and resources toward states genuinely unable to perform core governance functions (basic security, service delivery, rule of law), avoiding both indiscriminate non-intervention in the face of state collapse and indiscriminate intervention in functioning states.
% TRANSFER_FUNCTION: Moves formal decision authority over intervention legitimacy, and practical control over conditional aid and reconstruction resources, from the graded state's own institutions to the bodies that design and administer the capacity metrics — while moving reputational and material costs of 'deficient' scoring onto the graded state and its population.
% ABSENT_VOICES: Low-capacity and post-colonial states have no vote in designing the metrics that determine their own sovereignty grade; the populations who live under intervention regimes have no representation in the evaluation process at all. Middle-capacity states, whose future grading is uncertain, are also structurally absent from threshold-setting bodies dominated by already-high-scoring states.
% DISAPPEARANCE_RATIONALE: If graded sovereignty vanished as an operative doctrine, intervention decisions would have to be justified on some other basis (absolute non-intervention or atrocity-triggered forfeiture), capacity-index bodies would lose their gatekeeping function over aid conditionality and legitimacy discourse, and dozens of states currently subject to graduated oversight arrangements would revert to unconditional territorial authority claims — a substantial reorganization of who can authorize what against whom.
% FOUNDING_PROBLEM: Cold War-era non-intervention absolutism left the international system unable to respond to state collapse, mass atrocity, and governance vacuum (Somalia, Rwanda, Bosnia) without either ignoring catastrophe or violating sovereignty categorically; graded capacity assessment was built to supply a principled, calibrated middle path.
% FOUNDING_PROBLEM_CORROBORATION: UN reform bodies and R2P architects attest the underlying problem — how to respond to governance collapse without licensing unlimited intervention — remains live. Independent post-colonial and Global South legal scholars (outside the evaluation-authority and dominant-bloc beneficiary set) attest that the graded framework has substantially shifted from a targeted atrocity-response tool toward a general-purpose hierarchy-legitimating instrument whose metrics disproportionately grade formerly colonized states as deficient, and that no comparably rigorous capacity metric is ever applied reciprocally to the evaluating powers themselves.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the measured interval (0.42 to 0.68) as capacity-metric regimes matured from ad hoc post-Cold War crisis diagnostics into standing conditionality architecture (World Bank governance indicators, Fragile States Index, IMF program benchmarks) that now routinely gates aid, recognition, and intervention discourse — a rent-accumulation pattern layered onto a genuine original coordination need. Theater ratio climbs moderately (0.20 to 0.42) as index methodology proliferates without commensurate improvement in predictive or corrective value — much of the apparatus now performs rigor (numerical scores, published rankings) more than it produces it. Suppression is substantial but sub-maximal (ending at 0.61): low-capacity states are not physically barred from contesting their grade, but have essentially no practical channel to alter the metrics that grade them, which functions as suppression through structural exclusion from criteria-setting rather than direct coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the capacity-evaluation authority's seat, the framework reads as principled calibration: intervention scaled to genuine institutional deficit, a rational improvement over blunt binary sovereignty. From the low-capacity state's seat, the identical structure reads as a formalized hierarchy that launders great-power discretion through technical-sounding metrics — the same discretionary intervention decisions dominant states always made, now dressed in index scores they themselves designed. The engine's per-seat computation should register this divergence structurally: the agenda-setter/beneficiary seats compute closer to coordination, the payer seats closer to extraction, from the same underlying facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluation authorities and the dominant states whose institutions the metrics implicitly take as baseline sit at the beneficiary end: they set criteria, are never themselves scored against those criteria in any binding way, and gain expanded legitimate authority to act on others as a direct function of the framework's operation. Low-capacity and post-colonial fragile states sit at the target end: trapped exit options (they cannot simply exit the international system that grades them), no criteria-setting voice, and the costs of being graded (conditional sovereignty, external oversight) land on them and their populations directly. Middle-capacity states occupy an unstable position — excluded from agenda-setting but not yet subject to the harshest tiering consequences, which is why they are marked excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to respond to state collapse and atrocity without either paralysis or unlimited license — remains partly live, which is what prevents this from being flatly declared a pure snare: there is a real coordination residue (calibrated response beats binary absolutism in genuine collapse cases). But the framework has drifted from targeted crisis response toward a general-purpose tiering instrument whose criteria are set unilaterally and never applied reciprocally, which is the tangled-rope signature: real coordination function plus asymmetric extraction through the same structure, sustained by active enforcement (conditionality regimes, intervention authorization processes) rather than by voluntary participant benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_neutrality,
    'Do capacity/fragility indices measure genuine governance deficit, or do they encode the institutional forms and economic arrangements of the states that designed them as the implicit baseline of ''full capacity''?',
    'Comparative methodology audit: test whether index criteria would score historical or counterfactual dominant-state institutional configurations (e.g. 19th-century Western state administrative capacity, or non-liberal high-functioning governance models) as deficient under the same rubric applied to currently low-scored states.',
    'If the metrics are baseline-parochial rather than neutral, the entire graded-sovereignty framework is a false-neutral instrument for encoding an existing power hierarchy as a technical/scientific finding — sharply increasing the case for reclassifying toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_neutrality, conceptual, 'Whether capacity indices are baseline-neutral or encode dominant-state institutional forms as the norm.').

omega_variable(
    genealogy_versus_current_function,
    'Is the graded-sovereignty doctrine''s current operation continuous with its stated founding purpose (calibrated crisis response), or has it drifted into a standing hierarchy-maintenance function that would persist even absent any active crisis?',
    'Trace whether capacity-conditionality relationships (aid conditionality, governance benchmarking, oversight arrangements) are time-bound to specific crisis episodes and dissolve on capacity improvement, versus persisting indefinitely regardless of measured improvement.',
    'Persistence independent of measured improvement would support the mandatrophy reading (founding problem substantially resolved in many cases, framework persists as extraction); crisis-bound dissolution would support the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genealogy_versus_current_function, empirical, 'Whether the doctrine''s operation is crisis-bound or has become a standing hierarchy independent of its founding rationale.').

omega_variable(
    colonial_causation_of_scored_deficiency,
    'To what extent is the ''capacity deficit'' that triggers graduated oversight itself a product of the same international order (colonial administration, extractive economic integration, externally drawn borders) now empowered to grade and act on it?',
    'Historical-structural analysis correlating current capacity index scores against colonial administrative history, border-imposition patterns, and post-independence structural adjustment exposure.',
    'A strong correlation would establish the framework as substantially self-referential extraction — the same order that produced the deficiency now profits from diagnosing and managing it — reinforcing the victim designation for post_colonial_fragile_states specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_causation_of_scored_deficiency, empirical, 'Whether scored capacity deficits are substantially colonial-legacy artifacts rather than independent governance failures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__graded_sovereignty, theater_ratio, 5, 0.25).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.3).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__graded_sovereignty, theater_ratio, 15, 0.33).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.37).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.4).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, conditional_responsibility).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalia_sovereignty kernel. absolute_non_intervention treats territorial authority as categorical and any capacity-based tiering as per se illegitimate — it is logically FORECLOSED by this reading's core premise, since a scalar capacity axiom is incompatible with categorical inviolability in the same framework. conditional_responsibility treats sovereignty as conditional on an atrocity threshold rather than a continuous capacity scale; graded_sovereignty INFLUENCES that sibling by expanding the evidentiary and institutional infrastructure (capacity indices, conditionality regimes) that atrocity-threshold determinations increasingly draw on, without foreclosing the threshold logic itself. All three readings share victim/beneficiary overlap in practice (dominant states and evaluation bodies benefit across readings; weak/post-colonial states pay across readings) but instantiate structurally distinct triggers, enforcement mechanisms, and doctrinal justifications, and so are authored as three separate constraint stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
