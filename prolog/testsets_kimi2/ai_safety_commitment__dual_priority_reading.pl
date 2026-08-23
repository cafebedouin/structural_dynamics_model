% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety Dual-Priority Non-Competing Commitment
 *   domain: technological/governance/epistemic
 *
 * SUMMARY:
 *   This constraint is the dual-priority reading of the contested
 *   ai_safety_commitment kernel. It instantiates the claim that AI safety
 *   requires addressing both existential risk and near-term harms as
 *   non-competing priorities. The reading attempts a synthesis that treats
 *   both temporal horizons as simultaneously central, enforced through
 *   conference programming, funding eligibility, and professional norms
 *   within the AI safety field. Sibling readings instantiate exclusive x-risk
 *   and near-term frames. This constraint's structural delta is a victim set
 *   that is the union of both affected populations and a resource-allocation
 *   mechanism that splits attention and funding across an expansive agenda.
 *
 * KEY AGENTS:
 *   - umbrella_ai_safety_institutions (agenda_setter / institutional / identity_locked): Enforce the dual-priority frame to maintain coalition unity and institutional funding access.
 *   - large_tech_funders (beneficiary / powerful / mobile): Benefit from a broad, slow-moving field that avoids hard tradeoffs.
 *   - existential_risk_researchers (payer / moderate / identity_locked): Forced to share narrative and funding space with near-term work they often view as diluting urgent extinction prevention.
 *   - near_term_harm_advocates (payer / moderate / identity_locked): Forced to share space with speculative long-term work, diffusing focus from concrete present harms.
 *   - present_day_affected_communities (payer / powerless / trapped): Bear lived costs of diluted, under-resourced interventions.
 *   - independent_policy_analysts (observer / analytical): Note the incoherence under scarcity but are marginalized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.58).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.65).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety Dual-Priority Non-Competing Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technological/governance/epistemic").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'be642726-3933-4565-8137-4a426901053c').
narrative_ontology:cs_kernel_codification('be642726-3933-4565-8137-4a426901053c', distributed).
narrative_ontology:cs_authority_grounding('be642726-3933-4565-8137-4a426901053c', distributed).
narrative_ontology:cs_reading_relation('be642726-3933-4565-8137-4a426901053c', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('be642726-3933-4565-8137-4a426901053c', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('be642726-3933-4565-8137-4a426901053c', foundational, obligatory_dual_mandate).
narrative_ontology:cs_axiom_status(obligatory_dual_mandate, holdable).
narrative_ontology:cs_axiom_grounding('be642726-3933-4565-8137-4a426901053c', obligatory_dual_mandate, deontological).
narrative_ontology:cs_reference_frame('be642726-3933-4565-8137-4a426901053c', unified_safety_field).
narrative_ontology:cs_drift_state('be642726-3933-4565-8137-4a426901053c', contemporary_resource_scarcity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be642726-3933-4565-8137-4a426901053c', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, umbrella_ai_safety_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, large_tech_funders).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harm_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, present_day_affected_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the dual-priority framing through conferences, funding portfolios, and policy statements. They enforce inclusion of both near-term and existential risk in all official AI safety agendas. Their institutional survival depends on maintaining coalition unity across both camps; abandoning either priority would fragment their constituency and donor base.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, umbrella_ai_safety_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Provide substantial funding to AI safety organizations. They benefit from a broad, contested field that does not force immediate hard choices between present harms and speculative future risks, allowing continued deployment of profitable systems under the umbrella of safety work in progress. They can redirect funding if the frame shifts.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, large_tech_funders, beneficiary,
    powerful, biographical, mobile, global).

% Focus on extinction risks from advanced AI. They are structurally required to acknowledge near-term harms as equally valid safety priorities to retain access to conferences, funding, and professional standing, even when they believe this dilutes urgent long-term work. Exit means leaving the field or accepting outsider status.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, payer,
    moderate, civilizational, identity_locked, global).

% Document and resist present-day AI harms such as bias, surveillance, and labor exploitation. They are required to frame their work within the broader AI safety umbrella and share narrative space with speculative long-term risks to secure funding and institutional attention, which diffuses focus from immediate interventions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harm_advocates, payer,
    moderate, biographical, identity_locked, global).

% Bear the lived costs of algorithmic harm and surveillance. They receive rhetorical inclusion in the dual-priority framework but see diluted funding and delayed concrete intervention because resources are split across an expansive, abstract agenda. They have no meaningful voice in setting priorities.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, present_day_affected_communities, payer,
    powerless, immediate, trapped, local).

% Observe that the dual-priority frame prevents focused accountability by treating all priorities as equally central. They note that resource scarcity forces implicit competition despite the non-competing rhetoric, but their analysis is marginalized as splitting the movement.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, independent_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, umbrella_ai_safety_institutions).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents destructive schism in the AI safety field by creating a rhetorical and institutional umbrella large enough to contain both near-term and existential risk researchers, maintaining a coalition capable of influencing policy and funding flows.
% TRANSFER_FUNCTION: Moves credibility, funding, and labor between existential risk and near-term harm portfolios, forcing single-priority actors to subsidize a diffuse agenda that spreads resources thin across both temporal horizons.
% ABSENT_VOICES: Pure single-priority advocates on both sides who would prefer exclusive focus; affected populations who need concentrated intervention rather than divided attention; and funders who would demand measurable outcomes from a narrower portfolio.
% DISAPPEARANCE_RATIONALE: If the dual-priority commitment vanished, some actors would reorganize around pure existential risk advocacy, others around pure near-term harm advocacy, and the field would likely fragment into competing movements with separate funding streams and policy asks. Whether this fragmentation would improve or worsen overall outcomes is disputed.
% FOUNDING_PROBLEM: The AI safety field risked fragmentation and resource dissipation through internal conflict between long-term and short-term priorities, reducing its overall influence on AI development and governance.
% FOUNDING_PROBLEM_CORROBORATION: Umbrella institutions and some neutral field historians attest the fragmentation risk was real. Single-priority advocates and affected-community organizers outside the benefiting parties attest the founding threat was overstated and the arrangement now persists primarily to protect institutional legitimacy.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, contested).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the dual-priority frame structurally transfers resources and attention away from focused interventions toward a diffuse, incoherent agenda. Suppression (0.65) reflects the active professional enforcement required to stigmatize single-priority deviation. Theater_ratio (0.60) has risen over the interval as the non-competing claim became increasingly performative: conference panels include both topics but funding allocations reveal implicit competition. Accessibility_collapse (0.50) is moderate because pure single-priority alternatives exist but carry professional stigma. Resistance (0.55) reflects growing pushback from both camps against the forced marriage. Temporal measurements share a single grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats experience the constraint as necessary coalition management that keeps the field viable. The payer seats experience it as a dilution of their urgent priorities in service of institutional maintenance. The affected_communities seat experiences it as exclusion dressed as inclusion. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Umbrella institutions are low-d beneficiaries: they collect legitimacy and funding access from the big-tent frame, and their exit is identity-locked to the coalition. Large_tech_funders are also low-d but mobile. Existential_risk_researchers and near_term_harm_advocates are high-d targets: the constraint extracts focused effort and redirects it into dual-priority rhetoric; their identity-locked exit amplifies effective extraction. Present_day_affected_communities are the highest-d targets, with trapped exit and local scope. The observer seat sits at analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was field fragmentation. The constraint may be experiencing mandatrophy: umbrella institutions claim fragmentation is still the primary threat, while critics claim the field now suffers from lack of concrete outcomes due to the dual-priority dilution. The founding_problem_status is contested, which prevents automatic classification as either live coordination or dead extraction. The theater_ratio trajectory suggests increasing performance relative to function, consistent with early mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_coherence_test,
    'Does resource scarcity under current AI safety budgets make the dual-priority framework operationally coherent, or does it force zero-sum tradeoffs that contradict the non-competing premise?',
    'Audit of funding allocations showing whether increased near-term spending correlates with decreased x-risk spending, holding total budgets constant.',
    'If zero-sum, the non-competing axiom is empirically falsified and the constraint functions as extraction from both priorities; if not, the frame is structurally viable as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_coherence_test, empirical, 'Whether resource scarcity falsifies the non-competing priority thesis.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural or internalized?',
    'Post-exit suppression trajectory for researchers who leave the dual-priority field: if professional sanctions persist after institutional departure, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the AI safety field.').

omega_variable(
    synthesis_vs_strategic_truce,
    'Is the dual-priority reading a genuine normative synthesis or a strategic truce to prevent field fragmentation?',
    'Historical analysis of funding and rhetoric: did the dual-priority frame emerge from moral argument about obligations to both present and future, or from coalition-building among funders and institutions?',
    'If strategic truce, the coordination function is cover for institutional maintenance and the constraint trends toward snare; if genuine synthesis, it may remain a tangled rope or evolve toward scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_vs_strategic_truce, conceptual, 'Whether the dual-priority frame is a moral synthesis or institutional truce.').

omega_variable(
    kernel_fragmentation,
    'Does the ai_safety_commitment kernel have a determinate referent, or does it fragment into structurally distinct constraints under each reading?',
    'Engine classification divergence across the three kernel readings: if siblings compute as different constraint types, the kernel is irreducibly polysemous.',
    'If the kernel fragments, it cannot be treated as one constraint with adjustable parameters; it requires separate stories with linked network edges, as per the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_fragmentation, conceptual, 'Kernel referential unity vs reading-dependent fragmentation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t2, ai_safety_commitment__dual_priority_reading, theater_ratio, 2, 0.38).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__dual_priority_reading, theater_ratio, 4, 0.45).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__dual_priority_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__dual_priority_reading, theater_ratio, 8, 0.6).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_s_be_t2, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__dual_priority_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__dual_priority_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__dual_priority_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ai_s_su_t2, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__dual_priority_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__dual_priority_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__dual_priority_reading, suppression_requirement, 8, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_safety_commitment kernel, instantiating the dual-priority synthesis; siblings instantiate exclusive x-risk and near-term readings. Decomposition follows the epsilon-invariance principle: each reading has a distinct beneficiary/victim structure and epsilon, linked through network edges for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
