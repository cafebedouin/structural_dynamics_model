% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The competence reading of the preparedness commitment kernel holds that
 *   preparedness routines — drills, exercises, simulations, and recurring
 *   training — are live exercised knowledge that genuinely maintains
 *   operational capacity across generations. This reading asserts high
 *   adaptive capacity: drills test real decision-making under stress,
 *   generational turnover is absorbed through effective training pipelines,
 *   and the D5 break (the point where institutional memory fails to transmit)
 *   is avoided or contained. The constraint is the institutional commitment
 *   to sustain these exercised routines as a standing arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.15).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'a1db7947-7b82-45f5-b86a-8c8092fa5d27').
narrative_ontology:cs_kernel_codification('a1db7947-7b82-45f5-b86a-8c8092fa5d27', distributed).
narrative_ontology:cs_authority_grounding('a1db7947-7b82-45f5-b86a-8c8092fa5d27', practice).
narrative_ontology:cs_interpretation_layer_present('a1db7947-7b82-45f5-b86a-8c8092fa5d27').
narrative_ontology:cs_reading_relation('a1db7947-7b82-45f5-b86a-8c8092fa5d27', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1db7947-7b82-45f5-b86a-8c8092fa5d27', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('a1db7947-7b82-45f5-b86a-8c8092fa5d27', foundational, preparedness_requires_live_exercise).
narrative_ontology:cs_axiom_status(preparedness_requires_live_exercise, holdable).
narrative_ontology:cs_axiom_grounding('a1db7947-7b82-45f5-b86a-8c8092fa5d27', preparedness_requires_live_exercise, empirically_contingent).
narrative_ontology:cs_axiom('a1db7947-7b82-45f5-b86a-8c8092fa5d27', foundational, generational_turnover_absorbed_through_training).
narrative_ontology:cs_axiom_status(generational_turnover_absorbed_through_training, holdable).
narrative_ontology:cs_axiom_grounding('a1db7947-7b82-45f5-b86a-8c8092fa5d27', generational_turnover_absorbed_through_training, empirically_contingent).
narrative_ontology:cs_reference_frame('a1db7947-7b82-45f5-b86a-8c8092fa5d27', live_exercised_competence_framework).
narrative_ontology:cs_drift_state('a1db7947-7b82-45f5-b86a-8c8092fa5d27', contemporary_institutional_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a1db7947-7b82-45f5-b86a-8c8092fa5d27', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, public_citizens).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, intergenerational_responsibility_norm).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, exercised_competence_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates and funds preparedness exercises, drills, and training cycles. Bears budgetary cost but gains organizational resilience and political legitimacy. Can reallocate resources across domains; exit means shifting preparedness posture rather than abandoning it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, institutional_leadership, beneficiary).

% Invest significant time in drills, simulations, and recurring certification. Gain operational competence that directly affects survival and mission success. Exit is constrained by professional identity, certification requirements, and career path dependence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, frontline_responders, beneficiary).

% Receive the protective benefit of maintained response capacity without direct participation in exercises. Bear indirect cost through taxation. Can relocate jurisdictionally but cannot individually opt out of the preparedness system's effects.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, public_citizens, beneficiary,
    organized, generational, mobile, national).

% Neighboring or peer institutions that free-ride on regional preparedness externalities or resist mutual-aid cost-sharing. Would object to bearing proportional exercise costs but are structurally excluded from the commitment negotiation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, competing_jurisdictions, excluded,
    institutional, generational, constrained, global).

% Studies preparedness systems across regimes and eras. Sees the full structural pattern of which institutions sustain live competence versus which perform memorial rituals. Bears no direct cost or benefit from any specific preparedness constraint.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operational readiness for disaster response across generational personnel turnover through exercised routines that test real decision-making under stress.
% TRANSFER_FUNCTION: Moves training investment, exercise effort, and institutional attention from current personnel to future operational capacity, ensuring that when disaster strikes, the response apparatus functions without relearning.
% ABSENT_VOICES: Future generations who bear the consequences of preparedness failure but cannot participate in current commitment decisions; marginalized communities whose specific vulnerabilities are not exercised in standard drills.
% DISAPPEARANCE_RATIONALE: Without exercised preparedness routines, institutional memory decays within 5-7 years, response coordination fails under novel stress, and disasters produce cascading failures that overwhelm ad-hoc improvisation.
% FOUNDING_PROBLEM: The problem of maintaining disaster response capability across personnel turnover, organizational change, and the long intervals between major events — ensuring that the institution remembers how to act when the event occurs.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management literature (e.g., FEMA after-action reports, IAEM standards), institutional continuity studies from organizational science (e.g., Weick & Sutcliffe on high-reliability organizations), and independent audits of exercise programs (GAO, NAS) all confirm the founding problem persists and the competence reading's practices address it.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.15) because the arrangement's costs (training time, exercise budgets) are broadly proportional to the coordination benefit (maintained response capacity). Low suppression (0.20) because participation is largely professional norm rather than coercion — responders recognize the value. Very low theater (0.10) because exercises genuinely test and improve capability rather than perform compliance. Moderate accessibility collapse (0.40) because alternative preparedness models exist but this exercised-routine framework dominates practice. Low resistance (0.15) because the founding problem (turnover + long intervals) remains live and the solution is empirically validated.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (institutional leadership) experiences the constraint as a voluntary coordination investment they administer. Frontline responders experience it as a costly but necessary professional obligation — they pay in time and effort but gain survival-relevant competence. The public experiences it as a background protective infrastructure. These seats compute differently: leadership sees rope (coordination they control), responders see tangled_rope (coordination they pay for but also depend on), public sees mountain-like background assurance. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is net beneficiary (d ~0.15): they control the agenda, gain legitimacy, and can arbitrage across preparedness domains. Frontline responders are near-symmetric (d ~0.50): they bear the direct exercise burden but the competence they gain is professionally and existentially valuable. Public citizens are beneficiaries (d ~0.20): diffuse benefit, diffuse indirect cost, mobile exit. Competing jurisdictions are excluded — their exclusion is the free-rider problem the mutual-aid framework tries to solve. The directionality derivation from beneficiary/payer declarations + exit options captures this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence reading explicitly denies mandatrophy: the founding problem (maintaining capability across turnover and long inter-event intervals) remains live, and the arrangement's core function (exercised competence) directly addresses it. No drift into memorial performance is asserted. If mandatrophy were present, it would manifest as rising theater_ratio and extractiveness over time — the measurements show stability, supporting the reading's claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the preparedness commitment a single kernel with multiple readings, or are competence/husk/hybrid structurally distinct constraints that merely share a label?',
    'Trace institutional histories: if the same formal commitment produces competence in some institutions and husk in others under similar conditions, the kernel is a single commitment with divergent readings. If the formal commitments themselves differ structurally, they are distinct constraints.',
    'If single kernel, the three readings are indexical variants and cross-reading contamination analysis applies. If distinct constraints, each must be authored separately with its own ε and stakeholder structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three declared readings instantiate one kernel or three constraints.').

omega_variable(
    competence_husk_boundary,
    'What structural markers distinguish live exercised competence from memorial performance in preparedness routines?',
    'Compare exercise designs: competence exercises inject novel stressors, require unscripted decisions, and measure adaptation; husk exercises follow fixed scripts, test compliance, and measure completion. Independent after-action review of exercise fidelity.',
    'If the boundary is porous, many ''competence'' institutions may be hybrid or husk. If sharp, the competence reading''s low theater claim is falsifiable per institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_husk_boundary, empirical, 'Operational distinction between real competence exercise and memorial performance.').

omega_variable(
    d5_break_measurement,
    'How can we measure whether the D5 break (institutional memory failure at ~5-7 generational cycles) is truly avoided versus merely delayed?',
    'Longitudinal tracking of exercise novelty, decision-quality metrics under novel stress, and personnel turnover rates against exercise design evolution. Requires 50+ year datasets.',
    'If D5 break is only delayed, the competence reading''s stability claim is time-limited and the constraint may drift toward hybrid/husk at longer horizons not captured in the 50-year interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(d5_break_measurement, empirical, 'Whether the competence reading''s claimed D5 containment is permanent or temporary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_comp_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(prep_comp_tr_t0, observed).
narrative_ontology:measurement(prep_comp_tr_t10, preparedness_commitment__competence_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(prep_comp_tr_t10, observed).
narrative_ontology:measurement(prep_comp_tr_t20, preparedness_commitment__competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(prep_comp_tr_t20, observed).
narrative_ontology:measurement(prep_comp_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(prep_comp_tr_t30, observed).
narrative_ontology:measurement(prep_comp_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(prep_comp_tr_t40, observed).
narrative_ontology:measurement(prep_comp_tr_t50, preparedness_commitment__competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement_basis(prep_comp_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prep_comp_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(prep_comp_be_t0, observed).
narrative_ontology:measurement(prep_comp_be_t10, preparedness_commitment__competence_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(prep_comp_be_t10, observed).
narrative_ontology:measurement(prep_comp_be_t20, preparedness_commitment__competence_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(prep_comp_be_t20, observed).
narrative_ontology:measurement(prep_comp_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement_basis(prep_comp_be_t30, observed).
narrative_ontology:measurement(prep_comp_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(prep_comp_be_t40, observed).
narrative_ontology:measurement(prep_comp_be_t50, preparedness_commitment__competence_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(prep_comp_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_comp_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(prep_comp_su_t0, observed).
narrative_ontology:measurement(prep_comp_su_t10, preparedness_commitment__competence_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(prep_comp_su_t10, observed).
narrative_ontology:measurement(prep_comp_su_t20, preparedness_commitment__competence_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(prep_comp_su_t20, observed).
narrative_ontology:measurement(prep_comp_su_t30, preparedness_commitment__competence_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(prep_comp_su_t30, observed).
narrative_ontology:measurement(prep_comp_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(prep_comp_su_t40, observed).
narrative_ontology:measurement(prep_comp_su_t50, preparedness_commitment__competence_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement_basis(prep_comp_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, disaster_response_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, institutional_continuity_framework).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, mutual_aid_compacts).

% DUAL FORMULATION NOTE:
% This is the competence_reading of the preparedness_commitment kernel. The husk_reading and hybrid_reading are sibling constraints. The competence reading asserts the coordination function is genuine and dominant; the husk reading asserts it is largely performative; the hybrid reading asserts both elements coexist structurally. All three share the kernel_id but instantiate different constraints with different ε and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__competence_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_commitment__competence_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
