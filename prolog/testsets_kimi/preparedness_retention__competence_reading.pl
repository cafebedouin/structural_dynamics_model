% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint instantiates the competence_reading of the
 *   preparedness_retention kernel: the claim that preparedness is live
 *   exercised knowledge, maintained through drills and inspections that
 *   preserve operational capacity. It asserts a low ceremony-to-competence
 *   ratio and treats resource allocation as optimizing for skill retention
 *   and adaptive capacity. The beneficiary is diffuse population safety; no
 *   structural victim is declared because the coordination function is
 *   primary and extraction is minimal. The kernel is contested by the
 *   husk_reading (memorial performance) and the hybrid_reading (stratified
 *   competence). This JSON contains ONLY the competence_reading as a clean,
 *   epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - general_public: Diffuse beneficiary (powerless/organized scope) â receives safety from maintained response capacity.
 *   - emergency_operators: Direct beneficiary (moderate/constrained) â retain professional competence through repeated practice.
 *   - emergency_management_agencies: Agenda-setter and secondary beneficiary (institutional/constrained) â design, fund, and administer drill regimes.
 *   - oversight_bodies: Analytical observer (institutional/analytical) â reviews whether expenditures produce competence or ceremony.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'ad20bc71-e360-4382-9cad-0e73180cb65e').
narrative_ontology:cs_kernel_codification('ad20bc71-e360-4382-9cad-0e73180cb65e', distributed).
narrative_ontology:cs_authority_grounding('ad20bc71-e360-4382-9cad-0e73180cb65e', practice).
narrative_ontology:cs_interpretation_layer_present('ad20bc71-e360-4382-9cad-0e73180cb65e').
narrative_ontology:cs_reading_relation('ad20bc71-e360-4382-9cad-0e73180cb65e', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('ad20bc71-e360-4382-9cad-0e73180cb65e', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('ad20bc71-e360-4382-9cad-0e73180cb65e', foundational, live_exercise_as_essential_retention).
narrative_ontology:cs_axiom_status(live_exercise_as_essential_retention, holdable).
narrative_ontology:cs_axiom_grounding('ad20bc71-e360-4382-9cad-0e73180cb65e', live_exercise_as_essential_retention, empirically_contingent).
narrative_ontology:cs_axiom('ad20bc71-e360-4382-9cad-0e73180cb65e', foundational, adaptive_capacity_over_compliance).
narrative_ontology:cs_axiom_status(adaptive_capacity_over_compliance, holdable).
narrative_ontology:cs_axiom_grounding('ad20bc71-e360-4382-9cad-0e73180cb65e', adaptive_capacity_over_compliance, instrumental).
narrative_ontology:cs_reference_frame('ad20bc71-e360-4382-9cad-0e73180cb65e', live_competence_practice).
narrative_ontology:cs_drift_state('ad20bc71-e360-4382-9cad-0e73180cb65e', contemporary_audit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad20bc71-e360-4382-9cad-0e73180cb65e', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, general_public).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_management_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from maintained emergency response capacity; lives and property are better protected when drills preserve operator competence. Cannot opt out of societal preparedness infrastructure.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, general_public, beneficiary,
    powerless, generational, constrained, national).

% Maintain live competence through repeated drills and inspections; their professional capacity, safety, and effective response depend on practiced readiness. Career path is specialized and exit is costly.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_operators, beneficiary,
    moderate, biographical, constrained, national).

% Design, fund, and administer drill programs and inspection regimes to keep response systems operationally ready. Derive institutional legitimacy and budget from the mission of preserving competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_management_agencies, beneficiary).

% Review preparedness budgets and exercise outcomes; assess whether funds produce measurable competence rather than ceremonial activity. Can mandate reforms but do not directly administer drills.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve perishable operational skills and inter-organizational coordination across long intervals between infrequent disasters, ensuring that response capacity does not decay to ineffectiveness.
% TRANSFER_FUNCTION: Moves public funds and personnel labor into repeated drills, simulations, and inspections, converting them into retained competence and faster, more adaptive emergency response that protects population safety.
% ABSENT_VOICES: Future disaster victims who have no voice before the event; taxpayers in jurisdictions where preparedness is underfunded; critics of drill regimes who are excluded from safety planning forums.
% DISAPPEARANCE_RATIONALE: Without live exercised knowledge, emergency response would revert to protocol reliance and improvisation under stress; inter-agency coordination would fragment and casualty curves would shift in the next major event.
% FOUNDING_PROBLEM: Operational skills and coordination protocols decay when not exercised; rare disasters provide insufficient real-world feedback to maintain readiness without synthetic practice.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster sociology (Wildavsky, Clarke) and post-event inquiries (Katrina, Grenfell) corroborate that skill atrophy and coordination failure follow periods without live practice; these sources are outside the beneficiary set.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint moves resources into competence preservation rather than rent capture. Suppression is minimal (0.12) because alternatives (no drills) are not actively suppressed; they are simply known to produce skill decay. Theater_ratio is very low (0.08) reflecting the reading's explicit claim of low ceremony-to-competence ratio. Accessibility_collapse is moderate-low (0.25): once the decay function of unpracticed skills is understood, alternatives lose appeal but do not structurally disappear. Resistance is near-zero (0.08) because genuine competence preservation meets little opposition. Metrics are authored independently of the rope claim; they describe the constraint's actual operation under this reading.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is mild in this reading because it is a genuine rope: all named parties are net beneficiaries of the coordination. The emergency_management_agencies experience the constraint as mission fulfillment and institutional purpose (low d, near-beneficiary); the general_public experiences it as background safety (low d); emergency_operators experience it as professional maintenance (low d). The engine will compute all seats toward the rope cluster. Divergence from the sibling husk_reading is large, but that is a different constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Both declared beneficiaries (general_public and emergency_operators) receive low directionality because the constraint subsidizes their safety and professional capacity. The emergency_management_agencies sit near the agenda-setter pole but are not extractive targets: their institutional benefit is mission coherence, not captured rent. No victims are declared, so no stakeholder is pushed toward the target pole. The structural relationship is symmetric-to-beneficial for all parties.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy mislabeling because the absence of victims, the absence of active enforcement, and the very low theater_ratio keep it outside snare or piton territory. If the constraint were secretly ceremonial, the metrics would show higher theater_ratio and some suppressed alternative (e.g., outcome-based audit), which would push classification toward tangled_rope or piton. The authored metrics do not show that pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the observed drill and inspection regime actually preserve live competence, or does it produce a ceremonial husk that merely performs readiness?',
    'Empirical assessment of exercise outcomes: measure whether drill scenarios produce adaptive, context-specific responses or scripted, predictable performances; compare competence retention metrics between high-ceremony/low-outcome programs and low-ceremony/high-outcome programs.',
    'If the regime is largely ceremonial, this constraint dissolves into the husk_reading (higher extraction, theater_ratio rises, beneficiary shifts to agencies performing legitimacy rather than population safety). If genuinely competent, the competence_reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Ambiguity between live competence and ceremonial performance in preparedness regimes.').

omega_variable(
    resource_allocation_efficiency,
    'Does resource allocation for drills optimize for skill retention and adaptive capacity, or does it drift toward visible but inefficient preparedness theater?',
    'Budget trace analysis: map expenditures to measurable competence outputs (retention curves, error rates in simulated events) versus ceremonial outputs (documentation volume, participation counts without skill verification).',
    'If resources flow to ceremony, extraction rises (taxpayer-to-agency transfer without safety benefit); if to competence, the rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Whether preparedness spending tracks competence or ceremony.').

omega_variable(
    stratification_ambiguity,
    'Is preparedness competence uniformly distributed across the institutional landscape, or is it stratified with technical competence in specialized agencies and ceremonial memory elsewhere?',
    'Cross-institutional competence auditing comparing specialized technical bodies against broad administrative or political institutions.',
    'If stratified, the hybrid_reading is activated and the competence_reading applies only to a subset of the preparedness system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_ambiguity, empirical, 'Whether competence retention is uniform or stratified across institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_competence_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(preparedness_competence_tr_t5, preparedness_retention__competence_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement(preparedness_competence_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(preparedness_competence_tr_t15, preparedness_retention__competence_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(preparedness_competence_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.08).

% Extraction over time
narrative_ontology:measurement(preparedness_competence_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(preparedness_competence_be_t5, preparedness_retention__competence_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(preparedness_competence_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(preparedness_competence_be_t15, preparedness_retention__competence_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(preparedness_competence_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.15).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the competence_reading of the preparedness_retention kernel, which decomposes into three structurally distinct claims: competence_reading (live exercised knowledge), husk_reading (memorial performance), and hybrid_reading (stratified competence). The kernel's ambiguity is in the phrase 'preparedness retention', which conflates live competence with ceremonial repetition. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
