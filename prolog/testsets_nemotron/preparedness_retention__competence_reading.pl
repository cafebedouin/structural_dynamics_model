% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge — Competence Reading
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint story represents the competence_reading of the
 *   preparedness_retention kernel: the claim that drills and inspections are
 *   genuine competence-preserving practices. The kernel is contested —
 *   sibling readings (husk_reading, hybrid_reading) argue that much
 *   preparedness activity has become ceremonial or is stratified. This
 *   reading asserts low ceremony-to-competence ratio: resource allocation
 *   genuinely optimizes for skill retention and adaptive capacity. The
 *   beneficiary is population safety; the victim is fiscal efficiency only if
 *   investment exceeds the competence-maintenance frontier. The constraint
 *   functions as a rope: it solves a real coordination problem (maintaining
 *   competence across personnel turnover and low-frequency events) with
 *   minimal coercive overhead — institutions voluntarily exercise because
 *   they experience the competence benefit, and the population benefits
 *   without direct participation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.08).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Knowledge — Competence Reading").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'b2156476-071a-40e2-87ab-f3fc84bc0b34').
narrative_ontology:cs_kernel_codification('b2156476-071a-40e2-87ab-f3fc84bc0b34', formalized).
narrative_ontology:cs_authority_grounding('b2156476-071a-40e2-87ab-f3fc84bc0b34', lineage).
narrative_ontology:cs_interpretation_layer_present('b2156476-071a-40e2-87ab-f3fc84bc0b34').
narrative_ontology:cs_reading_relation('b2156476-071a-40e2-87ab-f3fc84bc0b34', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2156476-071a-40e2-87ab-f3fc84bc0b34', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('b2156476-071a-40e2-87ab-f3fc84bc0b34', foundational, competence_requires_live_exercise).
narrative_ontology:cs_axiom_status(competence_requires_live_exercise, holdable).
narrative_ontology:cs_axiom_grounding('b2156476-071a-40e2-87ab-f3fc84bc0b34', competence_requires_live_exercise, empirically_contingent).
narrative_ontology:cs_axiom('b2156476-071a-40e2-87ab-f3fc84bc0b34', secondary, drill_realism_preserves_adaptive_capacity).
narrative_ontology:cs_axiom_status(drill_realism_preserves_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('b2156476-071a-40e2-87ab-f3fc84bc0b34', drill_realism_preserves_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('b2156476-071a-40e2-87ab-f3fc84bc0b34', post_1953_institutionalized_exercise).
narrative_ontology:cs_drift_state('b2156476-071a-40e2-87ab-f3fc84bc0b34', contemporary_climate_adaptation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b2156476-071a-40e2-87ab-f3fc84bc0b34', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_at_risk).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, operational_institutions).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, fiscal_efficiency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, competence_preservation_through_practice).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, adaptive_capacity_requires_exercise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities in flood plains, seismic zones, or industrial hazard corridors. Their safety depends on institutions maintaining live competence for evacuation, shelter, and response. They cannot individually verify competence but experience outcomes when events occur. Exit means relocation, which is rarely feasible.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_at_risk, beneficiary,
    organized, biographical, constrained, regional).

% Water boards (Rijkswaterstaat), fire services, emergency management agencies. They design and run drills, maintain equipment, and hold the tacit knowledge. They benefit from the constraint because it funds and legitimizes their core activity — maintaining competence through practice. They also set the drill standards and inspection regimes.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, operational_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, operational_institutions, agenda_setter).

% Budget authorities (finance ministries, municipal treasuries) that fund preparedness. They bear the recurring cost of drills, inspections, and equipment maintenance. If preparedness is over-invested relative to demonstrated risk, resources are diverted from other public goods. They can reallocate budgets but face political resistance when cutting visible safety programs.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_efficiency, payer,
    institutional, immediate, arbitrage, national).

% Court of Audit, inspectorates, parliamentary committees. They evaluate whether drills and inspections actually maintain competence or have become ceremonial. Their assessments influence budget allocations and institutional mandates. They do not directly bear costs or collect benefits but shape the constraint's evolution.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, audit_and_oversight_bodies, observer,
    institutional, biographical, analytical, national).

% Firefighters, paramedics, dike wardens, crisis coordinators. Their individual and team competence is maintained through the drills the constraint mandates. They experience the constraint as skill preservation — the more realistic the exercise, the more their capacity is sustained. They can move between services but lose institution-specific tacit knowledge.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, frontline_responders, beneficiary,
    moderate, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains live operational competence for disaster response across institutions and communities through repeated, realistic exercise — replacing the need for each generation to rediscover procedures in crisis.
% TRANSFER_FUNCTION: Moves public funds (tax revenue) into institutional capacity (trained personnel, maintained equipment, exercised protocols) and distributes the resulting safety benefit to the at-risk population. The fiscal payer funds the institutional beneficiary's competence maintenance; the population receives the risk reduction.
% ABSENT_VOICES: Future generations who inherit the competence level maintained today — they cannot object to under-investment now. Residents in low-probability/high-consequence zones (e.g., distant flood plains) whose specific scenarios may be under-exercised because drills optimize for frequent threats.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — drills stopped, inspections ended, equipment maintenance lapsed — institutional competence would decay within 2–5 years. When a disaster next occurred, response would improvise from degraded knowledge, increasing casualties and damage. The world would rearrange toward higher disaster mortality and institutional failure.
% FOUNDING_PROBLEM: After the 1953 North Sea flood and subsequent near-misses, the Netherlands recognized that static plans and paper procedures decay without exercise. The founding problem was: how to maintain operational competence for low-frequency, high-consequence events across generations of personnel turnover.
% FOUNDING_PROBLEM_CORROBORATION: Independent after-action reports (1993/1995 near-floods, 2021 Limburg floods) confirm that exercised competence directly reduced casualties. The Dutch Safety Board (OVV) and Court of Audit attest that drill realism correlates with response effectiveness. No beneficiary institution claims the problem is solved — all report ongoing personnel turnover and evolving threat landscapes requiring continued exercise.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) — the constraint's operation primarily produces safety through competence, not extraction. The fiscal cost is the price of the coordination function, not rent. Suppression is low (0.08) — participation is largely voluntary for institutions (they value competence) and the population is not coerced. Theater ratio is low (0.15) — drills are designed for realism, not performance; after-action reviews focus on capability gaps, not compliance theater. Accessibility collapse is low (0.25) — alternative preparedness models exist (community-based, technology-mediated) and are not suppressed. Resistance is low (0.18) — the constraint faces budget pressure but not active opposition; institutions advocate for it.
 *
 * PERSPECTIVAL GAP:
 *   From the fiscal_efficiency seat, the constraint appears extractive (cost without visible return until disaster strikes). From operational_institutions and frontline_responders, it appears as essential coordination (they experience competence decay when drills lapse). The engine computes this divergence from the declared structural positions — the claimed rope type reflects the reading's own structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Population_at_risk and frontline_responders are structural beneficiaries: they receive safety/competence with d near 0.0 (subsidized by the constraint). Operational_institutions are dual: agenda_setters who also benefit (competence maintenance is their mission). Fiscal_efficiency is the payer: bears cost with d near 1.0 (target of extraction). Audit_bodies are analytical observers (d=0.5). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence across generations for low-frequency events) remains live — personnel turnover, climate change altering hazard profiles, and technological evolution all require continued exercise. No mandatrophy: the constraint's function has not atrophied. The hybrid_reading's claim of stratification is a separate constraint (different ε, different beneficiary/victim structure). This reading does not foreclose stratification — it asserts the *dominant* mode is competence-preserving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ceremony_competence_boundary,
    'At what ceremony-to-competence ratio does the constraint transition from rope to tangled_rope or snare?',
    'Longitudinal analysis of drill realism metrics (injection rate, surprise element, after-action depth) correlated with response performance in actual events. Threshold where marginal drill investment yields no measurable competence retention.',
    'If the boundary is crossed in practice, the constraint''s classification shifts from coordination to extraction. Current low theater ratio (0.15) suggests competence side, but the boundary location is unknown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_competence_boundary, empirical, 'Threshold where preparedness exercise becomes extractive performance').

omega_variable(
    reading_relations_structure,
    'Does the competence_reading logically foreclose the husk_reading, or do they coexist as descriptions of different institutional layers?',
    'Compare institutional drill regimes: if Rijkswaterstaat/water boards show low theater while municipal drills show high theater, the readings coexist (stratified). If all institutions show uniformly low theater, husk_reading is foreclosed within this framework.',
    'If forecloses, the kernel admits only one coherent reading. If coexists_with, the kernel is inherently stratified (supporting hybrid_reading). If influences, competence_reading''s resource allocation pressures husk_reading''s ceremonial layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relations_structure, conceptual, 'Logical relationship between competence and husk readings of the same kernel').

omega_variable(
    fiscal_efficiency_victim_status,
    'Is fiscal_efficiency a genuine victim (extraction) or the necessary cost of coordination (rope)?',
    'Cost-benefit analysis of marginal preparedness investment: if the last euro spent on drills reduces expected disaster loss by more than one euro, fiscal_efficiency is not a victim — the transfer is efficiency-enhancing. If marginal return < 1, the excess is extraction.',
    'If fiscal_efficiency is a genuine victim with asymmetric extraction, the constraint becomes tangled_rope. Current classification as rope assumes the transfer is the coordination price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_efficiency_victim_status, empirical, 'Whether fiscal cost represents extraction or coordination price').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(prep_tr_t40, observed).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__competence_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(prep_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.1).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement_basis(prep_be_t40, observed).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__competence_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement_basis(prep_be_t50, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This is the competence_reading of the preparedness_retention kernel. The husk_reading and hybrid_reading are sibling constraints from the same kernel. This reading claims low ceremony-to-competence ratio and genuine coordination; the others claim high theater and/or stratification. They share the kernel (the institutional commitment to preparedness) but differ in ε, beneficiary/victim structure, and classification. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__competence_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_retention__competence_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
