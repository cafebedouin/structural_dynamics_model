% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Reading of Separation of Powers
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Separation of powers under the functionalist reading is a constitutional
 *   doctrine permitting overlapping authority among the legislative,
 *   executive, and judicial branches, and legitimizing administrative
 *   agencies through intelligible-principle delegation. This reading
 *   instantiates one side of a three-way kernel contest with formalist and
 *   unitary-executive readings. The functionalist frame treats strict
 *   categorical separation as unworkable in a modern regulatory state and
 *   instead balances functional considerations to prevent tyranny while
 *   enabling governance. Agencies remain legitimate; Congress and the
 *   President share functions; courts defer to expertise. The constraint
 *   coordinates inter-branch governance but extracts from regulated entities
 *   by sustaining agency power against structural constitutional challenges.
 *
 * KEY AGENTS:
 *   - Federal judiciary (institutional/analytical): administers the doctrine through balancing tests and deference regimes.
 *   - Administrative agencies (institutional/constrained): primary beneficiaries of legitimizing doctrine.
 *   - Regulated entities (organized/constrained): bear the costs of sustained agency authority; their separation-of-powers challenges are typically rejected.
 *   - US Congress (institutional/constrained): beneficiary via delegation flexibility.
 *   - US President (institutional/constrained): beneficiary via flexible executive administration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.36).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.52).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Reading of Separation of Powers").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '46e30f6f-44f4-40e8-b3b3-a0e8bcea9779').
narrative_ontology:cs_kernel_codification('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', formalized).
narrative_ontology:cs_authority_grounding('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', lineage).
narrative_ontology:cs_interpretation_layer_present('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779').
narrative_ontology:cs_reading_relation('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', separation_of_powers_text__formalist_reading, forecloses).
narrative_ontology:cs_reading_relation('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', foundational, overlapping_authority_constitutionally_permissible).
narrative_ontology:cs_axiom_status(overlapping_authority_constitutionally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', overlapping_authority_constitutionally_permissible, conventional).
narrative_ontology:cs_axiom('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', foundational, intelligible_principle_validates_delegation).
narrative_ontology:cs_axiom_status(intelligible_principle_validates_delegation, holdable).
narrative_ontology:cs_axiom_grounding('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', intelligible_principle_validates_delegation, conventional).
narrative_ontology:cs_reference_frame('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', adaptive_governance_equilibrium).
narrative_ontology:cs_drift_state('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', contemporary_formalist_resurgence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46e30f6f-44f4-40e8-b3b3-a0e8bcea9779', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, us_congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, us_president).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_entities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the separation-of-powers framework through balancing tests, intelligible-principle review, and deference doctrines; maintains the functionalist reading by rejecting formalist categorical challenges to agency structure.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Exercise delegated legislative and executive authority; their constitutional survival depends on the functionalist reading's tolerance for overlapping functions and intelligible-principle delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Delegates broad policymaking authority to agencies under open-ended statutory standards; retains oversight capacity while avoiding granular regulatory specification.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, us_congress, beneficiary,
    institutional, generational, constrained, national).

% Exercises executive authority flexibly within the administrative state, including through appointments and policy directives that blur strict categorical boundaries between legislative and executive functions.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, us_president, beneficiary,
    institutional, generational, constrained, national).

% Subject to agency rulemaking and enforcement sustained by the functionalist framework; their constitutional challenges to agency structure on non-delegation or strict-separation grounds are typically rejected.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_entities, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of governmental power across legislative, executive, and administrative actors in a modern regulatory state by permitting overlapping authority and judicial deference to agency expertise, preventing either governmental paralysis or the accumulation of all power in a single branch.
% TRANSFER_FUNCTION: Moves constitutional legitimacy and operational authority to administrative agencies and coordinate branches; moves compliance burdens and limited structural recourse to regulated entities subject to agency action.
% ABSENT_VOICES: Formalist jurists and scholars who would enforce strict, impermeable categorical boundaries; regulated entities whose separation-of-powers challenges are rejected under the functionalist balancing framework but who would argue for agency invalidation.
% DISAPPEARANCE_RATIONALE: If the functionalist reading vanished, the constitutional basis for the modern administrative state would collapse; New Deal and post-New Deal agency structures would face invalidation under formalist non-delegation or strict separation doctrines, forcing a radical reorganization of federal governance around either formalist categorical boundaries or unitary executive control.
% FOUNDING_PROBLEM: How to maintain effective governance under a constitution of separated powers in a complex modern regulatory state without the paralysis that strict categorical boundaries would impose.
% FOUNDING_PROBLEM_CORROBORATION: Progressive-era and New Deal-era administrative reformers and legal realists attested to the necessity of flexible adaptation; contemporary formalist jurists and originalist scholars dispute that the Constitution permits such adaptation. Corroboration from outside the immediate beneficiary branches comes from administrative law historians and comparative constitutional scholars, though their accounts are themselves contested.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.36, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored to reflect the functionalist reading's actual operation: moderate extractiveness (0.36) because the doctrine channels real governance capacity but simultaneously insulates agency action from constitutional attack; moderate suppression (0.52) because the constraint's persistence requires active judicial maintenance of deference doctrines and the marginalization of formalist alternatives; rising theater_ratio (0.28) reflecting increasing performative defense of the framework as formalist challengers gain traction. Accessibility_collapse (0.62) captures that, within the functionalist framework, formalist alternatives are legally disfavored once the reading is accepted. Resistance (0.42) reflects ongoing formalist litigation and scholarly opposition.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and agencies experience this constraint as coordination (enabling governance across branches), while regulated entities experience it as extraction (their ability to invalidate agency structures on constitutional grounds is systematically denied). The engine computes this divergence from structural position: beneficiaries have constrained but institutionally powerful positions; payers have moderate power and constrained exit within the national regulatory space.
 *
 * DIRECTIONALITY LOGIC:
 *   Agencies, Congress, and the President are structural beneficiaries (low d) because the functionalist reading expands their legitimate authority. Regulated entities are structural targets (high d) because the same reading forecloses their preferred constitutional avenue of limiting agency power. The federal judiciary sits near symmetric (d ~0.5) because it both administers and is bound by the doctrinal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâgoverning a modern industrial nation under strict categorical separationâwas arguably live in the early twentieth century and remains contested today. The functionalist reading prevents mislabeling by requiring both coordination (inter-branch governance) and extraction (agency power sustained against challengers) to be present. If the coordination function were absent, this would be a snare enabling pure administrative tyranny; if extraction were absent, it would be a rope of inter-branch comity. Neither pure label fits because the same doctrinal structure that coordinates also insulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionalist_reading_scope,
    'Does the functionalist reading''s tolerance for overlapping authority rest on a genuine structural necessity of modern governance, or on an opportunistic interpretive move that benefits the administrative state?',
    'Comparative constitutional analysis of separation-of-powers regimes and empirical study of governance failure under formalist constraints.',
    'If structural necessity, the reading''s coordination function is primary and extraction secondary; if opportunistic, extraction rises toward snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalist_reading_scope, conceptual, 'Whether functionalism is necessitated by governance complexity or serves administrative state interests.').

omega_variable(
    delegation_boundary_ambiguity,
    'Where does permissible delegation under the intelligible-principle doctrine end and unconstitutional transfer of legislative power begin?',
    'Supreme Court case law evolution or formal constitutional amendment clarifying delegation standards.',
    'A clear boundary would reduce extraction from regulated entities by limiting agency discretion; absence of boundary sustains current extraction level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_boundary_ambiguity, conceptual, 'Ambiguity of the delegation boundary under functionalist reading.').

omega_variable(
    formalist_resurgence_pressure,
    'Is the current formalist resurgence a temporary political cycle or a structural shift in constitutional interpretation?',
    'Longitudinal analysis of judicial appointments and separation-of-powers jurisprudence over 20+ years.',
    'If structural shift, the functionalist reading may be reclassified as scaffold or piton; if cyclical, tangled_rope persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formalist_resurgence_pressure, empirical, 'Whether formalist challenge represents structural shift or cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__functionalist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__functionalist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__functionalist_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(sepa_tr_t50, separation_of_powers_text__functionalist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__functionalist_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__functionalist_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.34).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__functionalist_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(sepa_be_t50, separation_of_powers_text__functionalist_reading, base_extractiveness, 50, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__functionalist_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__functionalist_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__functionalist_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(sepa_su_t50, separation_of_powers_text__functionalist_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
