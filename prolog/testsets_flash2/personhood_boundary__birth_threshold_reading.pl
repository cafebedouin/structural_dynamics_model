% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Boundary: Birth Threshold Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'birth threshold' reading of the
 *   personhood boundary kernel. It asserts that personhood, and thus full
 *   moral and legal standing, begins unequivocally at birth for all humans.
 *   This reading is widely accepted in legal and ethical systems, functioning
 *   as a foundational 'mountain' that defines who counts as a protected
 *   subject. The metrics reflect its near-universal acceptance and the high
 *   suppression of alternative views.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.05).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.95).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca').
narrative_ontology:cs_kernel_codification('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', formalized).
narrative_ontology:cs_authority_grounding('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', lineage).
narrative_ontology:cs_interpretation_layer_present('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca').
narrative_ontology:cs_reading_relation('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', foundational, universal_post_birth_personhood).
narrative_ontology:cs_axiom_status(universal_post_birth_personhood, holdable).
narrative_ontology:cs_axiom_grounding('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', universal_post_birth_personhood, deontological).
narrative_ontology:cs_axiom('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', foundational, non_contingent_moral_standing).
narrative_ontology:cs_axiom_status(non_contingent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', non_contingent_moral_standing, deontological).
narrative_ontology:cs_reference_frame('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', universal_human_rights_framework).
narrative_ontology:cs_drift_state('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b36e2497-e8f1-42ed-a3ff-7ace6dc5d1ca', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, legal_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All born humans are granted full moral and legal standing, protecting them from arbitrary harm and ensuring their rights. This status is inherent and not contingent on any further criteria.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_humans, beneficiary,
    powerless, biographical, identity_locked, universal).

% The legal system codifies and enforces the birth threshold for personhood, treating all born humans as subjects of law with full protections. It defines homicide as the killing of a born human and prosecutes accordingly.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_system, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for a potential-based personhood criterion, which would grant standing based on the capacity for rational agency, potentially excluding severely disabled infants. Their arguments are largely outside the mainstream legal and ethical discourse that grounds personhood at birth.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, potential_based_advocates, excluded,
    moderate, generational, constrained, global).

% Advocates for personhood contingent on demonstrated fitness or specific capacities, which would allow for the exclusion of certain born individuals. Their views are generally considered extreme and are actively suppressed by the prevailing birth-threshold consensus.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, fitness_contingent_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unambiguous, and universally applicable boundary for moral and legal personhood, simplifying legal frameworks and ensuring consistent protection for all born individuals.
% TRANSFER_FUNCTION: Transfers full moral and legal standing to all born humans, ensuring their protection and rights, and imposing duties on others not to harm them. It also transfers the burden of proof for non-personhood to those who would deny it, which is effectively impossible post-birth.
% ABSENT_VOICES: Advocates for alternative personhood criteria (e.g., fitness-contingent, potential-based) are largely excluded from mainstream legal and ethical discourse, as their positions challenge the foundational birth-threshold axiom. Their arguments are often framed as dangerous or unethical.
% DISAPPEARANCE_RATIONALE: If the birth threshold for personhood vanished, the legal and ethical landscape would be thrown into chaos. The definition of homicide, rights, and duties would become immediately contested, leading to profound societal reorganization and potential re-evaluation of who counts as a protected human being.
% FOUNDING_PROBLEM: To establish a clear, non-arbitrary, and universally applicable criterion for moral and legal personhood, avoiding subjective or contingent definitions that could lead to arbitrary exclusion or discrimination.
% FOUNDING_PROBLEM_CORROBORATION: The problem of defining personhood remains live, as evidenced by ongoing debates in bioethics and philosophy. Legal scholars and human rights organizations universally corroborate the need for a clear, non-discriminatory personhood boundary, affirming the birth threshold as the most robust and least arbitrary solution.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, ExtMetricName, E),
    domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because this constraint primarily grants rights and protections, rather than extracting resources. Suppression is very high (0.95) because the legal and ethical systems actively suppress any attempts to deny personhood to born humans, treating such views as dangerous and discriminatory. Theater ratio is negligible (0.01) as the function of universal protection is genuinely performed. Accessibility collapse is high (0.98) as there are virtually no legitimate alternatives to granting personhood at birth in mainstream discourse. Resistance is low (0.02) because the consensus is strong.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans, this is a pure Mountain, an unchangeable natural law that grants their very existence moral weight. From the perspective of the legal system, it is a foundational principle that enables the consistent application of law. Alternative perspectives, which would challenge this boundary, are structurally suppressed and thus do not register as 'perspectives' within the dominant framework, but as 'excluded' voices.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans are full beneficiaries (d=0.0) as the constraint grants them inherent rights and protections. The legal system acts as an agenda-setter and enforcer, benefiting from the clarity and stability this boundary provides. Advocates for alternative personhood criteria are excluded, as their positions challenge the foundational premise of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine natural law, or a constructed constraint that benefits identifiable agents (born_humans, legal_system)?',
    'Analysis of historical and cross-cultural variations in personhood definitions. If significant variation exists, it suggests a constructed element; if universal and invariant, it supports natural law.',
    'If primarily constructed, the classification would shift from Mountain towards a Rope or Tangled Rope, acknowledging the active maintenance and potential for extraction (e.g., from those excluded by other readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between natural law and social construct for personhood at birth.').

omega_variable(
    personhood_boundary_kernel_context,
    'What are the full implications of this ''birth threshold'' reading compared to sibling readings like ''fitness-contingent'' or ''potential-based'' personhood?',
    'Detailed comparative analysis of legal and ethical consequences, victim sets, and enforcement mechanisms across all readings of the ''personhood_boundary'' kernel.',
    'This reading''s classification as a Mountain is robust only if its universal application is genuinely non-extractive. Sibling readings would likely compute as Snares or Tangled Ropes due to their exclusionary nature, highlighting the ''beneficiary'' aspect of this reading''s inclusiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_boundary_kernel_context, conceptual, 'This constraint is the ''birth_threshold_reading'' of the ''personhood_boundary'' kernel. Sibling readings include ''fitness_contingent_reading'' and ''potential_based_reading'', which would alter the victim set and the nature of moral standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(pers_tr_t25, personhood_boundary__birth_threshold_reading, theater_ratio, 25, 0.01).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__birth_threshold_reading, theater_ratio, 50, 0.01).
narrative_ontology:measurement(pers_tr_t75, personhood_boundary__birth_threshold_reading, theater_ratio, 75, 0.01).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__birth_threshold_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(pers_be_t25, personhood_boundary__birth_threshold_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__birth_threshold_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(pers_be_t75, personhood_boundary__birth_threshold_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__birth_threshold_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(pers_su_t25, personhood_boundary__birth_threshold_reading, suppression_requirement, 25, 0.95).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__birth_threshold_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(pers_su_t75, personhood_boundary__birth_threshold_reading, suppression_requirement, 75, 0.95).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__birth_threshold_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
