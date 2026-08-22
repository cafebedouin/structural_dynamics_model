% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Fitness-Contingent Personhood Boundary
 *   domain: moral philosophy / historical ethics / commitment systems
 *
 * SUMMARY:
 *   This constraint instantiates the fitness-contingent reading of the
 *   personhood-boundary kernel: moral standing is not automatic at birth but
 *   must be earned through a demonstration of fitness. Pre-fitness
 *   entitiesâincluding newborn infants awaiting examinationâare
 *   structurally excluded from the moral community and lack protections
 *   against exposure, abandonment, or denial of care. Historically realized
 *   in practices ranging from ancient civic exposure to
 *   early-twentieth-century eugenic screening, the constraint is presented as
 *   a rational membership criterion while functioning to permit extraction
 *   from the most powerless. The claim is Snare because the coordination
 *   story (defining community membership) serves as cover for asymmetric
 *   extraction (disposability of pre-fit infants).
 *
 * KEY AGENTS:
 *   - state_magistracy (agenda_setter/institutional): sets fitness criteria and enforces exclusion
 *   - fit_community_members (beneficiary/organized): enjoy full standing and resource allocation
 *   - pre_fitness_infants (payer/powerless): bear the cost of exclusion and exposure
 *   - medical_examiners (agenda_setter/organized): administer the test and legitimate the boundary
 *   - humanitarian_dissenters (excluded/moderate): oppose the framework but are kept out of deliberation
 *   - moral_philosophers (observer/analytical): map the logical structure of the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.88).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.82).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral philosophy / historical ethics / commitment systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, 'c34fdcd0-02d4-4961-ac9a-8c6763be7a7a').
narrative_ontology:cs_kernel_codification('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', formalized).
narrative_ontology:cs_authority_grounding('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', lineage).
narrative_ontology:cs_interpretation_layer_present('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a').
narrative_ontology:cs_reading_relation('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', foundational, moral_standing_requires_demonstrated_fitness).
narrative_ontology:cs_axiom_status(moral_standing_requires_demonstrated_fitness, holdable).
narrative_ontology:cs_axiom_grounding('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', moral_standing_requires_demonstrated_fitness, deontological).
narrative_ontology:cs_reference_frame('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', fitness_based_moral_community).
narrative_ontology:cs_drift_state('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c34fdcd0-02d4-4961-ac9a-8c6763be7a7a', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fit_community_members).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_magistracy).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the fitness test and adjudicates borderline cases; holds the power to declare pre-fitness entities non-persons and enforce their exclusion from the moral community.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_magistracy, agenda_setter,
    institutional, generational, constrained, national).

% Enjoy full moral and legal standing; benefit from resource allocation, political recognition, and social membership denied to pre-fitness entities.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fit_community_members, beneficiary,
    organized, generational, constrained, national).

% Newborn or pre-tested infants subject to fitness examination; bear the total cost of exclusion, including exposure, abandonment, or denial of care if deemed unfit or simply awaiting test.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, trapped, local).

% Physicians or ritual examiners who apply the fitness test to infants; their professional authority and social role are constituted by the personhood boundary they enforce.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, medical_examiners, agenda_setter,
    organized, biographical, constrained, national).

% Ethical and religious opponents who assert universal infant personhood; structurally excluded from fitness tribunal decisions and often from public deliberation.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, humanitarian_dissenters, excluded,
    moderate, biographical, constrained, national).

% Analytical observers who map the logical structure of the personhood boundary and its distributional consequences across different readings of the kernel.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal community by defining a membership boundary based on demonstrated capacity, resolving uncertainty about who falls under moral protection.
% TRANSFER_FUNCTION: Transfers moral standing and its protections from pre-fitness entities to the fit community and state, effectively moving the right to life and care away from those who fail or await the fitness test.
% ABSENT_VOICES: Pre-fitness infants themselves cannot speak; humanitarian dissenters and universalist ethicists who reject fitness criteria are often excluded from tribunal and policy deliberation.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent boundary vanished, pre-fitness infants would gain standing immediately, exposure and abandonment practices would cease, and the state would lose its authority to exclude on fitness groundsâthe moral and legal community would reorganize around universal inclusion at birth or conception.
% FOUNDING_PROBLEM: Uncertainty or dispute over which human-like entities require moral protection and communal resources; need for a boundary that allocates membership based on capacity rather than mere species membership.
% FOUNDING_PROBLEM_CORROBORATION: Historical civic and medical authorities attest the problem was managing scarce resources and maintaining civic vitality. Modern human rights advocates and ethicists outside the beneficiary set attest the founding problem was solvable by alternative universalist boundaries and the fitness arrangement persists as extraction; no neutral corroboration exists from a seat unaligned with either side.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint permits total extraction from pre-fitness entitiesâup to and including deathâby denying them moral standing entirely. Suppression is high (0.82) because the framework requires state and medical enforcement to overcome parental attachment and natural resistance to infant disposal. Theater ratio is moderate (0.45): the fitness examination is partly functional (assessing health) and partly performative (legitimating a pre-determined exclusion). Accessibility collapse is high (0.85) because once the fitness framework is culturally internalized, universal-personhood alternatives become nearly unthinkable within the community. Resistance is moderate (0.55) because humanitarian and religious opposition persists but is systematically marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The state magistracy and fit community members experience the constraint as a necessary boundary-maintaining institution that preserves civic or social quality. Pre-fitness infants experience it as an existential threat with no voice or exit. The engine will compute these seats differently: low directionality for beneficiaries, extreme high directionality for the trapped, powerless infant seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State magistracy and fit community members are structural beneficiaries: the constraint concentrates power and resources in their hands and subsidizes their standing. Pre-fitness infants are the full target: they bear the total extraction, are powerless, have no exit, and are identity-locked to their developmental stage. Medical examiners sit closer to the beneficiary end because their professional authority is constituted by the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring declared victims and by the Snare gate: if there were no victims and the constraint were purely classificatory, it might read as Rope or Mountain. But the presence of pre-fitness infants in the victim set, combined with high suppression and active enforcement, blocks that misclassification. The founding problemâuncertainty about moral membershipâis arguably dead or contested, yet the arrangement persists because it serves the state's authority and the fit community's resource monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'How would the structural classification change if the birth-threshold or potential-based reading of the personhood boundary kernel were adopted instead of the fitness-contingent reading?',
    'Comparative analysis of the three readings'' victim sets and extraction profiles: birth-threshold removes pre-fitness infants from the victim set, while potential-based reclassifies some disabled infants.',
    'Adopting a sibling reading would shift the primary victim set and likely reclassify the constraint type (e.g., from Snare to Tangled Rope or Rope depending on the reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Structural delta between sibling readings of the personhood boundary kernel').

omega_variable(
    fitness_criterion_empirical_basis,
    'Is the fitness test grounded in genuine medical or empirical capacity assessment, or is it a constructed social mask for selective exclusion?',
    'Historical comparison of fitness criteria across cultures: if criteria vary arbitrarily with social need rather than biological capacity, the test is socially constructed.',
    'If purely constructed, extraction is higher and the coordination story collapses, pushing classification toward Snare; if empirically grounded in recoverable capacity, some coordination function may survive as Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_empirical_basis, empirical, 'Whether fitness criteria have empirical content or are socially constructed masks').

omega_variable(
    parental_resistance_structural_or_suppressed,
    'Does the constraint''s persistence depend primarily on structural state enforcement or on the suppression of parental bonding and resistance?',
    'Comparative analysis of exposure rates where state enforcement is weak: if exclusion persists through cultural normalization alone, suppression is internalized; if it collapses without state coercion, suppression is structural.',
    'Internalized suppression indicates higher effective extraction because the target population (parents) carry the constraint even where state power is absent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(parental_resistance_structural_or_suppressed, conceptual, 'Whether enforcement is state-structural or culturally internalized').

omega_variable(
    historical_specificity_vs_universal_kernel,
    'Is this constraint a historically localized instantiation or does it represent a recurring structural possibility in any personhood system?',
    'Cross-cultural analysis of personhood boundaries: if fitness-contingency recurs wherever resources are scarce, it is a universal attractor; if it appears only under specific regimes, it is historically contingent.',
    'If universal, the constraint is a persistent structural risk in any personhood kernel; if local, its classification as Snare may be specific to authoritarian contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_specificity_vs_universal_kernel, conceptual, 'Historical localization versus universal structural recurrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__fitness_contingent_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__fitness_contingent_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__fitness_contingent_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__fitness_contingent_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__fitness_contingent_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__fitness_contingent_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__fitness_contingent_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__fitness_contingent_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__fitness_contingent_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__fitness_contingent_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the personhood-boundary kernel. The kernel decomposes into three structurally distinct readingsâbirth-threshold, fitness-contingent, and potential-basedâeach with different victim sets and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
