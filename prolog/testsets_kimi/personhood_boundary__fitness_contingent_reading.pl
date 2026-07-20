% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the fitness_contingent_reading of the contested
 *   personhood_boundary kernel. It holds that moral standing is contingent on
 *   demonstrated fitness (rational, physical, or genetic), and that
 *   pre-fitness entitiesâincluding infants who fail the testâlack moral
 *   standing and may be excluded from the community's protections. The
 *   state-medical authority administers the fitness test and enforces the
 *   boundary. Moral community members benefit from the clear allocation of
 *   rights and resources; infants and their parents bear the costs. The
 *   constraint is claimed as tangled_rope because it provides a genuine
 *   coordination function (a determinate moral boundary) while structurally
 *   extracting standing from a vulnerable class. Sibling readings include
 *   birth_threshold_reading (all born humans have standing) and
 *   potential_based_reading (standing grounded in developmental potential).
 *
 * KEY AGENTS:
 *   - state_medical_authority: Agenda-setter (institutional/constrained) â defines fitness criteria and enforces exclusion.
 *   - moral_community_members: Beneficiary (organized/identity_locked) â receive standing and resource priority.
 *   - infants_pre_fitness: Primary target (powerless/trapped) â denied standing and protections.
 *   - parents_of_excluded: Secondary target (moderate/constrained) â bear emotional and compliance costs.
 *   - bioethicist_critics: Analytical observer (analytical/analytical) â contests the boundary from human-rights frameworks.
 *   - disability_rights_advocates: Excluded voice (organized/constrained) â would object but historically kept from standard-setting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.78).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.72).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '1a145f1f-89f3-44b4-9ed4-7dd019dd6e29').
narrative_ontology:cs_kernel_codification('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', formalized).
narrative_ontology:cs_authority_grounding('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', expertise).
narrative_ontology:cs_interpretation_layer_present('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29').
narrative_ontology:cs_reading_relation('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', foundational, moral_standing_requires_demonstrated_capacity).
narrative_ontology:cs_axiom_status(moral_standing_requires_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', moral_standing_requires_demonstrated_capacity, deontological).
narrative_ontology:cs_axiom('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', secondary, state_may_exclude_pre_fitness_entities).
narrative_ontology:cs_axiom_status(state_may_exclude_pre_fitness_entities, holdable).
narrative_ontology:cs_axiom_grounding('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', state_may_exclude_pre_fitness_entities, conventional).
narrative_ontology:cs_reference_frame('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', fitness_based_moral_community).
narrative_ontology:cs_drift_state('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', contemporary_human_rights_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1a145f1f-89f3-44b4-9ed4-7dd019dd6e29', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, moral_community_members).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, infants_pre_fitness).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, parents_of_excluded).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers fitness criteria for personhood; certifies which infants meet the threshold and which lack standing. Exercises legal power to exclude pre-fitness entities from protections and enforces this boundary through medical and legal institutions.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_medical_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receive the protections and rights of personhood by virtue of passing the fitness threshold. Their standing is secured by the boundary, and public resources are allocated away from excluded entities toward the community's needs.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_community_members, beneficiary,
    organized, generational, identity_locked, national).

% Born or pre-born entities who have not yet demonstrated the required fitness criteria. They are denied moral standing, legal protections, and sometimes life-sustaining care. They cannot exit their condition or the jurisdiction of the authority.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, infants_pre_fitness, payer,
    powerless, immediate, trapped, local).

% Bear the emotional, social, and legal costs of their offspring's exclusion from moral standing. They may be compelled to surrender or not treat infants deemed unfit, with limited ability to challenge the fitness determination or emigrate with the infant.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, parents_of_excluded, payer,
    moderate, biographical, constrained, local).

% Analyze and publicly contest the fitness-contingent boundary from human-rights and disability-rights frameworks. They do not bear the constraint's costs directly but produce the arguments that erode its legitimacy.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, bioethicist_critics, observer,
    analytical, civilizational, analytical, global).

% Would object to the fitness criterion as discriminatory against disabled infants, but were historically excluded from the medical-ethical committees that set the standards.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate boundary for the moral community, solving the coordination problem of whom to protect, resource, and mourn by requiring a demonstrable capacity threshold.
% TRANSFER_FUNCTION: Transfers moral standing and its associated protections away from infants and entities that fail the fitness test, and transfers discretionary authority over life-and-death decisions to the state-medical apparatus.
% ABSENT_VOICES: Disability rights advocates and parental challengers are structurally excluded from the committees that define fitness criteria; their objections are ruled out of order as sentimentality or ignorance of medical science.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent boundary vanished, infants previously excluded would gain standing, state medical authorities would lose their gatekeeping power, resource allocation to neonatal and disabled care would shift, and the legal framework for infanticide and euthanasia of newborns would collapse.
% FOUNDING_PROBLEM: The problem of uncertain moral boundaries: without a clear criterion for personhood, societies cannot consistently allocate rights, duties, and medical resources, leading to arbitrary treatment of the vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Human rights institutions and disability rights advocates from outside the benefiting medical establishment attest that the founding problem is solved by universal dignity frameworks; the fitness-contingent reading's beneficiaries (state medical authorities) alone assert it remains live.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint strips all moral standing from a class of human beings, permitting their neglect or destruction; suppression (0.72) is high because the boundary requires active medical-legal enforcement to override parental and humanitarian resistance. Theater_ratio (0.45) reflects that the fitness criterion is partly presented as objective science and partly as a performative ritual of medical gatekeeping. Accessibility_collapse (0.70) is high because once the fitness framework is accepted, alternative universal-dignity frameworks are ruled out for pre-fitness entities. Resistance (0.55) is moderate because human-rights and disability-rights movements have contested the boundary, though it persists in specialized bioethical discourse. The temporal series show extraction peaking in the eugenics era, then declining as human-rights frameworks gained ground, while theater increased as the constraint's scientific legitimacy eroded and its maintenance became more performative.
 *
 * PERSPECTIVAL GAP:
 *   The state_medical_authority seat experiences the constraint as a necessary coordination mechanism for allocating scarce medical and social resources to those who can participate in community life. The infants_pre_fitness and parents_of_excluded seats experience it as an arbitrary and violent deprivation of standing. The engine computes this divergence from the structural asymmetry in power and exit: the agenda-setter has institutional power and constrained exit (career/ideological investment), while the targets are powerless and trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   State_medical_authority sits near the beneficiary end (low d) because it gains gatekeeping power and social control from the constraint, though as agenda-setter it also bears administrative costs. Moral_community_members are clear beneficiaries (low d) because their standing is secured by the boundary. Infants_pre_fitness are full targets (high d) because the constraint exists to deny them standing. Parents_of_excluded are intermediate targets (moderate-high d) because they bear secondary costs but are not the primary object of the boundary. Bioethicist_critics are analytical (d neutral).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâuncertain moral boundariesâwas genuinely live in early modern and eugenics-era medical ethics. However, the post-1945 human-rights consensus and disability-rights frameworks have provided alternative boundary mechanisms (birth threshold, universal dignity) that solve the coordination problem without the same extraction. The constraint's persistence in niche bioethics despite these alternatives is a mandatrophy signal: it continues to extract (deny standing) while its founding coordination function has been superseded. Classifying it as tangled_rope rather than snare captures the genuine coordination problem it once addressed, while the temporal measurements and founding_problem_status=dead flag the atrophied coordination and elevated theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the fitness_contingent_reading of kernel personhood_boundary; what structural element differentiates it from the birth_threshold_reading and potential_based_reading siblings?',
    'Comparative structural analysis of victim sets and authority grounding across the three readings.',
    'The fitness-contingent reading creates a victim set of born infants who fail the test, whereas birth-threshold removes all born infants from the victim set, and potential-based replaces current function with developmental capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural location of disagreement within the personhood boundary kernel.').

omega_variable(
    fitness_criterion_constructedness,
    'Is the fitness threshold a biologically natural kind or a socially constructed policy instrument?',
    'Historical comparison of fitness criteria across regimes (Sparta, early 20th century eugenics, contemporary bioethics) to assess stability.',
    'If constructed, the constraint''s coordination function is weaker than its extraction; if natural, the coordination claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_constructedness, empirical, 'Whether fitness threshold is natural kind or constructed policy.').

omega_variable(
    state_authority_nature,
    'Is the state authority to exclude pre-fitness entities delegated by the moral community, or an autonomous power created by the constraint?',
    'Jurisprudential and historical analysis of state power claims over infant life.',
    'Autonomous state authority raises suppression and extractiveness; delegated authority indicates more distributed coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_nature, conceptual, 'Nature of state authority under fitness-contingent personhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__fitness_contingent_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__fitness_contingent_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(pers_tr_t48, personhood_boundary__fitness_contingent_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(pers_tr_t64, personhood_boundary__fitness_contingent_reading, theater_ratio, 64, 0.44).
narrative_ontology:measurement(pers_tr_t80, personhood_boundary__fitness_contingent_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__fitness_contingent_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__fitness_contingent_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(pers_be_t48, personhood_boundary__fitness_contingent_reading, base_extractiveness, 48, 0.8).
narrative_ontology:measurement(pers_be_t64, personhood_boundary__fitness_contingent_reading, base_extractiveness, 64, 0.79).
narrative_ontology:measurement(pers_be_t80, personhood_boundary__fitness_contingent_reading, base_extractiveness, 80, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__fitness_contingent_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__fitness_contingent_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(pers_su_t48, personhood_boundary__fitness_contingent_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement(pers_su_t64, personhood_boundary__fitness_contingent_reading, suppression_requirement, 64, 0.74).
narrative_ontology:measurement(pers_su_t80, personhood_boundary__fitness_contingent_reading, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the personhood_boundary kernel, decomposed from the colloquial label 'personhood' into structurally distinct claims: birth_threshold_reading (standing at birth), fitness_contingent_reading (standing at demonstrated fitness), and potential_based_reading (standing at potential for agency). Each reading has a different victim set and epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
