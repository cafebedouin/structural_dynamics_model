% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Fitness-Contingent Personhood Boundary
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   The fitness-contingent reading of the personhood boundary kernel holds
 *   that moral standing is earned through demonstrated capacities
 *   (rationality, autonomy, self-consciousness, relational reciprocity).
 *   Entities that have not yet demonstrated or have lost these capacities —
 *   infants before developmental milestones, severely disabled persons,
 *   adults with advanced dementia — fall outside the moral community. The
 *   state or its delegated medical-scientific authorities administer the
 *   fitness test and define its criteria. This reading was dominant in early
 *   20th-century eugenics movements and persists in contemporary bioethical
 *   debates about neonatal personhood and end-of-life thresholds. The
 *   coordination function is a clear, administrable boundary for moral
 *   community; the extraction is the systematic denial of standing to
 *   vulnerable populations who cannot meet the criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.82).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.78).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood Boundary").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, 'c0eafd6d-a093-474d-b9d0-85c94ff8bc53').
narrative_ontology:cs_kernel_codification('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', distributed).
narrative_ontology:cs_authority_grounding('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', extraction).
narrative_ontology:cs_interpretation_layer_present('c0eafd6d-a093-474d-b9d0-85c94ff8bc53').
narrative_ontology:cs_reading_relation('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', personhood_boundary__potential_based_reading, influences).
narrative_ontology:cs_axiom('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', foundational, fitness_demonstration_required_for_personhood).
narrative_ontology:cs_axiom_status(fitness_demonstration_required_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', fitness_demonstration_required_for_personhood, empirically_contingent).
narrative_ontology:cs_axiom('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', secondary, state_authority_to_define_fitness_criteria).
narrative_ontology:cs_axiom_status(state_authority_to_define_fitness_criteria, holdable).
narrative_ontology:cs_axiom_grounding('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', state_authority_to_define_fitness_criteria, conventional).
narrative_ontology:cs_reference_frame('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', fitness_contingent_moral_community).
narrative_ontology:cs_drift_state('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c0eafd6d-a093-474d-b9d0-85c94ff8bc53', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, medical_establishment).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, eugenic_policy_architects).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_persons).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, cognitively_impaired_adults).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, elderly_with_dementia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, families_of_pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, families_of_pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, religious_traditionalists).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, rational_capacity_as_moral_ground).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, state_competence_to_define_personhood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates and enforces the fitness criteria for personhood through law, medical regulation, and resource allocation. Defines which capacities count as 'fitness' and administers the testing apparatus. Gains administrative control over vulnerable populations and resource distribution.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Provides the scientific-authoritative framework for fitness assessment (developmental milestones, cognitive testing, quality-of-life metrics). Gains professional jurisdiction over personhood determinations, research funding, and clinical authority. Can shift criteria through diagnostic revision (e.g., DSM changes, neonatal assessment protocols).
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, medical_establishment, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, medical_establishment, agenda_setter).

% Historical actors who designed and implemented fitness-contingent policies (sterilization laws, euthanasia programs, immigration restrictions). Gained political power, resource control, and ideological validation. Contemporary analogues exist in bioethics advisory roles and health policy frameworks that implicitly adopt fitness criteria.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, eugenic_policy_architects, beneficiary,
    powerful, biographical, arbitrage, national).

% Newborns and young children who have not yet demonstrated the required capacities (rationality, autonomy, self-consciousness). Cannot exit the pre-fitness category except by developing — which the constraint treats as uncertain. Bear the total cost: denial of legal personhood, medical treatment, and social recognition.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, identity_locked, local).

% Persons with severe cognitive or physical disabilities who cannot demonstrate fitness on the prescribed metrics. Identity-locked: their very condition is the reason for exclusion; they cannot 'pass' the test without ceasing to be who they are. Subject to institutionalization, denied treatment, and social erasure.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_persons, payer,
    powerless, biographical, identity_locked, local).

% Adults with intellectual disabilities, traumatic brain injury, or progressive neurological conditions who lose demonstrated fitness. Previously recognized as persons, they are reclassified when capacity falls below threshold. The constraint operates retrospectively — personhood is revocable.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, cognitively_impaired_adults, payer,
    powerless, biographical, identity_locked, local).

% Older adults with advanced dementia who lose the capacities that grounded their personhood. The fitness test becomes a mechanism for withdrawing care, legal standing, and familial authority. Their prior personhood status makes the revocation a distinct extraction: they paid into the moral community and are now expelled.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, elderly_with_dementia, payer,
    powerless, immediate, identity_locked, local).

% Parents, children, and caregivers of pre-fitness entities. Bear material costs (care burden without support), moral injury (complicity in or resistance to exclusion), and legal vulnerability. Some benefit incidentally from resource diversion (e.g., inheritance, care rationing). Exit is constrained: advocacy is possible but requires accepting the fitness framework's terms.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, families_of_pre_fitness_entities, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, families_of_pre_fitness_entities, beneficiary).

% Organize resistance to fitness-contingent personhood through rights frameworks, legal challenges, and alternative personhood models (social model of disability, capabilities approach). They do not pay the extraction directly but bear the cost of contestation. Their analytical seat sees the full structure: the fitness test as a moving boundary that always excludes the most vulnerable.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disability_rights_advocates, observer,
    organized, generational, analytical, global).

% Uphold birth-threshold or potential-based readings grounded in theological anthropology (imago dei, sanctity of life). Pay the cost of marginalization in secular bioethics discourse. Their exit is constrained: they cannot adopt the fitness framework without abandoning core commitments, but they cannot enforce their alternative in pluralistic polities.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, religious_traditionalists, observer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, religious_traditionalists, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an administrable, empirically verifiable boundary for the moral community that can be applied by state and medical institutions to allocate legal protections, medical resources, and social recognition.
% TRANSFER_FUNCTION: Moves moral standing, legal personhood, resource allocation, and care obligations from pre-fitness entities (who lose them) to state and medical authorities (who gain administrative control and resource discretion). The transfer is justified as 'rational allocation' but operates as status extraction.
% ABSENT_VOICES: The pre-fitness entities themselves — infants, severely disabled persons, advanced dementia patients — cannot speak in the bioethical discourse that determines their standing. Their voices are structurally excluded by the very criteria that define the moral community. Families and advocates speak for them but are filtered through the fitness framework's terms.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent reading vanished, legal personhood would revert to birth-threshold or potential-based defaults. Neonates and disabled persons would gain automatic protections; medical resource allocation would shift from 'quality of life' criteria to need-based or rights-based frameworks; state authority over personhood determinations would contract. The bioethical architecture built on capacity assessment would collapse.
% FOUNDING_PROBLEM: How to demarcate the moral community in a way that is administratively workable for modern states, conceptually defensible against religious metaphysics, and responsive to medical-scientific advances that create borderline cases (premature neonates, persistent vegetative states, genetic anomalies).
% FOUNDING_PROBLEM_CORROBORATION: The fitness-contingent reading's beneficiaries (state, medical establishment) attest the problem is live, citing advancing neonatal medicine and resource scarcity. Disability rights movements, religious traditions, and human rights frameworks (UN CRPD) attest the problem is dead or misdiagnosed: birth-threshold and capabilities approaches solve demarcation without extraction. The 1948 Universal Declaration and subsequent human rights instruments, authored outside the beneficiary set, presuppose universal personhood — corroborating that the founding problem was never fitness but universal recognition.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers moral standing — and with it legal protections, resource allocation, and social recognition — from pre-fitness entities to the authorities who define fitness. Suppression is high (0.78) because the constraint requires active legal and medical enforcement to maintain the boundary (institutionalization, sterilization, denial of treatment, 'life unworthy of life' determinations). Theater ratio is moderate (0.45): the coordination function (clear moral boundary) is genuine but increasingly performative as criteria expand to serve resource-allocation interests. Accessibility collapse (0.72) is high because alternative readings (birth-threshold, potential-based) are treated as conceptually incoherent within the fitness framework. Resistance (0.68) is substantial from disability rights movements, religious traditions, and human rights frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the state/medical seat, the constraint appears as necessary coordination: a workable boundary that prevents moral vagueness and allocates scarce care resources. From the pre-fitness entity seat (represented by families and advocates), the same structure is experienced as a death sentence — the boundary is the mechanism of exclusion. The engine computes this divergence from the structural data: beneficiaries with arbitrage-grade exit (policy architects can redefine criteria) versus targets with identity-locked exit (the pre-fitness cannot demonstrate what they by definition lack).
 *
 * DIRECTIONALITY LOGIC:
 *   State authority and medical establishment are structural beneficiaries (d near 0.1): they gain administrative power, resource control, and professional jurisdiction. Eugenic policy architects (historical) were concentrated beneficiaries. Pre-fitness infants, severely disabled persons, cognitively impaired adults, and elderly with dementia are full targets (d near 1.0): they bear the total cost of exclusion with identity-locked exit (cannot demonstrate fitness to escape). Families of pre-fitness entities are constrained payers (d ~0.7): they bear care burdens and moral injury but have some exit through advocacy. Disability rights advocates are observers (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'how to demarcate the moral community in a way that is administratively workable and conceptually defensible' — is contested. The fitness-contingent reading claims the problem is live (medical advances create new borderline cases). Critics argue the problem is dead: birth-threshold and potential-based readings provide stable boundaries without the extraction. The mandate has atrophied into extraction: the fitness test no longer primarily solves the demarcation problem but serves to legitimize resource denial. This is not a piton (no theatrical maintenance of a dead function) but a snare whose coordination cover has thinned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_fitness_contingent,
    'This constraint is one reading (fitness_contingent_reading) of the contested kernel personhood_boundary. What structural elements distinguish this reading from its siblings (birth_threshold_reading, potential_based_reading)?',
    'Compare victim sets, beneficiary structures, and enforcement mechanisms across the three readings. The fitness-contingent reading uniquely makes personhood dependent on demonstrated capacity rather than birth or potential.',
    'If the fitness test is the distinguishing structural element, then changes in what counts as ''fitness'' directly reconfigure the victim set — making this reading uniquely vulnerable to criterion drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_fitness_contingent, conceptual, 'Commitment-system framing: this constraint as a kernel reading').

omega_variable(
    fitness_criterion_drift,
    'How stable are the fitness criteria themselves? Do they function as a fixed standard or do they drift to serve the beneficiaries'' interests?',
    'Historical analysis of fitness criteria across regimes (eugenics-era intelligence thresholds, contemporary cognitive-capacity tests, proposed neonatal assessment protocols). Track whether criterion changes correlate with beneficiary interest shifts.',
    'If criteria drift to exclude groups that threaten beneficiary interests, the coordination function is cover and the constraint is a snare. If criteria are stable and independently justified, tangled_rope becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_drift, empirical, 'Whether the fitness threshold is a stable coordination standard or a movable extraction boundary').

omega_variable(
    internalized_suppression_of_families,
    'Is the suppression experienced by families of pre-fitness entities structural (legal denial of rights) or internalized (families come to believe their members lack moral worth)?',
    'Post-policy-change studies: when fitness-contingent policies are reversed, does the suppression of families persist? Comparative analysis of family advocacy in regimes with vs. without fitness-contingent personhood.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — families carry the exclusion with them after legal barriers fall. This would increase the constraint''s extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_families, empirical, 'Structural vs. internalized suppression mechanism for affected families').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pbfc_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pbfc_tr_t25, personhood_boundary__fitness_contingent_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(pbfc_tr_t50, personhood_boundary__fitness_contingent_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(pbfc_tr_t75, personhood_boundary__fitness_contingent_reading, theater_ratio, 75, 0.48).
narrative_ontology:measurement(pbfc_tr_t90, personhood_boundary__fitness_contingent_reading, theater_ratio, 90, 0.42).
narrative_ontology:measurement(pbfc_tr_t100, personhood_boundary__fitness_contingent_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(pbfc_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pbfc_be_t25, personhood_boundary__fitness_contingent_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(pbfc_be_t50, personhood_boundary__fitness_contingent_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(pbfc_be_t75, personhood_boundary__fitness_contingent_reading, base_extractiveness, 75, 0.78).
narrative_ontology:measurement(pbfc_be_t90, personhood_boundary__fitness_contingent_reading, base_extractiveness, 90, 0.72).
narrative_ontology:measurement(pbfc_be_t100, personhood_boundary__fitness_contingent_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pbfc_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pbfc_su_t25, personhood_boundary__fitness_contingent_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(pbfc_su_t50, personhood_boundary__fitness_contingent_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(pbfc_su_t75, personhood_boundary__fitness_contingent_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(pbfc_su_t90, personhood_boundary__fitness_contingent_reading, suppression_requirement, 90, 0.65).
narrative_ontology:measurement(pbfc_su_t100, personhood_boundary__fitness_contingent_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__fitness_contingent_reading, 0.08).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three readings with distinct victim sets and extraction profiles. birth_threshold_reading has near-zero extraction (mountain-like). potential_based_reading has moderate extraction (tangled_rope: coordinates around potential but extracts from severely disabled). fitness_contingent_reading has high extraction (snare: coordination cover for systematic exclusion). The upstream readings (birth_threshold, potential_based) are often cited to legitimize the downstream fitness-contingent reading through 'slippery slope' arguments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, institutional, 0.12).
constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, powerless, 0.95).
constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
