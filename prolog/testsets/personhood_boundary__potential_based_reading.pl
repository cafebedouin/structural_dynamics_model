% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood via Potential for Rational Agency
 *   domain: moral_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested personhood
 *   boundary kernel: the reading that grounds personhood in potential for
 *   rational agency. Under this reading, severely disabled infants without
 *   capacity or realistic potential for developing rational thought may lack
 *   full moral standing. This reading has deep roots in Enlightenment
 *   philosophy and remains influential in bioethics; it provides
 *   institutional authority for parental and medical decisions to assign
 *   lower moral weight to the interests of infants deemed to lack personhood
 *   potential. The constraint is CLAIMED as tangled_rope (it does coordinate
 *   resource allocation while imposing asymmetric extraction on the infants
 *   excluded from personhood). The measurement series track how suppression
 *   and theater have accumulated over the interval: enforcement intensity
 *   rose as disability advocacy challenged the framework, requiring more
 *   sophisticated argumentation and institutional defense to maintain the
 *   boundary. This is one constraint; the birth_threshold_reading and
 *   fitness_contingent_reading are separate constraints in the same kernel
 *   family.
 *
 * KEY AGENTS:
 *   - Severely disabled infants: entities excluded from full personhood, targets of differential moral treatment
 *   - Parental and medical authority: seats that assign personhood status and enforce differential treatment
 *   - Disability advocates (organized): bear costs of the framework, challenge its authority
 *   - Resource allocation systems (institutional): benefit from the framework's permission structure
 *   - Birth-threshold tradition (non-agent but vindicated): philosophical tradition that benefits from legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.68).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.71).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood via Potential for Rational Agency").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, '626be455-e519-4df0-ae56-6a35949894d6').
narrative_ontology:cs_kernel_codification('626be455-e519-4df0-ae56-6a35949894d6', distributed).
narrative_ontology:cs_authority_grounding('626be455-e519-4df0-ae56-6a35949894d6', lineage).
narrative_ontology:cs_interpretation_layer_present('626be455-e519-4df0-ae56-6a35949894d6').
narrative_ontology:cs_reading_relation('626be455-e519-4df0-ae56-6a35949894d6', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('626be455-e519-4df0-ae56-6a35949894d6', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('626be455-e519-4df0-ae56-6a35949894d6', foundational, rational_agency_necessary_for_full_personhood).
narrative_ontology:cs_axiom_status(rational_agency_necessary_for_full_personhood, holdable).
narrative_ontology:cs_axiom_grounding('626be455-e519-4df0-ae56-6a35949894d6', rational_agency_necessary_for_full_personhood, deontological).
narrative_ontology:cs_axiom('626be455-e519-4df0-ae56-6a35949894d6', foundational, potential_capacity_sufficient_for_exclusion).
narrative_ontology:cs_axiom_status(potential_capacity_sufficient_for_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('626be455-e519-4df0-ae56-6a35949894d6', potential_capacity_sufficient_for_exclusion, empirically_contingent).
narrative_ontology:cs_axiom('626be455-e519-4df0-ae56-6a35949894d6', secondary, parental_medical_authority_over_status_assignment).
narrative_ontology:cs_axiom_status(parental_medical_authority_over_status_assignment, holdable).
narrative_ontology:cs_axiom_grounding('626be455-e519-4df0-ae56-6a35949894d6', parental_medical_authority_over_status_assignment, conventional).
narrative_ontology:cs_reference_frame('626be455-e519-4df0-ae56-6a35949894d6', enlightenment_rationality_personhood_framework).
narrative_ontology:cs_drift_state('626be455-e519-4df0-ae56-6a35949894d6', contemporary_disability_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('626be455-e519-4df0-ae56-6a35949894d6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parental_medical_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, resource_allocation_systems).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, profoundly_cognitively_impaired_newborns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disability_advocates).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, rational_agency_as_personhood_criterion).
narrative_ontology:constraint_vindicates(personhood_boundary__potential_based_reading, parental_authority_over_medical_decisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Infants with severe neurological impairment, profound intellectual disability, or irreversible lack of capacity for rational thought. Under this reading, they are denied full moral standing on the grounds they lack or will never develop the potential for rational agency. Their exclusion from personhood status justifies differential moral weight assigned to their interests; medical and parental decisions may prioritize withdrawal of life-sustaining care or non-intervention in ways that would not be permitted for infants granted full personhood. They cannot consent to or resist the exclusion; their status depends entirely on medical/parental judgment of their potential.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, biographical, trapped, universal).

% Parents and physicians make judgments about which infants possess sufficient potential for rational agency to warrant full personhood status. This reading grants them the authority to assign lower moral standing to infants deemed to lack that potential. They exercise this authority through decisions about life-sustaining treatment, resource allocation, and end-of-life care. The framework empowers them to make difficult triage decisions with reduced moral constraint, justifying these decisions as respecting the hierarchy of personhood potentials.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parental_medical_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% Organizations and scholars who challenge the reading on grounds that it instrumentalizes disabled infants and opens pathways to devaluation and non-treatment. They argue that potential for rational agency is not a neutral criterion but a disguised fitness test that privileges certain neurotypes and disadvantages disabled people. They advocate for birth-threshold personhood instead. Their organizational capacity is real but faces institutional barriers: medical authority and parental decision-making are legally empowered to override their objections, and the framework treats their voices as outside legitimate discourse.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_advocates, payer,
    organized, biographical, constrained, universal).

% The Enlightenment-rationality philosophical tradition that grounds personhood in rational agency. This framework benefits from institutional adoption because the adoption provides intellectual legitimacy and legitimizes the entire tradition. As a non-agent, it collects no material benefit; it is vindicated by the constraint's operation and institutional acceptance.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethics_philosophical_tradition, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(personhood_boundary__potential_based_reading, bioethics_philosophical_tradition).

% Healthcare systems and public resource-allocation bodies benefit from this reading because it provides moral permission to allocate scarce intensive care resources away from infants deemed to lack personhood potential. Ventilators, ICU beds, and specialist care can be prioritized based on personhood status without triggering the same moral constraints that would apply under a birth-threshold reading.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, resource_allocation_systems, beneficiary,
    institutional, generational, arbitrage, universal).

% Philosophers and organizations who hold the birth-threshold reading are excluded from the authority structure grounded in the potential-based reading. Their objections are treated as outside rational discourse; they lack standing to challenge personhood-assignment decisions within this framework. They would argue for the birth_threshold_reading (a separate constraint) that treats all born humans as equal moral persons.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, birth_threshold_advocates, excluded,
    organized, biographical, constrained, universal).

% Documents the structural operation of this reading and tracks how the assignment of personhood status constrains moral reasoning and resource allocation. Measures rates of withdrawal-of-care decisions, differential resource allocation by assigned personhood status, and resistance from disability advocates. Does not hold authority within the framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, resource_allocation_systems).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled basis for allocating moral standing and medical resources: rather than treating all infants identically, it permits differential moral weight based on assessed potential for rational agency. This enables triage decisions in resource scarcity and End-of-Life care without treating all entities as absolute moral equals — solving a genuine problem of how to rank interests when resources are finite.
% TRANSFER_FUNCTION: Transfers moral authority from a universal criterion (birth) to a particularized one (potential for rational agency). This move transfers decision-making power to parents and medical professionals, who gain authority to exclude certain infants from full personhood status. It transfers moral weight away from infants deemed to lack personhood potential, permitting their interests to be overridden in resource allocation and end-of-life decisions.
% ABSENT_VOICES: Severely disabled individuals with lived experience (who cannot speak for infants), disability scholars and theorists who reject the reading on grounds it instrumentalizes disability, and the infants themselves (whose lack of agency is the basis for their exclusion). Birth-threshold advocates hold alternative readings but are not seated in decision-making structures; their objections are treated as external to legitimate discourse.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, medical decisions about severely disabled infants would be reframed under the birth-threshold reading or fitness-contingent reading, dramatically altering personhood status and legal protections. Resource allocation would be compelled to justify differential treatment differently. Courts and medical ethics boards would face institutional pressure to treat all born humans identically in moral standing, reorganizing end-of-life care and intensive-care protocols.
% FOUNDING_PROBLEM: How to allocate scarce medical resources (intensive care beds, ventilators, specialist expertise) when not every entity can receive maximal intervention, and how to frame end-of-life decisions for infants with severe neurological impairment. Early bioethics needed a framework that would permit triage and withdrawal-of-care decisions without treating all infants identically. The potential-based reading emerged to solve this: it grounds differential moral standing in rationality potential, offering a principled basis for resource allocation that appears objective and philosophical rather than arbitrary.
% FOUNDING_PROBLEM_CORROBORATION: Medical professionals and bioethicists trained in rationality-based frameworks attest the founding problem is live — resource scarcity requires triage decisions and the framework provides defensible criteria. Disability scholars and human-rights organizations attest the founding problem is framed incorrectly: scarcity need not be answered by differential personhood; the birth-threshold reading offers an alternative that solves scarcity through different institutional mechanisms. Historical analysis from outside both benefiting parties (archival bioethics scholarship, disability history) shows the potential-based framework gained dominance not because it uniquely solved scarcity, but because it resonated with Enlightenment rationality norms embedded in institutional philosophy. The scarcity problem was real; the framework choice was one option among alternatives.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval as institutional enforcement hardened against disability-rights challenges. Early in the interval, the framework could operate with less visible enforcement (theater ~0.22) because it aligned with dominant Enlightenment-rationality norms in academic and medical philosophy. As disability scholarship mounted systematic challenges, the framework required more explicit institutional defense, more careful argumentation in ethics committees, and more active suppression of alternative framings. By interval end, theater rises to 0.44 — nearly half the constraint's operation is now devoted to defending and restating the framework rather than simply applying it. Suppression remains high (0.71) throughout because the framework's authority depends on excluding disability advocates' voices from the decision-making structure: their objections are not engaged but rather treated as outside the legitimate discourse. This exclusion must be actively maintained.
 *
 * PERSPECTIVAL GAP:
 *   From the parental and medical authority seat, the constraint appears as genuine coordination: a principled framework enabling difficult resource-allocation decisions in the face of scarcity. From the disabled infants' seat (if they could express a perspective), it appears as pure exclusion: personhood status assigned on grounds (potential rationality) that systematically disadvantage them. From disability-advocate seats, it appears as enforced extraction: the framework permits the extraction of moral standing and resource priority from disabled infants to benefit resource-allocating institutions. The engine should compute these divergent types from the structural data: the beneficiary/victim asymmetry, the enforcement intensity, the suppression mechanism (excluding alternative voices). The claimed type (tangled_rope) reflects the parental-authority seat's perspective; the metrics reflect the disabled-infants seat and the advocate seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The disabled infants are the targets (d near 1.0): powerless, trapped, universal scope, bearing the extraction of moral standing. Parental and medical authority are beneficiaries (d near 0.0): institutional power, arbitrage options, setting the rules. Resource allocation systems also benefit (d near 0.0): extracting moral permission to allocate resources differently. Disability advocates are constrained payers (d near 0.7): they bear the cost of challenging the framework without sufficient authority to displace it; their power is organized but their exit options are constrained by law and institutional precedent. The framework systematically advantages beneficiaries and disadvantages targets because it concentrates decision-making authority among the beneficiaries and denies voice to those who bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint passes the tangled-rope gates: it coordinates genuine resource allocation in scarcity (beneficiaries present), it extracts moral standing from identified victims (victims present), it requires active enforcement (exclusion of alternative voices must be maintained). The constraint does NOT degrade to piton: disability advocates continue active resistance (resistance 0.58 is non-negligible), the founding problem remains contested (not dead), and the framework requires sustained institutional enforcement rather than mere theater. However, the rising theater ratio (0.22 to 0.44) signals that the constraint's functional coherence may be eroding — increasing defensive effort suggests the rational agency criterion is harder to maintain as a natural fact. This is a constraint under pressure, not yet inertial, but approaching a bifurcation: either the framework will be abandoned or it will harden into explicitly dogmatic form (moving from Enlightenment-rationality grounding to theological/conventional grounding in cs_structure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_measurement_ambiguity,
    'What constitutes ''potential for rational agency'' and who measures it? Is potential objectively determinable from neurological examination, or is it a judgment call that depends on the assessor''s assumptions about what counts as rational agency?',
    'Systematic study of how different medical and philosophical frameworks assess ''potential'': do they converge on the same exclusion boundaries or do they diverge based on underlying assumptions about rationality, disability, and human value?',
    'If potential is objectively measurable, the framework is grounded in fact and the extracted moral standing is a byproduct of correct classification. If potential is judgment-dependent, then the framework permits masking value choices as facts — the extraction is partially fabricated. This determines whether the constraint is a Rope (principled coordination) or a Snare (cover story for extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_measurement_ambiguity, empirical, 'Whether potential for rational agency is objectively measurable or judgment-dependent').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of disability-advocate voices structural (institutional exclusion, lack of formal channels, legal barriers to participation) or internalized (advocates themselves internalize the belief that they are outside the legitimate discourse)?',
    'Longitudinal study of disability-advocacy responses: if advocates withdraw because pathways are closed, suppression is structural; if advocates withdraw while pathways remain open, suppression has internalized. Post-institutional-change trajectory: do advocates re-enter discourse when formal exclusion is lifted?',
    'Structural suppression can be dismantled by opening institutions; internalized suppression persists after formal barriers are removed and requires additional remediation. This affects the cost and feasibility of constraint dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    rationality_as_fitness_disguise,
    'Is the rationality-potential criterion a genuine principled distinction, or a rationalization of prior value commitments to privilege certain neurotypes and exclude others?',
    'Historical analysis of rationality definitions across philosophical tradition; contemporary analysis of which disabled populations are systematically classified as lacking potential under different rationality frameworks. If rationality definitions shift to match pre-existing preferences about whose lives are worth living, the criterion is a cover story.',
    'If rationality is genuine, the framework is a principled constraint. If it is a cover story, the constraint is a Snare: the real extraction is based on fitness/disability status, but the rationality criterion launders it as principled. This determination hinges on the conceptual ambiguity of the rationality concept itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_as_fitness_disguise, conceptual, 'Whether rationality-potential is a principled criterion or a fitness cover story').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the potential-based reading logically foreclose the birth-threshold reading (they cannot both be true in a single framework), or do they coexist as live alternatives held by different communities?',
    'Philosophical analysis: if one reading''s core axiom directly contradicts the other''s, they foreclose. If both can be logically held by separate communities without one refuting the other, they coexist. Institutional evidence: do different institutions adopt different readings without claiming the other is incoherent, or does each claim the other is rationally indefensible?',
    'Foreclosure implies one reading will eventually dominate; coexistence implies both will persist as live alternatives across institutional sectors. This affects long-term trajectory prediction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between potential-based and birth-threshold readings').

omega_variable(
    identity_lock_in_disabled_advocates,
    'Do disability advocates'' exit options remain genuinely constrained (they cannot leave the constraint system), or have they become identity-locked (they remain engaged because their identity as advocates is fused with this struggle)?',
    'Biographical analysis of advocates: do individuals who leave advocacy work cite legal/institutional barriers, or do they cite loss of identity/meaning? If the barrier is institutional, exit_options remain constrained; if the barrier is identity, exit_options are identity_locked.',
    'Identity-locked agents can be sustained in active resistance despite high costs; constrained agents may eventually deplete and abandon resistance. This affects long-term resistance trajectory (metrics.resistance over extended interval).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_disabled_advocates, empirical, 'Whether disability advocates'' constraint is institutional or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__potential_based_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(pers_tr_t8, observed).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__potential_based_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(pers_tr_t16, observed).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__potential_based_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(pers_tr_t24, observed).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__potential_based_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(pers_tr_t32, observed).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(pers_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__potential_based_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(pers_be_t8, observed).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__potential_based_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(pers_be_t16, observed).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__potential_based_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(pers_be_t24, observed).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__potential_based_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(pers_be_t32, observed).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(pers_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__potential_based_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(pers_su_t8, observed).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__potential_based_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(pers_su_t16, observed).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__potential_based_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(pers_su_t24, observed).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__potential_based_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(pers_su_t32, observed).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(pers_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.18).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the personhood_boundary kernel. The birth_threshold_reading and fitness_contingent_reading are structurally distinct constraints with different epsilon values and victim sets; all three are linked because they offer alternative resolutions to the same contested kernel (what grounds moral personhood). The potential_based_reading is characterized by its grounding of personhood in rational agency potential, which permits exclusion of infants deemed to lack that potential. This differs from birth_threshold (which assigns personhood universally at birth) and fitness_contingent (which conditions personhood on demonstrated fit, not mere potential). Each reading has distinct authority structures, beneficiary sets, and victim sets. The three constraints together form a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
