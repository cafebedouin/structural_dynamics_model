% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood at Birth: Universal Moral Standing for All Born Humans
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the birth-threshold reading of the
 *   personhood-boundary kernel: moral and legal personhood attaches at live
 *   birth to every born human, without regard to capacity, prognosis,
 *   disability, or demonstrated fitness. Historically this reading displaced
 *   practices — exposure of infants, selective infanticide of the disabled or
 *   unwanted, discretionary non-treatment — that the fitness-contingent and
 *   potential-based readings would, in their strong forms, permit or at least
 *   leave open. The reading functions as a coordination device: it removes
 *   discretion from parents, clinicians, and the state at the exact moment
 *   discretion would be most dangerous to the least powerful party. This is
 *   one reading among three siblings sharing the same kernel; the
 *   fitness-contingent and potential-based readings are NOT represented in
 *   this file's metrics — they are separate constraints with their own ε
 *   values, victim sets, and stakeholder surfaces, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - born_infants: primary beneficiary (powerless/trapped) — the reading's entire function is protecting this group
 *   - disabled_newborns: beneficiary whose standing is precisely the contested margin against the potential-based reading
 *   - parents_and_guardians: payer — bear enforceable duty of care regardless of prognosis
 *   - the_state: agenda_setter — codifies and enforces the bright line, forecloses its own discretion to exclude
 *   - medical_practitioners: agenda_setter/payer — clinical discretion over treatment withdrawal is constrained
 *   - disability_rights_advocates: organized beneficiary-defenders of the bright line
 *   - fitness_and_potential_theorists: excluded from the operative legal decision procedure once this reading is codified
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.12).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.28).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood at Birth: Universal Moral Standing for All Born Humans").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'fcd3047d-7eec-4668-8f6c-929bd20b0fb0').
narrative_ontology:cs_kernel_codification('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', distributed).
narrative_ontology:cs_authority_grounding('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', distributed).
narrative_ontology:cs_reading_relation('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', foundational, birth_is_sufficient_and_necessary_for_standing).
narrative_ontology:cs_axiom_status(birth_is_sufficient_and_necessary_for_standing, holdable).
narrative_ontology:cs_axiom_grounding('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', birth_is_sufficient_and_necessary_for_standing, deontological).
narrative_ontology:cs_axiom('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', foundational, no_authority_may_condition_standing_on_capacity).
narrative_ontology:cs_axiom_status(no_authority_may_condition_standing_on_capacity, holdable).
narrative_ontology:cs_axiom_grounding('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', no_authority_may_condition_standing_on_capacity, deontological).
narrative_ontology:cs_reference_frame('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', common_law_born_alive_rule).
narrative_ontology:cs_drift_state('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', contemporary_bioethics_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fcd3047d-7eec-4668-8f6c-929bd20b0fb0', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, disabled_newborns).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, premature_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, foundling_and_abandoned_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, parents_and_guardians).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, disability_rights_advocates).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_and_guardians).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, medical_practitioners).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, bright_line_birth_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot advocate for themselves at all; their moral standing is entirely a function of whether the surrounding legal and social order recognizes birth as sufficient. Under this reading, the instant of birth confers full protection against killing, neglect-to-death, and exclusion regardless of any trait, capacity, or disability. They have no exit — their status is conferred or withheld entirely by others.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_infants, beneficiary,
    powerless, biographical, trapped, national).

% Under sibling readings that key personhood to potential or fitness, this group is the one whose standing is most contested. The birth-threshold reading is written specifically to close off any inquiry into their capacities as a condition of protection: birth alone suffices. They have no capacity to exit or contest their own classification.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, disabled_newborns, beneficiary,
    powerless, biographical, trapped, national).

% Occupy a boundary case for viability-based reasoning; the birth-threshold reading assigns them full standing at birth regardless of gestational maturity or viability prognosis, removing viability as a relevant variable entirely.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, premature_infants, beneficiary,
    powerless, immediate, trapped, national).

% Historically the clearest test case for the boundary: infants with no attached family, no demonstrated social value, sometimes no expectation of survival. Under this reading their standing does not depend on being claimed, valued, or expected to thrive — birth alone is dispositive.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, foundling_and_abandoned_infants, beneficiary,
    powerless, biographical, trapped, national).

% Bear the legal duty of care the reading imposes once birth occurs, including in cases of severe disability, extreme prematurity, or unwanted birth where the potential-based or fitness-contingent readings would have permitted non-treatment or exclusion. They cannot exit the duty once a birth has occurred within the jurisdiction; the reading forecloses the option of treating post-birth infanticide or withdrawal-to-death as a private discretionary matter.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_and_guardians, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, parents_and_guardians, beneficiary).

% Codifies and enforces the birth threshold through homicide law, mandatory reporting, and child-protection statutes. Under this reading the state has no discretion to authorize exclusion of any born human from protection — this is precisely what distinguishes the reading from the fitness-contingent alternative, which grants the state (or medical authorities acting for it) discretion to withhold standing pending demonstrated fitness.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, the_state, agenda_setter,
    institutional, generational, analytical, national).

% Administer neonatal care under a legal regime that treats every live birth as triggering full protection, removing clinical discretion to withhold resuscitation or treatment on the basis of predicted quality of life or disability status alone. This constrains treatment-withdrawal decisions that the potential-based reading would leave more open.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_practitioners, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, medical_practitioners, payer).

% Organized political actors who benefit from and actively defend the birth-threshold reading precisely because it forecloses the potential-based reading's carve-out for severely disabled infants. They litigate, lobby, and publish to keep the bright line at birth rather than at demonstrated capacity.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, disability_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Philosophers and bioethicists advancing the sibling readings are not silenced, but their positions have no purchase within the legal and clinical apparatus once the birth-threshold reading is codified into homicide law — the practical conversation about withholding treatment from a born infant on capacity grounds is foreclosed as a legal option even where it continues as an academic debate.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, fitness_and_potential_theorists, excluded,
    organized, civilizational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-negotiable, easily verified criterion (live birth) for who counts as protected by homicide law and child-protection duties, avoiding case-by-case adjudication of an infant's fitness, potential, or prognosis at the moment protection is most urgently needed.
% TRANSFER_FUNCTION: Moves the burden of uncertainty about capacity or prognosis away from the infant (who would otherwise bear the risk of being judged and excluded) and onto parents, medical practitioners, and the state, who must extend full protective duties regardless of the infant's prospects.
% ABSENT_VOICES: The infants themselves have no voice by definition. Proponents of the fitness-contingent and potential-based readings are not excluded from the debate generally, but are excluded from the operative legal and clinical decision procedure once this reading is codified — their objection that some born infants lack the relevant capacities for full moral status is not admissible as a defense to homicide or neglect charges under this reading.
% DISAPPEARANCE_RATIONALE: If the birth-threshold criterion vanished and jurisdictions reverted to fitness- or potential-based standards, neonatal medicine, child-protection law, and criminal homicide statutes would all require new administrable criteria for which born humans count as protected persons — reopening exactly the disability-based exclusion questions this reading was built to foreclose, and altering outcomes for premature, disabled, and unclaimed infants immediately.
% FOUNDING_PROBLEM: Historically and philosophically, ambiguity about which humans count as full moral persons has been used to justify infanticide, exposure of 'defective' or unwanted infants, and exclusion of disabled newborns from care — the birth-threshold reading was articulated to give a bright, administrable line that removes discretion from any party motivated to under-protect a particular infant.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations, external to the birth-threshold reading's own strongest institutional beneficiary (the state's enforcement apparatus), corroborate that the founding problem — discretionary exclusion of disabled or unwanted infants — remains live, citing contemporary debates over selective non-treatment of severely disabled newborns. Proponents of the potential-based reading, from outside this reading's camp, dispute that birth is the philosophically correct place to draw the line at all, though they do not dispute that some line-drawing problem exists.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).
:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because, assessed by this reading's own lights, the birth-threshold arrangement is not extracting from born infants — it is protecting them; the standing arrangement under contest (case-by-case discretionary exclusion) is what the reading displaces, and the reading's own operation extracts almost nothing from the class it protects. Suppression is moderate (0.28) because the reading does foreclose a real alternative practice (discretionary non-protection) via homicide law and mandatory-care statutes — that foreclosure is coercive toward parents and clinicians who might otherwise exercise discretion, even though it is protective toward infants. Theater ratio is low (0.10): enforcement (homicide prosecution, child-protection intervention) is substantive rather than performative. Accessibility collapse is high (0.72): once codified, the discretionary alternative is very difficult to access legally — a parent or clinician cannot lawfully treat a disabled newborn's status as contingent. Resistance is moderate (0.35): fitness- and potential-based theorists continue to contest the line academically and in select jurisdictions (e.g., debates over severely disabled neonates), so the boundary is not uncontested even though it is dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Born infants, disabled newborns, premature infants, and foundlings are beneficiaries with d near the full-beneficiary end — the reading subsidizes their protection at the direct cost of removing others' discretion. Parents, guardians, and medical practitioners are payers in the sense that they bear an enforceable duty they cannot decline once a birth occurs — the reading imposes exit-constrained obligation, though the burden is a duty of care rather than an extraction of value from them for a third party's benefit. The state is the agenda-setter, deriving legitimacy from and administering the bright line rather than benefiting materially from it. Disability rights advocates are organized beneficiaries who actively defend the reading. Fitness/potential theorists are excluded from practical effect though not from discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (discretionary exclusion enabling infanticide or neglect of disabled/unwanted infants) is contested rather than dead: disability-rights corroboration from outside the state's own enforcement apparatus confirms the underlying problem persists in live form (contemporary non-treatment debates for severely disabled newborns), which is why the reading is authored as still functionally load-bearing rather than a vestigial mandate. This blocks a premature 'this is just theater now' read: the bright line continues to do real protective work against a real and recurring pressure to reintroduce fitness-based discretion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_locus,
    'Where exactly does the birth-threshold reading''s core premise (birth alone is sufficient and necessary for full moral standing) conflict with the potential-based reading''s carve-out for severely disabled infants, and can any single legal framework hold both simultaneously?',
    'Examine whether any jurisdiction''s actual statute simultaneously grants full homicide protection to all born infants AND permits discretionary non-treatment of severely disabled newborns on potential-based grounds without internal contradiction; if none does, the forecloses relation is empirically supported rather than merely asserted.',
    'If no framework can coherently hold both, the birth-threshold reading forecloses the potential-based reading in any single jurisdiction''s operative law, even though both persist as live positions across different jurisdictions/theorists — this is the basis for the forecloses edge declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Whether birth-threshold and potential-based readings can coexist within one legal framework or are logically exclusive.').

omega_variable(
    bright_line_versus_case_by_case_tradeoff,
    'Does the administrability benefit of a bright birth-line (avoiding case-by-case fitness/potential adjudication under time pressure) outweigh the cost of removing clinical and parental discretion in genuinely difficult prognosis cases (e.g., extreme prematurity with near-certain non-survival)?',
    'Comparative outcome data from jurisdictions with bright-line versus discretionary regimes on neonatal outcomes, family and clinician distress, and documented cases of both over-treatment and under-protection.',
    'If discretionary regimes produce materially better outcomes without increased risk to disabled or marginal infants, the coordination case for the bright line weakens and the suppression cost (0.28) would be harder to justify as protective rather than merely rigid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bright_line_versus_case_by_case_tradeoff, empirical, 'Whether the administrability gain from a bright line justifies its removal of case-by-case clinical and parental discretion.').

omega_variable(
    founding_problem_persistence_scope,
    'Is the founding problem (discretionary infanticide/exclusion of disabled or unwanted infants) still a live risk at the same scale that motivated the reading historically, or has it substantially receded due to independent factors (neonatal medicine advances, changed social attitudes toward disability)?',
    'Track documented rates of selective non-treatment, neonaticide, and abandonment across jurisdictions with and without codified birth-threshold protections over time.',
    'If the underlying risk has substantially receded, the founding_problem_status would shift toward ''contested-trending-dead,'' which would not change the classification directly but would weaken the corroboration for treating enforcement intensity as still load-bearing rather than partly inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_scope, empirical, 'Whether the historical risk motivating the birth threshold has receded or remains at comparable scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__birth_threshold_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__birth_threshold_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__birth_threshold_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(pers_tr_t80, personhood_boundary__birth_threshold_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__birth_threshold_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__birth_threshold_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__birth_threshold_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__birth_threshold_reading, base_extractiveness, 60, 0.13).
narrative_ontology:measurement(pers_be_t80, personhood_boundary__birth_threshold_reading, base_extractiveness, 80, 0.13).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__birth_threshold_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__birth_threshold_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__birth_threshold_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(pers_su_t60, personhood_boundary__birth_threshold_reading, suppression_requirement, 60, 0.31).
narrative_ontology:measurement(pers_su_t80, personhood_boundary__birth_threshold_reading, suppression_requirement, 80, 0.29).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__birth_threshold_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'personhood boundary' kernel per the ε-invariance principle: birth_threshold_reading (this file, ε=0.12, rope-leaning, no victims — protective bright line), fitness_contingent_reading (separate file — personhood contingent on demonstrated fitness, expected to carry a victim set of pre-fitness entities and a substantially higher ε), and potential_based_reading (separate file — personhood grounded in potential for rational agency, expected to carry severely disabled infants as a contested victim/beneficiary boundary case). Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type; they are linked here rather than merged because measuring 'the personhood boundary' by different readings' lights yields structurally different constraints, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
