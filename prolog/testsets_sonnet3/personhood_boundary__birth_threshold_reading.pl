% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Birth as the Threshold of Moral Standing
 *   domain: moral philosophy / historical ethics / commitment systems
 *
 * SUMMARY:
 *   This story instantiates one reading of the personhood-boundary kernel:
 *   the claim that moral standing attaches unconditionally at live birth,
 *   with no further qualifying condition of capacity, fitness, or potential.
 *   The reading is presented here as the story's own claim — not weighed
 *   against, averaged with, or hedged by the sibling readings
 *   (fitness_contingent_reading, potential_based_reading), which are separate
 *   constraints entirely. Under this reading, extraction is very low: the
 *   arrangement confers protection rather than extracting from a payer class,
 *   and the closest thing to a cost (constrained clinical/parental discretion
 *   in hard cases) is a narrow side effect, not the arrangement's function.
 *   The reading is authored as near-mountain in character (low extraction,
 *   low suppression) because, from within its own framework, the birth
 *   criterion is presented as a discovered moral bright line rather than a
 *   constructed extraction mechanism — though the declared beneficiaries
 *   trigger FSM evaluation, addressed in the omegas below.
 *
 * KEY AGENTS:
 *   - born_infants: primary beneficiary (powerless/trapped) — recipients of unconditional standing
 *   - disabled_newborns: beneficiary (powerless/trapped) — standing independent of capacity, the reading's sharpest distinguishing case
 *   - medical_and_legal_institutions: agenda_setter (institutional/constrained) — administers but does not originate the boundary
 *   - parents_facing_difficult_births: payer (moderate/constrained) — bears discretion-loss as a side effect
 *   - advocates_of_fitness_and_potential_readings: excluded (organized/mobile) — holds a structurally different commitment, not incorporated here
 *   - bioethics_and_moral_philosophy_scholarship: analytical observer — documents the kernel contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.08).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.15).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Birth as the Threshold of Moral Standing").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral philosophy / historical ethics / commitment systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'ff115313-4196-4f7b-8658-d5d5efc317c5').
narrative_ontology:cs_kernel_codification('ff115313-4196-4f7b-8658-d5d5efc317c5', distributed).
narrative_ontology:cs_authority_grounding('ff115313-4196-4f7b-8658-d5d5efc317c5', distributed).
narrative_ontology:cs_reading_relation('ff115313-4196-4f7b-8658-d5d5efc317c5', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('ff115313-4196-4f7b-8658-d5d5efc317c5', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('ff115313-4196-4f7b-8658-d5d5efc317c5', foundational, birth_is_sufficient_and_necessary_for_standing).
narrative_ontology:cs_axiom_status(birth_is_sufficient_and_necessary_for_standing, holdable).
narrative_ontology:cs_axiom_grounding('ff115313-4196-4f7b-8658-d5d5efc317c5', birth_is_sufficient_and_necessary_for_standing, deontological).
narrative_ontology:cs_axiom('ff115313-4196-4f7b-8658-d5d5efc317c5', foundational, capacity_and_prognosis_are_morally_irrelevant_to_standing).
narrative_ontology:cs_axiom_status(capacity_and_prognosis_are_morally_irrelevant_to_standing, holdable).
narrative_ontology:cs_axiom_grounding('ff115313-4196-4f7b-8658-d5d5efc317c5', capacity_and_prognosis_are_morally_irrelevant_to_standing, deontological).
narrative_ontology:cs_reference_frame('ff115313-4196-4f7b-8658-d5d5efc317c5', unconditional_birth_criterion).
narrative_ontology:cs_drift_state('ff115313-4196-4f7b-8658-d5d5efc317c5', contemporary_bioethics_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ff115313-4196-4f7b-8658-d5d5efc317c5', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, born_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, disabled_newborns).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, premature_infants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_facing_difficult_births).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, bright_line_birth_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot advocate for themselves in any respect, yet under this reading are conferred full moral standing the moment they are born, regardless of capacity, disability, or degree of dependency. The reading treats the fact of live birth itself as sufficient; nothing about the infant's demonstrated abilities is at issue.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, born_infants, beneficiary,
    powerless, biographical, trapped, universal).

% Under this reading, severe disability at birth carries no bearing on moral standing — the bright line is birth, not capacity or projected quality of life. This is the reading's sharpest point of departure from the potential-based and fitness-contingent siblings, both of which open some space for disability-conditioned exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, disabled_newborns, beneficiary,
    powerless, biographical, trapped, universal).

% Extremely premature or medically fragile infants receive identical standing to any other born human under this reading — the threshold is passage through birth, not gestational viability, developmental milestone, or survival probability.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, premature_infants, beneficiary,
    powerless, biographical, trapped, universal).

% Hospitals, courts, and legislatures operationalize the birth threshold into concrete practice: birth certificates, homicide statutes, neonatal treatment mandates. They administer the boundary but did not invent the underlying moral claim; they can shift enforcement intensity (e.g., mandatory treatment statutes) but cannot relocate the threshold itself without abandoning this reading altogether.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_and_legal_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Some parents confronting severe neonatal prognoses experience the bright-line rule as foreclosing decisions they might otherwise have made under a capacity- or potential-based standard. They bear the emotional and resource costs of mandated treatment or the legal exposure of withholding it, but are not the reading's target — the rule's stringency toward them is a side effect of protecting the infant, not an extraction from them.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_facing_difficult_births, payer,
    moderate, biographical, constrained, local).

% Philosophers, bioethicists, and some clinicians who hold that moral standing should track demonstrated or potential rational agency rather than the bare fact of birth are not incorporated into this reading's framework — from within the birth-threshold commitment, their position is treated as simply mistaken about where the line falls, not as a competing consideration to be weighed.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, advocates_of_fitness_and_potential_readings, excluded,
    organized, generational, mobile, national).

% Documents, contests, and compares the birth-threshold, fitness-contingent, and potential-based readings across legal traditions and historical periods, without being a party that benefits or pays under any one reading.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, bioethics_and_moral_philosophy_scholarship, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively bright-line criterion for who counts as a bearer of full moral and legal protection, avoiding case-by-case adjudication of capacity, potential, or fitness for every individual born human.
% TRANSFER_FUNCTION: Confers unconditional protection against homicide and a floor of legal standing onto every born human, without extracting anything from a payer class in the ordinary sense; the closest thing to a 'cost' is borne by decision-makers (parents, clinicians) who lose discretion to withhold treatment or standing based on capacity judgments.
% ABSENT_VOICES: Advocates of fitness-contingent and potential-based readings are structurally outside this reading's own framework — their arguments are treated as addressing a different (and, from here, mistaken) question about where personhood begins, not as live alternatives within it.
% DISAPPEARANCE_RATIONALE: If the birth-threshold criterion vanished, homicide law, neonatal medical ethics, adoption and custody regimes, and disability rights protections premised on unconditional newborn standing would all require re-grounding on some other criterion (capacity, potential, viability), producing immediate and substantial reclassification of which born humans receive protection.
% FOUNDING_PROBLEM: Historically and philosophically, societies needed a non-arbitrary, non-discretionary criterion for extending legal and moral protection to the newly born, replacing older practices (e.g., paternal power of exposure, infanticide tolerated for disabled or unwanted infants) that made survival contingent on a private judgment of worth.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies and disability rights organizations (outside any single family's or clinician's interest) attest the founding problem remains live — infanticide and selective non-treatment of disabled newborns persist in some jurisdictions and informal practices. Advocates of the potential-based and fitness-contingent readings, from outside this reading's own commitments, dispute that birth is the correct or only defensible non-arbitrary line, corroborating that the boundary itself remains actively contested rather than settled.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.08 at interval end) because the birth-threshold reading, on its own terms, does not run a transfer mechanism that takes something from a payer class to give to a beneficiary class in the ordinary extractive sense — it confers protection. Suppression is low-moderate (0.15) reflecting the genuine, if modest, coercive weight of mandatory-treatment statutes and homicide law enforcement against those who would act on a different standard. Accessibility collapse is moderate (0.35), not the ~0.85+ expected of a genuine physical mountain, because alternative readings (fitness-contingent, potential-based) remain live, articulated, and held by organized advocates — the boundary is contested, not settled the way a law of physics is settled. Resistance is correspondingly moderate (0.2): real philosophical and some legal resistance exists, but it has not displaced the reading's dominance in most legal systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Born infants, disabled newborns, and premature infants are declared beneficiaries because the reading's entire function is to confer standing on them unconditionally; they have no exit and no capacity to negotiate, so directionality sits near the full-beneficiary end (subsidized, not extracted from) despite their powerlessness — power and directionality move independently here. Medical and legal institutions are agenda-setters who administer enforcement but do not capture rents from it. Parents facing difficult births are the closest thing to a payer, but the cost is discretion lost, not wealth or standing extracted — this is authored honestly as a real but narrow cost, not manufactured to force a snare or tangled-rope reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting the newly born from private judgments of worth, replacing exposure/infanticide with unconditional protection) is still live by outside corroboration (human rights and disability rights bodies), so this is not a mandatrophy case — the mandate has not obviously outlived its function. Classifying this as mountain-leaning rather than forcing it into tangled_rope prevents mislabeling a genuine (if contested) moral-boundary claim as pure extraction merely because it has declared beneficiaries; the FSM omega below is where that tension is made explicit rather than resolved by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_threshold_natural_or_constructed,
    'Is the birth-threshold criterion a discovered feature of moral reality (a genuine boundary that any adequate ethical framework must recognize), or a historically constructed convention that happens to benefit born infants at the expense of excluded alternative framings (e.g., prenatal entities under a different kernel reading, or severely disabled infants under a capacity standard that this reading forecloses)?',
    'Cross-cultural and cross-historical survey of where societies have drawn the personhood line, combined with philosophical argument-mapping of whether the birth criterion survives independent of the practical coordination benefits (administrability, avoidance of case-by-case capacity testing) it provides.',
    'If the criterion is substantially explained by its administrative convenience and its function of settling contested cases in favor of one class of infants (disabled, premature) rather than by an independently discoverable moral fact, the false_summit_mountain signature should fire and this constraint reclassifies toward tangled_rope, with disabled/premature infants as a coordinated-but-contested beneficiary class and advocates of rival readings as an effectively suppressed alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_threshold_natural_or_constructed, conceptual, 'Whether the birth threshold is a genuine moral-boundary mountain or a constructed convention with declared beneficiaries (FSM candidate).').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of the personhood_boundary kernel locate their disagreement — is it about the correct criterion for moral standing (a philosophical dispute), about how to handle uncertainty at the margins (an epistemic dispute), or about what follows practically once standing is granted (a policy dispute)?',
    'Structural decomposition of each reading''s axioms (as authored in cs_structure) to identify whether the disagreement is in the grounding_type (deontological vs. instrumental vs. empirically_contingent claims) or merely in the threshold value applied to a shared criterion.',
    'If the disagreement is purely about threshold placement on a shared empirically_contingent criterion (e.g., degree of demonstrated capacity), the readings could in principle converge with better evidence. If the disagreement is in fundamentally different deontological groundings, the readings are permanently coexisting rather than resolvable — this changes how the kernel''s overall stability should be modeled across the constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Committer-frame ambiguity: whether the personhood_boundary kernel''s sibling readings disagree on criterion, evidence, or grounding type.').

omega_variable(
    disabled_newborn_treatment_gap,
    'In practice, does the birth-threshold reading''s formal commitment to unconditional standing for disabled newborns actually translate into equal treatment, or does a fitness/potential logic re-enter through clinical discretion (e.g., withdrawal-of-care decisions) even where the formal legal rule declares the birth threshold controlling?',
    'Empirical audit of neonatal intensive care treatment-withdrawal decisions and their correlation with disability severity, compared against decisions for non-disabled infants with comparable prognosis.',
    'If a treatment gap exists, the birth-threshold reading''s suppression and accessibility_collapse values are likely understated — the formal boundary would be less effectively enforced against disability-based exclusion than this story assumes, moving the constraint toward tangled_rope on the enforcement dimension even if the mountain framing holds for the pure legal-philosophical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disabled_newborn_treatment_gap, empirical, 'Whether formal commitment to the birth threshold survives contact with clinical practice for disabled newborns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__birth_threshold_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__birth_threshold_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__birth_threshold_reading, theater_ratio, 30, 0.04).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__birth_threshold_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__birth_threshold_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__birth_threshold_reading, theater_ratio, 60, 0.05).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__birth_threshold_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__birth_threshold_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__birth_threshold_reading, base_extractiveness, 30, 0.07).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__birth_threshold_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__birth_threshold_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__birth_threshold_reading, base_extractiveness, 60, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(personhood_boundary__birth_threshold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the personhood_boundary kernel. birth_threshold_reading (this story) authors low extraction and near-mountain metrics because, on its own terms, it confers unconditional protection rather than running an extraction mechanism. fitness_contingent_reading and potential_based_reading are separate constraint files with their own ε values, stakeholder sets, and victim classes (entities failing the fitness or potential test), reflecting the ε-invariance principle: the same natural-language topic ('personhood') decomposes into structurally distinct claims that must not be averaged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
