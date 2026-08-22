% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Fitness-Contingent Personhood: Moral Standing Gated on Demonstrated Capacity
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the fitness-contingent reading of the
 *   personhood-boundary kernel: moral and legal standing attaches not at
 *   birth but only after an infant demonstrates a threshold of capacity or
 *   viability determined by state or institutional criteria. Under this
 *   reading, pre-fitness infants and, disproportionately, disabled neonates
 *   are excluded from the community of moral patients, and institutions
 *   administering the test and the resources it frees up are the structural
 *   beneficiaries. This is a single, ε-invariant reading: it does not
 *   describe the contest among readings, only the arrangement this reading
 *   itself endorses and the extraction it authors under that endorsement.
 *
 * KEY AGENTS:
 *   - state_authorities_administering_fitness_tests: institutional agenda_setter — designs and applies the test
 *   - resource_allocating_institutions: organized beneficiary — gains budgetable triage authority
 *   - eugenic_policy_architects: powerful beneficiary/agenda_setter — sets population-level fitness criteria
 *   - pre_fitness_infants: powerless payer — bears total exclusion risk with zero voice
 *   - disabled_neonates: powerless payer — disproportionately fails fitness criteria
 *   - disability_rights_advocates: excluded organized voice — structurally kept from criteria-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.81).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.88).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Fitness-Contingent Personhood: Moral Standing Gated on Demonstrated Capacity").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '19eef217-76ff-47d5-9496-2b66abf4a091').
narrative_ontology:cs_kernel_codification('19eef217-76ff-47d5-9496-2b66abf4a091', distributed).
narrative_ontology:cs_authority_grounding('19eef217-76ff-47d5-9496-2b66abf4a091', extraction).
narrative_ontology:cs_interpretation_layer_present('19eef217-76ff-47d5-9496-2b66abf4a091').
narrative_ontology:cs_reading_relation('19eef217-76ff-47d5-9496-2b66abf4a091', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('19eef217-76ff-47d5-9496-2b66abf4a091', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('19eef217-76ff-47d5-9496-2b66abf4a091', foundational, demonstrated_capacity_grounds_standing).
narrative_ontology:cs_axiom_status(demonstrated_capacity_grounds_standing, holdable).
narrative_ontology:cs_axiom_grounding('19eef217-76ff-47d5-9496-2b66abf4a091', demonstrated_capacity_grounds_standing, empirically_contingent).
narrative_ontology:cs_axiom('19eef217-76ff-47d5-9496-2b66abf4a091', foundational, state_holds_admission_authority_over_moral_community).
narrative_ontology:cs_axiom_status(state_holds_admission_authority_over_moral_community, holdable).
narrative_ontology:cs_axiom_grounding('19eef217-76ff-47d5-9496-2b66abf4a091', state_holds_admission_authority_over_moral_community, conventional).
narrative_ontology:cs_reference_frame('19eef217-76ff-47d5-9496-2b66abf4a091', capacity_grounded_moral_status).
narrative_ontology:cs_drift_state('19eef217-76ff-47d5-9496-2b66abf4a091', post_disability_rights_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('19eef217-76ff-47d5-9496-2b66abf4a091', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authorities_administering_fitness_tests).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, resource_allocating_institutions).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, eugenic_policy_architects).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, disabled_neonates).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, parents_of_untested_infants).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, capacity_grounds_moral_status_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, state_authority_over_membership_admission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the fitness criteria (physical viability, projected capacity, absence of defect) that determine which infants are admitted into the moral/legal community. Controls the timing of the test, the threshold for passing, and the consequences of failure, including withdrawal of protection, resource denial, or sanctioned exposure/euthanasia. Bears none of the direct costs of exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authorities_administering_fitness_tests, agenda_setter,
    institutional, generational, analytical, national).

% Hospitals, insurers, and welfare administrators benefit from a bright line that permits withholding costly care, inheritance, or citizenship registration from infants who have not yet passed the fitness threshold. The boundary converts what would otherwise be an open-ended obligation into a bounded, budgetable one.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, resource_allocating_institutions, beneficiary,
    organized, generational, arbitrage, national).

% Intellectual and political sponsors of population-quality programs who use the fitness threshold to justify selective exclusion at the population level, shaping what counts as 'fitness' to track class, disability, and other socially convenient markers. They set the definitional terms other institutions then apply.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, eugenic_policy_architects, beneficiary,
    powerful, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, eugenic_policy_architects, agenda_setter).

% Newborns who have not yet cleared the fitness assessment window have no legal standing, no capacity to contest the determination, and no exit: their moral status is wholly external to them, decided by an examiner applying criteria they cannot influence or appeal. If they fail, they may be denied care, registration, or life itself under sanction.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, trapped, local).

% Infants with visible or detected impairments are the population most likely to fail the fitness threshold regardless of viability with support. The test is disproportionately structured around able-bodied norms, so disability itself becomes the disqualifying condition rather than a neutral input to a capacity assessment.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disabled_neonates, payer,
    powerless, immediate, trapped, local).

% Bear the psychological and social cost of an interim period in which their child's standing is provisional, and in some historical applications are pressured or compelled to accept institutional determinations about withholding care or registration. Have limited recourse against a state-defined fitness criterion and limited ability to contest an unfavorable finding.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, parents_of_untested_infants, payer,
    moderate, biographical, constrained, local).

% Would argue that fitness-contingent standing simply re-labels disability discrimination as a metaphysical boundary question, and that the criteria used are neither neutral nor stable. Historically and in most jurisdictions today, this constituency has been excluded from the process that sets fitness criteria.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Examine the fitness-contingent framework in retrospect (or in ongoing neonatal ethics disputes), assess its consistency, and can recommend abolition or reform of testing-based standing determinations. Their findings feed into but do not control state policy.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, bioethics_review_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, diffuse).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides administrators, insurers, and state registries with a bright-line, budgetable criterion for when full legal and moral obligations attach to a new human being, avoiding open-ended commitments to entities of uncertain viability or capacity.
% TRANSFER_FUNCTION: Moves the burden of proof for moral standing from the community (which would otherwise owe protection by default) onto the newborn, and moves resources, protection, and legal recognition away from infants who fail the test toward institutions that no longer bear the cost of their care.
% ABSENT_VOICES: Disability rights advocates and, definitionally, the infants themselves are excluded from any determination of the criteria that decide their standing. Bioethicists critical of capacity-based moral status theories are marginalized in jurisdictions where the fitness criterion is state-codified.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent threshold were abolished and standing attached at birth (or earlier) unconditionally, resource-allocation institutions would lose their basis for triaging protection by capacity, exclusionary population-policy programs would lose a key administrative tool, and disabled neonates currently denied care or registration would gain enforceable protection immediately — a substantial reallocation of obligations and resources.
% FOUNDING_PROBLEM: Historically framed as solving the problem of allocating scarce care and social resources under uncertainty about a newborn's viability, and as a mechanism for population-quality management in eugenics-influenced policy regimes.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and resource-allocating institutions attest the problem (resource scarcity under viability uncertainty) remains live. Disability rights advocates and bioethics review bodies, from outside the beneficiary set, attest that the 'problem' was substantially a pretext for discriminatory exclusion and that viability triage can be handled without denying moral standing outright — their corroboration directly contradicts the architects' framing.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is high and rising (0.68 to 0.81) because the fitness threshold increasingly functions to justify withholding protection and resources from a widening interpretation of 'unfit' categories rather than resolving genuine viability uncertainty. Suppression is very high (0.88) because the excluded entities — infants — categorically cannot contest their own exclusion, and dissenting advocacy communities are kept outside the rule-making process. Accessibility collapse is comparatively low (0.35) because, unlike a genuine natural boundary, alternative framings (birth threshold, potential-based) remain visibly and actively contested in law and philosophy — the fitness criterion has not achieved uncontested naturalization. Resistance is correspondingly high (0.82): bioethicists, disability advocates, and reformed legal regimes actively challenge the framework wherever it is proposed or retained.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary institutions' seats, the arrangement resembles principled, resource-conscious coordination under uncertainty. From the payer seats — infants and their families — the identical structure operates as an imposed, unappealable exclusion mechanism with life-or-death stakes. The engine should compute markedly different per-seat classifications from this asymmetry; this divergence is exactly the seat-divergence phenomenon the framework exists to surface, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and eugenic policy architects sit at the beneficiary pole: they set criteria, bear no cost from failed tests, and use the boundary to achieve population-management or budgetary goals. Resource-allocating institutions similarly benefit by converting open-ended obligations into bounded ones. Pre-fitness infants and disabled neonates sit at the full-target pole: trapped exit, zero voice, maximal exposure to the criterion's consequences. Parents occupy an intermediate position — moderate power but constrained exit, since they can advocate but rarely overturn an institutional determination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resource allocation under genuine viability uncertainty) may have been narrowly real in early neonatal medicine, but the founding_problem_status is authored as contested precisely because the mechanism has been repeatedly redirected toward disability exclusion and population-quality goals unrelated to genuine medical uncertainty — a mandatrophy pattern in which a narrow coordination justification has been stretched to cover a much broader extractive practice. The rising extractiveness and suppression_requirement series document this drift directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criterion_stability,
    'Is the fitness criterion a stable, medically grounded threshold, or does its content shift opportunistically to track whatever population the administering authority wishes to exclude?',
    'Historical and comparative review of how fitness criteria have been defined and redefined across jurisdictions and eras; a criterion that tracks disability, class, or ethnicity markers rather than narrow viability indicates opportunistic redefinition.',
    'If the criterion is unstable and expansion-prone, this reading is closer to a pure exclusion mechanism (snare) than a genuine coordination device; if stable and narrowly medical, some coordination function may be defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_stability, empirical, 'Whether the fitness threshold is a stable medical criterion or an opportunistically redefined exclusion tool.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the fitness-contingent reading and the birth-threshold reading both be held as defensible interpretations of a single personhood concept, or does adopting one require rejecting the other''s core premise outright?',
    'Analysis of whether birth-threshold advocates could accept fitness-contingent exclusions as a special case, or whether the two premises (standing is unconditional at birth vs. standing is conditional on passing a test) are strictly incompatible within one legal framework.',
    'If genuinely incompatible, the two readings should be marked forecloses rather than coexists_with in future kernel-relation authoring; this affects how the framework models legal transitions between regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether fitness-contingent and birth-threshold readings are logically foreclosing or merely competing positions.').

omega_variable(
    test_administration_capture,
    'To what degree is the fitness test itself captured by the interests of the institutions that benefit from its outcomes (resource allocators, population-policy architects), versus administered by disinterested medical evaluators?',
    'Audit of who designs, funds, and revises fitness criteria across jurisdictions that have used this reading historically (e.g., early-20th-century eugenics-influenced neonatal policy).',
    'High capture would support classifying the administering authority''s seat as heavily extractive rather than neutrally coordinating; low capture would support a more genuine (though still ethically contested) coordination reading at that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(test_administration_capture, empirical, 'Whether fitness-test administration is captured by beneficiary institutions or genuinely disinterested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__fitness_contingent_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__fitness_contingent_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__fitness_contingent_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__fitness_contingent_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__fitness_contingent_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__fitness_contingent_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__fitness_contingent_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__fitness_contingent_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__fitness_contingent_reading, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__fitness_contingent_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__fitness_contingent_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__fitness_contingent_reading, suppression_requirement, 32, 0.87).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__fitness_contingent_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
