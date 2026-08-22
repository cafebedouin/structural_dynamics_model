% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Biological Sex as Sole Determinant of Legal/Social Category Membership
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This story authors the biology-reading of the sex/gender category kernel:
 *   category membership (and downstream access to sex-segregated spaces,
 *   legal sex markers, and competitive sport categories) is determined by
 *   chromosomes and anatomy present at birth, treated as fixed regardless of
 *   subsequent social, legal, or medical transition. This is a contested
 *   legal and cultural arrangement, not a settled natural fact — the reading
 *   is authored on its own terms, as the standing arrangement its own
 *   advocates defend, not as a synthesis with the identity-reading or
 *   hybrid-reading siblings (separate constraint files). Enforcement has
 *   intensified over the interval as more jurisdictions codify birth-sex
 *   definitions in statute and more sports bodies adopt biological
 *   eligibility testing, which is why suppression_requirement and
 *   extractiveness both rise over the modeled 40-unit interval.
 *
 * KEY AGENTS:
 *   - cis_women_in_sex_segregated_spaces: Primary beneficiary (organized/constrained) — gains predictable, litigation-resistant access boundaries
 *   - biology_based_advocacy_organizations: Agenda-setter (organized/mobile) — drafts and litigates the definitional criterion
 *   - sports_governing_bodies_using_binary_categories: Beneficiary and enforcer (institutional/mobile) — administers eligibility testing
 *   - trans_women: Primary target (powerless/trapped) — categorically excluded regardless of transition
 *   - trans_men: Secondary target (powerless/trapped) — misclassified in the opposite direction
 *   - intersex_individuals: Structurally incompatible target (powerless/trapped) — forced into a binary their own biology does not fit
 *   - family_courts_and_registrars: Enforcement machinery (institutional/analytical) — adjudicates legal sex determinations
 *   - medical_and_legal_scholars: Analytical observer (analytical/analytical) — documents empirical fit and cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.62).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Biological Sex as Sole Determinant of Legal/Social Category Membership").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '872bf86b-cb61-44e0-a3bb-4d600318ad6d').
narrative_ontology:cs_kernel_codification('872bf86b-cb61-44e0-a3bb-4d600318ad6d', distributed).
narrative_ontology:cs_authority_grounding('872bf86b-cb61-44e0-a3bb-4d600318ad6d', distributed).
narrative_ontology:cs_reading_relation('872bf86b-cb61-44e0-a3bb-4d600318ad6d', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('872bf86b-cb61-44e0-a3bb-4d600318ad6d', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('872bf86b-cb61-44e0-a3bb-4d600318ad6d', foundational, reproductive_biology_is_the_sole_valid_category_criterion).
narrative_ontology:cs_axiom_status(reproductive_biology_is_the_sole_valid_category_criterion, holdable).
narrative_ontology:cs_axiom_grounding('872bf86b-cb61-44e0-a3bb-4d600318ad6d', reproductive_biology_is_the_sole_valid_category_criterion, empirically_contingent).
narrative_ontology:cs_axiom('872bf86b-cb61-44e0-a3bb-4d600318ad6d', foundational, birth_anatomy_is_immutable_for_legal_and_social_purposes).
narrative_ontology:cs_axiom_status(birth_anatomy_is_immutable_for_legal_and_social_purposes, holdable).
narrative_ontology:cs_axiom_grounding('872bf86b-cb61-44e0-a3bb-4d600318ad6d', birth_anatomy_is_immutable_for_legal_and_social_purposes, empirically_contingent).
narrative_ontology:cs_reference_frame('872bf86b-cb61-44e0-a3bb-4d600318ad6d', birth_registration_biological_sex).
narrative_ontology:cs_drift_state('872bf86b-cb61-44e0-a3bb-4d600318ad6d', contemporary_legal_contest_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('872bf86b-cb61-44e0-a3bb-4d600318ad6d', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_in_sex_segregated_spaces).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, biology_based_advocacy_organizations).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, sports_governing_bodies_using_binary_categories).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_men).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on sex-segregated spaces (shelters, prisons, sports, changing rooms) being defined by reproductive biology to preserve privacy, safety from male-pattern violence risk, and fair competitive categories. This reading validates their claim that the relevant boundary is biological, not self-declared, and gives them standing to exclude trans women from those spaces without needing to litigate individual cases.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women_in_sex_segregated_spaces, beneficiary,
    organized, generational, constrained, national).

% Lobby legislatures and courts to codify chromosomal/anatomical sex as the legal basis for category membership, litigate to enforce it in sports and single-sex services, and produce the definitional language used in statutes. They set the terms of the boundary and benefit reputationally and organizationally from its adoption.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, biology_based_advocacy_organizations, agenda_setter,
    organized, biographical, mobile, national).

% Administer eligibility rules for competitive categories and use birth-sex/biological criteria to police the female category, citing performance-relevant physiological differences. They both benefit from a clean administrable rule and enforce it directly through testing and exclusion.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sports_governing_bodies_using_binary_categories, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, sports_governing_bodies_using_binary_categories, agenda_setter).

% Are categorically excluded from the 'woman' category regardless of transition status, legal documentation, or years of social and hormonal transition. Face exclusion from women's shelters, sports, prisons, and legal recognition. Exit is not available — the classification follows them regardless of relocation, documentation change, or medical intervention, since the criterion is birth anatomy/chromosomes, which cannot be altered.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    powerless, biographical, trapped, national).

% Are categorized as female regardless of transition, which can force inclusion in women's spaces they no longer identify with or exclusion from male categories/services they do identify with, creating a distinct but structurally symmetric harm to trans women's exclusion — misclassification in the opposite direction.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_men, payer,
    powerless, biographical, trapped, national).

% Have chromosomal, gonadal, or anatomical configurations that do not map cleanly onto the binary the reading requires. Are administratively forced into one category or the other — sometimes via infant surgical assignment they never consented to — because the classification system has no accommodation for biological ambiguity, which the reading's own premise (immutable binary biology) does not actually describe for this population.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Administer birth certificates, identity documents, and legal sex determinations. Under this reading they treat birth-recorded sex as fixed and are the enforcement machinery that adjudicates disputes, denies amendments, or requires medical proof of 'biological' status for any legal change.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, family_courts_and_registrars, agenda_setter,
    institutional, generational, analytical, national).

% Study the empirical and definitional adequacy of chromosome/anatomy-based classification, including its poor fit for intersex variation and its divergence from clinical consensus on gender-affirming care. They document the boundary's costs and inconsistencies without holding power to change the rule.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_and_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively simple, non-negotiable criterion for allocating access to sex-segregated resources (shelters, prisons, sports categories, some medical and legal contexts) without requiring case-by-case adjudication of identity claims.
% TRANSFER_FUNCTION: Moves the benefit of unambiguous, low-litigation-cost category access to those whose birth-recorded sex matches their lived identity, while moving the cost of exclusion, misgendering, and forced administrative reclassification onto trans and intersex individuals whose lived identity diverges from or does not map onto their birth-recorded biology.
% ABSENT_VOICES: Intersex individuals whose biology does not fit either binary pole are rarely consulted in drafting the classification criteria; the reading's own advocates and cis-women beneficiary groups dominate the drafting and litigation process, while intersex advocacy organizations and trans individuals are typically excluded from the rooms where the statutory language is set.
% DISAPPEARANCE_RATIONALE: Beneficiary groups (sports bodies, biology-based advocacy organizations, some cis women's organizations) argue that removing the biological criterion would dissolve sex-segregated spaces and fairness protections built around it — a real rearrangement. Trans and intersex advocates argue the underlying coordination need (safety, fair competition) could be met by other criteria (self-identification, testosterone thresholds, case-by-case review) without the exclusionary costs, meaning the world would rearrange only at the margin. The parties do not agree on which is true.
% FOUNDING_PROBLEM: Historically, sex-segregated spaces and categories (prisons, shelters, sports, some legal statuses) were built on the assumption that reproductive biology tracked social role and vulnerability in a stable, verifiable way, and needed one clear administrable criterion rather than case-by-case identity assessment.
% FOUNDING_PROBLEM_CORROBORATION: Biology-based advocacy organizations and sports governing bodies attest the founding problem (physiological difference relevant to safety/fairness) remains live. Medical and legal scholars outside both advocacy camps note that intersex variation and the diversity of trans embodiment were never well-described by the binary model even at its founding, and that clinical consensus on gender-affirming outcomes has shifted substantially since the criterion was first codified — an assessment from outside the beneficiary set that the founding premise was always a simplification, not a settled biological fact.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, contested).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine but partial coordination function: sex-segregated safety and competitive-fairness concerns are real, but the criterion imposes categorical, non-negotiable exclusion on trans individuals regardless of any individualized safety assessment, and administratively forces intersex individuals into a binary the criterion's own premise does not describe. Suppression (0.62) is high because enforcement depends on birth records, chromosomal/anatomical verification, and legal non-recognition of transition — mechanisms that require active institutional maintenance (courts, sports bodies, registrars) rather than voluntary consensus. Resistance (0.78) is high because trans and intersex advocacy communities, medical and legal scholars, and some cis-women's organizations actively contest the criterion in courts, legislatures, and professional bodies. Accessibility_collapse (0.55) is moderate rather than near-total: legal and social alternatives (self-identification frameworks, hybrid medical-gatekeeping models) exist and are actively practiced in other jurisdictions, so the biology-reading has not achieved the near-complete alternative-foreclosure a mountain would show.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats (biology-based advocacy organizations, cis women's organizations, sports bodies), the arrangement reads as protective coordination — solving a genuine safety/fairness problem with a bright-line, low-discretion rule. From the payer seats (trans women, trans men, intersex individuals), the identical structure reads as categorical exclusion enforced by state and institutional machinery regardless of individual circumstance. The engine's per-seat computation should reflect this: agenda-setter/beneficiary seats compute closer to rope/tangled_rope; the powerless, trapped payer seats compute closer to snare, because for them suppression is total (no legal or administrative path around the criterion) even though the aggregate metrics show a hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women in sex-segregated spaces and sports bodies sit near the beneficiary end: the criterion is written to match their existing status, costing them nothing structurally while providing an enforceable boundary. Biology-based advocacy organizations are agenda-setters with mobile exit (they can relocate lobbying efforts, are not bound by the rule themselves). Trans women, trans men, and intersex individuals sit at the full-target end: the classification is applied TO them, they cannot alter the criterion (chromosomes/birth anatomy are by definition immutable), and legal/administrative exit is blocked (trapped) because the rule follows them across jurisdictions that adopt it and cannot be resolved by documentation, surgery, or hormone therapy under this reading's own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifiable, stable criterion for safety-relevant sex-segregated access) may remain partly live for narrow physiological contexts (some competitive sport categories) while being largely obsolete for others (general single-sex social services, legal identity documents) where the safety rationale is weaker and the exclusionary cost is high. Classifying this as tangled_rope rather than snare or rope prevents two errors: (1) treating the whole arrangement as pure protective coordination (rope) would erase the real, uncompensated cost imposed on trans and intersex populations who have no exit; (2) treating it as pure extraction (snare) would erase the genuine, independently-corroborated safety/fairness concerns some beneficiary groups hold that are not reducible to rent-seeking. The tangled_rope classification requires both a real coordination function AND an identifiable victim class paying through the same structure — both conditions are met here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biology_reading_sibling_contest,
    'Is the biology_reading the uniquely correct instantiation of sex/gender category membership, or is it one of three live contested readings (biology_reading, hybrid_reading, identity_reading) of the same underlying kernel, each held by different institutional and advocacy coalitions?',
    'No empirical resolution mechanism exists because this is a live, values-laden jurisdictional and definitional dispute — track legislative and judicial adoption rates across jurisdictions, and whether any reading achieves durable cross-partisan consensus versus remaining contested indefinitely.',
    'If treated as the sole legitimate reading, the exclusionary costs to trans and intersex populations are naturalized as the cost of a settled fact; if treated as one of three live readings, those costs are visible as the structural output of a specific, contestable policy choice among available alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biology_reading_sibling_contest, conceptual, 'Whether this reading is the sole legitimate framing or one of three contested siblings sharing a kernel.').

omega_variable(
    intersex_binary_forcing_ambiguity,
    'Does the biology_reading''s own premise (immutable chromosomal/anatomical sex) coherently apply to intersex individuals, or does the reading require an administrative binary-forcing step (surgical assignment, categorical sorting) that is not actually derived from the stated biological criterion?',
    'Clinical and legal review of intersex variation taxonomies against the statutory/administrative binary categories currently in use; document how many intersex presentations require discretionary sorting versus clean biological classification.',
    'If the binary-forcing step is discretionary rather than biologically derived, the reading''s claim to be a pure, objective biological criterion is undermined for a subset of the population it governs — suggesting the criterion is partly constructed even on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_binary_forcing_ambiguity, empirical, 'Whether the reading''s binary application to intersex individuals is genuinely derived from biology or is an administrative overlay.').

omega_variable(
    safety_rationale_scope_ambiguity,
    'For which specific contexts (prisons, shelters, sports, legal documents) is the safety/fairness rationale independently corroborated by evidence outside the advocacy coalitions, versus asserted without context-specific support?',
    'Context-by-context review of incident data, competitive performance data, and independent (non-advocacy) research for each sex-segregated domain the reading governs.',
    'If the rationale holds strongly in some contexts (e.g., some competitive sport categories) but weakly in others (e.g., general legal documentation), the tangled_rope classification may resolve into a rope in narrow contexts and a snare in others — supporting further decomposition into context-specific constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_rationale_scope_ambiguity, empirical, 'Whether the safety/fairness coordination rationale is uniformly strong or context-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__biology_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__biology_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__biology_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(sex__tr_t32, sex_gender_category__biology_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__biology_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__biology_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__biology_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__biology_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(sex__be_t32, sex_gender_category__biology_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__biology_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__biology_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__biology_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__biology_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(sex__su_t32, sex_gender_category__biology_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__biology_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the sex_gender_category kernel. sex_gender_category__identity_reading authors self-identification as the sole criterion (excludes no one on biological grounds; victim set shifts to cis women who dispute the criterion's fit for single-sex-space safety concerns). sex_gender_category__hybrid_reading authors a medical-gatekeeping combination (partial inclusion contingent on medical transition markers; victim set includes both non-medically-transitioning trans individuals and those who object to medicalization as a precondition for recognition). All three share the same underlying kernel (what determines category membership) but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications — per the ε-invariance principle they are not merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
