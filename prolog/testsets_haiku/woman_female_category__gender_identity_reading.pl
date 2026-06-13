% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity as Category Membership (Woman/Female)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The constraint operationalizes gender identity (internal
 *   self-identification with a gender category, typically informed by
 *   neurobiology, psychology, and social experience) as the determinant of
 *   membership in the woman/female category, independent of chromosomal sex,
 *   reproductive anatomy, or developmental biology. This reading instantiates
 *   one contested framing of the kernel 'what makes someone a woman/female'—a
 *   kernel that also admits at least two other structurally distinct readings
 *   (sex_biology_reading: biological sex determines membership;
 *   hybrid_contextual_reading: context selects the criterion). This
 *   constraint story captures ONLY the gender-identity reading, treating it
 *   as a single internally coherent claim with its own beneficiaries,
 *   victims, extraction profile, and epistemic uncertainties. Sibling
 *   readings are separate constraints with separate ε values, separate
 *   stakeholder sets, and separate metrics. This story does not evaluate
 *   whether this reading is true, desirable, or superior to siblings—it
 *   models the structural dynamics and extraction profile of living under
 *   this rule.
 *
 * KEY AGENTS:
 *   - transgender_women: primary beneficiaries (gain legal recognition and category membership); identity-locked (cannot exit without existential cost)
 *   - biological_women in contested spaces: primary victims (lose clarity of sex-based protective categories; compressed access/recognition)
 *   - civil rights advocates (identity primacy): organized beneficiaries (legislative and cultural wins)
 *   - women's rights advocates (sex category defense): organized payers (marginalized positions, lost legislative contests)
 *   - legislative/judicial bodies: agenda-setters (define and enforce the rule; distributed power)
 *   - medical professionals: payers (navigate dual classification systems; constrained exit)
 *   - intersectional bodies: observers (document harms across constituencies)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.68).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.72).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity as Category Membership (Woman/Female)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'cb300d3e-cf80-4600-8543-bfaa11faf611').
narrative_ontology:cs_kernel_codification('cb300d3e-cf80-4600-8543-bfaa11faf611', formalized).
narrative_ontology:cs_authority_grounding('cb300d3e-cf80-4600-8543-bfaa11faf611', extraction).
narrative_ontology:cs_interpretation_layer_present('cb300d3e-cf80-4600-8543-bfaa11faf611').
narrative_ontology:cs_reading_relation('cb300d3e-cf80-4600-8543-bfaa11faf611', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('cb300d3e-cf80-4600-8543-bfaa11faf611', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('cb300d3e-cf80-4600-8543-bfaa11faf611', foundational, gender_identity_constitutes_sex_category).
narrative_ontology:cs_axiom_status(gender_identity_constitutes_sex_category, holdable).
narrative_ontology:cs_axiom_grounding('cb300d3e-cf80-4600-8543-bfaa11faf611', gender_identity_constitutes_sex_category, deontological).
narrative_ontology:cs_axiom('cb300d3e-cf80-4600-8543-bfaa11faf611', foundational, biological_sex_materially_irrelevant_to_woman_status).
narrative_ontology:cs_axiom_status(biological_sex_materially_irrelevant_to_woman_status, holdable).
narrative_ontology:cs_axiom_grounding('cb300d3e-cf80-4600-8543-bfaa11faf611', biological_sex_materially_irrelevant_to_woman_status, empirically_contingent).
narrative_ontology:cs_reference_frame('cb300d3e-cf80-4600-8543-bfaa11faf611', legal_recognition_based_on_identity).
narrative_ontology:cs_drift_state('cb300d3e-cf80-4600-8543-bfaa11faf611', contemporary_enforcement_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb300d3e-cf80-4600-8543-bfaa11faf611', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_nonconforming_individuals_seeking_recognition).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, biological_women_in_identity_contested_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, individuals_navigating_sex_segregated_contexts).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint benefits one group (transgender individuals, gaining recognition) by imposing costs on another (women navigating sex-segregated contexts, losing clarity). The extraction is asymmetric: the beneficiaries gain a fundamental dignity good (recognition and legal standing), while the payers lose a protective clarity (the unambiguous scope of 'woman' for sex-segregation purposes). Suppression (0.72) is elevated because the constraint's persistence depends on actively enforcing the identity definition against alternative definitions and on marginalizing the voices of those who read the kernel differently. Theater (0.41 at interval end) is moderate: a real identity-recognition function underlies the rule, but enforcement activity increasingly centers on defending against biological alternatives and managing the contested boundary rather than on the core identity function. Resistance (0.71) is high because the constraint is genuinely contested—no party has achieved unified legitimacy; the rule persists through legal/institutional enforcement, not through consensus. Accessibility collapse (0.62): alternatives (hybrid rules, context-specific boundaries, intersex/non-binary categories) have not completely disappeared, but institutionalization of the identity rule has narrowed the range of openly discussable alternatives. The measurement series trace rising extraction as the constraint's scope expands (more domains keyed to identity-based definitions), rising theater as enforcement machinery hardens against biological definitions, and rising suppression as dissenting voices are marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (transgender individuals) experiences the constraint as liberation and dignity-restoration—a rule that finally recognizes their actual identity and grants them legal standing. The victim seat (biological women in contested spaces) experiences the same rule as erosion of protective clarity and loss of exclusive category function. The agenda-setter seat (legislative bodies) experiences the constraint as a solution to the coordination problem of defining the category legally, but also as perpetually contested—every new domain (prisons, shelters, sports, medical contexts) reopens the definitional fight. The observer seat (judges, international bodies) sees the constraint as producing harms to all parties: misgendering for those whose identity is not recognized, loss of sex-based safety for those relying on it, inability to craft coherent policy without sacrificing someone's core needs. These divergences are structural, not opinion-based—the constraint's design necessarily creates different outcomes for different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women are full beneficiaries (d ≈ 0.1-0.2): they gain recognition, legal standing, access to female-designated spaces and protections; their identity-locked exit means they cannot escape the constraint without existential cost, but that doesn't lower their d value—it means the constraint hits them less hard (they want the recognition it offers). Biological women in contested spaces are full targets (d ≈ 0.8-0.9): they bear costs (competing for access, losing category clarity) through the same mechanism the beneficiaries benefit from. Civil rights advocates sit near beneficiaries (d ≈ 0.2): they collect legislative wins and cultural normalization but can exit if the movement shifts. Women's rights advocates sit near targets (d ≈ 0.75): they bear the cost of losing legislative contests and platform exclusion. Medical professionals are constrained targets (d ≈ 0.7): they bear the cost of dual classification systems without choosing the constraint. Legislative bodies are near-symmetric (d ≈ 0.5): they benefit from having a legal rule (clarity for certain purposes) but are constantly attacked from both sides and bear the cost of adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophy by the piton definition. It is a live, contested, actively enforced constraint whose persistence is driven by both beneficiary capture and genuine coordination need. Transgender individuals will defend the rule because it provides them recognition and legal standing (they benefit and their identity depends on it). Civil rights advocates will defend it as a matter of dignity and equality. The mandate—establishing a unified definition of 'woman'/'female' for legal purposes—is alive, not atrophied. The theater (0.41) reflects enforcement activity beyond the core identity function (defending against alternatives, managing boundary disputes), but it is not sufficient to classify this as piton. A true piton would show high theater (0.65+) with beneficiaries who don't actually use or defend it, and victims diffuse enough that no organized group is hurt enough to fix it. Here, both beneficiaries and victims are organized and actively defending/attacking the rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_definition_empirical_stability,
    'How stable and universally measurable is gender identity as a category criterion? Can it be applied consistently across legal contexts without systematic ambiguity or manipulation?',
    'Long-term administrative data on how identity-based category assignments evolve over individual lifespans; comparative law examining jurisdictions using identity criteria vs. biology criteria; audit studies on consistency of application.',
    'If identity is inherently fluid or context-dependent, the constraint cannot function as a stable legal definition and will require constant re-adjudication. If it is stable and measurable, it can function as a coordinate rule. The stability question affects whether extraction is a feature of the rule design or an inevitable consequence of its indeterminacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_definition_empirical_stability, empirical, 'Measurability and stability of gender identity as a legal category.').

omega_variable(
    suppression_vs_legitimate_boundary_defense,
    'To what extent is the measured suppression (0.72) active exclusion of legitimate dissent, and to what extent is it necessary maintenance of a unified legal boundary?',
    'Post-exit analysis: if dissenters are excluded from spaces where they could voice coherent objections and build coalitions, suppression is political; if they have platforms and resources but are losing democratic contests, the suppression is the normal operation of majoritarian law. Institutional autonomy study: do jurisdictions adopting identity criteria suppress alternative definitions, or do they coexist?',
    'If suppression is legitimate (needed to maintain a unified rule), the constraint may be reclassified as rope—genuine coordination with necessary enforcement. If suppression is political (excluding voices that could defeat the rule in open debate), extraction is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_legitimate_boundary_defense, empirical, 'Whether suppression maintains a legitimate legal boundary or excludes dissenting voices from democratic process.').

omega_variable(
    biological_sex_salience_by_context,
    'In which domains is biological sex information actually necessary or predictively useful, and in which is identity-based categorization sufficient or superior?',
    'Domain-by-domain evidence review: reproductive health (biology salient), cancer risk assessment (biology salient), athletic competition (contested—biology is relevant to fairness but identity-based participation has social benefits), criminal justice (safety depends on context—segregation by biology vs. identity vs. individual case assessment), legal marriage/custody (identity or biology salient?).',
    'If domains can be cleanly sorted (identity sufficient for legal/social, biology necessary for medical/athletic), a hybrid reading becomes viable and may reduce extraction by eliminating the need to force a universal rule. If boundaries between domains are porous (medical decisions affect legal status, legal categories affect safety), the constraint is necessarily extractive because no single rule can serve all purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_salience_by_context, empirical, 'Contextual salience of biological sex vs. gender identity across institutional domains.').

omega_variable(
    identity_locked_vs_chosen_identity,
    'Is gender identity a property that is chosen, discovered, or constitutively given—and does this distinction affect the constraint''s legitimacy as a rule?',
    'Neuroscience and psychology of gender identity formation; longitudinal studies of identity persistence and change; phenomenological accounts from trans and cis individuals on the choice/discovery/given distinction.',
    'If identity is discovered/given, the constraint recognizes a pre-existing fact about people and the extraction of suppression falls entirely on those whose identity differs from their biology. If identity is chosen, the constraint creates a new legal category that privileges expressed identity over biological facts, shifting who bears costs. Either way, the suppression (0.72) is justified differently depending on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_chosen_identity, conceptual, 'Ontological status of gender identity: chosen, discovered, or constitutively given.').

omega_variable(
    kernel_reading_foreclosure_analysis,
    'Does the gender_identity_reading logically foreclose the sex_biology_reading in the same legal framework, or can they coexist through jurisdictional division or contextual separation?',
    'Constitutional and statutory analysis: if a law establishes gender identity as THE criterion for the category woman, does it logically exclude sex biology as valid in any context? Empirical study: have jurisdictions successfully maintained both readings in parallel (identity for civil rights, biology for medical), or does one reading inevitably contaminate the other?',
    'If readings foreclose each other, the constraint is a zero-sum contest and the extraction profile is stable. If readings can coexist (via hybrid_contextual_reading), the constraint is a temporary equilibrium and may collapse or evolve into a more complex arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_analysis, empirical, 'Whether the gender_identity_reading logically forecloses or merely competes with sex_biology_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__gender_identity_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__gender_identity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__gender_identity_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(woma_be_t25, woman_female_category__gender_identity_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(woma_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(woma_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(woma_su_t25, woman_female_category__gender_identity_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, title_ix_sex_based_protections).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, sex_segregated_space_access).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, reproductive_healthcare_categorization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading kernel family on woman/female category membership. Gender-identity reading (this file) asserts identity determines membership; sex-biology reading asserts chromosomal/reproductive biology determines membership; hybrid-contextual reading asserts context selects which criterion applies. Each reading has distinct beneficiary/victim sets, distinct ε values (this one 0.68, biology reading ~0.45, hybrid ~0.52), and distinct suppression profiles (this one 0.72, biology ~0.58, hybrid ~0.65). The readings are linked via network.affects_constraints to enable contamination analysis and cross-reading comparison. The kernel contest is live across all US jurisdictions and most democracies; no unified answer exists. Comparative enforcement across readings will reveal which suppression mechanisms are essential vs. which are contested-position-specific.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__gender_identity_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
