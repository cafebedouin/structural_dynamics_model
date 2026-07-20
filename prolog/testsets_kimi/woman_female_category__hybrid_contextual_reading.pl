% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Category Membership: Sex for Medical/Sports/Safety, Gender Identity for Social/Legal
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The hybrid contextual reading of woman/female category membership assigns
 *   biological sex as the relevant criterion in medical, sports, and safety
 *   contexts, while assigning gender identity as the relevant criterion in
 *   social and legal recognition contexts. It is one reading of the contested
 *   kernel 'woman_female_category', coexisting with sex_biology_reading and
 *   gender_identity_reading. This constraint is authored as a Tangled Rope:
 *   it provides genuine coordination for institutions seeking to minimize
 *   conflict between irreconcilable ontological claims, but it asymmetrically
 *   extracts from whichever group's preferred framework is subordinated in a
 *   given context. The claim/metric independence is maintained: the claimed
 *   type is tangled_rope while the metrics describe moderate but persistent
 *   extractiveness and rising enforcement requirements.
 *
 * KEY AGENTS:
 *   - institutional_adjudicators: Primary agenda-setter and beneficiary (institutional/constrained) â courts, sports bodies, medical boards, and HR departments that administer contextual boundary rules and benefit from conflict reduction.
 *   - sex_based_advocates: Primary payer (organized/constrained) â bear costs when biological sex is excluded from social/legal recognition contexts.
 *   - gender_identity_advocates: Primary payer (organized/constrained) â bear costs when gender identity is excluded from medical/sports/safety contexts.
 *   - non_binary_intersex_communities: Excluded (powerless/trapped) â do not fit the binary partition and are largely invisible to the hybrid framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.58).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.65).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Category Membership: Sex for Medical/Sports/Safety, Gender Identity for Social/Legal").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '78911033-3cb2-4c44-8d0d-d9d8f7fa867a').
narrative_ontology:cs_kernel_codification('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', distributed).
narrative_ontology:cs_authority_grounding('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', practice).
narrative_ontology:cs_interpretation_layer_present('78911033-3cb2-4c44-8d0d-d9d8f7fa867a').
narrative_ontology:cs_reading_relation('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', woman_female_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', woman_female_category__gender_identity_reading, influences).
narrative_ontology:cs_axiom('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', foundational, contextual_domain_sovereignty).
narrative_ontology:cs_axiom_status(contextual_domain_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', contextual_domain_sovereignty, conventional).
narrative_ontology:cs_axiom('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', foundational, pragmatic_conflict_mediation).
narrative_ontology:cs_axiom_status(pragmatic_conflict_mediation, holdable).
narrative_ontology:cs_axiom_grounding('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', pragmatic_conflict_mediation, instrumental).
narrative_ontology:cs_reference_frame('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', contextual_partition_equilibrium).
narrative_ontology:cs_drift_state('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', contemporary_policy_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78911033-3cb2-4c44-8d0d-d9d8f7fa867a', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_adjudicators).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_advocates).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_identity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, sports governing bodies, medical boards, and HR departments that must render decisions on sex and gender categorization. They benefit from reduced political and legal conflict because the hybrid rule allows them to defer metaphysical questions and issue domain-specific rulings. They enforce contextual boundaries through eligibility panels, policy manuals, and legal precedent. Their exit is constrained because abandoning the hybrid would force them to adopt a universally contested single criterion and absorb the resulting instability.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_adjudicators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, institutional_adjudicators, beneficiary).

% Advocates and organizations arguing that biological sex is the material and immutable basis of female category membership. They bear costs when courts and institutions recognize gender identity over biological sex in social, legal, and carceral contexts. While their framework dominates medical and some sports domains, its subordination in social recognition produces a fragmented and costly advocacy landscape with no exit from the institutional system.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_advocates, payer,
    organized, generational, constrained, global).

% Advocates and organizations arguing that self-identified gender identity determines female category membership. They bear costs when medical protocols, sports eligibility rules, and safety provisions rely on biological sex markers. While their framework dominates social and legal recognition domains, its subordination in sports and medicine forces repeated boundary battles with no institutional exit.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_advocates, payer,
    organized, generational, constrained, global).

% People whose bodies or identities do not align with binary sex or binary gender identity categories. The hybrid framework's partition offers no membership criterion for non-binary, intersex, or genderqueer persons, rendering them invisible or forcing them into ill-fitting categories across all contexts. They are not party to the compromise and lack voice in its administration.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, non_binary_intersex_communities, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, institutional_adjudicators).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions category membership disputes into domain-specific jurisdictions, allowing institutions to apply different membership criteria in different contexts without resolving the underlying ontological conflict between sex-based and gender-identity-based frameworks.
% TRANSFER_FUNCTION: Moves authority to define category boundaries from contested universal claims to institutional context-adjudicators; moves costs of subordination to whichever group's preferred framework is excluded in a given context.
% ABSENT_VOICES: Non-binary, intersex, and gender-nonconforming persons whose lived realities do not map cleanly onto either biological sex or binary gender identity categories are largely excluded from the hybrid framework's binary partitioning. Also absent are radical eliminativists who reject sex/gender categories entirely.
% DISAPPEARANCE_RATIONALE: If the hybrid contextual rule vanished overnight, medical protocols, sports eligibility rules, legal sex recognition frameworks, and anti-discrimination jurisprudence would all face immediate ontological conflict without a mediating principle; institutions would be forced to choose universal sex-based or gender-identity-based rules, and the current conflict-minimization equilibrium would collapse.
% FOUNDING_PROBLEM: Irreconcilable conflict between sex-based and gender-identity-based claims to category membership in law, medicine, and sport, where universal adoption of either framework produces unacceptable costs to the other group's interests and triggers political and legal instability.
% FOUNDING_PROBLEM_CORROBORATION: Institutional adjudicators (courts, sports bodies, medical associations) attest the conflict is live and that the hybrid approach reduces litigation and policy instability. Critics from both sex-based and gender-identity advocacy camps attest the 'conflict' is actually a rights-claim that the hybrid framework improperly compromises; independent legal scholars and political philosophers note the framework's emergence as a pragmatic judicial compromise rather than a principled resolution.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the constraint does not extract material rents but rather extracts ontological recognition â subordinating one framework per context. Suppression is substantial (0.65) because maintaining contextual boundaries requires active institutional enforcement (eligibility panels, legal precedent, medical protocols) and suppresses universal claims from both sides. Theater ratio is moderate-high (0.48) because institutions perform principled neutrality while actually enforcing a contested compromise. Accessibility collapse is moderate (0.48) because universal alternatives (sex everywhere or gender identity everywhere) remain thinkable but are institutionally blocked in specific domains. Resistance is moderate-high (0.58) because both advocacy camps actively resist in contexts where they lose.
 *
 * PERSPECTIVAL GAP:
 *   Institutional adjudicators experience the constraint as coordination (reduced conflict, manageable caseloads); sex-based advocates experience it as extraction in social/legal domains; gender-identity advocates experience it as extraction in medical/sports domains. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional adjudicators sit near the beneficiary end (d low) because the constraint subsidizes their need for decisional manageability and conflict avoidance. Both advocacy groups sit near the target end (d high) in the contexts where their reading is subordinated; as structurally persistent agents they experience the constraint as targeting their universal claims. Non-binary/intersex communities sit at full target (d highest) because the binary hybrid framework erases their existence entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mislabeling by requiring both coordination and extraction: it is not a Rope because it has identifiable victims (both advocacy camps in subordinated contexts); it is not a Snare because the coordination function (conflict reduction, domain-specific clarity) is genuine and not merely cover; it is not a Scaffold because it lacks a sunset clause and is not framed as transitional; it is not a Piton because its function is not atrophied â the institutional demand for conflict management is live. False summit risk is low because the constraint does not claim to be a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_stability,
    'Is the hybrid contextual reading a stable principled position or an unstable compromise between two irreconcilable kernel readings?',
    'Track institutional adoption rates, legal precedent stability, and policy reversal frequency over a multi-decade interval; measure whether the hybrid reading is consolidating or eroding.',
    'If the hybrid is unstable, the constraint may be a scaffold transitioning toward sex_biology_reading or gender_identity_reading; if stable, it is a distinct tangled_rope coordinating through enforced contextual partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the hybrid reading is a stable equilibrium or a transient compromise.').

omega_variable(
    victim_set_symmetry,
    'Does the victim set include both sex-based and gender-identity advocates symmetrically, or does one group bear systematically higher subordination costs across contexts?',
    'Empirical audit of legal outcomes, sports eligibility rulings, medical protocol adoptions, and discrimination statistics across jurisdictions weighted by population exposure.',
    'Asymmetric victimization would reclassify the constraint toward snare if one group is systematically targeted; symmetric costs support the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_symmetry, empirical, 'Whether extraction is symmetrically distributed across the two advocacy camps.').

omega_variable(
    context_boundary_artificiality,
    'Are the boundaries between medical/sports/safety and social/legal contexts naturally jointed or artificially imposed to sustain the hybrid reading?',
    'Examine boundary cases where domains overlap (e.g., healthcare access as a legal right, sports as social participation, safety in social spaces) and evaluate whether the hybrid framework produces coherent or absurd outcomes.',
    'If boundaries are arbitrary, the constraint''s coordination function is weaker than claimed and its extraction (enforcement costs on outliers) is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_boundary_artificiality, conceptual, 'Whether the contextual boundaries are functionally grounded or administratively imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__hybrid_contextual_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__hybrid_contextual_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(woma_tr_t40, woman_female_category__hybrid_contextual_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(woma_be_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(woma_be_t30, woman_female_category__hybrid_contextual_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(woma_be_t40, woman_female_category__hybrid_contextual_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(woma_su_t30, woman_female_category__hybrid_contextual_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(woma_su_t40, woman_female_category__hybrid_contextual_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
