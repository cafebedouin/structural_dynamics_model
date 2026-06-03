% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   The woman_category kernel concerns what grounds membership in the legal,
 *   social, and competitive category 'woman.' Three structurally distinct
 *   readings compete: (1) sex_biology_reading: membership determined by
 *   chromosomal and anatomical criteria (typical female biology); (2)
 *   gender_identity_reading: membership determined by internal gender
 *   identity regardless of assigned sex; (3) intersex_accommodation_reading
 *   (this story): membership acknowledges biological sex as a spectrum and
 *   includes people whose biology is female-typical or ambiguous (not fitting
 *   the male typical pattern). This reading emerged as a focal point in elite
 *   sports (the Caster Semenya case) where the categorical contradiction
 *   became acute: intersex athletes with elevated androgen levels have
 *   neither typical female biology nor male biology, yet existing policies
 *   offered no coherent framework for accommodation. The
 *   intersex_accommodation_reading grounds category membership in biological
 *   position on the sex spectrum rather than in identity alone or chromosomal
 *   essentialism. This generates distinct victim/beneficiary patterns from
 *   the sibling readings: intersex athletes benefit from explicit biological
 *   accommodation but face new extraction mechanisms (biological testing,
 *   disclosure requirements); sports regulators face coordination demands
 *   (allocating athletes fairly without biological essentialism); the piton
 *   observation (that binary sex categories are performatively maintained
 *   despite internal biological incoherence) becomes analytically central.
 *   The constraint's extractiveness trajectory shows gradual increase as
 *   accommodation policies create enforcement overhead and biological testing
 *   expands.
 *
 * KEY AGENTS:
 *   - Intersex individuals with female-typical or ambiguous biology: Primary victims in elite sports contexts (powerless/trapped in competition); primary beneficiaries in civil/healthcare policy (moderate/constrained); face both accommodation and disclosure extraction.
 *   - Feminist and disability rights advocacy organizations: Primary beneficiaries (institutional/arbitrage); leverage this reading to strengthen case against biological essentialism; benefit from framework validation without facing policy implementation burden.
 *   - Sports governing bodies and regulatory agencies: Secondary extractors and coordinators (institutional/constrained); must develop enforcement protocols and categorization standards; experience tangled coordination-extraction dynamic.
 *   - Sex-segregated category systems as institutional structures: Piton actor (institutional/arbitrage); maintain binary categorization machinery despite contradictions exposed by intersex cases; theater increases as accommodation creates exception-handling overhead.
 *   - Intersex individuals in civil/healthcare domains: Secondary beneficiaries (moderate/constrained); benefit from legal recognition and healthcare access through category accommodation; constrained by privacy exposure and categorization requirements inherent to the mechanism.
 *   - Analytical observer: Witnesses how the natural-law mountain (biological sex determined by nature) collapses into a false summit when beneficiaries exist (the binary category system benefits those who fit the typical case).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.38).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.52).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '3cb27fc7-0dbe-48a7-b689-a45e2133e9d3').
narrative_ontology:cs_kernel_codification('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', formalized).
narrative_ontology:cs_authority_grounding('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', distributed).
narrative_ontology:cs_reading_relation('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', foundational, biological_sex_spectrum_acknowledgment).
narrative_ontology:cs_axiom_status(biological_sex_spectrum_acknowledgment, holdable).
narrative_ontology:cs_axiom_grounding('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', biological_sex_spectrum_acknowledgment, empirically_contingent).
narrative_ontology:cs_axiom('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', foundational, category_membership_follows_biological_position).
narrative_ontology:cs_axiom_status(category_membership_follows_biological_position, holdable).
narrative_ontology:cs_axiom_grounding('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', category_membership_follows_biological_position, conventional).
narrative_ontology:cs_reference_frame('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', spectrum_acknowledged_category_framework).
narrative_ontology:cs_drift_state('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', contemporary_policy_development, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('3cb27fc7-0dbe-48a7-b689-a45e2133e9d3', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals_with_female_typical_or_ambiguous_biology).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, feminist_and_disability_rights_advocates_affirming_biological_diversity).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_athletes_in_sex_segregated_sports).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, enforcement_officials_navigating_ambiguous_cases).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, legal_clarity_and_institutional_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERSEX ATHLETE IN ELITE SPORTS (SNARE) — Faces categorical enforcement that produces contradictory requirements (accommodated in policy, excluded in competition eligibility). Cannot exit elite sport without abandoning career; experiences biological categorization as extractive enforcement. The constraint extracts career opportunity and competitive access. Maximum perceived suppression because the categorization mechanism itself (defining which biological variations count as 'woman') is the extraction apparatus.
constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERSEX INDIVIDUAL IN CIVIL/SOCIAL POLICY (TANGLED ROPE) — Benefits from accommodation in healthcare, legal recognition, and social policy where biological diversity is acknowledged. But faces costs: legal documentation requires specification of category for access to sex-specific services; medical privacy is compromised through exposure of biological variation. Coordination function (recognizing that intersex variation exists and requires policy accommodation) coexists with extraction (the mechanism of recognition itself requires disclosure and categorization). Constrained exit because moving away from the category entirely means losing access to female-typical healthcare and legal protections.
constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEMINIST AND DISABILITY RIGHTS ADVOCACY ORGANIZATIONS (ROPE) — Experience this reading as pure coordination: recognizing intersex variation as part of biological diversity strengthens the case against biological essentialism in gender enforcement and expands the protection umbrella. These advocates benefit from the framework's validation of their core claim (biological diversity is real and relevant). Arbitrage exit: can shift focus to other domains where this frame produces credibility gains. The constraint coordinates their interests by providing legitimate voice for intersex accommodation.
constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SPORTS GOVERNING BODIES AND REGULATORY AGENCIES (TANGLED ROPE) — Face genuine coordination problem: allocating athletes fairly to categories when biological variation exists across a spectrum. But the constraint also extracts administrative burden and liability exposure. The intersex accommodation reading requires agencies to develop biological testing protocols, documentation standards, and case-by-case adjudication systems. Benefits: clearer legal standing for accommodation policies; reduction in legal challenges. Costs: high enforcement overhead; exposed to criticism from both binary-enforcement factions (sex_biology_reading) and identity-priority factions (gender_identity_reading). Constrained exit: cannot simply adopt either pure reading without facing legal exposure and fairness claims from the excluded faction.
constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SEX-SEGREGATED CATEGORY SYSTEMS (PITON) — The machinery of sex-segregated categories (in sports, bathrooms, prisons, military) persists largely through institutional inertia despite internal contradictions when applied to intersex variation. The intersex accommodation reading exposes the piton: the entire categorical system (binary male/female) is performative — it was designed for the statistical majority case and breaks at the edges (intersex variation, hormonal anomalies). The system continues to operate, issuing tests and rulings, but with declining functional legitimacy. Theater ratio high because the system generates compliance theater (athlete testing, documentation) while the underlying categories lack coherent biological grounding.
constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scope, one reading of the constraint appears as a natural law: biological sex is determined by chromosomes, reproductive anatomy, and hormone profiles, and these vary on spectra, not binary categories. Intersex variation is a biological fact, not a policy choice. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit. The 'biological fact' framing naturalizes what is actually a contested reading of how to classify ambiguous biological states. The false summit detection reveals that biological naturalism is itself a committer position, not a view-from-nowhere.
constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(woman_category__intersex_accommodation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, TR),
    TR >= 0.70.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The intersex accommodation constraint imposes measurable costs on targeted groups (biological testing, disclosure, category eligibility determination) but also provides coordination benefits (legal recognition, healthcare access, explicit policy acknowledgment). The extraction asymmetry is lower than sex_biology_reading's (which extracts from transgender people) and gender_identity_reading's (which can extract from athletes facing performance-boundary cases). But extractiveness varies dramatically by domain: low in healthcare/legal policy (mostly coordination), high in elite sports (the Semenya case shows extractive testing and eligibility denial). This story measures elite sports as the critical domain (highest structural tension), yielding moderate extractiveness. The trajectory (0.15 → 0.38 over 20 years) reflects increasing enforcement overhead as accommodation policies mature and biological testing protocols become standardized. Suppression (0.52): Moderate-high. The constraint suppresses alternatives (the pure sex_biology and pure gender_identity readings) through legal and institutional enforcement of the spectrum acknowledgment framework. But suppression is incomplete — both sibling readings maintain institutional presence, and the spectrum claim itself is increasingly empirically validated. Theater ratio (0.45): Moderate. The constraint generates functional activity (biological testing, category adjudication, policy development) but not primarily performative theater. However, the theater ratio increases as enforcement machinery develops (0.25 → 0.45), reflecting the piton observation: the binary category system begins running exception-handling protocols (testing, special cases) that grow increasingly performative as boundaries become explicit.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap appears between the intersex athlete in elite sports (snare: pure extraction from categorization mechanism) and the feminist advocacy organization (rope: pure coordination from biological diversity legitimation). The athlete bears the full cost of enforcement; the advocate benefits from the framework's theoretical validation. The sports regulator (tangled rope) occupies an intermediate position: genuine coordination function (fair allocation of athletes) coexists with extraction (administrative burden, liability, need to develop testing protocols that require disclosure and biological investigation). The piton perspective reveals that the entire sex-segregated category system is maintained through institutional inertia despite contradictions: the machinery generates compliance theater (testing, documentation, eligibility hearings) while the underlying binary categories lack biological coherence. The false-summit mountain perspective risks naturalizing the intersex_accommodation_reading itself ('biology determines category') when what is actually at stake is a specific policy choice about how to handle spectrum acknowledgment. All three readings (biology, identity, accommodation) already accept that biological variation exists; the kernel dispute is about what grounds membership given that spectrum.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) derives from directionality (d) — the agent's structural relationship to the constraint — filtered through power level and exit options. Intersex athletes in sports have high d (1.0 → victimhood from the categorization apparatus); powerless and trapped status amplify this to maximum extractiveness. Feminist advocates have low d (near 0 → beneficiaries from framework legitimation); institutional power with arbitrage exit produces low or negative χ. Sports agencies have intermediate d (0.55-0.60): they benefit from clearer categorization standards but bear enforcement costs. The piton classification derives from high theater_ratio despite moderate extractiveness: the system's functional output (fair category allocation) is real, but the mechanism increasingly relies on performative testing and exception-handling. The false-summit mountain reveals high d for the analytical observer who sees 'biological fact' as natural law: d ≈ 0.72 (observer position), but the structural beneficiaries exist (sex-segregated categories benefit those who fit typical patterns), triggering FSM reclassification to tangled_rope or snare depending on whether the natural-law framing conceals genuine extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not manifest the full mandatrophy paradox because the reading is domain-specific. In elite sports, extractiveness is high (0.48-0.62 depending on athlete's power level); in healthcare/legal policy, extractiveness is low (0.08-0.20). The constraint resolves the domain-split: measurement at high-extraction context (sports) yields snare/tangled_rope; measurement at low-extraction context (legal/healthcare) yields rope/scaffold. The mandatrophy appears only if one tries to force a unified classification across both domains — that failure is instructive, pointing to the ε-invariance principle: 'woman_category in elite sports' and 'woman_category in healthcare' are structurally different constraints with different ε values. The kernel indeterminacy (omega 4) is the remaining mandatrophy signature: the intersex_accommodation_reading does not resolve whether category membership should follow biological position, identity, or other factors; it specifies what biology is (spectrum) but not what biological position grounds membership. All three readings can in principle accept spectrum biology while disagreeing on membership grounds. The mandatrophy is resolved by accepting that the kernel dispute persists beneath the accommodationist surface, and that the policy task is not to resolve it but to manage the coexistence of readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_advantage_boundary_definitional,
    'What level of measured performance advantage constitutes a categorically relevant biological variation requiring separate competition categories?',
    'Meta-analysis of performance gap distributions between typical-female athletes and intersex athletes with elevated androgen levels; statistical modeling of whether gaps exceed normal within-category variance',
    'If threshold < 2 percentile: almost all intersex variation is performance-neutral (low ε in sports, constraint reduces to social accommodation). If threshold > 10 percentile: many intersex athletes are performance-advantaged (high ε in sports, constraint becomes extraction mechanism). Interpretation varies by reading: sex_biology_reading privileges biological difference; gender_identity_reading privileges identification; intersex_accommodation_reading must specify which biological variations are relevant to performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_advantage_boundary_definitional, empirical, 'Performance-advantage threshold for categorizing intersex variation as competitively relevant').

omega_variable(
    medical_necessity_vs_categorical_purity,
    'Should medical necessity (access to female-typical healthcare, legal sex documentation for civil purposes) drive category membership independently of competitive performance considerations?',
    'Policy mapping: analyze whether healthcare access and legal documentation categories must be identical to sports categories, or whether separate categorization systems for different institutional domains are coherent and legally sustainable',
    'If decoupled: low ε in civil/healthcare domains (pure coordination), but high ε in sports (extraction mechanism). If coupled: unified category definition required, creating zero-sum conflict between domains. Current institutional structure attempts coupling with exceptions, producing high theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_categorical_purity, conceptual, 'Whether medical/legal and sports categorization must be unified or can remain decoupled').

omega_variable(
    biological_spectrum_vs_discrete_categories,
    'Is biological sex a continuous spectrum requiring threshold-based categorization, or are there natural biological discontinuities that justify discrete categories?',
    'Biological review: distribution analysis of sex-determining characteristics (chromosomes, hormones, reproductive anatomy) across human populations to identify whether clusters exist or whether variation is unimodal',
    'If continuous/unimodal: intersex_accommodation_reading''s biological premise is empirically grounded, but requires explicit threshold-setting (which biological variations count as ''woman''?). If discrete clusters exist: biological grounding for distinct categories may support sex_biology_reading. If bimodal with tail: intersex_accommodation_reading''s spectrum claim is accurate, but the majority-case sex_biology_reading remains descriptively accurate for ~99% of humans.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_spectrum_vs_discrete_categories, empirical, 'Whether biological sex is continuous spectrum or contains natural discrete clusters').

omega_variable(
    kernel_reading_indeterminacy,
    'This constraint is one reading of the contested woman_category kernel. Does the intersex_accommodation_reading genuinely resolve the kernel ambiguity, or does it sidestep it by introducing a third dimension (spectrum acknowledgment) that all readings could in principle accept?',
    'Test whether sex_biology_reading and gender_identity_reading can each incorporate ''biological sex is a spectrum'' without collapsing into intersex_accommodation_reading. (Sex_biology reading: ''spectrum exists but category membership follows most common biological pathway.'' Identity reading: ''spectrum exists and category membership follows identity regardless of position on spectrum.'') If both can incorporate spectrum claim, then intersex_accommodation_reading''s core contribution is not spectrum acknowledgment but the specific claim that category membership FOLLOWS biological position on spectrum.',
    'If both readings can incorporate spectrum: intersex_accommodation_reading is a specific policy choice about what grounds category membership, not a resolution of the kernel. The kernel dispute persists (what grounds membership?) but now all parties agree biology is a spectrum. If neither reading can incorporate spectrum without collapsing: intersex_accommodation_reading is a genuine synthesis. Current evidence suggests the former — all readings in principle accept that biological variation exists; they disagree about what (identity, biology, or other factors) should ground category membership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether this reading resolves or sidesteps the kernel contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t10, woman_category__intersex_accommodation_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(woma_be_t10, woman_category__intersex_accommodation_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t10, woman_category__intersex_accommodation_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, elite_sports_categorization_fairness).

% DUAL FORMULATION NOTE:
% The woman_category kernel has three structurally distinct readings, each with its own constraint story. This story models the intersex_accommodation_reading. The sex_biology_reading and gender_identity_reading are separate constraints with different victim/beneficiary sets, ε values, and classification patterns. Network edges link all three to show that they share a kernel (the definition of 'woman') and influence one another (the existence of one reading constrains or pressures the others). All three readings also influence the elite_sports_categorization_fairness constraint, which operates at higher specificity (a particular domain where the kernel dispute becomes acute).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
