% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Woman/Female Category Membership: Hybrid Contextual Reading
 *   domain: political_philosophy/bioethics/law/gender_studies
 *
 * SUMMARY:
 *   The hybrid contextual reading of the woman/female category instantiates a
 *   pragmatic institutional compromise: category membership is determined by
 *   biological sex (chromosomes, reproductive anatomy, developmental biology)
 *   in medical, reproductive, and sports-segregation contexts, and by gender
 *   identity (self-identification with gender category, social role, legal
 *   recognition) in social and legal-recognition contexts. This reading
 *   coexists with two sibling readings—the sex-biology reading (category
 *   determined by sex in all contexts) and the gender-identity reading
 *   (category determined by gender identity in all contexts)—as three
 *   distinct policy frameworks that different institutional actors and
 *   advocacy coalitions can hold simultaneously. The hybrid reading's appeal
 *   is as a conflict-minimization strategy: by allowing both sex and
 *   gender-identity criteria to operate in their respective domains,
 *   institutional actors (courts, medical regulators, sports bodies) can
 *   defer choosing a single ontology and reduce the political salience of the
 *   category itself. However, the hybrid framework is structurally
 *   extractive: it achieves conflict-minimization by subordinating one
 *   reading's agents in the domains where the other reading applies. Trans
 *   women face systematic extraction in medical contexts (subordination of
 *   gender-identity criteria); trans men face systematic extraction in
 *   biological-sex-triggered legal contexts (subordination of gender-identity
 *   criteria); both sex-category and gender-identity advocates accept partial
 *   defeat in half the institutional domain. The extractiveness value (0.52)
 *   and rising suppression trajectory reflect the active enforcement required
 *   to maintain context-boundaries and the increasing political contestation
 *   over whether those boundaries are legitimate. Theater ratio (0.65)
 *   reflects the performative dimension of the institutional compromise: the
 *   framework is partially maintained through rhetorical emphasis on
 *   institutional necessity ('we need sex for medical safety'), not solely
 *   through structural inevitability.
 *
 * KEY AGENTS:
 *   - Trans women in medical/sports contexts: Powerless/trapped — subordinated in medical classification, excluded from women's sports categories or included under sex-based rules that conflict with gender identity
 *   - Trans men in legal/social contexts: Powerless/trapped — forced legal recognition by female category in reproductive rights and family law, subordinated when sex-based law applies
 *   - Sex-category stability seekers (biomedical/sports communities): Moderate/constrained — benefit from sex-based medical classification but bear costs of maintaining context-boundaries and defending the legitimacy of sex-category criteria
 *   - Gender-identity legal recognition seekers: Moderate/constrained — benefit from gender-identity-based legal recognition but bear subordination in sex-biology-triggered contexts
 *   - Institutional conflict-minimizers (courts, regulators, healthcare systems): Institutional/arbitrage — primary beneficiaries; extract from the ability to use both criteria strategically and defer definitional conflict to subordinated agents
 *   - Organized trans advocacy coalitions: Organized/constrained — see hybrid reading as partial victory (legal recognition) and partial defeat (medical subordination)
 *   - Organized sex-category advocacy coalitions: Organized/constrained — see hybrid reading as partial preservation (medical contexts) and partial loss (legal contexts)
 *   - Pluralism and temporal compromise advocates: Organized/constrained — see hybrid reading as a pragmatic scaffold with sunset logic, waiting for better context-specific alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.52).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.58).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Woman/Female Category Membership: Hybrid Contextual Reading").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/law/gender_studies").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, 'd17fc23b-9ee0-40dd-ac76-8443b251feb6').
narrative_ontology:cs_kernel_codification('d17fc23b-9ee0-40dd-ac76-8443b251feb6', distributed).
narrative_ontology:cs_authority_grounding('d17fc23b-9ee0-40dd-ac76-8443b251feb6', distributed).
narrative_ontology:cs_reading_relation('d17fc23b-9ee0-40dd-ac76-8443b251feb6', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('d17fc23b-9ee0-40dd-ac76-8443b251feb6', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('d17fc23b-9ee0-40dd-ac76-8443b251feb6', foundational, category_membership_context_dependent).
narrative_ontology:cs_axiom_status(category_membership_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('d17fc23b-9ee0-40dd-ac76-8443b251feb6', category_membership_context_dependent, conventional).
narrative_ontology:cs_axiom('d17fc23b-9ee0-40dd-ac76-8443b251feb6', foundational, institutional_conflict_minimization_legitimate).
narrative_ontology:cs_axiom_status(institutional_conflict_minimization_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d17fc23b-9ee0-40dd-ac76-8443b251feb6', institutional_conflict_minimization_legitimate, instrumental).
narrative_ontology:cs_created_at('d17fc23b-9ee0-40dd-ac76-8443b251feb6', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_conflict_minimizers).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, legal_recognition_actors).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_sex_segregated_medical_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_men_in_gender_identity_legal_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_category_definitional_stability_seekers).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_identity_legal_recognition_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANS WOMEN IN MEDICAL/SPORTS CONTEXTS (SNARE) — Trapped by the subordination of gender identity within medical classification frameworks. The hybrid reading mandates sex-category membership in medical contexts regardless of gender identity, creating structural extraction: exclusion from women's spaces when sex-based segregation applies, forced disclosure of medical/biological status in contexts seeking gender-identity recognition. No exit option — cannot change biological sex status retroactively or context-independently. Maximum experienced extraction.
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANS MEN IN LEGAL/SOCIAL RECOGNITION CONTEXTS (SNARE) — Trapped by the subordination of sex-biology categories within legal recognition frameworks. The hybrid reading mandates gender-identity membership in legal/social contexts regardless of biological sex, creating structural extraction: forced legal recognition by female category when sex-based law applies (reproductive rights, family law), erasure of gender identity in contexts where sex biology is the operative criterion. Cannot exit — legal status determined by biological category at birth or by cumbersome administrative change. Maximum experienced extraction in biological-sex-triggered contexts.
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SEX-CATEGORY STABILITY SEEKERS (TANGLED ROPE) — Constrained by resource barriers and institutional dependencies on sex-based classification. The hybrid reading offers genuine coordination benefit: preserves sex-segregation rationale in medical safety contexts (reproductive medicine, hormone-responsive conditions, average sex differences in athletic performance). But extraction exists: the hybrid framework requires defending and maintaining dual classification systems across institutional contexts, creating administrative burden and ongoing definitional conflict. Partial beneficiary (gets classification stability in medical/sports domains) and partial victim (bears costs of context-switching and legitimacy maintenance).
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GENDER IDENTITY LEGAL RECOGNITION SEEKERS (TANGLED ROPE) — Constrained by administrative barriers to legal status change and institutional dependencies on identity-based classification. The hybrid reading offers genuine coordination benefit: preserves gender-identity recognition in legal/social contexts (name, pronouns, social roles, legal status). But extraction exists: gender-identity-based classification is systematically subordinated when sex-biology law triggers (reproductive rights, custodial law, criminal sentencing in sex-segregated facilities, medical contexts). Partial beneficiary (gets legal recognition in civil/social domains) and partial victim (bears cost of subordination in sex-biology domains).
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL CONFLICT-MINIMIZERS (ROPE) — Institutional actors (courts, medical regulators, healthcare systems, sports governing bodies, legal departments) experience the hybrid reading as a coordination mechanism. The framework allows them to defer definitional conflict by adopting context-sensitive rules: 'sex-category for medical, gender-identity for legal' distributes responsibility and reduces the need to choose a single ontology. This is a beneficiary perspective — institutional actors extract from the ability to use both categories strategically, deferring costs to subordinated agents (trans people in contexts where their reading is not operative). Arbitrage exit: can move resources between contexts and adopt the reading-set most convenient to their institutional position.
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED ADVOCACY COALITIONS (TANGLED_ROPE) — Both trans rights advocates and sex-category stability advocates are organized but constrained by political feasibility and institutional dependence. Trans advocates see the hybrid reading as a partial victory (legal recognition) and a partial defeat (medical subordination). Sex-category advocates see the hybrid reading as a partial preservation (medical contexts) and a partial loss (legal contexts). Each coalition provides genuine coordination function (representing constituency preferences) while also participating in the extraction mechanism (accepting subordination of the other coalition's reading in half the domain). Neither has true arbitrage — both are locked into ongoing negotiation.
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PLURALISM AND TEMPORAL COMPROMISE ADVOCATES (SCAFFOLD) — Advocates for genuine pluralism and context-sensitive institutional design see the hybrid reading as a temporary scaffold: a pragmatic framework that acknowledges legitimate needs in both domains while avoiding metaphysical claims about what 'woman' or 'female' 'really' are. The sunset logic: as better alternative frameworks emerge (context-specific criteria for sex-segregation rather than categorical inclusion, improved trans-inclusive medical protocols), the binary hybrid framework becomes unnecessary. The coalition's constraint is that the scaffold is politically dependent — requires active enforcement of context-boundaries. If boundaries erode (medical contexts adopting gender-identity criteria, legal contexts adopting sex-biology criteria), the scaffold collapses.
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — The analytical observer at civilizational scope risks classifying category membership as an immutable property of persons and institutions: 'The category woman/female is determinate by nature; context-variation is only epistemological.' This perspective treats the hybrid reading as a necessary accommodation to human diversity rather than as a constructed institutional arrangement. However, the structural data contradicts the mountain classification — institutional beneficiaries exist, subordination mechanisms operate, and context-switching rules are actively enforced rather than naturally emerging. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(woman_female_category__hybrid_contextual_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The hybrid reading achieves conflict-minimization through systematic subordination of one reading's agents in half the institutional domain. Trans women bear extraction costs in medical contexts (exclusion from female-category medical services, forced disclosure, structural exclusion from sex-segregated medical research and treatment); trans men bear extraction costs in legal contexts (forced female-category legal status, subordination in reproductive rights law, sex-segregated incarceration). Sex-category and gender-identity advocates each accept subordination of their reading in half the domain. The extractiveness value reflects that the subordination is systematic and structural, not incidental. Suppression (0.58): Moderate-high. The hybrid framework is maintained through active enforcement of context-boundaries. Barriers include legal enforcement (sex-category registration requirements in medical law, legal gender-recognition procedures in civil law), institutional enforcement (medical classification requirements, sports eligibility rules), and rhetorical maintenance (emphasis on medical/sports necessity justifies sex-category subordination; emphasis on legal equality justifies gender-identity recognition). The suppression trajectory rises because as institutional actors more clearly identify the framework's contradictions, enforcement intensifies: more explicit context-switching rules, more litigation over boundary cases, more institutional guidance to maintain coherence. Theater ratio (0.65): Moderate-high. The hybrid reading relies substantially on institutional performance of category clarity. Much of the enforcement is rhetorical ('we need sex for medical safety') rather than directly tied to structural necessity. The theater ratio rises because the institutional necessity claims are increasingly contested—as medical protocols improve and individual assessment becomes more feasible, the sex-category requirement appears more performative than essential.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival divergence (Snare, Tangled Rope, Rope, Scaffold, Mountain) reveals the core structural conflict: the hybrid reading's appeal to institutional actors (coordination, conflict-minimization) is directly enabled by its extraction mechanism (subordination in context-specific domains). The trapped agents (trans women/men) see pure extraction. The moderate agents (sex/gender advocates) see mixed extraction-coordination. The beneficiary institutions see pure coordination. The analytical observer risks naturalization (Mountain). This pattern is diagnostic of a Tangled Rope constraint: genuine coordination function (institutional conflict-minimization) coupled with asymmetric extraction (subordinated agents pay the cost of context-switching).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: power level, exit options, and relationship to extraction flow. Trans women/men in trapped powerless positions bear high d (high experienced extractiveness) because they have no exit option and their reading is subordinated in half the domain. Sex-category and gender-identity seekers in moderate/constrained positions have moderate d because they have partial agency (can mobilize politically) but structural dependence (their recognition depends on maintaining institutional boundaries they don't control). Institutional actors in institutional/arbitrage positions have low d because they benefit from the ability to use both criteria strategically and can shift resources between contexts. Organized advocacy coalitions in organized/constrained positions have moderate d because they provide genuine constituency representation (coordination benefit) while accepting half-domain subordination (extraction cost). The engine derives d automatically from beneficiary/victim declarations and exit options; the commentary reflects the structural rationale for each positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that the hybrid reading's classification is inherently perspectival. The constraint is NOT 'which type is correct?' but 'what does each perspective see, and why do their classifications diverge?' The trapped agents see Snare (maximum extraction, no exit). The moderate agents see Tangled Rope (coordination benefit + extraction cost). The institutional beneficiaries see Rope (pure coordination benefit). The analytical observer risks Mountain (naturalization). The engine's mandatrophy resolution rules identify this as a multiply-satisfied constraint: different indexical tuples produce different types, but all types are correct within their perspective. The false-summit detection for the Mountain perspective flags that naturalization is an error — the constraint is constructed institutional arrangement, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_safety_threshold,
    'Which specific medical, reproductive, or athletic contexts actually require sex-category-based classification, and which can operate on individual medical parameters or gender-identity-based criteria?',
    'Empirical analysis of medical outcomes, athletic performance, and safety data by context: do sex-category-based rules produce better outcomes than individual medical assessment or gender-identity-based inclusion? Which specific conditions require reproductive-anatomy-based classification vs. hormone-status vs. individual assessment?',
    'If most medical contexts can operate on individual parameters: sex-category subordination in medical contexts is institutional convenience rather than necessity, reclassifying medical contexts toward gender-identity criteria and reducing extraction. If specific medical contexts genuinely require sex-category: legitimacy of medical sex-category subordination increases, but extraction persists in non-medical contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_safety_threshold, empirical, 'Which medical/athletic contexts genuinely require sex-category vs. individual assessment').

omega_variable(
    legal_recognition_inclusivity,
    'Can legal/social recognition frameworks operate on gender-identity criteria while maintaining sex-category registration for medical contexts, or does legal recognition of gender identity create systemic pressure to revise all sex-category registrations?',
    'Cross-jurisdictional comparison of dual-system implementations (legal gender recognition + maintained sex-category records): do they produce the reported institutional benefits (clarification, reduced conflict) or create new points of friction and definitional contest? Analysis of cascade effects: does legal gender recognition systematically trigger pressure for medical context revision?',
    'If dual systems function stably: hybrid reading is viable as a stable arrangement. If dual systems cascade or create new conflicts: hybrid reading is inherently unstable, foreshadowing convergence toward one reading (gender-identity or sex-biology dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_recognition_inclusivity, empirical, 'Whether dual legal/medical recognition systems remain stable').

omega_variable(
    institutional_beneficiary_identification,
    'Which specific institutions benefit most from the ability to use both sex-category and gender-identity criteria, and does this benefit correlate with the degree of extraction imposed on trans agents?',
    'Institutional analysis: mapping which actors (courts, healthcare regulators, sports bodies, military, corrections) defer definitional conflict through the hybrid framework, and quantifying the extraction costs borne by trans agents in each institutional context. Correlation analysis: do institutions with the highest apparent benefit (greatest flexibility) impose the highest extraction costs on trans agents?',
    'If strong correlation: hybrid reading is primarily an institutional convenience mechanism extracting from trans agents. If weak correlation: institutional flexibility may be a genuine coordination benefit rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_identification, empirical, 'Institutional beneficiaries and correlation with extraction costs').

omega_variable(
    competing_kernel_readings_foreclosure,
    'Does the hybrid contextual reading coexist with the sex-biology and gender-identity readings as three live policy positions, or does it foreclose one of the siblings by making one ontology impossible to sustain?',
    'Philosophical and legal analysis of commitment structure: does adopting the hybrid reading''s framework (sex-category for medical, gender-identity for legal) permit a party to maintain that sex-category or gender-identity is metaphysically foundational? Can institutions and agents simultaneously affirm that women are determined by sex-biology AND that gender-identity is determinative for legal recognition without contradiction?',
    'If coexists_with is correct: all three readings remain live and the contest is pragmatic. If hybrid forecloses one sibling: the contest is metaphysically zero-sum and one reading''s adoption systematically eliminates the logical space for the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_kernel_readings_foreclosure, conceptual, 'Whether hybrid reading forecloses or coexists with sibling readings').

omega_variable(
    false_summit_natural_law_claim,
    'Is the variability of woman/female across contexts a discovery of natural law (category membership is context-sensitive by nature) or a constructed institutional arrangement (actors created context-switching rules to manage conflict)?',
    'Historical institutional analysis: does the hybrid framework emerge from observation of natural category properties, or from institutional actors deliberately adopting context-sensitive rules to manage political conflict? If deliberate adoption: by whom, when, and what alternatives were rejected?',
    'If natural law: mountain classification is correct and the hybrid reading is an accurate discovery. If constructed: false-summit detection triggers, and the mountain perspective is naturalization of institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether category variability is natural law or constructed institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfc_hybrid_theater_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(wfc_hybrid_theater_t5, woman_female_category__hybrid_contextual_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(wfc_hybrid_theater_t10, woman_female_category__hybrid_contextual_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(wfc_hybrid_extract_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wfc_hybrid_extract_t5, woman_female_category__hybrid_contextual_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(wfc_hybrid_extract_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wfc_hybrid_suppress_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(wfc_hybrid_suppress_t5, woman_female_category__hybrid_contextual_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(wfc_hybrid_suppress_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, sex_segregated_medical_access).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, sex_segregated_sports_eligibility).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, legal_gender_recognition).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, reproductive_rights_entitlement).

% DUAL FORMULATION NOTE:
% The woman/female category kernel decomposes into three constraint stories with different ε values and structural configurations: sex_biology_reading (ε ≈ 0.25, Mountain for specific institutional contexts), gender_identity_reading (ε ≈ 0.28, Rope in social/legal contexts), and this hybrid_contextual_reading (ε = 0.52, Tangled Rope across mixed domains). These are not measurements of the same constraint from different angles — they are three structurally distinct policy frameworks with different institutional beneficiaries, victim sets, and classification outcomes. The hybrid reading is upstream of specific institutional constraints (medical access, sports eligibility, legal recognition, reproductive rights) whose extractiveness values depend on whether the hybrid framework or a pure reading (sex-biology or gender-identity dominant) is operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
