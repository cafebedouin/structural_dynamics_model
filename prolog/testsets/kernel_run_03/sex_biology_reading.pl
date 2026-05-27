% ============================================================================
% CONSTRAINT STORY: sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_biology_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sex_biology_reading
 *   human_readable: Woman/Female Defined by Biological Sex (Sex-Biology Reading)
 *   domain: social_ontology/medical_classification/rights_frameworks
 *
 * SUMMARY:
 *   The sex-biology reading of 'woman' and 'female' defines these categories
 *   by chromosomal, reproductive, and anatomical markers. This reading
 *   grounds much contemporary legal doctrine on sex-based discrimination,
 *   medical classification, and rights protection. It serves genuine
 *   coordination functions: it provides stable reference for addressing
 *   pregnancy discrimination, sexual harassment targeting reproductive
 *   capacity, and access to sex-segregated spaces designed for safety from
 *   male violence. However, the reading also enforces asymmetric extraction:
 *   it defines out of the victim set those whose biology does not align with
 *   binary sex categories (intersex individuals) and those whose gender
 *   identity diverges from biological sex (trans women), despite evidence
 *   that these groups experience forms of gendered harm. The constraint
 *   exhibits all seven classification types across different perspectives,
 *   revealing the structural tensions inherent in the reading. This is ONE
 *   READING of a contested kernel (woman_female_category). Sibling readings
 *   (gender_identity_reading, intersectional_coexistence_reading) are
 *   separate constraint stories with different ε values and different
 *   victim/beneficiary sets. The sex-biology reading is a distinct structural
 *   choice with measurable institutional consequences.
 *
 * KEY AGENTS:
 *   - Cis women bearing sex-based discrimination: primary victims (powerless/trapped) — face reproductive, bodily, and social harms specific to reproductive capacity
 *   - Cis women's rights movements: organized beneficiary (organized/constrained) — benefit from unified sex-category legal doctrine and sex-segregated protections while bearing enforcement costs
 *   - Sex-based legal doctrine authorities: institutional beneficiary (institutional/arbitrage) — courts, legislators, regulatory bodies benefit from stable biological definition enabling consistent doctrine application
 *   - Intersex individuals: primary victims (powerless/trapped) — face classification instability and forced sorting into binary categories that deny their embodiment
 *   - Trans women seeking recognition: constrained victim (moderate/constrained) — excluded from sex-based discrimination victim set despite experiencing gendered harm; emerging alternative frameworks create scaffold sunset logic
 *   - Medical classification systems: institutional actor (institutional/arbitrage) — perpetuate sex-biology definition through inertia; perform biological verification without acknowledging its contestability
 *   - Analytical observer: civilizational analyst (analytical/analytical) — sees the reading as simultaneously coordinating and extractive; capable of recognizing the structural consequences of the reading choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_biology_reading, 0.52).
domain_priors:suppression_score(sex_biology_reading, 0.68).
domain_priors:theater_ratio(sex_biology_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_biology_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(sex_biology_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sex_biology_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_biology_reading, "Woman/Female Defined by Biological Sex (Sex-Biology Reading)").
narrative_ontology:topic_domain(sex_biology_reading, "social_ontology/medical_classification/rights_frameworks").

domain_priors:requires_active_enforcement(sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_biology_reading, '5f3d1d94-0dd0-46c4-842d-e611f0c3a335').
narrative_ontology:cs_created_at('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', '').
narrative_ontology:cs_kernel_codification('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', formalized).
narrative_ontology:cs_authority_grounding('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', extraction).
narrative_ontology:cs_interpretation_layer_present('5f3d1d94-0dd0-46c4-842d-e611f0c3a335').
narrative_ontology:cs_kernel_id(sex_biology_reading, woman_female_category).
narrative_ontology:cs_reading_relation('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', intersectional_coexistence_reading, influences).
narrative_ontology:cs_axiom('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', foundational, biological_sex_constitutes_woman_category).
narrative_ontology:cs_axiom_status(biological_sex_constitutes_woman_category, holdable).
narrative_ontology:cs_axiom_grounding('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', biological_sex_constitutes_woman_category, empirically_contingent).
narrative_ontology:cs_axiom('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', foundational, reproductive_capacity_marks_sex_based_harm).
narrative_ontology:cs_axiom_status(reproductive_capacity_marks_sex_based_harm, holdable).
narrative_ontology:cs_axiom_grounding('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', reproductive_capacity_marks_sex_based_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', biomedical_essentialist_authority).
narrative_ontology:cs_drift_state('5f3d1d94-0dd0-46c4-842d-e611f0c3a335', contemporary_trans_and_intersex_visibility_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_biology_reading, cis_women_spatial_access_protectors).
narrative_ontology:constraint_beneficiary(sex_biology_reading, sex_based_discrimination_legal_doctrine).
narrative_ontology:constraint_victim(sex_biology_reading, cis_women_excluded_from_male_violence_categories).
narrative_ontology:constraint_victim(sex_biology_reading, intersex_individuals_classification_instability).
narrative_ontology:constraint_victim(sex_biology_reading, trans_women_excluded_victim_set).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIS WOMEN (SNARE) — Cis women bear the reproductive, bodily, and social costs of sex-based discrimination (pregnancy discrimination, sexual harassment based on reproductive capacity, access to sex-segregated spaces for safety). Under the sex-biology reading, they are the canonical victims of 'sex-based' harm. High suppression: biological reality of reproductive capacity creates structural vulnerability that cannot be exited. Classification: Snare — maximum experienced extraction from trapped position.
constraint_indexing:constraint_classification(sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIS WOMEN'S RIGHTS MOVEMENTS (TANGLED ROPE) — Organized cis women's movements benefit from sex-category consolidation (unified legal doctrine of sex-based discrimination, protection of sex-segregated spaces) while also bearing costs of the category's enforcement (restricted coalition formation, mandatory biological testing in some contexts). Classification: Tangled Rope — genuine coordination function (unified rights strategy) AND asymmetric extraction (enforcement overhead, exclusion of trans and intersex voices from victim advocacy).
constraint_indexing:constraint_classification(sex_biology_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SEX-BASED LEGAL DOCTRINE (ROPE) — Legal authorities (courts, legislators, regulatory bodies) benefit from treating 'woman' as a stable biological category: it simplifies doctrinal application, provides clear legal boundaries, and avoids complexity. The constraint operates as pure coordination from this perspective — defining the category enables consistent application of sex discrimination law. No extraction experienced at this institutional level; the category is their primary tool.
constraint_indexing:constraint_classification(sex_biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERSEX INDIVIDUALS (SNARE) — Intersex people face classification instability under a sex-biology reading: chromosomal, reproductive, and anatomical markers may not align (e.g., XY chromosomes with female-typical reproductive anatomy, or mosaic chromosome patterns). The constraint forces sorting into binary categories that do not accommodate their embodiment. High suppression: forced classification creates bodily and legal precarity with no exit option. Classification: Snare — trapped in an ontology that denies their existence.
constraint_indexing:constraint_classification(sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANS WOMEN (SCAFFOLD) — Trans women are excluded from the victim set of sex-based discrimination under a sex-biology reading (classified as not-women by biological criteria). Yet many trans women face forms of gendered harm (trans-specific violence, reproductive discrimination against trans people, gendered social marginalization). The sex-biology reading creates a temporary constraint: alternative rights frameworks (gender-identity readings, intersectional frameworks) are emerging as parallel structures that recognize trans women's harms without requiring biological sex alignment. Sunset logic: as gender-identity and intersectional frameworks mature, the sex-biology constraint loses exclusive authority over who bears what harms.
constraint_indexing:constraint_classification(sex_biology_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL CLASSIFICATION SYSTEMS (PITON) — Medical ontologies (DSM, ICD, medical law) originally treated biological sex as the referent of 'woman' and 'female' categories. The classification persists through institutional inertia: medical training, insurance coding, and hospital protocols still use it. But the functional value has declined: medical research increasingly shows that trans women's health needs align with cis women's in some domains (e.g., hormone-responsive tissues) and diverge in others (e.g., prostate health), making the sex-biology category incomplete. Theater ratio is moderate (0.55) because medical practice performs biological verification (chromosome tests, reproductive anatomy assessment) without acknowledging that the verification is contestable and incomplete.
constraint_indexing:constraint_classification(sex_biology_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical frame, the sex-biology reading serves genuine coordination: it anchors legal doctrine, enables medical practice, and provides stable reference for addressing sex-based harms. BUT it also enforces asymmetric extraction: it denies recognition to those whose biology does not align with the category (trans, intersex individuals), excludes them from victim protection frameworks, and forces institutional sorting that creates bodily precarity. Classification: Tangled Rope — the reading coordinates sex-discrimination law AND extracts by defining out of existence those whose embodiment does not fit.
constraint_indexing:constraint_classification(sex_biology_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_biology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sex_biology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sex_biology_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sex_biology_reading, TR),
    TR >= 0.70.

:- end_tests(sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The sex-biology reading coordinates legal doctrine for sex-discrimination protection (genuine coordination benefit for cis women facing reproductive harms) but also extracts by defining out victim sets (intersex individuals, trans women). The asymmetric extraction is substantial because those excluded from the victim set face real harms but lack institutional recognition or protection. The reading's extractiveness has increased over the interval (from 0.38 to 0.52) as awareness of trans and intersex experiences has grown but institutional recognition has lagged — the gap between visibility of harm and institutional inclusion in victim protection creates accumulating extraction. Suppression (0.68): High. Multiple suppression mechanisms: (1) biological verification enforcement (chromosome tests, reproductive anatomy assessment) creates bodily precarity; (2) institutional investment in the sex-biology reading creates barriers to alternative framings; (3) cultural naturalization of biological sex as inevitable category ('just biology') prevents questioning the framework; (4) exclusion of trans and intersex voices from victim advocacy coalitions suppresses alternative perspectives. Theater ratio (0.55): Moderate. The sex-biology reading performs biological verification (medical testing, anatomical classification) without acknowledging that the verification is contestable. The performance is not as high as pure ritual (which would be 0.70+) because the reading does coordinate actual sex-discrimination doctrine. But significant theater exists: the reading naturalizes an institutional choice (to use biological markers rather than gender identity or intersectional markers) as biological necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates deep perspectival divergence across institutional positions. Cis women facing sex-based harm (Snare perspective) need the victim category protected and unified — the sex-biology reading serves their material interests. Legal doctrine authorities (Rope perspective) benefit from the category's stability and clarity. But intersex individuals trapped in classification systems (Snare perspective) experience the same institutional framework as extractive and dehumanizing — the category denies their existence. Trans women constrained by exclusion from victim sets (Scaffold perspective) see emerging alternative frameworks that could supersede the sex-biology reading. The piton perspective reveals institutional inertia: medical systems perpetuate the sex-biology category not because it optimally serves medical practice but because changing it would require institutional reorganization. The analytical observer perspective (Tangled Rope) recognizes both the genuine coordination the reading achieves and the real extraction it imposes — the reading cannot be dismissed as pure ideology or pure coordination, but must be assessed as a hybrid with measurable consequences for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the constraint. Cis women bearing sex-based harm are the canonical victims of the sex-biology reading (high d): they face extraction through reproductive vulnerability while lacking exit options (trapped). The reading's victim set is constructed to protect them — they are beneficiaries of the victim category itself while bearing the bodily costs the category addresses. Intersex individuals are victims (high d) by exclusion: they are trapped in binary classification systems that deny their embodiment. Trans women are excluded from the victim set (which reduces their d as potential victims while raising d for those the reading prioritizes), creating a perspectival gap — they face gendered harm but lack institutional recognition for it. Legal doctrine authorities are beneficiaries (low d) with arbitrage options: they can exit to alternative frameworks but benefit from the stability of the sex-biology reading. The piton perspective reflects institutional inertia rather than directionality: the medical system has low d (beneficiary of reduced complexity) but perpetuates the category through path dependence rather than active extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint instantiates the mandatrophy in the form of competing institutional readings of the same kernel. The mandate (provide sex-discrimination protection to those harmed by reproductive vulnerability) is legitimate and essential. But the trophe (realization through a specific reading that defines certain agents as not-women) creates secondary extraction. The mandatrophy is resolved not by choosing between protection and inclusion, but by recognizing that this reading (sex-biology) achieves its coordination goal while imposing costs that alternative readings could reduce. The analytical observer's Tangled Rope classification captures this: the reading is neither pure coordination (it excludes from protection those experiencing gendered harm) nor pure extraction (it genuinely protects against sex-based discrimination), but a hybrid that redistributes who bears and who receives recognition of gendered harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_marker_alignment,
    'Which biological markers (chromosomal, reproductive, anatomical, endocrine) define ''biological sex'' when they do not align?',
    'Empirical case analysis of intersex individuals; medical consensus on hierarchy of markers; legal precedent in classification disputes',
    'If chromosomal hierarchy: intersex XY individuals with female reproductive anatomy are classified as male, excluding them from sex-based protections. If reproductive hierarchy: XY individuals with female reproductive anatomy are classified as female, including them in victim set. Different marker choices produce different victim sets and different ethical outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_sex_marker_alignment, empirical, 'Which biological sex marker is canonical when markers misalign').

omega_variable(
    kernel_reading_contested,
    'Is ''woman'' constitutively defined by biological sex (sex-biology reading) or by gender identity and social role (gender-identity reading), or can both readings coexist in a single framework?',
    'Commitment-system analysis: examination of legal doctrine, medical ontology, and social practice to determine whether the two readings foreclose each other or coexist. If they coexist, the kernel accommodates multiple readings simultaneously; if they foreclose, only one reading can ground institutional authority.',
    'If forecloses: this reading (sex-biology) makes the gender-identity reading logically impossible within a single legal/medical framework; institutional choice of this reading excludes the alternative. If coexists: both readings remain live, and the constraint becomes a choice of which reading institutions will privilege, with consequences for victim recognition and rights protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'Whether sex-biology and gender-identity readings foreclose each other or coexist').

omega_variable(
    embodied_harm_alignment,
    'Do trans women experience the same forms of sex-based harm as cis women (reproductive discrimination, sexual harassment targeting reproductive capacity, pregnancy-related employment discrimination)?',
    'Epidemiological data on trans women''s experiences of gendered violence, workplace discrimination, and healthcare access; comparative analysis of harm categories across cis and trans women',
    'If trans women experience distinct harms aligned with reproductive capacity: the sex-biology reading''s victim set is accurate, and trans women''s harms are fundamentally different. If trans women experience overlapping harms: the sex-biology reading excludes from protection those who bear similar harms, revealing a mismatch between the reading''s victim set and actual embodied vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_harm_alignment, empirical, 'Whether trans women experience sex-based harms aligned with reproductive capacity').

omega_variable(
    institutional_reading_contingency,
    'Is the sex-biology reading institutionally contingent (chosen by institutional authority over alternatives) or necessary (dictated by the logic of biological classification)?',
    'Historical analysis of when and how institutions adopted the sex-biology reading vs alternatives; examination of whether institutional choice could have gone differently without internal logical contradiction',
    'If contingent: institutions could choose alternative readings (gender-identity, intersectional) without sacrificing logical coherence; the sex-biology reading is one institutional strategy among others. If necessary: institutional adoption of the sex-biology reading follows necessarily from biological fact; alternatives are incoherent. Contingency is the basis for reform pressure; necessity blocks it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reading_contingency, conceptual, 'Whether sex-biology reading is institutionally contingent or logically necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_biology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_biology_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sex__tr_t20, sex_biology_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(sex__tr_t40, sex_biology_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_biology_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sex__be_t20, sex_biology_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(sex__be_t40, sex_biology_reading, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_biology_reading, sex_discrimination_legal_doctrine).
narrative_ontology:affects_constraint(sex_biology_reading, sex_segregated_space_policy).
narrative_ontology:affects_constraint(sex_biology_reading, intersex_medical_classification).
narrative_ontology:affects_constraint(sex_biology_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% The sex-biology reading is part of a constraint family decomposing the contested kernel 'woman_female_category'. Each sibling reading (gender_identity_reading, intersectional_coexistence_reading) is a separate constraint story with its own ε value and its own victim/beneficiary set. The ε values differ because the readings make different empirical claims about which agents experience what harms: sex-biology reading (ε=0.52) includes reproductive-capacity harms; gender-identity reading includes gender-specific harms (e.g., misgendering, social role discrimination); intersectional reading includes simultaneously-experienced axes of oppression. These are not the same constraint viewed from different angles — they are structurally distinct claims about what constitutes women/female and who bears what harms. They are linked by network.affects_constraints to enable analysis of how institutional choice of one reading constrains alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
