% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership Grounded in Biological Sex (Birth Anatomy Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the biological sex reading of the
 *   gendered_category_membership kernel: the framework where social
 *   categories 'woman' and 'man' are grounded in immutable biological markers
 *   (chromosomes, reproductive anatomy at birth) that are treated as natural,
 *   pre-social, and determinative of category membership. This is ONE reading
 *   among three structurally distinct readings of the same kernel
 *   (gendered_category_membership). The sibling readings are: (1)
 *   gender_identity_reading, where category membership is grounded in the
 *   individual's internal gender identity; (2) social_role_reading, where
 *   category membership is grounded in socially constructed roles and
 *   performances. All three readings instantiate different ε values,
 *   different beneficiary/victim structures, and different perspectives on
 *   the same social category system. This story focuses exclusively on the
 *   biological sex reading and makes no claims about the empirical truth or
 *   falsity of any sibling reading. The biological sex reading has high
 *   extractiveness (0.68) concentrated on transgender and intersex
 *   populations, with cisgender populations and institutional authorities
 *   positioned as beneficiaries. The high theater ratio (0.58, rising from
 *   0.35 to 0.58 over the 30-year measurement interval) indicates that the
 *   reading's institutional maintenance is increasingly performative —
 *   invoking biological immutability even as the empirical biological science
 *   becomes more complex. The suppression requirement (0.72, rising from
 *   0.62) shows that institutional enforcement of the reading's boundary has
 *   intensified as the reading faces organized contestation from the sibling
 *   readings.
 *
 * KEY AGENTS:
 *   - Transgender populations: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with a category the reading defines as impossible; bears maximum extraction through category denial
 *   - Intersex populations: Primary victim (powerless/trapped) — face structural barriers to category membership under the reading's binary requirement; forced into category assignments that deny biological reality
 *   - Cisgender women: Ambiguous beneficiary (moderate/constrained) — positioned as beneficiaries of sex-segregated protections but constrained by requirement to participate in boundary enforcement; experience tangled rope (coordination + extraction cost)
 *   - Medical/Legal Institutions: Secondary beneficiary (institutional/arbitrage) — derive institutional authority from gatekeeping biological sex category determination; low experienced extraction because they control the category definition itself
 *   - Rights-based reform movements: Organized challenger (organized/constrained) — see the reading as temporary institutional arrangement with sunset; building alternative frameworks through legal reform and medical guideline revision
 *   - Naturalized biology discourse: Institutional inertia (institutional/arbitrage) — the reading's authority mechanism relies on performative repetition of its naturalness claim; empirical basis eroding (piton classification)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.68).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.72).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, snare).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership Grounded in Biological Sex (Birth Anatomy Reading)").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, '4859b6dc-b62f-4968-8b73-5513b2dc32e2').
narrative_ontology:cs_kernel_codification('4859b6dc-b62f-4968-8b73-5513b2dc32e2', formalized).
narrative_ontology:cs_authority_grounding('4859b6dc-b62f-4968-8b73-5513b2dc32e2', lineage).
narrative_ontology:cs_interpretation_layer_present('4859b6dc-b62f-4968-8b73-5513b2dc32e2').
narrative_ontology:cs_reading_relation('4859b6dc-b62f-4968-8b73-5513b2dc32e2', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('4859b6dc-b62f-4968-8b73-5513b2dc32e2', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('4859b6dc-b62f-4968-8b73-5513b2dc32e2', foundational, category_membership_biologically_determined).
narrative_ontology:cs_axiom_status(category_membership_biologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('4859b6dc-b62f-4968-8b73-5513b2dc32e2', category_membership_biologically_determined, empirically_contingent).
narrative_ontology:cs_axiom('4859b6dc-b62f-4968-8b73-5513b2dc32e2', foundational, binary_sex_categories_exhaustive).
narrative_ontology:cs_axiom_status(binary_sex_categories_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('4859b6dc-b62f-4968-8b73-5513b2dc32e2', binary_sex_categories_exhaustive, empirically_contingent).
narrative_ontology:cs_reference_frame('4859b6dc-b62f-4968-8b73-5513b2dc32e2', binary_biological_sex_determinism).
narrative_ontology:cs_drift_state('4859b6dc-b62f-4968-8b73-5513b2dc32e2', contemporary_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4859b6dc-b62f-4968-8b73-5513b2dc32e2', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cisgender_populations).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, category_boundary_maintainers).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, transgender_populations).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, intersex_populations).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, category_ambiguity_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSGENDER POPULATION (SNARE) — Structurally mobile (can navigate legal systems, change appearance, modify documentation in some jurisdictions) but identity-locked by the reading's category framework. The constraint defines the target's identity as categorically impossible within the biological sex framework: a trans woman is positioned as not-a-woman by the reading's core claim. Exit from the category definition would require abandoning the identity itself. Suppression operates through institutional gatekeeping (medicine, law, records), social denial, and the target's internalization of the reading's boundary as metaphysically fixed. Maximum extraction: the target bears the full cost of the category dispute without the power to revise the definitional framework.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERSEX POPULATION (SNARE) — Faces structural barriers to category membership under the biological sex reading. The framework requires clear chromosomal or reproductive anatomy status; intersex variation produces ambiguous or mixed markers that the binary boundary cannot accommodate. Unlike the transgender target (whose exit is identity-locked), the intersex target is trapped by the reading's requirement for categorical clarity. Medical interventions often enforce binary classification in infancy; assignment decisions become irreversible barriers. The reading treats intersex existence as a violation of its own boundary conditions — the target is forced to inhabit a category that denies their biological reality. High suppression: medical protocols, legal documentation, social expectation all enforce binary assignment.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CISGENDER WOMEN (TANGLED ROPE) — Positioned by the reading as primary beneficiaries of the sex-segregated boundary (spaces, sports, privacy protections grounded in biological sex category). However, the classification is Tangled Rope rather than Rope because the reading produces internal costs: women must participate in enforcement of the boundary (contesting trans inclusion claims, defending sex-segregated institutions) even when this enforcement carries reputational or relational costs. The constraint coordinates genuine collective goods (access to sex-segregated spaces, athletic fairness standards based on physiology) alongside asymmetric costs borne by out-of-category groups. Moderate chi: women experience both coordination benefits and extraction costs. Some women refuse enforcement participation, incurring social penalties; others participate and experience internal conflict. Exit options are constrained by the reading's framing of sex-segregated protection as natural and necessary.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL AND LEGAL INSTITUTIONS (ROPE) — Gatekeepers of the biological sex category definition. Derive institutional authority from the reading: medical credentials enable determination of sex; legal power enables registration and documentation. These institutions experience the constraint as legitimate coordination — establishing shared definitions of sex category enables sports regulation, medical research standardization, healthcare resource allocation, and legal status assignment. They have arbitrage options: they can endorse or contest the reading without losing access to the system itself. The reading benefits institutional actors through authority expansion: the requirement to adjudicate biological sex category gives medical and legal systems expanded jurisdiction over identity determination, medical protocols (including irreversible interventions on intersex infants), and documentation gatekeeping. Low effective extraction experienced by institutional beneficiaries — they see the constraint as coordination, not coercion.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHTS-BASED LEGAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents (human rights bodies, trans advocacy coalitions, intersex organizations) perceive the biological sex reading as a temporary institutional arrangement undergoing structural revision. This perspective sees the reading's suppression mechanisms as increasingly indefensible under rights frameworks and decouples category membership from birth-assigned sex. The scaffold gate applies: the reading has a sunset clause (formalized through legal reform, medical guideline updates, documentation system changes). Over a generational horizon (15-25 years in jurisdictions with active reform), the biological sex reading is being displaced by alternatives. Low effective extraction from this perspective because the organized agents see an exit pathway and have the institutional capacity to construct it.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURALIZED BIOLOGY DISCOURSE (PITON) — The reading's primary mechanism of legitimation is performative naturalization: the claim that biological sex is immutable, binary, and determinative of social category operates as theater maintaining institutional inertia. The actual biological science shows significant complexity (chromosomal variation, hormonal mosaicism, reproductive anatomy variation) that the reading's discourse suppresses through selectivity. The reading persists because it appears self-evident ('of course sex is binary, everyone knows this') even as the evidentiary base for binary biological sex has eroded in medical and biological literature. Theater ratio (0.58) reflects that roughly half the reading's institutional maintenance is performative (invoking biological immutability) and half functional (coordinating actual sex-segregated institutions). The piton classification captures that the reading's authority now relies on theatrical repetition of its naturalness claim rather than on empirical strength.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational, universal perspective, the biological sex reading is often presented as immutable law: biology determines sex category; sex determines social role; deviation from the sex-binary is a violation of natural order. This perspective naturalizes the reading by treating it as pre-social, discovered rather than constructed. However, the structural data contradicts the mountain classification. The reading exhibits high extractiveness (0.68) concentrated on powerless and identity-locked agents, high suppression (0.72) requiring institutional enforcement, and significant theater (0.58). These metrics indicate a constructed constraint with identifiable beneficiaries and high extraction. The engine's false summit detector will reclassify this perspective as naturalized ideology rather than natural law.
constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gendered_category_membership__biological_sex_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, TR),
    TR >= 0.70.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The biological sex reading produces direct extraction from transgender and intersex populations through category denial, institutional gatekeeping, and forced category assignment. The extraction is asymmetric — those whose biology aligns with the binary framework (cisgender populations) experience low or no extraction, while those whose biology deviates from or contradicts the binary (transgender, intersex) bear concentrated costs. The trajectory from 0.52 to 0.72 over 30 years reflects extraction accumulation: as the sibling readings gain organizational coherence (gender_identity_reading, social_role_reading), the biological sex reading's enforcement intensifies to maintain its boundary, producing higher extraction. Suppression (0.72): Very high. Institutional gatekeeping through medical protocols, legal documentation, social denial, and internalized stigma operates at multiple levels. Transgender targets face suppression of both identity and category access (institutional denial + identity-lock). Intersex targets face suppression through forced binary assignment, often implemented in infancy through irreversible medical intervention. The suppression is structural, not incidental — the reading cannot function without enforcing the binary boundary. Theater ratio (0.58, rising from 0.35): Significant and increasing. The reading's institutional authority mechanism relies heavily on performative naturalization — invoking biological immutability as self-evident. The rising trajectory indicates that as the reading faces empirical and philosophical contestation, its institutional maintenance becomes more theatrical. Medical and legal institutions continue enforcing the boundary through rituals (sex designation on birth certificates, sex-segregated institutional protocols) that invoke biological determinism despite scientific complexity. The theater increase reflects that the reading's legitimacy claim (we are simply reflecting biology) is increasingly disconnected from actual biological science complexity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits perspectival divergence across all six types. The transgender population sees snare (identity-locked target with no exit from category denial). The intersex population sees snare (trapped target with structural barriers to membership). Cisgender women see tangled rope (genuine coordination function in sex-segregated institutions, but constrained by enforcement participation). Medical/legal institutions see rope (legitimate coordination, low experienced extraction). Rights-based movements see scaffold (temporary reading with sunset through legal reform). Institutional discourse sees piton (naturalized theater about immutability). The analytical observer risks seeing mountain (immutable biological law), but the structural data — high extractiveness, concentrated on specific populations, requiring high suppression, backed by performative theater — indicates false summit: the naturalization of a politically contested reading. The core perspectival gap is between those whose biology fits the reading (cisgender/binary) and those whose biology contradicts or deviates from it (transgender/intersex/non-binary). The reading makes these biological differences socially consequential through institutional enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position relative to the constraint. Transgender populations: victim status + identity_locked exit = high d (0.87–0.93), producing high f(d) and high chi. They bear extraction without the power to revise the category framework; their identity is constituted within the constraint's boundary, making exit equivalent to identity dissolution. Intersex populations: victim status + trapped exit = very high d (0.92–0.98), producing maximum f(d) and maximum chi. They face structural barriers (forced infant assignment, medical interventions, documentation gatekeeping) with no meaningful exit option. Cisgender women: mixed (beneficiary + victim) status + constrained exit = moderate d (0.48–0.58), producing moderate f(d) and moderate chi. They benefit from the reading's boundary but are constrained by requirement to maintain it; some experience suppression from enforcement participation. Medical/legal institutions: beneficiary status + arbitrage exit = low d (0.12–0.22), producing low f(d) and potentially negative chi. They control the category definition and can shift their institutional position if needed; they are least vulnerable to the constraint they administer. Rights movements: organized + constrained = moderate d (0.52–0.62), producing moderate chi; they have agency and exit pathways (legal reform) even if constrained by political resistance. The piton and mountain perspectives collapse these into single institutional contexts without agent differentiation — they represent institutional-level, not individual-agent, positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The biological sex reading instantiates mandatrophy through the contest between different readings of the gendered_category_membership kernel. The reading claims to provide mere coordination (reflection of biological fact) but delivers extraction (asymmetric costs concentrated on specific populations). The mandatrophy resolution requires asking: Is the reading primarily a coordination mechanism (Rope: legitimate standard-setting to enable sex-segregated institutions) or an extraction mechanism (Snare: institutional gatekeeping and category denial that benefits cisgender and institutional populations)? The perspectives answer: for powerless targets (transgender, intersex), it is snare. For moderate populations constrained by enforcement, it is tangled rope. For institutional beneficiaries, it is rope. For organized reform movements, it is scaffold (sunset). For institutional discourse, it is piton (theater). The mandatrophy is resolved not by choosing one type, but by recognizing that the reading functions as coordination for those whose biology fits its boundary and as extraction for those whose biology contradicts it. The entire constraint system exists because the boundary is contested — if biological sex categories were truly natural and uncontroversial, no institutional enforcement would be necessary. The high suppression (0.72) required to maintain the boundary reveals that the reading's biological claim is not self-evident but requires active institutional work. This work produces the extraction that makes the reading snare rather than rope for powerless targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_definitional_boundary,
    'What constitutes ''biological sex'' for category membership purposes — chromosome pair, reproductive anatomy, hormone profile, gamete production capacity, or some combination?',
    'Medical and biological science review of sex determination mechanisms; comparison of definitional frameworks across medical specialties and jurisdictions; empirical analysis of how many individuals fall into each category under different definitions',
    'Different definitions produce dramatically different boundary positions. XY chromosome + functional female anatomy = female under anatomy definition, male under chromosome definition. Hormone replacement therapy changes hormone profile but not chromosomes. Definition choice determines which groups are included/excluded and thus who experiences extraction. This is the fundamental omega for the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_sex_definitional_boundary, empirical, 'Operational definition of biological sex determining category membership').

omega_variable(
    kernel_reading_contest_framing,
    'Is this constraint ONE reading of a contested kernel (gendered_category_membership), or is it a natural law claim that biology determines category membership universally?',
    'If treated as reading: the other readings (gender_identity_reading, social_role_reading) are live alternatives held by different parties; classification follows indexical tuple. If treated as natural law: the reading claims biological necessity; the engine applies false summit detection (beneficiary presence + institutional enforcement trigger reclassification from mountain to snare/tangled_rope). This omega documents the interpretive choice.',
    'High. Treatment as reading legitimizes the three-way contest and enables perspectival analysis. Treatment as natural law naturalizes a politically contested claim, obscuring the reading''s beneficiaries and suppression mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Framing choice: kernel reading vs natural law naturalization').

omega_variable(
    identity_lock_mechanism_specificity,
    'For transgender populations, is the binding mechanism through the reading''s definitional framework (identity cannot fit the category, so identity is locked to rejection), or through internalization of the reading''s naturalness claim (the target believes they are categorically impossible)?',
    'Narrative and psychological analysis of identity formation under the reading; comparison of trans identity trajectories in jurisdictions where the reading is dominant vs where the identity_reading is dominant; analysis of internalized stigma vs structural exclusion',
    'If binding is definitional (the reading''s framework itself makes the identity unthinkable): the identity_locked classification is structurally justified; exit requires framework revision. If binding is internalization (the target has absorbed the reading''s naturalness claim): the target could exit the category lock through epistemological shift; reclassify as constrained rather than identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_specificity, empirical, 'Mechanism of identity-lock binding for transgender targets').

omega_variable(
    intersex_category_erasure,
    'Does the biological sex reading''s binary framework constitute a form of category erasure for intersex populations, or does it provide necessary clarity and medical coherence?',
    'Empirical analysis of medical outcomes for intersex individuals under interventions enforcing binary assignment vs non-binary recognition; legal analysis of documentation systems and category availability; intersex self-reported experience of category system fit',
    'If erasure: the reading''s suppression of intersex existence is structural — the reading cannot accommodate intersex biology without revision. If coherence: the reading''s binary requirement is a justified coordination mechanism despite imposing costs on minority populations. This determines whether the intersex population should be reclassified from trapped to constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_category_erasure, empirical, 'Whether binary framework erases or clarifies intersex category status').

omega_variable(
    cis_women_actual_beneficiary_status,
    'Do cisgender women actually benefit from sex-segregated institutions grounded in the biological sex reading, or do they experience the reading as a constraint that limits their autonomy by tying their interests to binary boundary maintenance?',
    'Survey and narrative analysis of cis women''s experienced benefits from sex-segregated spaces vs costs of boundary enforcement; analysis of coalition formation between cis women and trans women in rejecting the biological sex reading; measurement of variation across domains (sports, healthcare, prisons, shelters) in whether sex-segregation serves stated protective function',
    'If genuine benefit: the tangled rope classification stands; women experience mixed extraction and coordination. If actual constraint: reclassify cis women to snare alongside trans/intersex populations; the reading''s primary beneficiary is institutional authority, not the populations it claims to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_actual_beneficiary_status, empirical, 'Whether cisgender women actually benefit from biological sex reading').

omega_variable(
    institutional_authority_extraction,
    'Does the biological sex reading primarily serve legitimate coordination functions (medical research standardization, sports fairness, privacy protection) or primarily serve institutional interest expansion (medical gatekeeping authority, legal documentation control, expanded jurisdiction over identity determination)?',
    'Historical analysis of how the reading''s institutional enforcement expanded over time; comparison of stated coordination functions vs actual resource flows and authority expansion; analysis of alternative coordination mechanisms that could serve coordination functions without the reading''s suppression costs',
    'If legitimate coordination dominates: institutional rope classification justified; snare classification primarily applies to powerless targets. If institutional extraction dominates: the entire constraint reclassifies as snare with institutional beneficiaries and multiple victim classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_extraction, empirical, 'Institutional authority expansion vs coordination function legitimacy').

omega_variable(
    medical_science_evolution,
    'How does the reading''s empirical basis evolve as biological science advances in understanding sex determination, hormone biology, and reproductive variation?',
    'Longitudinal analysis of medical/biological science literature on sex determination and reproductive biology; tracking of how institutional guidelines and definitions incorporate new findings; analysis of lag between scientific advancement and institutional framework revision',
    'If scientific basis erodes faster than institutional framework adapts: the piton classification is confirmed; the reading is increasingly theater. If science sustains the reading: the mountain perspective gains credibility; the false summit detector must account for genuine scientific complexity underlying the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_science_evolution, empirical, 'Evolution of biological science basis for sex category definitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcm_bio_theater_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gcm_bio_theater_t15, gendered_category_membership__biological_sex_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(gcm_bio_theater_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(gcm_bio_extract_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gcm_bio_extract_t15, gendered_category_membership__biological_sex_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(gcm_bio_extract_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gcm_bio_suppr_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(gcm_bio_suppr_t15, gendered_category_membership__biological_sex_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(gcm_bio_suppr_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, sex_segregated_institutional_access).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, medical_sex_assignment_protocols).

% DUAL FORMULATION NOTE:
% The gendered_category_membership kernel decomposes into three structurally distinct constraints: biological_sex_reading (this file), gender_identity_reading (sibling), and social_role_reading (sibling). Each reading has different ε, different beneficiary/victim structures, and different perspectives. The three are linked as siblings in the network; changes in one reading's institutional authority affect the others. When legal systems shift from the biological_sex_reading to the gender_identity_reading (as occurs through legal reform), the constraint's extractiveness profile changes. When medical protocols shift from sex assignment at birth to multi-dimensional sex determination, the biological_sex_reading's suppression mechanism is undermined. The network links document these dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
