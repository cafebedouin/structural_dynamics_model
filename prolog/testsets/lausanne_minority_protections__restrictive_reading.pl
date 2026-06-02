% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Lausanne Protections Restrictive Reading: Institutional Autonomy as Domestic Matter
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty of 1923 established protections for religious
 *   minorities in Turkey (Greek Orthodox, Armenian Apostolic, Jewish
 *   communities) in the aftermath of the population exchange. The treaty
 *   language is ambiguous regarding institutional autonomy: it explicitly
 *   protects individual worship rights but leaves unclear whether minority
 *   religious institutions retain autonomous control over property,
 *   theological education, and legal personality. The restrictive reading
 *   interprets Lausanne as protecting only individual worship — institutional
 *   matters are construed as domestic affairs subject to Turkish law. Under
 *   this reading, the state can enforce property confiscation (framed as tax
 *   enforcement or heritage preservation), prevent theological education
 *   (framed as educational regulation), and deny institutional legal
 *   personality (framed as corporate law application). This constraint story
 *   models the restrictive reading as an extraction mechanism: it preserves
 *   formal treaty compliance ('individuals may worship') while enabling
 *   comprehensive institutional foreclosure. The extractiveness trajectory
 *   (0.35 → 0.62 over 40 years) reflects the accumulating effect of legal
 *   restrictions, property confiscations, and institutional capacity
 *   degradation. The suppression requirement (rising from 0.45 to 0.68)
 *   reflects increasing administrative and legal coercion needed to enforce
 *   the interpretation as minority institutions resist and international
 *   pressure mounts. Theater ratio (0.48 → 0.55) indicates moderate
 *   performative content — the state claims legal consistency with Lausanne
 *   while behaving inconsistently with international human rights standards.
 *
 * KEY AGENTS:
 *   - Turkish State Apparatus: Primary beneficiary (institutional/arbitrage) — gains control over minority institutional capacity through domestic law subordination; benefits from formal Lausanne compliance while achieving de facto institutional control
 *   - Minority Religious Institutions: Primary victims (powerless/trapped) — Greek Orthodox Patriarchate, Armenian Apostolic Church, Jewish communities; face property confiscation, education foreclosure, legal personality denial; cannot exit or appeal to treaty protection under this reading
 *   - Individual Minority Believers: Secondary victims (moderate/constrained) — retain formal worship rights but pay rising costs maintaining institutions through domestic-law restrictions; constrained exit from institutional system
 *   - International Human Rights Bodies: Organized monitors (organized/constrained) — UN, EU, ICC create pressure but operate with constrained enforcement power; see the gap between individual and institutional protection
 *   - Treaty Interpretation Establishment: Institutional actors (institutional/arbitrage) — national courts, government legal advisors, academic commentators; maintain restrictive interpretation through procedural legitimacy despite accumulating counterevidence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating a contested interpretation as natural legal consequence rather than strategic reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.62).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.68).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Lausanne Protections Restrictive Reading: Institutional Autonomy as Domestic Matter").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, '01f805d4-361e-4db4-916b-5a71e4314982').
narrative_ontology:cs_kernel_codification('01f805d4-361e-4db4-916b-5a71e4314982', fixed_text).
narrative_ontology:cs_authority_grounding('01f805d4-361e-4db4-916b-5a71e4314982', extraction).
narrative_ontology:cs_interpretation_layer_present('01f805d4-361e-4db4-916b-5a71e4314982').
narrative_ontology:cs_reading_relation('01f805d4-361e-4db4-916b-5a71e4314982', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('01f805d4-361e-4db4-916b-5a71e4314982', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('01f805d4-361e-4db4-916b-5a71e4314982', foundational, minority_institutions_not_protected).
narrative_ontology:cs_axiom_status(minority_institutions_not_protected, holdable).
narrative_ontology:cs_axiom_grounding('01f805d4-361e-4db4-916b-5a71e4314982', minority_institutions_not_protected, conventional).
narrative_ontology:cs_axiom('01f805d4-361e-4db4-916b-5a71e4314982', secondary, state_domestic_law_supremacy).
narrative_ontology:cs_axiom_status(state_domestic_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('01f805d4-361e-4db4-916b-5a71e4314982', state_domestic_law_supremacy, conventional).
narrative_ontology:cs_reference_frame('01f805d4-361e-4db4-916b-5a71e4314982', lausanne_treaty_narrowly_construed).
narrative_ontology:cs_drift_state('01f805d4-361e-4db4-916b-5a71e4314982', contemporary_institution_deterioration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('01f805d4-361e-4db4-916b-5a71e4314982', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_religious_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, theological_education_systems).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_property_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Minority religious institutions (Greek Orthodox Ecumenical Patriarchate, Armenian Apostolic Church, Jewish communities) face comprehensive extraction: property confiscation under domestic law, theological education foreclosure, denial of legal personality, inability to maintain institutional continuity. Exit is impossible — institutions cannot relocate or abandon their property; they face progressive institutional capture. Maximum experienced extraction.
constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Individual believers retain formal worship rights under Lausanne but pay rising costs for institutional maintenance. They cannot exit the institutional system without abandoning religious identity and community. Constrained exit despite formal protections — the constraint extracts through the gap between individual and institutional rights. Significant extraction, though less absolute than institutional experience.
constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% State benefits from the restrictive reading as coordination: it solves the problem of controlling minority institutional capacity while maintaining formal Lausanne compliance. The state can claim protection of individual worship rights while subordinating institutional autonomy to domestic law. Net beneficiary with strategic escape routes via administrative law. Low or negative experienced extraction.
constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% UN human rights monitoring, EU accession standards, and ICC precedent create coordination pressure: states using the restrictive reading face documented criticism and conditional aid/membership. Organized international actors benefit from clarifying the gap between individual and institutional protections. Mixed extraction and coordination — the monitoring constrains state action but also constrains minority institutions that cannot appeal to institutional rights under this reading.
constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The formal interpretation of Lausanne as protecting individual worship only persists through institutional inertia despite accumulating counterevidence (minority institutional collapse, property confiscations, education system failures). The restrictive reading is maintained because alternatives haven't achieved consensus, not because it works — the theater ratio (0.55) reflects the gap between formal legal claim and structural reality. Degraded institutional function maintained through procedural repetition.
constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the restrictive reading might appear to be a natural consequence of treaty text interpretation: the Lausanne Treaty specifies protection for individual worship rights, and interpreting this narrowly is a matter of legal methodology. However, this perspective risks naturalizing a contested reading as if it were the inevitable product of neutral legal analysis. The structural data reveals the beneficiary structure and extraction mechanisms — the false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lausanne_minority_protections__restrictive_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, TR),
    TR >= 0.70.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The state captures significant benefit through institutional control, property acquisition, and educational authority without formal treaty violation. But the extraction is constrained by international monitoring and minority institutional resistance — this is not maximal extraction (which would be snare at 0.80+). The base extractiveness reflects that the mechanism is systematic (property confiscations follow administrative procedures) but partially checked by international pressure. Suppression (0.68): High. Substantial barriers to exit and institutional maintenance exist: property laws exclude minorities from owning religious institutions, education law prevents theological training, corporate law denies institutional legal personality, administrative procedures are opaque and subject to state discretion. Suppression is not maximal (0.80+) because individual believers retain worship rights in practice and international oversight creates some procedural predictability. Theater ratio (0.55): Moderate. The restrictive reading maintains formal legal consistency (applying general law uniformly) while achieving selective institutional subordination — the performance is that the law treats minorities equally, the reality is differential impact. The moderate theater reflects that the mechanism requires ongoing legal justification and administrative procedures rather than pure coercion, but outcomes are systematically asymmetric.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary (state, rope) and victim (minorities, snare) is maximal. The state experiences the constraint as coordination of legitimate regulatory goals (applying domestic law uniformly, maintaining state sovereignty). Minority institutions experience pure extraction with no coordination benefit — they have no say in the rules they must follow and no exit route. The international human rights observer (tangled rope) sees the gap itself as the problem: the constraint coordinates some state interests while extracting from minorities. The treaty interpreter (piton) sees performative maintenance of a degraded institution (formal legal interpretation that doesn't match observable state behavior). The analytical observer (mountain) risks naturalizing this as legal methodology rather than recognizing the extraction driver.
 *
 * DIRECTIONALITY LOGIC:
 *   Turkish state apparatus experiences low or negative experienced extraction (d ≈ 0.15, f(d) ≈ -0.01): institutional power, arbitrage-level exit options, beneficiary status. Minority institutions experience maximum extraction (d ≈ 0.95, f(d) ≈ 1.42): powerless position, trapped exit, victim status. Individual believers experience high extraction (d ≈ 0.85, f(d) ≈ 1.15): moderate power, constrained exit, victim status. International monitors experience moderate extraction (d ≈ 0.55, f(d) ≈ 0.75): organized power, constrained enforcement exit, neither pure beneficiary nor pure victim. Scope modifier for national scope (σ = 1.0) does not amplify or reduce; the extraction is primarily driven by the power differential and trapped exit, not by verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy between snare and other types is resolved by recognizing that the constraint exhibits different types from different structural positions. From the state's perspective (institutional/arbitrage), it is coordination (rope). From the minority institutional perspective (powerless/trapped), it is pure extraction (snare). From the international monitor perspective (organized/constrained), it is mixed (tangled rope). The analytical perspective risks treating all readings as equivalent and natural — false summit detection fires because beneficiaries are clearly identified (state apparatus) and extraction mechanism is systematic (institutional foreclosure). The constraint is not a natural law of treaty interpretation — it is a strategic reading that extracts from minorities while maintaining formal compliance. Resolving mandatrophy requires recognizing that 'treaty interpretation' is not a neutral technical process but a site where different parties have structural interests in different readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_ambiguity,
    'Does the Lausanne Treaty text logically entail the restrictive reading (individual worship only), or does it permit institutional autonomy interpretation?',
    'Comparative philological analysis of Treaty articles; examination of negotiation records; cross-reference with contemporary treaty practice for minority protections. Determine whether ''religious rights'' language was historically understood to include institutional autonomy.',
    'If text permits institutional autonomy: the restrictive reading is a political choice, not legal necessity — shifts the axiom grounding from conventional (agreed meaning) to extraction-driven (beneficiary-selected interpretation). If text logically entails restriction: the reading is textually grounded — changes the classification from snare toward mountain or piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_text_ambiguity, empirical, 'Whether Lausanne text logically supports restrictive or expansive reading').

omega_variable(
    institutional_autonomy_necessity,
    'Can minority religious communities maintain religious practice and transmission without institutional autonomy (property, legal personality, education control)?',
    'Comparative analysis of minority community stability across jurisdictions with/without institutional autonomy; historical tracking of transmission failure in restricted-autonomy systems; sociological evidence on religious continuity mechanisms.',
    'If institutional autonomy is necessary: the individual worship protection is illusory — real protection requires institutional rights. Reclassifies snare boundary as more severe. If alternative transmission mechanisms exist: the gap between formal protection and lived experience is smaller than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_autonomy_necessity, empirical, 'Structural necessity of institutional autonomy for religious practice transmission').

omega_variable(
    state_competing_interest,
    'Does the Turkish state have a legitimate non-extractive interest in subordinating minority institutions to domestic law (e.g., preventing security threats, enforcing labor/safety standards)?',
    'Forensic examination of specific property confiscations, education restrictions, and legal personality denials: do they serve stated security/regulatory goals, or do they systematically target minority capacity? Comparison with application of same laws to majority institutions.',
    'If state interests are legitimate: the snare classification may overstate extraction — some suppression is regulatory necessity. If confiscations/restrictions are selective: confirms extraction mechanism. If inconsistently applied: confirms targeted extraction of minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_competing_interest, empirical, 'Whether state regulatory interests justify institutional subordination').

omega_variable(
    restrictive_reading_kernel_identity,
    'Is the restrictive reading a stable interpretation of the Lausanne kernel, or a strategic deployment of ambiguity to achieve extraction?',
    'Historical analysis of interpretation consistency: does the state apply the restrictive reading uniformly, or does it switch between restrictive and expansive readings depending on political pressure and strategic advantage? Examination of state behavior in comparable treaty obligations (does it apply the same interpretive methodology to majority-benefiting treaties?).',
    'If reading is stable/consistent: it may be a coherent legal position (piton or mountain). If reading is deployed strategically/selectively: confirms that the restriction is driven by extraction logic, not legal principle — snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restrictive_reading_kernel_identity, empirical, 'Stability vs. strategic deployment of the restrictive reading').

omega_variable(
    committer_frame_kernel_codification,
    'How is the Lausanne kernel codified from this reading''s perspective — as fixed historical text, as living document subject to interpretive evolution, or as formal instrument subordinate to state sovereignty?',
    'Analysis of state legal briefs, treaty interpretation statements, and constitutional positioning of Lausanne. Determine which codification framework the restrictive reading relies on.',
    'If codified as fixed text: the reading rests on textual claim (requires philological resolution). If codified as subordinate to state sovereignty: the reading rests on extraction-driven authority grounding. If codified as evolving interpretation: the reading must explain why interpretation evolved toward restriction rather than expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_codification, conceptual, 'Kernel codification status assumed by the restrictive reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_restrict_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(lausanne_restrict_tr_t20, lausanne_minority_protections__restrictive_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(lausanne_restrict_tr_t40, lausanne_minority_protections__restrictive_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(lausanne_restrict_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lausanne_restrict_be_t20, lausanne_minority_protections__restrictive_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(lausanne_restrict_be_t40, lausanne_minority_protections__restrictive_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_restrict_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(lausanne_restrict_su_t20, lausanne_minority_protections__restrictive_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(lausanne_restrict_su_t40, lausanne_minority_protections__restrictive_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, turkish_religious_institution_legal_status).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate_institutional_capacity).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel decomposes into three constraint stories corresponding to three structurally distinct readings. Each reading has its own epsilon value reflecting its empirical plausibility and extraction structure. restrictive_reading (this file): ε=0.62, snare, state-driven institutional foreclosure. expansive_reading: ε=0.25, rope or tangled_rope, institutional autonomy as treaty requirement. guarantor_reading: ε=0.35, rope or scaffold, international enforcement of minority rights with sunset as guarantor states withdraw. The three readings do not coexist in a single Turkish legal framework — the state adopts the restrictive reading as law. But they remain live positions in international discourse and in minority communities' legal claims. The network links show how each reading logically affects the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
