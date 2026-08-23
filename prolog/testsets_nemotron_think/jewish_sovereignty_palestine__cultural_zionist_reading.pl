% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Jewish Cultural Renaissance in Palestine (Cultural Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The cultural zionist reading (exemplified by Ahad Ha'am, Buber, Magnes,
 *   and the Brit Shalom circle) frames Jewish return to Palestine as a
 *   project of cultural renaissance — a 'spiritual center' where Hebrew
 *   language and Jewish creativity can flourish, without requiring political
 *   sovereignty, demographic majority, or displacement of the Arab
 *   population. This reading was marginalized after 1948 when political
 *   zionism achieved statehood, but it persists as a critical reference point
 *   for binationalist, post-zionist, and diasporist Jewish thought. The
 *   constraint story models this reading on its own terms: low
 *   extractiveness, genuine coordination function, Palestinians as
 *   co-inhabitants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.15).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.1).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Jewish Cultural Renaissance in Palestine (Cultural Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, 'ea1fc786-52d2-4098-b113-0f3d6f339ec9').
narrative_ontology:cs_kernel_codification('ea1fc786-52d2-4098-b113-0f3d6f339ec9', distributed).
narrative_ontology:cs_authority_grounding('ea1fc786-52d2-4098-b113-0f3d6f339ec9', practice).
narrative_ontology:cs_interpretation_layer_present('ea1fc786-52d2-4098-b113-0f3d6f339ec9').
narrative_ontology:cs_reading_relation('ea1fc786-52d2-4098-b113-0f3d6f339ec9', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea1fc786-52d2-4098-b113-0f3d6f339ec9', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea1fc786-52d2-4098-b113-0f3d6f339ec9', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('ea1fc786-52d2-4098-b113-0f3d6f339ec9', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('ea1fc786-52d2-4098-b113-0f3d6f339ec9', foundational, jewish_cultural_vitality_sufficient).
narrative_ontology:cs_axiom_status(jewish_cultural_vitality_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('ea1fc786-52d2-4098-b113-0f3d6f339ec9', jewish_cultural_vitality_sufficient, conventional).
narrative_ontology:cs_axiom('ea1fc786-52d2-4098-b113-0f3d6f339ec9', foundational, palestinian_cohabitation_nonnegotiable).
narrative_ontology:cs_axiom_status(palestinian_cohabitation_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('ea1fc786-52d2-4098-b113-0f3d6f339ec9', palestinian_cohabitation_nonnegotiable, deontological).
narrative_ontology:cs_reference_frame('ea1fc786-52d2-4098-b113-0f3d6f339ec9', cultural_sovereignty_framework).
narrative_ontology:cs_drift_state('ea1fc786-52d2-4098-b113-0f3d6f339ec9', post_1948_statehood, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea1fc786-52d2-4098-b113-0f3d6f339ec9', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_practitioners).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_inhabitants).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, international_jewish_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_inhabitants).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, cultural_self_determination_sufficiency).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, shared_homeland_cohabitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writers, educators, artists, and intellectuals building Hebrew-language culture in Palestine — publishing houses, schools, theaters, journals. They benefit from a territorial center where Hebrew functions as a living public language. Exit means returning to diaspora cultural production or shifting to other languages; their professional networks are portable but the Hebrew public sphere is not.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_cultural_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Arab residents of Palestine (Muslim, Christian, Druze) who share the land. In this reading they are co-inhabitants of a shared cultural space — potential participants in a bilingual, binational cultural sphere. They bear costs when cultural institutions acquire land or resources; their exit is constrained by rootedness. The reading asserts mutual benefit but the power asymmetry is real.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_inhabitants, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_inhabitants, payer).

% The Zionist Organization, Jewish Agency, and later Israeli state institutions that set the dominant political agenda. They initially tolerated cultural zionism as a complement but ultimately subordinated it to state-building. They control immigration certificates, land acquisition, and military force. Their exit options are maximal — they can pivot to statehood, partition, or other strategies.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_organizations, agenda_setter,
    institutional, generational, arbitrage, national).

% Diaspora Jewish communities providing philanthropy, immigration, and political support. They gain a living Hebrew cultural center that renews Jewish identity worldwide without requiring aliyah. Their exit is mobile — they can redirect support to other Jewish projects (day schools, camps, Israel bonds, progressive causes).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, international_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% The Mandatory administration (1920–1948) that regulated immigration, land transfer, and public order. They viewed cultural zionism through the lens of imperial administration and Arab opposition. Excluded from the cultural zionist self-understanding but structurally determinative. Their exit was trapped — they could not resolve the mandate's contradictions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, british_mandate_authorities, excluded,
    institutional, immediate, trapped, regional).

% Historians, political theorists, and postcolonial scholars analyzing the cultural zionist project. They neither collect nor pay; they trace how the reading's claims hold up against the historical record of displacement, state formation, and ongoing conflict. Their analytical exit is unconstrained.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinating Jewish cultural production, Hebrew revival, and spiritual renaissance in a territorial center without requiring a Jewish state apparatus — solving the problem of cultural assimilation in diaspora by creating a Hebrew public sphere in Palestine.
% TRANSFER_FUNCTION: Moves cultural capital, linguistic authority, and spiritual leadership from European diaspora centers (Odessa, Warsaw, Vienna, Berlin) to Palestine; resources flow from diaspora philanthropy (Keren Hayesod, private donors) to Hebrew schools, presses, universities, and cultural institutions.
% ABSENT_VOICES: Palestinian nationalist voices who view any Jewish demographic-cultural project as an existential threat to Arab majority status; religious anti-zionist Jewish voices (Satmar, Neturei Karta) who see cultural zionism as heretical rebellion against exile; both are excluded from the cultural zionist framework's internal conversation.
% DISAPPEARANCE_RATIONALE: If the cultural zionist commitment vanished overnight, the network of Hebrew cultural institutions (Hebrew University, Habima Theater, Hebrew press, school system) would lose its organizing rationale — the Hebrew revival might have remained a diaspora project or stalled, and the cultural infrastructure that became the substrate of the Israeli state would not exist.
% FOUNDING_PROBLEM: Jewish cultural assimilation and spiritual decay in the diaspora; the inability to sustain a living Hebrew culture and collective Jewish creativity without a territorial center where Hebrew functions as the language of daily public life.
% FOUNDING_PROBLEM_CORROBORATION: Ahad Ha'am's essays (1890s–1920s) and contemporary cultural zionist intellectuals (e.g., Hannah Arendt's early writings, Martin Buber's binationalism) attest the problem was cultural-spiritual. Palestinian historians (Rashid Khalidi, Walid Khalidi) and post-zionist scholars (Tom Segev, Ilan Pappé) dispute the premise — they argue the 'cultural center' always functioned as a beachhead for political sovereignty and displacement, corroborated by land-purchase records and demographic planning documents from the same period.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the reading explicitly rejects zero-sum sovereignty and displacement; the cultural institutions it coordinates (schools, press, university) are presented as positive-sum. Suppression is low (0.10) in the reading's self-understanding — it relies on persuasion and philanthropy, not coercion. But the measurement series shows a spike in suppression_requirement (0.35–0.40) during 1948–1967, reflecting the historical fact that cultural zionist institutions were absorbed into a state apparatus that did enforce displacement. Theater_ratio rises in that period (0.25–0.30) as cultural rhetoric masks political-military expansion. The end-state values (2024) return to low levels because the reading, as an analytical position, reclaims its non-coercive core.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different types per seat: from the jewish_cultural_practitioner seat, the constraint appears as a rope (genuine coordination, net benefit). From the palestinian_inhabitant seat, the historical trajectory (suppression spike 1948–1967) may compute as tangled_rope or snare — the same institutions that coordinated culture also enabled displacement. From the political_zionist_organization seat, it reads as scaffold (transitional, sunset at statehood). The divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish cultural practitioners and diaspora communities are beneficiaries (d near 0.0) — they gain a cultural center without bearing displacement costs. Palestinian inhabitants are dual-role: beneficiaries of shared cultural space in the reading's ideal (d ~ 0.3) but payers of land/resource costs in practice (d ~ 0.6). Political zionist organizations are agenda_setters with arbitrage exit — they set the terms but can pivot; their effective directionality is complex (derived d ~ 0.25, overridden to 0.45 below). British authorities are excluded (trapped, no directionality). Observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora cultural assimilation) is contested — assimilation continues but Hebrew culture thrives in Israel and persists in diaspora. The arrangement (cultural institutions) persists but its mandate has drifted: it became the substrate of a state the reading did not seek. This is not mandatrophy (function atrophied, form remains) but mandate capture (form repurposed). The reading's low extraction claim holds for its self-declared scope; the historical extraction belongs to the political zionist constraint that subsumed it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_political_separability,
    'Can the cultural zionist project be analytically separated from the political zionist project that subsumed it, or was cultural renaissance always the soft edge of a sovereignty drive?',
    'Historical analysis of Ahad Ha''am''s correspondence with political zionist leaders; land-purchase records of cultural institutions; demographic planning documents of the Jewish Agency''s cultural department.',
    'If inseparable, the reading''s low extractiveness claim collapses — the cultural institutions were always extraction infrastructure. If separable, the reading stands as a genuine low-extraction coordination alternative that was politically defeated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_political_separability, conceptual, 'Whether cultural zionism is a distinct constraint or a facade for political zionism.').

omega_variable(
    shared_space_viability,
    'Is a ''shared cultural space'' with Palestinians viable under conditions of asymmetric power, or does the cultural center''s resource needs (land, water, labor, capital) inevitably produce displacement?',
    'Comparative study of binational cultural institutions (e.g., Haifa''s mixed neighborhoods, Hand in Hand schools) vs. settlement patterns; economic modeling of cultural institution resource footprints.',
    'If viable, the reading''s beneficiary declaration for Palestinians holds. If not, Palestinians are de facto payers regardless of the reading''s intent — the constraint reclassifies toward tangled_rope or snare from their seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shared_space_viability, empirical, 'Whether the cohabitation ideal survives material resource competition.').

omega_variable(
    diaspora_vitality_counterfactual,
    'Would Hebrew culture have revived in diaspora without a territorial center (e.g., via Yiddishland modernization, American Hebrew schools, digital networks)?',
    'Counterfactual historical modeling; comparison with other stateless cultural revivals (Catalan, Basque, Welsh, Māori).',
    'If diaspora revival was plausible, the cultural center''s coordination function is non-unique — the constraint''s necessity claim weakens. If implausible, the coordination function is genuine and the reading''s rope claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_vitality_counterfactual, empirical, 'Whether the coordination problem required this specific territorial solution.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''jewish_sovereignty_palestine'' admit the cultural zionist reading as a legitimate instantiation, or does ''sovereignty'' in the kernel name structurally exclude non-statist readings?',
    'Genealogical analysis of the term ''sovereignty'' in early zionist texts (Herzl, Nordau, Ahad Ha''am, Buber); semantic field mapping of ''ribonut'' / ''melucha'' in Hebrew discourse 1896–1948.',
    'If the kernel''s name smuggles in statism, the cultural zionist reading is a category error — it should be a different kernel (e.g., ''jewish_cultural_autonomy_palestine''). This would affect cross-reading coupling analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel label biases the reading set toward statist framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 1896, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_czr_tr_t1896, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1896, 0.05).
narrative_ontology:measurement(jsp_czr_tr_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(jsp_czr_tr_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jsp_czr_tr_t1967, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(jsp_czr_tr_t1993, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(jsp_czr_tr_t2024, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(jsp_czr_be_t1896, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1896, 0.08).
narrative_ontology:measurement(jsp_czr_be_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1917, 0.12).
narrative_ontology:measurement(jsp_czr_be_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1948, 0.18).
narrative_ontology:measurement(jsp_czr_be_t1967, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(jsp_czr_be_t1993, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1993, 0.15).
narrative_ontology:measurement(jsp_czr_be_t2024, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jsp_czr_su_t1896, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1896, 0.05).
narrative_ontology:measurement(jsp_czr_su_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1917, 0.15).
narrative_ontology:measurement(jsp_czr_su_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1948, 0.35).
narrative_ontology:measurement(jsp_czr_su_t1967, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(jsp_czr_su_t1993, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 1993, 0.2).
narrative_ontology:measurement(jsp_czr_su_t2024, jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__cultural_zionist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_revival).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_institutions).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, binationalist_palestine_proposals).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel 'jewish_sovereignty_palestine' by separating cultural self-determination from political sovereignty. The liberal_nationalist_reading and religious_zionist_reading treat sovereignty as statehood; this reading treats it as cultural autonomy. The settler_colonial_reading and post_zionist_reading are external critiques that engage the kernel's political instantiation. All five stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
