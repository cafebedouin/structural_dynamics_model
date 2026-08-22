% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Territorial Claim: Jewish Spiritual and Cultural Center in Palestine
 *   domain: political/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates the cultural_zionism_reading of the
 *   jewish_territorial_claim kernel. Rooted in Ahad Ha'am's thought, it
 *   proposes a Jewish spiritual and cultural center in Palestine as the
 *   solution to diaspora Jewish cultural degeneration, explicitly rejecting
 *   the necessity of political sovereignty or a Jewish demographic majority.
 *   The constraint arranges selective Jewish settlement, land purchase, and
 *   Hebrew cultural institution-building in Palestine under British Mandate
 *   protection while disclaiming the sovereign and demographic demands of
 *   Political and Revisionist Zionism. It presents itself as coordination
 *   (cultural renewal, spiritual center) but operates through territorial
 *   concentration that extracts land and institutional space from Palestinian
 *   Arab society.
 *
 * KEY AGENTS:
 *   - Cultural Zionist leadership (agenda_setter, organized/mobile): promotes quality-over-quantity settlement and Hebrew institutions
 *   - Jewish diaspora communities (beneficiary, moderate/mobile): receive cultural-spiritual capital without bearing territorial costs
 *   - Palestinian Arab peasantry (primary payer, powerless/trapped): lose land access and face demographic pressure from institutional settlement
 *   - Palestinian Arab elite (secondary payer, organized/constrained): oppose the national threat and loss of self-determination
 *   - British Mandate authority (observer, institutional/arbitrage): facilitates and regulates the legal framework
 *   - Anti-Zionist Jewish intelligentsia (excluded, moderate/mobile): contest the founding premise but are kept out of the Zionist framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.52).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.58).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Territorial Claim: Jewish Spiritual and Cultural Center in Palestine").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85').
narrative_ontology:cs_kernel_codification('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', fixed_text).
narrative_ontology:cs_authority_grounding('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', lineage).
narrative_ontology:cs_interpretation_layer_present('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85').
narrative_ontology:cs_reading_relation('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', foundational, jewish_spiritual_center_sufficient).
narrative_ontology:cs_axiom_status(jewish_spiritual_center_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', jewish_spiritual_center_sufficient, deontological).
narrative_ontology:cs_axiom('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', foundational, arab_presence_compatible).
narrative_ontology:cs_axiom_status(arab_presence_compatible, holdable).
narrative_ontology:cs_axiom_grounding('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', arab_presence_compatible, deontological).
narrative_ontology:cs_reference_frame('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', hebrew_cultural_renaissance_framework).
narrative_ontology:cs_drift_state('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', zionist_statehood_consensus, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a9ade6a6-36f2-4d55-bdfa-1b7a64b29f85', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_peasantry).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Intellectuals and institution-builders centered on Ahad Ha'am's circle who promote selective Jewish settlement in Palestine focused on Hebrew cultural renewal, educational institutions, and spiritual regeneration rather than mass colonization or statehood. They set the agenda for quality-over-quantity settlement and directly administer cultural institutions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, cultural_zionist_leadership, agenda_setter,
    organized, generational, mobile, national).

% Jewish communities in Europe and elsewhere who fund and culturally benefit from a renewed Hebrew center in Palestine, gaining spiritual identity reinforcement and national cultural pride without necessarily emigrating or bearing the direct costs of territorial contestation.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_communities, beneficiary,
    moderate, biographical, mobile, global).

% Palestinian Arab peasants and villagers who lose land access through sales to Jewish settlement institutions, face rising land prices and landlessness, and experience demographic and institutional pressure from selective but permanent Jewish settlement focused on cultural infrastructure.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_peasantry, payer,
    powerless, immediate, trapped, local).

% Palestinian Arab nationalist leaders, notables, and emerging political organizations who oppose Zionist immigration and land purchase as threats to Arab majority, self-determination, and sovereignty, even under the cultural-center framing that explicitly disclaims political statehood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_elite, payer,
    organized, generational, constrained, national).

% British colonial administration operating under the League of Nations Mandate for Palestine, which facilitates Jewish immigration and land transfer through legal and administrative frameworks while attempting to balance Arab interests and maintain imperial order.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, british_mandate_authority, observer,
    institutional, biographical, arbitrage, global).

% Jewish intellectuals, Bundists, assimilationists, and Orthodox opponents of secular nationalism who contest the premise that Palestine is necessary for Jewish cultural survival or that a territorial claim is justified, but are structurally excluded from the Zionist institutional and congressional framework.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, anti_zionist_jewish_intelligentsia, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial locus for Jewish national cultural renewal and Hebrew linguistic-spiritual renaissance, solving the perceived problem of diaspora cultural degeneration by concentrating cultural institutions, schools, and settlement in Palestine.
% TRANSFER_FUNCTION: Moves land, institutional space, and diaspora philanthropic capital from Palestinian Arab society and global Jewish donors into Hebrew cultural institutions and selective settlement in Palestine.
% ABSENT_VOICES: Anti-Zionist Jewish intellectuals who see robust cultural life possible without Palestine; Palestinian Arab peasants often excluded from land negotiations where absentee landlords sell to Zionist institutions; binationalist advocates who would demand full parity rather than cultural-center dominance.
% DISAPPEARANCE_RATIONALE: If the cultural Zionist claim and its institutional apparatus vanished, the Hebrew cultural institutions in Palestine would not exist, diaspora Jewish cultural renewal would have followed different paths (likely Yiddishist or assimilationist), and Palestinian Arab landholding and political development would have proceeded under different demographic and institutional pressures.
% FOUNDING_PROBLEM: Jewish cultural and spiritual degeneration in the diaspora; the perceived emptiness of assimilation and the need for a Hebrew national spiritual center to regenerate Jewish culture and identity.
% FOUNDING_PROBLEM_CORROBORATION: Jewish ethnographers and historians documented Eastern European Jewish communal conditions, but the specific diagnostic framing of 'degeneration requiring territorial concentration in Palestine' is contested by assimilationist, Bundist, and Yiddishist intellectuals outside the Zionist beneficiary framework. British and Ottoman observers noted Jewish communal conditions without adopting the Zionist therapeutic framing. No fully external corroboration of the problem-as-diagnosed exists.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: lower than statehood-oriented Zionisms because this reading explicitly forgoes sovereignty and majority-seeking, but substantial because land purchase and institutional building still concentrate Jewish territorial presence at Palestinian expense. Suppression (0.58) reflects the need for British legal enforcement of Jewish land transfer and immigration rights against Arab resistance; it is moderate because Cultural Zionism does not deploy the maximalist military coercion of Revisionism, but still requires active legal-administrative enforcement. Theater ratio (0.33) captures the growing gap between cultural-spiritual rhetoric and the emerging political reality of Jewish institutional dominance under the Mandate. Accessibility collapse (0.42) is partial: Palestinian alternatives to Zionist land purchase existed early but narrowed as the Mandate legal framework and Jewish institutional density solidified. Resistance (0.48) is moderate, reflecting both Arab nationalist opposition and internal Jewish debate about the sufficiency of cultural Zionism relative to political solutions.
 *
 * PERSPECTIVAL GAP:
 *   From the Cultural Zionist leadership seat, the constraint appears as genuine coordination solving a real cultural crisis in the diaspora; from the Palestinian Arab seats, the same arrangement reads as settler-colonial extraction using cultural rhetoric as its legitimizing cover. The engine computes this divergence from the structural data: the same land purchase mechanisms register as cultural institution-building to the beneficiary and as dispossession to the payer. The Jewish diaspora seat experiences a rope-like structure (genuine benefit, low cost, mobile exit), while the Palestinian peasant seat experiences snare-like extraction (trapped exit, high cost, powerless).
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities are structural beneficiaries (low d): they receive cultural-spiritual renewal and national identity reinforcement without directly paying the territorial costs. Palestinian Arab peasantry and elite are structural targets (high d): they pay through land loss, demographic anxiety, and political subordination. The Cultural Zionist leadership sits as agenda-setter with mixed d: they both coordinate the project and benefit from its institutional prestige, but also invest heavily in its maintenance. The British Mandate authority holds analytical d: they regulate and observe without being primary beneficiaries or targets. Anti-Zionist Jewish intelligentsia are excluded from the framework entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by acknowledging the genuine coordination function â Hebrew cultural renaissance and diaspora spiritual renewal were real achievements â while refusing to let that function mask the territorial extraction required to achieve it. A pure rope reading would ignore Palestinian land loss and institutional displacement; a pure snare reading would ignore the authentic cultural and linguistic transformation the project accomplished. Tangled rope captures both: the coordination is real, but the same structure that coordinates Jewish cultural renewal asymmetrically extracts from Palestinian Arabs through land concentration and demographic pressure. The Mandate-era drift toward statehood (rising theater ratio in measurements) suggests the coordination function was increasingly captured by political extraction, but the base reading remains structurally distinct from its sovereignty-seeking siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How does the cultural zionism reading''s rejection of sovereignty requirements change the structural extraction profile relative to sibling readings that mandate statehood?',
    'Comparative analysis of land acquisition rates, demographic displacement, and institutional dominance across the four kernel readings in the constraint family.',
    'If sovereignty-agnostic framing significantly reduces measurable extraction, this reading may compute as a distinct constraint type from its maximalist siblings; if land claims produce similar dispossession regardless of sovereignty intent, the extraction is driven by the territorial kernel itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural delta between cultural and political zionist readings').

omega_variable(
    cultural_need_vs_territorial_claim,
    'Is the Jewish need for cultural renewal a genuine coordination problem that justifies territorial concentration, or does the territorial claim function as extraction regardless of cultural framing?',
    'Historical counterfactual analysis examining whether comparable Jewish cultural renaissance occurred without territorial concentration (Yiddishist centers in Eastern Europe, American Jewish cultural development in the early 20th century).',
    'If robust cultural renewal occurred without Palestine-specific territorial claims, the coordination function is separable from the territorial extraction; if Palestine was uniquely necessary, the coordination and extraction are structurally fused.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_need_vs_territorial_claim, empirical, 'Whether cultural renewal required territorial claim').

omega_variable(
    binational_viability,
    'Did the cultural zionism reading''s stated compatibility with Arab presence and binational frameworks actually prevent asymmetric extraction, or merely delay and mask its manifestation?',
    'Analysis of institutional power-sharing proposals, actual land-use patterns, and labor market segmentation under the cultural zionist settlement model during the Mandate period.',
    'If binational compatibility was structurally viable and practiced, extraction may have been genuinely lower than statehood-oriented alternatives; if institutional dominance and labor segmentation emerged regardless of intent, the constraint type converges with extraction-heavy siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_viability, empirical, 'Whether binational framing reduced actual extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jewi_tr_t30, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(jewi_tr_t50, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 50, 0.33).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(jewi_be_t30, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(jewi_be_t50, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(jewi_su_t30, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(jewi_su_t50, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_territorial_claim kernel, decomposed per the Îµ-invariance principle. The kernel 'Jewish territorial claim' conflates four structurally distinct claims: cultural center without sovereignty (this file), sovereign statehood (political_zionism_reading), socialist settlement and labor conquest (labor_zionism_reading), and maximalist territorial sovereignty (revisionist_zionism_reading). Each has distinct Îµ values, beneficiary structures, and enforcement mechanisms. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
