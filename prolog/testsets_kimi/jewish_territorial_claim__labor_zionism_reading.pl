% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__labor_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionist Territorial Claim via Hebrew Labor and Settlement
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   The Labor Zionist reading of the Jewish territorial claim kernel holds
 *   that Jewish national regeneration requires the conquest of labor â the
 *   substitution of Jewish for Arab workers in Palestine through settlement
 *   expansion, institutional economic separation, and the creation of a
 *   closed Hebrew labor market. Between roughly 1904 and 1948, Zionist labor
 *   institutions directed land purchase, immigrant absorption, and employment
 *   policy to build a self-sufficient Jewish economic sector. Palestinian
 *   Arab workers and peasants were structurally excluded from this economy
 *   and displaced from land targeted for settlement. The constraint
 *   coordinates Jewish national reconstruction while simultaneously
 *   transferring land and labor opportunity from the indigenous population.
 *
 * KEY AGENTS:
 *   - zionist_labor_institutions: Agenda-setter (institutional/arbitrage) â administers settlement, land, and labor policy
 *   - jewish_labor_settlers: Beneficiary (moderate/identity_locked) â receives land and employment priority fused with national mission
 *   - palestinian_arab_workers: Payer (powerless/trapped) â excluded from Jewish labor market by institutionalized ethnic boundary
 *   - palestinian_peasantry: Payer (powerless/trapped) â displaced by land acquisition and settlement expansion
 *   - british_mandatory_authority: Observer (institutional/analytical) â colonial overseer enabling the structural framework
 *   - international_labor_movement: Excluded (organized/constrained) â potential class-solidarity critics kept outside discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.72).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.7).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist Territorial Claim via Hebrew Labor and Settlement").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '4acf96f5-7ea0-4f2f-a29d-48d837103079').
narrative_ontology:cs_kernel_codification('4acf96f5-7ea0-4f2f-a29d-48d837103079', distributed).
narrative_ontology:cs_authority_grounding('4acf96f5-7ea0-4f2f-a29d-48d837103079', practice).
narrative_ontology:cs_interpretation_layer_present('4acf96f5-7ea0-4f2f-a29d-48d837103079').
narrative_ontology:cs_reading_relation('4acf96f5-7ea0-4f2f-a29d-48d837103079', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('4acf96f5-7ea0-4f2f-a29d-48d837103079', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('4acf96f5-7ea0-4f2f-a29d-48d837103079', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('4acf96f5-7ea0-4f2f-a29d-48d837103079', foundational, hebrew_labor_exclusionary_principle).
narrative_ontology:cs_axiom_status(hebrew_labor_exclusionary_principle, holdable).
narrative_ontology:cs_axiom_grounding('4acf96f5-7ea0-4f2f-a29d-48d837103079', hebrew_labor_exclusionary_principle, instrumental).
narrative_ontology:cs_axiom('4acf96f5-7ea0-4f2f-a29d-48d837103079', foundational, incremental_fact_based_sovereignty).
narrative_ontology:cs_axiom_status(incremental_fact_based_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4acf96f5-7ea0-4f2f-a29d-48d837103079', incremental_fact_based_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('4acf96f5-7ea0-4f2f-a29d-48d837103079', hebrew_labor_settlement_praxis).
narrative_ontology:cs_drift_state('4acf96f5-7ea0-4f2f-a29d-48d837103079', late_mandate_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4acf96f5-7ea0-4f2f-a29d-48d837103079', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_labor_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_labor_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_workers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers settlement programs, land purchase funds, and labor market policies across Palestine. Enforces the Hebrew labor principle through hiring discrimination in Jewish enterprises, separate wage structures, and allocation of institutional resources exclusively to Jewish workers. Justifies the arrangement as socialist nation-building and the material foundation for Jewish national regeneration.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_labor_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive allocated agricultural land, employment priority in the Jewish economic sector, housing, and social services through Zionist labor institutions. Their livelihood and self-concept are fused with the settlement project; leaving means abandoning both material support and the national mission of conquering labor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_labor_settlers, beneficiary,
    moderate, biographical, identity_locked, regional).

% Excluded from employment in Jewish settlements and enterprises by institutionalized Hebrew labor policies. Face a contracting agricultural labor market due to Zionist land purchases and competitive displacement. Their labor is structurally devalued in the emerging Jewish economy and they are barred from entering it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_workers, payer,
    powerless, immediate, trapped, local).

% Lose land access through Zionist settlement expansion, land transfers to the Jewish National Fund, and demographic marginalization. Subject to displacement as Jewish settlement builds demographic and territorial facts on the ground. Their historical agrarian existence is undermined by the same institutional mechanisms that create the closed Jewish labor economy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_peasantry, payer,
    powerless, immediate, trapped, local).

% Oversees Palestine under the League of Nations mandate incorporating the Jewish national home provision. Intermittently enforces or tolerates Zionist labor market segmentation and land settlement policies while nominally protecting existing non-Jewish communities. Does not directly collect the constraint's gains but enables the structural framework through legal and military presence.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandatory_authority, observer,
    institutional, biographical, analytical, national).

% Socialist and labor internationalists who might contest the ethnic segmentation of labor markets on class-solidarity grounds. Their exclusion from Zionist labor policy discourse allows socialist transformation rhetoric to proceed without substantive class-based solidarity challenges to the Hebrew labor boundary.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, international_labor_movement, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Jewish national regeneration through collective labor and socialist transformation; building a self-sufficient Jewish economy in Palestine capable of sustaining Jewish demographic growth and eventual sovereignty without dependence on Arab labor or external charity.
% TRANSFER_FUNCTION: Land and employment opportunity from Palestinian Arab peasants and workers to Jewish labor settlers and Zionist economic institutions; the transfer is mediated through Zionist land funds, settlement expansion, and enforced labor market segmentation under Hebrew labor doctrine.
% ABSENT_VOICES: Palestinian Arab workers and peasants are structurally excluded from Zionist labor policy discourse; international socialist movements that might challenge ethnic labor segmentation on class-solidarity grounds are marginalized; anti-Zionist Jewish voices are silenced within the Zionist institutional framework.
% DISAPPEARANCE_RATIONALE: If the Hebrew labor mechanism and settlement infrastructure vanished overnight, the Jewish economic sector in Palestine would collapse as a distinct entity, land control would revert to prior arrangements, and the demographic-political facts on the ground underwriting the territorial claim would dissolve.
% FOUNDING_PROBLEM: Jewish economic and social vulnerability in the Diaspora; the need for a productive Jewish working class engaged in agriculture and manual labor rather than urban middle-class professions; antisemitic exclusion from European labor markets and periodic pogroms.
% FOUNDING_PROBLEM_CORROBORATION: Zionist labor historians and institutions attest the problem was live and required territorial solution. Palestinian historians and Marxist critics attest the solution created a new problem of colonial displacement. British Mandatory reports and League of Nations commissions documented both Jewish refugee needs and Arab agrarian disruption, providing external corroboration of the contested dual reading.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__labor_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__labor_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint systematically transfers land and labor opportunity from Palestinian Arabs to Jewish settlers through enforceable ethnic boundaries in land and employment markets. Suppression (0.70) reflects the active institutional machinery required to maintain Hebrew labor â hiring discrimination, separate wage structures, land transfer restrictions, and physical settlement expansion. Theater ratio (0.45) captures the growing gap between socialist rhetoric and exclusionary practice. Accessibility collapse (0.60) indicates that Arab-Palestinian alternatives to the Jewish economy were progressively foreclosed but never fully eliminated. Resistance (0.60) registers sustained Palestinian opposition and labor market competition. The measurement series trace intensification from early Second Aliyah idealism through Mandate-era institutionalization to the peak of partition-war separation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination for national survival and socialist transformation; the beneficiary seat experiences it as identity-fused material opportunity; the payer seats experience identical structural measures as dispossession and enforced exclusion. The engine computes this divergence from beneficiary declarations, exit options, and power without requiring the seats to agree on a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist labor institutions and Jewish settlers are declared beneficiaries: they receive the coordinated allocation of land and labor (low d, damped effective extraction). Palestinian Arab workers and peasants are declared victims: they bear the cost of exclusion and displacement (high d, amplified effective extraction). British authorities sit near symmetric as enablers without being primary targets or collectors. International labor critics are excluded, bearing no direct structural load.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false binary of labeling Labor Zionism either as pure national liberation (rope) or pure settler colonialism (snare). The constraint solves a genuine coordination problem â Jewish refugee resettlement, diasporic economic vulnerability, and the creation of a Jewish working class. However, it solves that problem through the same structural mechanism that asymmetrically extracts from Palestinian Arabs. The mandatrophy risk would be declaring it a scaffold (it has no sunset clause) or a rope (ignoring the victims). The extraction is not incidental to the coordination; it is the method of the coordination â demographic and economic separation. The presence of both beneficiaries and victims, plus active enforcement, forces the hybrid classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_liberation_vs_settler_colonialism,
    'Is the Labor Zionist constraint a legitimate national liberation project solving genuine Jewish dispossession, or a settler-colonial mechanism extracting land and labor from an indigenous population?',
    'Comparative historical analysis against other settler-colonial and national liberation movements; examination of whether the constraint''s persistence requires indigenous displacement or could operate without it.',
    'If resolved as settler-colonialism, classification shifts toward snare and the coordination function is delegitimized; if resolved as national liberation, the extraction metric may reflect necessary defensive coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_liberation_vs_settler_colonialism, conceptual, 'Fundamental framing ambiguity of the Labor Zionist project').

omega_variable(
    hebrew_labor_economic_viability,
    'Could the Jewish economy in Palestine have developed without the exclusion of Arab labor and the active suppression of Palestinian agrarian land use?',
    'Counterfactual economic history and analysis of Zionist land and labor market data from the Mandate period.',
    'If viable without exclusion, the Hebrew labor principle is revealed as extractive surplus rather than coordination necessity; if non-viable, part of the extraction metric represents genuine structural coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hebrew_labor_economic_viability, empirical, 'Whether economic separation was structurally necessary or extractive').

omega_variable(
    british_mandatory_role_ambiguity,
    'To what extent did British Mandatory policy independently enforce the constraint versus merely tolerate Zionist labor market segmentation?',
    'Archival analysis of British land and labor policy enforcement decisions during the Mandate.',
    'If British enforcement was essential, the constraint''s suppressive capacity was partly exogenous; if British tolerance was sufficient, the constraint was endogenously enforced by Zionist institutions alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_mandatory_role_ambiguity, empirical, 'Exogenous versus endogenous enforcement of the labor market boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t8, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(jewi_tr_t16, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(jewi_tr_t24, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(jewi_tr_t32, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(jewi_tr_t44, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 44, 0.45).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(jewi_be_t8, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(jewi_be_t16, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(jewi_be_t24, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(jewi_be_t32, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(jewi_be_t44, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 44, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jewi_su_t8, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(jewi_su_t16, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(jewi_su_t24, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(jewi_su_t32, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(jewi_su_t44, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 44, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_territorial_claim kernel. The labor_zionism_reading instantiates the territorial claim through socialist praxis and Hebrew labor exclusion. Sibling readings instantiate the same kernel through political-statehood, cultural-spiritual, and maximalist-military framings. Decomposition follows the epsilon-invariance principle: each reading has distinct beneficiaries, victims, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
