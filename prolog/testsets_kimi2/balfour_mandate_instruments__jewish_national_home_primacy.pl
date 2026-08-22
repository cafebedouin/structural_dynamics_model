% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: British Mandate: Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint is the 'jewish_national_home_primacy' reading of the
 *   balfour_mandate_instruments kernel â the legal instruments establishing
 *   British mandatory rule over Palestine (Balfour Declaration 1917, San Remo
 *   1920, League Mandate 1922). Under this reading, 'national home' is
 *   interpreted as a proto-state entitlement requiring systematic demographic
 *   transformation, land access facilitation, and Jewish institutional
 *   supremacy. The constraint operated as a colonial legal framework actively
 *   enforced by the British mandatory administration, yielding high
 *   extractiveness for Palestinian Arab landholders and political leadership
 *   while coordinating Zionist state-building. The engine measures the
 *   divergence between this reading's structural premises and the metrics of
 *   its operation; the claim of tangled rope is authored independently from
 *   the extraction scores.
 *
 * KEY AGENTS:
 *   - Zionist institutions: Primary beneficiary (institutional/mobile) â captured quasi-governmental status and land acquisition capacity.
 *   - Jewish migrants: Secondary beneficiary (moderate/mobile) â received facilitated immigration and settlement support.
 *   - Palestinian Arab landholders: Primary target (moderate/constrained) â bore territorial dispossession through facilitated land transfers.
 *   - Palestinian Arab political leadership: Primary target (organized/constrained) â bore political exclusion and representation collapse.
 *   - British mandatory administration: Agenda-setter (institutional/arbitrage) â enforced the primacy interpretation and colonial order.
 *   - League of Nations: Analytical observer (institutional/analytical) â provided legitimating frame without operational oversight.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.82).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.79).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.82).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "British Mandate: Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '1e55d55b-b1a4-45c3-8510-5aa26bb26e71').
narrative_ontology:cs_kernel_codification('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', fixed_text).
narrative_ontology:cs_authority_grounding('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', lineage).
narrative_ontology:cs_interpretation_layer_present('1e55d55b-b1a4-45c3-8510-5aa26bb26e71').
narrative_ontology:cs_reading_relation('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', foundational, national_home_as_proto_state_entitlement).
narrative_ontology:cs_axiom_status(national_home_as_proto_state_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', national_home_as_proto_state_entitlement, conventional).
narrative_ontology:cs_axiom('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', foundational, jewish_institutional_supremacy_as_mandate_telos).
narrative_ontology:cs_axiom_status(jewish_institutional_supremacy_as_mandate_telos, holdable).
narrative_ontology:cs_axiom_grounding('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', jewish_institutional_supremacy_as_mandate_telos, conventional).
narrative_ontology:cs_reference_frame('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', balfour_mandate_trusteeship_frame).
narrative_ontology:cs_drift_state('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', post_1936_arab_revolt, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e55d55b-b1a4-45c3-8510-5aa26bb26e71', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, national_home_as_proto_state_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained quasi-governmental status under Mandate Article 4, operating immigration bureaus, land purchasing agencies, and proto-state institutions. Received preferential access to state-building resources, legal recognition, and diplomatic support from the mandatory power.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    institutional, generational, mobile, national).

% Received facilitated immigration certificates, land settlement support, housing, and infrastructure under the mandate's national home policy, entering a framework that reserved institutional capacity and legal advantages for their community.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, mobile, national).

% Lost land through sales and transfers systematically facilitated by mandatory land ordinances and registry changes. Faced legal structures that disadvantaged Arab tenure, limited access to credit, and pressure to sell to Zionist institutions operating with state-like support.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    moderate, generational, constrained, national).

% Denied proportional legislative institutions and elected representative councils. Petitions to the mandatory administration and the League of Nations were overruled or ignored, and proposals for an Arab legislative assembly were rejected. Political development was structurally subordinated to Jewish national home construction.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    organized, generational, constrained, national).

% Administered the mandate territory, set immigration quotas favoring Jewish entry, enforced land transfer regulations, and interpreted the national home clause as requiring Jewish institutional supremacy while maintaining colonial public order and strategic British interests.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Formally supervised the mandate through the Permanent Mandates Commission but exercised limited effective oversight over British interpretive discretion. Served as the legitimating international frame without operational enforcement of alternative readings of the mandate text.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__jewish_national_home_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish a British-administered colonial territory in Palestine after Ottoman collapse, facilitating Jewish immigration and land settlement to construct the infrastructure of a Jewish national home while maintaining public order and basic governance.
% TRANSFER_FUNCTION: Transfers land, demographic majority, and political institutional capacity from Palestinian Arab landholders and political communities to Zionist institutions and Jewish migrants through preferential immigration quotas, facilitated land sales, and structurally downgraded Arab representation.
% ABSENT_VOICES: Palestinian Arab peasantry and urban workers were excluded from mandatory commissions and League petitions; anti-Zionist British mandatory officials were sidelined in policy formation; Arab legislative council proposals were repeatedly rejected.
% DISAPPEARANCE_RATIONALE: Without the mandate instruments interpreted as Jewish national home primacy, Zionist immigration facilitation and land acquisition would lose their legal-administrative backbone, Zionist institutional growth would stall, and Palestinian Arab political and territorial structures would reassert; the demographic and state-formation trajectory would reverse.
% FOUNDING_PROBLEM: The collapse of Ottoman administration in Palestine after World War I created a governance vacuum; the Balfour Declaration and San Remo conference sought to establish stable British-administered rule while satisfying Zionist aspirations for a national home and Allied strategic interests in the region.
% FOUNDING_PROBLEM_CORROBORATION: British Colonial Office assessments by the mid-1920s attested that basic governance had been restored. Palestinian Arab delegations to the League of Nations and testimony to the Peel Commission attested the mandate had shifted from trusteeship to facilitating Jewish statehood. Zionist leaders argued the founding problem remained live until sovereign statehood was achieved. Corroboration from outside the benefiting parties: League Permanent Mandates Commission critiques and British parliamentary opposition questioned by the 1930s whether the mandate still served its original trusteeship function.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.82 because the mandate systematically transferred land and political capacity to Zionist institutions while subordinating Arab rights. Suppression is 0.79 because the constraint required active British enforcement of immigration quotas, land ordinances, and exclusion of Arab political institutions. Theater ratio is 0.45: the colonial administration performed genuine governance functions (infrastructure, courts, order) but an increasing share of its activity served to maintain the trusteeship facade while pursuing demographic transformation. Accessibility collapse is 0.72 because alternatives (Arab self-determination, independent land registries, proportional legislatures) were structurally foreclosed once the mandate was established. Resistance is 0.68 due to sustained Palestinian Arab opposition (1920 riots, 1929 revolt, 1936-39 Great Revolt, petitions to Geneva). Measurements trace rising extraction and theater from 1920 to 1948 on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Zionist institutions, Jewish migrants) experience the mandate as enabling coordination â legal protection, immigration facilitation, and institutional development. The payer seats (Palestinian Arab landholders and political leadership) experience the identical framework as extractive dispossession and political subordination. The British mandatory seat experiences it as a manageable colonial obligation with strategic benefits. The League observer seat experiences it as a legitimating text with weak enforcement. These divergences are structurally derived from the beneficiary/victim declarations and exit options; the engine computes per-seat classification from this surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants are declared beneficiaries, feeding low directionality and damped effective extraction. Palestinian Arab landholders and political leadership are declared victims (payers), feeding high directionality and amplified effective extraction. The British administration is the agenda_setter with arbitrage-grade exit (could modify policy), placing it near the beneficiary end despite its enforcement role. The League of Nations is an analytical observer with no stake. Directionality is structurally derived from these declarations and the exit modulation; no override is required.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling in both directions. Against the snare reading: it is not pure extraction because the British administration did provide genuine coordination (public order, infrastructure, a legal system). Against the rope reading: the coordination was asymmetric â one party was coordinated into statehood and the other was coordinated out of land and political capacity, requiring active enforcement to hold the asymmetry. The founding problem (post-Ottoman governance vacuum) was dead by the mid-1920s, yet the arrangement persisted to serve state-formation rather than trusteeship, indicating mandatrophy without collapsing into pure piton because the beneficiaries remained concentrated and active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_transformation_as_coordination_or_extraction,
    'Is the facilitation of Jewish demographic transformation under the mandate a genuine coordination function (building a national home infrastructure) or an extractive transfer of territory and political capacity from the existing population?',
    'Comparative historical analysis of alternative trusteeship models that protected indigenous majority rule; assessment of whether comparable infrastructure could have been built without territorial dispossession.',
    'If purely extractive, classification shifts toward snare; if inseparable from a real coordination function, remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_transformation_as_coordination_or_extraction, conceptual, 'Coordination-extraction boundary for demographic transformation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression maintained by British colonial military and legal force alone, or also by the international legal framing that internalized Palestinian Arab subordination within a trusteeship discourse?',
    'Archival analysis of Palestinian petitioning behavior and British legal argumentation; post-exit trajectory of Palestinian political claims after mandate termination.',
    'If internalized, effective suppression exceeds structural measure and resistance metrics underestimate the constraint''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_operative_validity,
    'Which reading of the balfour_mandate_instruments kernel captures the structurally operative constraint: Jewish national home primacy, dual obligation to indigenous rights, or mandatory interpretive discretion?',
    'Comparative historical analysis of mandatory policy outputs (immigration quotas, land transfer rates, political representation structures) against the predictions of each reading.',
    'If dual obligation or discretion is the better predictor, this constraint''s authored epsilon is misattributed and the operative constraint lies in a different reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_operative_validity, conceptual, 'Kernel reading structural validity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_jnhp_tr_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(balfour_jnhp_tr_t5, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 5, 0.35).
narrative_ontology:measurement(balfour_jnhp_tr_t10, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 10, 0.4).
narrative_ontology:measurement(balfour_jnhp_tr_t15, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 15, 0.5).
narrative_ontology:measurement(balfour_jnhp_tr_t20, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 20, 0.6).
narrative_ontology:measurement(balfour_jnhp_tr_t25, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 25, 0.65).
narrative_ontology:measurement(balfour_jnhp_tr_t28, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 28, 0.7).

% Extraction over time
narrative_ontology:measurement(balfour_jnhp_be_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(balfour_jnhp_be_t5, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(balfour_jnhp_be_t10, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(balfour_jnhp_be_t15, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(balfour_jnhp_be_t20, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(balfour_jnhp_be_t25, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(balfour_jnhp_be_t28, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 28, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(balfour_jnhp_su_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(balfour_jnhp_su_t5, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(balfour_jnhp_su_t10, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(balfour_jnhp_su_t15, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(balfour_jnhp_su_t20, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(balfour_jnhp_su_t25, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(balfour_jnhp_su_t28, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 28, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the balfour_mandate_instruments kernel. The natural-language label 'British Mandate for Palestine' conflates three structurally distinct constraints: a primacy reading (high extraction, Jewish state-building), a dual-obligation reading (indigenous rights protection), and a discretion reading (British interpretive authority as the operative system). They are modeled as separate stories linked by the kernel and by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
