% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments â Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The League of Nations Mandate for Palestine (1922) and the Balfour
 *   Declaration (1917) constitute a commitment-system kernel authorizing
 *   British administration to facilitate a 'Jewish national home.' This
 *   constraint story instantiates the reading that interprets 'national home'
 *   as proto-state sovereignty requiring active demographic and territorial
 *   transformation. Under this reading, the mandate instruments operate as a
 *   colonial-legal framework that coordinates international legitimacy and
 *   administrative capacity for Jewish settlement while extracting land and
 *   political authority from the indigenous Palestinian Arab population.
 *   Jewish Agency gains quasi-governmental status (Article 4), immigration
 *   quotas are skewed toward Jewish entry, land sales from Arabs to Jews are
 *   systematically facilitated, and Arab political representation is
 *   structurally downgraded. The constraint is claimed as tangled_rope: it
 *   contains a genuine coordination function (international legal framework
 *   for Jewish refugee settlement and state-building) fused with asymmetric
 *   extraction (Palestinian dispossession and political marginalization). The
 *   metrics are authored independently: high extractiveness, high
 *   suppression, and rising theater ratio document the constraint's actual
 *   operation, regardless of the coordinating claims embedded in the mandate
 *   text.
 *
 * KEY AGENTS:
 *   - british_mandatory_power: Agenda-setter (institutional/arbitrage) â colonial administrator interpreting and enforcing the mandate
 *   - zionist_institutions: Primary beneficiary/agenda-setter secondary (organized/constrained) â quasi-governmental Jewish Agency and settlement institutions
 *   - jewish_migrants: Beneficiary (moderate/constrained) â facilitated immigrants receiving land and institutional access
 *   - palestinian_arab_landholders: Primary target/payer (moderate/trapped) â indigenous landholders facing facilitated transfer
 *   - palestinian_political_leadership: Target/payer (organized/constrained) â leadership structurally excluded from self-determination
 *   - anti_zionist_jewish_dissidents: Excluded voice (moderate/constrained) â internal Jewish opposition marginalized
 *   - league_permanent_mandates_commission: Observer (institutional/analytical) â monitoring body without enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.82).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.85).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.82).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments â Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '6bf40b49-2b8c-451f-a863-57997daf94f7').
narrative_ontology:cs_kernel_codification('6bf40b49-2b8c-451f-a863-57997daf94f7', fixed_text).
narrative_ontology:cs_authority_grounding('6bf40b49-2b8c-451f-a863-57997daf94f7', lineage).
narrative_ontology:cs_interpretation_layer_present('6bf40b49-2b8c-451f-a863-57997daf94f7').
narrative_ontology:cs_reading_relation('6bf40b49-2b8c-451f-a863-57997daf94f7', balfour_mandate_instruments__dual_obligation_indigenous_rights, forecloses).
narrative_ontology:cs_reading_relation('6bf40b49-2b8c-451f-a863-57997daf94f7', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('6bf40b49-2b8c-451f-a863-57997daf94f7', foundational, jewish_national_home_as_protosovereign_entitlement).
narrative_ontology:cs_axiom_status(jewish_national_home_as_protosovereign_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('6bf40b49-2b8c-451f-a863-57997daf94f7', jewish_national_home_as_protosovereign_entitlement, conventional).
narrative_ontology:cs_axiom('6bf40b49-2b8c-451f-a863-57997daf94f7', foundational, demographic_transformation_as_mandatory_imperative).
narrative_ontology:cs_axiom_status(demographic_transformation_as_mandatory_imperative, holdable).
narrative_ontology:cs_axiom_grounding('6bf40b49-2b8c-451f-a863-57997daf94f7', demographic_transformation_as_mandatory_imperative, conventional).
narrative_ontology:cs_reference_frame('6bf40b49-2b8c-451f-a863-57997daf94f7', jewish_national_home_as_protosovereignty).
narrative_ontology:cs_drift_state('6bf40b49-2b8c-451f-a863-57997daf94f7', late_mandatory_period_1939_1948, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6bf40b49-2b8c-451f-a863-57997daf94f7', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_political_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Palestine under League of Nations Mandate authority. Interprets 'national home' as requiring active facilitation of Jewish immigration, land acquisition, and institutional development. Enforces the legal and military framework that maintains this interpretation against indigenous resistance.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_power, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains quasi-governmental recognition under Mandate Article 4 to advise and cooperate with the Administration. Controls immigration facilitation, land acquisition trusts, and parallel governance institutions. Benefits from legal privileging of Jewish land purchase and institutional supremacy over Arab political bodies.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter).

% Receive facilitated immigration quotas, preferential land settlement access, and institutional support under the mandate framework. Their demographic presence is the intended mechanism for establishing Jewish sovereignty.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, constrained, national).

% Face systematic state facilitation of land transfers to Jewish purchasers through land courts, state domain declarations, and preferential lease terms. Their tenure security erodes under a legal regime that treats Jewish settlement as a mandatory objective.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    moderate, generational, trapped, regional).

% Demands for self-determination and representative legislative council are structurally downgraded. Arab Executive Committee petitions are overridden by Zionist institutional priorities. Political representation is subordinated to the national home imperative.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_political_leadership, payer,
    organized, generational, constrained, national).

% Jewish voices opposing the national home framework or demanding binational equality are marginalized within Zionist institutions and lack recognition by the mandatory administration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, anti_zionist_jewish_dissidents, excluded,
    moderate, biographical, constrained, national).

% Monitors mandate compliance from Geneva, receives petitions from Palestinian Arabs, issues critical reports on land and immigration policy, but lacks enforcement capacity to alter British administrative practice.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international legal legitimacy and administrative infrastructure for Jewish refugee settlement, land development, and parallel state-building in Palestine under British colonial administration.
% TRANSFER_FUNCTION: Moves land, political authority, and demographic capacity from Palestinian Arab population to Zionist institutions and Jewish migrants, under the legal and administrative framework of the British Mandate.
% ABSENT_VOICES: Palestinian Arab peasantry and tenant farmers excluded from land court processes; anti-Zionist Jewish minorities marginalized within the Yishuv; Permanent Mandates Commission critics of demographic imbalance whose recommendations were non-binding.
% DISAPPEARANCE_RATIONALE: If the mandate instruments and their Jewish-national-home-primacy interpretation vanished overnight, Jewish immigration and land acquisition would lose their legal-administrative infrastructure, Zionist quasi-governmental institutions would lack authority, and Palestinian Arab political and territorial claims would reassert â the entire colonial-demographic project would stall and the territory would reorganize around indigenous self-determination or competing claims.
% FOUNDING_PROBLEM: Jewish statelessness and minority vulnerability in Europe, particularly following World War I displacements and pogroms, for which the international community sought a territorially grounded solution.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions and British colonial officials attest to the founding problem from beneficiary and administrative seats. Palestinian Arab leadership and later anti-colonial historians attest that the problem was mislocated and the solution imposed on a non-consenting population; the Permanent Mandates Commission and international legal scholars outside the beneficiary set documented the tension between national-home facilitation and indigenous protection.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.82) is high because the mandate framework systematically transfers land and political standing from Palestinian Arabs to Zionist institutions. Suppression (0.85) is higher still because the primacy reading requires active enforcement of immigration controls, land court biases, and military pacification of indigenous resistance (1920 riots, 1936â39 Arab Revolt). Theater ratio (0.50) captures the performative maintenance of 'dual obligation' language and trusteeship rhetoric while actual administration pursued demographic transformation. Accessibility collapse (0.78) reflects the near-total closure of Palestinian self-determination and independent statehood alternatives once the mandate framework was entrenched. Resistance (0.72) registers sustained Palestinian Arab opposition, including the General Strike and armed revolt. The temporal series show monotonic intensification as Jewish institutional capacity deepened and Arab political space compressed.
 *
 * PERSPECTIVAL GAP:
 *   From the Zionist institutional seat, the constraint is legitimate international coordination solving Jewish statelessness; from the Palestinian Arab seat, it is colonial extraction enforced by British military and legal power. The British mandatory seat experiences it as imperial administration with declining returns. These divergences are structurally derived: beneficiaries (low directionality) face constrained but subsidized exit; victims (high directionality) face trapped or blocked exit; the agenda-setting colonial power retains arbitrage-grade exit (can withdraw or re-interpret) but is identity-locked to imperial prestige.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions and Jewish migrants are declared beneficiaries: they receive facilitated immigration, land access, and institutional supremacy (directionality near 0.0, effective extraction damped into subsidy). Palestinian Arab landholders and political leadership are declared victims: they bear land loss and political downgrade (directionality near 1.0, effective extraction amplified). The British mandatory power is not declared in either beneficiary or victim arrays because its structural relationship is primarily administrative enforcement with diffuse colonial benefit; it receives the canonical institutional fallback directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this constraint as tangled_rope prevents the error of treating it as pure coordination (rope) â which would ignore the Palestinian dispossession â or as pure extraction (snare) â which would deny that the mandate did provide a genuine, historically significant coordination function for Jewish refugees and stateless populations. The founding problem (Jewish statelessness and minority vulnerability in Europe) was live, but the solution's asymmetric extraction from indigenous Palestinians creates the hybrid structure. The rising theater ratio and temporal intensification signal that coordination rhetoric became increasingly performative as the demographic transformation accelerated, preventing false benignity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Jewish national home primacy reading the only operational reading of the Mandate, or did the dual obligation to indigenous rights constitute a structurally competing constraint?',
    'Archival analysis of British administrative instructions and League debates to determine whether dual obligation language was operational or decorative.',
    'If dual obligations were structurally operational, this constraint''s epsilon is overestimated because some extraction was genuinely constrained by protective duties; if decorative, the current reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Competing kernel reading ambiguity between primacy and dual obligation.').

omega_variable(
    trusteeship_coordination_genuineness,
    'Did the Mandate system provide a genuine coordination function (international legal framework for Jewish settlement) separable from its extractive function (Palestinian dispossession)?',
    'Counterfactual analysis of whether Jewish state-building could have proceeded without the mandatory framework''s land and immigration privileges.',
    'If separable, the constraint is a Tangled Rope with genuine coordination; if inseparable, it approaches Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trusteeship_coordination_genuineness, conceptual, 'Whether international trusteeship coordination was genuine or cover.').

omega_variable(
    active_enforcement_decay_hypothesis,
    'Did the mandatory power''s enforcement of Jewish national home primacy intensify monotonically, or did it oscillate with periods of indigenous rights accommodation?',
    'Temporal measurement of enforcement intensity, White Papers (1939), and policy shifts.',
    'Oscillation with accommodation would lower suppression and theater ratio at specific intervals, changing temporal classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_enforcement_decay_hypothesis, empirical, 'Enforcement intensity trajectory over the mandate period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0, 0.22).
narrative_ontology:measurement(balf_tr_t4, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 4, 0.28).
narrative_ontology:measurement(balf_tr_t8, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 8, 0.34).
narrative_ontology:measurement(balf_tr_t12, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 12, 0.4).
narrative_ontology:measurement(balf_tr_t16, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 16, 0.44).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 20, 0.47).
narrative_ontology:measurement(balf_tr_t24, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 24, 0.49).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 28, 0.5).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(balf_be_t4, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(balf_be_t8, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(balf_be_t12, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(balf_be_t16, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(balf_be_t24, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 28, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(balf_su_t4, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(balf_su_t8, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(balf_su_t12, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(balf_su_t16, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 16, 0.78).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(balf_su_t24, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 28, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the balfour_mandate_instruments kernel. It is decomposed from the natural-language 'Mandate for Palestine' into structurally distinct claims per the epsilon-invariance principle. Sibling readings instantiate competing normative frameworks from the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
