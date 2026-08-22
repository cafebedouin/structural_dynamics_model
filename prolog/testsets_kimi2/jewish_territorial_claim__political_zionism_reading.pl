% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__political_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionist Territorial Sovereignty with Jewish Majority
 *   domain: political/nationalism/settler-colonialism
 *
 * SUMMARY:
 *   This constraint instantiates the political Zionist reading of the Jewish
 *   territorial claim kernel (1897â1967). It treats sovereign Jewish
 *   statehood as the necessary and sufficient solution to antisemitism,
 *   requiring a Jewish demographic majority achieved through immigration,
 *   land acquisition, and ultimately population transfer. The Arab population
 *   is structurally positioned as an obstacle to this majority rather than a
 *   partner in shared sovereignty. The constraint is claimed as coordination
 *   (solving Jewish persecution) while the metrics independently describe
 *   high extraction (dispossession) and suppression (enforced demographic
 *   engineering). This is one reading of a contested kernel; siblings include
 *   cultural, labor, and revisionist Zionisms.
 *
 * KEY AGENTS:
 *   - Zionist state institutions (agenda_setter/institutional): Direct immigration, land purchase, and state-building.
 *   - Jewish immigrants and settlers (beneficiary/moderate): Receive land, citizenship, and collective sovereignty.
 *   - Palestinian Arab population (payer/powerless): Bear dispossession, displacement, and political exclusion.
 *   - British Mandatory Authority (agenda_setter/institutional): Facilitate and legally underwrite the demographic project.
 *   - International diplomatic observers (observer/institutional): Attempt mediation and partition without enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.88).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionist Territorial Sovereignty with Jewish Majority").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political/nationalism/settler-colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '5e6d866a-c633-47eb-991e-6c9c1f6803e1').
narrative_ontology:cs_kernel_codification('5e6d866a-c633-47eb-991e-6c9c1f6803e1', formalized).
narrative_ontology:cs_authority_grounding('5e6d866a-c633-47eb-991e-6c9c1f6803e1', lineage).
narrative_ontology:cs_interpretation_layer_present('5e6d866a-c633-47eb-991e-6c9c1f6803e1').
narrative_ontology:cs_reading_relation('5e6d866a-c633-47eb-991e-6c9c1f6803e1', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('5e6d866a-c633-47eb-991e-6c9c1f6803e1', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e6d866a-c633-47eb-991e-6c9c1f6803e1', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('5e6d866a-c633-47eb-991e-6c9c1f6803e1', foundational, jewish_statehood_as_antidote_to_persecution).
narrative_ontology:cs_axiom_status(jewish_statehood_as_antidote_to_persecution, holdable).
narrative_ontology:cs_axiom_grounding('5e6d866a-c633-47eb-991e-6c9c1f6803e1', jewish_statehood_as_antidote_to_persecution, empirically_contingent).
narrative_ontology:cs_axiom('5e6d866a-c633-47eb-991e-6c9c1f6803e1', foundational, demographic_majority_prerequisite_to_nation_state).
narrative_ontology:cs_axiom_status(demographic_majority_prerequisite_to_nation_state, holdable).
narrative_ontology:cs_axiom_grounding('5e6d866a-c633-47eb-991e-6c9c1f6803e1', demographic_majority_prerequisite_to_nation_state, conventional).
narrative_ontology:cs_reference_frame('5e6d866a-c633-47eb-991e-6c9c1f6803e1', herzlian_territorial_sovereignty).
narrative_ontology:cs_drift_state('5e6d866a-c633-47eb-991e-6c9c1f6803e1', post_1948_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e6d866a-c633-47eb-991e-6c9c1f6803e1', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_state_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, nation_state_solution_to_minority_persecution).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, demographic_majority_as_sovereignty_prerequisite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formed through the Zionist Congresses and Jewish Agency, this institutional network lobbies imperial powers for charter and mandate, organizes Jewish immigration, acquires land, and builds parallel state infrastructure in Palestine with the explicit goal of achieving Jewish majority sovereignty.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Migrate from Europe and elsewhere to Palestine under Zionist auspices, settle on purchased or allocated land, participate in building a Hebrew-speaking society and economy, and accept the political goal of Jewish statehood as the premise of their migration.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_settlers, beneficiary,
    moderate, biographical, identity_locked, regional).

% Comprise the majority of Palestine's population until the late 1940s, work the land as peasants and urban laborers, see their land transferred to Jewish ownership through sale and expropriation, and are progressively marginalized from political representation by the mandate's pro-Zionist immigration and land policies.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Administers Palestine under League of Nations mandate after 1923, issues the Balfour Declaration favoring a Jewish national home, regulates Jewish immigration and land transfers while nominally protecting Arab rights, and ultimately refers the issue to the United Nations before withdrawing in 1948.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandatory_authority, agenda_setter,
    institutional, generational, mobile, global).

% Represent the League of Nations, the United Nations, and various imperial commissions, issuing reports and partition plans that attempt to reconcile Jewish national claims with Arab indigenous rights and regional stability.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_diplomatic_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To resolve Jewish statelessness and vulnerability to persecution in Europe by establishing a territorially sovereign nation-state with a Jewish demographic majority capable of self-defense and self-determination.
% TRANSFER_FUNCTION: Moves land, political sovereignty, and demographic control from the indigenous Palestinian Arab population to the Jewish settler-collective and its state apparatus; population transfer is considered a necessary mechanism to secure the majority.
% ABSENT_VOICES: The Palestinian Arab population is excluded from Zionist Congress deliberations and British-Zionist bargaining; binationalist Jewish voices and non-Zionist Jewish communities (Bundists, ultra-orthodox) are marginalized within the movement's internal consensus.
% DISAPPEARANCE_RATIONALE: Without the territorial sovereignty claim and the necessity of a Jewish majority, the political Zionist project dissolves into cultural or binational alternatives; the demographic and territorial facts on the ground would not have been established, and the 1948 displacement would not have been structurally incentivized.
% FOUNDING_PROBLEM: Jewish dispersion and statelessness in Europe leading to persistent persecution, pogroms, and civil discrimination; the Jewish Question as framed by Herzl, in which no existing state could or would guarantee Jewish collective safety.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Jewish diaspora experiences of persecution documented by non-Zionist historians and international commissions (pogroms, Dreyfus affair, Holocaust). Contested by Marxist and Bundist analyses attributing Jewish vulnerability to capitalism and minority status within states rather than statelessness per se, and by post-colonial critiques arguing the solution reproduced persecution structures against the Palestinian population.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__political_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__political_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.25 to 0.84 because the constraint's core mechanismâmaintaining a Jewish majority in a contested territoryârequires progressively intensifying dispossession and exclusion. Suppression peaks at 0.90 during 1948 because the demographic objective can only be realized through active enforcement against the indigenous majority. Theater rises to 0.50 as state institutions perform legitimate sovereignty while administering absentee property laws and military rule that serve demographic goals. Resistance is high (0.85) throughout due to Arab revolts and international criticism. The claim/metric independence is deliberate: the author claims tangled_rope (genuine coordination for Jewish safety plus asymmetric extraction from Arabs) while the metrics describe the heavily extractive operational reality.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish immigrant seat experiences the constraint as salvation and self-determination (low d, subsidized by the arrangement). The Palestinian Arab seat experiences the same structure as dispossession and erasure (high d, maximally extracted). The British seat experiences it as an imperial administrative burden with strategic benefit (near-symmetric). The engine computes this divergence from beneficiary/victim declarations and exit options; the author does not reconcile the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist state institutions and Jewish immigrants are declared beneficiaries and receive low directionality (the constraint subsidizes their demographic and political project). The Palestinian Arab population is declared victim and receives high directionality (the constraint extracts land, sovereignty, and presence from them). British authorities are neither beneficiary nor victim of the Zionist claim itself and revert to the institutional canonical fallback. No override is needed because the structural derivation matches the analytical judgment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâJewish statelessness and persecution in Europeâwas genuine and well-corroborated. The classification resists mislabeling by documenting both the live founding problem and the extraction mechanism: the constraint coordinates Jewish safety through the same structure that extracts from the indigenous population. If the founding problem is treated as resolved (state established in 1948) but the majority-enforcement mechanism persists, the mismatch between founding_problem_status and disappearance_verdict flags potential mandatrophy or zombie persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_cultural_zionism_foreclosure,
    'Does the political Zionist claim to territorial sovereignty and Jewish majority foreclose the cultural Zionist reading of a spiritual center without statehood?',
    'Comparative analysis of institutional resource allocation and constitutional structure: if the state apparatus actively subordinates cultural institutions to security and demographic priorities, foreclosure is operational regardless of theoretical compatibility.',
    'If foreclosed, the political reading structurally dominates the kernel; if coexistent, the kernel remains genuinely polysemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_cultural_zionism_foreclosure, conceptual, 'Whether political Zionism forecloses cultural Zionism within the same kernel').

omega_variable(
    majority_mechanism_naturalness,
    'Is the requirement for a Jewish demographic majority a contingent political construct of the 20th-century nation-state system, or a necessary feature of Jewish self-determination?',
    'Comparative historical analysis of binational and consociational state models and their stability for similarly situated groups.',
    'If contingent, the constraint is a Tangled Rope or Snare of its era; if necessary, it leans toward Rope for the beneficiary population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majority_mechanism_naturalness, conceptual, 'Contingency of the demographic majority requirement').

omega_variable(
    transfer_as_implementation,
    'Was population transfer a contingent wartime outcome or an inherent structural feature of the political Zionist majority-project?',
    'Archival analysis of pre-1948 Zionist planning documents and committee records to establish how integral transfer was to the state-building design.',
    'If inherent, extractiveness is structurally high and the coordination story is largely cover; if contingent, the constraint may show lower authored extraction at earlier time points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_as_implementation, empirical, 'Whether transfer was inherent or contingent to political Zionism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_pol_tr_t0, jewish_territorial_claim__political_zionism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jtc_pol_tr_t10, jewish_territorial_claim__political_zionism_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(jtc_pol_tr_t20, jewish_territorial_claim__political_zionism_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(jtc_pol_tr_t30, jewish_territorial_claim__political_zionism_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(jtc_pol_tr_t40, jewish_territorial_claim__political_zionism_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(jtc_pol_tr_t51, jewish_territorial_claim__political_zionism_reading, theater_ratio, 51, 0.45).
narrative_ontology:measurement(jtc_pol_tr_t60, jewish_territorial_claim__political_zionism_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(jtc_pol_tr_t70, jewish_territorial_claim__political_zionism_reading, theater_ratio, 70, 0.48).

% Extraction over time
narrative_ontology:measurement(jtc_pol_be_t0, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jtc_pol_be_t10, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(jtc_pol_be_t20, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(jtc_pol_be_t30, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(jtc_pol_be_t40, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(jtc_pol_be_t51, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 51, 0.85).
narrative_ontology:measurement(jtc_pol_be_t60, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(jtc_pol_be_t70, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 70, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(jtc_pol_su_t0, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jtc_pol_su_t10, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(jtc_pol_su_t20, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(jtc_pol_su_t30, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(jtc_pol_su_t40, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(jtc_pol_su_t51, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 51, 0.9).
narrative_ontology:measurement(jtc_pol_su_t60, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(jtc_pol_su_t70, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 70, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_territorial_claim kernel, instantiated as the political Zionist reading prioritizing state sovereignty and Jewish majority. Other readings decompose the same kernel into structurally distinct constraints with different epsilon values and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
