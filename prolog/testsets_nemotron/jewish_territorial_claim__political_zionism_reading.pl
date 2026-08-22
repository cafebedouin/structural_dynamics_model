% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__political_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: jewish_territorial_claim__political_zionism_reading
 *   human_readable: Political Zionism: Jewish Statehood as Solution to Antisemitism Requiring Territorial Sovereignty with Jewish Majority
 *   domain: political_history/settl_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story captures the political Zionist reading of the
 *   Jewish territorial claim kernel: the insistence that Jewish safety and
 *   national normalization require a sovereign state with a Jewish
 *   demographic majority in Palestine. The reading treats the Arab population
 *   as a demographic obstacle to be managed — through transfer, partition, or
 *   military conquest — rather than as a partner in a shared polity. The
 *   constraint's extraction is the transfer of land, sovereignty, and
 *   demographic destiny from Palestinians to the Jewish national project; its
 *   coordination function is the concentration of a stateless people into a
 *   defensible polity. The engine computes per-seat types from the structural
 *   data: the Zionist leadership and immigrants experience coordination with
 *   subsidy; Palestinians experience enforced extraction with constrained
 *   exit; British authority experiences a declining coordination function
 *   that becomes pure administrative burden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__political_zionism_reading, 0.78).
domain_priors:suppression_score(jewish_territorial_claim__political_zionism_reading, 0.82).
domain_priors:theater_ratio(jewish_territorial_claim__political_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(jewish_territorial_claim__political_zionism_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__political_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__political_zionism_reading, "Political Zionism: Jewish Statehood as Solution to Antisemitism Requiring Territorial Sovereignty with Jewish Majority").
narrative_ontology:topic_domain(jewish_territorial_claim__political_zionism_reading, "political_history/settl_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__political_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__political_zionism_reading, '59e51e39-fc7e-40a5-96d4-c574a4e74b1f').
narrative_ontology:cs_kernel_codification('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', formalized).
narrative_ontology:cs_authority_grounding('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', lineage).
narrative_ontology:cs_interpretation_layer_present('59e51e39-fc7e-40a5-96d4-c574a4e74b1f').
narrative_ontology:cs_reading_relation('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', foundational, jewish_majority_sovereignty_necessary_for_safety).
narrative_ontology:cs_axiom_status(jewish_majority_sovereignty_necessary_for_safety, holdable).
narrative_ontology:cs_axiom_grounding('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', jewish_majority_sovereignty_necessary_for_safety, empirically_contingent).
narrative_ontology:cs_axiom('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', foundational, arab_population_as_demographic_obstacle).
narrative_ontology:cs_axiom_status(arab_population_as_demographic_obstacle, holdable).
narrative_ontology:cs_axiom_grounding('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', arab_population_as_demographic_obstacle, instrumental).
narrative_ontology:cs_reference_frame('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', herzlian_statist_solution).
narrative_ontology:cs_drift_state('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', post_1948_statehood, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59e51e39-fc7e-40a5-96d4-c574a4e74b1f', '2026-08-15T12:00:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_leadership).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_peasantry_fellahin).
narrative_ontology:constraint_victim(jewish_territorial_claim__political_zionism_reading, palestinian_urban_notables).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__political_zionism_reading, zionist_leadership).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, jewish_national_self_determination).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__political_zionism_reading, antisemitism_requires_territorial_solution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the political program for Jewish statehood, builds institutions (Jewish Agency, Haganah, Histadrut), negotiates with imperial powers, and directs settlement. Benefits from institutional control and international recognition. Exit means abandoning the statehood project; constrained by ideological commitment and sunk organizational investment.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, zionist_leadership, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, zionist_leadership, beneficiary).

% Arrive as refugees and ideologues; receive land, employment, and political membership in the emerging polity. Their identity fuses with the project — exit means not just leaving a place but unmaking a self-concept. The constraint subsidizes them with territory and rights extracted from the Arab population.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_immigrants_settlers, beneficiary,
    moderate, biographical, identity_locked, national).

% Mobilizes philanthropic and political resources for the project; gains a sovereign fallback and symbolic center. Not personally subject to the constraint's extraction on the ground; can redirect support if the project fails. Mobile exit — institutional, not existential.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, jewish_diaspora_leadership, beneficiary,
    organized, generational, mobile, global).

% Subject to land expropriation, demographic displacement, political exclusion, and military conquest. Organized national movement emerges in response but is structurally outgunned. Exit options constrained by attachment to land, lack of alternative sovereignty, and British mandatory suppression of rebellion.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_arab_population, payer,
    organized, generational, constrained, national).

% Lose land tenure to Jewish National Fund purchases and colonial land law; become wage laborers or refugees. No political voice, no exit — trapped in place by poverty, colonial administration, and the logic of 'conquest of labor' that displaces them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_peasantry_fellahin, payer,
    powerless, biographical, trapped, local).

% Attempt political negotiation and petition to British; excluded from the constraint's decision structure (the Mandate's 'Jewish National Home' clause privileges the Zionist claim). Their authority erodes as the constraint's enforcement hardens; constrained exit via exile or accommodation.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, palestinian_urban_notables, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, palestinian_urban_notables, excluded).

% Administers the constraint via the Mandate; both enables and restrains Zionist state-building. Extracts imperial strategic value (Suez, oil, air routes) but loses control as the constraint's internal logic outpaces British management. Mobile exit — withdraws in 1948 when the constraint becomes unmanageable.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__political_zionism_reading, british_mandate_authority, observer).

% Produces the partition resolution (UNGA 181) and later human rights frameworks. Observes the constraint's operation from outside; its verdicts legitimize or delegitimize but do not control enforcement. Analytical exit — can revise interpretation without material cost.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__political_zionism_reading, international_legal_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of Jewish collective survival under antisemitism by concentrating a dispersed people into a sovereign territorial polity with a Jewish demographic majority, enabling self-defense, immigration control, and international legal personality.
% TRANSFER_FUNCTION: Transfers land, labor, political sovereignty, and demographic weight from the indigenous Arab population to the Jewish immigrant-settler collective, mediated by British imperial administration and Zionist institutions.
% ABSENT_VOICES: Palestinian Arab peasants and urban poor had no representation in the League of Nations Mandate system, the Zionist Congress, or British cabinets; their opposition was treated as rioting rather than politics. Arab women, Bedouin communities, and non-elite religious minorities were entirely excluded from the diplomatic frame.
% DISAPPEARANCE_RATIONALE: If the political Zionist constraint vanished in 1897, no Jewish state emerges in Palestine; the Jewish Question remains unresolved in Europe (leading to different outcomes: assimilation, emigration to Americas, or genocide); Arab Palestine evolves under Ottoman then British rule without settler-colonial displacement; the Middle East map is unrecognizable.
% FOUNDING_PROBLEM: The 'Jewish Question' in Europe: persistent antisemitism, legal discrimination, pogroms, and the failure of emancipation to secure Jewish safety and equality; the need for a territorial solution where Jews exercise sovereign self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Zionist leadership (Herzl, Weizmann, Ben-Gurion) attests the problem is live and requires statehood. Palestinian leadership (Husseini, Nashashibi) and Arab states attest the problem is European, not Palestinian, and the solution extracts from the wrong population. British Royal Commission (Peel 1937) attests the conflict is irreconcilable — two national claims on one land. Holocaust survivors' testimony corroborates the European danger but not the Palestinian extraction.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__political_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__political_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__political_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_territorial_claim__political_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__political_zionism_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint's operation transfers the core material and political assets of an indigenous population to an immigrant collective. Suppression (0.82) is very high because the constraint's persistence depends on British military power, then Jewish military power, actively suppressing Palestinian political agency and physical presence. Theater ratio (0.28) is moderate: the 'civilizing mission' and 'making the desert bloom' narratives perform coordination while the mechanism is conquest. Accessibility collapse (0.73) is high because the binary of Jewish state vs. Arab Palestine leaves little room for bi-national or federal alternatives once the constraint is understood. Resistance (0.67) is substantial: Palestinian revolts (1936-39), diplomatic rejection, and military opposition persisted throughout.
 *
 * PERSPECTIVAL GAP:
 *   The Zionist seat experiences a rope-like coordination structure (solving Jewish statelessness); the Palestinian seats experience a snare-like extraction structure (land and sovereignty taken by force); the British seat experiences a scaffold that collapses (transitional mandate that cannot manage the forces it unleashed). The engine computes this divergence from the declared roles, power, and exit options — the claimed_type 'tangled_rope' reflects the author's judgment that the constraint as a whole has BOTH genuine coordination (for Jews) AND asymmetric extraction (from Palestinians) requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist leadership and immigrants are structural beneficiaries (d near 0.0–0.2): the constraint subsidizes them with land, rights, and sovereignty. Palestinian peasantry is fully trapped (d ≈ 1.0): no exit, total extraction. Palestinian notables are constrained payers (d ≈ 0.7–0.8): some agency but structurally excluded. British authority shifts from agenda-setter with mobile exit (d ≈ 0.3 early) to trapped administrator (d ≈ 0.6 late) as the constraint outpaces imperial control. Jewish diaspora leadership is mobile beneficiary (d ≈ 0.1): gains symbolic capital without material cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (European antisemitism) was real and catastrophic; the Holocaust confirmed it. But the constraint's mechanism — solving a European problem by extracting from a non-European population — means the solution carries its own structural extraction that persists after the founding crisis. The mandate has not atrophied; it intensified into statehood. The 'mandatrophy' question is whether the constraint's current form (an expansionist Jewish state) still serves the original coordination function or has become self-perpetuating extraction. The status 'contested' reflects that Zionists say yes (security still requires maximum land and demographic control); Palestinians and critics say no (the founding problem is solved for Jews, the extraction continues for its own sake).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_necessity_ambiguity,
    'Was population transfer (displacement of Arabs) structurally necessary for a Jewish demographic majority, or was it a contingent choice by Zionist leadership?',
    'Counterfactual analysis of 1947-49: could a Jewish state with a stable majority have emerged without mass expulsion, given the demographic ratios and the UN partition borders? Compare with binationalist proposals (Magnes, Buber) and the actual military dynamics.',
    'If necessary, the extraction is baked into the coordination function (tangled_rope is the only honest type). If contingent, the extraction is a political choice layered on top of coordination (could be rope with a snare overlay).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_necessity_ambiguity, conceptual, 'Whether the constraint''s extraction is intrinsic to its coordination or a separable political choice.').

omega_variable(
    british_complicity_vs_constraint,
    'Was British mandatory policy an independent enforcement of the constraint, or was it captured by Zionist pressure?',
    'Archive analysis of British cabinet debates, Colonial Office correspondence, and military reports 1917-1948: track when British policy leads vs. follows Zionist demands.',
    'If British enforcement is independent imperial logic, the constraint has institutional depth beyond Zionist agency. If captured, the constraint''s enforcement is a Zionist-British fusion that collapses when British interest ends (1948).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_complicity_vs_constraint, empirical, 'Whether the constraint''s active enforcement is autonomously British or Zionist-captured.').

omega_variable(
    founding_problem_persistence,
    'Does the Holocaust''s confirmation of European antisemitism validate the constraint''s founding problem permanently, or does the founding problem''s resolution (Jewish state exists) change the constraint''s legitimacy?',
    'Track Zionist discourse 1948-1967: does the rhetoric shift from ''refuge from antisemitism'' to ''historical right to the whole land''? Measure the gap between declared security needs and actual territorial expansion.',
    'If founding problem remains live, the constraint retains coordination legitimacy. If founding problem is dead but constraint expands, mandatrophy resolves toward snare/piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, preference, 'Whether the constraint''s original justification remains its operating logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__political_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewish_territorial_claim_pzr_tr_t1897, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement(jewish_territorial_claim_pzr_tr_t1917, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1917, 0.22).
narrative_ontology:measurement(jewish_territorial_claim_pzr_tr_t1936, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(jewish_territorial_claim_pzr_tr_t1948, jewish_territorial_claim__political_zionism_reading, theater_ratio, 1948, 0.28).

% Extraction over time
narrative_ontology:measurement(jewish_territorial_claim_pzr_be_t1897, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1897, 0.45).
narrative_ontology:measurement(jewish_territorial_claim_pzr_be_t1917, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1917, 0.62).
narrative_ontology:measurement(jewish_territorial_claim_pzr_be_t1936, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1936, 0.75).
narrative_ontology:measurement(jewish_territorial_claim_pzr_be_t1948, jewish_territorial_claim__political_zionism_reading, base_extractiveness, 1948, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewish_territorial_claim_pzr_su_t1897, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1897, 0.35).
narrative_ontology:measurement(jewish_territorial_claim_pzr_su_t1917, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1917, 0.58).
narrative_ontology:measurement(jewish_territorial_claim_pzr_su_t1936, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(jewish_territorial_claim_pzr_su_t1948, jewish_territorial_claim__political_zionism_reading, suppression_requirement, 1948, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__political_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__political_zionism_reading, 0.18).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, palestinian_national_movement_constraint).
narrative_ontology:affects_constraint(jewish_territorial_claim__political_zionism_reading, british_mandate_constraint).

% DUAL FORMULATION NOTE:
% This reading (political_zionism) shares the kernel jewish_territorial_claim with three sibling readings. The epsilon values diverge: cultural_zionism (ε≈0.2, rope), labor_zionism (ε≈0.55, tangled_rope), revisionist_zionism (ε≈0.85, snare). This reading sits at ε≈0.78 (tangled_rope) — state-building prioritized, transfer necessary. The network edges represent the constraint family: each reading inherits the kernel's territorial claim but differs in extraction mechanism and coordination scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, organized, 0.15).
constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, powerless, 0.95).
constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, moderate, 0.75).
constraint_indexing:directionality_override(jewish_territorial_claim__political_zionism_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
