% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionism's 'Conquest of Labor' and Settlement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint describes the Labor Zionist reading of the Jewish
 *   territorial claim in Palestine, focusing on 'conquest of labor' and
 *   socialist settlement as the primary mechanisms for national regeneration.
 *   It involves building a self-sufficient Jewish economy and society, often
 *   at the expense of existing Palestinian Arab economic structures and
 *   labor. The constraint is actively enforced through Zionist institutions
 *   that prioritize Jewish labor and land acquisition, leading to the
 *   exclusion and displacement of Palestinian Arab workers and landowners.
 *
 * KEY AGENTS:
 *   - jewish_settlers: Primary beneficiary (moderate/constrained) — directly benefits from exclusive labor and land.
 *   - zionist_organizations: Agenda setter/beneficiary (institutional/arbitrage) — orchestrates settlement, land acquisition, and labor policies.
 *   - palestinian_arab_laborers: Primary victim (powerless/trapped) — excluded from Jewish economy, displaced from traditional livelihoods.
 *   - palestinian_landowners: Victim (moderate/constrained) — land acquired through various means, often under duress or through legal loopholes.
 *   - british_mandate_authorities: Observer/agenda_setter (institutional/analytical) — nominally governs, but often facilitates Zionist aims or fails to prevent their expansion.
 *   - arab_nationalist_leaders: Payer/excluded (organized/constrained) — resist Zionist expansion but lack effective means to halt it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.75).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism's 'Conquest of Labor' and Settlement").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '8bd408f2-b316-4b16-a4e2-e457f04572f8').
narrative_ontology:cs_kernel_codification('8bd408f2-b316-4b16-a4e2-e457f04572f8', formalized).
narrative_ontology:cs_authority_grounding('8bd408f2-b316-4b16-a4e2-e457f04572f8', lineage).
narrative_ontology:cs_interpretation_layer_present('8bd408f2-b316-4b16-a4e2-e457f04572f8').
narrative_ontology:cs_reading_relation('8bd408f2-b316-4b16-a4e2-e457f04572f8', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bd408f2-b316-4b16-a4e2-e457f04572f8', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bd408f2-b316-4b16-a4e2-e457f04572f8', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('8bd408f2-b316-4b16-a4e2-e457f04572f8', foundational, national_regeneration_through_labor).
narrative_ontology:cs_axiom_status(national_regeneration_through_labor, holdable).
narrative_ontology:cs_axiom_grounding('8bd408f2-b316-4b16-a4e2-e457f04572f8', national_regeneration_through_labor, deontological).
narrative_ontology:cs_axiom('8bd408f2-b316-4b16-a4e2-e457f04572f8', foundational, incremental_state_building_through_facts_on_ground).
narrative_ontology:cs_axiom_status(incremental_state_building_through_facts_on_ground, holdable).
narrative_ontology:cs_axiom_grounding('8bd408f2-b316-4b16-a4e2-e457f04572f8', incremental_state_building_through_facts_on_ground, instrumental).
narrative_ontology:cs_reference_frame('8bd408f2-b316-4b16-a4e2-e457f04572f8', socialist_pioneering_ethos).
narrative_ontology:cs_drift_state('8bd408f2-b316-4b16-a4e2-e457f04572f8', post_1948_statehood, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8bd408f2-b316-4b16-a4e2-e457f04572f8', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_organizations).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Immigrant Jewish individuals and families who settled in Palestine, working the land and building new communities. They directly benefited from the policies of exclusive Jewish labor and land acquisition, seeing it as fulfilling a national and personal mission. Their exit options were constrained by ideological commitment and the difficulty of returning to their countries of origin.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, beneficiary,
    moderate, biographical, constrained, local).

% Institutions like the Jewish Agency, Jewish National Fund, and Histadrut (General Federation of Jewish Labor) that actively planned, funded, and implemented land purchases, settlement projects, and labor policies. They set the agenda for the 'conquest of labor' and enforced economic separation, benefiting from the consolidation of Jewish national infrastructure and political influence.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Indigenous Arab workers who were systematically excluded from employment in Jewish enterprises and settlements, often losing their traditional livelihoods and facing economic hardship. Their options were severely limited by the expanding Jewish economy and the lack of alternative opportunities.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers, payer,
    powerless, immediate, trapped, local).

% Arab families and individuals who owned land in Palestine, some of which was purchased by Zionist organizations. While some sales were voluntary, others occurred under economic pressure or through intermediaries, leading to displacement and loss of ancestral lands. Their ability to resist sales was constrained by economic factors and the political landscape.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_landowners, payer,
    moderate, generational, constrained, local).

% The governing power in Palestine from 1920-1948, tasked with implementing the Balfour Declaration while also protecting the rights of existing non-Jewish communities. Their policies often facilitated Zionist land acquisition and settlement, or failed to effectively counter the exclusion of Arab labor, making them complicit in the constraint's operation despite nominal neutrality.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, agenda_setter).

% Political and religious leaders representing the Palestinian Arab community, who actively opposed Zionist expansion and the 'conquest of labor' policies. They sought to mobilize resistance and appeal to international bodies but faced significant power imbalances and suppression from both Zionist organizations and, at times, the British Mandate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_nationalist_leaders, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, zionist_organizations).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the settlement, economic development, and social organization of the Jewish Yishuv (community) in Palestine, fostering a self-sufficient national economy and society through collective labor and land ownership.
% TRANSFER_FUNCTION: Transfers land, economic opportunities, and political influence from Palestinian Arab laborers and landowners to Jewish settlers and Zionist organizations, facilitating the establishment of a Jewish national home.
% ABSENT_VOICES: Palestinian Arab political and economic representatives were largely excluded from the decision-making processes of Zionist organizations and the British Mandate that shaped these policies. They would have advocated for equal labor rights, protection of land ownership, and self-determination.
% DISAPPEARANCE_RATIONALE: If the 'conquest of labor' and settlement policies vanished, the entire economic and social structure of the Yishuv would collapse. Jewish settlements would lose their exclusive labor force, land acquisition would halt, and the trajectory towards a Jewish state would be fundamentally altered, leading to a complete reorganization of the region's political and economic landscape.
% FOUNDING_PROBLEM: The founding problem was the perceived need for Jewish national regeneration and self-determination in Palestine, driven by antisemitism in Europe and the desire to create a self-sufficient Jewish society rooted in manual labor and agriculture.
% FOUNDING_PROBLEM_CORROBORATION: Zionist organizations and Jewish settlers consistently attested to the live status of the founding problem, citing ongoing antisemitism and the need for national self-sufficiency. Palestinian Arab leaders and international observers, however, contested the methods, arguing that the 'solution' for one group created a problem for another, but the core Zionist aspiration for national regeneration remained a driving force.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it presents a coordination function (building a Jewish national home, creating a self-sufficient economy) but achieves this through asymmetric extraction and suppression. Extractiveness is high (0.85) due to the systematic transfer of land and economic opportunities from Palestinian Arabs to Jewish settlers. Suppression is also high (0.75) as it requires active enforcement by Zionist institutions and, implicitly, the British Mandate's tolerance or complicity, to exclude Arab labor and facilitate land acquisition. The theater ratio is low (0.1) because the 'conquest of labor' was a genuinely held ideological and practical goal, not merely a performance.
 *
 * PERSPECTIVAL GAP:
 *   Jewish settlers and Zionist organizations experienced this as a necessary and legitimate act of national building and self-determination, a 'Rope' coordinating their collective efforts. Palestinian Arab laborers and landowners experienced it as a 'Snare' of dispossession and economic marginalization. The engine's classification as Tangled Rope reflects this dual nature, acknowledging the internal coordination while highlighting the external extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers and Zionist organizations are beneficiaries (low d) as they directly gain land, employment, and national infrastructure. Palestinian Arab laborers and landowners are victims (high d) as they lose land, employment, and economic autonomy. British Mandate authorities are complex: nominally neutral, their policies often facilitated Zionist expansion, making them indirect beneficiaries or complicit agenda-setters. Arab nationalist leaders are targets of the constraint's expansion, bearing the costs of its success.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'conquest of labor' mandate was to build a self-sufficient Jewish economy. While the problem of Jewish national regeneration was live, the methods employed (exclusion of Arab labor, land acquisition) created a structure that became increasingly extractive. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the victims) or a pure Snare (ignoring the genuine coordination among settlers). The persistence of the constraint beyond the initial 'pioneering' phase, even as the founding problem of basic economic viability was arguably met, indicates a drift towards sustained extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_settler_colonialism,
    'Is the ''conquest of labor'' a legitimate act of national self-determination or a mechanism of settler-colonial dispossession?',
    'Analysis of land acquisition methods, labor market exclusion policies, and international legal frameworks concerning indigenous rights and self-determination.',
    'If primarily settler-colonial, the constraint''s extractiveness and suppression are higher, reclassifying it closer to a Snare. If primarily national self-determination, the coordination function is emphasized, potentially reclassifying it as a more benign Tangled Rope or even a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_vs_settler_colonialism, conceptual, 'Ambiguity between national self-determination and settler-colonialism.').

omega_variable(
    labor_zionism_vs_other_readings,
    'How would the structural classification of the Jewish territorial claim change if viewed through the lens of Political Zionism or Revisionist Zionism?',
    'Generate separate constraint stories for each reading, comparing their declared beneficiaries, victims, and metric profiles.',
    'Political Zionism would likely emphasize state-building and security, potentially leading to higher suppression and extractiveness through military means. Revisionist Zionism would likely show maximalist territorial claims and even higher suppression/extractiveness due to its ''Iron Wall'' doctrine. This reading (Labor Zionism) emphasizes economic and social transformation as the primary mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_zionism_vs_other_readings, conceptual, 'Impact of alternative Zionist readings on constraint classification.').

omega_variable(
    economic_separation_sustainability,
    'Was the economic separation (Hebrew labor) truly sustainable as a long-term strategy for national regeneration, or was it always dependent on external funding and political protection?',
    'Historical economic analysis of the Yishuv''s balance of payments, sources of capital, and labor market dynamics without external Zionist funding.',
    'If unsustainable, the ''conquest of labor'' aspect of the constraint was more performative (higher theater_ratio) and less a genuine coordination mechanism, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_separation_sustainability, empirical, 'Sustainability of economic separation as a national strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1900, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(jewi_tr_t1910, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1910, 0.08).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(jewi_tr_t1940, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1940, 0.09).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(jewi_be_t1910, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1910, 0.68).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1920, 0.75).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1930, 0.8).
narrative_ontology:measurement(jewi_be_t1940, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1940, 0.83).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(jewi_su_t1910, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1910, 0.5).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(jewi_su_t1940, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1940, 0.72).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Jewish territorial claim' kernel, focusing on Labor Zionism's 'conquest of labor' and settlement. Other readings (Political, Cultural, Revisionist Zionism) represent distinct constraints with different mechanisms and impacts, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
