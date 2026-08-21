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
 *   human_readable: Labor Zionism: Jewish National Regeneration via Socialist Transformation and 'Conquest of Labor'
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story describes the 'Labor Zionism' reading of the Jewish
 *   territorial claim in Palestine, focusing on the period from the Second
 *   Aliyah (1904) to the establishment of Israel (1948). This reading
 *   emphasized Jewish national regeneration through productive labor,
 *   socialist ideals, and the 'conquest of labor' (Kibbush ha-Avoda), which
 *   entailed building an exclusive Jewish economy and excluding Arab labor.
 *   This process was seen as essential for creating 'facts on the ground' and
 *   laying the foundation for a future Jewish state. The constraint operates
 *   as a Tangled Rope, combining a genuine coordination function (building a
 *   national economy) with asymmetric extraction (displacing Arab labor and
 *   land).
 *
 * KEY AGENTS:
 *   - jewish_settlers: Primary beneficiary (organized/identity_locked) — benefits from exclusive labor and land access.
 *   - zionist_institutions: Agenda setter (institutional/constrained) — administers and enforces policies, collects power.
 *   - palestinian_arab_laborers: Primary target (powerless/trapped) — bears economic exclusion and displacement.
 *   - palestinian_arab_landowners: Payer (moderate/constrained) — experiences land acquisition and loss of livelihood.
 *   - british_mandate_authorities: Observer (institutional/analytical) — indirectly facilitates the constraint's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.85).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.9).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism: Jewish National Regeneration via Socialist Transformation and 'Conquest of Labor'").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '03f32b52-8fa2-4ca6-b26d-75aafd10b19e').
narrative_ontology:cs_kernel_codification('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', formalized).
narrative_ontology:cs_authority_grounding('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', lineage).
narrative_ontology:cs_interpretation_layer_present('03f32b52-8fa2-4ca6-b26d-75aafd10b19e').
narrative_ontology:cs_reading_relation('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', foundational, hebrew_labor_as_national_regeneration).
narrative_ontology:cs_axiom_status(hebrew_labor_as_national_regeneration, holdable).
narrative_ontology:cs_axiom_grounding('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', hebrew_labor_as_national_regeneration, conventional).
narrative_ontology:cs_axiom('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', foundational, incremental_settlement_as_state_building).
narrative_ontology:cs_axiom_status(incremental_settlement_as_state_building, holdable).
narrative_ontology:cs_axiom_grounding('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', incremental_settlement_as_state_building, instrumental).
narrative_ontology:cs_reference_frame('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', socialist_pioneering_settlement).
narrative_ontology:cs_drift_state('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', post_1948_statehood, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('03f32b52-8fa2-4ca6-b26d-75aafd10b19e', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from exclusive access to land and labor opportunities, fostering a sense of national regeneration and self-sufficiency. Their identity is deeply intertwined with the success of the settlement project and the 'conquest of labor' ideology.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, beneficiary,
    organized, generational, identity_locked, local).

% Administer land acquisition, settlement funding, and labor policies that prioritize Jewish workers. They actively enforce the 'Hebrew labor' principle and manage the economic separation, seeing it as essential for national building. They derive legitimacy and power from the success of the project.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_institutions, agenda_setter,
    institutional, generational, constrained, regional).

% Are systematically excluded from the Jewish economy and labor market, leading to economic marginalization and loss of traditional livelihoods. Their options are limited by the expanding Jewish economic sphere and lack of alternative employment.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers, payer,
    powerless, immediate, trapped, local).

% Experience land acquisition by Zionist institutions, often through legal but coercive means, leading to displacement and loss of agricultural base. Their resistance is met with institutional and sometimes physical force.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_landowners, payer,
    moderate, biographical, constrained, local).

% Oversee the territory and its populations, often balancing competing claims. While officially neutral, their policies and inaction frequently facilitated Zionist settlement and land acquisition, indirectly enabling the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, observer,
    institutional, biographical, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment of a self-sufficient Jewish national economy and society in Palestine, ensuring that Jewish labor and capital build the new nation from the ground up, rather than relying on external (Arab) labor.
% TRANSFER_FUNCTION: Transfers land and labor opportunities from Palestinian Arabs to Jewish settlers, consolidating economic control and demographic presence for the Jewish national project.
% ABSENT_VOICES: Palestinian Arab political leadership and civil society were largely excluded from the decision-making processes that shaped the 'conquest of labor' policies. They would articulate the dispossession and economic marginalization of the indigenous population.
% DISAPPEARANCE_RATIONALE: If the 'conquest of labor' ideology and its enforcement vanished, the economic and social structures of early Zionist settlement would collapse. Jewish settlers would face competition from Palestinian Arab laborers, land acquisition would slow, and the vision of a purely Jewish national economy would be fundamentally altered, leading to a significant rearrangement of power and resources.
% FOUNDING_PROBLEM: The problem of Jewish national regeneration, perceived as requiring a 'return to the soil' and productive labor, coupled with the need to establish a secure economic base for a future Jewish state, free from reliance on non-Jewish labor.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and ideologues attest to the ongoing relevance of national self-sufficiency and productive labor. Palestinian historians and international observers, while acknowledging the historical context, corroborate the problem's status as 'live' in the sense that its legacy continues to shape the conflict, but dispute its legitimacy as a founding principle for exclusive economic development.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high due to the systematic displacement of Palestinian Arab labor and the acquisition of land for exclusive Jewish settlement, which created a dual economy. Suppression (0.90) is very high, reflecting the active enforcement of 'Hebrew labor' policies by Zionist institutions, often backed by paramilitary groups, and the limited legal or economic recourse for Palestinian Arabs. The theater ratio (0.20) is low, as the 'conquest of labor' was a genuinely functional, albeit extractive, nation-building project, not primarily performative. Accessibility collapse (0.70) is substantial, as alternatives for Palestinian Arab laborers and landowners were actively suppressed. Resistance (0.80) was high, manifesting in Arab revolts and ongoing political opposition.
 *
 * PERSPECTIVAL GAP:
 *   Jewish settlers and Zionist institutions experienced this as a necessary, even liberating, act of national self-determination and economic development. For Palestinian Arab laborers and landowners, it was a process of dispossession, economic marginalization, and colonial settlement. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a 'Rope' or 'Scaffold' and victims experiencing a 'Snare' or 'Tangled Rope'.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers and Zionist institutions are beneficiaries, as the constraint directly enabled their national and economic goals. Palestinian Arab laborers and landowners are victims, bearing the direct costs of exclusion and displacement. The British Mandate authorities, while not direct beneficiaries, often acted in ways that facilitated the Zionist project, making them indirect enablers or observers whose inaction amplified the constraint's effects on victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as pure coordination (Rope) by acknowledging the significant, actively enforced extraction from Palestinian Arabs. It also avoids mislabeling it as pure extraction (Snare) by recognizing the genuine, albeit exclusive, coordination function of building a self-sufficient Jewish national economy. The 'founding problem' of Jewish national regeneration was 'live' for its proponents, but its resolution involved substantial, unacknowledged costs to the indigenous population, which is captured by the high extractiveness and suppression metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hebrew_labor_necessity,
    'Was the exclusion of Arab labor (Kibbush ha-Avoda) a structural necessity for the establishment of a self-sufficient Jewish national economy, or an ideological choice that amplified extraction?',
    'Counterfactual historical analysis comparing economic development models that integrated rather than excluded indigenous labor, or comparative studies of other settler-colonial projects.',
    'If structurally necessary, the extractiveness might be re-evaluated as an unavoidable cost of a specific form of national building. If an ideological choice, it reinforces the classification of high, avoidable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hebrew_labor_necessity, conceptual, 'Ambiguity regarding the structural necessity versus ideological choice of ''Hebrew labor'' policies.').

omega_variable(
    land_acquisition_legitimacy,
    'To what extent was land acquisition by Zionist institutions from Palestinian Arabs a legitimate market transaction, versus a process of coercive displacement enabled by political and economic power imbalances?',
    'Detailed historical and legal analysis of specific land transactions, including the context of Ottoman land laws, British Mandate policies, and the economic vulnerability of Arab sellers.',
    'If primarily legitimate market transactions, the extractiveness related to land might be lower. If primarily coercive, it reinforces high extractiveness and suppression, highlighting the structural violence inherent in the process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_acquisition_legitimacy, empirical, 'Ambiguity regarding the legitimacy of land acquisition processes.').

omega_variable(
    identity_lock_sustainability,
    'How sustainable was the ''identity_locked'' exit option for Jewish settlers, given the inherent conflict and external pressures, and how did it evolve over time?',
    'Sociological studies of settler communities, analysis of internal debates within the Labor Zionist movement, and examination of periods of increased external threat or economic hardship.',
    'If the identity lock proved brittle or required increasing external support, it suggests a higher underlying fragility of the constraint, potentially leading to higher future theater ratios or suppression requirements to maintain cohesion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_sustainability, empirical, 'The long-term sustainability and evolution of identity-locked commitment among Jewish settlers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.1).
narrative_ontology:measurement(jewi_tr_t1918, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1918, 0.15).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1930, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement(jewi_be_t1918, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1918, 0.7).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1930, 0.8).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.65).
narrative_ontology:measurement(jewi_su_t1918, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1918, 0.75).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Jewish territorial claim' kernel. Its structural delta (economic separation, incremental state-building, exclusion of Arab workers) distinguishes it from political, cultural, and revisionist Zionist readings, which emphasize different mechanisms or goals for national regeneration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
