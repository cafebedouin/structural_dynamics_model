% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis: Settler-Colonial Reading
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint story analyzes Zionism as a European settler-colonial
 *   movement, focusing on its structural role in establishing an ethno-state
 *   through the systematic displacement of indigenous Palestinians. This
 *   reading emphasizes the colonial nature of the project, where the
 *   legitimacy of the state is derived from and maintained by the ongoing
 *   dispossession and control of the native population, rather than from a
 *   purely national liberation or religious restoration narrative. The high
 *   extractiveness and suppression reflect the material and political costs
 *   borne by the indigenous population.
 *
 * KEY AGENTS:
 *   - State of Israel: Primary agenda-setter and enforcer of the settler-colonial structure.
 *   - Zionist Settlers: Direct beneficiaries of land and resources, acting within the state's framework.
 *   - Indigenous Palestinians: Primary victims, experiencing displacement, dispossession, and suppression.
 *   - International Human Rights Organizations: Observers documenting and challenging the human rights impacts.
 *   - Anti-Colonial Scholars: Analytical observers providing the theoretical framework for this reading.
 *   - Historical European Powers: Early beneficiaries who facilitated the project for geopolitical gain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.88).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.92).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis: Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '1eaf8a04-f427-45e6-b66a-ceed20eec770').
narrative_ontology:cs_kernel_codification('1eaf8a04-f427-45e6-b66a-ceed20eec770', formalized).
narrative_ontology:cs_authority_grounding('1eaf8a04-f427-45e6-b66a-ceed20eec770', extraction).
narrative_ontology:cs_interpretation_layer_present('1eaf8a04-f427-45e6-b66a-ceed20eec770').
narrative_ontology:cs_reading_relation('1eaf8a04-f427-45e6-b66a-ceed20eec770', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('1eaf8a04-f427-45e6-b66a-ceed20eec770', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('1eaf8a04-f427-45e6-b66a-ceed20eec770', foundational, settler_colonialism_as_structural_feature).
narrative_ontology:cs_axiom_status(settler_colonialism_as_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('1eaf8a04-f427-45e6-b66a-ceed20eec770', settler_colonialism_as_structural_feature, empirically_contingent).
narrative_ontology:cs_axiom('1eaf8a04-f427-45e6-b66a-ceed20eec770', foundational, indigenous_displacement_as_constitutive).
narrative_ontology:cs_axiom_status(indigenous_displacement_as_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('1eaf8a04-f427-45e6-b66a-ceed20eec770', indigenous_displacement_as_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('1eaf8a04-f427-45e6-b66a-ceed20eec770', european_colonial_expansion_framework).
narrative_ontology:cs_drift_state('1eaf8a04-f427-45e6-b66a-ceed20eec770', contemporary_post_colonial_studies_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1eaf8a04-f427-45e6-b66a-ceed20eec770', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, historical_european_powers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional entity that formalizes and enforces policies of land acquisition, settlement expansion, and control over indigenous populations. It benefits directly from the territorial and demographic outcomes of the settler-colonial project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, state_of_israel, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals and communities who migrate to and establish residence in the territory, benefiting from land grants, state protection, and the displacement of indigenous inhabitants. Their actions are enabled and protected by the state's policies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_settlers, beneficiary,
    powerful, biographical, constrained, regional).

% The original inhabitants of the land who experience displacement, dispossession of property, loss of sovereignty, and suppression of their national identity and rights. They bear the primary costs of the settler-colonial project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, indigenous_palestinians, payer,
    powerless, generational, trapped, regional).

% Organizations that monitor and report on human rights violations, including those related to displacement, occupation, and discrimination. They advocate for international law and accountability but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Academics and researchers who analyze Zionism through a settler-colonial framework, documenting its historical and ongoing impacts on indigenous populations. They contribute to the intellectual discourse challenging the legitimacy basis.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, anti_colonial_scholars, observer,
    analytical, biographical, analytical, global).

% Colonial powers (e.g., Great Britain) that facilitated the early stages of the Zionist project, benefiting from geopolitical influence and the management of regional populations, often at the expense of indigenous self-determination.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, historical_european_powers, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, state_of_israel).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the migration, settlement, and state-building efforts of European Jewish populations in Palestine, creating a new society and political entity.
% TRANSFER_FUNCTION: Transfers land, resources, and political sovereignty from indigenous Palestinian inhabitants to Zionist settlers and the State of Israel, facilitated by military and legal mechanisms.
% ABSENT_VOICES: Indigenous Palestinians were systematically excluded from the international political processes (e.g., League of Nations, UN partition plan) that legitimized the Zionist project, and their right to self-determination was largely ignored. Their voices would articulate the experience of dispossession and demand decolonization.
% DISAPPEARANCE_RATIONALE: If the settler-colonial structure underpinning the State of Israel vanished, the entire political, legal, and territorial arrangement would collapse. Land ownership, citizenship, and governance would need to be fundamentally re-evaluated and reorganized, leading to a radical transformation of the region.
% FOUNDING_PROBLEM: The perceived need for a secure Jewish homeland due to centuries of antisemitism and persecution in Europe, culminating in the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: Zionist narratives and many international supporters attest that the founding problem of Jewish insecurity remains live. However, indigenous Palestinian accounts and anti-colonial scholars argue that while Jewish insecurity was a real problem in Europe, the solution implemented created a new problem of insecurity and dispossession for another people, rendering the original problem 'dead' in its original context but transformed into a new, extractive dynamic.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the core function of the constraint, from this reading, is the transfer of land, resources, and sovereignty from indigenous Palestinians to the settler population and the state. Suppression is also very high (0.92) due to the active and continuous military, legal, and administrative measures required to maintain control, suppress indigenous resistance, and prevent the return of displaced populations. Theater ratio is moderate-high (0.65) as justifications for actions often rely on narratives of security, historical right, or self-defense, which, from this perspective, serve to mask the underlying extractive and displacement-oriented structure. Accessibility collapse is high for indigenous Palestinians as alternatives to displacement or subjugation are systematically removed, while resistance remains high due to ongoing struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the State of Israel and Zionist settlers, the constraint is often framed as a national liberation project or a necessary security measure, implying a 'rope' or 'scaffold' type. However, from the perspective of indigenous Palestinians and anti-colonial scholars, the same structure operates as a 'snare' due to its coercive, extractive, and suppressive nature, with identifiable victims and suppressed alternatives. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Zionist settlers are clear beneficiaries, gaining land, resources, and political power, placing them at the low-d end. Historical European powers also benefited geopolitically. Indigenous Palestinians are the primary targets, bearing the costs of displacement and dispossession, placing them at the high-d end. International human rights organizations and anti-colonial scholars function as analytical observers, attempting to expose the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by explicitly identifying the extractive and coercive elements often obscured by 'national liberation' or 'security' narratives. It argues that the 'mandate' of providing a safe haven for Jews has, in this specific historical and structural instantiation, become entangled with (and, from this reading, superseded by) a settler-colonial dynamic that extracts from and suppresses an indigenous population. The high extractiveness and suppression, coupled with identifiable victims, firmly classify it as a snare, regardless of any claimed coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_status_ambiguity,
    'Is the claim of indigenous status for Jewish people in historical Palestine compatible with the settler-colonial framework applied to European Jewish migration?',
    'Historical and anthropological analysis of pre-Zionist Jewish communities in Palestine versus the demographic and political characteristics of the Zionist movement''s migration waves.',
    'If a significant portion of the Zionist movement is recognized as indigenous, it complicates the ''settler-colonial'' classification, potentially shifting the constraint towards a ''tangled_rope'' or ''contested'' status regarding land claims. If not, the settler-colonial framework is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_status_ambiguity, conceptual, 'Ambiguity regarding the indigenous status of Jewish people in the context of Zionist migration.').

omega_variable(
    security_vs_expansion_motivation,
    'To what extent are the State of Israel''s actions (e.g., settlement expansion, military operations) primarily driven by genuine security concerns versus a constitutive drive for territorial expansion and demographic control?',
    'Analysis of declassified state documents, independent military and intelligence assessments, and comparison of stated security needs with actual territorial outcomes and demographic policies.',
    'If security is demonstrably the primary driver, it might introduce a stronger ''coordination'' element, potentially shifting the classification towards a ''tangled_rope''. If expansion is primary, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_expansion_motivation, empirical, 'Distinguishing security motivations from expansionist settler-colonial drives.').

omega_variable(
    international_law_application_ambiguity,
    'How should international law (e.g., self-determination, laws of occupation, anti-apartheid conventions) be applied to the historical and contemporary situation, and what are the implications for the constraint''s legitimacy?',
    'Adjudication by international courts (e.g., ICJ), consensus among international legal scholars, and consistent application by UN bodies and states.',
    'A consistent application of international law supporting indigenous rights and condemning settler-colonial practices would severely undermine the constraint''s claimed legitimacy, reinforcing its ''snare'' classification and increasing pressure for its dismantling. Contested application maintains ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_law_application_ambiguity, preference, 'Ambiguity in the application of international law to the conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1880, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement(zion_tr_t1908, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1908, 0.3).
narrative_ontology:measurement(zion_tr_t1936, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1936, 0.45).
narrative_ontology:measurement(zion_tr_t1964, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1964, 0.55).
narrative_ontology:measurement(zion_tr_t1992, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1992, 0.6).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(zion_be_t1880, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(zion_be_t1908, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1908, 0.55).
narrative_ontology:measurement(zion_be_t1936, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1936, 0.7).
narrative_ontology:measurement(zion_be_t1964, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1964, 0.8).
narrative_ontology:measurement(zion_be_t1992, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1992, 0.85).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1880, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(zion_su_t1908, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1908, 0.5).
narrative_ontology:measurement(zion_su_t1936, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1936, 0.75).
narrative_ontology:measurement(zion_su_t1964, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1964, 0.85).
narrative_ontology:measurement(zion_su_t1992, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1992, 0.9).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, gaza_blockade).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, west_bank_settlement_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'zionist_legitimacy_basis' kernel, alongside 'national_liberation_reading' and 'religious_restoration_reading'. Each reading offers a distinct structural analysis of Zionism's core nature and legitimacy, leading to different ε values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
