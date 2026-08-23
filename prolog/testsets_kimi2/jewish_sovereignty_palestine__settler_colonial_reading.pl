% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Jewish Sovereignty in Palestine: Settler-Colonial Reading
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of Jewish
 *   sovereignty in Palestine. From this analytical seat, Zionism is not
 *   primarily a national-liberation movement or a response to European
 *   antisemitism, but a structurally instantiated European settler-colonial
 *   project. Jewish immigration functions as a displacement mechanism
 *   regardless of refugee intent; the primary victims are the indigenous
 *   Palestinian Arab population, denied return, sovereignty, and equal
 *   territorial claim. The beneficiaries include the Jewish settler
 *   population (which receives land, state infrastructure, and political
 *   supremacy), the Israeli state apparatus (which administers the
 *   exclusionary regime), and the colonial metropole (initially Britain,
 *   subsequently the United States, which gains regional military-proxy and
 *   geopolitical alignment benefits). The coordination functionâorganizing
 *   Jewish settlement, defense, and state-buildingâoperates through the
 *   same institutional machinery that extracts land and political rights from
 *   Palestinians, making the constraint a hybrid of coordination and
 *   extraction rather than pure extraction or pure coordination.
 *
 * KEY AGENTS:
 *   - palestinian_arab_population: Primary target (powerless/trapped) â bears dispossession, occupation, and exclusion from return.
 *   - palestinian_refugee_diaspora: Secondary target (powerless/trapped) â excluded from political deliberation and physical return.
 *   - jewish_settler_population: Coordinated beneficiary (organized/identity_locked) â receives territorial and material benefits from the regime.
 *   - israeli_state_apparatus: Agenda-setter (institutional/constrained) â administers enforcement, settlement expansion, and differentiated citizenship.
 *   - colonial_metropole: External beneficiary (powerful/arbitrage) â extracts geopolitical rents and strategic alignment.
 *   - postcolonial_scholar_analyst: Analytical observer (analytical/analytical) â provides comparative settler-colonial framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine: Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '3709b1aa-19b1-48ed-a974-422ac884e4c3').
narrative_ontology:cs_kernel_codification('3709b1aa-19b1-48ed-a974-422ac884e4c3', fixed_text).
narrative_ontology:cs_authority_grounding('3709b1aa-19b1-48ed-a974-422ac884e4c3', lineage).
narrative_ontology:cs_interpretation_layer_present('3709b1aa-19b1-48ed-a974-422ac884e4c3').
narrative_ontology:cs_reading_relation('3709b1aa-19b1-48ed-a974-422ac884e4c3', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3709b1aa-19b1-48ed-a974-422ac884e4c3', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3709b1aa-19b1-48ed-a974-422ac884e4c3', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3709b1aa-19b1-48ed-a974-422ac884e4c3', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('3709b1aa-19b1-48ed-a974-422ac884e4c3', foundational, territorial_supremacy_via_dispossession).
narrative_ontology:cs_axiom_status(territorial_supremacy_via_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('3709b1aa-19b1-48ed-a974-422ac884e4c3', territorial_supremacy_via_dispossession, empirically_contingent).
narrative_ontology:cs_axiom('3709b1aa-19b1-48ed-a974-422ac884e4c3', foundational, colonial_intent_irrelevance).
narrative_ontology:cs_axiom_status(colonial_intent_irrelevance, holdable).
narrative_ontology:cs_axiom_grounding('3709b1aa-19b1-48ed-a974-422ac884e4c3', colonial_intent_irrelevance, conventional).
narrative_ontology:cs_reference_frame('3709b1aa-19b1-48ed-a974-422ac884e4c3', settler_colonial_territorial_regime).
narrative_ontology:cs_drift_state('3709b1aa-19b1-48ed-a974-422ac884e4c3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3709b1aa-19b1-48ed-a974-422ac884e4c3', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Indigenous population of Palestine displaced from lands and political sovereignty through successive waves of Zionist immigration and state-building. Those remaining under Israeli control are subject to military occupation, siege, or second-class citizenship. Refugees and their descendants are denied return and restitution. Exit to sovereignty or full territorial recovery is structurally blocked by the regime.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Descendants of the 1948 Nakba dispersed in camps and host countries across the Middle East. Hold legal right of return under UN Resolution 194 but are systematically excluded from political deliberations over the territory's future. Their physical presence is barred by the regime while their claims are delegitimized as security threats.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora, excluded).

% Receives subsidized housing, infrastructure, military protection, and preferential land allocation from the Israeli state. Benefits from the exclusion of Palestinian property claims and refugee return. Emigration is physically possible but identity-costly due to ideological fusion with the Zionist project and institutional embedding in the territorial regime.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_population, beneficiary,
    organized, biographical, identity_locked, national).

% Administrates the sovereign regime through military, legal, and bureaucratic institutions. Enforces territorial exclusion of Palestinian refugees, expands settlements in occupied territories, and manages differentiated citizenship and residency regimes to preserve Jewish demographic supremacy. Institutional survival depends on maintaining the exclusionary territorial framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Initially Britain as Mandatory power facilitating Jewish immigration under the Balfour Declaration; subsequently the United States as primary diplomatic, military, and economic sponsor. Benefits from regional military proxy, intelligence cooperation, and ideological alignment. Support level fluctuates with imperial strategic interests rather than moral commitment to either party.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole, beneficiary,
    powerful, generational, arbitrage, global).

% Analyzes the regime through the lens of comparative settler-colonial studies, identifying structural parallels with South Africa, Australia, and North America. Does not participate in the constraint's operation but provides the conceptual vocabulary for identifying its mechanics and situating it within global colonial history.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, postcolonial_scholar_analyst, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish immigration, settlement, military defense, and state-building in Palestine under European imperial sponsorship, organizing a settler society with territorial, economic, and political infrastructure that supplants the indigenous population.
% TRANSFER_FUNCTION: Transfers land, water resources, political sovereignty, and demographic majority from the indigenous Palestinian Arab population to the Jewish settler society and allied imperial interests.
% ABSENT_VOICES: Palestinian refugees and the diaspora are structurally excluded from political deliberation over the territory's future; their claims to return and self-determination are delegitimized in international forums that recognize the regime. Anti-Zionist Jewish voices and Palestinian citizens of Israel advocating for a secular democratic state are marginalized within domestic political institutions.
% DISAPPEARANCE_RATIONALE: If the settler-colonial sovereignty regime disappeared overnight, Palestinian refugees would reclaim property and political rights, the Jewish settler society would lose its exclusive territorial foundation and associated state privileges, and regional geopolitical alignments would fundamentally shift. The arrangements organized around Jewish demographic supremacy would unravel.
% FOUNDING_PROBLEM: European antisemitism and the crisis of Jewish statelessness in the late 19th and early 20th centuries; concurrent British imperial interest in securing a Suez-adjacent client presence and geopolitical foothold in the Eastern Mediterranean.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest to the ongoing need for a Jewish refuge and territorial haven. Palestinian historians, postcolonial scholars, and critical international-relations analysts attest that the founding crisis of Jewish statelessness has been resolved by the establishment of the state itself, while the imperial military-interest framing is corroborated by British diplomatic archives and scholarship outside the benefiting parties.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is high because the territorial logic is zero-sum: Jewish demographic and territorial supremacy is structurally incompatible with Palestinian refugee return, full restitution, and equal sovereignty. Suppression (0.82) is high because the regime's persistence depends on actively suppressing Palestinian political organization, armed resistance, and international legal claims, as well as suppressing internal dissent within the Jewish population that might challenge the exclusionary framework. Theater ratio (0.45) reflects substantial performative activityâdemocratic claims, security framings, peace-process negotiations, and temporary-occupation legal fictionsâthat mask the steady expansion of territorial facts. Accessibility collapse (0.78) is high because once the settler-colonial structure is understood, the available alternatives (liberal Zionism, two-state solution, civic nationalism within current borders) collapse as structurally incompatible with the regime's territorial supremacy. Resistance (0.75) is high due to sustained Palestinian armed and unarmed resistance, international solidarity movements, and growing legal challenges at the ICJ and ICC. The temporal series show accelerating extraction and suppression from the Mandate period through the Nakba, the 1967 occupation, and the contemporary siege and bombardment of Gaza, with theater rising as international legitimacy erodes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (Israeli state apparatus, Jewish settler population, colonial metropole) experience the constraint as a legitimate security and nation-building project, or as a strategic asset. From these seats, the coordination functionâorganizing Jewish collective life and regional allianceâis foregrounded and the extraction from Palestinians is backgrounded or justified. From the payer seats (Palestinian Arab population, refugee diaspora), the identical structure is experienced as dispossession, fragmentation, and elimination. The engine computes this divergence from the structural data: identical constraint, opposite directionality. The colonial metropole seat adds a third perspective: the constraint is experienced as a low-cost, high-yield geopolitical proxy whose domestic suppression costs are borne locally.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality (d approaching 0): the Jewish settler population receives subsidized housing, military protection, and sovereign political rights; the Israeli state apparatus receives tax revenue and institutional continuity; the colonial metropole receives strategic alliance benefits. Victim declarations map to high directionality (d approaching 1): the Palestinian Arab population loses land, political sovereignty, and freedom of movement; the refugee diaspora loses the right of return and restitution. The settler population's exit is identity_lockedâemigration is physically possible but ideologically and materially costlyâkeeping d low but not at the absolute beneficiary floor. Palestinian exit is trappedârefugees are physically barred from return, and those under occupation are encircledâpushing d toward the full-target ceiling.
 *
 * MANDATROPHY ANALYSIS:
 *   The settler-colonial reading prevents mislabeling the coordination function (Jewish refugee absorption, state-building, military defense) as proof that the constraint is merely a Rope. The coordination is realâit organizes a societyâbut it is inseparable from the extraction it requires. Without the extraction (Palestinian dispossession), the coordination could not have produced a Jewish-majority state in Palestine. The Tangled Rope classification captures this inseparability. It also prevents the opposite error of classifying the constraint as a pure Snare: the Jewish settler population genuinely benefits from the coordination function (it is not merely a cover story for them), and their commitment to the regime is not purely coerced. The classification therefore rests on the structural fact that coordination and extraction operate through the same institutional machinery, not on the subjective intent of any party.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_metropole_vs_settler_autonomy,
    'Does the contemporary Israeli state operate as an autonomous settler-colonial agenda-setter, or does it remain subordinate to U.S. imperial interests as primary beneficiary?',
    'Comparative analysis of military-aid conditionality, diplomatic veto patterns, and private capital flows to determine who captures the greater surplus from the territorial regime.',
    'If the metropole remains primary beneficiary, the constraint aligns with classical colonial extraction; if settler autonomy is high, extraction is captured locally and the directionality shifts toward the settler state itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_metropole_vs_settler_autonomy, empirical, 'Whether the colonial metropole or the settler state is the primary beneficiary seat').

omega_variable(
    intent_structural_irrelevance,
    'Does the refugee-origin intent of Jewish immigration alter the structural classification of the dispossession mechanism, or is the outcome invariant to intent?',
    'Comparative case studies of other refugee-settler movements to test whether refugee intent correlates with different structural outcomes for indigenous populations.',
    'If intent-variant outcomes exist, the regardless-of-intent axiom is domain-limited rather than universal, complicating the settler-colonial reading for populations fleeing persecution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intent_structural_irrelevance, conceptual, 'Whether refugee intent is structurally irrelevant to colonial classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 0, 104).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_scr_tr_t0, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jsp_scr_tr_t28, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 28, 0.35).
narrative_ontology:measurement(jsp_scr_tr_t49, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 49, 0.4).
narrative_ontology:measurement(jsp_scr_tr_t67, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 67, 0.45).
narrative_ontology:measurement(jsp_scr_tr_t83, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 83, 0.5).
narrative_ontology:measurement(jsp_scr_tr_t104, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 104, 0.55).

% Extraction over time
narrative_ontology:measurement(jsp_scr_be_t0, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(jsp_scr_be_t28, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 28, 0.65).
narrative_ontology:measurement(jsp_scr_be_t49, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 49, 0.75).
narrative_ontology:measurement(jsp_scr_be_t67, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 67, 0.78).
narrative_ontology:measurement(jsp_scr_be_t83, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 83, 0.82).
narrative_ontology:measurement(jsp_scr_be_t104, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 104, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jsp_scr_su_t0, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jsp_scr_su_t28, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 28, 0.7).
narrative_ontology:measurement(jsp_scr_su_t49, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 49, 0.78).
narrative_ontology:measurement(jsp_scr_su_t67, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 67, 0.8).
narrative_ontology:measurement(jsp_scr_su_t83, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 83, 0.82).
narrative_ontology:measurement(jsp_scr_su_t104, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 104, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jewish_sovereignty_palestine. The kernel decomposes into multiple structurally distinct claims about Jewish sovereignty in Palestine. This reading isolates the settler-colonial structural claim; other readings instantiate liberal-nationalist, religious, cultural, and post-Zionist interpretations. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
