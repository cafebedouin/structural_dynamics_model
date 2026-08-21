% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Jewish Sovereignty in Palestine: Settler-Colonial Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'settler-colonial reading' of
 *   Jewish sovereignty in Palestine. It posits that Zionism, regardless of
 *   its initial motivations, structurally functions as a European
 *   settler-colonial project, leading to the systematic displacement and
 *   dispossession of the indigenous Palestinian population. Jewish
 *   immigration, even when driven by genuine refugee crises, is viewed as
 *   constituting a displacement regime. The primary beneficiaries are the
 *   Israeli state apparatus and global imperial powers, while the Palestinian
 *   indigenous population are the primary victims of high extraction and
 *   suppression. The claimed type is 'snare' due to the high extraction,
 *   active enforcement, and identifiable victims, despite the 'founding
 *   problem' being contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.95).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine: Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '20a29af0-a430-4dab-a409-025bf643547d').
narrative_ontology:cs_kernel_codification('20a29af0-a430-4dab-a409-025bf643547d', formalized).
narrative_ontology:cs_authority_grounding('20a29af0-a430-4dab-a409-025bf643547d', extraction).
narrative_ontology:cs_interpretation_layer_present('20a29af0-a430-4dab-a409-025bf643547d').
narrative_ontology:cs_reading_relation('20a29af0-a430-4dab-a409-025bf643547d', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20a29af0-a430-4dab-a409-025bf643547d', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20a29af0-a430-4dab-a409-025bf643547d', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20a29af0-a430-4dab-a409-025bf643547d', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('20a29af0-a430-4dab-a409-025bf643547d', foundational, zionism_as_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('20a29af0-a430-4dab-a409-025bf643547d', zionism_as_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('20a29af0-a430-4dab-a409-025bf643547d', foundational, indigenous_dispossession_as_primary_outcome).
narrative_ontology:cs_axiom_status(indigenous_dispossession_as_primary_outcome, holdable).
narrative_ontology:cs_axiom_grounding('20a29af0-a430-4dab-a409-025bf643547d', indigenous_dispossession_as_primary_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('20a29af0-a430-4dab-a409-025bf643547d', european_colonial_expansion_pattern).
narrative_ontology:cs_drift_state('20a29af0-a430-4dab-a409-025bf643547d', contemporary_postcolonial_critique, gap(stable, minor, false)).
narrative_ontology:cs_created_at('20a29af0-a430-4dab-a409-025bf643547d', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, global_imperial_powers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary victims of structural dispossession, land confiscation, and displacement. Their national and individual rights are systematically denied, and their existence is framed as an obstacle to the settler-colonial project. Exit means forced exile or internal displacement, often with loss of identity and connection to land.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, local).

% Regardless of their individual intent or refugee status, they are positioned as beneficiaries of the settler-colonial project, occupying land and resources from which the indigenous population has been dispossessed. Their presence reinforces the displacement regime. Exit means leaving the colonial project, which may entail significant personal cost and identity crisis.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_settlers, beneficiary,
    moderate, biographical, constrained, local).

% The primary agent enforcing the displacement regime, managing land allocation, military occupation, and legal frameworks that privilege Jewish citizens over Palestinians. It benefits directly from territorial expansion and control. Exit is not a meaningful option for the state itself, as its existence is predicated on this structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Historically, Britain as the mandatory power, and later the United States, benefit from the geopolitical stability and strategic positioning offered by the Israeli state as an outpost of Western interests in the region. They provide diplomatic, military, and economic support that sustains the settler-colonial project. Their exit options are broad, allowing them to shift support or influence as geopolitical interests change.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, global_imperial_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Document and condemn human rights violations and dispossession, advocating for Palestinian rights and an end to the occupation. They operate from an analytical distance, seeking to influence global public opinion and international law. Their 'exit' is to disengage from the issue, but their mission compels engagement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of the settler-colonial project, it coordinates the establishment and maintenance of a new society on appropriated land, including infrastructure, security, and governance for the settler population.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from the indigenous Palestinian population to the Jewish settler population and the Israeli state apparatus. It also transfers geopolitical influence and strategic advantage to global imperial powers.
% ABSENT_VOICES: The voices of the dispossessed Palestinian population are systematically marginalized or silenced within the dominant narratives and political structures that legitimize the settler-colonial project. Their historical claims, narratives of resistance, and proposals for decolonization are actively suppressed.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework vanished overnight, the entire geopolitical structure of the region would fundamentally rearrange. Land ownership, citizenship rights, and power dynamics would be inverted, leading to a radical reordering of society, economy, and international relations. The Israeli state as currently constituted would cease to exist, and Palestinian self-determination would become the dominant force.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and persecution in Europe, and the desire for national self-determination in an ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: While the historical problem of Jewish persecution is widely acknowledged, this reading argues that the 'solution' adopted (settler-colonialism) created a new, equally severe problem for Palestinians. Corroboration for the 'contested' status comes from postcolonial scholars, Palestinian historians, and international legal bodies who document the ongoing dispossession and challenge the legitimacy of the founding narrative as a justification for colonial practices.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.95) because the constraint operates on a zero-sum territorial logic where the establishment and expansion of one group's sovereignty directly entails the dispossession of another. Suppression is also very high (0.90) due to the active military occupation, legal discrimination, and physical barriers (e.g., checkpoints, separation wall) required to maintain the displacement regime against indigenous resistance. Theater ratio is low (0.10) because the primary function is direct control and extraction, with minimal performative cover; the 'security' justifications are seen as directly serving the colonial project rather than being a separate, performative layer. Accessibility collapse is high (0.85) as alternatives for Palestinians (e.g., self-determination, return) are systematically foreclosed by the state's actions. Resistance is high (0.80) reflecting ongoing Palestinian struggle against the occupation.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading fundamentally diverges from other readings by framing Jewish immigration and state-building as inherently extractive and suppressive for Palestinians, rather than as a legitimate exercise of national self-determination or a response to persecution. This leads to a classification of 'snare' for the settler-colonial reading, whereas other readings might classify the same historical events as 'rope' or even 'mountain' (e.g., divine right). The engine's computation of per-seat classification will reflect this: for Palestinians, it is a snare; for the Israeli state, it is a self-sustaining, highly beneficial structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state apparatus (agenda_setter) and global imperial powers (beneficiary) are positioned at the beneficiary end, as they directly gain from the geopolitical and territorial outcomes. Jewish immigrants/settlers are also beneficiaries, as their presence is structurally enabled by and contributes to the displacement regime, regardless of individual intent. The Palestinian indigenous population are unequivocally at the target end, bearing the full cost of dispossession and suppression. International human rights advocates are observers, analyzing the situation without direct participation in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate (solving Jewish statelessness) has been superseded by the structural reality of settler-colonialism. The persistence of the constraint is not due to an ongoing coordination problem for all parties, but rather the active maintenance of an extractive regime. The classification as 'snare' prevents mislabeling this as a coordination mechanism, highlighting the coercive and victimizing aspects. The 'contested' status of the founding problem further supports this, indicating that the original justification no longer holds universally and is used as cover for ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure_ambiguity,
    'Does the individual intent of Jewish immigrants (e.g., seeking refuge from persecution) alter the structural classification of their collective presence as part of a settler-colonial project?',
    'Analysis of the legal and material outcomes of immigration, independent of individual motivations: if outcomes consistently lead to dispossession of indigenous populations, the structural classification holds regardless of intent.',
    'If intent were to alter classification, the ''jewish_immigrants_settlers'' seat''s directionality might shift towards beneficiary of a ''rope'' or ''scaffold'', rather than a ''snare''. This reading asserts that structural outcomes override individual intent in determining the nature of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_structure_ambiguity, conceptual, 'Ambiguity regarding the role of individual intent versus structural outcome in defining settler-colonialism.').

omega_variable(
    metropole_beneficiary_ambiguity,
    'To what extent do contemporary global imperial powers (e.g., the United States) actively benefit from and sustain the settler-colonial project, versus merely tolerating it?',
    'Detailed analysis of military aid, diplomatic support, UN vetoes, and economic investments, tracing direct and indirect benefits to the imperial power''s strategic interests.',
    'If benefits are found to be negligible or merely passive, the ''global_imperial_powers'' seat''s directionality would shift towards ''observer'' or ''constrained beneficiary'', reducing the overall ''snare'' character of the constraint. This reading asserts active, material benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_beneficiary_ambiguity, empirical, 'Uncertainty regarding the degree of active imperial benefit from the settler-colonial project.').

omega_variable(
    decolonization_pathways_feasibility,
    'What are the feasible pathways for decolonization and the establishment of Palestinian self-determination, and what are the structural barriers to their implementation?',
    'Comparative analysis of historical decolonization processes, assessment of current political and military power balances, and evaluation of proposed solutions (e.g., one-state, two-state, confederation) against the settler-colonial framework.',
    'If decolonization pathways are deemed structurally impossible within the existing framework, it reinforces the ''snare'' classification by highlighting the lack of viable exit for victims. If pathways are found to be feasible but suppressed, it emphasizes the ''suppression'' metric and the need for external intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decolonization_pathways_feasibility, preference, 'Feasibility and structural barriers to decolonization pathways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.92).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2014, 0.94).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2014, 0.88).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
