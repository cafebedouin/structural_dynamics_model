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
 *   This constraint models the claim that Zionism, as implemented,
 *   instantiates a European settler-colonial pattern, where Jewish
 *   immigration and the establishment of a Jewish-majority state constitute a
 *   displacement regime for indigenous Palestinians, regardless of the intent
 *   of individual immigrants. The constraint's persistence relies on active
 *   enforcement and suppression of Palestinian resistance, with significant
 *   extraction of land and sovereignty. This is one reading of the 'Jewish
 *   Sovereignty in Palestine' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.9).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.95).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine: Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0').
narrative_ontology:cs_kernel_codification('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', formalized).
narrative_ontology:cs_authority_grounding('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', extraction).
narrative_ontology:cs_interpretation_layer_present('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0').
narrative_ontology:cs_reading_relation('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', foundational, zionism_as_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', zionism_as_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', foundational, indigenous_rights_to_self_determination).
narrative_ontology:cs_axiom_status(indigenous_rights_to_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', indigenous_rights_to_self_determination, deontological).
narrative_ontology:cs_reference_frame('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', pre_zionist_indigenous_sovereignty).
narrative_ontology:cs_drift_state('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0b0d4ffc-6601-4e9c-bb9d-51c2f0fc49a0', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_powers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, indigenous_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience ongoing dispossession of land, resources, and self-determination. Their presence is framed as an obstacle to the settler project, leading to displacement, military occupation, and denial of rights. Exit means abandoning ancestral lands and identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, indigenous_palestinians, payer,
    powerless, generational, trapped, local).

% Regardless of individual intent or refugee status, their immigration is seen as part of a larger structural process of settlement and displacement. They benefit from the infrastructure and security provided by the settler-colonial state, often at the expense of the indigenous population.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_settlers, beneficiary,
    organized, generational, mobile, regional).

% Administers and enforces policies that facilitate Jewish settlement and maintain control over Palestinian territories. It benefits from the territorial expansion and consolidation of power, acting as the primary agent of the settler-colonial project.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Historically, Britain facilitated the initial stages of the project. Currently, powers like the United States provide diplomatic, military, and economic support, benefiting from regional influence and strategic alliances, while externalizing the costs of conflict.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Advocate for Palestinian rights and challenge the settler-colonial framing of Zionism. They seek to expose the structural injustices and mobilize international pressure for decolonization and self-determination.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_solidarity_movements, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the establishment and maintenance of a Jewish-majority state in Palestine by facilitating Jewish immigration, land acquisition, and the development of state institutions, while managing the indigenous population.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from indigenous Palestinians to Jewish settlers and the Israeli state apparatus, supported by colonial metropole powers.
% ABSENT_VOICES: The voices of displaced and exiled Palestinians, whose narratives of dispossession are systematically marginalized or denied within the dominant discourse, are absent from the power structures maintaining the constraint.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework vanished, the entire political, demographic, and territorial arrangement would fundamentally shift. Land ownership, citizenship rights, and the distribution of power would be radically reconfigured, leading to a decolonized state or states.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and persecution in Europe, and the desire for a national homeland, was addressed through a project that, from this reading, inherently involved colonial expansion into an already inhabited land.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem of Jewish persecution is widely acknowledged. However, its resolution through a settler-colonial project is attested by postcolonial scholars, Palestinian historians, and international human rights organizations, who provide extensive documentation of land expropriation, displacement, and the imposition of a racialized hierarchy, corroborating the claim that the solution itself created a new problem of dispossession.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).

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
 *   Extractiveness is very high (0.9) due to the zero-sum territorial logic of settler-colonialism, where the gains of one group directly correspond to the losses of the indigenous population. Suppression is also very high (0.95) as the project requires continuous military, legal, and demographic control to overcome indigenous resistance and maintain the displacement regime. Theater ratio is low (0.1) because the state's actions are primarily functional in achieving and maintaining colonial control, with little performative cover for non-existent coordination. Accessibility collapse is high (0.8) for Palestinians, as alternatives to dispossession are systematically foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of indigenous Palestinians, the constraint is a clear snare of dispossession. From the perspective of Jewish immigrants (as settlers), it is a system that provides security and belonging, though this reading argues that this benefit is structurally tied to the dispossession of others. The Israeli state and its metropole backers frame it as a legitimate exercise of national self-determination, a claim this reading contests.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous Palestinians are the primary targets and victims, experiencing maximal extraction and suppression (d=1.0). Jewish immigrants, regardless of their personal circumstances, are structurally positioned as beneficiaries of the settler-colonial project (d closer to 0.0). The Israeli state apparatus is the agenda-setter and primary beneficiary, actively enforcing the constraint. Colonial metropole powers are also beneficiaries, gaining strategic influence. International solidarity movements act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_framing_validity,
    'Is the settler-colonial framework the most accurate and comprehensive lens through which to understand the historical and ongoing dynamics of Jewish sovereignty in Palestine?',
    'Comparative historical analysis with other settler-colonial contexts (e.g., Australia, South Africa, US) focusing on land acquisition, demographic engineering, and indigenous resistance, alongside critical examination of alternative frameworks (e.g., post-Holocaust refuge, national liberation).',
    'If validated, it strengthens the classification as a snare and highlights the structural nature of Palestinian victimhood. If alternative frameworks are found more fitting, the extractiveness and suppression metrics might be re-evaluated under a different lens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_framing_validity, conceptual, 'The appropriateness of the settler-colonial framework for this context.').

omega_variable(
    intent_vs_outcome_dissonance,
    'To what extent does the individual intent of Jewish immigrants (e.g., seeking refuge from persecution) mitigate or alter the structural outcome of displacement for indigenous Palestinians?',
    'Sociological studies examining the lived experiences and motivations of immigrants versus the documented effects of their collective presence on indigenous populations, and legal analysis of how individual rights claims interact with collective historical injustices.',
    'This reading asserts that structural outcomes override individual intent in defining the colonial pattern. If intent were found to significantly alter the structural classification, the ''beneficiary'' role for Jewish immigrants might be nuanced, though the ''payer'' role for Palestinians would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_outcome_dissonance, conceptual, 'The role of individual intent versus structural outcome in defining settler-colonialism.').

omega_variable(
    role_of_external_powers,
    'What is the precise degree of ongoing material and political support from ''colonial_metropole_powers'' (e.g., the US) that sustains the settler-colonial pattern, and how would its withdrawal impact the constraint?',
    'Geopolitical analysis of aid packages, military support, diplomatic interventions, and UN Security Council vetoes, combined with counterfactual modeling of scenarios where such support is significantly reduced or withdrawn.',
    'If external support is found to be a critical enabler, it reinforces the ''snare'' classification and the ''beneficiary'' role of these powers. Its withdrawal would likely lead to a significant reduction in the constraint''s extractiveness and suppression, potentially shifting its type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(role_of_external_powers, empirical, 'The extent to which external powers sustain the settler-colonial dynamic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.6).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.5).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.92).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'Jewish Sovereignty in Palestine' kernel. Its high extractiveness and suppression metrics reflect a structural analysis of dispossession, contrasting with readings that emphasize national liberation or cultural revival.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
