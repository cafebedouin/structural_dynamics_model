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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Jewish Sovereignty in Palestine: Settler-Colonial Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'settler_colonial_reading' of
 *   Jewish sovereignty in Palestine. It posits that Zionism, regardless of
 *   the intentions of individual immigrants, functions as a European
 *   settler-colonial project, leading to the systematic displacement and
 *   dispossession of the indigenous Palestinian population. The constraint's
 *   persistence relies on active enforcement and the suppression of
 *   Palestinian resistance, with identifiable beneficiaries (Israeli state,
 *   Jewish settlers, supporting metropole powers) and victims (Palestinians).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.85).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Jewish Sovereignty in Palestine: Settler-Colonial Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '33ce51a7-9c89-4fee-bc37-8253321b58c8').
narrative_ontology:cs_kernel_codification('33ce51a7-9c89-4fee-bc37-8253321b58c8', formalized).
narrative_ontology:cs_authority_grounding('33ce51a7-9c89-4fee-bc37-8253321b58c8', extraction).
narrative_ontology:cs_interpretation_layer_present('33ce51a7-9c89-4fee-bc37-8253321b58c8').
narrative_ontology:cs_reading_relation('33ce51a7-9c89-4fee-bc37-8253321b58c8', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('33ce51a7-9c89-4fee-bc37-8253321b58c8', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('33ce51a7-9c89-4fee-bc37-8253321b58c8', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('33ce51a7-9c89-4fee-bc37-8253321b58c8', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('33ce51a7-9c89-4fee-bc37-8253321b58c8', foundational, zionism_as_settler_colonial_project).
narrative_ontology:cs_axiom_status(zionism_as_settler_colonial_project, holdable).
narrative_ontology:cs_axiom_grounding('33ce51a7-9c89-4fee-bc37-8253321b58c8', zionism_as_settler_colonial_project, empirically_contingent).
narrative_ontology:cs_axiom('33ce51a7-9c89-4fee-bc37-8253321b58c8', foundational, indigenous_dispossession_is_primary).
narrative_ontology:cs_axiom_status(indigenous_dispossession_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('33ce51a7-9c89-4fee-bc37-8253321b58c8', indigenous_dispossession_is_primary, deontological).
narrative_ontology:cs_reference_frame('33ce51a7-9c89-4fee-bc37-8253321b58c8', european_colonial_expansion_pattern).
narrative_ontology:cs_drift_state('33ce51a7-9c89-4fee-bc37-8253321b58c8', contemporary_anti_colonial_resistance, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('33ce51a7-9c89-4fee-bc37-8253321b58c8', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_powers).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_settlers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of dispossession, displacement, and violence. Their land, resources, and self-determination are systematically denied. Exit is forced exile or life under occupation, with no viable alternative to the existing regime.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinians, payer,
    powerless, generational, trapped, regional).

% Benefit from the acquisition of land and resources, and the establishment of a state that prioritizes their security and national identity. Their immigration, regardless of individual intent or refugee status, contributes to the displacement regime. Exit means abandoning the state project or facing insecurity, often tied to a strong national identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_settlers, beneficiary,
    moderate, biographical, identity_locked, global).

% Administers and enforces the policies of land acquisition, settlement expansion, and control over Palestinian life. It benefits from the consolidation of sovereignty and resources, and maintains its legitimacy through a narrative of national self-determination and security, while actively suppressing alternatives.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Historically, Britain facilitated the initial colonial project. Currently, powers like the U.S. provide diplomatic, military, and economic support, benefiting from regional stability aligned with their strategic interests, and from the perpetuation of a client state in the region.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Advocate for Palestinian rights and against the settler-colonial framework. They challenge the legitimacy of the constraint through advocacy, boycotts, and legal efforts, but lack direct power to alter its operation, acting primarily as external critics.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_solidarity_movements, observer,
    organized, generational, mobile, global).

% While often critical of specific Israeli policies, their foundational commitment to a Jewish state in Palestine prevents them from fully acknowledging the settler-colonial nature of the project as defined by this reading. Their arguments are seen as legitimizing the underlying structure, thus excluding them from this reading's anti-colonial framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, liberal_zionists, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint does not solve a genuine coordination problem for all parties. Instead, it coordinates the acquisition and control of land and resources for the benefit of the settler population and the state apparatus, at the expense of the indigenous population.
% TRANSFER_FUNCTION: Transfers land, water, natural resources, and sovereignty from the indigenous Palestinian population to the Jewish settler population and the Israeli state. It also transfers the burden of security and maintenance of the colonial project to the metropole powers.
% ABSENT_VOICES: The voices of dispossessed Palestinians, particularly those in exile or under occupation, whose narratives of displacement and resistance are systematically marginalized or suppressed within the dominant discourse. Also, anti-colonial theorists and indigenous rights advocates whose frameworks are dismissed as biased.
% DISAPPEARANCE_RATIONALE: If the settler-colonial framework vanished overnight, the entire structure of land ownership, citizenship, and political power would collapse. Palestinians would reclaim their land and sovereignty, the Israeli state as currently constituted would cease to exist, and regional power dynamics would fundamentally shift.
% FOUNDING_PROBLEM: The problem of European antisemitism and the desire for Jewish self-determination in a historical homeland, which this reading argues was addressed by externalizing the problem onto an indigenous population through a colonial project.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian historians, postcolonial scholars, and international human rights organizations consistently corroborate this framing, often citing historical documents, land records, and testimonies of displacement. This corroboration comes from outside the benefiting parties (Israeli state, Jewish settlers, metropole powers).
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the ongoing transfer of land, resources, and sovereignty from Palestinians. Suppression is very high (0.9) reflecting the military occupation, legal frameworks, and physical barriers used to maintain control and prevent Palestinian self-determination. Theater ratio is moderate (0.45) as claims of self-defense, historical right, and security are used to justify actions that, from this reading, primarily serve to consolidate colonial control. Accessibility collapse is high (0.9) for Palestinians, as viable alternatives for self-determination and return are systematically foreclosed. Resistance is high (0.8) reflecting continuous Palestinian struggle against the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and many Jewish settlers, the constraint is a legitimate exercise of national self-determination and a response to historical persecution. From the Palestinian perspective, and this settler-colonial reading, it is a structure of ongoing dispossession and oppression. The engine's classification will highlight this divergence by computing a Snare from the structural data, contrasting with potential self-justifying claims of a Rope or even Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinians are the primary targets (payers) of this constraint, bearing the costs of dispossession and violence. Jewish immigrants/settlers are beneficiaries, gaining land and security within the established system. The Israeli state apparatus is the agenda-setter and primary beneficiary, actively enforcing the constraint. Colonial metropole powers are also beneficiaries, gaining strategic influence and stability. International solidarity movements are observers, while liberal Zionists are excluded from this reading's analytical framework due to their foundational commitments.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_intent_vs_structural_outcome,
    'Does the individual intent or refugee status of Jewish immigrants alter the structural settler-colonial nature of their collective presence and its impact on indigenous Palestinians?',
    'Analysis of historical and contemporary policies regarding land acquisition, citizenship, and population transfer, independent of individual motivations. If policies consistently result in indigenous dispossession, individual intent is secondary to structural outcome.',
    'If individual intent is deemed irrelevant to structural outcome, the settler-colonial classification is strengthened, emphasizing systemic rather than individual agency. If intent is given significant weight, it might soften the classification towards a more complex, less purely extractive type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_intent_vs_structural_outcome, conceptual, 'Distinction between individual motivations and systemic effects in settler-colonialism.').

omega_variable(
    metropole_role_evolution,
    'How has the role of external colonial metropole powers (e.g., Britain, later U.S.) evolved from direct facilitation to indirect support, and what is the current extent of their structural benefit from the constraint''s persistence?',
    'Historical analysis of diplomatic, military, and economic aid, voting patterns in international bodies, and strategic interests in the region. Quantify the material and political support provided by external powers over time.',
    'If the metropole''s benefit is substantial and ongoing, it reinforces the ''colonial'' aspect of the settler-colonial reading, highlighting external perpetuation of the constraint. If support is found to be negligible or purely humanitarian, it would weaken this aspect of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_role_evolution, empirical, 'The evolving role and benefit of external powers in perpetuating the settler-colonial structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1963, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1963, 0.28).
narrative_ontology:measurement(jewi_tr_t1978, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(jewi_tr_t2008, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2008, 0.43).
narrative_ontology:measurement(jewi_tr_t2023, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement(jewi_be_t1963, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1963, 0.72).
narrative_ontology:measurement(jewi_be_t1978, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1978, 0.78).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.81).
narrative_ontology:measurement(jewi_be_t2008, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(jewi_be_t2023, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1963, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1963, 0.78).
narrative_ontology:measurement(jewi_su_t1978, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1978, 0.83).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.86).
narrative_ontology:measurement(jewi_su_t2008, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2008, 0.88).
narrative_ontology:measurement(jewi_su_t2023, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'jewish_sovereignty_palestine' kernel, each representing a distinct structural claim. This specific reading focuses on the settler-colonial interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
