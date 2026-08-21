% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Jewish Sovereignty in Palestine (Post-Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents a 'post-Zionist' reading of Jewish sovereignty
 *   in Palestine, where the achievement of statehood, while fulfilling a
 *   national aspiration, has evolved into an ethnic-national framework that
 *   actively obstructs civic equality for non-Jewish citizens and prevents
 *   regional integration. The constraint's persistence relies on active
 *   enforcement of discriminatory laws and policies, leading to high
 *   extraction from Palestinian populations. This reading views the state's
 *   founding narrative as a cover for ongoing settler-colonial practices and
 *   ethnic privilege.
 *
 * KEY AGENTS:
 *   - jewish_citizens_of_israel: Primary beneficiary (institutional/mobile)
 *   - palestinian_citizens_of_israel: Primary payer (organized/identity_locked)
 *   - occupied_palestinian_populations: Primary payer (powerless/trapped)
 *   - israeli_state_institutions: Agenda setter (institutional/constrained)
 *   - regional_integration_advocates: Excluded (moderate/constrained)
 *   - international_human_rights_organizations: Observer (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.78).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.85).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Sovereignty in Palestine (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '60a0cd20-eabc-4045-bebc-d27cfdc26869').
narrative_ontology:cs_kernel_codification('60a0cd20-eabc-4045-bebc-d27cfdc26869', fixed_text).
narrative_ontology:cs_authority_grounding('60a0cd20-eabc-4045-bebc-d27cfdc26869', extraction).
narrative_ontology:cs_interpretation_layer_present('60a0cd20-eabc-4045-bebc-d27cfdc26869').
narrative_ontology:cs_reading_relation('60a0cd20-eabc-4045-bebc-d27cfdc26869', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('60a0cd20-eabc-4045-bebc-d27cfdc26869', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('60a0cd20-eabc-4045-bebc-d27cfdc26869', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('60a0cd20-eabc-4045-bebc-d27cfdc26869', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('60a0cd20-eabc-4045-bebc-d27cfdc26869', foundational, ethnic_national_framework_obstructs_civic_equality).
narrative_ontology:cs_axiom_status(ethnic_national_framework_obstructs_civic_equality, holdable).
narrative_ontology:cs_axiom_grounding('60a0cd20-eabc-4045-bebc-d27cfdc26869', ethnic_national_framework_obstructs_civic_equality, empirically_contingent).
narrative_ontology:cs_axiom('60a0cd20-eabc-4045-bebc-d27cfdc26869', foundational, zionist_project_has_settler_colonial_dimensions).
narrative_ontology:cs_axiom_status(zionist_project_has_settler_colonial_dimensions, holdable).
narrative_ontology:cs_axiom_grounding('60a0cd20-eabc-4045-bebc-d27cfdc26869', zionist_project_has_settler_colonial_dimensions, empirically_contingent).
narrative_ontology:cs_reference_frame('60a0cd20-eabc-4045-bebc-d27cfdc26869', civic_equality_and_regional_integration).
narrative_ontology:cs_drift_state('60a0cd20-eabc-4045-bebc-d27cfdc26869', contemporary_state_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('60a0cd20-eabc-4045-bebc-d27cfdc26869', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ethnic-national framework through preferential immigration laws (Law of Return), land allocation policies, and state symbols that affirm Jewish national identity. They are structurally privileged within the state apparatus.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel, beneficiary,
    institutional, generational, mobile, national).

% Bear the costs of a state framework that defines them as a national minority within their homeland. They face systemic discrimination in land, housing, and resource allocation, and their civic equality is obstructed by laws prioritizing Jewish national character. Their identity is tied to the land, making exit unthinkable.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    organized, generational, identity_locked, national).

% Experience the most severe forms of extraction and suppression, living under military occupation with restricted movement, land confiscation, and limited political rights. They are structurally excluded from the state's civic framework and are direct targets of the ethnic-national project's expansion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations, payer,
    powerless, generational, trapped, regional).

% Administer and enforce the laws and policies that maintain the ethnic-national framework, including the Law of Return, land laws, and security doctrines. They are the primary agents of the constraint's persistence and extraction.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for a non-ethnic, inclusive framework that would allow for greater civic equality and regional cooperation. Their proposals are systematically marginalized by the existing ethnic-national state structure, which views such integration as an existential threat.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_integration_advocates, excluded,
    moderate, generational, constrained, regional).

% Document and report on human rights violations and discriminatory practices stemming from the ethnic-national framework. They provide external scrutiny but have limited direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the collective identity and security of Jewish people in their ancestral homeland, providing a framework for self-determination and refuge.
% TRANSFER_FUNCTION: Transfers land, resources, and political power from indigenous Palestinian populations to Jewish citizens, while also transferring a sense of national belonging and security to Jewish citizens.
% ABSENT_VOICES: Palestinian refugees and their descendants, who would demand the right of return and full civic equality, are systematically excluded from the political discourse and decision-making processes that shape the state's ethnic-national character.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight, the state's foundational laws (e.g., Law of Return, Nation-State Law) would be nullified, leading to a complete re-evaluation of citizenship, land ownership, and national symbols. This would fundamentally alter the power dynamics and potentially lead to a more equitable, but radically different, political entity.
% FOUNDING_PROBLEM: The Zionist project aimed to solve the problem of Jewish statelessness and persecution by establishing a sovereign Jewish state in Palestine.
% FOUNDING_PROBLEM_CORROBORATION: Jewish citizens and the Israeli state institutions largely attest that the founding problem of Jewish security and self-determination remains live. Palestinian citizens, occupied populations, and international human rights organizations attest that while statehood was achieved, the ethnic-national framework has created new problems of inequality and occupation, rendering the original problem's 'solution' a source of ongoing conflict and extraction for others.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the systemic transfer of resources and rights from Palestinian populations to Jewish citizens, particularly through land and citizenship laws. Suppression (0.85) is severe due to military occupation, legal discrimination, and the active suppression of Palestinian political and civil rights. The theater ratio (0.4) indicates that while some state functions serve a genuine purpose, a significant portion of state activity is dedicated to maintaining the ethnic-national character and its associated privileges, often under the guise of security. Accessibility collapse (0.7) is high for Palestinians, as alternatives to the existing state structure are severely limited or actively suppressed. Resistance (0.75) is also high, reflecting ongoing Palestinian struggle against the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish citizens and state institutions, the constraint is a legitimate expression of national self-determination, providing security and belonging (closer to a Rope or even Mountain). From the perspective of Palestinian citizens and occupied populations, it is a structure of ongoing extraction and oppression (a clear Snare). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens are beneficiaries due to preferential laws and policies (low d). Palestinian citizens and occupied populations are targets, bearing the costs of discrimination and occupation (high d). Israeli state institutions are agenda setters, actively enforcing the constraint. Regional integration advocates are excluded, as their vision directly challenges the constraint's ethnic-national core.
 *
 * MANDATROPHY ANALYSIS:
 *   This post-Zionist reading argues that the original mandate of Jewish self-determination has atrophied into a mechanism for ethnic privilege and obstruction of civic equality. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism (Rope) or a natural outcome (Mountain), highlighting its coercive and extractive nature. The persistence is not due to a live coordination problem for all parties, but due to the concentrated benefits for the agenda-setter and beneficiaries, maintained through active suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethnic_privilege_vs_security_necessity,
    'To what extent are the state''s ethnic-national laws and policies genuinely necessary for Jewish security, versus serving primarily to maintain ethnic privilege?',
    'Comparative analysis with other states facing similar security challenges but operating under civic-national frameworks, assessing their security outcomes and levels of civic equality. Expert legal and security analysis on the efficacy of specific ethnic-national provisions.',
    'If primarily for privilege, the constraint''s extractiveness and suppression are higher than justified by security needs, strengthening the Snare classification. If genuinely necessary for security, the coordination function is more salient, potentially shifting towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethnic_privilege_vs_security_necessity, empirical, 'Distinguishing security imperatives from ethnic privilege in state policy.').

omega_variable(
    founding_narrative_vs_contemporary_reality,
    'Is the founding narrative of a ''land without a people for a people without a land'' a historical justification for contemporary ethnic privilege, or a legitimate historical claim that continues to shape the state''s identity?',
    'Historical and archaeological research into pre-1948 demographics and land use, combined with critical discourse analysis of how the narrative is deployed in contemporary political discourse. Engagement with indigenous historical accounts.',
    'If primarily a historical justification for privilege, the constraint''s theatricality and suppression are higher, as the narrative serves to obscure ongoing extraction. If a legitimate, continuously relevant claim, the constraint''s ''naturalness'' (emerges_naturally) might be perceived differently by some, though this reading would still emphasize its extractive outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_narrative_vs_contemporary_reality, conceptual, 'The role of founding narratives in legitimizing current power structures.').

omega_variable(
    regional_integration_feasibility,
    'Would a de-Zionized state framework genuinely lead to regional integration and peace, or would it create new forms of instability and conflict?',
    'Scenario planning and political science modeling of alternative state structures and regional dynamics. Historical analysis of other post-colonial transitions and their outcomes.',
    'If regional integration is genuinely feasible and beneficial, the current framework''s obstruction of it highlights its extractive and exclusionary nature. If it leads to new instability, the constraint''s coordination function (for Jewish security) might be re-evaluated, though its extractive aspects would remain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_integration_feasibility, preference, 'The potential outcomes of alternative state frameworks for regional stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t2000, observed).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t2018, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.75).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.76).
narrative_ontology:measurement_basis(jewi_be_t2000, observed).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement_basis(jewi_be_t2018, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.8).
narrative_ontology:measurement_basis(jewi_su_t1993, observed).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement_basis(jewi_su_t2000, observed).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2018, 0.84).
narrative_ontology:measurement_basis(jewi_su_t2018, observed).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.85).
narrative_ontology:measurement_basis(jewi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_citizenship_law).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_land_allocation_policies).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, two_state_solution_viability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
