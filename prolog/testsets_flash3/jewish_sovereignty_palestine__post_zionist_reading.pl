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
 *   This constraint describes the post-Zionist reading of Jewish sovereignty
 *   in Palestine, where the founding narrative and ethnic-national framework
 *   of the state, while achieving Jewish self-determination, now actively
 *   obstruct civic equality for non-Jewish citizens and regional integration.
 *   The constraint is framed as a Tangled Rope, acknowledging a genuine
 *   coordination function (Jewish self-determination) intertwined with
 *   significant, actively enforced extraction from Palestinian populations.
 *   The metrics reflect high extractiveness and suppression, with a rising
 *   theater ratio as the state's actions are increasingly justified by
 *   security narratives that mask ethnic privilege.
 *
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
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Sovereignty in Palestine (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '5d42696c-7c4e-42a9-9293-e13107a36617').
narrative_ontology:cs_kernel_codification('5d42696c-7c4e-42a9-9293-e13107a36617', formalized).
narrative_ontology:cs_authority_grounding('5d42696c-7c4e-42a9-9293-e13107a36617', lineage).
narrative_ontology:cs_interpretation_layer_present('5d42696c-7c4e-42a9-9293-e13107a36617').
narrative_ontology:cs_reading_relation('5d42696c-7c4e-42a9-9293-e13107a36617', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d42696c-7c4e-42a9-9293-e13107a36617', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d42696c-7c4e-42a9-9293-e13107a36617', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d42696c-7c4e-42a9-9293-e13107a36617', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('5d42696c-7c4e-42a9-9293-e13107a36617', foundational, ethnic_national_framework_obstructs_equality).
narrative_ontology:cs_axiom_status(ethnic_national_framework_obstructs_equality, holdable).
narrative_ontology:cs_axiom_grounding('5d42696c-7c4e-42a9-9293-e13107a36617', ethnic_national_framework_obstructs_equality, empirically_contingent).
narrative_ontology:cs_axiom('5d42696c-7c4e-42a9-9293-e13107a36617', foundational, civic_equality_regional_integration_are_imperatives).
narrative_ontology:cs_axiom_status(civic_equality_regional_integration_are_imperatives, holdable).
narrative_ontology:cs_axiom_grounding('5d42696c-7c4e-42a9-9293-e13107a36617', civic_equality_regional_integration_are_imperatives, deontological).
narrative_ontology:cs_reference_frame('5d42696c-7c4e-42a9-9293-e13107a36617', zionist_project_achieves_statehood).
narrative_ontology:cs_drift_state('5d42696c-7c4e-42a9-9293-e13107a36617', contemporary_postcolonial_critique, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d42696c-7c4e-42a9-9293-e13107a36617', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinians_in_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the state's ethnic-national framework, including the Law of Return, preferential land allocation, and institutionalized Jewish character. They are the primary beneficiaries of the state's self-definition as a Jewish state, which grants them a privileged status.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel, beneficiary,
    institutional, generational, mobile, national).

% Bear the costs of the state's ethnic-national definition, experiencing systemic discrimination in land, housing, and civic life. Their identity as Palestinian is often marginalized within the state's Jewish character, leading to a struggle for full civic equality.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    organized, generational, identity_locked, national).

% Experience the most severe forms of extraction and suppression, living under military occupation and denied basic civic and human rights. Their land is subject to confiscation, and their movement is severely restricted, with no political representation within the Israeli state.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinians_in_occupied_territories, payer,
    powerless, generational, trapped, regional).

% Document and report on human rights violations and discriminatory practices stemming from the state's ethnic-national framework. They advocate for civic equality and an end to occupation, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_organizations, observer,
    organized, biographical, analytical, global).

% Administer and enforce the laws and policies that uphold the state's Jewish character, including the Nation-State Law, land laws, and immigration policies. They are the primary agents responsible for maintaining the ethnic-national framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state's ethnic-national framework coordinates the collective identity and self-determination of the Jewish people, providing a secure homeland and cultural center.
% TRANSFER_FUNCTION: Transfers land, resources, and civic privileges from Palestinian populations to Jewish citizens, while simultaneously transferring the burden of maintaining an ethnic-national state onto all citizens, including those excluded from its core identity.
% ABSENT_VOICES: Palestinian refugees and their descendants, who were dispossessed during the founding of the state, are structurally excluded from the political discourse and legal framework, despite their historical claims to the land. Their voices would fundamentally challenge the state's founding narrative.
% DISAPPEARANCE_RATIONALE: If the state's ethnic-national framework vanished overnight, the legal and social structures underpinning Jewish privilege would collapse. This would necessitate a fundamental reordering of land ownership, citizenship rights, and political representation, leading to a radically different civic and regional landscape.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, necessitating a secure homeland where they could exercise self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Jewish citizens and many international supporters attest that the problem of Jewish insecurity remains live, citing ongoing antisemitism and regional threats. Palestinian citizens, occupied populations, and international human rights organizations attest that while the founding problem for Jewish people may have been addressed, the current framework has created new problems of dispossession and inequality, and that the original problem is now used as cover for ongoing extraction.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) due to systemic discrimination in land, citizenship, and political rights for Palestinians. Suppression is very high (0.85) because the state actively enforces laws and policies that maintain its ethnic-national character, including military occupation and control over Palestinian populations. The theater ratio is moderate (0.4) as security concerns, while real, are increasingly used to justify policies that primarily serve to maintain Jewish demographic and political dominance. The historical measurements show a steady increase in extractiveness and suppression, particularly after 1967, reflecting the deepening of occupation and the hardening of the ethnic-national framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish citizens, the state's framework is a necessary and legitimate expression of self-determination, a Rope that provides security and belonging. From the perspective of Palestinians, it is a Snare, an extractive and suppressive structure that denies their rights and dispossesses them. The post-Zionist reading attempts to bridge this gap by acknowledging the historical necessity of the Zionist project while critiquing its current manifestation as an obstacle to equality and peace.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens are the primary beneficiaries (d near 0.0) due to preferential laws and institutional support. Palestinian citizens of Israel and Palestinians in occupied territories are the primary targets (d near 1.0), bearing the brunt of discrimination, dispossession, and occupation. Israeli state institutions act as the agenda-setter, actively enforcing the constraint. International human rights organizations serve as observers, documenting the effects without direct power to alter the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (Jewish self-determination) is still live for many, but its *form* has atrophied into an extractive mechanism. The classification as Tangled Rope prevents mislabeling it as pure extraction (Snare) by acknowledging the coordination function, but also prevents mislabeling it as pure coordination (Rope) by highlighting the asymmetric extraction and active enforcement. The rising theater ratio and contested founding problem status indicate a drift towards a more extractive and performative maintenance of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethnic_national_vs_civic_state,
    'Is a state''s self-definition as ''Jewish and democratic'' inherently contradictory, or can these two principles be reconciled in practice?',
    'Legal and political reforms that prioritize civic equality and universal rights over ethnic-national privilege, followed by empirical observation of their impact on discrimination and integration.',
    'If contradictory, the constraint is fundamentally extractive and requires de-Zionization of state institutions for civic equality. If reconcilable, the constraint could evolve into a more equitable Rope, but would require substantial re-framing and policy changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethnic_national_vs_civic_state, conceptual, 'The inherent tension between ethnic-national identity and democratic civic equality within the state''s foundational principles.').

omega_variable(
    security_narrative_vs_ethnic_privilege,
    'To what extent are security concerns genuinely driving policies that maintain ethnic privilege, versus being used as a cover for such policies?',
    'Independent audits of security justifications for policies affecting Palestinian populations, comparing stated security threats with actual outcomes and alternative policy options.',
    'If security is primarily a cover, the theater ratio is higher than currently estimated, and the constraint is more purely extractive (Snare). If security concerns are genuinely paramount, the coordination function is stronger, and the constraint is closer to a Tangled Rope with high but justified costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_narrative_vs_ethnic_privilege, empirical, 'The degree to which security narratives mask or genuinely drive policies that maintain ethnic privilege.').

omega_variable(
    identity_locked_palestinian_citizens,
    'Is the ''identity_locked'' exit option for Palestinian citizens of Israel primarily due to structural barriers, or has it become internalized through generations of marginalization?',
    'Post-reform studies: if structural barriers are removed (e.g., full civic equality), does a significant portion of the population still feel ''locked'' due to internalized identity or historical trauma? This would indicate a higher internalized component.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the constraint''s effects persist even after formal barriers are removed. This would complicate resolution, requiring deeper societal shifts beyond legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_palestinian_citizens, empirical, 'Structural vs. internalized suppression mechanism for Palestinian citizens of Israel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1987, 0.75).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2014, 0.78).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1987, 0.83).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2014, 0.85).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_land_laws).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, law_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, nation_state_law).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_occupation_regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
