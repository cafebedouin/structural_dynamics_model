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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Jewish Sovereignty in Palestine: Post-Zionist Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'post-Zionist' reading of Jewish
 *   sovereignty in Palestine, where the achievement of statehood, while
 *   fulfilling a historical aspiration, has created an ethnic-national
 *   framework that now actively obstructs civic equality for non-Jewish
 *   citizens and regional integration. The constraint is viewed as a Tangled
 *   Rope: it provides a coordination function for Jewish self-determination
 *   but simultaneously extracts from and suppresses Palestinian populations
 *   through its ethnic-national character and associated policies. This
 *   reading emphasizes the ongoing structural inequalities and the need to
 *   move beyond the foundational narratives to achieve a more equitable
 *   future.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.7).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.65).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Jewish Sovereignty in Palestine: Post-Zionist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '94eef60d-8be6-45e0-ac01-d7f6dfb8cc59').
narrative_ontology:cs_kernel_codification('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', formalized).
narrative_ontology:cs_authority_grounding('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', lineage).
narrative_ontology:cs_interpretation_layer_present('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59').
narrative_ontology:cs_reading_relation('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', foundational, ethnic_privilege_obstructs_equality).
narrative_ontology:cs_axiom_status(ethnic_privilege_obstructs_equality, holdable).
narrative_ontology:cs_axiom_grounding('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', ethnic_privilege_obstructs_equality, empirically_contingent).
narrative_ontology:cs_axiom('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', secondary, founding_narrative_perpetuates_inequality).
narrative_ontology:cs_axiom_status(founding_narrative_perpetuates_inequality, holdable).
narrative_ontology:cs_axiom_grounding('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', founding_narrative_perpetuates_inequality, empirically_contingent).
narrative_ontology:cs_reference_frame('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', civic_egalitarian_state).
narrative_ontology:cs_drift_state('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94eef60d-8be6-45e0-ac01-d7f6dfb8cc59', '').
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

% Benefit from the state's ethnic-national framework, including the Law of Return, preferential land allocation, and cultural dominance. They experience the state as a guarantor of their collective self-determination and security, often viewing its foundational narrative as essential.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_of_israel, beneficiary,
    institutional, generational, mobile, national).

% Bear the costs of the state's ethnic-national character, experiencing systemic discrimination in land, housing, and civic participation. They are citizens but are often treated as a demographic threat, with their national identity suppressed within the state's framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    organized, generational, identity_locked, national).

% Live under military occupation or blockade, with their self-determination denied and their land and resources subject to control by the Israeli state. They are directly impacted by the expansion of settlements and the denial of civic and political rights.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, occupied_palestinian_populations, payer,
    powerless, generational, trapped, regional).

% Monitor and report on human rights violations, often critiquing the state's policies as discriminatory or violating international law. They advocate for civic equality and an end to occupation, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% Administer and enforce laws and policies that enshrine the state's Jewish character, including those related to citizenship, land, and national symbols. They are responsible for maintaining the existing ethnic-national framework and managing its internal and external challenges.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective self-determination and security of the Jewish people in their ancestral homeland, providing a framework for national identity, cultural preservation, and a refuge from antisemitism.
% TRANSFER_FUNCTION: Transfers land, resources, and civic/political rights from Palestinian populations (both citizens and occupied) to Jewish citizens, in service of maintaining an ethnic-national majority and character for the state.
% ABSENT_VOICES: Palestinian refugees and their descendants, who would demand the right of return and full civic equality, are excluded from the political discourse within Israel and often from international forums that might challenge the state's foundational premises.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight, the state's legal and social structures would undergo a profound transformation. The Law of Return would be abolished, land ownership would be contested, and the civic status of all inhabitants would be re-negotiated, leading to a fundamental reordering of power and identity.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, culminating in the Holocaust, necessitated a sovereign homeland to ensure their safety and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Jewish historical narratives and international recognition of the right to self-determination. However, Palestinian historians and postcolonial scholars, from outside the benefiting parties, contest the framing, arguing that the solution to Jewish statelessness was achieved at the expense of indigenous Palestinian rights, rendering the 'problem' for one group a 'catastrophe' for another.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).

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
 *   Extractiveness is high (0.7) due to the systemic transfer of resources and rights from Palestinians to Jewish citizens, enforced through laws like the Law of Return and land policies. Suppression (0.65) is significant, reflecting the active enforcement required to maintain the ethnic-national character of the state against internal and external challenges, including the suppression of Palestinian national identity and political aspirations. Theater ratio (0.2) is relatively low, as the state's institutions are genuinely functional in maintaining the existing order, though some performative aspects exist in justifying policies as purely defensive or democratic. The historical measurements show a rise in extractiveness after 1967 with the occupation of new territories, and fluctuations in suppression reflecting periods of conflict and attempts at political resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish citizens, the state's ethnic-national framework is a legitimate and necessary expression of self-determination, a 'Rope' ensuring their survival. From the perspective of Palestinian citizens and occupied populations, the same framework is a 'Snare' or 'Tangled Rope' that perpetuates their subjugation and denies their rights. This divergence is central to the post-Zionist critique, which highlights how the same structure is experienced as liberation by one group and oppression by another.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish citizens are the primary beneficiaries (d near 0.0), as the state's framework is designed to serve their collective identity and security. Palestinian citizens and occupied populations are the primary targets (d near 1.0), bearing the costs of discrimination, dispossession, and denial of self-determination. Israeli state institutions act as the agenda-setter, actively enforcing the constraint. International human rights organizations serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring Jewish self-determination and security) is still 'live' for its beneficiaries, but for its victims, the original problem has been superseded by the problem of ongoing extraction and inequality. The post-Zionist reading argues that the founding narrative, while historically justified, has become a mechanism for perpetuating an unjust status quo, indicating a form of mandatrophy where the means (ethnic-national state) have become an end that obstructs broader goals of civic equality and regional peace. The 'contested' status of the founding problem reflects this divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethnic_vs_civic_state,
    'Is the ethnic-national character of the state an inherent and unchangeable feature, or can it be reformed towards a more civic and egalitarian model without undermining Jewish self-determination?',
    'Empirical observation of constitutional reforms or political movements that successfully decouple ethnic identity from state institutions while maintaining security and cultural flourishing.',
    'If reformable, the constraint''s extractiveness and suppression could be significantly reduced, potentially reclassifying it towards a Rope or even a Scaffold (if transitional). If unchangeable, the current classification as Tangled Rope (or even Snare) is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethnic_vs_civic_state, conceptual, 'The possibility of decoupling ethnic identity from state institutions.').

omega_variable(
    founding_narrative_legitimacy,
    'To what extent does the founding narrative of Jewish self-determination and refuge continue to legitimately justify the current ethnic-national framework, given its impact on Palestinian populations?',
    'Historical and ethical analysis that weighs the historical context of Zionism against contemporary human rights norms and the experiences of all affected populations. This is a continuous re-evaluation.',
    'If the founding narrative''s justifying power is significantly eroded, the constraint''s ''coordination'' function becomes more tenuous, pushing it closer to a pure Snare. If it retains strong moral force, the Tangled Rope classification is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_narrative_legitimacy, preference, 'The ongoing moral weight of the founding narrative.').

omega_variable(
    regional_integration_potential,
    'Would a shift towards a more civic state genuinely facilitate regional integration and peace, or are other geopolitical factors the primary obstacles?',
    'Comparative political science analysis of other post-conflict regions and scenario modeling of alternative political arrangements in the Middle East.',
    'If civic equality is a strong driver of regional peace, the current framework''s obstruction becomes a more severe cost. If other factors dominate, the benefits of ''de-Zionization'' for regional integration are less certain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_integration_potential, empirical, 'Impact of civic state on regional integration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.65).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_sovereignty_palestine' kernel. Other readings (liberal_nationalist_reading, settler_colonial_reading, religious_zionist_reading, cultural_zionist_reading) offer alternative structural analyses of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
