% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination in Ancestral Homeland (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the liberal nationalist reading of Jewish
 *   self-determination in Palestine, asserting the legitimacy of Jewish
 *   statehood in its ancestral homeland while acknowledging the co-equal
 *   self-determination rights of the Palestinian people. This reading
 *   typically advocates for a two-state solution or a binational framework,
 *   implying territorial compromise and shared sovereignty. The constraint is
 *   claimed as a Tangled Rope because it combines a genuine coordination
 *   function (Jewish national self-determination) with an inherent, actively
 *   enforced extraction from the Palestinian people, whose co-equal claims
 *   are recognized but often not fully realized in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.55).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination in Ancestral Homeland (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '1d9a9251-c6d5-44ab-945f-dda3446c7350').
narrative_ontology:cs_kernel_codification('1d9a9251-c6d5-44ab-945f-dda3446c7350', formalized).
narrative_ontology:cs_authority_grounding('1d9a9251-c6d5-44ab-945f-dda3446c7350', lineage).
narrative_ontology:cs_interpretation_layer_present('1d9a9251-c6d5-44ab-945f-dda3446c7350').
narrative_ontology:cs_reading_relation('1d9a9251-c6d5-44ab-945f-dda3446c7350', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('1d9a9251-c6d5-44ab-945f-dda3446c7350', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d9a9251-c6d5-44ab-945f-dda3446c7350', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('1d9a9251-c6d5-44ab-945f-dda3446c7350', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('1d9a9251-c6d5-44ab-945f-dda3446c7350', foundational, jewish_people_are_a_nation_with_self_determination_rights).
narrative_ontology:cs_axiom_status(jewish_people_are_a_nation_with_self_determination_rights, holdable).
narrative_ontology:cs_axiom_grounding('1d9a9251-c6d5-44ab-945f-dda3446c7350', jewish_people_are_a_nation_with_self_determination_rights, deontological).
narrative_ontology:cs_axiom('1d9a9251-c6d5-44ab-945f-dda3446c7350', foundational, palestinians_are_a_nation_with_co_equal_self_determination_rights).
narrative_ontology:cs_axiom_status(palestinians_are_a_nation_with_co_equal_self_determination_rights, holdable).
narrative_ontology:cs_axiom_grounding('1d9a9251-c6d5-44ab-945f-dda3446c7350', palestinians_are_a_nation_with_co_equal_self_determination_rights, deontological).
narrative_ontology:cs_reference_frame('1d9a9251-c6d5-44ab-945f-dda3446c7350', two_states_for_two_peoples_framework).
narrative_ontology:cs_drift_state('1d9a9251-c6d5-44ab-945f-dda3446c7350', contemporary_era_of_stalled_peace_process, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d9a9251-c6d5-44ab-945f-dda3446c7350', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_zionist_advocates).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_nationalism_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to exercise its right to self-determination and maintain a sovereign state in its ancestral homeland, balancing national aspirations with democratic values and the rights of other claimants. Benefits from the existence and recognition of the state.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation, beneficiary,
    institutional, generational, constrained, national).

% Claims co-equal rights to self-determination and statehood in the same ancestral homeland. Bears the costs of ongoing conflict, occupation, and displacement, and seeks to overcome structural disadvantages to achieve full sovereignty.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_people_as_nation, payer,
    institutional, generational, trapped, national).

% Implement policies and maintain security based on the liberal nationalist interpretation of Jewish self-determination, navigating the tension between national security, democratic principles, and the unresolved Palestinian question. Enforces borders and laws.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Promote a vision of a secure, democratic Jewish state alongside a viable Palestinian state, benefiting from the legitimacy and moral standing of this framing in international discourse. Their advocacy seeks to align state practice with liberal ideals.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_zionist_advocates, beneficiary,
    organized, biographical, mobile, global).

% Interpret and apply international law regarding self-determination, statehood, human rights, and the laws of armed conflict to the situation. Their pronouncements influence the legitimacy and international standing of the constraint.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% Attempt to broker agreements and facilitate negotiations between the parties, typically based on two-state solutions or other partition frameworks that align with liberal nationalist principles of mutual recognition and territorial compromise.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_peace_mediators, observer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for the Jewish people to exercise their right to self-determination and establish a sovereign state in their ancestral homeland, while acknowledging the co-equal self-determination rights of the Palestinian people, necessitating a negotiated resolution.
% TRANSFER_FUNCTION: Transfers political sovereignty, territorial control, and national identity to the Jewish collective, while simultaneously acknowledging the need for a reciprocal transfer of these to the Palestinian people through partition or a binational framework.
% ABSENT_VOICES: Radical elements on both sides who reject any compromise or recognition of the other's national rights are structurally excluded from the liberal nationalist discourse, as their positions fundamentally undermine the possibility of a shared, negotiated future.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist framework for Jewish self-determination and statehood vanished overnight, the entire political and legal architecture governing the conflict would collapse. This would lead to immediate and profound reorganization of power, claims, and territorial control, likely resulting in intensified conflict or a radical shift in the nature of the dispute.
% FOUNDING_PROBLEM: The historical persecution and statelessness of the Jewish people, necessitating a secure homeland, alongside the emergence of modern nationalism and the desire for national self-expression.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by international recognition of Jewish self-determination, historical context of antisemitism and the Holocaust, and ongoing security concerns. While the Jewish collective and its advocates attest to its continued live status, Palestinian narratives and postcolonial scholars contest this framing as insufficient to address their dispossession and the ongoing conflict.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55 at end) because while the framework aims for compromise, the current reality involves significant territorial control and resource allocation by the Israeli state, which extracts from Palestinians. Suppression is higher (0.72 at end) due to the active enforcement required to maintain state borders, control over disputed territories, and manage the ongoing conflict. Theater ratio is moderate (0.30 at end) as diplomatic efforts and rhetorical commitments to a two-state solution often persist even as practical progress stalls, creating a gap between stated intent and functional outcome. The metrics show a trend of increasing extractiveness and suppression over time, reflecting the hardening of the conflict and the erosion of prospects for a liberal-nationalist resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish collective (beneficiary), the constraint is a legitimate exercise of national rights, a necessary framework for security and cultural flourishing. From the perspective of the Palestinian people (payer/victim), the same constraint, even in its liberal nationalist framing, represents an ongoing imposition that limits their self-determination and extracts resources and land, despite rhetorical commitments to co-equality.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish collective, as the primary beneficiary of statehood and national self-determination, sits at the beneficiary end of directionality. The Palestinian people, whose co-equal claims are acknowledged but whose practical exercise of self-determination is constrained by the existing state structure and ongoing conflict, sit at the target end. Israeli state institutions act as agenda-setters, enforcing the constraint. Liberal Zionist advocates benefit from the legitimacy of this framing, while international bodies observe and mediate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (Jewish self-determination) remains live, but its liberal nationalist framing struggles to resolve the inherent tension with Palestinian self-determination. The 'tangled' aspect arises because the coordination function for one group (Jewish statehood) is inextricably linked with an extractive function for another (Palestinian dispossession/subordination), preventing it from being a pure Rope. The persistence of the constraint, despite the lack of full resolution for Palestinians, indicates that the coordination for the Jewish collective is maintained, even if the 'liberal' promise of co-equality remains unfulfilled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_equality_in_practice,
    'To what extent does the practical implementation of this constraint genuinely uphold the ''co-equal'' self-determination rights of the Palestinian people, versus merely acknowledging them rhetorically?',
    'Empirical analysis of land allocation, resource distribution, freedom of movement, and political autonomy in areas under Israeli control or influence, compared to international standards for self-determination.',
    'If practical implementation falls significantly short of co-equality, the constraint''s effective extractiveness and suppression are higher than currently measured, pushing it closer to a Snare. If co-equality is substantially realized, it moves closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_equality_in_practice, empirical, 'Gap between rhetorical commitment to co-equality and practical outcomes for Palestinians.').

omega_variable(
    territorial_compromise_feasibility,
    'Is genuine territorial compromise, as envisioned by this liberal nationalist reading, still a feasible path given current demographic and political realities, or has it been foreclosed by facts on the ground?',
    'Expert assessment by demographers, urban planners, and political scientists on the viability of a two-state solution or binational framework, considering settlement expansion and population distribution.',
    'If compromise is no longer feasible, the ''liberal'' aspect of the reading becomes performative, increasing the theater_ratio and pushing the constraint towards a more purely extractive Snare, as the coordination story for Palestinians collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_compromise_feasibility, empirical, 'Feasibility of territorial compromise as a core tenet of the liberal nationalist reading.').

omega_variable(
    national_vs_liberal_values_tension,
    'Can a state founded on ethnic-national self-determination truly uphold universal liberal democratic values for all its citizens and residents, or is there an inherent, unresolved tension?',
    'Comparative political science analysis of other ethnic-national democracies and their treatment of minority populations, alongside legal analysis of citizenship laws and civil rights within the state.',
    'If an inherent tension is found to systematically disadvantage non-Jewish citizens, the constraint''s internal coherence as a ''liberal'' nationalist project is undermined, increasing its effective extractiveness and suppression for those minorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_vs_liberal_values_tension, conceptual, 'Inherent tension between ethnic-national self-determination and universal liberal democratic values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'Jewish sovereignty in Palestine' kernel. Each reading represents a distinct structural claim with its own ε and classification, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
