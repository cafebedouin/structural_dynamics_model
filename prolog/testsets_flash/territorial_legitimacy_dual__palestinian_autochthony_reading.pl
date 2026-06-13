% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony and Right of Return
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the Palestinian reading of territorial
 *   legitimacy, grounded in continuous habitation, the trauma of displacement
 *   (Nakba), and the non-negotiable right of return. It frames the 1948
 *   displacement as an ongoing injustice requiring remedy, views territorial
 *   reduction as severe deprivation, and contests the legitimacy of the
 *   Israeli state on these grounds. This is one reading of the
 *   'territorial_legitimacy_dual' kernel, which also includes
 *   'zionist_refuge_reading' and 'two_state_coexistence_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.85).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.92).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '279c7ece-51b9-4304-a355-38de29628627').
narrative_ontology:cs_kernel_codification('279c7ece-51b9-4304-a355-38de29628627', distributed).
narrative_ontology:cs_authority_grounding('279c7ece-51b9-4304-a355-38de29628627', lineage).
narrative_ontology:cs_interpretation_layer_present('279c7ece-51b9-4304-a355-38de29628627').
narrative_ontology:cs_reading_relation('279c7ece-51b9-4304-a355-38de29628627', territorial_legitimacy_dual__zionist_refuge_reading, forecloses).
narrative_ontology:cs_reading_relation('279c7ece-51b9-4304-a355-38de29628627', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('279c7ece-51b9-4304-a355-38de29628627', foundational, continuous_palestinian_habitation_precedes_1948).
narrative_ontology:cs_axiom_status(continuous_palestinian_habitation_precedes_1948, holdable).
narrative_ontology:cs_axiom_grounding('279c7ece-51b9-4304-a355-38de29628627', continuous_palestinian_habitation_precedes_1948, empirically_contingent).
narrative_ontology:cs_axiom('279c7ece-51b9-4304-a355-38de29628627', foundational, right_of_return_is_inalienable).
narrative_ontology:cs_axiom_status(right_of_return_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('279c7ece-51b9-4304-a355-38de29628627', right_of_return_is_inalienable, deontological).
narrative_ontology:cs_reference_frame('279c7ece-51b9-4304-a355-38de29628627', pre_1948_palestinian_sovereignty).
narrative_ontology:cs_drift_state('279c7ece-51b9-4304-a355-38de29628627', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('279c7ece-51b9-4304-a355-38de29628627', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced from their ancestral lands since 1948, they bear the direct cost of statelessness and denied return. Their identity is deeply tied to the right of return, making any alternative a form of identity-lock.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Experience ongoing territorial reduction, restrictions on movement, and contested sovereignty. Their daily lives are shaped by the denial of full self-determination and the continuous assertion of external control over their land.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_residents_occupied_territories, payer,
    powerless, generational, identity_locked, local).

% Benefits from the preservation of a unified Palestinian identity and the moral claim of return, which mobilizes international support and maintains a collective narrative. While not directly suffering displacement, their political and cultural identity is reinforced by this constraint.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora, beneficiary,
    organized, generational, constrained, global).

% Administers the political and diplomatic efforts to assert Palestinian rights, including the right of return and self-determination. Their legitimacy is largely derived from upholding these core tenets, making compromise on them a threat to their authority.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% The primary counter-party whose legitimacy is contested by this reading. It is excluded from the internal framing of Palestinian autochthony, which views its existence on disputed land as a continuous injustice.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, excluded,
    institutional, generational, mobile, national).

% Analyze the situation through the lens of international law, human rights, and self-determination, often corroborating the Palestinian narrative of injustice and the right of return. They exert moral and political pressure but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective identity, political aspirations, and historical narrative of the Palestinian people, ensuring a unified front in asserting claims to land and return despite geographic dispersion and political fragmentation.
% TRANSFER_FUNCTION: Transfers the moral and political burden of historical injustice and ongoing displacement onto the Israeli state and the international community, while consolidating a shared sense of grievance and purpose among Palestinians.
% ABSENT_VOICES: The Israeli state and its supporters are structurally excluded from this framing of legitimacy; they would argue for a different historical narrative and a recognition of their own claims to the land, but their perspective is not integrated into the core tenets of Palestinian autochthony.
% DISAPPEARANCE_RATIONALE: If the foundational claims of Palestinian autochthony, displacement trauma, and the right of return vanished, the collective identity and political project of the Palestinian people would fundamentally collapse. The basis for their resistance, international advocacy, and internal cohesion would be removed, leading to a profound reorganization of the regional political landscape.
% FOUNDING_PROBLEM: The dispossession and displacement of Palestinians in 1948 (the Nakba), leading to a loss of land, sovereignty, and national identity, and the ongoing denial of their right to return.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by Palestinian refugees, residents of occupied territories, and international human rights organizations, who document ongoing displacement, human rights violations, and the denial of return. This corroboration comes from outside the direct political leadership, affirming the lived reality of the problem.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Snare because it involves high extraction (0.85) from Palestinian refugees and residents of occupied territories, coupled with very high suppression (0.92) of their ability to exercise their claimed rights. The 'right of return' is a core identity-locked exit for many, making alternatives unthinkable. Resistance is high (0.75) due to ongoing political and armed struggle. Theater ratio is low (0.1) as the claims are deeply held and actively pursued, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Palestinian refugees and residents, this constraint is a fundamental assertion of their identity and rights, but its enforcement by external powers makes them victims of a snare. From the perspective of the Palestinian political leadership and diaspora, it is a necessary framework for collective action and identity preservation, making them beneficiaries of its coordination function, even as it imposes costs on others.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian refugees and residents are full targets (high d) due to their direct suffering and lack of exit options. The Palestinian diaspora and political leadership are beneficiaries (low d) as their collective identity and political power are sustained by this narrative, despite the costs borne by others. The Israeli state is the primary target of the claims, but is excluded from the internal framing of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (remedying historical injustice and securing the right of return) is very much 'live' for its primary victims and beneficiaries. There is no evidence of mandatrophy; the persistence of the constraint is driven by the ongoing nature of the founding problem and the active resistance it generates, not by inertia or theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_narrative_contestation,
    'To what extent is the Palestinian narrative of continuous habitation and displacement trauma universally accepted, or is it fundamentally contested by alternative historical accounts?',
    'Comprehensive, internationally mediated historical commission with access to all archives and testimonies, leading to a consensus historical account.',
    'If the narrative is universally corroborated, the constraint''s moral force is amplified, increasing pressure for remedies. If fundamentally contested, its legitimacy as a ''natural'' claim is weakened, potentially shifting it towards a ''preference'' or ''conventional'' type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_narrative_contestation, empirical, 'The degree of contestation over the foundational historical narrative.').

omega_variable(
    right_of_return_feasibility,
    'Is the full ''right of return'' for all Palestinian refugees (and their descendants) practically feasible without fundamentally altering the demographic character of the Israeli state?',
    'Detailed demographic and logistical studies, coupled with political negotiations exploring various models of return, compensation, or resettlement.',
    'If deemed infeasible without radical demographic change, the ''right of return'' might be re-framed as a claim for compensation or symbolic recognition, altering the constraint''s specific demands. If feasible, the moral and political pressure for its implementation intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Practical feasibility of implementing the full right of return.').

omega_variable(
    identity_lock_vs_structural_trapping,
    'For Palestinian refugees, is their ''trapped'' exit option primarily due to identity-lock (unwillingness to abandon the right of return) or structural barriers (denial of entry by Israel)?',
    'Surveys and qualitative studies among refugee populations exploring their preferences and perceived options, alongside legal analysis of entry restrictions.',
    'If primarily identity-locked, the constraint''s persistence is more internal to the group''s self-conception. If primarily structural, the external suppressive force is the dominant factor, reinforcing the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trapping, empirical, 'Distinguishing identity-lock from structural trapping for refugees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.07).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1987, 0.09).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1987, 0.82).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2014, 0.86).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1987, 0.88).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2014, 0.91).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_settlement_expansion).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, gaza_blockade).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_legitimacy_dual' kernel, focusing on Palestinian autochthony and the right of return. It is structurally distinct from the 'zionist_refuge_reading' and 'two_state_coexistence_reading', which offer alternative framings of legitimacy and resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
