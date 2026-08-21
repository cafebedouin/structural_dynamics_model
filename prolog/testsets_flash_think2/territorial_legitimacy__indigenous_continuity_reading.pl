% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy: Indigenous Continuity Reading (1948 as Nakba)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'indigenous continuity' reading of
 *   territorial legitimacy, which posits that sovereignty over historic
 *   Palestine inherently belongs to the indigenous Palestinian people,
 *   viewing the 1948 establishment of Israel as the 'Nakba' (catastrophe) and
 *   a settler-colonial act. From this perspective, the Israeli state is an
 *   illegitimate entity, and the right of return for 1948 refugees is a
 *   structurally central demand. The constraint is claimed as a 'mountain'
 *   because this reading asserts indigenous rights and anti-colonial
 *   principles as fundamental, unchangeable truths of justice and
 *   international law. However, the high extractiveness, suppression, and
 *   resistance metrics reflect the ongoing violation of this 'mountain' by
 *   the existing political reality, triggering the False Summit Mountain
 *   detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.92).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, mountain).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy: Indigenous Continuity Reading (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).
domain_priors:emerges_naturally(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435').
narrative_ontology:cs_kernel_codification('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', implicit).
narrative_ontology:cs_authority_grounding('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', lineage).
narrative_ontology:cs_interpretation_layer_present('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435').
narrative_ontology:cs_reading_relation('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', foundational, indigenous_rights_are_inalienable).
narrative_ontology:cs_axiom_status(indigenous_rights_are_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', indigenous_rights_are_inalienable, deontological).
narrative_ontology:cs_axiom('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', foundational, settler_colonialism_is_illegitimate).
narrative_ontology:cs_axiom_status(settler_colonialism_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', settler_colonialism_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', pre_nakba_indigenous_sovereignty).
narrative_ontology:cs_drift_state('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', contemporary_occupation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('735e7acd-f2f5-4ba3-b0ed-3fab2f4f3435', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, israeli_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the indigenous population, they are the rightful beneficiaries of continuous sovereignty over historic Palestine. Simultaneously, they are the primary victims of the ongoing dispossession and denial of self-determination, bearing the costs of occupation, displacement, and statelessness. Their identity is deeply tied to the land and the right of return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_people, payer).

% From this reading's perspective, the Israeli state is the primary enforcer of the current arrangement that violates indigenous continuity. It benefits from the existing territorial control but would bear the cost of dismantling the settler-colonial structure if this reading were implemented.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Scholars, NGOs, and legal bodies who interpret international law through an anti-colonial lens, advocating for indigenous rights and self-determination. They analyze the constraint's operation and its violation, seeking to influence global discourse and policy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_law_advocates, observer,
    analytical, civilizational, analytical, global).

% International organizations tasked with upholding international law and human rights. While they have passed resolutions affirming Palestinian rights, their enforcement power is constrained by geopolitical realities. They are a site of contestation over the interpretation and application of territorial legitimacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, un_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who benefit from the current territorial control and settlement expansion. They are beneficiaries of the status quo but would face significant disruption and potential displacement if the indigenous continuity reading were to be fully realized.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_settlers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, israeli_settlers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a just and equitable framework for territorial sovereignty based on the inalienable rights of indigenous populations and the principles of anti-colonial self-determination.
% TRANSFER_FUNCTION: This reading asserts the transfer of sovereignty and land rights from the settler-colonial entity to the indigenous Palestinian population, facilitating the right of return for 1948 refugees and their descendants.
% ABSENT_VOICES: The voices of those dispossessed and displaced since 1948, whose narratives and claims to land are often marginalized or actively suppressed in dominant international and national discourses.
% DISAPPEARANCE_RATIONALE: If the principle of indigenous continuity and anti-colonial self-determination were universally and immediately recognized and enforced, the entire geopolitical structure of historic Palestine would fundamentally shift. The Israeli state, as currently constituted, would be deemed illegitimate, and a new political order centered on Palestinian sovereignty and the right of return would emerge, leading to a complete reorganization of land ownership, citizenship, and regional power dynamics.
% FOUNDING_PROBLEM: The historical injustice of settler-colonialism and the dispossession of the indigenous Palestinian population, culminating in the 1948 Nakba, which established a state on land claimed by another people, violating their continuous habitation and right to self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Numerous international human rights organizations (e.g., Amnesty International, Human Rights Watch), historical archives, UN resolutions, and a vast body of academic scholarship in post-colonial studies and international law corroborate the historical dispossession and ongoing struggle, independent of the direct beneficiaries. This corroboration supports the claim that the founding problem remains live and unresolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_legitimacy__indigenous_continuity_reading),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is very high (0.92) because the current arrangement fundamentally dispossesses the indigenous population of their land and self-determination. Suppression is also very high (0.88) as the persistence of the current state relies on active military, legal, and political measures to suppress Palestinian resistance, claims, and the right of return. Theater ratio is very low (0.05) because the claims of indigenous continuity are deeply held, actively pursued, and represent a genuine, existential struggle, not a performative one. Resistance is extremely high (0.95) due to continuous Palestinian struggle against occupation and dispossession. The metrics reflect the severity of the violation of this 'mountain' of justice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Palestinian people, this constraint is an unyielding moral and legal truth that is being brutally violated, leading to immense suffering and extraction. From the perspective of the Israeli state and its supporters, this 'mountain' is either denied or reframed as a competing claim, often seen as a threat to their existence, leading to a fundamentally different experience of the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian people are simultaneously the rightful beneficiaries of the just order this 'mountain' represents (low d) and the primary victims of its ongoing violation (high d). The Israeli state and settlers are beneficiaries of the current arrangement that violates this 'mountain' (low d for the current state, but high d if the 'mountain' were enforced). International law advocates and UN bodies are observers and agenda-setters, whose directionality depends on their specific actions and interpretations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (indigenous rights, anti-colonial self-determination) is considered timeless and unfulfilled. The high extractiveness and suppression are not signs of a decaying function, but rather of an active, ongoing violation of a fundamental principle. The classification as a 'mountain' (albeit a false summit) prevents mislabeling this as a 'snare' or 'tangled_rope' that has merely outlived its purpose; instead, it highlights a foundational injustice that persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is the claim of indigenous continuity and anti-colonial self-determination a genuine natural law (a structural feature of reality) or a constructed legal and moral framework?',
    'Philosophical and legal analysis of the foundations of rights, combined with cross-cultural anthropological studies of land tenure and sovereignty. If its universality is contingent on specific legal traditions, it leans towards constructed.',
    'If a genuine natural law, its violation is a fundamental injustice regardless of human agreement. If a constructed framework, its legitimacy depends on its acceptance and enforcement within international legal systems, potentially altering its perceived ''mountain'' status to a ''rope'' or ''tangled_rope'' that requires active defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of indigenous rights.').

omega_variable(
    feasibility_of_full_sovereignty,
    'What are the practical implications and feasibility of implementing full Palestinian sovereignty over all of historic Palestine, including the right of return, given the existing demographic and political realities?',
    'Detailed demographic, economic, and political modeling of various implementation scenarios, including transitional justice mechanisms, and comparative analysis with other post-colonial transitions.',
    'If implementation is deemed practically impossible without severe disruption to all populations, it might shift the focus of advocacy towards alternative forms of justice or shared sovereignty, potentially altering the perceived ''accessibility_collapse'' and ''resistance'' dynamics. If feasible, it strengthens the ''mountain'' claim by demonstrating a path to its realization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_of_full_sovereignty, empirical, 'Practical challenges of implementing the full scope of indigenous continuity claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2005, 0.9).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.83).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, right_of_return_claim).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, israeli_settlement_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel. Its ε value and structural properties differ significantly from the 'partition_reading' and 'security_necessity_reading', necessitating separate constraint stories. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
