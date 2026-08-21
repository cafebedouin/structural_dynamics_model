% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency Requirement for Lethal Force
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents a specific reading of International
 *   Humanitarian Law (IHL) and the Martens Clause, asserting that human moral
 *   judgment is an irreducible requirement for lethal force application. It
 *   prohibits delegating life-and-death decisions to machines, thereby
 *   suppressing the development and deployment of fully autonomous lethal
 *   weapons systems (LAWS). While framed as a coordination mechanism for IHL
 *   compliance, it imposes significant costs on military innovation and
 *   operational efficiency, leading to its classification as a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.78).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.85).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '8d9b5f27-06c5-47cf-8efd-c093a2a97713').
narrative_ontology:cs_kernel_codification('8d9b5f27-06c5-47cf-8efd-c093a2a97713', fixed_text).
narrative_ontology:cs_authority_grounding('8d9b5f27-06c5-47cf-8efd-c093a2a97713', lineage).
narrative_ontology:cs_interpretation_layer_present('8d9b5f27-06c5-47cf-8efd-c093a2a97713').
narrative_ontology:cs_reading_relation('8d9b5f27-06c5-47cf-8efd-c093a2a97713', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('8d9b5f27-06c5-47cf-8efd-c093a2a97713', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('8d9b5f27-06c5-47cf-8efd-c093a2a97713', foundational, human_moral_judgment_is_irreducible).
narrative_ontology:cs_axiom_status(human_moral_judgment_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('8d9b5f27-06c5-47cf-8efd-c093a2a97713', human_moral_judgment_is_irreducible, deontological).
narrative_ontology:cs_axiom('8d9b5f27-06c5-47cf-8efd-c093a2a97713', foundational, martens_clause_prohibits_delegation_to_machines).
narrative_ontology:cs_axiom_status(martens_clause_prohibits_delegation_to_machines, holdable).
narrative_ontology:cs_axiom_grounding('8d9b5f27-06c5-47cf-8efd-c093a2a97713', martens_clause_prohibits_delegation_to_machines, deontological).
narrative_ontology:cs_reference_frame('8d9b5f27-06c5-47cf-8efd-c093a2a97713', human_centric_ihl_interpretation).
narrative_ontology:cs_drift_state('8d9b5f27-06c5-47cf-8efd-c093a2a97713', contemporary_autonomy_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8d9b5f27-06c5-47cf-8efd-c093a2a97713', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_dignity_advocates).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_planners).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bodies like the ICRC and international legal scholars who interpret and advocate for IHL principles. They benefit from the constraint by maintaining the centrality of human moral judgment and their interpretive authority in warfare.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, beneficiary).

% Responsible for developing and implementing military doctrine and technology. They bear the cost of the constraint through reduced operational efficiency and limitations on the development and deployment of fully autonomous lethal weapons systems (LAWS).
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_planners, payer,
    institutional, biographical, constrained, global).

% Companies and research institutions developing LAWS. They are victimized by the constraint as it prohibits or severely restricts the deployment of their most advanced, fully autonomous systems, forcing them to focus on human-supervised autonomy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, excluded).

% NGOs and civil society groups campaigning for ethical technology governance and the preservation of human dignity in armed conflict. They benefit from the constraint as it aligns with their core principles against delegating life-and-death decisions to machines.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_dignity_advocates, beneficiary,
    organized, generational, mobile, global).

% Populations in conflict zones who are the ultimate beneficiaries of IHL's protections. The constraint aims to reduce the risk of indiscriminate or disproportionate harm that might arise from machine-led lethal force decisions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% States actively investing in and developing autonomous weapons systems. They observe the debate and legal interpretations, often pushing back against strict prohibitions to preserve their military advantage, but must ultimately contend with IHL obligations.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_developing_laws, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, diffuse).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the application of lethal force in armed conflict remains subject to human moral judgment, thereby coordinating compliance with IHL principles of distinction and proportionality and maintaining accountability.
% TRANSFER_FUNCTION: Transfers the ultimate responsibility and moral burden of life-and-death decisions from machines back to human operators, imposing a cost on military operational efficiency and technological autonomy.
% ABSENT_VOICES: Proponents of fully autonomous lethal systems who argue for a purely outcomes-based interpretation of IHL, or those who believe machines can make more 'objective' decisions than humans, are structurally excluded from the interpretive framework of this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, militaries would rapidly accelerate the deployment of fully autonomous lethal systems, fundamentally altering the nature of warfare, accountability structures, and the perceived moral threshold for lethal force application. The international legal and ethical landscape would be profoundly reshaped.
% FOUNDING_PROBLEM: Preventing indiscriminate killing, disproportionate harm, and ensuring accountability for lethal force in armed conflict, as codified in IHL, particularly the Geneva Conventions and their Additional Protocols.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, human rights organizations, and a significant number of states consistently corroborate the ongoing necessity of these principles, especially in light of emerging military technologies. This is evidenced in UN debates, expert group meetings, and national policy statements.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because the constraint imposes substantial costs on military development and operational flexibility by limiting the use of advanced autonomous systems. Suppression (0.85) is also high, reflecting the active and ongoing efforts by IHL authorities and civil society to prohibit or strictly regulate LAWS, effectively collapsing the alternative of machine-led lethal force. The theater ratio (0.15) is low, as the requirement for human judgment is a genuine and actively defended principle, not merely performative. Accessibility collapse is high (0.88) because the core premise of this reading is that fully autonomous lethal systems are fundamentally incompatible with IHL, thus collapsing them as a legitimate alternative. Resistance is high (0.70) due to ongoing military and state efforts to develop and deploy LAWS, challenging this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IHL authorities and human dignity advocates, this constraint is a vital Rope, coordinating ethical conduct and upholding fundamental principles. From the perspective of military planners and developers, it is a Snare, extracting efficiency and suppressing innovation. The engine's classification as Tangled Rope captures this dual nature, acknowledging both the coordination function and the asymmetric extraction/suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities, human dignity advocates, and civilian populations are beneficiaries, as the constraint upholds principles they champion and offers protection. Military planners and autonomous weapons developers are victims, as they bear the costs of restricted technological development and operational limitations. The constraint's active enforcement ensures these costs are borne, while the benefits accrue to the integrity of IHL and human-centric ethics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaningful_human_control_definition,
    'What constitutes ''meaningful human control'' in the context of lethal autonomous weapons, and how can it be operationally verified?',
    'Development of internationally agreed-upon technical and doctrinal standards for human-machine interaction in lethal force application, coupled with independent verification mechanisms.',
    'A clear, verifiable definition would strengthen the constraint''s enforceability and reduce ambiguity, potentially increasing its effective suppression. Lack of clarity allows for ''control washing'' where human oversight is nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_human_control_definition, empirical, 'Ambiguity in the operationalization of human control requirements.').

omega_variable(
    empirical_feasibility_of_human_judgment,
    'Is it empirically feasible for humans to exercise ''meaningful moral judgment'' in high-speed, complex, and distributed combat environments where autonomous systems operate at machine speeds?',
    'Empirical studies and simulations of human-machine teaming in realistic combat scenarios, assessing cognitive load, decision latency, and moral reasoning capacity under stress.',
    'If human judgment is found to be empirically infeasible in critical scenarios, this reading''s core premise is undermined, potentially shifting the debate towards outcomes-based or categorical prohibition readings. If feasible, it strengthens the reading''s practical grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_feasibility_of_human_judgment, empirical, 'The practical limits of human moral judgment in autonomous warfare.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint best understood as a ''human_agency_reading'' or does it lean more towards a ''categorical_prohibition_reading'' or ''outcomes_based_reading''?',
    'Analysis of state practice and interpretive statements: if states consistently allow human-supervised autonomy but reject full autonomy, this reading is robust. If they push for full autonomy (outcomes-based) or total bans (categorical prohibition), the reading''s distinctness is challenged.',
    'If the distinction blurs, the classification of this constraint might shift towards a more absolute prohibition (Snare) or a more permissive, outcomes-focused approach (Rope/Piton), depending on the dominant interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the precise boundaries of this IHL kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2000, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(ihl__tr_t2005, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ihl__tr_t2024, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2000, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(ihl__be_t2005, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2000, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(ihl__su_t2005, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_development_norms).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'ihl_distinction_proportionality' kernel, each with different structural implications for autonomous weapons systems. This 'human_agency_reading' focuses on the irreducible need for human moral judgment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
