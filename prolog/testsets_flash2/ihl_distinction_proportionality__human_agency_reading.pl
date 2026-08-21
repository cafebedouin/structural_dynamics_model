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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency Requirement for Lethal Force Application
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'human agency' reading of International
 *   Humanitarian Law's (IHL) distinction and proportionality obligations,
 *   specifically in the context of lethal autonomous weapons systems (LAWS).
 *   It asserts that irreducible human moral judgment is required for lethal
 *   force application, prohibiting the delegation of life/death decisions to
 *   machines based on Martens Clause principles of humanity. This reading
 *   effectively suppresses the development and deployment of fully autonomous
 *   systems, authorizing only human-supervised autonomy. It benefits IHL
 *   interpretive authorities by maintaining their centrality in defining
 *   ethical boundaries, while imposing costs on military operational
 *   efficiency and autonomous weapons developers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.85).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.75).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, snare).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force Application").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'd2d0215a-4617-464c-b3fe-d1e18d497585').
narrative_ontology:cs_kernel_codification('d2d0215a-4617-464c-b3fe-d1e18d497585', formalized).
narrative_ontology:cs_authority_grounding('d2d0215a-4617-464c-b3fe-d1e18d497585', lineage).
narrative_ontology:cs_interpretation_layer_present('d2d0215a-4617-464c-b3fe-d1e18d497585').
narrative_ontology:cs_reading_relation('d2d0215a-4617-464c-b3fe-d1e18d497585', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2d0215a-4617-464c-b3fe-d1e18d497585', ihl_distinction_proportionality__outcomes_based_reading, coexists_with).
narrative_ontology:cs_axiom('d2d0215a-4617-464c-b3fe-d1e18d497585', foundational, human_moral_judgment_is_irreducible).
narrative_ontology:cs_axiom_status(human_moral_judgment_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('d2d0215a-4617-464c-b3fe-d1e18d497585', human_moral_judgment_is_irreducible, deontological).
narrative_ontology:cs_axiom('d2d0215a-4617-464c-b3fe-d1e18d497585', foundational, martens_clause_prohibits_delegation_of_life_death_to_machines).
narrative_ontology:cs_axiom_status(martens_clause_prohibits_delegation_of_life_death_to_machines, holdable).
narrative_ontology:cs_axiom_grounding('d2d0215a-4617-464c-b3fe-d1e18d497585', martens_clause_prohibits_delegation_of_life_death_to_machines, deontological).
narrative_ontology:cs_reference_frame('d2d0215a-4617-464c-b3fe-d1e18d497585', human_centric_ihl_interpretation).
narrative_ontology:cs_drift_state('d2d0215a-4617-464c-b3fe-d1e18d497585', contemporary_ai_advances, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d2d0215a-4617-464c-b3fe-d1e18d497585', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, human_rights_advocates).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, states_pursuing_full_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Committee of the Red Cross (ICRC) and other expert bodies that interpret and promote IHL. They assert the necessity of human control over lethal force decisions to uphold the principles of distinction and proportionality, maintaining their centrality in defining ethical boundaries for warfare.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Organizations and individuals campaigning for human control over autonomous weapons. They benefit from this reading as it aligns with their advocacy for preventing machine-decided killing and upholding human dignity in armed conflict.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Military forces seeking to leverage fully autonomous systems for speed, precision, and reduced risk to personnel. This constraint imposes a 'human-in-the-loop' or 'human-on-the-loop' requirement, limiting the potential for full automation and potentially reducing operational efficiency in certain scenarios.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency, payer,
    institutional, biographical, constrained, global).

% Defense contractors and research institutions developing AI-powered lethal autonomous weapons systems (LAWS). This reading restricts the design space for their products, requiring them to integrate human oversight mechanisms, which can increase complexity and cost, and limit market potential for fully autonomous solutions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    powerful, biographical, constrained, global).

% Nations investing heavily in and advocating for the deployment of fully autonomous weapons systems, viewing them as a strategic advantage. This reading directly challenges their policy and military doctrine, imposing legal and ethical barriers to their desired capabilities.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_pursuing_full_autonomy, payer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation and application of IHL principles (distinction and proportionality) in the context of emerging military technologies, ensuring a common understanding of legal and ethical boundaries for lethal force.
% TRANSFER_FUNCTION: Transfers the ultimate responsibility and moral burden of lethal force decisions from machines to human operators, and transfers the authority to define acceptable weapon systems from military developers to IHL interpretive bodies.
% ABSENT_VOICES: Proponents of a purely outcomes-based approach to IHL, who argue that the law should focus on whether systems achieve better compliance with distinction and proportionality, regardless of human involvement. They are often excluded from the core IHL interpretive discussions that emphasize human agency.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and ethical landscape for autonomous weapons would fundamentally shift. Militaries would be free to develop and deploy fully autonomous systems without human oversight, potentially leading to a rapid proliferation of machine-decided killing and a redefinition of accountability in warfare.
% FOUNDING_PROBLEM: The problem of ensuring compliance with IHL principles (distinction and proportionality) in the face of increasingly complex and autonomous military technologies, particularly concerning the moral and legal accountability for lethal force decisions.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and numerous human rights organizations consistently attest to the ongoing and evolving nature of this problem, citing rapid advancements in AI and robotics. Independent legal scholars and ethicists also corroborate the live status of the challenge, emphasizing the need for clear legal and ethical boundaries.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high because this reading imposes significant limitations on the design and deployment of LAWS, effectively extracting the potential for full automation from military and industry actors. Suppression (0.75) is also high, as it requires active legal and ethical enforcement to prevent states from pursuing fully autonomous systems. The theater ratio is low (0.1) because the interpretive authorities genuinely believe in and actively promote this principle, with little performative maintenance. Accessibility collapse is moderate (0.6) as alternatives (fully autonomous systems) are significantly constrained but not entirely eliminated, and resistance is high (0.7) from states and developers pushing for greater autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IHL interpretive authorities and human rights advocates, this constraint is a necessary 'rope' or 'scaffold' to uphold fundamental humanitarian principles. However, from the perspective of military planners and autonomous weapons developers, it operates as a 'snare' that extracts strategic advantage and technological potential, imposing significant costs and limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities and human rights advocates are beneficiaries, as this reading reinforces their mandate and ethical positions (low d). Military operational efficiency, autonomous weapons developers, and states pursuing full autonomy are targets, as they bear the costs of restricted autonomy and increased oversight requirements (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the underlying problem of ensuring human control over lethal force in armed conflict remains live and increasingly urgent with technological advancements. The classification as a 'snare' (despite the claimed 'rope' or 'scaffold' by beneficiaries) highlights the extractive impact on those seeking full autonomy, preventing mislabeling genuine extraction as mere coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_definition,
    'What constitutes ''irreducible human moral judgment'' in the context of lethal force application, and at what point in the kill chain must it occur?',
    'Development of internationally agreed-upon technical and operational definitions for ''meaningful human control'' or ''human-in-the-loop'' requirements, potentially through a legally binding instrument.',
    'A precise definition would clarify the boundaries for LAWS development, potentially reducing the perceived extractiveness for developers by providing clear design parameters, or increasing it if the definition is highly restrictive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_definition, conceptual, 'Ambiguity in defining the precise nature and timing of required human moral judgment.').

omega_variable(
    martens_clause_scope,
    'To what extent does the Martens Clause, as a principle of humanity and public conscience, categorically prohibit machine-decided killing, even if such systems could achieve superior IHL compliance?',
    'Further legal scholarship and state practice clarifying the normative weight of the Martens Clause in relation to technological means of warfare, potentially through an advisory opinion from the International Court of Justice.',
    'If the Martens Clause is interpreted as a categorical prohibition, it strengthens this reading''s suppressive force. If it is seen as more flexible, allowing for outcomes-based considerations, it weakens the constraint''s extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(martens_clause_scope, conceptual, 'Uncertainty regarding the categorical prohibitive scope of the Martens Clause.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal/ethical barriers) or internalized (moral/reputational costs for states/developers)?',
    'Post-deployment analysis of LAWS in states that disregard this reading: if states still face significant reputational or diplomatic costs, internalized suppression is a factor. If not, it''s purely structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as actors self-regulate to avoid moral opprobrium. If purely structural, removal of legal barriers would lead to rapid proliferation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for LAWS development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ihl__tr_t2018, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(ihl__tr_t2021, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(ihl__tr_t2024, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(ihl__be_t2018, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2018, 0.75).
narrative_ontology:measurement(ihl__be_t2021, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(ihl__su_t2018, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(ihl__su_t2021, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'IHL distinction and proportionality' kernel. It focuses on the requirement for human agency in lethal force decisions, distinct from a categorical prohibition or a purely outcomes-based approach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
