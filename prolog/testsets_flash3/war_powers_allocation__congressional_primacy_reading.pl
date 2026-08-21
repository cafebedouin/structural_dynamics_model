% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'congressional primacy' reading of the
 *   U.S. Constitution's war powers allocation, asserting that military force
 *   beyond immediate defense requires explicit congressional authorization.
 *   It is one reading of the broader 'war_powers_allocation' kernel, which is
 *   highly contested. This reading views executive unilateral action as an
 *   extraction of power from the legislative branch, requiring high
 *   suppression of alternative interpretations to maintain its structural
 *   integrity.
 *
 * KEY AGENTS:
 *   - congress: Agenda setter, institutional power, constrained exit
 *   - executive_branch: Payer, institutional power, constrained exit
 *   - constitutional_order: Beneficiary, analytical power, analytical exit
 *   - military_personnel: Payer, powerless, trapped exit
 *   - judiciary: Observer, institutional power, analytical exit
 *   - public_opinion: Excluded, organized power, constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.7).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.85).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '9f8e0b8a-1604-44d2-8483-3cdf752367ff').
narrative_ontology:cs_kernel_codification('9f8e0b8a-1604-44d2-8483-3cdf752367ff', fixed_text).
narrative_ontology:cs_authority_grounding('9f8e0b8a-1604-44d2-8483-3cdf752367ff', lineage).
narrative_ontology:cs_interpretation_layer_present('9f8e0b8a-1604-44d2-8483-3cdf752367ff').
narrative_ontology:cs_reading_relation('9f8e0b8a-1604-44d2-8483-3cdf752367ff', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f8e0b8a-1604-44d2-8483-3cdf752367ff', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('9f8e0b8a-1604-44d2-8483-3cdf752367ff', foundational, congressional_declaration_of_war_is_prerequisite).
narrative_ontology:cs_axiom_status(congressional_declaration_of_war_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('9f8e0b8a-1604-44d2-8483-3cdf752367ff', congressional_declaration_of_war_is_prerequisite, deontological).
narrative_ontology:cs_axiom('9f8e0b8a-1604-44d2-8483-3cdf752367ff', foundational, executive_power_is_limited_to_execution).
narrative_ontology:cs_axiom_status(executive_power_is_limited_to_execution, holdable).
narrative_ontology:cs_axiom_grounding('9f8e0b8a-1604-44d2-8483-3cdf752367ff', executive_power_is_limited_to_execution, deontological).
narrative_ontology:cs_reference_frame('9f8e0b8a-1604-44d2-8483-3cdf752367ff', constitutional_original_intent_separation_of_powers).
narrative_ontology:cs_drift_state('9f8e0b8a-1604-44d2-8483-3cdf752367ff', post_cold_war_executive_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f8e0b8a-1604-44d2-8483-3cdf752367ff', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, constitutional_order).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the body constitutionally mandated to declare war and raise armies, Congress is the primary beneficiary of this reading. It seeks to assert its authority over military deployments beyond immediate defense, viewing executive unilateral action as an infringement on its powers. Its exit options are constrained by political will and the executive's ability to act unilaterally.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% The executive branch, particularly the President as Commander-in-Chief, is the primary target of this constraint. It bears the cost of requiring explicit authorization for military actions, which can be seen as hindering rapid response or strategic flexibility. Its exit options are constrained by constitutional limits and political pressure, but it often seeks to expand its inherent authority.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, payer,
    institutional, biographical, constrained, global).

% The overall constitutional framework, emphasizing checks and balances and the separation of powers, benefits from this reading. It reinforces the idea that significant military action should be a collective decision, preventing unchecked executive power. This is an abstract beneficiary, representing the integrity of the system.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, constitutional_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(war_powers_allocation__congressional_primacy_reading, constitutional_order).

% Military personnel are victims of this constraint when deployed without clear congressional authorization, facing legal ambiguity and potential political fallout. They bear the direct risks of combat, and their legitimacy can be questioned if the constitutional basis for their deployment is contested. Their exit options are severely limited by military discipline.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_personnel, payer,
    powerless, immediate, trapped, global).

% The judiciary observes and interprets the constitutional allocation of war powers. While generally reluctant to intervene in political questions, its rulings can shape the boundaries of executive and legislative authority. It does not directly benefit or pay but acts as an arbiter of the constitutional framework.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Public opinion, while influential, is often excluded from the direct decision-making process regarding war powers. It would largely support a clear constitutional process for military action but is often presented with faits accomplis by the executive. Its influence is indirect through elections and protests.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, public_opinion, excluded,
    organized, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the decision to commit the nation to military force, beyond immediate self-defense, is a deliberative act involving both the legislative and executive branches, preventing unilateral executive action and fostering broader public consensus through elected representatives.
% TRANSFER_FUNCTION: Transfers the authority to initiate large-scale military force from the executive branch to the legislative branch, requiring explicit authorization. This also transfers political accountability for such actions to Congress.
% ABSENT_VOICES: The 'inherent executive authority' perspective is structurally excluded from this reading's framework, as it directly contradicts the premise of congressional primacy. Advocates for rapid, decisive executive action without legislative 'delays' are marginalized.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the executive branch would likely assert broader inherent authority to deploy military force, leading to more frequent unilateral actions. Congress would lose a significant check on executive power, fundamentally altering the balance of powers and potentially leading to more frequent or less scrutinized military engagements.
% FOUNDING_PROBLEM: The framers of the Constitution sought to prevent the executive from unilaterally committing the nation to war, a power historically abused by monarchs, by vesting the power to declare war in the legislative branch.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars widely corroborate the framers' intent to limit executive war-making power. Congressional debates and resolutions consistently reaffirm this principle, even when executive actions challenge it. Legal analyses from non-partisan bodies also support the ongoing relevance of this founding problem.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) reflects the significant power transfer from the executive to Congress that this reading demands, often resisted by the executive. Suppression (0.85) is high because this reading requires active legal and political enforcement to counter the executive's tendency towards unilateral action and claims of inherent authority. The theater ratio (0.4) indicates that while there is genuine constitutional debate, a substantial portion of executive action is framed as 'defensive' or 'limited' to avoid explicit authorization, creating a performative aspect to compliance. The metrics show a slight increase in extractiveness and suppression over time, reflecting the ongoing contestation and the executive's persistent efforts to bypass this constraint.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's perspective, this constraint is a necessary Rope, ensuring constitutional balance. From the Executive's perspective, it is a Snare, unduly restricting its ability to act decisively in national security. Military personnel experience it as a Tangled Rope, caught between constitutional mandates and executive orders. The engine's per-seat classification will reflect these divergences based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress is a beneficiary (d=0.0-0.1) as it gains authority. The Executive Branch is a target (d=0.8-0.9) as its power is constrained. Military personnel are also targets (d=0.9-1.0) due to their trapped exit and direct exposure to the consequences of contested deployments. The constitutional order is an abstract beneficiary, representing the system's integrity. The judiciary and public opinion are observers or excluded, with directionality near symmetric or slightly targeted depending on their engagement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate is to uphold the constitutional separation of powers regarding war. It is not mandatrophic; the founding problem (preventing unilateral executive war-making) is still live and actively contested. The classification as Tangled Rope reflects the ongoing coordination function (ensuring deliberative war decisions) intertwined with asymmetric extraction (from the executive by Congress) and active enforcement against executive overreach. It prevents mislabeling as a Snare by acknowledging the genuine coordination problem it addresses, even amidst the power struggle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    executive_unilateralism_scope,
    'What specific types of military action (e.g., counter-terrorism strikes, humanitarian interventions, cyber warfare) fall under ''immediate defense'' versus requiring ''explicit congressional authorization''?',
    'Supreme Court rulings clarifying the scope of Commander-in-Chief powers, or legislative action defining ''hostilities'' and ''imminent threat'' more precisely.',
    'A narrower definition of ''immediate defense'' would increase the constraint''s effective extractiveness on the executive; a broader definition would reduce it, shifting the classification towards a more executive-friendly type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_unilateralism_scope, conceptual, 'Ambiguity in defining the boundary between executive''s inherent defense powers and Congress''s authorization power.').

omega_variable(
    congressional_will_enforcement,
    'To what extent does Congress genuinely exercise its war powers, versus deferring to or tacitly approving executive actions due to political expediency or lack of will?',
    'Analysis of congressional voting records on war authorizations, use of the War Powers Resolution, and legislative oversight of military operations over time.',
    'If Congress frequently defers, the constraint''s effective suppression of executive action is lower than stated, and its theater ratio is higher, indicating a drift towards a Piton or a more executive-dominated Tangled Rope. If Congress actively asserts its power, the constraint''s force is maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_will_enforcement, empirical, 'The gap between Congress''s constitutional authority and its actual exercise of that authority.').

omega_variable(
    reading_conflict_location,
    'Where is the core disagreement between the ''congressional_primacy_reading'' and its siblings (''inherent_executive_reading'', ''functional_accommodation_reading'') located structurally?',
    'Comparative analysis of the foundational axioms and reference frames of each reading, identifying the specific constitutional clauses or historical precedents that are interpreted differently.',
    'This reading asserts a deontological axiom of legislative control over war. The ''inherent_executive_reading'' contradicts this by asserting an instrumental axiom of executive efficiency in national security. The ''functional_accommodation_reading'' seeks to bridge this by proposing a context-dependent allocation. The impact is on the coherence and stability of the overall ''war_powers_allocation'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_conflict_location, conceptual, 'The specific structural element (e.g., constitutional clause, historical precedent, normative axiom) where the readings diverge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__congressional_primacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__congressional_primacy_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__congressional_primacy_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(war__tr_t60, war_powers_allocation__congressional_primacy_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(war__tr_t70, war_powers_allocation__congressional_primacy_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(war__be_t60, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement(war__be_t70, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 70, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.83).
narrative_ontology:measurement(war__su_t60, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 60, 0.86).
narrative_ontology:measurement(war__su_t70, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 70, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, executive_order_authority).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, national_security_classification_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. This 'congressional_primacy_reading' emphasizes legislative control, contrasting with the 'inherent_executive_reading' and 'functional_accommodation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
