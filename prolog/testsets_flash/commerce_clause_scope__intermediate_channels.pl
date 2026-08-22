% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope: Intermediate Channels Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint describes the 'intermediate channels' reading of the
 *   Commerce Clause, which defines the scope of federal power over interstate
 *   commerce. It acknowledges federal authority over channels,
 *   instrumentalities, and activities substantially affecting commerce, but
 *   introduces limiting principles for non-economic activity, aggregation,
 *   and attenuated causal chains. This reading attempts to balance federal
 *   and state power, but its limiting principles are often criticized for
 *   their conceptual instability.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda setter (institutional/analytical) - defines and enforces the scope of its own power.
 *   - state_governments: Beneficiary/Payer (institutional/constrained) - benefits from retained autonomy in certain areas but pays by being subject to federal preemption in others.
 *   - regulated_entities_with_non_economic_activity: Payer (moderate/constrained) - bears the cost of federal regulation if their activity is deemed 'economic' or sufficiently connected to interstate commerce.
 *   - supreme_court: Agenda setter/Observer (institutional/analytical) - adjudicates the boundaries of federal power, shaping the interpretation of the Commerce Clause.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.45).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.6).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope: Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'dc994ac4-5d49-4987-8d71-21ff3d159c90').
narrative_ontology:cs_kernel_codification('dc994ac4-5d49-4987-8d71-21ff3d159c90', fixed_text).
narrative_ontology:cs_authority_grounding('dc994ac4-5d49-4987-8d71-21ff3d159c90', lineage).
narrative_ontology:cs_interpretation_layer_present('dc994ac4-5d49-4987-8d71-21ff3d159c90').
narrative_ontology:cs_reading_relation('dc994ac4-5d49-4987-8d71-21ff3d159c90', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_reading_relation('dc994ac4-5d49-4987-8d71-21ff3d159c90', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_axiom('dc994ac4-5d49-4987-8d71-21ff3d159c90', foundational, federal_power_limited_by_categorical_distinctions).
narrative_ontology:cs_axiom_status(federal_power_limited_by_categorical_distinctions, holdable).
narrative_ontology:cs_axiom_grounding('dc994ac4-5d49-4987-8d71-21ff3d159c90', federal_power_limited_by_categorical_distinctions, conventional).
narrative_ontology:cs_axiom('dc994ac4-5d49-4987-8d71-21ff3d159c90', foundational, national_economic_problems_require_federal_solutions).
narrative_ontology:cs_axiom_status(national_economic_problems_require_federal_solutions, holdable).
narrative_ontology:cs_axiom_grounding('dc994ac4-5d49-4987-8d71-21ff3d159c90', national_economic_problems_require_federal_solutions, instrumental).
narrative_ontology:cs_reference_frame('dc994ac4-5d49-4987-8d71-21ff3d159c90', post_new_deal_federalism).
narrative_ontology:cs_drift_state('dc994ac4-5d49-4987-8d71-21ff3d159c90', contemporary_judicial_review, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dc994ac4-5d49-4987-8d71-21ff3d159c90', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence_of_commerce_power).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, regulated_entities_with_non_economic_activity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary enforcer and beneficiary of the Commerce Clause, the federal government defines and expands its regulatory reach over economic activities. It benefits from the ability to address national economic problems but is constrained by judicial interpretations of its limits.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% States benefit from retaining authority over local, non-economic matters (e.g., family law, criminal law, education). However, they are payers when federal regulations preempt state laws or impose unfunded mandates, limiting their autonomy and policy choices.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_governments, payer).

% These entities (e.g., local schools, gun owners, victims of gender-motivated violence) bear the cost of federal regulation if their activities, though non-economic, are deemed to fall under the Commerce Clause due to a jurisdictional element or connection to channels/instrumentalities. Their ability to resist is limited by federal supremacy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, regulated_entities_with_non_economic_activity, payer,
    moderate, biographical, constrained, local).

% The Supreme Court is the ultimate arbiter of the Commerce Clause's scope. Its interpretations shape the boundaries of federal power, influencing the balance between federal and state authority. It observes the practical effects of its rulings but also actively sets the agenda for federalism.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, supreme_court, observer).

% The conceptual coherence of the Commerce Clause's scope suffers from the unstable and manipulable distinctions (e.g., economic vs. non-economic activity, attenuated causal chains) introduced by this reading. This creates legal uncertainty and makes consistent application challenging.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, conceptual_coherence_of_commerce_power, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, conceptual_coherence_of_commerce_power).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates federal and state regulatory authority by defining the boundaries of federal power over interstate commerce, allowing for national solutions to economic problems while preserving some state autonomy.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from states to the federal government for activities deemed to fall within the Commerce Clause's scope, while reserving other areas for state control.
% ABSENT_VOICES: Advocates for a strictly limited federal government (e.g., states' rights proponents, libertarians) would argue that the limiting principles are insufficient and that federal power remains too expansive, encroaching on traditional state prerogatives. They are often marginalized in national policy debates.
% DISAPPEARANCE_RATIONALE: If this reading of the Commerce Clause vanished, the balance of power between federal and state governments would fundamentally shift. Either federal power would become virtually unlimited (reverting to a broad effects test) or severely curtailed (reverting to a narrow originalist view), leading to a complete reorganization of regulatory authority and economic governance.
% FOUNDING_PROBLEM: The original problem was to create a national market by preventing states from erecting trade barriers and to allow the federal government to address national economic issues that individual states could not effectively manage.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars widely corroborate the founding problem of national economic coordination. The status as 'live' is attested by ongoing debates over federal regulatory authority in areas like environmental protection, healthcare, and technology, where national solutions are often sought for issues with interstate impact. Legal scholars and political scientists outside the federal government confirm the continued relevance of this problem.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).
:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while federal power is extensive, the limiting principles do impose some checks. Suppression (0.6) is significant as federal authority can preempt state law and regulate broad swathes of economic activity. Theater ratio (0.2) reflects that while the limiting principles are real, their application can be inconsistent, leading to a degree of performative adherence rather than strict structural constraint. The claimed type is Tangled Rope because it genuinely coordinates national economic activity (benefiting the federal government) but also extracts from states and individuals through federal preemption and regulation, requiring active enforcement to maintain the balance.
 *
 * PERSPECTIVAL GAP:
 *   The federal government views this as a necessary coordination mechanism for a national economy, with appropriate checks on its power. State governments and regulated entities, however, often experience the limiting principles as insufficient or inconsistently applied, leading to federal overreach and extraction. The Supreme Court's role is to mediate these differing perspectives, but its interpretations themselves become part of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a primary beneficiary, as the constraint grants it broad authority over national economic issues. State governments are both beneficiaries (retaining autonomy over non-economic local matters) and payers (subject to federal preemption). Regulated entities engaging in non-economic activity are payers, as they bear the cost of federal regulation if their activity is deemed to fall within the Commerce Clause's scope. The conceptual coherence of the Commerce Power is a victim, as the limiting principles introduce instability and manipulability.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic; the problem of defining federal power over commerce remains live. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring the coordination function for national markets) or a pure Rope (ignoring the extraction from states and individuals due to federal preemption and the instability of limiting principles). The active enforcement requirement and the presence of both beneficiaries and victims are key to this classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_distinction,
    'Is the distinction between ''economic'' and ''non-economic'' activity a stable and predictable limiting principle, or is it subject to judicial manipulation?',
    'Analysis of future Supreme Court decisions applying the distinction across diverse factual scenarios; consistency of application across different judicial philosophies.',
    'If unstable, the constraint''s suppression of federal power is more theatrical than real, increasing effective extraction from states and individuals by allowing federal overreach under a guise of limits. If stable, it genuinely limits federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_distinction, conceptual, 'Ambiguity of the economic/non-economic distinction in limiting federal power.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''intermediate_channels'' reading of the ''commerce_clause_scope'' kernel. What would a ''broad_effects_test'' or ''narrow_originalist'' reading change structurally?',
    'Comparative legal analysis of judicial outcomes under each reading; legislative responses to shifts in judicial interpretation.',
    'A ''broad_effects_test'' reading would expand federal power, increasing extractiveness from states and individuals. A ''narrow_originalist'' reading would severely restrict federal power, shifting extractiveness to states and potentially creating coordination failures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative Commerce Clause readings on federal power and state autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.2).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.2).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__intermediate_channels, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__intermediate_channels, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__intermediate_channels, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__intermediate_channels, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__intermediate_channels, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause scope kernel. Each reading represents a different structural claim about the extent of federal power, with differing extractiveness and suppression profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
