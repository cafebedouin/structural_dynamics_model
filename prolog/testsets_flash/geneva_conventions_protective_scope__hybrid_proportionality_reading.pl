% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope (Hybrid Proportionality Reading)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid proportionality reading' of the
 *   Geneva Conventions' protective scope. It posits that protections scale by
 *   conflict type (AP I for international armed conflict, AP II/Common
 *   Article 3 for non-international) and that proportionality analysis
 *   determines their application. This reading acknowledges the
 *   differentiated legal regimes but attempts to integrate a universalizing
 *   principle (proportionality) across them, leading to a complex and often
 *   ambiguous application that can be leveraged by powerful actors. The
 *   constraint is claimed as a rope by its proponents, but its operational
 *   metrics reveal significant extraction and suppression.
 *
 * KEY AGENTS:
 *   - powerful_states: Agenda-setter (institutional/arbitrage) — define conflict types, interpret proportionality
 *   - military_commanders: Beneficiary (institutional/constrained) — operate within interpreted rules, benefit from ambiguity
 *   - non_state_armed_groups: Payer (powerless/trapped) — often denied full protections, subject to proportionality calculus by adversaries
 *   - civilians_in_non_international_conflict: Victim (powerless/trapped) — receive reduced protections, bear costs of proportionality judgments
 *   - international_legal_scholars: Observer (analytical/analytical) — analyze application, critique interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope (Hybrid Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '7884a591-8e20-4b22-b236-967fa0c64ad3').
narrative_ontology:cs_kernel_codification('7884a591-8e20-4b22-b236-967fa0c64ad3', formalized).
narrative_ontology:cs_authority_grounding('7884a591-8e20-4b22-b236-967fa0c64ad3', lineage).
narrative_ontology:cs_interpretation_layer_present('7884a591-8e20-4b22-b236-967fa0c64ad3').
narrative_ontology:cs_reading_relation('7884a591-8e20-4b22-b236-967fa0c64ad3', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('7884a591-8e20-4b22-b236-967fa0c64ad3', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('7884a591-8e20-4b22-b236-967fa0c64ad3', foundational, proportionality_as_balancing_principle).
narrative_ontology:cs_axiom_status(proportionality_as_balancing_principle, holdable).
narrative_ontology:cs_axiom_grounding('7884a591-8e20-4b22-b236-967fa0c64ad3', proportionality_as_balancing_principle, conventional).
narrative_ontology:cs_axiom('7884a591-8e20-4b22-b236-967fa0c64ad3', foundational, differentiated_conflict_regimes).
narrative_ontology:cs_axiom_status(differentiated_conflict_regimes, holdable).
narrative_ontology:cs_axiom_grounding('7884a591-8e20-4b22-b236-967fa0c64ad3', differentiated_conflict_regimes, conventional).
narrative_ontology:cs_reference_frame('7884a591-8e20-4b22-b236-967fa0c64ad3', post_additional_protocols_framework).
narrative_ontology:cs_drift_state('7884a591-8e20-4b22-b236-967fa0c64ad3', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7884a591-8e20-4b22-b236-967fa0c64ad3', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As signatories and enforcers of the Geneva Conventions, they define conflict classifications and interpret proportionality, often in ways that align with their strategic interests. They benefit from the flexibility and ambiguity of the hybrid approach.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).

% They operate within the legal framework, benefiting from the discretion afforded by proportionality assessments and conflict classification. This allows them to pursue military objectives while claiming adherence to IHL, even if the interpretation is self-serving.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders, beneficiary,
    institutional, biographical, constrained, global).

% Often denied full combatant status and the protections of AP I, they are subject to the more limited protections of AP II/Common Article 3. Their actions are judged by states under a proportionality calculus they have no input into, leading to disproportionate harm.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    powerless, immediate, trapped, local).

% They receive fewer protections under AP II/Common Article 3 compared to those in international armed conflicts. Their lives and property are subject to proportionality judgments made by warring parties, often resulting in significant harm with little recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict, payer,
    powerless, immediate, trapped, local).

% States or entities with limited military or political power who find their conflicts classified in ways that reduce their protections, or whose actions are disproportionately scrutinized under proportionality rules by more powerful adversaries.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_in_conflict, payer,
    moderate, biographical, constrained, regional).

% They analyze the application of IHL, critique interpretations of conflict classification and proportionality, and advocate for clearer, more equitable standards. Their influence is primarily through academic discourse and policy recommendations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% They advocate for broader protective scopes and stricter application of proportionality, often documenting violations. While they provide critical services, their direct influence on legal interpretation and enforcement by states is limited, and their calls for expanded protection are often resisted.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for regulating the conduct of hostilities and protecting victims of armed conflict, adapting the level of protection to the nature of the conflict (international vs. non-international) and balancing military necessity with humanitarian considerations through proportionality.
% TRANSFER_FUNCTION: Transfers interpretive discretion and reduced accountability to powerful states and military commanders, while transferring uncertainty, reduced protection, and disproportionate harm to non-state armed groups, civilians in non-international conflicts, and weaker parties.
% ABSENT_VOICES: Humanitarian organizations and victims' advocacy groups are often excluded from the direct interpretation and enforcement mechanisms, despite being primary stakeholders. They would argue for a more expansive and less conditional application of protections, challenging the current balance of military necessity and humanitarian concerns.
% DISAPPEARANCE_RATIONALE: If this hybrid proportionality reading of the Geneva Conventions vanished, the legal landscape governing armed conflict would be thrown into chaos. States would lose a framework for justifying their actions, and victims would lose even the limited protections currently afforded, leading to a significant increase in unregulated violence and a scramble for new legal or ethical frameworks.
% FOUNDING_PROBLEM: The original problem was to codify humanitarian protections in armed conflict, recognizing the different legal and practical realities of international (state-on-state) versus non-international (internal) conflicts, and to provide a mechanism for balancing military necessity with humanitarian concerns.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and military commanders argue the problem is still live, citing the ongoing need for flexible rules in complex conflicts. International legal scholars and humanitarian organizations, however, contend that while the need for protection remains, the current 'hybrid proportionality' reading has drifted from its original intent, becoming a tool for justifying harm rather than preventing it, thus rendering the founding problem 'dead' in its original, pure form, and now serving a different, more extractive function. This is corroborated by extensive reports from UN bodies and NGOs documenting civilian harm and interpretive abuses.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the inherent ambiguity in classifying conflicts and applying proportionality, which powerful states and military commanders can exploit to their advantage, reducing their obligations and increasing harm to weaker parties. Suppression (0.75) is high because the legal framework is enforced by states themselves, who have a strong incentive to interpret it in their favor, and there are limited effective mechanisms for victims to challenge these interpretations. Theater ratio (0.20) is moderate; while there is genuine legal and humanitarian effort, a portion of the 'proportionality' discourse serves to legitimize actions that would otherwise be clear violations. Accessibility collapse (0.40) is moderate, as some protections always exist, but the most robust ones are often inaccessible to those who need them most. Resistance (0.50) is also moderate, coming from human rights organizations and some states, but it struggles against the power of state interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and military commanders perceive this as a legitimate, albeit complex, framework for regulating conflict (closer to a rope), balancing military necessity with humanitarian concerns. For non-state armed groups and civilians in non-international conflicts, it often functions as a snare, where their protections are conditional, reduced, and subject to the interpretation of their adversaries. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and military commanders are beneficiaries (d near 0.0) as they retain significant interpretive discretion and can leverage the framework to achieve military objectives while claiming compliance. Non-state armed groups and civilians in non-international conflicts are victims (d near 1.0) as their protective status is often diminished or denied, and they bear the brunt of 'proportionality' judgments made by their adversaries. Weaker parties in conflict are also victims, as the ambiguity disproportionately affects them.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely attempts to coordinate humanitarian protection in conflict (beneficiaries of protection exist, even if limited) but simultaneously enables asymmetric extraction through interpretive ambiguity and differential application. The mandatrophy analysis focuses on whether the 'coordination' function (humanitarian protection) has atrophied relative to the 'extraction' function (state discretion/impunity). The rising extractiveness and suppression over time suggest a drift towards greater extraction, indicating a potential mandatrophy where the original mandate of protection is being overshadowed by the utility of legal ambiguity for powerful actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_classification_ambiguity,
    'Is the classification of a conflict as international or non-international genuinely objective, or is it subject to political interpretation by powerful states?',
    'Independent international judicial review of conflict classifications, or a clear, universally accepted set of criteria for classification that is not subject to state veto.',
    'If classification is politically driven, the protective scope is a snare for weaker parties, as powerful states can unilaterally reduce their obligations. If objective, it functions as a more legitimate, albeit complex, rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conflict_classification_ambiguity, empirical, 'Ambiguity in conflict classification affects protective scope.').

omega_variable(
    proportionality_calculus_subjectivity,
    'To what extent is the proportionality calculus (balancing military advantage against civilian harm) a genuinely objective legal standard, versus a subjective judgment influenced by military necessity and political goals?',
    'Development of universally accepted, quantifiable metrics for military advantage and civilian harm, and independent, ex-post facto review of proportionality assessments by non-military bodies.',
    'If highly subjective, proportionality becomes a cover for excessive civilian harm, making the constraint more extractive. If objective, it serves as a legitimate, if difficult, coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_calculus_subjectivity, conceptual, 'Subjectivity in proportionality calculus affects protective scope.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''geneva_conventions_protective_scope'' kernel. This ''hybrid_proportionality_reading'' emphasizes scaled protections and proportionality. What would change if the ''state_centric_reading'' or ''universal_rights_reading'' were adopted?',
    'Analysis of legal precedent and state practice under alternative interpretive frameworks.',
    'The ''state_centric_reading'' would narrow the victim set and increase extraction from non-state actors. The ''universal_rights_reading'' would broaden the victim set and reduce extraction from all non-combatants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is a specific reading of the Geneva Conventions'' protective scope, with alternative interpretations having different structural impacts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'Geneva Conventions Protective Scope' kernel. This 'hybrid_proportionality_reading' focuses on scaled protections and proportionality, distinct from the 'state_centric_reading' (focused on combatant status) and the 'universal_rights_reading' (focused on universal human rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
