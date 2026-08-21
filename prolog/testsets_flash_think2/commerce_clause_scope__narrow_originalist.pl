% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope: Narrow Originalist Reading
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   This constraint instantiates the 'narrow_originalist' reading of the
 *   Commerce Clause's scope, which holds that federal power is limited to
 *   facilitating trade crossing state lines and removing state barriers, not
 *   restricting intrastate economic activity. Sibling readings,
 *   'broad_effects_test' and 'intermediate_channels', propose significantly
 *   wider federal authority. This reading emphasizes state autonomy and a
 *   limited federal role in economic regulation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.25).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.3).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.25).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope: Narrow Originalist Reading").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '506b28b9-3451-4f7e-9110-40d4f5e894eb').
narrative_ontology:cs_kernel_codification('506b28b9-3451-4f7e-9110-40d4f5e894eb', fixed_text).
narrative_ontology:cs_authority_grounding('506b28b9-3451-4f7e-9110-40d4f5e894eb', lineage).
narrative_ontology:cs_interpretation_layer_present('506b28b9-3451-4f7e-9110-40d4f5e894eb').
narrative_ontology:cs_reading_relation('506b28b9-3451-4f7e-9110-40d4f5e894eb', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('506b28b9-3451-4f7e-9110-40d4f5e894eb', commerce_clause_scope__intermediate_channels, forecloses).
narrative_ontology:cs_axiom('506b28b9-3451-4f7e-9110-40d4f5e894eb', foundational, commerce_is_trade_crossing_state_lines).
narrative_ontology:cs_axiom_status(commerce_is_trade_crossing_state_lines, holdable).
narrative_ontology:cs_axiom_grounding('506b28b9-3451-4f7e-9110-40d4f5e894eb', commerce_is_trade_crossing_state_lines, conventional).
narrative_ontology:cs_axiom('506b28b9-3451-4f7e-9110-40d4f5e894eb', foundational, regulate_means_facilitate_not_restrict).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_restrict, holdable).
narrative_ontology:cs_axiom_grounding('506b28b9-3451-4f7e-9110-40d4f5e894eb', regulate_means_facilitate_not_restrict, conventional).
narrative_ontology:cs_reference_frame('506b28b9-3451-4f7e-9110-40d4f5e894eb', founding_era_limited_federalism).
narrative_ontology:cs_drift_state('506b28b9-3451-4f7e-9110-40d4f5e894eb', post_new_deal_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('506b28b9-3451-4f7e-9110-40d4f5e894eb', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_environmental_labor_civil_rights_agencies).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement_in_recalcitrant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_advocacy_groups_uniformity_civil_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreter and enforcer of the Commerce Clause, tasked with upholding the constitutional framework. This reading limits their ability to sanction broad federal legislation, requiring them to strike down laws that exceed the narrow definition of 'commerce'.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from retained sovereignty over intrastate economic activity, allowing for diverse state-level regulatory approaches without federal preemption. This reading protects their autonomy from federal encroachment.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, mobile, national).

% Benefit from reduced federal regulatory burdens and the ability to operate under state-specific rules, which may be less stringent or more tailored to local conditions. They are shielded from national mandates that might not suit their scale or market.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses, beneficiary,
    moderate, biographical, mobile, local).

% Bear the cost of limited federal power, as their ability to enact and enforce national standards for environmental protection, labor conditions, or civil rights is severely curtailed. They must rely on state action, which may be inconsistent or insufficient.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_environmental_labor_civil_rights_agencies, payer,
    institutional, generational, constrained, national).

% Advocate for national solutions to social and economic problems, and thus bear the cost of a narrow Commerce Clause interpretation that prevents federal intervention. Their goals of uniform standards and protections are frustrated by this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_advocacy_groups_uniformity_civil_rights, payer,
    organized, generational, constrained, national).

% Advocate for this reading based on historical textual analysis and original intent. They observe and critique judicial decisions that depart from this narrow interpretation, seeking to restore what they view as the correct constitutional balance.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, constitutional_scholars_originalist, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate free trade among states by preventing state-imposed protectionist barriers and ensuring uniform commercial rules for interstate transactions, while simultaneously preserving state sovereignty over intrastate economic activity.
% TRANSFER_FUNCTION: Transfers significant regulatory authority over intrastate economic and social matters from the federal government to state governments. It also transfers the burden of addressing national collective action problems (e.g., civil rights, environmental protection) to individual states.
% ABSENT_VOICES: Those who believe in a robust national government capable of addressing collective action problems that states cannot or will not solve, such as national civil rights protections, environmental standards, or economic stability measures. Their arguments for a more unified national approach are excluded by this interpretation.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, federal power would expand dramatically, leading to a reorganization of regulatory authority. Many state laws would be preempted, and federal agencies would gain broad authority to regulate intrastate activities, fundamentally altering the balance of federalism.
% FOUNDING_PROBLEM: To prevent states from erecting protectionist barriers that would stifle interstate commerce, thereby creating a unified national market, while simultaneously limiting the scope of federal power to preserve a federal system of government and state autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Constitutional Convention debates, the Federalist Papers, and early Supreme Court cases (e.g., Gibbons v. Ogden) corroborate the dual intent to both facilitate commerce and limit federal power. Legal historians and political scientists, even those who dispute the narrow interpretation, acknowledge this foundational tension and the historical context of the clause.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.25) because this reading primarily functions as a constraint on federal power, preventing it from extracting regulatory authority from states. Suppression is also low (0.30) as it limits federal overreach, rather than actively coercing states. The accessibility collapse is high (0.70) for broad federal regulatory alternatives, as this interpretation actively closes off such avenues. Resistance is high (0.75) because this interpretation has been consistently challenged by those advocating for a more expansive federal role, particularly during periods of national crisis or social change. The theater ratio is low (0.10) as this is a foundational legal principle, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this constraint is a protective rope, preserving their sovereignty. From the perspective of federal agencies and national advocacy groups, it is a snare, preventing necessary national action and extracting the potential for uniform solutions. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are clear beneficiaries, as this reading protects their autonomy and shields them from federal regulation. Federal agencies and national advocacy groups are targets, as their ability to pursue national regulatory uniformity and civil rights enforcement is curtailed. The federal judiciary acts as the agenda-setter, interpreting and enforcing these limits.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the preservation of state autonomy as federal overreach. By defining 'commerce' narrowly, it ensures that federal power remains within its intended bounds, preventing the mandate of facilitating interstate trade from morphing into a general police power that extracts from state sovereignty. The 'live' status of the founding problem (balancing federal and state power) indicates that the constraint's original purpose remains relevant, even if its interpretation is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Is the original intent of ''commerce among the several states'' truly limited to ''trade crossing state lines,'' or did the framers intend a broader meaning that could encompass economic activities with interstate effects?',
    'Further historical and linguistic analysis of 18th-century legal and economic texts, as well as a re-evaluation of the framers'' understanding of a national economy.',
    'If a broader original intent is established, the justification for this narrow reading weakens, potentially shifting its classification towards a snare for federal power or a piton maintained by ideological inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the framers'' precise definition of ''commerce''.').

omega_variable(
    regulate_meaning_ambiguity,
    'Does ''regulate'' in the Commerce Clause mean only ''make regular'' (facilitate) or can it legitimately include ''prohibit'' or ''control'' (restrict)?',
    'Analysis of 18th-century legal dictionaries and usage in other constitutional clauses, as well as historical practice of early federal legislation.',
    'If ''regulate'' is found to include ''prohibit'' or ''control,'' the federal government''s power under this reading would expand, reducing the constraint''s suppressive effect on federal agencies and potentially altering its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulate_meaning_ambiguity, conceptual, 'Ambiguity regarding the meaning of ''regulate''.').

omega_variable(
    federalism_balance_preference,
    'Is the preference for state autonomy over national uniformity, as embodied in this reading, a constitutionally mandated structural principle or a policy preference of the interpreters?',
    'A deeper philosophical and historical inquiry into the nature of federalism and the role of judicial review in balancing competing governmental interests, acknowledging the normative choices inherent in constitutional interpretation.',
    'If it is primarily a policy preference, the constraint''s claim to being a ''rope'' (coordination) weakens, as it would be seen as imposing a specific political outcome rather than merely clarifying a structural limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_balance_preference, preference, 'Whether the federalism balance is a constitutional mandate or a policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__narrow_originalist, theater_ratio, 20, 0.09).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__narrow_originalist, theater_ratio, 40, 0.1).
narrative_ontology:measurement(comm_tr_t60, commerce_clause_scope__narrow_originalist, theater_ratio, 60, 0.09).
narrative_ontology:measurement(comm_tr_t80, commerce_clause_scope__narrow_originalist, theater_ratio, 80, 0.1).
narrative_ontology:measurement(comm_tr_t100, commerce_clause_scope__narrow_originalist, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__narrow_originalist, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__narrow_originalist, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(comm_be_t60, commerce_clause_scope__narrow_originalist, base_extractiveness, 60, 0.23).
narrative_ontology:measurement(comm_be_t80, commerce_clause_scope__narrow_originalist, base_extractiveness, 80, 0.24).
narrative_ontology:measurement(comm_be_t100, commerce_clause_scope__narrow_originalist, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__narrow_originalist, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__narrow_originalist, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(comm_su_t60, commerce_clause_scope__narrow_originalist, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(comm_su_t80, commerce_clause_scope__narrow_originalist, suppression_requirement, 80, 0.29).
narrative_ontology:measurement(comm_su_t100, commerce_clause_scope__narrow_originalist, suppression_requirement, 100, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'commerce_clause_scope' kernel. It represents the narrow originalist interpretation, which defines federal power over commerce restrictively. It forecloses the 'broad_effects_test' and 'intermediate_channels' readings due to fundamentally different definitions of 'commerce' and 'regulate'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
