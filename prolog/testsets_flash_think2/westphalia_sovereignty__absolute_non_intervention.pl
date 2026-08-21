% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Principle (Westphalian Sovereignty Reading)
 *   domain: international_relations/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'absolute non-intervention' reading of
 *   Westphalian sovereignty, asserting that external interference in a
 *   state's domestic affairs is illegitimate, regardless of internal conduct.
 *   It emerged from the Treaty of Westphalia (1648) to establish interstate
 *   peace by defining clear territorial jurisdiction. While it genuinely
 *   coordinates state relations by preventing external meddling, it
 *   simultaneously enables significant extraction from populations by state
 *   elites, particularly authoritarian regimes, who are shielded from
 *   accountability for domestic abuses. The 'regardless of internal conduct'
 *   clause is central to its extractive nature.
 *
 * KEY AGENTS:
 *   - State_elites: Primary beneficiary and agenda-setter (institutional/arbitrage) — benefits from unfettered authority.
 *   - Authoritarian_regimes: Beneficiary (institutional/constrained) — relies on the norm for legitimacy against external pressure.
 *   - Populations_under_authoritarian_rule: Primary target/payer (powerless/trapped) — bears the cost of state impunity.
 *   - Human_rights_advocates: Payer/excluded (organized/constrained) — constrained by the norm in their advocacy.
 *   - International_organizations: Agenda-setter/payer (institutional/constrained) — bound by the norm, often paralyzed by it.
 *   - Liberal_democracies: Observer/payer (powerful/constrained) — constrained from intervention despite humanitarian concerns.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.78).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.8).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.78).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Principle (Westphalian Sovereignty Reading)").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_relations/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '4b316b21-773a-4286-aed4-37191f2b480e').
narrative_ontology:cs_kernel_codification('4b316b21-773a-4286-aed4-37191f2b480e', formalized).
narrative_ontology:cs_authority_grounding('4b316b21-773a-4286-aed4-37191f2b480e', lineage).
narrative_ontology:cs_interpretation_layer_present('4b316b21-773a-4286-aed4-37191f2b480e').
narrative_ontology:cs_reading_relation('4b316b21-773a-4286-aed4-37191f2b480e', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('4b316b21-773a-4286-aed4-37191f2b480e', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('4b316b21-773a-4286-aed4-37191f2b480e', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4b316b21-773a-4286-aed4-37191f2b480e', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('4b316b21-773a-4286-aed4-37191f2b480e', foundational, non_interference_in_domestic_affairs).
narrative_ontology:cs_axiom_status(non_interference_in_domestic_affairs, holdable).
narrative_ontology:cs_axiom_grounding('4b316b21-773a-4286-aed4-37191f2b480e', non_interference_in_domestic_affairs, conventional).
narrative_ontology:cs_reference_frame('4b316b21-773a-4286-aed4-37191f2b480e', treaty_of_westphalia_1648).
narrative_ontology:cs_drift_state('4b316b21-773a-4286-aed4-37191f2b480e', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4b316b21-773a-4286-aed4-37191f2b480e', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, international_organizations).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, liberal_democracies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ruling class or political leadership of a state. They benefit directly from the principle of absolute non-intervention as it grants them unfettered authority over their domestic affairs, free from external scrutiny or interference, allowing them to consolidate power and resources.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites, agenda_setter,
    institutional, generational, arbitrage, global).

% States whose governance relies on suppressing dissent and maintaining strict internal control. This principle provides a crucial shield against external pressure or intervention, legitimizing their internal conduct regardless of human rights abuses.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, global).

% Citizens living under regimes that commit mass atrocities or systematically violate human rights. They bear the direct costs of the absolute non-intervention principle, as it denies them external protection or recourse when their own state is the perpetrator.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule, payer,
    powerless, biographical, trapped, national).

% Non-governmental organizations, activists, and legal experts who champion human rights globally. They are structurally constrained by the absolute non-intervention principle, as it limits their ability to effect change through international mechanisms, forcing them to work within the confines of state sovereignty.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, human_rights_advocates, payer,
    organized, generational, constrained, global).

% Bodies like the United Nations, which are founded on the principle of state sovereignty but also have mandates related to peace and human rights. They are bound by the non-intervention norm, which often paralyzes their ability to act in cases of domestic atrocities, effectively paying the cost of inaction.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_organizations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, international_organizations, payer).

% States that often advocate for human rights and democratic values internationally. While they may wish to intervene in cases of mass atrocities, they are constrained by the non-intervention norm, facing diplomatic and legal costs if they act unilaterally.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, liberal_democracies, observer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, liberal_democracies, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear jurisdictional boundaries between states, preventing interstate warfare and promoting stability by ensuring mutual recognition of territorial integrity and non-interference in internal affairs.
% TRANSFER_FUNCTION: Transfers the exclusive right to determine internal affairs and manage populations to the state, from any external actors or international bodies, effectively granting states a monopoly on legitimate force and governance within their borders.
% ABSENT_VOICES: Populations suffering mass atrocities or systematic human rights abuses, who would demand external protection and accountability from their own governments, but are silenced by the principle's protection of state authority.
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention principle vanished overnight, the international system would face immediate and widespread interventions, potentially leading to increased interstate conflict, a collapse of the current state-centric order, or the emergence of a new, more interventionist global governance structure.
% FOUNDING_PROBLEM: The chaos and endless religious and dynastic wars of post-Reformation Europe, where conflicts frequently spilled across borders, leading to widespread instability and suffering.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars widely corroborate the historical context of the Treaty of Westphalia and the subsequent development of the state system. However, human rights advocates and some international legal scholars contest whether the original problem (interstate war) remains the primary threat, arguing that intrastate atrocities now pose a greater challenge to global peace and security.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant power and resources state elites can consolidate and extract from their populations when shielded from external accountability. Suppression (0.8) is high because the principle legitimizes and protects a state's capacity to control and suppress internal dissent without external interference. Theater ratio (0.4) is moderate; while states genuinely invoke the principle, it is often used performatively to deflect criticism or avoid intervention when human rights are violated. Accessibility collapse (0.85) is high as it severely limits alternatives for both external actors (intervention) and internal populations (recourse against state abuses). Resistance (0.6) is substantial, primarily from human rights advocates and affected populations, who continuously challenge the principle's moral and practical implications.
 *
 * PERSPECTIVAL GAP:
 *   State elites and authoritarian regimes perceive this constraint as a legitimate 'rope' that ensures state stability and international order, protecting their sovereign rights. Conversely, populations under authoritarian rule and human rights advocates experience it as a 'snare' that enables impunity for atrocities and traps victims within oppressive systems. The engine's computation of 'tangled_rope' reflects this dual function: genuine coordination for states, but with severe asymmetric extraction from populations.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes are clear beneficiaries (low d) as the principle directly protects their power and allows them to extract from their populations. Populations under authoritarian rule and human rights advocates are targets (high d) as they bear the costs of state impunity and are denied external recourse. International organizations and liberal democracies are complex; while they administer the system, they also pay the cost of inaction or diplomatic friction when the principle prevents addressing atrocities, placing them closer to the symmetric or payer end depending on the specific context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_primacy,
    'Is the primary function of absolute non-intervention the coordination of interstate relations (preventing war) or the protection of state power (enabling extraction from populations)?',
    'Empirical analysis of historical outcomes: if the principle''s application consistently leads to greater interstate peace at the cost of increased intrastate violence, it suggests a shift in functional primacy. If interventions consistently lead to worse outcomes, it supports the coordination claim.',
    'If primarily coordination, the constraint leans more towards a Rope; if primarily protection of state power, it leans more towards a Snare. The current Tangled Rope classification reflects the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_primacy, conceptual, 'Ambiguity of the constraint''s core function.').

omega_variable(
    cost_of_non_intervention_for_populations,
    'What is the quantifiable human cost (lives lost, suffering, displacement) of adhering to absolute non-intervention in cases of mass atrocities, compared to the potential costs of intervention?',
    'Comparative case studies and counterfactual modeling of humanitarian crises, assessing outcomes under intervention vs. non-intervention scenarios. This is an empirical question with significant data challenges.',
    'Higher quantifiable costs for populations under non-intervention would strengthen the ''snare'' aspect of the constraint and increase its effective extractiveness, particularly for the ''populations_under_authoritarian_rule'' seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_of_non_intervention_for_populations, empirical, 'Empirical cost-benefit analysis of non-intervention for affected populations.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives for populations under authoritarian rule primarily structural (state apparatus, legal barriers) or internalized (fear, national identity narratives, lack of external support)?',
    'Post-regime-change analysis: if suppression persists (e.g., through self-censorship, continued fear) after the formal extractive mechanism is removed, it indicates a significant internalized component. If alternatives rapidly emerge, it suggests structural suppression was dominant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as populations carry the suppression with them even if the state''s external shield is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for affected populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(west_tr_t1815, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1815, 0.15).
narrative_ontology:measurement(west_tr_t1919, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.4).
narrative_ontology:measurement(west_be_t1815, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1815, 0.5).
narrative_ontology:measurement(west_be_t1919, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1919, 0.6).
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.3).
narrative_ontology:measurement(west_su_t1815, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1815, 0.4).
narrative_ontology:measurement(west_su_t1919, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1919, 0.5).
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, state_sovereignty__un_charter_article_2_7).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalia_sovereignty' kernel, focusing on the absolute non-intervention principle. It is structurally distinct from the 'conditional_responsibility' and 'graded_sovereignty' readings, which offer different interpretations of state authority and intervention legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
