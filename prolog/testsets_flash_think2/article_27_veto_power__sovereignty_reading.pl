% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty Principle
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint is the 'sovereignty_reading' of the
 *   'article_27_veto_power' kernel. It frames the P5 veto as a structural
 *   inevitability reflecting the Westphalian principle of state sovereignty,
 *   particularly for great powers with global-reach enforcement capacity.
 *   From this perspective, the veto is not a policy choice or an extractive
 *   mechanism, but a formal recognition of an underlying power reality.
 *   Sibling readings include the 'coordination_reading' (veto as conflict
 *   prevention) and the 'oligopoly_reading' (veto as power entrenchment).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.1).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty Principle").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'ccfbd0b3-16a3-4086-b2cf-b9e69d422910').
narrative_ontology:cs_kernel_codification('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', formalized).
narrative_ontology:cs_authority_grounding('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', practice).
narrative_ontology:cs_reading_relation('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', foundational, great_power_compulsion_is_impossible).
narrative_ontology:cs_axiom_status(great_power_compulsion_is_impossible, holdable).
narrative_ontology:cs_axiom_grounding('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', great_power_compulsion_is_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', contemporary_multipolar_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ccfbd0b3-16a3-4086-b2cf-b9e69d422910', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_sovereignty_principle).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, great_power_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As states possessing global-reach enforcement capacity and nuclear arsenals, their inherent power makes them immune to external compulsion, embodying the Westphalian principle that no state can be bound without its consent. The veto is a formal recognition of this underlying reality, which they assert and maintain.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, great_powers, agenda_setter,
    institutional, civilizational, analytical, global).

% While subject to the global order shaped by this principle, they are structurally excluded from altering it. Their lack of global-reach enforcement capacity means they cannot unilaterally assert the same level of sovereignty in practice, and they must navigate a system where great power consent is paramount.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, smaller_states, excluded,
    powerless, biographical, constrained, global).

% Analyze the P5 veto as a reflection of fundamental power realities in international relations, arguing that it is a structural feature rather than a policy choice or a mechanism for extraction. They document its implications for the international legal order.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizes the principle that great powers cannot be compelled by international law without their consent, preventing attempts to impose obligations that would be unenforceable or lead to conflict.
% TRANSFER_FUNCTION: No direct transfer of resources; rather, it transfers the locus of ultimate authority on matters of international security from a collective body to individual great powers, reflecting their inherent sovereignty.
% ABSENT_VOICES: Advocates for a more egalitarian international legal order, who would argue that the veto undermines collective security and the rule of law, are structurally excluded from altering this fundamental principle.
% DISAPPEARANCE_RATIONALE: If the principle that great powers cannot be bound without consent vanished, the entire international legal and security architecture would collapse, as attempts to compel nuclear states would lead to direct confrontation or the irrelevance of international institutions. The world would have to fundamentally rearrange its power structures or face constant conflict.
% FOUNDING_PROBLEM: The historical reality of sovereign states, particularly those with significant military capabilities, refusing to be bound by external authority, leading to the failure of collective security mechanisms that did not acknowledge this power dynamic.
% FOUNDING_PROBLEM_CORROBORATION: International relations realists and historians attest to the enduring nature of state sovereignty and great power politics, citing historical precedents and the current geopolitical landscape. This view is corroborated by the consistent behavior of great powers in international forums.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it is interpreted as a direct consequence of the physical and military realities of great power. Its extractiveness is near-zero because it is not seen as extracting rents, but rather as reflecting an irreducible limit on international governance. Suppression is low because the 'constraint' is not actively enforced against unwilling parties, but rather describes a fundamental condition of their existence. Theater ratio is low as its function is seen as genuinely reflecting power dynamics, not performative maintenance. Accessibility collapse is high because alternatives (binding great powers without their consent) are considered structurally impossible without leading to global conflict.
 *
 * PERSPECTIVAL GAP:
 *   This reading fundamentally differs from the 'oligopoly_reading' by denying that the veto is a mechanism for extraction or power entrenchment; instead, it views it as a necessary reflection of sovereign power. It also differs from the 'coordination_reading' by emphasizing the underlying structural reality over the functional coordination aspect. The engine's classification will highlight how these different framings lead to divergent classifications for the same formal rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers are identified as 'agenda_setters' because they embody and assert this principle through their actions and capabilities, rather than being 'beneficiaries' in an extractive sense. Smaller states are 'excluded' because, while affected by this structural reality, they lack the agency to alter it. International law scholars are 'observers' who analyze this structural feature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_inevitability_vs_political_choice,
    'Is the P5 veto truly a structural inevitability given great power capabilities, or is it a constructed political choice that could be altered?',
    'Counterfactual analysis of historical attempts to reform the Security Council, or empirical observation of how great powers respond to attempts to bind them without consent.',
    'If it is a constructed choice, the constraint would reclassify from Mountain to a more extractive type (e.g., Snare or Tangled Rope), as its persistence would depend on active enforcement and suppression of alternatives, rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_inevitability_vs_political_choice, conceptual, 'Ambiguity between the veto as a natural law of power and a human-made institutional design.').

omega_variable(
    consent_as_coercion,
    'Is the ''consent'' of great powers, as embodied by the veto, truly voluntary, or is it a form of coerced consent driven by the threat of non-compliance and conflict?',
    'Analysis of the historical context of veto use and the geopolitical consequences of resolutions passed without great power consent.',
    'If consent is found to be coerced, the underlying principle of sovereignty in this context would be re-evaluated, potentially shifting the constraint''s extractiveness upward as it would imply a hidden cost borne by the international system to maintain the illusion of voluntary participation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_as_coercion, empirical, 'The nature of great power consent in international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__sovereignty_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__sovereignty_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__sovereignty_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__sovereignty_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__sovereignty_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__sovereignty_reading, base_extractiveness, 80, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__sovereignty_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__sovereignty_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__sovereignty_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__sovereignty_reading, suppression_requirement, 80, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
