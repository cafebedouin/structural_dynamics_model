% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UNSC Article 27 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story analyzes the UN Security Council's Article 27 veto
 *   power from an 'oligopoly reading,' focusing on its role in entrenching
 *   the geopolitical dominance of the P5 permanent members. It argues that
 *   the veto, while initially framed as a coordination mechanism to prevent
 *   great power conflict, has evolved into a structural tool for extracting
 *   authority rents and blocking institutional reforms that would
 *   redistribute power to the broader UN membership. This reading classifies
 *   the veto as a Snare, emphasizing its extractive and suppressive
 *   functions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.85).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.92).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UNSC Article 27 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '4ecdf474-5ba4-4178-88f9-c4bb70684779').
narrative_ontology:cs_kernel_codification('4ecdf474-5ba4-4178-88f9-c4bb70684779', fixed_text).
narrative_ontology:cs_authority_grounding('4ecdf474-5ba4-4178-88f9-c4bb70684779', extraction).
narrative_ontology:cs_interpretation_layer_present('4ecdf474-5ba4-4178-88f9-c4bb70684779').
narrative_ontology:cs_reading_relation('4ecdf474-5ba4-4178-88f9-c4bb70684779', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ecdf474-5ba4-4178-88f9-c4bb70684779', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('4ecdf474-5ba4-4178-88f9-c4bb70684779', foundational, geopolitical_oligopoly_entrenchment).
narrative_ontology:cs_axiom_status(geopolitical_oligopoly_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('4ecdf474-5ba4-4178-88f9-c4bb70684779', geopolitical_oligopoly_entrenchment, empirically_contingent).
narrative_ontology:cs_axiom('4ecdf474-5ba4-4178-88f9-c4bb70684779', secondary, charter_immutability_as_rent_extraction).
narrative_ontology:cs_axiom_status(charter_immutability_as_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('4ecdf474-5ba4-4178-88f9-c4bb70684779', charter_immutability_as_rent_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('4ecdf474-5ba4-4178-88f9-c4bb70684779', post_wwii_great_power_consensus).
narrative_ontology:cs_drift_state('4ecdf474-5ba4-4178-88f9-c4bb70684779', contemporary_multipolar_world, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('4ecdf474-5ba4-4178-88f9-c4bb70684779', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_governance_reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members of the UN Security Council (China, France, Russia, United Kingdom, United States) hold the veto power, allowing them to unilaterally block any substantive resolution. This entrenches their authority, prevents reforms that would dilute their power, and allows them to act with relative impunity in their spheres of influence. They benefit from the status quo and actively resist any attempts to reform the veto.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% The vast majority of UN member states, who are subject to Security Council resolutions but have no veto power. They bear the costs of an unresponsive international system, are often victims of geopolitical stalemates, and have no effective mechanism to reform the veto, despite representing a global majority. Their 'exit' is to withdraw from the UN, which is politically and practically prohibitive.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_member_states, payer,
    organized, generational, constrained, global).

% Academics, NGOs, and smaller states who actively campaign for UN reform, including abolition or modification of the veto. They are 'identity_locked' by their commitment to multilateralism and international law, making exit from the system unthinkable, despite the frustration of being perpetually blocked by the P5.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_governance_reform_advocates, payer,
    moderate, generational, identity_locked, global).

% The administrative body of the UN, tasked with implementing Security Council mandates. They observe the paralysis caused by vetoes and the resulting erosion of the Council's effectiveness, but have no power to alter the veto mechanism itself.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto was originally intended to ensure great power consensus on critical security issues, preventing the UN from taking action that could lead to direct conflict between nuclear-armed states.
% TRANSFER_FUNCTION: Transfers effective control over international security policy and institutional reform from the collective will of UN member states to the five permanent members, allowing them to extract ongoing authority rents and maintain their geopolitical dominance.
% ABSENT_VOICES: The populations of states whose humanitarian crises are ignored or prolonged due to vetoes, and future generations who inherit an international system unable to address global challenges effectively due to entrenched power structures. They are excluded by the very design of the Security Council's power distribution.
% DISAPPEARANCE_RATIONALE: If the veto power vanished overnight, the UN Security Council would immediately become more active and representative. Resolutions on long-standing conflicts and humanitarian crises would pass, and the balance of power in international relations would shift dramatically, forcing the P5 to negotiate rather than dictate. This would lead to a significant rearrangement of global governance structures.
% FOUNDING_PROBLEM: The problem of preventing a third world war and ensuring the cooperation of major powers in maintaining international peace and security, particularly after the failure of the League of Nations.
% FOUNDING_PROBLEM_CORROBORATION: The P5 members argue the problem is still live, citing ongoing great power rivalries. Non-P5 states and reform advocates argue the original problem has evolved, and the veto now exacerbates rather than solves new global challenges, with corroboration from numerous academic studies on UN effectiveness and statements from regional blocs.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the veto allows the P5 to unilaterally block actions that might challenge their interests or spheres of influence, effectively extracting a 'toll' on global governance. Suppression (0.92) is also very high, as the veto mechanism fundamentally suppresses the agency of the non-P5 majority to shape international security policy or reform the UN Charter. The theater ratio is low (0.1) because the veto is a direct, functional exercise of power, not a performative one. Accessibility collapse is high (0.8) because there are virtually no viable alternatives for non-P5 states to achieve collective security or institutional reform outside the UN framework. Resistance (0.7) is substantial, evidenced by decades of calls for reform from the General Assembly and various state blocs, but this resistance is largely ineffective due to the veto itself.
 *
 * PERSPECTIVAL GAP:
 *   The P5 members experience the veto as a necessary safeguard for international stability (the 'coordination reading'), ensuring their vital interests are protected. In contrast, the non-P5 states and reform advocates experience it as an arbitrary power that entrenches an outdated oligarchy, blocking legitimate collective action and institutional evolution. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 are clear beneficiaries (d near 0.0) as the veto directly subsidizes their geopolitical influence and protects them from accountability. Non-P5 member states and reform advocates are targets (d near 1.0) as they bear the costs of an unresponsive system and have their agency suppressed. The UN Secretariat is an observer, constrained by the veto's existence but not directly benefiting or paying in the same way.
 *
 * MANDATROPHY ANALYSIS:
 *   This oligopoly reading suggests that the veto's original mandate (preventing great power war) has either been superseded or is now used as a cover for rent extraction. The classification as a Snare, rather than a Rope or Tangled Rope, highlights that the coordination function has atrophied or become secondary to the extractive function, preventing mislabeling it as a benign or even necessary coordination mechanism. The high extractiveness and suppression, coupled with the contested status of the founding problem, point to a significant mandatrophy where the constraint persists primarily due to the beneficiaries' power, not its original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_necessity_for_great_power_peace,
    'Is the P5 veto still a necessary mechanism to prevent great power conflict and ensure their participation in the UN, or would its abolition lead to a more stable and equitable international system?',
    'Counterfactual historical analysis, game-theoretic modeling of alternative UN structures, and observation of great power behavior in non-veto-bound international forums.',
    'If proven unnecessary, the veto''s coordination justification collapses, strengthening its classification as a pure Snare. If proven necessary, it would suggest a stronger ''tangled_rope'' element, acknowledging a genuine (though asymmetric) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_necessity_for_great_power_peace, conceptual, 'Whether the veto''s original coordination function remains essential for great power peace.').

omega_variable(
    charter_immutability_vs_evolution,
    'Is the UN Charter''s immutability (particularly regarding the veto) a fundamental constitutional principle, or an obstacle to necessary institutional evolution in response to changing global power dynamics?',
    'Legal and political analysis of constitutional amendment processes in other international bodies, and the historical evolution of international law and norms.',
    'If immutability is seen as a fundamental principle, the constraint''s ''mountain-like'' persistence is reinforced, even if extractive. If seen as an obstacle, it highlights the constructed nature of the constraint and the P5''s active role in maintaining it for their benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_immutability_vs_evolution, conceptual, 'The tension between Charter immutability and institutional adaptability.').

omega_variable(
    oligopoly_vs_hegemony,
    'Is the P5 veto best understood as an oligopolistic arrangement among five powers, or as a mechanism for a single hegemonic power (or a subset of the P5) to exert its will?',
    'Detailed analysis of voting patterns, veto usage, and geopolitical alignments over time, identifying consistent blocs or dominant actors.',
    'If a single hegemon consistently dictates outcomes, the extraction is more concentrated. If it''s a true oligopoly, the extraction is shared, but the collective suppression of non-P5 states remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oligopoly_vs_hegemony, empirical, 'The precise distribution of power and extraction within the P5.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__oligopoly_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__oligopoly_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(arti_be_t2024, article_27_veto_power__oligopoly_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__oligopoly_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__oligopoly_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(arti_su_t2024, article_27_veto_power__oligopoly_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_security_council_resolution_process).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_peacekeeping_mandates).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Article 27 veto power kernel: 'coordination_reading', 'oligopoly_reading', and 'sovereignty_reading'. Each represents a distinct structural claim about the veto's function and beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
