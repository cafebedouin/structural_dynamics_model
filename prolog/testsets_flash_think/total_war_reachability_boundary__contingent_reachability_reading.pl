% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary: Contingent Reachability Reading
 *   domain: international_relations/strategic_studies/nuclear_deterrence_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contingent reachability' reading
 *   of the total war reachability boundary. It posits that the feasibility of
 *   total war is fundamentally dependent on the prevailing technological
 *   equilibrium. The current 'contraction' of strategic space, where total
 *   war is perceived as unwinnable, is seen as a temporary state (a
 *   piton-like atrophied capability) that could reverse with significant
 *   technological change. Therefore, the boundary itself is classified as a
 *   Scaffold: a temporary constraint whose justification is transitional,
 *   dependent on the current technological landscape, and subject to a
 *   potential 'sunset' if technology shifts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.65).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.9).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary: Contingent Reachability Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence_theory").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'bb6af9f4-367f-47eb-bf25-06c1ddbab7e6').
narrative_ontology:cs_kernel_codification('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', implicit).
narrative_ontology:cs_authority_grounding('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', self_enforcing).
narrative_ontology:cs_reading_relation('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', foundational, technological_determinism_of_reachability).
narrative_ontology:cs_axiom_status(technological_determinism_of_reachability, holdable).
narrative_ontology:cs_axiom_grounding('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', technological_determinism_of_reachability, empirically_contingent).
narrative_ontology:cs_reference_frame('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', post_cold_war_strategic_stability).
narrative_ontology:cs_drift_state('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', contemporary_technological_competition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb6af9f4-367f-47eb-bf25-06c1ddbab7e6', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the current technology-dependent reachability boundary by seeking to develop or acquire technologies that could shift the strategic equilibrium, potentially gaining a temporary advantage or undermining existing deterrence frameworks. Their investment in these technologies is a form of arbitrage against the current scaffold.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_destabilizing_technologies, beneficiary,
    powerful, generational, arbitrage, global).

% Global populations bear the ultimate, catastrophic risk should the reachability boundary shift in a way that makes total war feasible or more likely, leading to deterrence failure. They also bear the indirect costs of arms races and strategic instability.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_populations, payer,
    powerless, civilizational, trapped, universal).

% These states are the primary architects and maintainers of the nuclear deterrence framework. While they benefit from the prevention of total war, they are also the drivers of technological change that could destabilize the reachability boundary, creating a constant tension between maintaining stability and seeking strategic advantage.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% These states live under the strategic shadow of nuclear deterrence and the contingent reachability boundary. They bear the indirect costs of global instability and arms races, with limited agency to influence the technological or strategic shifts that define the boundary.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% These experts study the dynamics of nuclear deterrence, technological advancements, and their implications for strategic stability. They provide critical analysis of the reachability boundary's current state and potential future shifts, informing policy debates but not directly controlling the constraint.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent total war by establishing a strategic equilibrium where the costs of escalation outweigh any potential gains, contingent on current technological capabilities and their mutual understanding.
% TRANSFER_FUNCTION: Transfers the risk of direct military conflict between major powers into a perpetual state of strategic competition, technological arms races, and the existential threat of deterrence failure, borne by global populations.
% ABSENT_VOICES: Future generations, who would inherit a strategic landscape shaped by current technological choices and the stability (or instability) of the total war reachability boundary. Their interests are implicitly represented by non-proliferation advocates and long-term strategic planners.
% DISAPPEARANCE_RATIONALE: If the technological boundary on total war reachability vanished overnight (e.g., through a sudden, unmanageable technological breakthrough or collapse of deterrence), the global strategic landscape would fundamentally reorganize, likely leading to rapid escalation, widespread conflict, or a complete breakdown of international order.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons and other advanced military technologies, which made total war potentially self-annihilating, necessitating a mechanism to prevent its occurrence.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals, ongoing strategic dialogues, and the persistent investment in deterrence capabilities by major powers, alongside the warnings from international organizations and scientific bodies about the risks of escalation, all corroborate that the founding problem remains live.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Scaffold because its persistence and character are explicitly tied to a contingent technological equilibrium, implying a transitional nature rather than a fixed, permanent state. Extractiveness (0.65) is substantial due to the ongoing costs of maintaining deterrence (arms races, strategic competition) and the catastrophic risk to global populations. Suppression (0.9) is very high, reflecting the success of deterrence in preventing total war, but it requires active enforcement through constant vigilance and technological counter-development. Theater ratio is low (0.1) because the threat and the mechanisms of deterrence are very real, not performative. Accessibility collapse (0.9) is high because alternatives to the current deterrence framework are perceived as leading to catastrophic outcomes. Resistance (0.3) is moderate; while populations resist the idea of total war, direct resistance to the strategic boundary itself is limited, as most actors are invested in its maintenance, albeit with different interpretations of its stability.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers and strategic analysts might view the current boundary as a stable, albeit dynamic, equilibrium. However, states investing in destabilizing technologies see it as an opportunity for strategic gain, while global populations perceive it as a constant, existential threat. The 'piton' aspect of current contraction (atrophied capability) is a key point of divergence: some see it as a permanent shift, others as a temporary lull before a technological breakthrough.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in destabilizing technologies are beneficiaries, as they seek to exploit the contingent nature of the boundary for strategic advantage. Global populations are victims, bearing the ultimate risk and indirect costs. Nuclear powers act as agenda-setters, shaping the technological and strategic environment that defines the boundary. Non-nuclear states are payers, subject to the dynamics without significant agency. Strategic analysts are observers, providing critical analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to prevent total war. This reading acknowledges that the *current state* of 'contraction' (where total war is difficult to wage) might be a Piton – an atrophied capability. However, the *boundary itself* is a Scaffold because this atrophied state is temporary and contingent on technology. The constraint's justification is transitional, awaiting potential technological shifts that could reverse the contraction. The classification as Scaffold prevents mislabeling it as a permanent Mountain (which would ignore technological contingency) or a pure Snare (which would miss its coordination function in preventing war). The 'piton' aspect highlights the fragility of the current strategic stability, which is not self-sustaining but dependent on a transient technological advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_breakthrough_impact,
    'How would a significant technological breakthrough (e.g., perfect missile defense, undetectable offensive weapons, AI-driven decision-making) alter the total war reachability boundary?',
    'Future strategic analysis, wargaming, and empirical observation of military technological development and its integration into strategic doctrines.',
    'A breakthrough could fundamentally shift the claimed type from Scaffold to a more stable Rope (if it reinforces deterrence) or a more extractive Snare (if it creates a first-strike advantage), or even a Mountain (if it makes total war truly impossible or inevitable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_breakthrough_impact, empirical, 'The impact of future technology on strategic stability.').

omega_variable(
    scaffold_or_piton_ambiguity,
    'Is the current ''contraction'' of total war reachability truly a temporary Scaffold (transitional, technology-dependent) or a more permanent Piton (an atrophied capability that will not easily reverse)?',
    'Long-term historical analysis of technological cycles and strategic shifts, combined with expert consensus on the irreversibility of certain military-technological advancements.',
    'If it''s a Piton, the constraint is more stable but potentially less responsive to change; if a Scaffold, it implies a greater need for active management of technological transitions and a higher risk of sudden shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_or_piton_ambiguity, conceptual, 'Ambiguity between temporary technological contingency and permanent atrophy of total war capability.').

omega_variable(
    framing_under_determination_total_war_reachability,
    'Does the ''implicit'' kernel codification and ''self_enforcing'' authority grounding represent the only defensible framing, or would an alternative framing (e.g., a ''formalized'' kernel based on international law or treaties) produce a different cs_pattern classification?',
    'Comparative analysis of strategic stability frameworks, examining how different legal or normative framings of total war influence perceived reachability and deterrence effectiveness. This would involve assessing whether a ''formalized'' kernel could genuinely capture the emergent properties of strategic capabilities.',
    'If an alternative framing were adopted, the cs_pattern classification could shift, potentially highlighting different points of leverage for intervention or different sources of instability. For example, a formalized kernel might emphasize legal obligations over technological realities, leading to a different assessment of drift and authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_total_war_reachability, conceptual, 'Framing under-determination regarding the kernel codification and authority grounding of total war reachability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(tota_tr_t1998, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(tota_tr_t2012, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(tota_tr_t2018, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(tota_be_t1998, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(tota_be_t2012, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2012, 0.62).
narrative_ontology:measurement(tota_be_t2018, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.85).
narrative_ontology:measurement(tota_su_t1998, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1998, 0.86).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2005, 0.87).
narrative_ontology:measurement(tota_su_t2012, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(tota_su_t2018, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2018, 0.89).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, nuclear_proliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, arms_control_treaties).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, strategic_stability_doctrines).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
