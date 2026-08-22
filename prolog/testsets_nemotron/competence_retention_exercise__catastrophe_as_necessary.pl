% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe as Necessary Teacher
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint instantiates the 'catastrophe as necessary' reading of
 *   the competence_retention_exercise kernel. The claim is that organizations
 *   only maintain genuine high-reliability competence through the visceral
 *   stakes and organizational learning produced by actual catastrophic
 *   events; simulation and near-miss analysis are rehearsal but not the real
 *   thing. The constraint extracts safety investment from frontline operators
 *   and the affected public while insulating executive leadership and
 *   captured regulators who benefit from the narrative that catastrophes are
 *   inevitable system resets rather than preventable failures. The
 *   measurement series shows extraction and suppression rising over five
 *   decades as simulation fidelity improved but was systematically dismissed,
 *   near-miss reporting systems were undermined, and safety budgets were
 *   redirected toward post-catastrophe response rather than prevention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.78).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.85).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.78).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, snare).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe as Necessary Teacher").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d').
narrative_ontology:cs_kernel_codification('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', distributed).
narrative_ontology:cs_authority_grounding('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', extraction).
narrative_ontology:cs_interpretation_layer_present('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d').
narrative_ontology:cs_reading_relation('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', foundational, visceral_stakes_necessity).
narrative_ontology:cs_axiom_status(visceral_stakes_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', visceral_stakes_necessity, empirically_contingent).
narrative_ontology:cs_axiom('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', foundational, simulation_inadequacy_doctrine).
narrative_ontology:cs_axiom_status(simulation_inadequacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', simulation_inadequacy_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', early_hro_theory_catastrophe_learning).
narrative_ontology:cs_drift_state('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', contemporary_resilience_engineering_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a47a3f3-c3f0-4522-ac1b-8a6b2b86945d', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, safety_regulator_capture).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, executive_leadership_insulation).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, affected_public).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, near_miss_reporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regulatory bodies that gain expanded authority, budget, and political capital after each catastrophe. They control the investigation process, define the lessons learned, and certify the 'system resets' that follow. Their capture is maintained by the catastrophe narrative: without catastrophes, their expanded mandate contracts. They move between industry and regulation, arbitraging the revolving door.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_regulator_capture, beneficiary,
    institutional, generational, arbitrage, national).

% Senior executives who set safety budgets, approve simulation fidelity, and design organizational incentives. They insulate themselves from accountability by adopting the catastrophe-as-necessary narrative: when disasters occur, they commission inquiries that validate the narrative and protect their positions. They arbitrage across organizations — a CEO who 'managed through' a catastrophe is often hired elsewhere as a crisis leader.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, executive_leadership_insulation, agenda_setter,
    institutional, biographical, arbitrage, global).

% Operators, controllers, maintainers, and first responders who bear the physical and psychological costs of catastrophes. Their professional identity is fused with the high-hazard mission — leaving means abandoning their community and self-concept. They are blamed for 'human error' when systems fail, and their near-miss reports are used against them. Simulation training is their only safe practice, but it is dismissed as insufficient, denying them the repetition needed for genuine competence.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Communities, passengers, patients, and downstream populations who suffer the direct consequences of catastrophes. They have no voice in the safety system, no exit from the hazard (they live near the plant, fly on the aircraft, depend on the infrastructure), and no organized representation in the post-catastrophe inquiries that shape the 'lessons learned.'
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, affected_public, payer,
    powerless, immediate, trapped, local).

% Operators and analysts who report near-misses and minor failures — the data that could prevent catastrophes without the catastrophe. They are structurally excluded because their success undermines the 'catastrophe is necessary' narrative: if near-miss learning works, catastrophes are not necessary teachers. They face retaliation, their reporting systems are defunded, and their data is dismissed as 'not visceral enough.'
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, near_miss_reporters, excluded,
    moderate, biographical, constrained, local).

% Researchers who study how organizations actually maintain safety. They document that high-fidelity simulation, near-miss learning, and resilience practices reduce catastrophe rates — but their findings are marginalized by the dominant catastrophe narrative. They see the full structure: the extraction, the suppression, and the alternatives that work but are not adopted.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, resilience_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains organizational competence in high-hazard industries by ensuring that safety systems are tested under real stakes. The claim is that only actual catastrophes generate the visceral learning that prevents competence decay during long incident-free periods.
% TRANSFER_FUNCTION: Moves safety investment from prevention (simulation fidelity, near-miss systems, frontline training, resilience engineering) to post-catastrophe response (inquiries, litigation, regulatory expansion, public relations). Moves the costs of catastrophes (lives, health, trust, economic damage) from leadership and regulators to frontline operators and the public. Moves organizational legitimacy from 'we prevent harm' to 'we learn from harm.'
% ABSENT_VOICES: Frontline operators who have left the industry due to identity-locked exit barriers; affected public communities who are never consulted on risk acceptance; simulation and resilience engineering experts whose methods are dismissed; near-miss reporters who were silenced. They are absent because the constraint's suppression mechanism (retaliation, defunding, narrative dominance) keeps them out of the rooms where safety policy is made.
% DISAPPEARANCE_RATIONALE: If the catastrophe-as-necessary constraint vanished overnight, organizations would shift investment to high-fidelity simulation and near-miss learning systems (which already exist and work), catastrophe rates would fall, frontline operators would get genuine practice without dying, and the public would be safer. The safety regulator and executive leadership would lose their catastrophe-driven authority and insulation — the world would rearrange around prevention rather than response.
% FOUNDING_PROBLEM: In the 1970s-1980s, high-hazard industries (nuclear, aviation, chemical) discovered that organizations become complacent during long incident-free periods. Simulation technology was primitive, near-miss reporting was informal, and several major accidents (Three Mile Island, Bhopal, Challenger) revealed that procedural compliance without visceral stakes produced brittle competence. The founding problem was genuine: how to maintain readiness when nothing bad happens for decades.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by resilience engineering researchers (Hollnagel, Woods, Dekker), NASA's Aviation Safety Reporting System data, nuclear industry INPO/ERNEA peer review data, and the commercial aviation safety record (2010-2020 zero-fatality years in developed nations) — all from outside the benefiting parties (regulators and executives who gain from the catastrophe narrative). The benefiting parties attest the problem is still live, citing 'new failure modes' and 'complexity growth,' but offer no independent corroboration.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because the constraint diverts resources from prevention (simulation, near-miss systems, frontline training) toward post-catastrophe response and inquiry, while the human and economic costs of the 'necessary' catastrophes are borne by operators and the public. Suppression is very high (0.85) because alternatives are actively suppressed: near-miss reporters face retaliation, simulation investment is defunded as 'not real training,' and regulatory capture ensures the catastrophe narrative dominates policy. Theater ratio (0.42) reflects genuine safety rituals (drills, audits) that persist but increasingly serve as performance rather than functional competence maintenance. Accessibility collapse (0.68) is moderate-high because the 'catastrophe is necessary' framing makes alternatives cognitively inaccessible — organizations that haven't had a catastrophe believe they're safe; those that have believe they've learned. Resistance (0.55) is moderate: frontline operators and near-miss reporters resist but lack structural power; the public reacts after catastrophes but not systematically.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/executive seat, the constraint appears as hard-won wisdom: 'we only learn from blood.' From the operator/public seat, it appears as a protection racket: 'we sacrifice you to maintain our narrative.' The engine computes this divergence from the structural data — the claimed type (snare) reflects the authoring seat's assessment that the extraction is structural and the coordination story is cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators and executive leadership are structural beneficiaries: regulators gain expanded authority and budget after each catastrophe; leadership insulates itself from accountability by framing catastrophes as inevitable system resets rather than management failures (d near beneficiary end). Frontline operators are primary targets: they bear the physical and psychological costs of catastrophes, face retaliation for near-miss reporting, and are blamed for 'human error' when the system fails (d near target end). The affected public pays in lives, health, and trust but has no organized voice in the safety system. Near-miss reporters are doubly victimized: they provide the data that could prevent catastrophes but are suppressed because their success undermines the 'catastrophe is necessary' narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence in high-hazard industries) is real, but this reading has outlived its function. Early high-reliability theory (1970s-1980s) correctly identified simulation limits, but the field has since developed high-fidelity simulation, near-miss learning systems, and resilience engineering that this reading actively suppresses. The mandate has atrophied into a justification for tolerating preventable harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine discovery about organizational cognition, or a constructed justification for tolerating preventable harm?',
    'Track whether organizations that embrace this reading show higher actual catastrophe rates than those investing in simulation fidelity and near-miss learning systems, controlling for industry risk profile.',
    'If constructed, the constraint is a snare extracting safety budget from frontline operators while insulating leadership; if genuine, it identifies a hard boundary of organizational learning that demands different governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the catastrophe-as-necessary claim describes a structural limit or a self-serving narrative').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives (near-miss learning, simulation investment) structural via regulatory capture and budget allocation, or internalized via professional identity that equates ''real safety'' with ''having survived catastrophe''?',
    'Post-exit suppression trajectory: if safety professionals who leave high-reliability organizations continue to dismiss simulation and near-miss data in new roles, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making industry-wide reform harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in high-reliability safety culture').

omega_variable(
    competence_decay_observability,
    'Can competence decay during incident-free periods be measured independently of the catastrophes it supposedly predicts?',
    'Develop leading indicators of competence decay (drill performance variance, procedure deviation rates, expertise retention metrics) and test their predictive validity against actual catastrophe occurrence.',
    'If measurable, the constraint''s claim that decay is ''invisible'' becomes falsifiable; if not measurable, the claim functions as an unfalsifiable justification for inaction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_observability, empirical, 'Whether the claimed invisible competence decay is empirically tractable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 10, 0.31).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 20, 0.36).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 30, 0.4).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.41).
narrative_ontology:measurement(comp_tr_t50, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(comp_be_t50, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(comp_su_t50, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the competence_retention_exercise kernel into three readings with distinct ε values and victim/beneficiary structures. The catastrophe_as_necessary reading has the highest extraction (0.78) and suppression (0.85); the near_miss_as_bridge reading has moderate extraction (~0.45) as a tangled rope; the simulation_as_sufficient reading has low extraction (~0.25) as a rope. The readings are linked by the kernel but are structurally distinct constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, institutional, 0.15).
constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
