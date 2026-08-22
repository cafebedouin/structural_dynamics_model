% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational-Choice Dropout: Great-Power War Remains Reachable But Cost-Dominated
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the 'rational dropout' reading of the nuclear
 *   impossibility kernel: nuclear weapons did not make great-power war
 *   physically impossible (the structural_contraction_reading's claim) nor
 *   produce a self-undermining incredibility of the deterrent threat (the
 *   credibility_paradox_reading's claim). Instead, on this reading, war
 *   remains a structurally reachable state — command chains, delivery
 *   systems, and escalation pathways to full nuclear exchange all continue to
 *   exist and are periodically exercised in planning — but rational
 *   cost-benefit calculation by state leaderships has removed it from the set
 *   of choices actively considered, because no conceivable political or
 *   territorial gain survives the expected-cost calculation once nuclear
 *   retaliation is priced in. The constraint is the *maintained belief* that
 *   this calculation holds, not a change in what is physically or logically
 *   possible. Because the reading treats war as reachable-but-deprioritized
 *   rather than foreclosed, it registers as a Tangled Rope: there is a
 *   genuine coordination function (mutually legible crisis stability) riding
 *   alongside asymmetric extraction (arsenal costs on domestic populations,
 *   residual coercion transferred onto non-nuclear states, accident risk
 *   transferred onto the future) sustained by active doctrinal, industrial,
 *   and command enforcement.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda-setting beneficiaries who administer and periodically re-certify the cost-dominance calculation
 *   - extended_deterrence_allies: derivative beneficiaries dependent on a ledger they do not control
 *   - defense_industrial_base: organized beneficiary whose interest is in the constraint's continued maintenance regardless of whether the underlying calculation is re-tested
 *   - non_nuclear_states_under_coercion: payers who absorb the coercive leverage the constraint does not eliminate
 *   - domestic_populations_bearing_arsenal_costs and future_generations_under_accident_risk: diffuse, powerless payers across biographical and civilizational horizons
 *   - strategic_studies_analysts: analytical observers who both model and contest the constraint's self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.42).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.58).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational-Choice Dropout: Great-Power War Remains Reachable But Cost-Dominated").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '7de2ec73-e87d-43a1-862c-4a29b20289a8').
narrative_ontology:cs_kernel_codification('7de2ec73-e87d-43a1-862c-4a29b20289a8', distributed).
narrative_ontology:cs_authority_grounding('7de2ec73-e87d-43a1-862c-4a29b20289a8', distributed).
narrative_ontology:cs_reading_relation('7de2ec73-e87d-43a1-862c-4a29b20289a8', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7de2ec73-e87d-43a1-862c-4a29b20289a8', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('7de2ec73-e87d-43a1-862c-4a29b20289a8', foundational, war_remains_reachable_but_cost_dominated).
narrative_ontology:cs_axiom_status(war_remains_reachable_but_cost_dominated, holdable).
narrative_ontology:cs_axiom_grounding('7de2ec73-e87d-43a1-862c-4a29b20289a8', war_remains_reachable_but_cost_dominated, empirically_contingent).
narrative_ontology:cs_axiom('7de2ec73-e87d-43a1-862c-4a29b20289a8', secondary, rational_leadership_continuously_reassesses_calculation).
narrative_ontology:cs_axiom_status(rational_leadership_continuously_reassesses_calculation, holdable).
narrative_ontology:cs_axiom_grounding('7de2ec73-e87d-43a1-862c-4a29b20289a8', rational_leadership_continuously_reassesses_calculation, instrumental).
narrative_ontology:cs_reference_frame('7de2ec73-e87d-43a1-862c-4a29b20289a8', cold_war_rational_deterrence_consensus).
narrative_ontology:cs_drift_state('7de2ec73-e87d-43a1-862c-4a29b20289a8', post_missile_defense_and_multipolar_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7de2ec73-e87d-43a1-862c-4a29b20289a8', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_base).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states_under_coercion).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, domestic_populations_bearing_arsenal_costs).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, future_generations_under_accident_risk).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, expected_utility_war_termination_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_deterrence_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain arsenals sized and postured on the explicit calculation that any first-use scenario's expected costs (retaliation, escalation, economic collapse, radiological harm) dominate any conceivable territorial or political gain. They administer declaratory doctrine, targeting plans, and command-and-control that keep the option technically live while treating its exercise as irrational. They set the terms of the cost-benefit ledger that other states must accept or contest.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states, beneficiary).

% Rely on a nuclear patron's cost-dominance calculation to substitute for their own weapons programs. They benefit from the umbrella but have no independent control over the ledger; if the patron's calculus shifts, their security depends on a decision made elsewhere. Exiting the arrangement means either acquiring independent capability or accepting exposure.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).

% Manufactures, modernizes, and services the arsenals whose existence is justified by the cost-dominance argument. Revenue and institutional survival depend on the constraint being maintained and periodically re-affirmed through modernization programs, independent of whether the underlying rational-choice claim is ever tested.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% Face the cost-dominance logic asymmetrically: a nuclear state's leadership may judge that coercive signaling short of full use still yields net-positive expected value against a non-nuclear target, even though full-scale nuclear exchange would not. These states absorb the residual coercive leverage the rational-choice constraint does not eliminate, with no comparable capability of their own to shift the ledger.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_states_under_coercion, payer,
    powerless, immediate, trapped, regional).

% Fund arsenal maintenance and modernization through taxation and foregone public investment, and bear the latent risk of accident, miscalculation, or command failure, without a vote on whether the cost-dominance calculation is correctly specified or how large a margin of safety it requires.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, domestic_populations_bearing_arsenal_costs, payer,
    powerless, generational, trapped, national).

% Inherit whatever residual probability of inadvertent or unauthorized use the current posture carries, compounded over decades. The rational-choice framing assumes rational execution at every node in the command chain across all future crises; they cannot object to or renegotiate that assumption.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, future_generations_under_accident_risk, payer,
    powerless, civilizational, trapped, global).

% Model the cost-benefit ledger, debate whether 'rational dropout' correctly describes state behavior or merely rationalizes continued arsenal maintenance, and produce the doctrine literature (deterrence stability theory, escalation ladders) that either corroborates or challenges the constraint's own self-description.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, mutually legible expectation among nuclear-armed states that full-scale war has become cost-dominated rather than merely difficult, allowing crisis behavior, arms-control negotiation, and alliance planning to proceed on a stable, common assumption about what outcomes are worth pursuing.
% TRANSFER_FUNCTION: Moves resources from domestic taxpayers and foregone public goods toward arsenal maintenance and modernization, and moves coercive leverage from non-nuclear states toward nuclear-armed ones in disputes that fall below the full-war threshold the constraint declares irrational.
% ABSENT_VOICES: Non-nuclear states subject to sub-nuclear coercive signaling, and future populations exposed to accident or miscalculation risk, are not parties to the doctrine debates that set and re-certify the cost-dominance calculation; they experience its downstream effects without a seat in the ledger's specification.
% DISAPPEARANCE_RATIONALE: If the rational-choice constraint dissolved overnight — if leaderships genuinely believed victory's costs no longer exceeded its benefits — crisis stability collapses: arms racing resumes without the shared cost-dominance assumption, extended deterrence commitments become unreliable, and states currently relying on the umbrella must reconsider independent arsenals or accommodation.
% FOUNDING_PROBLEM: Following Hiroshima and Nagasaki and the Soviet bomb, strategists needed to explain why great-power war, which had recurred roughly every generation for centuries, might not recur in a matter that avoided civilizational destruction — the problem was constructing a stable expectation that made restraint the rational choice for state leaderships.
% FOUNDING_PROBLEM_CORROBORATION: Cold War-era declassified war-gaming and command studies (e.g., RAND analyses, the Cuban Missile Crisis post-mortems) attest that leaderships genuinely reasoned in cost-dominance terms during real crises, corroborating the problem's historical liveness from outside current arsenal-holders. Contemporary arms-control researchers and non-proliferation scholars — largely outside the nuclear weapon states' own institutions — argue the problem has narrowed to crisis management among a shrinking set of dyads while arsenal modernization programs are justified by inertia and industrial interest rather than a freshly re-derived cost calculation.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).
:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 by 2025) because the coordination function is genuine — a shared cost-dominance expectation really does stabilize crisis behavior — but it is not zero, since arsenal costs and residual coercive leverage are real transfers riding on that coordination. Suppression is asymmetric and event-driven rather than monotonic: it spikes around crises (Cuban Missile Crisis era, ~1962) when the rational-dropout assumption is actively tested and enforced through signaling and control measures, and eases during détente/post-Cold-War periods before rising again with renewed great-power competition. Theater ratio rises gradually as declaratory doctrine and modernization programs increasingly serve institutional and industrial self-justification rather than a freshly re-derived calculation. Accessibility collapse is moderate (0.55): unlike a mountain, the alternative (actually fighting a great-power war) has not vanished from the possibility space — it has been argued out of the active choice set, which is a weaker and more contestable form of closure than physical impossibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and their industrial base sit at the beneficiary end: they set and administer the calculation and capture the coordination benefit (crisis stability, alliance leverage) plus institutional rents (arsenal budgets). Extended deterrence allies are secondary beneficiaries with no independent control over the ledger, which is why their exit is 'constrained' rather than 'arbitrage.' Non-nuclear states under coercion, domestic taxpayers, and future generations are targets: they bear costs (coercive leverage, fiscal burden, accident risk) generated by a calculation they did not help specify and cannot renegotiate, and their exit options are uniformly 'trapped' — there is no unilateral move that removes the risk or cost they carry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing recurrent great-power war in a nuclear-armed world) remains genuinely live for the core dyads the calculation was built around, which is why founding_problem_status is 'contested' rather than 'dead' — this blocks a naive mandatrophy verdict. But the constraint's administration has drifted: modernization programs and doctrinal restatements are increasingly justified by institutional inertia and industrial interest (rising theater_ratio) rather than a fresh re-derivation of the cost-benefit ledger for current threat environments. The Tangled Rope classification captures exactly this: a real coordination function persists alongside extraction that the coordination function does not, by itself, justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_foreclosure,
    'Is nuclear great-power war genuinely still in the reachable set of state behavior (this reading''s premise), or has the physical/organizational infrastructure of mutual destruction actually foreclosed it as the structural_contraction_reading claims?',
    'Historical near-miss analysis (1962, 1983, 1995 Norwegian rocket incident) showing how close command chains actually came to authorized or unauthorized launch would bear on whether ''dropout'' is a live choice being continuously re-made or a foreclosed option that only appears reachable in retrospective doctrine debates.',
    'If near-misses show launch was genuinely one command decision away at multiple points, the rational_dropout framing (war deprioritized but reachable) is vindicated over structural_contraction (war foreclosed); if near-misses show automated or structural safeguards would have prevented escalation regardless of leadership choice, the constraint is better read as physical impossibility, favoring the sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_foreclosure, empirical, 'Whether nuclear war''s persistence-avoidance is best modeled as an ongoing rational choice or a foreclosed physical outcome.').

omega_variable(
    calculation_stability_vs_capture,
    'Is the cost-dominance calculation this reading describes a stable, continuously-valid rational assessment, or has it been captured by the defense-industrial and doctrinal apparatus that benefits from its perpetual re-affirmation regardless of whether the underlying costs and benefits have shifted?',
    'Compare independently-derived cost-benefit assessments (academic, non-proliferation NGO, foreign-government) against official doctrinal restatements over multiple administrations; persistent divergence with official assessments consistently favoring larger arsenals would indicate capture.',
    'If independent assessments consistently track official ones, the Tangled Rope''s coordination component dominates and the constraint is closer to a genuine Rope; if independent assessments diverge sharply and consistently in the direction of smaller justified arsenals, the extraction component dominates and reclassification toward Snare becomes defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(calculation_stability_vs_capture, conceptual, 'Whether the rational-choice calculation is genuinely re-derived or institutionally captured.').

omega_variable(
    rational_dropout_reading_scope,
    'This story is one of three readings of the nuclear_impossibility_kernel (credibility_paradox_reading, rational_dropout_reading, structural_contraction_reading). The three readings share the observable — no nuclear great-power war since 1945 — but attribute it to different mechanisms (self-undermining deterrent credibility; rational cost-dominance calculation with war still reachable; physical impossibility of any rational path to victory). Which mechanism actually governs current leadership behavior?',
    'Comparative process-tracing across historical crises (Cuban Missile Crisis, Able Archer 83, Kargil War, current Russia-NATO tensions) to identify which decision-theoretic language (''too costly to attempt,'' ''threat isn''t credible so we can act,'' ''physically cannot win'') dominates actual leadership deliberation as revealed in declassified records and memoirs.',
    'If leaders consistently reason in cost-dominance terms (this reading), the constraint is best modeled as choice-theoretic and reversible if the cost calculation shifts (e.g., missile defense breakthroughs). If leaders reason in credibility-paradox terms, coercive sub-nuclear brinksmanship becomes the dominant risk. If leaders reason in physical-impossibility terms, the constraint is closer to a Mountain and less sensitive to doctrinal re-justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_dropout_reading_scope, conceptual, 'Which of the three kernel readings best matches actual leadership decision processes, with different classification consequences for each.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(nucl_tr_t1979, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1979, 0.2).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(nucl_tr_t2008, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.22).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(nucl_be_t1979, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1979, 0.38).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(nucl_be_t2008, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2008, 0.34).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(nucl_su_t1979, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(nucl_su_t2008, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_non_proliferation_regime).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_alliance_commitments).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'nuclear impossibility' claim per the ε-invariance principle: structural_contraction_reading (physical impossibility of victory — closest to a Mountain), rational_dropout_reading (this story — cost-dominance removes war from active choice while leaving it reachable — a Tangled Rope), and credibility_paradox_reading (the deterrent threat is self-undermining because use guarantees destruction). Each carries its own ε because each attributes the same observable (no great-power nuclear war since 1945) to a structurally distinct mechanism with different beneficiary/victim structure and different fragility properties. All three are linked via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
