% ============================================================================
% CONSTRAINT STORY: proceduralist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proceduralist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proceduralist_reading
 *   human_readable: Proceduralist Reading: Precommitment as the Price of Evidentiary Standing
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the proceduralist reading of the kernel
 *   'positional disagreement becomes evidence.' On this reading, a
 *   disagreement earns evidentiary weight not because of who holds the
 *   position or what practical outcome it produces, but because it survived a
 *   designed, costly, precommitted procedure — adversarial collaboration with
 *   a jointly authored protocol, preregistration of predictions, and declared
 *   kill conditions agreed in advance by both sides. The evidentiary force is
 *   located in the cost and design of the procedure itself, not in the
 *   position's social standing (the standpoint reading) nor its practical
 *   payoff (the pragmatist/instrumentalist readings). Structurally this
 *   converts a standing-based legitimacy axis into a compliance-based one:
 *   whoever can afford, access, and administratively survive the
 *   precommitment apparatus gains standing regardless of prior social
 *   position, and whoever cannot access or is excluded from the apparatus
 *   loses standing regardless of being correct. The beneficiary/victim
 *   structure here therefore tracks procedural compliance capacity, not
 *   social position — this is the structural delta from the standpoint
 *   reading, where beneficiaries and victims track social location directly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proceduralist_reading, 0.42).
domain_priors:suppression_score(proceduralist_reading, 0.55).
domain_priors:theater_ratio(proceduralist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proceduralist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(proceduralist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(proceduralist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(proceduralist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(proceduralist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proceduralist_reading, tangled_rope).
narrative_ontology:human_readable(proceduralist_reading, "Proceduralist Reading: Precommitment as the Price of Evidentiary Standing").
narrative_ontology:topic_domain(proceduralist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(proceduralist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proceduralist_reading, '4628209d-902e-478f-9dbd-451a92f0a837').
narrative_ontology:cs_kernel_codification('4628209d-902e-478f-9dbd-451a92f0a837', distributed).
narrative_ontology:cs_authority_grounding('4628209d-902e-478f-9dbd-451a92f0a837', practice).
narrative_ontology:cs_interpretation_layer_present('4628209d-902e-478f-9dbd-451a92f0a837').
narrative_ontology:cs_reading_relation('4628209d-902e-478f-9dbd-451a92f0a837', proceduralist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('4628209d-902e-478f-9dbd-451a92f0a837', proceduralist_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4628209d-902e-478f-9dbd-451a92f0a837', proceduralist_reading__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('4628209d-902e-478f-9dbd-451a92f0a837', foundational, evidentiary_force_located_in_procedure_cost).
narrative_ontology:cs_axiom_status(evidentiary_force_located_in_procedure_cost, holdable).
narrative_ontology:cs_axiom_grounding('4628209d-902e-478f-9dbd-451a92f0a837', evidentiary_force_located_in_procedure_cost, conventional).
narrative_ontology:cs_axiom('4628209d-902e-478f-9dbd-451a92f0a837', secondary, precommitment_disciplines_motivated_reasoning_better_than_standing_or_payoff).
narrative_ontology:cs_axiom_status(precommitment_disciplines_motivated_reasoning_better_than_standing_or_payoff, holdable).
narrative_ontology:cs_axiom_grounding('4628209d-902e-478f-9dbd-451a92f0a837', precommitment_disciplines_motivated_reasoning_better_than_standing_or_payoff, instrumental).
narrative_ontology:cs_reference_frame('4628209d-902e-478f-9dbd-451a92f0a837', adversarial_collaboration_as_neutral_arbiter).
narrative_ontology:cs_drift_state('4628209d-902e-478f-9dbd-451a92f0a837', contemporary_credentialing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4628209d-902e-478f-9dbd-451a92f0a837', '').
narrative_ontology:cs_kernel_id(proceduralist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proceduralist_reading, procedure_compliant_disputants).
narrative_ontology:constraint_beneficiary(proceduralist_reading, adversarial_collaboration_brokers).
narrative_ontology:constraint_beneficiary(proceduralist_reading, preregistration_infrastructure_operators).
narrative_ontology:constraint_victim(proceduralist_reading, resource_poor_challengers).
narrative_ontology:constraint_victim(proceduralist_reading, novel_hypothesis_originators).
narrative_ontology:constraint_victim(proceduralist_reading, non_compliant_but_correct_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have the institutional support, funding, and time to design preregistered adversarial collaborations, declare kill conditions in advance, and see the process through. Their positions gain legitimacy not because they are correct but because they survived the costly procedure. They can afford to lose a preregistered bet occasionally and still keep playing.
narrative_ontology:constraint_stakeholder(proceduralist_reading, procedure_compliant_disputants, beneficiary,
    organized, biographical, mobile, national).

% Hold positions that may be correct but cannot afford the time, funding, or institutional backing to run a preregistered adversarial collaboration. Their disagreement is treated as noise until it survives a procedure they structurally cannot access. Exit means either abandoning the claim or trying to piggyback on a well-resourced collaborator's protocol.
narrative_ontology:constraint_stakeholder(proceduralist_reading, resource_poor_challengers, payer,
    powerless, biographical, constrained, national).

% Propose genuinely new positions that do not yet have an established opposing camp willing to co-design a kill condition. The procedure requires two committed adversaries; a position with no institutional rival cannot be proceduralized and therefore cannot earn evidentiary standing under this reading, regardless of its merit.
narrative_ontology:constraint_stakeholder(proceduralist_reading, novel_hypothesis_originators, payer,
    moderate, biographical, constrained, national).

% Design, certify, and administer the precommitment procedures — the adversarial collaboration protocols, the preregistration platforms, the kill-condition templates. They decide what counts as a sufficiently rigorous procedure and can gatekeep which disputes are proceduralizable at all. They collect prestige and funding from running the apparatus itself, independent of which side wins any given dispute.
narrative_ontology:constraint_stakeholder(proceduralist_reading, adversarial_collaboration_brokers, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate the registries, timestamping services, and journals that certify a procedure was followed. Their institutional value depends on the belief that procedure-survival is the correct standard for evidentiary force; they benefit whenever more disputes are routed through their infrastructure, regardless of the truth-value of the outcomes it certifies.
narrative_ontology:constraint_stakeholder(proceduralist_reading, preregistration_infrastructure_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Hold a position that later turns out to be correct but never entered a precommitment procedure, either by choice or by exclusion. Under this reading their claim carries no evidentiary weight until and unless it is retroactively proceduralized, which may happen only after the practical stakes have already been resolved by other means.
narrative_ontology:constraint_stakeholder(proceduralist_reading, non_compliant_but_correct_dissenters, payer,
    powerless, biographical, trapped, national).

% Track which positions survived precommitment procedures and which did not, and compare this record against later-settled truth to evaluate whether procedure-survival tracked correctness or merely tracked institutional access to the procedure.
narrative_ontology:constraint_stakeholder(proceduralist_reading, field_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(proceduralist_reading, diffuse).
narrative_ontology:fixing_cost_class(proceduralist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuine solution to the problem of unfalsifiable, ex-post-rationalized dispute: forcing both sides to declare in advance what would count as losing prevents motivated reasoning from reinterpreting any outcome as a win, and the shared cost of designing the kill condition disciplines vague or unfalsifiable positions on both sides.
% TRANSFER_FUNCTION: Moves evidentiary standing away from positions that cannot afford or access precommitment infrastructure and toward positions whose holders can fund, staff, and administratively survive a designed adversarial procedure — a transfer of epistemic legitimacy from the substance of a claim to the compliance capacity of its holder.
% ABSENT_VOICES: Resource-poor challengers and originators of genuinely novel positions without an established institutional rival are structurally unable to proceduralize their disagreement and so never enter the evidentiary record on this reading's terms; they would object that the procedure measures who can play, not who is right.
% DISAPPEARANCE_RATIONALE: Adversarial collaboration brokers and infrastructure operators would say the field regresses to unfalsifiable positional combat if the procedure vanished. Resource-poor challengers and novel-hypothesis originators would say the actual distribution of correct claims would be unaffected, since the procedure was never tracking their correctness anyway, and only the institutional prestige economy built on top of it would collapse.
% FOUNDING_PROBLEM: Positional disagreements in contested empirical and theoretical domains (forecasting, contested psychological effects, contested policy predictions) were being settled by rhetorical persistence, tribal signaling, and post-hoc reinterpretation of ambiguous outcomes rather than by anything that could discipline belief update.
% FOUNDING_PROBLEM_CORROBORATION: Adversarial collaboration brokers and infrastructure operators attest the founding problem is fully live and the procedure is the solution working as intended. Field historians and several resource-poor challengers, from outside the benefiting institutional set, attest that the procedure has increasingly become a compliance filter correlated with funding access rather than a corrective to motivated reasoning, citing cases where well-funded but ultimately wrong positions survived procedures that poorer but correct positions never had the standing to enter.
narrative_ontology:disappearance_verdict(proceduralist_reading, contested).
narrative_ontology:founding_problem_status(proceduralist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(proceduralist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(proceduralist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(proceduralist_reading, 0.42, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proceduralist_reading_tests).
:- end_tests(proceduralist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.22 to 0.42) because the coordination function is genuine at the outset — precommitment really does discipline motivated reasoning — but the apparatus for administering procedures (accreditation of adversarial collaboration protocols, registry infrastructure, brokerage prestige) has accreted its own extraction over time as it became a gatekept credentialing layer. Suppression is moderate (0.55) because access to the procedure, not the procedure's logic itself, is the mechanism that excludes; theater ratio is meaningfully high and rising (0.38) because an increasing share of 'the procedure was followed' certification has become a signal of institutional access rather than of actual epistemic discipline having been exercised.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (adversarial collaboration brokers), the arrangement looks like a rope: a genuine, hard-won solution to an intractable coordination problem in contested epistemics. From the payer seats (resource-poor challengers, novel-hypothesis originators), the same structure looks like a tangled rope shading toward snare: a real coordination function riding on top of an access filter that happens to correlate with existing institutional power. The engine should register this divergence structurally rather than resolve it — the claim (tangled_rope) already reflects that both the coordination function and the asymmetric extraction are present and require active enforcement (certification, gatekeeping of what counts as a valid procedure) to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Procedure-compliant disputants and infrastructure operators are structural beneficiaries: they collect legitimacy or institutional value from running the apparatus regardless of substantive outcome. Resource-poor challengers, novel-hypothesis originators without an institutional rival to co-design a kill condition, and non-compliant-but-correct dissenters are targets: the procedure structurally cannot register their claims, converting exclusion from the apparatus into exclusion from evidentiary standing. This is the reading's defining move — directionality here tracks compliance capacity, not the social-location axis the standpoint reading would use, and not the practical-payoff axis the instrumentalist/pragmatist readings would use.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unfalsifiable positional combat — remains partially live, which is why this is authored as tangled_rope rather than piton: the coordination function still does real work when the procedure is actually followed by parties with equal access. But the founding_problem_status is authored as contested because a second function has grown alongside the original one: procedural compliance has become a credentialing layer whose value to infrastructure operators is partly independent of whether it is still solving the original epistemic problem. This is a Tangled Rope, not a Piton, because there ARE concentrated beneficiaries (brokers, infrastructure operators) actively maintaining the arrangement through certification gatekeeping — a piton would require no one profiting enough to maintain it, which is not the case here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_access_vs_position_correctness,
    'Does survival of a precommitted adversarial procedure actually correlate with substantive correctness, or does it primarily correlate with the disputants'' prior institutional access and funding?',
    'Longitudinal audit comparing procedure-survival outcomes against later-settled ground truth across a large sample of adversarial collaborations, controlling for the funding and institutional status of each side at the time the procedure was designed.',
    'If correlation with correctness is high and independent of access, this reading''s coordination function dominates and the arrangement is closer to a genuine rope. If correlation with prior access is high and correctness is weak, the arrangement is closer to a snare wearing a coordination costume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_access_vs_position_correctness, empirical, 'Whether procedure-survival tracks truth or institutional access.').

omega_variable(
    proceduralizability_gate,
    'Is the requirement that a dispute have two willing, resourced adversaries able to co-design a kill condition an inherent feature of rigorous precommitment, or is it a contingent, potentially reformable gatekeeping bottleneck?',
    'Examine whether alternative procedural designs (e.g., unilateral preregistration with third-party adjudication, algorithmic kill-condition generation) can substitute for bilateral co-design without losing the discipline against motivated reasoning.',
    'If bilateral co-design is inherent, the exclusion of novel-hypothesis originators without an institutional rival is a structural, hard-to-fix limitation. If substitutable, the exclusion is a contingent design choice and the current arrangement''s extraction is more easily reformable, weakening the claim to tangled_rope in favor of scaffold with a feasible sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proceduralizability_gate, conceptual, 'Whether bilateral precommitment is structurally necessary or a reformable design choice.').

omega_variable(
    committer_framing_alternative,
    'Could this same dispute be authored under the standpoint_reading''s framing instead, with beneficiary/victim tracking social position rather than procedural compliance, and would that alternative framing change the computed classification?',
    'Compare this story''s stakeholder set and beneficiary/victim assignments against a parallel standpoint_reading story generated for the same underlying dispute; check whether the same named agents (e.g. resource_poor_challengers) occupy structurally analogous positions under both framings or diverge.',
    'If the two framings assign the same agents to victim/beneficiary roles for different underlying reasons (compliance capacity vs. social standing), the readings are complementary lenses on the same population and the kernel decomposition is doing real work. If they diverge sharply, the choice of reading materially changes who counts as harmed, which is exactly the committer-structure the kernel framework is designed to surface via separate stories rather than a single averaged constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the proceduralist and standpoint readings converge or diverge on victim identification for the same underlying dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proceduralist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, proceduralist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(proc_tr_t4, proceduralist_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(proc_tr_t8, proceduralist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(proc_tr_t12, proceduralist_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement(proc_tr_t16, proceduralist_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(proc_tr_t20, proceduralist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(proc_tr_t24, proceduralist_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, proceduralist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(proc_be_t4, proceduralist_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(proc_be_t8, proceduralist_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(proc_be_t12, proceduralist_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(proc_be_t16, proceduralist_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(proc_be_t20, proceduralist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(proc_be_t24, proceduralist_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(proc_su_t0, proceduralist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(proc_su_t4, proceduralist_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(proc_su_t8, proceduralist_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(proc_su_t12, proceduralist_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(proc_su_t16, proceduralist_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(proc_su_t20, proceduralist_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(proc_su_t24, proceduralist_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proceduralist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(proceduralist_reading, 0.12).
narrative_ontology:affects_constraint(proceduralist_reading, standpoint_reading).
narrative_ontology:affects_constraint(proceduralist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(proceduralist_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel positional_disagreement_as_evidence. Each reading (proceduralist, standpoint, pragmatist, instrumentalist) locates evidentiary legitimacy in a structurally distinct place — procedural survival, social position, practical workability, and predictive payoff respectively — and each authors its own ε and beneficiary/victim structure per the ε-invariance principle. They are linked here rather than merged because measuring 'positional disagreement as evidence' by any one of these observables yields a different ε and a different victim set than measuring it by another; forcing them into one constraint would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
