% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition Reading of Autonomous Weapons under the Martens Clause
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the categorical prohibition reading of the IHL
 *   distinction/proportionality kernel as it applies to lethal autonomous
 *   weapons systems (LAWS). Under this reading, drawn from CCW-forum advocacy
 *   and a strand of Martens Clause scholarship, the wrongness of autonomous
 *   lethal targeting is located in the act of machine decision itself —
 *   'crossing the threshold' — not in any measurable output. This makes the
 *   reading categorically insensitive to technical performance data: a LAWS
 *   that outperformed human operators on every distinction/proportionality
 *   metric would remain unlawful per se under this reading, which is the
 *   structural feature that most sharply distinguishes it from the sibling
 *   outcomes_based_reading. The reading functions as a real coordination
 *   device (verification-light bright line in an environment where
 *   comparative performance auditing during hostilities is largely
 *   infeasible) while also functioning as an asymmetric transfer mechanism,
 *   moving technological and reputational advantage from states/firms with
 *   mature autonomous systems programs toward states without that capability
 *   and toward the civil society coalitions that authored the norm.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.42).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition Reading of Autonomous Weapons under the Martens Clause").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '7074b7e6-9f16-484b-a4bf-03bf9fc62bbf').
narrative_ontology:cs_kernel_codification('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', distributed).
narrative_ontology:cs_authority_grounding('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', distributed).
narrative_ontology:cs_reading_relation('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', foundational, machine_decision_dignity_violation_per_se).
narrative_ontology:cs_axiom_status(machine_decision_dignity_violation_per_se, holdable).
narrative_ontology:cs_axiom_grounding('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', machine_decision_dignity_violation_per_se, deontological).
narrative_ontology:cs_axiom('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', foundational, technical_performance_normatively_irrelevant).
narrative_ontology:cs_axiom_status(technical_performance_normatively_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', technical_performance_normatively_irrelevant, deontological).
narrative_ontology:cs_reference_frame('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', martens_clause_as_gap_filling_residual_protection).
narrative_ontology:cs_drift_state('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', post_ccw_group_of_governmental_experts_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('7074b7e6-9f16-484b-a4bf-03bf9fc62bbf', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, arms_control_advocacy_organizations).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_technology_industrial_base).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_commanders_seeking_precision_tools).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_as_categorical_limit).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_as_freestanding_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalitions like the Campaign to Stop Killer Robots draft treaty language, lobby the CCW and UN bodies, and frame machine-decided killing as a categorical dignity violation. They gain moral authority, funding, and political standing from the prohibition succeeding, and bear none of the military or economic costs of a ban. Their exit from the debate is costless; a failed campaign simply continues into the next diplomatic cycle.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, agenda_setter).

% States without the industrial or data infrastructure to build competitive autonomous weapons systems support a categorical ban because it freezes the military technology gap at a point favorable to them, locking a rival's emerging capability advantage into illegality rather than closing the gap through their own investment.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    institutional, generational, mobile, global).

% Nations with mature autonomous targeting and sensor-fusion programs face a legal instrument that would render years of R&D and deployed systems unlawful per se, regardless of demonstrated distinction/proportionality performance. Their exit options are treaty non-ratification or reservation, but the instrument's normative weight (Martens Clause framing, public conscience appeal) generates reputational and diplomatic costs even for non-signatories.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, civilizational, constrained, global).

% Contractors and research programs building autonomous targeting systems face sunk R&D losses, contract cancellation risk, and a prohibition applied ex ante to a technology class rather than to demonstrated failures. They can lobby against ratification and diversify into human-in-the-loop variants, but the categorical framing forecloses the performance-based defense they would otherwise mount.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_technology_industrial_base, payer,
    powerful, biographical, constrained, national).

% Field commanders who might use autonomous systems to reduce collateral harm relative to human-operated systems under time pressure or degraded communications are denied that option categorically, regardless of a system's measured performance, because the reading treats the delegation itself — not the outcome — as the violation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_commanders_seeking_precision_tools, payer,
    moderate, immediate, trapped, regional).

% Civilians whose exposure to lethal force would be shaped by whichever targeting regime is actually deployed have no seat in the treaty negotiations. Their interest — minimizing wrongful death regardless of whether the decision-maker is human or machine — is asserted by advocates on their behalf but is not independently tested against comparative performance data.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilian_populations_in_conflict_zones, excluded,
    powerless, immediate, trapped, regional).

% Legal scholars and the ICRC interpret the Martens Clause's reach, weighing whether 'principles of humanity and dictates of public conscience' operate as an independent, freestanding prohibition or as an interpretive gap-filler subordinate to the treaty text. Their scholarship shapes which reading gains traction in state practice and opinio juris.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, icrc_and_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, verification-light rule that avoids the difficulty of measuring comparative distinction/proportionality performance between machine and human decision-makers in the fog of war — a real coordination problem, since case-by-case performance auditing during active hostilities is largely infeasible.
% TRANSFER_FUNCTION: Moves military-technological advantage away from states and firms with mature autonomous targeting programs toward states and advocacy coalitions that gain from freezing the technology at illegality; moves reputational and diplomatic capital toward civil society organizations that authored the norm.
% ABSENT_VOICES: Civilian populations in active conflict zones, whose actual casualty outcomes under alternative targeting regimes are the ostensible object of concern, are not party to the treaty negotiations; their interests are asserted by advocacy coalitions rather than tested against comparative empirical performance data.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition reading were abandoned, the human_agency and outcomes_based readings would immediately fill the interpretive space of the same underlying IHL text — the kernel itself (distinction/proportionality obligations) would persist. Advocacy coalitions dispute that the world would be unchanged (they hold the categorical bar as doing independent normative work); states with LAWS capability would treat its disappearance as removing a legal and reputational constraint on deployment.
% FOUNDING_PROBLEM: The Martens Clause was drafted in 1899 to prevent a normative vacuum whenever treaty text failed to anticipate a new weapon or method of warfare, ensuring 'principles of humanity and dictates of public conscience' would fill gaps rather than leaving unregulated conduct lawful by omission.
% FOUNDING_PROBLEM_CORROBORATION: ICRC legal opinions and a substantial body of IHL scholarship attest that the gap-filling function remains live for genuinely unanticipated weapons. Independent international law scholars outside the advocacy coalitions (e.g., academic commentary skeptical of Article 36 review politics) note that the clause's use here extends beyond its historical gap-filling function into a freestanding categorical bar not clearly supported by state practice or opinio juris — a reading contested by states with advanced systems and by scholars who see the clause as interpretive aid rather than independent prohibition.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, contested).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-moderate (0.68) and rising over the measurement interval: as the categorical framing has hardened from a 2013 human-rights-report proposal into treaty-negotiation language pressed at the CCW, its practical effect of foreclosing an entire technology class regardless of performance has become more consequential for states and firms with deployed or near-deployed systems. Suppression is moderate (0.42) — the mechanism operates mainly through reputational and diplomatic pressure and Article 36-style review politics rather than binding enforcement, since no binding categorical ban currently exists; that keeps the raw suppression figure well below what a fully codified treaty prohibition would carry. Accessibility collapse is authored low-moderate (0.35): the human_agency and outcomes_based readings remain fully live alternatives within the same legal community, so the categorical reading has not foreclosed the interpretive space even where it has captured significant advocacy momentum. Resistance is authored high (0.72): states with advanced systems, defense industrial actors, and outcomes-based legal scholars actively contest the categorical framing in CCW sessions and academic literature.
 *
 * PERSPECTIVAL GAP:
 *   From the advocacy-coalition seat, this reading is a rope: a genuine humanitarian coordination achievement closing a dangerous gap in the law before an irreversible technology entrenches. From the seat of a state with a mature, human-rights-compliant autonomous targeting program, the same reading operates as a tangled rope at best — a real coordination function (avoiding fog-of-war performance litigation) bundled with an asymmetric transfer that ignores their system's actual measured performance. The engine's per-seat computation should reflect that divergence structurally, not because either seat is wrong about their own position.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society organizations and states lacking LAWS capability are the structural beneficiaries: the former gain political and moral capital independent of any casualty outcome, the latter gain a legal instrument that freezes a capability gap they cannot otherwise close through investment. States with advanced autonomous systems, their defense industrial base, and field commanders who might have used performance-vindicated systems are the targets — the categorical framing extracts from them precisely because it is performance-insensitive: no amount of favorable test data changes their legal exposure under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Martens Clause gap-filling for genuinely unanticipated weapons) remains partially live — new weapons categories continue to emerge faster than treaty text — but the specific extension to a categorical, performance-insensitive prohibition on an entire technology class is a much stronger claim than the clause's historical gap-filling function, and that extension is exactly what independent scholarship (per founding_problem_corroboration) contests. This is not mandatrophy in the classic sense of an arrangement whose function fully died; it is a live contest over how far a genuinely still-live founding function (gap-filling) can be stretched to justify a categorical rather than a case-by-case rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    martens_clause_freestanding_scope,
    'Does the Martens Clause function as an independent, freestanding source of prohibition capable of banning an entire technology class regardless of demonstrated performance, or does it function only as an interpretive gap-filler subordinate to treaty text and customary practice, activated only where no other rule addresses the conduct?',
    'Track state practice and opinio juris at CCW sessions and subsequent ICJ or treaty-body pronouncements; a body of state practice treating the clause as freestanding would support this reading, while practice treating it as strictly interpretive would undercut the categorical claim''s legal grounding.',
    'If the clause is freestanding, this reading''s high ε reflects a legally grounded categorical prohibition; if the clause is merely interpretive, the same ε reflects advocacy pressure exceeding the clause''s actual legal authority, and the constraint''s persistence would depend more on suppression/reputational mechanisms than on settled law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_freestanding_scope, conceptual, 'Whether the Martens Clause independently supports a categorical ban or only fills interpretive gaps.').

omega_variable(
    performance_insensitivity_justification,
    'Is a rule that ignores demonstrated distinction/proportionality performance data justified on precautionary grounds (irreversibility, verification difficulty in hostilities) or does performance-insensitivity function mainly to protect the reading''s political tractability by avoiding a performance debate the categorical coalition might lose?',
    'Comparative empirical research on autonomous vs. human targeting performance in realistic combat conditions, and analysis of whether advocacy coalitions engage with or avoid such data when it becomes available.',
    'If precautionary logic dominates, the high ε partly reflects a defensible risk-management stance; if political tractability dominates, the categorical framing functions closer to protecting a beneficiary coalition''s negotiating position from a performance-based rebuttal it could not win.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performance_insensitivity_justification, empirical, 'Whether performance-insensitivity is precautionary or strategically self-protective.').

omega_variable(
    civilian_interest_representation_fidelity,
    'Does the categorical prohibition reading actually track the interests of civilians in conflict zones (the stated beneficiaries of the underlying humanitarian concern), or does it track the interests of the advocacy coalitions and technologically disadvantaged states that author and benefit from the norm, with civilian interest serving as legitimating rhetoric?',
    'Compare casualty and harm outcomes across conflicts using autonomous versus human-directed targeting where data exists, and assess whether advocacy positioning shifts when performance data favors autonomous systems.',
    'If civilian interest and the categorical rule diverge in cases where autonomous systems demonstrably reduce harm, that divergence would support treating the excluded-voices gap as substantively consequential rather than merely procedural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_interest_representation_fidelity, empirical, 'Whether the categorical rule tracks civilian welfare or serves as legitimating cover for coalition interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2013, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(ihl__tr_t2017, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement(ihl__tr_t2019, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2019, 0.23).
narrative_ontology:measurement(ihl__tr_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2022, 0.26).
narrative_ontology:measurement(ihl__tr_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2013, 0.45).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(ihl__be_t2017, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2017, 0.55).
narrative_ontology:measurement(ihl__be_t2019, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(ihl__be_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(ihl__be_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2015, 0.33).
narrative_ontology:measurement(ihl__su_t2017, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2017, 0.36).
narrative_ontology:measurement(ihl__su_t2019, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2019, 0.38).
narrative_ontology:measurement(ihl__su_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2022, 0.4).
narrative_ontology:measurement(ihl__su_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.1).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ihl_distinction_proportionality kernel. categorical_prohibition_reading (this file) authors the highest ε: it forecloses an entire technology class regardless of performance. human_agency_reading authors a narrower ε: it requires irreducible human judgment at the moment of force but does not categorically ban the underlying technology (a human-supervised autonomous system with a genuine decision point could satisfy it). outcomes_based_reading authors the lowest ε among the three: it treats law as technology-neutral and satisfied by demonstrated performance parity, making beneficiary/victim assignment performance-contingent rather than fixed. Each reading has its own stable ε assessed against the same underlying standing arrangement (current state practice and treaty negotiation posture around LAWS), per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
