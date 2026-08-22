% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency Requirement for Lethal Force Decisions
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the human_agency_reading of the
 *   ihl_distinction_proportionality kernel. It holds that IHL's distinction
 *   and proportionality obligations structurally require a human moral agent
 *   at the moment of lethal force application — not merely a system that
 *   produces statistically compliant outputs. The Martens Clause is read as
 *   protecting the irreducible role of human judgment, not as a gap-filler
 *   for unregulated weapons. The constraint suppresses fully autonomous
 *   weapons systems (LAWS) by declaring them categorically unlawful, while
 *   authorizing human-supervised autonomy. This reading benefits IHL
 *   interpretive authorities (especially ICRC) by maintaining their
 *   centrality as the arbiters of what constitutes sufficient human control,
 *   while extracting operational efficiency from militaries and foreclosing
 *   development paths for autonomous weapons developers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.78).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.85).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force Decisions").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '4a04668d-99f4-4bf6-9e3d-d2ede143e8b2').
narrative_ontology:cs_kernel_codification('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', formalized).
narrative_ontology:cs_authority_grounding('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', lineage).
narrative_ontology:cs_interpretation_layer_present('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2').
narrative_ontology:cs_reading_relation('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', foundational, human_moral_judgment_irreducible_at_force_application).
narrative_ontology:cs_axiom_status(human_moral_judgment_irreducible_at_force_application, holdable).
narrative_ontology:cs_axiom_grounding('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', human_moral_judgment_irreducible_at_force_application, deontological).
narrative_ontology:cs_axiom('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', secondary, martens_clause_protects_human_agency_not_only_outcomes).
narrative_ontology:cs_axiom_status(martens_clause_protects_human_agency_not_only_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', martens_clause_protects_human_agency_not_only_outcomes, deontological).
narrative_ontology:cs_reference_frame('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', martens_clause_humanity_public_conscience).
narrative_ontology:cs_drift_state('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', contemporary_laws_development_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a04668d-99f4-4bf6-9e3d-d2ede143e8b2', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_monopoly).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, states_pursuing_laws_capability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce IHL's distinction and proportionality rules through commentary, advisory opinions, and diplomatic pressure. Maintain that human moral judgment is irreducible and non-delegable. Their institutional relevance depends on being the authoritative voice on what IHL requires in novel technological contexts.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Holds unique mandate as guardian of IHL; its interpretive authority is reinforced when the law requires continuous human judgment rather than technical compliance metrics. Gains institutional centrality from the reading that IHL's core obligations cannot be automated.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_monopoly, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, icrc_interpretive_monopoly, agenda_setter).

% Bears the cost of keeping humans in lethal decision loops: slower engagement cycles, cognitive load on operators, inability to leverage speed and scale advantages of full autonomy. Constrained exit because treaty obligations and reputational costs bind major militaries to IHL compliance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency, payer,
    powerful, biographical, constrained, global).

% Investment in fully autonomous targeting systems is suppressed; R&D must pivot to human-supervised architectures. Exit constrained by dual-use technology controls and the fact that defense procurement follows IHL interpretive consensus.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    organized, biographical, constrained, global).

% States seeking military advantage through LAWS face legal and diplomatic barriers. The reading authorizes only human-supervised autonomy, forcing costly compliance architectures or strategic restraint. Exit constrained by treaty regimes and the political cost of being labeled an IHL violator.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, states_pursuing_laws_capability, payer,
    powerful, generational, constrained, global).

% Theoretically protected by the human judgment requirement — human operators may exercise restraint that algorithms would not. But also bear risk if human judgment fails or is corrupted. Trapped exit: cannot opt out of being subject to targeting decisions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict, beneficiary,
    powerless, biographical, trapped, global).

% Negotiate potential LAWS treaty at CCW. Their mandate is shaped by which reading of IHL prevails — human agency reading pushes toward a binding protocol requiring meaningful human control; outcomes-based reading permits technical compliance standards.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, laws_treaty_negotiators, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared interpretive framework for applying IHL's distinction and proportionality rules to emerging weapons technologies, preventing a race to the bottom where states deploy untested autonomous systems and claim compliance.
% TRANSFER_FUNCTION: Transfers operational speed, scale, and risk-tolerance from military forces to the constraint: forces must accept slower, human-limited engagement cycles in exchange for legal legitimacy and the protective framework IHL provides to their own personnel and civilians.
% ABSENT_VOICES: Non-state armed groups and lesser military powers who cannot afford human-supervised autonomy architectures but also are not at the CCW negotiating table. They would argue the constraint entrenches capability asymmetry favoring wealthy states. Also absent: future civilians in conflicts not yet begun, whose protection the constraint claims to secure.
% DISAPPEARANCE_RATIONALE: If the human agency requirement vanished overnight, major militaries would rapidly deploy fully autonomous targeting systems; the CCW process would collapse or pivot to purely technical standards; IHL interpretive authorities would lose their central adjudicative role; the Martens Clause would become a dead letter in weapons review.
% FOUNDING_PROBLEM: The Martens Clause and IHL's core principles were formulated when human agency was the only possible agency. The founding problem is whether the law's moral logic — that distinction and proportionality require a moral agent capable of context-sensitive judgment — survives the technological possibility of machines that simulate the outputs of that judgment without its inner structure.
% FOUNDING_PROBLEM_CORROBORATION: ICRC and major IHL scholars (e.g., Sassòli, Dörmann) attest the problem is live: the moral structure of IHL requires a human judge. States with advanced LAWS programs (US, Israel, Russia) and outcomes-based legal scholars attest the problem is substantially solved: law governs effects, not internal processes. No neutral arbiter exists — the contest is the point.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the constraint forecloses an entire technological trajectory (full autonomy in lethal decisions) that major military powers are actively pursuing, forcing them into more costly human-supervised architectures. Suppression is very high (0.85) because the constraint's persistence depends on active diplomatic and legal enforcement — the CCW process, weapons review obligations, and reputational pressure — not on voluntary compliance. Theater ratio is low-moderate (0.22) because the human-supervised autonomy authorized by this reading performs genuine coordination (maintaining legal coherence, preventing unaccountable killing) but a growing share of enforcement activity serves to defend the interpretive authority of the IHL establishment against outcomes-based challenges. Accessibility collapse (0.68) reflects that once the moral logic of irreducible human judgment is accepted, alternative framings (outcomes-based, categorical prohibition) appear as either morally insufficient or politically unrealistic. Resistance (0.55) is significant: major military powers resist the reading through foot-dragging at CCW, broad interpretations of 'human supervision,' and parallel development of autonomous capabilities.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC/interpretive authority seat, the constraint is genuine coordination: it prevents a regulatory vacuum where states deploy unaccountable systems. From the military/operational seat, the same constraint operates as enforced extraction: it forces acceptance of known tactical disadvantages (speed, scale, force protection) in exchange for legal legitimacy that adversaries may not reciprocate. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities and ICRC are structural beneficiaries (d near 0.0): the constraint secures their institutional relevance and interpretive monopoly. Military operational efficiency, autonomous weapons developers, and states pursuing LAWS are structural targets (d near 1.0): they bear the full cost of compliance and foreclosed capability. Civilian populations are incidental beneficiaries (d ~0.3): they gain theoretical protection but cannot enforce it and bear risk if human judgment fails. LAWS treaty negotiators are analytical observers (d = 0.5): their role is to mediate the contest, not to win or lose from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem — whether IHL's moral logic survives machine agency — remains genuinely contested. If autonomous systems eventually demonstrate distinction/proportionality performance exceeding humans in all measurable dimensions, the coordination function (preventing a race to the bottom) may atrophy while the extraction (foreclosing superior capability) persists — a classic mandatrophy trap. The constraint currently shows no sunset mechanism; its persistence depends on the interpretive authority's ability to maintain the moral distinction between human judgment and machine simulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducible_judgment_boundary,
    'Is there a principled boundary between human moral judgment and machine simulation of that judgment that survives improving technical performance, or does the distinction collapse into a performance threshold?',
    'Philosophical analysis of moral agency combined with empirical testing of whether human operators in supervised-autonomy systems exercise genuine judgment or merely rubber-stamp algorithmic outputs (automation bias studies).',
    'If the boundary is principled, the reading''s core claim holds regardless of technical progress; if it collapses to performance, the reading converges toward outcomes_based_reading or becomes a pure extraction barrier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreducible_judgment_boundary, conceptual, 'Whether the human/machine moral agency distinction is structurally robust or performance-contingent.').

omega_variable(
    interpretive_authority_capture,
    'Does the ICRC/interpretive authority''s beneficiary position create a structural incentive to maintain the human agency requirement beyond its genuine coordination function?',
    'Historical analysis of ICRC positions on previous weapons technologies (chemical, nuclear, landmines) — did interpretive authority expand, contract, or track genuine humanitarian need?',
    'If interpretive authority systematically expands to maintain relevance, part of the measured extraction is rent-seeking by the interpretive establishment rather than humanitarian coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_capture, empirical, 'Whether the beneficiary structure indicates institutional self-preservation masquerading as moral principle.').

omega_variable(
    committer_frame_location,
    'Where exactly does the structural disagreement between the three kernel readings locate — in the axiom of human moral irreducibility, the scope of Martens Clause, or the definition of ''meaningful human control''?',
    'Map each reading''s axioms to specific treaty articles, ICJ advisory opinions, and ICRC commentaries to identify the minimal structural delta.',
    'If the delta is axiom-level (human_agency_reading holds ''judgment is irreducible''; outcomes_based_reading holds ''effects are what law governs''), forecloses is the correct relation. If the delta is scope-of-application, coexists_with or influences may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Commitment-system framing under-determination: which structural element the readings actually disagree on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(ihl__tr_t2014, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(ihl__tr_t2018, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(ihl__tr_t2022, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(ihl__tr_t2026, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2030, 0.22).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(ihl__be_t2014, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(ihl__be_t2018, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement(ihl__be_t2022, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2022, 0.7).
narrative_ontology:measurement(ihl__be_t2026, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2026, 0.75).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(ihl__su_t2014, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement(ihl__su_t2018, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(ihl__su_t2022, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2022, 0.78).
narrative_ontology:measurement(ihl__su_t2026, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2026, 0.82).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ccw_laws_protocol_negotiations).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, weapons_review_obligations_article36).

% DUAL FORMULATION NOTE:
% This story is one member of the ihl_distinction_proportionality constraint family. The kernel decomposes into three readings with different ε values and different beneficiary/victim structures. This reading (human_agency) has ε=0.78 and tangled_rope structure; categorical_prohibition_reading likely has higher ε (snare) and outcomes_based_reading lower ε (rope or scaffold). All three share the Martens Clause as kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, powerless, 0.35).
constraint_indexing:directionality_override(ihl_distinction_proportionality__human_agency_reading, powerful, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
