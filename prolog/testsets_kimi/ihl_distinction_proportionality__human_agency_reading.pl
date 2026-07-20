% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Distinction and Proportionality: Human Agency Reading
 *   domain: legal/military/technological
 *
 * SUMMARY:
 *   This constraint instantiates the human_agency_reading of the
 *   ihl_distinction_proportionality kernel. It holds that existing
 *   International Humanitarian Lawâspecifically distinction and
 *   proportionality obligations, read through the Martens Clauseâcontains
 *   an irreducible requirement for human moral judgment at the moment of
 *   lethal force application. The reading renders fully autonomous lethal
 *   systems categorically unlawful and authorizes only human-supervised
 *   autonomy. Sibling readings are outcomes_based_reading
 *   (performance-neutral means) and categorical_prohibition_reading (ban
 *   regardless of technical performance). The claim/metric independence is
 *   deliberate: the constraint is claimed as tangled_rope because it carries
 *   a genuine coordination function in civilian protection, while the
 *   authored metrics describe an actively enforced, substantially extractive
 *   structure that accumulates institutional centrality for IHL authorities
 *   at the cost of military operational freedom.
 *
 * KEY AGENTS:
 *   - ihl_interpretive_authorities: Agenda-setter and beneficiary (institutional/global/identity_locked) â maintains interpretive centrality and institutional relevance through the human-agency gate.
 *   - military_operators: Primary payer (organized/global/constrained) â bears operational risk and force-protection costs of human-in-the-loop requirements.
 *   - military_innovator_states: Secondary payer (institutional/global/constrained) â diplomatically and legally constrained from fielding autonomous systems.
 *   - civilian_populations_in_conflict: Coordination beneficiary (powerless/local/trapped) â receives the protective promise of human accountability.
 *   - autonomous_weapons_industry: Excluded target (powerful/global/constrained) â foreclosed from fully autonomous lethal markets.
 *   - outcomes_based_advocates: Excluded voice (moderate/global/constrained) â structurally shut out of the interpretive framework.
 *   - independent_legal_observers: Analytical observer (analytical/global/analytical) â tracks text-to-practice divergence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.78).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Distinction and Proportionality: Human Agency Reading").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "legal/military/technological").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'e718710b-1c4c-4b6c-9a81-6ed056ae7dc1').
narrative_ontology:cs_kernel_codification('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', fixed_text).
narrative_ontology:cs_authority_grounding('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', lineage).
narrative_ontology:cs_interpretation_layer_present('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1').
narrative_ontology:cs_reading_relation('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', foundational, irreducible_human_moral_judgment).
narrative_ontology:cs_axiom_status(irreducible_human_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', irreducible_human_moral_judgment, deontological).
narrative_ontology:cs_axiom('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', foundational, machine_delegation_violates_humanity).
narrative_ontology:cs_axiom_status(machine_delegation_violates_humanity, holdable).
narrative_ontology:cs_axiom_grounding('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', machine_delegation_violates_humanity, deontological).
narrative_ontology:cs_reference_frame('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', human_judgment_as_legal_prerequisite).
narrative_ontology:cs_drift_state('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', autonomous_weapons_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e718710b-1c4c-4b6c-9a81-6ed056ae7dc1', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operators).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_innovator_states).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_humanity_principle).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, irreducible_human_judgment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and promote IHL norms, defining what constitutes legally sufficient human control over lethal targeting. Their institutional relevance, funding access, and agenda-setting authority in the CCW and UN processes depend on maintaining that human moral judgment remains the non-negotiable center of the legal framework.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, beneficiary).

% Must design targeting chains that preserve a human in or on the loop even when autonomous systems could execute faster or more accurately. They bear increased risk to their own forces and the operational friction of machine reaction times being throttled by human deliberation cycles.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operators, payer,
    organized, biographical, constrained, global).

% Diplomatically constrained from deploying fully autonomous lethal systems by the legal interpretive environment. Must invest in meaningful human control compliance architectures and Article 36 legal reviews that add cost and delay to procurement.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_innovator_states, payer,
    institutional, generational, constrained, global).

% The declared protected party under IHL; the arrangement promises that a human moral agent will remain accountable for any lethal decision affecting them, preserving legal recourse and, in theory, reducing indiscriminate machine error.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Legally foreclosed from selling fully autonomous lethal targeting systems to states adhering to this reading. Their product lines and R&D investment in autonomous lethality are suppressed by the interpretive barrier.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_industry, excluded,
    powerful, biographical, constrained, global).

% Technologists and legal scholars arguing that performance-based regulation would better protect civilians. Their position is structurally excluded from the human-agency framework because the constraint predicates lawfulness on the identity of the decision-maker rather than on measurable outcomes.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, outcomes_based_advocates, excluded,
    moderate, biographical, constrained, global).

% Assess whether the human agency requirement is a faithful interpretation of existing IHL or a normative innovation constructing new law under the guise of interpretation. They track the divergence between treaty text and applied standard.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, independent_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that life-and-death targeting decisions in armed conflict are subject to human moral judgment, intended to protect civilian populations from unaccountable machine targeting and preserve legal accountability chains.
% TRANSFER_FUNCTION: Transfers interpretive authority and institutional centrality to IHL bodies and operational risk to military forces, while foreclosing market access for fully autonomous lethal systems developers.
% ABSENT_VOICES: Technologists and outcomes-based legal scholars who argue that performance parity would better protect civilians; military innovators who could reduce force protection risks through autonomy; populations in states that reject the human-agency reading and are excluded from CCW consensus processes.
% DISAPPEARANCE_RATIONALE: If the human agency requirement vanished, fully autonomous lethal systems would become legally viable for states that wanted them, shifting military procurement, targeting doctrine, and civilian risk profiles. IHL interpretive authorities would lose their gatekeeping role over the technology, and the global normative structure would reorganize around outcomes-based or prohibition-based alternatives.
% FOUNDING_PROBLEM: The advent of autonomous weapons systems threatening to remove human moral judgment from targeting decisions, potentially causing unaccountable civilian harm and eroding the legal and ethical foundations of the law of armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: IHL interpretive authorities and humanitarian NGOs attest the problem is live and worsening. Military historians and weapons scientists attest that the problem is partially manufacturedâautonomous systems might reduce certain categories of human errorâand that the founding problem conflates technological change with legal crisis. No neutral corroboration exists outside the humanitarian legal community and its critics within the military-technical complex; the two camps are adversarial rather than corroborative.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the constraint suppresses an entire technology class irrespective of demonstrated performance, transferring operational risk to military parties. Suppression is high (0.72) because persistence depends on active legal enforcement through treaty interpretation, weapons review processes, and diplomatic pressure in the CCW. Theater is moderate (0.45): the protective function is real, but an increasing share of enforcement activity defends the human-agency boundary as a legal-theater performance rather than measurable civilian protection. Accessibility collapse is high (0.82) because once the reading is accepted, fully autonomous alternatives are legally foreclosed with no residual pathway. Resistance is moderate-high (0.68) because major military powers and technologists actively contest the reading in diplomatic fora.
 *
 * PERSPECTIVAL GAP:
 *   From the IHL interpretive seat, the constraint is a necessary guardian of legal accountability and humanitarian protectionâa bulwark against moral outsourcing. From the military operator seat, the same structure reads as institutionalized friction that extracts force protection and operational tempo to sustain the authority of a non-combatant interpretive class. The engine computes this divergence from identical structural data; the authored claim (tangled_rope) does not adjudicate which seat is correct, only that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities sit near the beneficiary end: they collect institutional centrality, agenda control, and funding relevance from the constraint's operation. Civilian populations sit near the beneficiary end for coordination but are powerless and trapped, which modulates their effective position. Military operators and innovator states sit near the full-target end: they bear the extraction in operational risk and procurement constraint. The autonomous weapons industry is an excluded targetâits exclusion is the enforcement object itself. The engine computes per-seat directionality from these structural declarations and exit options; no override is needed because the beneficiary-victim structure is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure coordination (rope) by naming the asymmetric extraction that ICRC centrality and military operational cost represent. It also prevents mislabeling it as pure extraction (snare) by preserving the genuine coordination function: civilian populations do receive a protective legal structure that would vanish if the constraint disappeared. If the founding problemâunaccountable machine killingâis ever solved by technical means (explainable AI, robust accountability chains) yet the human-agency requirement persists unchanged, the T17 and theater-ratio signals would push reclassification toward piton or snare, flagging mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_equivalence_threshold,
    'Can autonomous systems achieve functional equivalence to human distinction and proportionality assessment, rendering the irreducible human judgment requirement technically obsolete?',
    'Empirical demonstration of LAWS performance in realistic combat scenarios with civilian casualty and collateral damage rates at or below human-operated baselines, subject to independent verification and adversarial red-teaming.',
    'If functional equivalence is proven, the coordination function (civilian protection) can be decoupled from the human-agency requirement, collapsing the constraint toward pure extraction (snare) or institutional inertia (piton). If impossible, the coordination function remains load-bearing and the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_equivalence_threshold, empirical, 'Whether machine performance parity dissolves the need for human judgment.').

omega_variable(
    icrc_agency_rent,
    'Does the human agency requirement primarily serve civilian protection, or does it primarily serve to maintain the interpretive centrality of IHL institutions against technocratic displacement?',
    'Comparative analysis of institutional funding flows, CCW agenda-setting access, citation networks, and career-path dependence in humanitarian law academia under human-agency-dominant versus alternative regulatory scenarios.',
    'If centrality maintenance dominates, the beneficiary structure is more extractive than coordinating and the theater ratio is higher than surface appearance suggests. If protection dominates, the extraction is the necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icrc_agency_rent, conceptual, 'Whether institutional centrality or civilian protection is the dominant beneficiary logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_dphar_tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ihl_dphar_tr_t5, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ihl_dphar_tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ihl_dphar_tr_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ihl_dphar_tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(ihl_dphar_tr_t25, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement(ihl_dphar_tr_t30, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ihl_dphar_be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ihl_dphar_be_t5, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ihl_dphar_be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(ihl_dphar_be_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(ihl_dphar_be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(ihl_dphar_be_t25, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(ihl_dphar_be_t30, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ihl_dphar_su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ihl_dphar_su_t5, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ihl_dphar_su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ihl_dphar_su_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ihl_dphar_su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ihl_dphar_su_t25, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 25, 0.715).
narrative_ontology:measurement(ihl_dphar_su_t30, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ihl_distinction_proportionality kernel. The human_agency_reading instantiates a legal-interpretive claim that irreducible human judgment is required under existing IHL. Its epsilon, beneficiary/victim structure, and normative foundation differ from the outcomes_based_reading (which permits machine performance parity) and the categorical_prohibition_reading (which bans autonomous weapons regardless of performance). Decomposition follows the epsilon-invariance principle: these are structurally distinct claims with different failure modes, empirical status, and authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
