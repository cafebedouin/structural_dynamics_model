% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary (Contingent Technological Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel
 *   'total_war_reachability_boundary': the CONTINGENT_REACHABILITY reading.
 *   This reading asserts that total war reachability — the question of
 *   whether a large-scale conflict could be fought and won — is fundamentally
 *   dependent on current technological equilibrium and could reverse if that
 *   equilibrium shifts. Under this reading, the apparent post-1990
 *   contraction in reachability (the sense that total war has become
 *   unwinnable) is best understood not as a permanent structural change but
 *   as a historically contingent atrophying of capability that current
 *   strategic competition aims to restore. The reading is contested by two
 *   sibling readings: the CONTRACTION reading (asserting reachability has
 *   permanently contracted to zero) and the DROPPING reading (asserting
 *   reachability remains non-zero but its probability has fallen to
 *   negligible levels). These three readings share a common kernel — the
 *   empirical and strategic status of total war — but interpret it
 *   differently. This story generates the CONTINGENT_REACHABILITY reading in
 *   isolation, with structural data and metrics consistent with that reading
 *   alone. Sibling readings are other constraint stories; they are NOT part
 *   of this file. The contest between readings is recorded in
 *   cs_structure.reading_relations and in omega variables that document the
 *   irreducible ambiguities.
 *
 * KEY AGENTS:
 *   - States developing destabilizing technologies (strategic competitor): high power, arbitrage exit — benefit from maintaining reachability-as-contingent interpretation
 *   - Strategic planners maintaining uncertainty (institutional seat): institutional power, constrained exit — invested in the ambiguity
 *   - Civilian populations under deterrence (payer): powerless, trapped — bear the risk if the reading is wrong
 *   - Non-nuclear states (payer): moderate power, constrained exit — collateral risk-bearers
 *   - Arms control advocates (excluded): organized, constrained exit — would argue for structural contraction and irreversible locking-in
 *   - Defense technologists (observer/beneficiary): organized, mobile — profit from boundary research under the contingency frame
 *   - Deterrence theorists and analysts (observer): moderate power, mobile — frame-setters whose scholarly claims shape strategic belief
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.71).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary (Contingent Technological Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '18e5d783-2fc0-4b4b-9e93-049a5673ccf0').
narrative_ontology:cs_kernel_codification('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', distributed).
narrative_ontology:cs_authority_grounding('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', extraction).
narrative_ontology:cs_interpretation_layer_present('18e5d783-2fc0-4b4b-9e93-049a5673ccf0').
narrative_ontology:cs_reading_relation('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', foundational, reachability_technologically_reversible).
narrative_ontology:cs_axiom_status(reachability_technologically_reversible, holdable).
narrative_ontology:cs_axiom_grounding('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', reachability_technologically_reversible, empirically_contingent).
narrative_ontology:cs_axiom('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', secondary, strategic_optionality_requires_boundary_uncertainty).
narrative_ontology:cs_axiom_status(strategic_optionality_requires_boundary_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', strategic_optionality_requires_boundary_uncertainty, instrumental).
narrative_ontology:cs_reference_frame('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', open_reachability_equilibrium).
narrative_ontology:cs_drift_state('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18e5d783-2fc0-4b4b-9e93-049a5673ccf0', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_developing_destabilizing_technologies).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, strategic_planners_maintaining_uncertainty).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_deterrence).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, defense_technologists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest in weapons systems (hypersonics, AI-guided delivery, space-based platforms) designed to degrade adversary detection and response capacity. Benefit from the persistent ambiguity about whether total war reachability has actually contracted or merely shifted in technical form. Their strategic interest lies in maintaining the possibility that reachability could expand — that today's contraction is reversible and that technological surprise could restore winnable scenarios. They frame the reachability boundary as contingent on current equilibrium, not as a structural plateau.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_developing_destabilizing_technologies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, states_developing_destabilizing_technologies, agenda_setter).

% Plan doctrine, posture, and force structure around the assumption that total war reachability remains contingent — that it could become winnable or unwinnnable depending on the next technological cycle. Benefit from the absence of certainty that reachability has permanently contracted. Invest in capabilities to explore and exploit the boundary's movement. Their interest is in perpetuating the interpretation that the boundary is technological-contingent rather than structural-permanent.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_planners_maintaining_uncertainty, beneficiary,
    institutional, biographical, constrained, global).

% Live under the assumption that total war reachability has been sufficiently contracted to prevent escalation in a major power crisis. Carry the cost of deterrence failure if the interpretation is wrong — if technological change or strategic miscalculation restores winnable war to the feasible set. They cannot exit the jurisdiction or the constraint's scope; they depend on the reading being correct.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_under_deterrence, payer,
    powerless, biographical, trapped, global).

% Operate under the security architecture that assumes total war reachability among nuclear powers has contracted. If the reading is wrong — if reachability returns — they have no capability to shape the outcome and bear the collateral costs of the resulting conflict or deterrence breakdown.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Argue that reachability contraction is structural and permanent, achieved through nonproliferation, test bans, and transparency regimes, and that it should be locked in place by irreversible disarmament or verification frameworks. They are excluded from the strategic-planning conversation insofar as their voice would argue for reading the boundary as fixed, not contingent — a reading that threatens the destabilizing-technology beneficiaries' investment thesis.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% Research and develop capabilities that operate on the contested reachability boundary — precision strike, autonomous systems, space-based platforms, cyber-enabled force. Benefit from the framing of reachability as contingent and reversible, which creates demand for boundary-shifting research. Positioned to observe the strategic contest and to benefit from whichever reading wins institutional backing.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, defense_technologists, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, defense_technologists, observer).

% Analyze and debate whether reachability is contracted, contingent, or stable. Position themselves as neutral analysts but their academic and policy influence shape which reading gets institutionalized. Some emphasize technological contingency; others emphasize structural contraction. Their role is observational but their framing influences state strategies.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, deterrence_theorists_and_security_analysts, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared strategic assumption — that escalation to total war is sufficiently constrained by technical and strategic conditions to make deterrence a viable equilibrium — by anchoring that assumption in a claim about technological contingency. Coordinates the behavior of all actors by keeping reachability interpretation open rather than settled, which permits continued strategic competition on the boundary rather than resignation to a fixed constraint.
% TRANSFER_FUNCTION: Transfers the burden of deterrence failure (civilization-ending risk) from strategic planners and capability developers to civilian populations globally. Also transfers legitimacy and research funding to states that maintain boundary-exploration programs. Moves the question of war/peace from the civilian sphere into the expert-analyst and strategic-planning sphere, concentrating decision authority in institutional hands.
% ABSENT_VOICES: Arms control advocates, disarmament movements, and the academic peace studies community are largely excluded from the strategic-planning conversation in which this reading is operative. They would argue that reachability should be read as structurally contracted and permanently locked in place through irreversible arms control and verification regimes — a reading that directly contradicts the contingency reading because it denies the premise that reachability could reverse. Their absence from the planning conversation means the risk assessment reflects only those who benefit from keeping the question open.
% DISAPPEARANCE_RATIONALE: From the contingent reading's own frame: if the boundary technologically resolved (reachability became definitively known to be permanently zero or permanently non-zero), the strategic competition on the boundary would end and the constraint would dissolve or transform into a mountain. But this resolution would require achieving empirical certainty the reading treats as impossible. From the contraction reading's frame: the constraint has already effectively disappeared — reachability is already zero, only institutional confusion prevents recognition. From a practical standpoint: disappearance depends on whether a major technological or strategic shock breaks the identity-lock binding institutional elites to the contingency frame.
% FOUNDING_PROBLEM: After nuclear weapons demonstrated civilization-ending destructive capacity in 1945, strategic thinkers confronted an irreducible uncertainty: has this destructive threshold made total war unwinnable and thus impossible to plan for? Or does the prospect of winning (even at catastrophic cost) remain feasible, meaning deterrence is the only bulwark against conflict? The reachability boundary is the set of technical and strategic conditions that would determine the answer. The founding problem is: can we know where that boundary is, and can we stabilize it?
% FOUNDING_PROBLEM_CORROBORATION: Defense strategists and military technologists (beneficiary seats) attest the founding problem is unresolved and possibly unresolvable — empirical evidence cannot test total-war winnability without running the war. Arms control analysts and peace researchers attest the founding problem is resolved: reachability has structurally contracted and should be locked in place. No external authoritative source exists — the disagreement is among the parties to the constraint, with beneficiaries on one side and victims/advocates on the other. No corroboration from outside the strategic establishment can be cited because the constraint lives entirely within state-level strategic discourse; international institutions, civil society, and non-specialist observers have no technical standing in the debate.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Under the contingent-reachability reading, the constraint is a SCAFFOLD: a temporary arrangement whose justification is the transition it enables, not the steady-state it sustains. The beneficiaries (states investing in destabilizing technology) collect rents and maintain strategic optionality by keeping reachability framed as contingent and reversible. The victims (civilian populations and non-nuclear states) bear the risk that deterrence will fail if the reading is wrong. Extractiveness is moderate-high (0.58) because the reading extracts legitimacy and resources for capability development from populations who rely on deterrence. It is not pure extraction (snare level) because there is a genuine coordination function — the reachability boundary does constrain state behavior and does substitute for direct conflict in many cases. Theater is high (0.62) because a significant share of strategic activity is devoted to maintaining the appearance of boundary-movement and contingency rather than substantively shifting the reachability boundary itself. As of 2015-2026, the theater ratio stabilizes: hypersonic research, AI-guided systems, and space-based platforms dominate strategic discourse, but actual reachability may not have shifted materially. Suppression is high (0.71) because the reading requires active suppression of the contraction-reading's voice in strategic discourse — arms control advocates and disarmament movements are largely excluded from the planning conversation. Accessibility collapse is low (0.48) because the reading leaves open the possibility of different technological futures and different strategic outcomes — the boundary is not presented as fixed, so alternatives (disarmament, verification regimes, arms control) remain theoretically available, even if they are suppressed in practice. Resistance is high (0.74) from the arms control and disarmament communities, which mount continuous argument for the contraction reading. The measurement series show a steady rise in theater_ratio from 1945 to 2015, indicating a drift from strategic competition on the actual boundary to performative boundary-maintenance. Extractiveness rises steeply 1945-2001 (Cold War and post-Cold War destabilization) then plateaus 2001-2026, suggesting the constraint has matured into stable theater. Suppression rises throughout, reflecting increasing institutional investment in excluding alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute fundamentally different constraint types from this structural data. From the beneficiary perspective (states investing in destabilizing tech): this is a genuine coordination problem (how do we maintain strategic optionality in a nuclear-armed world?) that the contingency frame solves. From the payer perspective (civilians): this is extraction masked as coordination — the benefits accrue to strategic elites and the risks accrue to populations. The engine computes this per-seat divergence from the structural data (beneficiary vs. victim declarations, exit options, power levels). The claimed type (scaffold) asserts the reading is transitive — it should end when technological equilibrium resolves. But the analysis permits that the reading could calcify into a piton (perpetual theater with no functional purpose) or transform into a snare if the beneficiaries succeed in permanently suppressing the contraction reading.
 *
 * DIRECTIONALITY LOGIC:
 *   States developing destabilizing technologies and strategic planners have low directionality (d toward beneficiary end): they collects legitimacy, funding, and strategic optionality from the contingency frame. The reading itself is their authored interpretation. Civilian populations and non-nuclear states have high directionality (d toward target end): they pay in the form of deterrence-failure risk under a reading they did not author and cannot easily exit. Arms control advocates are excluded rather than directly targeted, but if they were in the conversation their directionality would be high (opposite of the beneficiaries). Defense technologists sit near the beneficiary end because their institutional interests align with boundary research under contingency, even though they present themselves as neutral observers. Deterrence theorists are near-symmetric: they benefit from framing-authority but pay costs if the reading proves wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the decay of a constraint's founding mandate — is central to this reading. The contingent-reachability reading was founded to answer: how do we know whether total war is still winnable? Once nuclear weapons reached mutual-destruction capacity, this question became the organizing principle of strategic discourse. By 2015, the mandate had substantially decayed: no new empirical evidence has resolved the question; strategic technology has proliferated and complexity has risen, but the boundary itself remains unmeasured and unmeasurable. The theater_ratio rise from 0.08 to 0.62 indicates that strategic activity has increasingly shifted from actually exploring the boundary to performing the boundary's contingency. The constraint persists through institutional inertia (defense establishments invested in the research agenda) and through the beneficiaries' interest in keeping the question open. A mandatrophy reading would flag this as a piton (atrophied function, persistent theater). The claimed type (scaffold) asserts it is still transitive, still pointing toward resolution. The metrics suggest the transition is stalled. An omega variable addresses whether the scaffold has already decayed into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingency_vs_contraction_empirically_resolvable,
    'Is the reachability boundary genuinely contingent on technological change, or has it structurally contracted to zero? What empirical evidence could resolve this question?',
    'Direct empirical measurement is impossible (cannot run total-war scenarios). Proxy evidence: (1) analysis of new weapons systems'' technical specs against strategic models of winnability; (2) red-team exercises and war gaming; (3) reverse-engineering from stated doctrine whether planners actually believe reachability is contingent; (4) adversary decision-making in crises (do they escalate escalation as if reachability is possible?). No single source is dispositive.',
    'If empirically resolvable: the contingent reading transitions to snare (beneficiaries maintain it despite evidence against, for institutional reasons). If empirically unresolvable: the reading persists as a genuine irreducible uncertainty, and the constraint remains a scaffold. If resolved toward contraction: mandatrophy is confirmed and the constraint should be reclassified as piton or dissolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingency_vs_contraction_empirically_resolvable, empirical, 'Whether the contingency is a real feature of the strategic landscape or a cover story maintained by institutional beneficiaries.').

omega_variable(
    mandatrophy_stage_assessment,
    'Has the contingent-reachability scaffold already decayed into a piton? Is the research agenda still transitioning toward resolution, or has it become pure institutional theater with no functional purpose?',
    'Track the theater_ratio trend and the content of strategic discourse. If theater_ratio continues rising (approaching 1.0) while measurable boundary-movement stalls, mandatrophy is advancing. Compare strategic research investment to actual boundary-clarification outcomes. Interview strategic planners about whether they expect the contingency question to be resolved in their careers.',
    'If mandatrophy is substantial (theater_ratio > 0.70, boundary unchanged for >15 years): reclassify to piton. If mandatrophy is minimal (research agenda still advancing, theater < 0.50): maintain scaffold classification. If intermediate: increase omega confidence in the piton-drift assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_stage_assessment, empirical, 'Whether the contingency frame has become a zombie constraint maintained by institutional inertia rather than a genuine transitional equilibrium.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of the contraction and arms-control readings primarily structural (legal/institutional barriers, access denial, funding gatekeeping) or internalized (the strategic establishment has absorbed the contingency frame as the ''only serious'' position)?',
    'Counterfactual analysis: if institutional barriers to alternative readings fell (funding opened, platforms appeared, legal constraints lifted), would the contraction reading rapidly gain adoption? Or has it been so thoroughly delegitimized that even barriers-down would not restore it? Interview arms-control advocates and disarmament researchers about what would need to change to make their voice heard.',
    'If structural: removing barriers could rapidly shift the discourse and reclassify the constraint. If internalized: the suppression is self-perpetuating and harder to break — the reading would persist as snare rather than scaffold even if formal barriers fell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the suppression of alternative readings is barrier-enforced or belief-enforced, which determines whether institutional change alone could shift the constraint''s status.').

omega_variable(
    technological_reversibility_assumption,
    'What is the core unstated assumption separating the contingent reading from the contraction reading? Is it empirical (reachability CAN technologically reverse) or normative (reachability SHOULD be kept contingent for strategic optionality)?',
    'Examine the cited technical claims: do strategic planners argue that hypersonics, AI, space-based systems CAN materially alter reachability (empirical), or that keeping the boundary open is strategically desirable (normative)? If empirical: look for falsifiable predictions from the hypothesis. If normative: the disagreement is about what states should do, not what is technologically possible.',
    'If empirical: the contingent reading is falsifiable by technical advances. If normative: the reading is preference-indexed and reclassifies as snare (interests disguised as facts). If hybrid: the reading is partially falsifiable — resolve the empirical part, leaving the normative disagreement exposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_reversibility_assumption, conceptual, 'Whether the core disagreement is about what is technically possible or about what strategic choice is justified.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel ''total war reachability'' framed as a single empirical question (winnable or not?) or as three distinct questions that happen to be linguistically bundled (contraction vs. contingency vs. probability-dropping are structurally different claims)?',
    'Map the logical dependencies: contraction (reachability=0) forecloses contingency and dropping. Contingency (reachability is reversible) is compatible with either dropping (probability low) or rising (probability high). Dropping (probability ~0 but reachability nonzero) is compatible with contingency or with permanent structural limitation. The three readings are not just different answers to one question; they answer different questions under different logical frames.',
    'If the kernel is genuinely triple-structured: the three readings do not compete as alternatives — they operate in different frames and should be understood as family members (affects_constraints links) not as opposed interpretations. If the kernel is unified: one reading must eventually dominate. This changes how the constraint corpus organizes these stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel contest is about competing answers to one question or about whether the question itself is well-formed.').

omega_variable(
    identity_lock_brittleness,
    'How firmly is the contingent-reachability reading locked in place by the professional identity of defense strategists and technologists? Would a major institutional shock (loss of funding, generational turnover, public delegitimization) cause rapid reading-shift, or is the identity lock deep enough to survive such shocks?',
    'Historical precedent: track shifts in official doctrine and strategic consensus over major breaks (Cold War end, 9/11, nuclear crises, arms control advances). Assess the turnover rate of senior strategists and institutional gatekeepers. Interview mid-career researchers about whether they could plausibly shift to a different reading without career cost.',
    'If brittleness is high: institutional shocks could rapidly reverse the reading and reclassify the constraint. If brittleness is low: the reading persists even through institutional change, suggesting it has calcified into institutional identity rather than provisional strategic judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_brittleness, empirical, 'Whether the identity-lock binding strategic elites to the contingency frame could break under institutional pressure, or whether it is self-perpetuating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1979, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1979, 0.28).
narrative_ontology:measurement(tota_tr_t2001, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2001, 0.51).
narrative_ontology:measurement(tota_tr_t2015, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2015, 0.62).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2026, 0.62).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(tota_be_t1979, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1979, 0.42).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(tota_be_t2015, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1962, 0.48).
narrative_ontology:measurement(tota_su_t1979, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1979, 0.58).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(tota_su_t2015, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.25).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% The kernel 'total_war_reachability_boundary' decomposes into three structurally distinct constraint stories, each instantiating a different reading. The CONTINGENT_REACHABILITY reading (this file) asserts reachability is technology-dependent and reversible — a scaffold with beneficiaries investing in destabilizing tech and victims bearing deterrence-failure risk. The CONTRACTION reading asserts reachability has permanently contracted below winning thresholds — a mountain (natural law of nuclear weapons). The DROPPING reading asserts reachability remains non-zero but its probability has fallen to near-zero — a rope (coordination equilibrium). The three readings are not observable-dependent variants of a single constraint; they make fundamentally different structural claims about the same kernel. They are linked by affects_constraints because adoption of one reading directly shapes the credibility and resource environment for the others. The three stories form a kernel family; each must be read with awareness that the others exist and that the choice between readings is not purely empirical.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
