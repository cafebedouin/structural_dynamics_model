% ============================================================================
% CONSTRAINT STORY: decision_quality_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decision_quality_degradation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: decision_quality_degradation
 *   human_readable: Decision Quality Degradation in Complex Systems
 *   domain: organizational_governance/information_asymmetry
 *
 * SUMMARY:
 *   Decision quality degradation is a constraint that emerges when
 *   information asymmetries, organizational hierarchy, and incentive
 *   misalignment interact to produce systematic gaps between the quality of
 *   decisions made and the quality that would result from full stakeholder
 *   access to relevant information. The constraint operates across public
 *   institutions, corporations, non-profits, and international organizations.
 *   Gatekeepers (executives, elected officials, technical experts) occupy
 *   positions that grant them control over what information stakeholders
 *   receive, how it is framed, and which decision criteria are privileged.
 *   This structural position generates incentives to suppress, filter, or
 *   reframe information in ways that sustain gatekeeper authority, even when
 *   such filtering reduces decision quality for the larger system. The
 *   constraint exhibits elements of all six DR types: pure extraction (snare)
 *   from the perspective of stakeholders with no exit; coordination and
 *   extraction (tangled rope) from analysts whose roles are partially
 *   constituted through the degraded system; coordination (rope) from
 *   gatekeepers whose authority depends on information control; a temporary
 *   architecture with sunset pathways (scaffold) from decentralization
 *   advocates; degraded ritual (piton) in formal procedures that appear to
 *   ensure quality but operate downstream of information suppression; and
 *   false natural law (mountain) in the analytical view that information
 *   asymmetry is inevitable at scale.
 *
 * KEY AGENTS:
 *   - Decision Gatekeepers: Primary beneficiaries (institutional/arbitrage) — capture authority, reputation, and control through concentrated information access; can exit or restructure rules at low cost
 *   - Stakeholders Bearing Consequences: Primary victims (powerless/trapped) — lack access to decision-relevant information; cannot exit decisions that affect them; bear implementation costs of poor choices
 *   - Organizational Analysts: Secondary actors (moderate/constrained) — constrained by information access and hierarchy; benefit from appearing neutral and valuable; bear costs when degraded decisions create downstream problems
 *   - Decentralization Movement: Organized agents (organized/mobile) — transparency advocates, open-data initiatives, participatory governance models building alternative decision pathways
 *   - Bureaucratic Process: Institutional mechanism (institutional/arbitrage) — formal procedures maintain appearance of quality control but operate downstream of information suppression; persist through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as immutable consequences of scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decision_quality_degradation, 0.58).
domain_priors:suppression_score(decision_quality_degradation, 0.62).
domain_priors:theater_ratio(decision_quality_degradation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decision_quality_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(decision_quality_degradation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(decision_quality_degradation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decision_quality_degradation, tangled_rope).
narrative_ontology:human_readable(decision_quality_degradation, "Decision Quality Degradation in Complex Systems").
narrative_ontology:topic_domain(decision_quality_degradation, "organizational_governance/information_asymmetry").

domain_priors:requires_active_enforcement(decision_quality_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decision_quality_degradation, decision_gatekeepers).
narrative_ontology:constraint_beneficiary(decision_quality_degradation, information_intermediaries).
narrative_ontology:constraint_victim(decision_quality_degradation, stakeholders_bearing_consequences).
narrative_ontology:constraint_victim(decision_quality_degradation, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STAKEHOLDER BEARING CONSEQUENCES (SNARE) — Lacks access to decision-relevant information; cannot exit or influence decisions that affect them. Bears full cost of degraded decisions through implementation consequences. No alternative mechanism to challenge or correct degradation.
constraint_indexing:constraint_classification(decision_quality_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZATIONAL ANALYST (TANGLED ROPE) — Constrained by information access, career incentives, and institutional hierarchy. Benefits from the degraded system insofar as analysis appears neutral and valued; bears costs when poor decisions create downstream problems affecting their domain. Mixed extraction and coordination.
constraint_indexing:constraint_classification(decision_quality_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DECISION GATEKEEPER (ROPE) — Controls information flow and framing. Benefits from concentrated authority; experiences constraint as coordination mechanism enabling centralised control. Low effective extraction because this agent has both power and mobility — they can leave the system, change its rules, or delegate differently.
constraint_indexing:constraint_classification(decision_quality_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION MOVEMENT (SCAFFOLD) — Organized actors (transparency advocates, open-data initiatives, participatory governance models) see decision quality degradation as a temporary failure of centralized architecture with a sunset: distributed decision-making, transparent criteria, and stakeholder input channels offer alternative pathways. Sunset clause inherent in the logic — as decentralization matures, the gatekeeper's control mechanism loses force.
constraint_indexing:constraint_classification(decision_quality_degradation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BUREAUCRATIC PROCESS (PITON) — Formal decision-making procedures (committee reviews, approval workflows, documentation requirements) persist through institutional inertia. Theater ratio (0.64) reflects that much of the process is performative: procedures appear to ensure quality but information degradation is already embedded upstream. Process persists because alternatives have not fully replaced it, not because it functions effectively.
constraint_indexing:constraint_classification(decision_quality_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information degradation in complex systems may appear immutable: organizational size, complexity, and bounded rationality inherently limit decision quality. However, this perspective naturalizes what is actually a contingent institutional arrangement — choice of information architecture, incentive structures, and governance processes shape whether degradation accelerates or is arrested.
constraint_indexing:constraint_classification(decision_quality_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decision_quality_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decision_quality_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decision_quality_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decision_quality_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decision_quality_degradation, TR),
    TR >= 0.70.

:- end_tests(decision_quality_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts significant value for gatekeepers through sustained authority and reduced accountability. The extraction is measurable in decision outcomes that favor gatekeeper preferences over stakeholder interests, in reduced transparency of decision rationale, and in suppressed feedback mechanisms. However, extraction is not maximal because some information does flow (reducing from 0.70+ snare range), and some organizations have introduced transparency mechanisms that partially arrest degradation. The measured value reflects steady increase over the observation interval (0.32 → 0.58), indicating that as organizational complexity grows, the extraction mechanism becomes more effective. Suppression (0.62): High. Barriers to exit from the constraint are substantial: stakeholders cannot easily obtain decision-relevant information, cannot participate in decision-making, and face career or practical costs if they attempt to circumvent gatekeeping structures. However, suppression is not absolute (otherwise it would be mountain-level, ≥ 0.85) — some information flows, some stakeholders have partial access, and some organizations have implemented disclosure requirements. Theater ratio (0.64): Moderately high. Formal decision procedures (committee reviews, regulatory approvals, stakeholder consultations) create appearance of quality oversight, but much of the process occurs after information has already been filtered or reframed by gatekeepers. The theater increases as organizations grow and add more procedures without addressing the upstream information asymmetry. Claimed type (tangled_rope): Justified. The constraint exhibits both genuine coordination function (information centralization enables faster decisions, reduces redundancy) and asymmetric extraction (gatekeepers benefit disproportionately, stakeholders bear costs). Both features are structurally necessary — it is not a snare masquerading as rope, but a hybrid mechanism where coordination and extraction are genuinely entangled.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence from identical base metrics. The gatekeeper sees rope (coordination enabling efficient decisions) while the stakeholder sees snare (pure extraction with no exit). The analyst sees tangled rope (mixed coordination and extraction) because their position is structurally intermediate — they benefit from appearing neutral but depend on information access gatekeepers control. The decentralization advocate sees scaffold because they have agency and a structural exit path (alternative governance models that are becoming viable). The bureaucrat sees piton (ritual persisting through inertia despite low function). The analytical observer risks false mountain (naturalizing hierarchy as inherent to complexity). The perspectival gap reveals that 'decision quality degradation' is not a single phenomenon viewed from different angles — it is a different constraint for each positioned agent. The shared base metrics (extractiveness, suppression, theater) allow the engine to measure these gap widths and detect when gatekeeper rhetoric (rope) and stakeholder experience (snare) are describing fundamentally different structural realities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's relationship to information flow and decision authority. Gatekeepers with arbitrage exit options (can exit role, restructure authority, delegate) experience low d (~0.15) — they are beneficiaries of the constraint and have structural mobility. Stakeholders with trapped status (cannot exit decisions affecting them, cannot access information) experience high d (~0.95) — they are targets of extraction with no alternatives. Analysts with constrained exit (can exit role but face career costs, have partial information access) experience moderate d (~0.65). Decentralization advocates with mobile exit and organized power experience lower d (~0.45) because they have agency and alternatives. The engine applies the sigmoid f(d) to convert d to effective power modifier — high d values produce high f(d) values that amplify experienced extraction for trapped agents; low d values produce negative f(d) values that reduce effective extraction for beneficiaries. This arithmetic is not repeated here (per v1.2 guidance) but is the mechanism by which structurally similar extractiveness (0.58) produces divergent χ values across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint resolves potential mandatrophy (confusion between coordination and extraction) by clearly declaring structural functions: information centralization IS a genuine coordination mechanism (it enables faster decisions, reduces redundancy, allows expert judgment); simultaneously, the authority concentration that enables this coordination IS being extracted by gatekeepers for their own benefit. Both are structurally real. The constraint is neither 'really' rope (coordination) nor 'really' snare (extraction) — it is tangled_rope where coordination and extraction are genuinely entangled. The perspectival gap (gatekeeper sees rope, stakeholder sees snare) is not a measurement problem — it is a real feature of the constraint's structure. The analytical observer's mountain classification is a false summit: the framing 'information asymmetry is inherent to scale' naturalizes what is actually a choice of governance architecture. Organizations at similar scale with different information policies show measurably different decision quality, proving the mountain is not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_versus_systemic_degradation,
    'Is decision quality degradation primarily driven by deliberate suppression of information by gatekeepers, or by systemic features (cognitive limits, organizational complexity, information overload)?',
    'Longitudinal analysis of information availability vs. decision quality correlation; comparison of organizations with different governance transparency levels; measurement of gatekeeper incentive alignment with stakeholder outcomes',
    'If deliberate: snare classification dominates (pure extraction). If systemic: tangled_rope or scaffold more accurate (mixed coordination and extraction). Changes remediation strategy fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_versus_systemic_degradation, empirical, 'Whether degradation stems from deliberate suppression or systemic complexity').

omega_variable(
    decentralization_viability_threshold,
    'At what organizational scale or complexity level does decentralized decision-making become infeasible, making centralization with quality degradation inevitable?',
    'Cross-organizational study of decision quality vs. decentralization level at different scales; identification of coordination cost threshold where centralization becomes necessary',
    'If threshold exists at organization''s current scale: scaffold sunset is theoretical (cannot be achieved). If threshold higher: decentralization is genuinely possible and sunset is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_viability_threshold, empirical, 'Feasibility threshold for decentralized decision-making at scale').

omega_variable(
    procedural_theater_measurement,
    'What proportion of formal decision procedures actually improve decision quality versus merely creating legitimacy and accountability appearance?',
    'Randomized comparison of decisions made with vs. without formal procedure oversight; correlation between procedure adherence and outcome quality; post-decision analysis of whether procedure prevented or merely documented poor choices',
    'If procedures improve quality: theater_ratio should be lower (< 0.50), constraint is more rope than tangled_rope. If primarily theatrical: theater_ratio justified, piton classification accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_theater_measurement, empirical, 'Proportion of decision procedures that improve quality versus create appearance').

omega_variable(
    information_asymmetry_persistence,
    'Does stakeholder access to decision-relevant information increase when transparency mechanisms are implemented, or do gatekeepers find alternative suppression methods?',
    'Before-after analysis of transparency initiatives; tracking of information flow and stakeholder comprehension post-implementation; identification of displacement effects where suppression moves from information availability to interpretation/framing',
    'If access increases: suppression metric overstated, constraint is less severe than measured. If displacement occurs: suppression persists through alternative mechanisms, constraint is more entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether transparency actually increases stakeholder access or enables suppression displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decision_quality_degradation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dqd_tr_t0, decision_quality_degradation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dqd_tr_t5, decision_quality_degradation, theater_ratio, 5, 0.51).
narrative_ontology:measurement(dqd_tr_t10, decision_quality_degradation, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(dqd_be_t0, decision_quality_degradation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dqd_be_t5, decision_quality_degradation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dqd_be_t10, decision_quality_degradation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decision_quality_degradation, enforcement_mechanism).
narrative_ontology:affects_constraint(decision_quality_degradation, organizational_accountability_erosion).
narrative_ontology:affects_constraint(decision_quality_degradation, stakeholder_epistemic_exclusion).

% DUAL FORMULATION NOTE:
% Decision quality degradation is upstream of specific accountability failures and epistemic exclusions in organizations. It represents a distinct structural constraint on information flows and decision authority that affects downstream mechanisms. The three constraints form a causal family: degradation in decision quality → erosion of accountability mechanisms → systematic exclusion of stakeholders from epistemic participation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
