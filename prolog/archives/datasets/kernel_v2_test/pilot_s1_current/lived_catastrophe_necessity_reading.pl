% ============================================================================
% CONSTRAINT STORY: lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lived_catastrophe_necessity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lived_catastrophe_necessity_reading
 *   human_readable: Lived Catastrophe Necessity: Competence Requires Real-Stakes Activation
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'exercise as competence maintenance': the claim that only lived
 *   catastrophe (or high-stakes activation under real operational conditions)
 *   genuinely exercises the competence required to manage complex, dangerous
 *   systems. Simulation is presented as necessary but fundamentally
 *   insufficient — competence atrophies in ways undetectable by simulation
 *   because simulation lacks the irreducible pressure, consequence, and
 *   unpredictability that separate rehearsal from the thing itself. This
 *   reading vindicates a stakes-asymmetry doctrine: operators trained only
 *   through simulation carry hidden competence deficits that will surface
 *   when real consequences materialize. The constraint operates through
 *   suppression of alternative verification methods (organizations adopt
 *   simulation, regulatory capture institutionalizes
 *   simulation-as-sufficient, questioning the framework becomes career-risky)
 *   and through extraction (doctrine advocates, crisis readiness consultants,
 *   and high-consequence verification frameworks collect authority and
 *   resource allocation). The structural data — rising extractiveness (0.45 →
 *   0.65) and rising theater ratio (0.52 → 0.68) over 20 years — reflects
 *   increasing reliance on simulation protocols despite accumulating cases
 *   where simulation-trained operators failed under real conditions, and
 *   increasing performativity of the regulatory apparatus maintaining the
 *   simulation framework.
 *
 * KEY AGENTS:
 *   - Exposed Populations: Powerless/trapped (global/biographical) — depend on operator competence trained only through simulation; cannot verify preparedness; bear full risk of competence gaps that surface in crises
 *   - Simulation-Dependent Organizations: Moderate/constrained (national/generational) — aviation, nuclear, hospital emergency systems; benefit from coordination (simulation allows competence maintenance without catastrophe) but extracted from by regulatory theater and hidden decay
 *   - Crisis Readiness Doctrine Advocates: Institutional/arbitrage (global/immediate) — regulators, safety consultants, doctrine-writers; benefit from vindication of the lived-catastrophe-necessity reading; their authority and expertise increase; see pure coordination benefit
 *   - Regulatory Compliance Theater: Institutional/constrained (national/biographical) — certification apparatus, audit procedures, documented exercises; maintains performative form despite atrophied verification function; piton classification
 *   - High-Consequence Stress-Testing Coalition: Organized/constrained (global/generational) — aviation safety boards, nuclear regulators, pandemic teams; building escalating stress-test protocols as an alternative pathway; represent the scaffold perspective with sunset logic
 *   - Analytical Observer: Analytical/analytical (universal/civilizational) — risks naturalizing institutional choice (simulation-only) as immutable fact of human cognition; vulnerable to false summit classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lived_catastrophe_necessity_reading, 0.65).
domain_priors:suppression_score(lived_catastrophe_necessity_reading, 0.72).
domain_priors:theater_ratio(lived_catastrophe_necessity_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lived_catastrophe_necessity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(lived_catastrophe_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lived_catastrophe_necessity_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lived_catastrophe_necessity_reading, snare).
narrative_ontology:human_readable(lived_catastrophe_necessity_reading, "Lived Catastrophe Necessity: Competence Requires Real-Stakes Activation").
narrative_ontology:topic_domain(lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lived_catastrophe_necessity_reading, '45f27e57-4322-4e9d-b64c-de71291297c3').
narrative_ontology:cs_kernel_codification('45f27e57-4322-4e9d-b64c-de71291297c3', formalized).
narrative_ontology:cs_authority_grounding('45f27e57-4322-4e9d-b64c-de71291297c3', extraction).
narrative_ontology:cs_interpretation_layer_present('45f27e57-4322-4e9d-b64c-de71291297c3').
narrative_ontology:cs_reading_relation('45f27e57-4322-4e9d-b64c-de71291297c3', lived_catastrophe_necessity_reading__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('45f27e57-4322-4e9d-b64c-de71291297c3', lived_catastrophe_necessity_reading__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('45f27e57-4322-4e9d-b64c-de71291297c3', foundational, irreducibility_of_real_stakes).
narrative_ontology:cs_axiom_status(irreducibility_of_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('45f27e57-4322-4e9d-b64c-de71291297c3', irreducibility_of_real_stakes, empirically_contingent).
narrative_ontology:cs_axiom('45f27e57-4322-4e9d-b64c-de71291297c3', foundational, covert_decay_indetectability).
narrative_ontology:cs_axiom_status(covert_decay_indetectability, holdable).
narrative_ontology:cs_axiom_grounding('45f27e57-4322-4e9d-b64c-de71291297c3', covert_decay_indetectability, empirically_contingent).
narrative_ontology:cs_reference_frame('45f27e57-4322-4e9d-b64c-de71291297c3', simulation_as_sufficient_competence_proxy).
narrative_ontology:cs_drift_state('45f27e57-4322-4e9d-b64c-de71291297c3', contemporary_complexity_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45f27e57-4322-4e9d-b64c-de71291297c3', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lived_catastrophe_necessity_reading, crisis_readiness_doctrine_advocates).
narrative_ontology:constraint_victim(lived_catastrophe_necessity_reading, simulation_dependent_organizations).
narrative_ontology:constraint_victim(lived_catastrophe_necessity_reading, exposed_populations).
narrative_ontology:constraint_victim(lived_catastrophe_necessity_reading, personnel_under_untested_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED POPULATIONS (SNARE) — Powerless agents who depend on the competence of operators trained only through simulation. Bear full risk if competence decay occurs between exercises. Cannot exit exposure or demand real-stakes verification. No mechanism to verify operator preparedness without triggering the catastrophe itself. Maximum experienced extraction: trapped in dependence on untested competence.
constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SIMULATION-DEPENDENT ORGANIZATIONS (TANGLED ROPE) — Nuclear plants, aviation systems, hospital emergency protocols. Benefit from simulation: coordination mechanism solves the problem of how to maintain competence without catastrophe. But also extracted from: forced into compliance with performative exercises; constrained by regulatory theater; competence decay hidden within passing simulations. Significant extraction but not maximal — the organizations have agency in designing simulations and have some causal control over actual outcomes.
constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CRISIS READINESS DOCTRINE ADVOCATES (ROPE) — Regulators, safety consultants, doctrine-writers who advocate for mandatory real-stakes testing (or at minimum, stress-testing to breaking points). Benefit from the constraint: their expertise is vindicated, their authority increases, their frameworks structure organizational learning. Experience the constraint as pure coordination: 'We must verify competence under conditions that matter.' Net beneficiary — the doctrine advocates collect authority and legitimacy from the constraint's operation.
constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COMPLIANCE THEATER (PITON) — The apparatus of certification, audit trails, tabletop exercises, documented procedures. Maintained through institutional inertia despite atrophied function. Serves as proof-of-compliance rather than actual competence verification. Theater ratio of 0.68 reflects that the majority of regulatory exercise time is spent on documentation, scenario credibility, and audit satisfaction rather than on testing operators to failure. The function (verifying competence) has degraded; the form (exercises, certifications, compliance checkboxes) persists.
constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From civilizational scale, some decay of competence without real-stakes activation appears natural: human skill degrades with disuse, memory fades, muscle memory atrophies. This perspective risks treating the constraint as an immutable fact of human cognition and physiology. But this naturalization obscures the institutional choice: the organization chose simulation as the competence mechanism; the choice to avoid real-stakes activation is not forced by nature but by organizational risk tolerance and liability concerns. The engine's false summit detector will identify this as a naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HIGH-CONSEQUENCE STRESS-TESTING COALITION (SCAFFOLD) — Organized actors (aviation safety boards, nuclear regulatory bodies, pandemic preparedness teams) building escalating stress-test protocols: simulations → tabletop escalations → live system stress tests → managed low-consequence activations. These are not full catastrophes but higher-stakes versions where competence is tested under pressure without accepting society-level harm. Sees the constraint as temporary and solvable: develop verification methods that exercise competence under real pressure but bounded consequence. Low effective extraction because the coalition has agency and sees an exit pathway (the stress-test ladder).
constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lived_catastrophe_necessity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lived_catastrophe_necessity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lived_catastrophe_necessity_reading, TR),
    TR >= 0.70.

:- end_tests(lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Moderately high, rising. The constraint extracts through multiple mechanisms: (1) organizations are forced into simulation-only compliance despite knowing decay occurs; (2) doctrine advocates collect legitimacy by vindicating stakes-asymmetry; (3) crisis readiness consultants gain market share; (4) exposed populations bear the asymmetric risk. The rise from 0.45 to 0.65 reflects that as operational systems become more complex (modern aviation, grid-dependent infrastructure), the gap between simulation capability and real-world demand widens, forcing deeper reliance on the framework despite degrading effectiveness. Suppression (0.72): High and rising. Suppression operates through: regulatory capture (simulation-sufficiency doctrine embedded in regulatory frameworks), career risk (questioning the framework threatens careers in safety management), and organizational inertia (simulation infrastructure is mature and expensive to replace). The rise from 0.58 to 0.72 reflects hardening of the regulatory consensus around simulation-sufficiency despite accumulating counterevidence. Theater ratio (0.68): High and rising. The majority of regulatory exercise time goes to documentation, scenario credibility assessment, and audit satisfaction rather than testing operators to failure. The rise from 0.52 to 0.68 reflects increasing emphasis on compliance proof rather than actual competence verification as regulatory apparatus becomes more formalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion. The beneficiary (crisis readiness doctrine advocates) sees a pure coordination mechanism (Rope) — they experience the constraint as the legitimate insight that competence requires stakes. The trapped victims (exposed populations) see pure extraction (Snare) — they depend on untested competence with no verification mechanism. The moderate actors (organizations) see a hybrid (Tangled Rope) — they benefit from avoiding real catastrophe through simulation but are extracted from by regulatory theater and hidden decay. The regulatory apparatus sees its own degradation (Piton) — the functional verification has atrophied; the compliance form persists. The organized coalition sees a solvable problem (Scaffold) — stress-testing protocols can exercise competence under pressure without accepting society-level harm. The analytical observer risks naturalizing contingent choice (Mountain) — treating simulation-only dependence as an inherent constraint of human learning rather than an institutional decision. The perspectival gap reveals that this reading is not about discovering an objective fact but about which institutional actors benefit from treating the constraint as natural law vs. contingent choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from agent position relative to the constraint. Exposed populations (powerless/trapped) face d ≈ 1.0 — full targets bearing all risk with no exit. Regulatory theater (institutional/constrained) faces d ≈ 0.3 — constrained but with some capacity to adapt within the institutional framework. Crisis readiness advocates (institutional/arbitrage) face d ≈ 0.0 — full beneficiaries who collect authority and legitimacy without bearing the catastrophe risk. Organizations (moderate/constrained) face d ≈ 0.5 — symmetric: they benefit from coordination (avoiding catastrophe through simulation) but are extracted from through regulatory theater and competence decay risk. The scaffold coalition (organized/constrained) faces d ≈ 0.4 — constrained by regulatory inertia but with agency to develop alternatives. Effective extraction (χ) is computed by the engine from d and other factors; beneficiaries at low d see negative χ (subsidy), victims at high d see high χ (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   READING RESOLUTION: This reading (lived catastrophe necessity) asserts that the founding mandate — to maintain operator competence safely through simulation without waiting for real catastrophe — has become a victim of its own success: the simulation framework became so institutionalized that questioning it became heretical, competence decay became invisible within passing simulations, and the original mandate (maintain competence) was displaced by a secondary mandate (maintain compliance with simulation protocols). The mandatrophy is not yet resolved because the organizational commitment to the simulation mandate persists even as evidence accumulates that competence decay occurs. Resolution would require acknowledging that simulation-only is insufficient and adopting the stress-testing pathway (scaffold reading) or accepting intermittent catastrophe (implicit cost) or developing new competence-verification methods. The reading also highlights that different actors have different stakes in mandatrophy resolution: doctrine advocates benefit from perpetuating the mandate; organizations want to resolve it quietly; exposed populations want visible resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Which reading of the exercise-as-competence-maintenance kernel is structurally true: that lived catastrophe is necessary (this reading), that simulation sufficiently exercises competence (sibling), or that decay and simulation exist in mutual equilibrium (sibling)?',
    'Historical correlation analysis: organizations relying solely on simulation vs. those conducting stress tests or managed activations; measurement of competence decay rates under each regime; post-event reviews comparing predicted vs. actual operator performance. Longitudinal organizational learning curves.',
    'If lived-catastrophe reading is true: current simulation-dependent systems carry hidden competence deficits that will surface in crises. If simulation-sufficiency reading is true: stress-testing programs are wasteful theater. If hybrid reading is true: organizations optimize by mixing simulation and escalating stress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, empirical, 'Which reading of exercise as competence maintenance is structurally true').

omega_variable(
    covert_competence_decay_observability,
    'Can competence decay be detected before catastrophe, or does it only become visible when stakes are real?',
    'Detailed operator performance tracking in simulation vs. high-fidelity analog exercises (e.g., simulator-to-cockpit transfer studies); post-event autopsies comparing pre-event exercise performance to actual crisis performance; early warning signal discovery in competence metrics.',
    'If detectable: simulation-dependent organizations can identify decay before catastrophe and remediate. The constraint becomes a coordination problem (Rope) rather than a snare. If undetectable: decay is covert and competence verification requires real stakes. The snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covert_competence_decay_observability, empirical, 'Whether competence decay is observable before real-stakes activation').

omega_variable(
    acceptable_risk_asymmetry,
    'Can low-consequence stress testing (aviation carrier landings, nuclear system shutdowns under controlled conditions, pandemic response drills with modified disease) achieve competence verification without accepting catastrophic risk?',
    'Controlled experiments comparing operator performance in escalating stakes regimes; measurement of transfer from stress-test to crisis contexts; identification of the minimum consequence level that exercises true competence vs. remaining simulation-like.',
    'If low-consequence stress testing works: the scaffold pathway succeeds and organizations can exit the ''simulation only'' or ''wait for catastrophe'' dilemma. If it fails: competence verification genuinely requires either simulation (with hidden decay) or catastrophe (with real cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_risk_asymmetry, empirical, 'Whether low-consequence stress testing can exercise genuine competence').

omega_variable(
    reading_foreclosure_test,
    'Does this reading (lived catastrophe necessity) logically foreclose the simulation-sufficiency reading, or do they coexist as live options chosen by different institutional actors?',
    'Logical analysis: if lived catastrophe is necessary, can an actor rationally choose simulation alone? Or is the choice between them a matter of risk tolerance and preference, not logical necessity? Examine whether organizations citing simulation-sufficiency are misunderstanding the constraint or making a deliberate trade-off.',
    'If foreclosed: the two readings cannot coexist in a single framework; organizations must choose one. If coexisting: both are live institutional options; the contest is about risk acceptance and resource allocation, not truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether this reading logically forecloses the simulation-sufficiency reading').

omega_variable(
    organizational_mandate_obsolescence,
    'Has the organizational mandate to maintain competence through simulation alone been rendered obsolete by escalating operational complexity, or does the mandate persist due to institutional inertia?',
    'Historical analysis of when simulation-only became standard (post-WWII aviation, post-TMI nuclear regulation) vs. when complexity outpaced simulator fidelity; correlation between simulation reliance and accident/incident rates in high-complexity domains; documentation of deliberate choices to escalate to stress-testing vs. resistance to change.',
    'If obsolete: the constraint is a piton maintained through inertia; mandatrophy is resolved by acknowledging degraded function. If still live: simulation may still be the optimal trade-off between competence maintenance and catastrophe avoidance. Affects the piton vs. snare classification for regulatory theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_mandate_obsolescence, empirical, 'Whether simulation-only mandate has become obsolete').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lived_catastrophe_necessity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lcat_theater_t0, lived_catastrophe_necessity_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lcat_theater_t10, lived_catastrophe_necessity_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(lcat_theater_t20, lived_catastrophe_necessity_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(lcat_extractiveness_t0, lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lcat_extractiveness_t10, lived_catastrophe_necessity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lcat_extractiveness_t20, lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lcat_suppression_t0, lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(lcat_suppression_t10, lived_catastrophe_necessity_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(lcat_suppression_t20, lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lived_catastrophe_necessity_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(lived_catastrophe_necessity_reading, hybrid_decay_reading).
narrative_ontology:affects_constraint(lived_catastrophe_necessity_reading, regulatory_theater_maintenance).
narrative_ontology:affects_constraint(lived_catastrophe_necessity_reading, organizational_competence_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The sibling readings (simulation_sufficiency and hybrid_decay) are separate constraint stories with different ε values and different perspectival structures. The network links represent the kernel's internal contest, not causal dependency. Each reading has its own claim about the true structural nature of exercise and competence; the three readings together map the observational disagreement across stakeholder positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lived_catastrophe_necessity_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
