% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real Incident Necessity for Competence Occupation
 *   domain: high_reliability_organizations/safety_training/competence_maintenance
 *
 * SUMMARY:
 *   The incident-necessity reading of the competence occupation kernel claims
 *   that only actual catastrophic failures provide the authentic operational
 *   conditions necessary for personnel to genuinely occupy competence. This
 *   reading creates a structural trap: organizations must maintain
 *   operational personnel as competent, yet the reading forbids the very
 *   condition (real failure) that would prove competence. The constraint
 *   exhibits the diagnostic signature of a snare — high extraction, high
 *   suppression, rising theater ratio as simulation apparatus persists
 *   despite theoretical disavowal. Operating personnel are trapped between
 *   impossible demands: demonstrate competence without the only condition
 *   declared sufficient. Safety regulators and liability frameworks benefit
 *   from the constraint by deferring accountability until catastrophe makes
 *   negligence undeniable. Simulation training persists as a piton —
 *   organizationally necessary but epistemically disavowed by the reading.
 *   The analytical observer risks naturalizing this as an immutable asymmetry
 *   in verification (mountain perspective), when it should be recognized as a
 *   choice about what evidence counts. The constraint is one reading among
 *   three of a contested kernel about competence occupation; the sibling
 *   readings (simulation_sufficiency and hybrid_occupation) represent
 *   alternative paths for how organizations can verify and maintain
 *   operational competence.
 *
 * KEY AGENTS:
 *   - Operating Personnel: Primary victims (powerless/trapped) — must be competent yet cannot access the condition declared necessary to prove competence
 *   - Safety Regulatory Authority: Beneficiary and coordinator (organized/constrained) — solves liability and oversight problems while maintaining impossibility
 *   - Insurance and Liability Framework: Primary beneficiary (institutional/arbitrage) — incident-necessity enables clean liability transfer by deferring proof of negligence
 *   - Simulation Training Apparatus: Degraded coordinator (institutional/arbitrage) — functions pragmatically but theoretically disavowed, persisting through institutional necessity despite reading's disqualification
 *   - Hybrid Occupation Movement: Organized alternative (organized/mobile) — developing multi-mechanism competence occupation with sunset trajectory replacing incident-dependency
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the verification asymmetry as law rather than recognized choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.68).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.72).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, snare).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real Incident Necessity for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training/competence_maintenance").

domain_priors:requires_active_enforcement(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, '5fa9586a-e184-4b2a-8958-722fd0dee039').
narrative_ontology:cs_kernel_codification('5fa9586a-e184-4b2a-8958-722fd0dee039', formalized).
narrative_ontology:cs_authority_grounding('5fa9586a-e184-4b2a-8958-722fd0dee039', extraction).
narrative_ontology:cs_interpretation_layer_present('5fa9586a-e184-4b2a-8958-722fd0dee039').
narrative_ontology:cs_reading_relation('5fa9586a-e184-4b2a-8958-722fd0dee039', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('5fa9586a-e184-4b2a-8958-722fd0dee039', competence_occupation__hybrid_occupation, coexists_with).
narrative_ontology:cs_axiom('5fa9586a-e184-4b2a-8958-722fd0dee039', foundational, authenticity_requires_real_conditions).
narrative_ontology:cs_axiom_status(authenticity_requires_real_conditions, holdable).
narrative_ontology:cs_axiom_grounding('5fa9586a-e184-4b2a-8958-722fd0dee039', authenticity_requires_real_conditions, empirically_contingent).
narrative_ontology:cs_axiom('5fa9586a-e184-4b2a-8958-722fd0dee039', secondary, simulation_bounded_by_designer_foresight).
narrative_ontology:cs_axiom_status(simulation_bounded_by_designer_foresight, holdable).
narrative_ontology:cs_axiom_grounding('5fa9586a-e184-4b2a-8958-722fd0dee039', simulation_bounded_by_designer_foresight, empirically_contingent).
narrative_ontology:cs_reference_frame('5fa9586a-e184-4b2a-8958-722fd0dee039', catastrophic_incident_as_sole_competence_proof).
narrative_ontology:cs_drift_state('5fa9586a-e184-4b2a-8958-722fd0dee039', contemporary_simulation_maturity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fa9586a-e184-4b2a-8958-722fd0dee039', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, operating_personnel).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, public_safety).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, organizational_learning_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATING PERSONNEL (SNARE) — Trapped in a competence maintenance framework that requires catastrophic incidents to demonstrate adequacy. Personnel cannot exit; their careers and safety depend on a constraint that forbids the very condition (actual failure) needed to prove competence. Maximum extraction: career risk without legitimate path to competence occupation. The suppression is extreme — all alternatives to real incidents are declared insufficient by the reading's own logic.
constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY REGULATORY AUTHORITY (TANGLED ROPE) — Faces a genuine coordination problem (how to verify operational competence?) alongside extraction (the real-incident-only standard minimizes liability exposure by making incompetence deniable until catastrophe proves it). Benefits from the constraint by deferring accountability; constrained by the epistemic impossibility of the standard — can never actually verify competence until the worst outcome occurs. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSURANCE AND LIABILITY FRAMEWORK (ROPE) — The real-incident-only standard enables clean liability transfer: any incident prior to the one that definitively proves incompetence is classified as 'unavoidable' rather than 'proof of negligent training.' The framework benefits from the constraint through coordination: it solves the liability allocation problem by making competence demonstration impossible until catastrophe. No exit cost — this is the framework's native operating logic.
constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SIMULATION-BASED TRAINING APPARATUS (PITON) — High-fidelity simulation and simulator-based competence certification persist despite the reading's claim that they are epistemically insufficient. The apparatus continues through institutional inertia — organizations cannot actually abandon simulation (catastrophe-only verification is operationally impossible) yet the reading's logic declares it invalid. Theater ratio (0.55) reflects that simulation training functions pragmatically but is theoretically disavowed by the incident-necessity reading. The apparatus is maintained as performative backup while the reading denies its adequacy.
constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, there is a genuine logical asymmetry: simulation occupies a different epistemic space than real operation. No amount of simulation can prove that personnel will perform under catastrophic stress until they actually do. This asymmetry appears to be a natural law of verification — real incidents are the only authentic ground for the claim 'this person is truly competent.' However, the engine will detect this as a false summit: the asymmetry is real but naturalized into inevitability when it should be recognized as a choice about what evidence counts.
constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HYBRID OCCUPATION MOVEMENT (SCAFFOLD) — Alternative reading (the hybrid_occupation sibling) represents a sunset to the incident-necessity regime. Organized safety researchers, incident investigation communities, and reformed organizations are developing multi-mechanism competence occupation (simulation + refresher training + procedural audits + selective line exercises) with the explicit goal of replacing catastrophe-dependency. Low extraction because these agents have agency and see a path forward. The sunset is institutional norm maturation — as evidence accumulates that multi-mechanism approaches prevent incidents effectively, the incident-necessity reading loses epistemic ground.
constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_occupation__real_incident_necessity, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_occupation__real_incident_necessity, TR),
    TR >= 0.70.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts safety margin from operating personnel (they must be ready for catastrophe but cannot train under catastrophic conditions) and from organizations (catastrophe is the only acceptable proof of adequacy, yet organizations cannot ethically permit catastrophe). The reading creates an inescapable dilemma: prove competence through simulation (which the reading declares insufficient) or wait for catastrophe (which is unacceptable and legally indefensible). Rising extractiveness (0.52→0.68) reflects accumulating pressure as alternative approaches mature and the incident-necessity reading appears increasingly obsolete. Suppression (0.72): Very high. All alternatives to real incidents are suppressed — simulation is declared epistemically invalid, scenario training is dismissed as insufficient, procedural audits are treated as theater. Personnel have no legitimate path to competence occupation except catastrophe. Rising suppression reflects hardening of the incident-necessity position as it faces empirical challenge. Theater ratio (0.55): Moderate-high. Organizations claim to follow incident-necessity logic while simultaneously operating under hybrid multi-mechanism approaches. Simulation apparatus, refresher training, and procedural audits persist as performative activity — organizationally necessary but theoretically disavowed. Rising theater reflects the widening gap between the reading's claims and operational reality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates six incommensurable perceptions: Operating personnel see extraction (Snare) — impossible demands. Regulators see coordination with extraction (Tangled Rope) — solving the oversight problem while extracting from personnel. Liability frameworks see pure coordination (Rope) — their problem is cleanly solved. Simulation apparatus sees degradation (Piton) — it works but is theoretically invalid. The analytical observer risks seeing natural law (Mountain) — the verification asymmetry appears immutable. The hybrid occupation movement sees a solvable problem with a sunset (Scaffold) — alternative approaches can replace incident-dependency. This perspectival spread reveals that the incident-necessity reading is not describing an objective constraint but rather enacting a choice about what evidence counts as legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint has no beneficiary structure in the conventional sense — catastrophes are unacceptable, so nobody genuinely wants the condition the reading declares necessary. However, several institutional actors benefit from the constraint's logical structure itself: Regulators benefit by deferring accountability (they cannot be negligent in oversight if competence can only be demonstrated through incident). Liability frameworks benefit through clean transfer (no negligent training can be proven until catastrophe makes it obvious). This creates a peculiar directionality: the constraint benefits from the reading's existence even though it cannot benefit from the reading's logical conclusion (catastrophe as proof). The suppression flows toward operating personnel and public safety — both are trapped in a framework that demands the impossible.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint achieves extractiveness 0.68 (>0.70) with mandatrophy_resolved: true. The mandatrophy is resolved by recognizing that the incident-necessity reading WANTS to be a Snare but MUST operate as a Piton in reality. The reading's logical conclusion (only catastrophes prove competence) is operationally impossible — no organization can ethically wait for catastrophe to verify personnel readiness. Therefore, the reading persists as performative disavowal: organizations theoretically endorse incident-necessity while actually operating under hybrid multi-mechanism approaches. The Snare classification applies to what the reading's logic demands; the Piton classification applies to what organizations actually do. The constraint resolves its contradiction not by correcting its premises but by fragmenting into theory (Snare: the reading's logical structure) and practice (Piton: the apparatus that violates the reading while claiming to follow it). This fragmentation is the constraint's actual form of existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_demonstration_impossibility,
    'Is the incident-necessity reading logically foreclosing or merely claiming superior epistemic status for real incidents?',
    'Philosophical analysis: Does ''only real incidents can demonstrate competence'' mean (a) all other evidence is logically invalid (forecloses), or (b) real incidents are just better evidence but not the only valid evidence (coexists)? If (a), no simulation/training can ever count as competence proof. If (b), the reading is making a claim about sufficiency that rivals but does not eliminate alternatives.',
    'If foreclosing: the reading is self-refuting (organizations cannot operate under incident-only competence verification). If coexisting: the reading is holding an empirical position that could be overridden by evidence that alternatives prevent incidents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_demonstration_impossibility, conceptual, 'Whether incident-necessity logically forecloses alternatives or claims epistemic superiority').

omega_variable(
    extraction_vs_safety_alignment,
    'Is this constraint a mechanism for genuine safety assurance or a mechanism for liability deferral that incidentally demands catastrophe?',
    'Empirical comparison: Do organizations operating under incident-necessity reading show lower incident rates than organizations using hybrid multi-mechanism approaches? Causal analysis: Does the constraint''s existence improve safety or merely defer liability until catastrophe makes it unavoidable?',
    'If genuine safety: the snare classification is incorrect (should be rope or mountain). If liability deferral: the snare classification is confirmed — the constraint extracts safety from personnel through imposed uncertainty while deferring organizational accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_safety_alignment, empirical, 'Whether incident-necessity improves safety or defers liability').

omega_variable(
    simulation_sufficiency_empirical_status,
    'How much of modern high-reliability operational competence can be authentically occupied through simulation, scenario-based training, and procedural reinforcement WITHOUT actual catastrophic incidents?',
    'Longitudinal incident data in aviation, nuclear operations, maritime, and emergency response comparing organizations with advanced simulation programs to those relying on incident-based learning. Analysis of near-miss systems and how they enable competence occupation below the catastrophe threshold.',
    'If sufficient (>85% of operational competence can be occupied): simulation_sufficiency reading has strong empirical basis and incident-necessity is unnecessarily restrictive. If insufficient (<50%): incident-necessity has empirical support and hybrid approaches are inadequate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_sufficiency_empirical_status, empirical, 'Empirical adequacy of simulation for competence occupation').

omega_variable(
    kernel_reading_committer_content,
    'This constraint instantiates ONE reading of a contested kernel about how competence occupation occurs. The sibling readings are simulation_sufficiency and hybrid_occupation. What structural assumptions distinguish this reading''s epistemic claims?',
    'Axiomatic analysis: The incident-necessity reading assumes (1) authenticity requires real-world conditions, (2) simulation is inherently bounded by designer foresight, (3) competence under stress cannot be extrapolated from competence under controlled conditions. Each sibling makes different assumptions. Empirical data on stress-response transferability and simulation fidelity can resolve whether these axioms hold.',
    'If authenticity axiom holds: incident-necessity forecloses simulation_sufficiency. If it doesn''t hold: the readings coexist as different empirical claims about the same competence phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_content, conceptual, 'Structural distinction between incident-necessity, simulation_sufficiency, and hybrid_occupation readings').

omega_variable(
    organizational_impossibility,
    'Can an organization actually operate under incident-necessity reading without immediately violating duty-of-care requirements?',
    'Legal and regulatory analysis: Does the incident-necessity standard constitute gross negligence (knowingly operating without alternative competence verification)? Practical analysis: Does any actual high-reliability organization use ONLY incident-based competence confirmation?',
    'If operationally impossible: the reading is aspirational or theoretical, not a real constraint. Organizations that claim to follow incident-necessity are actually operating under hybrid_occupation (piton classification applies — the reading is performative disavowal of what actually happens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_impossibility, empirical, 'Whether incident-necessity is operationally feasible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmptn_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cmptn_tr_t10, competence_occupation__real_incident_necessity, theater_ratio, 10, 0.51).
narrative_ontology:measurement(cmptn_tr_t20, competence_occupation__real_incident_necessity, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(cmptn_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cmptn_be_t10, competence_occupation__real_incident_necessity, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(cmptn_be_t20, competence_occupation__real_incident_necessity, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cmptn_su_t0, competence_occupation__real_incident_necessity, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cmptn_su_t10, competence_occupation__real_incident_necessity, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(cmptn_su_t20, competence_occupation__real_incident_necessity, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, near_miss_suppression_in_hro_learning).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, liability_deferral_through_competence_indefinability).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel has three distinct readings (real_incident_necessity, simulation_sufficiency, hybrid_occupation), each with its own constraint story and ε value. This story is the incident-necessity reading only (ε=0.68, Snare). Sibling stories have different ε values reflecting their different structural claims. Links via network.affects_constraints show how the three readings interact: incident-necessity inflates extractiveness in hybrid approaches (which must perform both real and simulated competence occupation); simulation_sufficiency competes for epistemic legitimacy; hybrid_occupation represents the institutional escape path from the incident-necessity trap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__real_incident_necessity, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
