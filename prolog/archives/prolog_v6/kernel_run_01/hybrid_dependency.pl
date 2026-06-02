% ============================================================================
% CONSTRAINT STORY: hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_dependency, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_dependency
 *   human_readable: Hybrid Competence Dependency: Simulation Foundation + Real-World Anchoring
 *   domain: safety_engineering/organizational_learning/high_reliability
 *
 * SUMMARY:
 *   The hybrid competence dependency emerges in high-reliability
 *   organizations (aviation, nuclear operations, maritime) where
 *   high-consequence decisions require both rapid procedural automation
 *   (achieved through simulation) and calibrated environmental awareness
 *   (achieved through real-world operations). The constraint is neither
 *   purely coordinative (simulation alone would be cheaper and sufficient for
 *   many scenarios) nor purely extractive (real-world anchoring genuinely
 *   prevents competence degradation). Instead, it is a hybrid mechanism:
 *   regulators mandate both simulation foundation and periodic real-world
 *   anchoring (line operations, non-jeopardy audits, actual aircraft time) to
 *   maintain competence. This creates a structured tension between three
 *   competing logics: (1) simulation sufficiency — advancing technology might
 *   eventually make real aircraft time unnecessary; (2) hybrid necessity —
 *   proprioceptive and temporal feedback from real operations are irreducibly
 *   important for maintaining competence; (3) catastrophe as the ultimate
 *   anchor — only exposure to failure modes (which cannot be safely
 *   simulated) reveals true limits to competence. This constraint is ONE
 *   READING of the contested kernel 'competence_exercise_requirement.' The
 *   reading instantiates the position that hybrid regime is necessary and
 *   sufficient given current and near-future technology, while acknowledging
 *   that pure simulation may eventually be adequate and that catastrophic
 *   exposure reveals limits beyond what hybrid training can address.
 *
 * KEY AGENTS:
 *   - Line Pilots: Primary victims (moderate/constrained) — bear cost of mandatory real-world anchoring (scheduling inflexibility, aircraft time requirements, recurrent audit burden)
 *   - Airline Operators: Secondary victims and partial beneficiaries (institutional/constrained) — extract mandatory training costs but also benefit from genuine competence assurance and reduced incident liability
 *   - Regulatory Authorities: Primary beneficiaries (institutional/arbitrage) — mandate hybrid regime to ensure systemic safety culture, maintain certification legitimacy, avoid competence gap liability; have flexibility to adjust simulation/real-world balance
 *   - Simulation Technology Sector: Transitional beneficiary (organized/constrained) — current interest in maintaining hybrid (ensures continued procurement) but sunset logic suggests simulation-adequacy reading may eventually reduce real-world requirements
 *   - Safety Culture / Organizational Learning: Diffuse beneficiary (institutional/analytical) — hybrid regime supports genuine organizational learning and high-reliability norms; real-world anchoring reinforces safety mindset
 *   - Analytical Observer (Neuroscience View): Risk of false summit — may naturalize contingent regulatory choice as cognitive necessity without examining whether simulation fidelity improvements could eventually close the gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_dependency, 0.52).
domain_priors:suppression_score(hybrid_dependency, 0.65).
domain_priors:theater_ratio(hybrid_dependency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(hybrid_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hybrid_dependency, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(hybrid_dependency, "Hybrid Competence Dependency: Simulation Foundation + Real-World Anchoring").
narrative_ontology:topic_domain(hybrid_dependency, "safety_engineering/organizational_learning/high_reliability").

domain_priors:requires_active_enforcement(hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_dependency, '8f40ff63-63b4-4bc2-b84c-26b25cae2294').
narrative_ontology:cs_created_at('8f40ff63-63b4-4bc2-b84c-26b25cae2294', '').
narrative_ontology:cs_kernel_codification('8f40ff63-63b4-4bc2-b84c-26b25cae2294', formalized).
narrative_ontology:cs_authority_grounding('8f40ff63-63b4-4bc2-b84c-26b25cae2294', expertise).
narrative_ontology:cs_kernel_id(hybrid_dependency, competence_exercise_requirement).
narrative_ontology:cs_reading_relation('8f40ff63-63b4-4bc2-b84c-26b25cae2294', simulation_as_adequate_exercise, influences).
narrative_ontology:cs_reading_relation('8f40ff63-63b4-4bc2-b84c-26b25cae2294', catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_axiom('8f40ff63-63b4-4bc2-b84c-26b25cae2294', foundational, proprioceptive_feedback_materially_important).
narrative_ontology:cs_axiom_status(proprioceptive_feedback_materially_important, holdable).
narrative_ontology:cs_axiom('8f40ff63-63b4-4bc2-b84c-26b25cae2294', foundational, simulation_foundation_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(simulation_foundation_necessary_but_insufficient, holdable).
narrative_ontology:cs_reference_frame('8f40ff63-63b4-4bc2-b84c-26b25cae2294', simulation_enabled_competence_regime).
narrative_ontology:cs_drift_state('8f40ff63-63b4-4bc2-b84c-26b25cae2294', contemporary_high_fidelity_simulation_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_dependency, line_operators).
narrative_ontology:constraint_beneficiary(hybrid_dependency, organizational_safety_culture).
narrative_ontology:constraint_beneficiary(hybrid_dependency, regulatory_certification_bodies).
narrative_ontology:constraint_victim(hybrid_dependency, pilot_training_cost).
narrative_ontology:constraint_victim(hybrid_dependency, operational_efficiency).
narrative_ontology:constraint_victim(hybrid_dependency, training_schedule_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINE PILOT (SNARE) — Structurally constrained by mandatory hybrid training (cannot exit without losing certification). Bears high cost: ongoing real-aircraft time requirements, scheduling inflexibility, recurrent audits beyond initial qualification. Suppression is substantial — no viable alternative training pathway exists that satisfies regulatory requirement. Effective extraction asymmetric: the constraint extracts pilot time and scheduling compliance while offering minimal direct coordination benefit to the pilot beyond regulatory compliance.
constraint_indexing:constraint_classification(hybrid_dependency, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: AIRLINE OPERATOR (TANGLED ROPE) — Experiences hybrid as both coordination mechanism and extraction requirement. Coordination function: ensures genuine operational competence, reduces incident risk, aligns with safety culture. Extraction function: mandatory recurrent training, aircraft release requirements for line audits, scheduling burden on fleet. Requires active enforcement of real-world anchoring component — pure simulation pathway is cheaper and would be selected absent regulatory mandate. Moderate exit option (constrained): could lobby for simulation-only pathway but faces regulatory resistance and incident liability if competence gaps emerge.
constraint_indexing:constraint_classification(hybrid_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Primary beneficiary with arbitrage exit. Hybrid mandate serves genuine coordination function: maintains systemic safety culture, enables legitimate incident prevention, reduces catastrophic risk exposure. The authority experiences the constraint as pure coordination — the real-world anchoring requirement prevents the competence degradation that pure simulation-trained pilots would eventually exhibit. Beneficiary position: regulatory authority avoids liability for competence gaps and can claim evidence-based safety mandate. Arbitrage exit: can adjust the balance between simulation hours and real-world requirements without eliminating the hybrid regime entirely.
constraint_indexing:constraint_classification(hybrid_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SIMULATION TECHNOLOGY SECTOR (SCAFFOLD) — Organized beneficiary with constrained exit. Current interest: maintaining hybrid regime (ensures continued simulation procurement while avoiding full simulation sufficiency standard). Scaffold sunset logic: as simulation fidelity increases and learning science improves transfer-of-training measurement, the real-world anchoring requirement may decline — high-fidelity simulators might eventually achieve sufficient competence transfer to justify reducing mandatory aircraft time. Sunset estimated 20-30 years pending VR/haptic/full-envelope simulation maturity. Suppression is moderate in this perspective (can advocate for simulation-as-adequate through research and standards bodies); effective extraction lower than pilot perspective because the sector has organizational agency.
constraint_indexing:constraint_classification(hybrid_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COGNITIVE NEUROSCIENCE (MOUNTAIN) — From civilizational scope, the hybrid requirement appears to reflect irreducible principles of skill transfer and memory consolidation: simulation provides foundation (procedural automation, knowledge structure) but cannot fully capture proprioceptive, temporal, and high-consequence environmental feedback that real aircraft operations provide. The gap between simulation and real-world performance is neurologically inevitable — transfer-of-training failures are laws of cognitive science, not policy artifacts. However, structural data contradicts pure mountain classification: identifiable beneficiaries (regulators, airlines) and extraction victims (pilots) suggest institutional constructedness rather than natural law. FALSE SUMMIT risk: cognitive science may be invoked to naturalize what is actually a contingent regulatory choice.
constraint_indexing:constraint_classification(hybrid_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LEGACY TRAINING BUREAUCRACY (PITON) — Institutional inertia perspective. The hybrid mandate persists partly through genuine safety reasoning (real-world anchoring is real) but also through bureaucratic maintenance of traditional gatekeeping. Recurrent audit procedures, line operation sign-offs, and aircraft-release scheduling are largely performative — they repeat historical practice without continuous verification that the specific real-world hours specified are optimal. Theater ratio high (0.58 baseline, rising) because the rituals of 'getting current' and 'passing check rides' have accumulated procedural weight beyond their functional contribution. The constraint is maintained not because pure simulation has been disproven, but because the training regime has institutional momentum and no institutional actor has sufficiently strong incentive to challenge it.
constraint_indexing:constraint_classification(hybrid_dependency, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_dependency, TR),
    TR >= 0.70.

:- end_tests(hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts substantial pilot time (mandatory real-aircraft hours) and scheduling compliance (audits, non-jeopardy operations) while offering pilots only the coordination benefit of regulatory compliance and safety assurance. The extraction is not maximal (snare-level) because the real-world anchoring genuinely improves competence and reduces incident risk — it is not pure coercion. However, the asymmetry is real: pilots bear mandatory time costs while airlines and regulators bear monitoring costs. Suppression (0.65): Moderate-high. Pilots have constrained but not trapped exit: they cannot fully exit without losing certification, but they can (theoretically) lobby for simulation-only standards or transition to non-safety-critical roles. Airlines similarly face suppression — they cannot unilaterally eliminate real-aircraft requirements without regulatory approval and potential incident liability. Suppression derives from regulatory mandate (legal barrier) and from genuine uncertainty about whether simulation alone is sufficient (epistemic barrier). Theater ratio (0.58, rising): Moderate. The hybrid regime includes significant performative elements: recurrent audits and line operations sometimes function as ritual validation of competence rather than discovery of genuine gaps. As simulation fidelity improves, the gap between simulated and real aircraft narrows, making real-world anchoring increasingly about regulatory-compliance ritual rather than essential competence verification. Theater ratio is rising over the interval (0.42 → 0.58) because the functional gap is shrinking faster than regulatory requirements are adjusting.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a fundamental disagreement on competence necessity. The regulatory authority (rope perspective) sees hybrid as pure coordination — necessary for genuine safety. The line pilot (snare perspective) experiences extraction — mandatory real-world time that could be replaced by improved simulation. The airline (tangled rope perspective) sees both: genuine safety coordination AND regulatory extraction. The simulation sector (scaffold perspective) sees a transitional regime that will eventually sunset as technology improves. The analytical observer (mountain perspective) risks naturalizing the constraint as a cognitive law rather than a regulatory choice. The legacy training bureaucracy (piton perspective) maintains the regime partly through institutional inertia. The perspectival gap reveals that 'competence necessity' is not factually determined — it depends on whether simulation fidelity has achieved sufficient transfer-of-training (empirical question), whether proprioceptive feedback is irreducibly important (neuroscience question), and whether regulators are conservative or evidence-driven (preference question).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from structural position relative to the hybrid requirement. Regulators (beneficiary + arbitrage) have low d → low/negative f(d) → beneficiary experienced extractiveness. Pilots (victim + constrained) have high d → high f(d) → target experienced extraction is amplified. Airlines (both coordination and extraction) have moderate d → moderate f(d) reflecting mixed position. The simulation sector (partial beneficiary + constrained) derives moderate d because they benefit from continued mandates but face constrained exit (cannot unilaterally eliminate the hybrid regime). The piton perspective (institutional + constrained) has moderate-high d because the legacy system's inertia constrains all actors, not just pilots.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the reading boundary. The hybrid_dependency reading claims that both simulation and real-world anchoring are NECESSARY to maintain competence. This is neither pure coordination (rope) nor pure extraction (snare) — it is a genuine hybrid where both functions exist. The mandatrophy is resolved by accepting that the constraint serves real coordination (safety culture, competence assurance) while extracting real costs (pilot time, airline scheduling burden). The constraint avoids being mislabeled as pure coordination (simulation-adequacy reading, which claims real-world anchoring is unnecessary) or pure extraction (catastrophe-as-anchor reading, which claims hybrid regime is insufficient). The mandate is that competence requires BOTH simulation foundation AND real-world anchoring — neither alone is sufficient, and both are necessary given current technology and ethical constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency_threshold,
    'At what level of simulation fidelity (visual, motion, haptic, environmental variability) does transfer-of-training to real aircraft become indistinguishable from real-aircraft training?',
    'Longitudinal performance correlation studies: simulator-trained vs. aircraft-trained pilots on objective metrics (incident rates, check-ride performance, emergency response time, decision quality). Cross-validation across aircraft types and operational scenarios.',
    'If threshold achievable in 10-15 years: scaffold sunset logic activates, hybrid regime transitions to simulation-primary with optional real-world anchoring. If threshold is forever-receding: mountain claim strengthens, hybrid is revealed as essential, real-world anchoring non-negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency_threshold, empirical, 'Simulation fidelity threshold for full transfer-of-training equivalence').

omega_variable(
    high_consequence_proprioceptive_irreducibility,
    'Is the proprioceptive and temporal feedback from real aircraft operations irreducibly different from simulator feedback, or is the difference skill-learnable and eventually compensated through simulation adaptation?',
    'Neuroscience and motor learning research: comparative cerebellar and vestibular system activation under simulator vs. aircraft conditions; measurement of adaptation curves for simulator-trained pilots exposed to real aircraft; controlled transfer-of-training experiments with progressive simulation realism.',
    'If irreducibly different: hybrid regime is cognitively necessary (mountain claim valid). If learnable/compensable: hybrid is preference-based or regulatory conservatism, not cognitive necessity (snare/tangled_rope claim valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_consequence_proprioceptive_irreducibility, empirical, 'Whether proprioceptive/temporal aircraft feedback is irreducibly different or learnable-compensable').

omega_variable(
    competence_degradation_timeline,
    'How quickly does competence degrade in the absence of real-world anchoring? Does the observed degradation derive from lack of procedural reinforcement or from loss of proprioceptive/environmental calibration?',
    'Historical data on pilot performance gaps after extended simulation-only periods (e.g., currency lapses, return-to-flight assessments); measurement of specific error types that emerge (procedural vs. environmental awareness vs. physical control); comparison with other high-consequence domains that use pure simulation or pure practice.',
    'If degradation rapid and severe: real-world anchoring is non-negotiable (hybrid is essential coordination, not extraction). If degradation gradual and recoverable: hybrid threshold may be lower than currently mandated (extraction component visible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_degradation_timeline, empirical, 'Timeline and mechanism of competence degradation without real-world anchoring').

omega_variable(
    reading_boundary_simulation_adequacy,
    'This constraint is one reading (hybrid_dependency) of the contested kernel ''competence_exercise_requirement''. The sibling reading ''simulation_as_adequate_exercise'' claims that sufficiently high-fidelity simulation, with appropriate learning-science protocols, can substitute entirely for real aircraft time. Under what empirical or theoretical conditions would THIS reading (hybrid_dependency) concede that that sibling reading is correct?',
    'Specify the empirical threshold at which hybrid_dependency reading would accept simulation-adequacy: e.g., ''if incident rates for simulation-trained pilots match aircraft-trained pilots for 5 years across 10,000+ pilot cohorts, hybrid loses its empirical ground.'' Or theoretical concession: ''if neuroscience demonstrates that proprioceptive gaps are learnable within 20 simulation hours of targeted transfer-of-training training.'' Without this specification, the reading is unfalsifiable.',
    'Clarifies the boundary between readings: hybrid_dependency claims real-world anchoring is NECESSARY given current and near-future technology. If the empirical threshold is met, the sibling reading becomes correct, and hybrid_dependency becomes a historical artifact of technology limitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_simulation_adequacy, conceptual, 'Empirical concession threshold for simulation-adequacy reading to be correct instead of hybrid_dependency').

omega_variable(
    catastrophe_as_knowledge_source,
    'The third sibling reading ''catastrophe_as_necessary_anchor'' claims that real-world competence ultimately depends on exposure to actual catastrophic failure modes, and that even real-aircraft training (without real crashes) is insufficient. How does this reading relate structurally to hybrid_dependency? Does hybrid_dependency FORECLOSE catastrophe_as_anchor, or do they COEXIST?',
    'Analyze the logical structure: if hybrid_dependency asserts that simulation-foundation + real-aircraft-time achieves necessary competence, does this claim require that such competence is achievable WITHOUT catastrophic exposure? Or is hybrid_dependency compatible with the claim that residual unknown-unknown failure modes exist that only actual incidents reveal?',
    'If foreclosed: hybrid_dependency is a complete competence theory and catastrophe_as_anchor is false. If coexistent: hybrid_dependency addresses achievable competence within ethical/practical constraints, while catastrophe_as_anchor describes limits to that competence (residual unknowns). The reading boundary clarifies the scope of the regulatory claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_as_knowledge_source, conceptual, 'Structural relationship between hybrid_dependency reading and catastrophe_as_necessary_anchor sibling reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_dependency, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_tr_t0, hybrid_dependency, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hybrid_tr_t5, hybrid_dependency, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hybrid_tr_t10, hybrid_dependency, theater_ratio, 10, 0.58).
narrative_ontology:measurement(hybrid_tr_t15, hybrid_dependency, theater_ratio, 15, 0.63).

% Extraction over time
narrative_ontology:measurement(hybrid_be_t0, hybrid_dependency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hybrid_be_t5, hybrid_dependency, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(hybrid_be_t10, hybrid_dependency, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(hybrid_be_t15, hybrid_dependency, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(hybrid_dependency, catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(hybrid_dependency, pilot_fatigue_scheduling_extraction).

% DUAL FORMULATION NOTE:
% The hybrid_dependency constraint is the structural center of the competence_exercise_requirement kernel. The sibling readings (simulation_as_adequate_exercise, catastrophe_as_necessary_anchor) are separate constraint stories with different ε values and beneficiary/victim structures. All three readings are linked in the network to show the kernel structure. This story addresses the reading that both simulation AND real-world anchoring are necessary; see sibling constraint stories for the readings claiming one or the other is sufficient/necessary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
