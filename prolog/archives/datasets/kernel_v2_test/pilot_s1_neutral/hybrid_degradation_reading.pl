% ============================================================================
% CONSTRAINT STORY: hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_degradation_reading, []).

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
 *   constraint_id: hybrid_degradation_reading
 *   human_readable: Simulation-Maintained Competence with Generational Tacit Knowledge Decay
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint represents a reading of the catastrophe_proxy_sufficiency
 *   kernel: the claim that simulation can substitute for real-world
 *   catastrophes as a mechanism for preserving organizational learning and
 *   safety capacity. This reading (hybrid_degradation_reading) asserts that
 *   simulation successfully maintains procedural competence—operators can
 *   execute standardized procedures, follow checklists, and handle scripted
 *   scenarios—while tacit knowledge and stress-response capacity degrade
 *   across generational timescales. The constraint is a tangled_rope because
 *   it contains both genuine coordination (simulation solves the legitimate
 *   problem of scalable, standardizable competency assessment) and asymmetric
 *   extraction (the certification industry and regulatory bureaucracy benefit
 *   from simulation-sufficiency doctrine while long-term safety margins and
 *   operator stress-response capacity bear the hidden costs). The measurement
 *   trajectory (extractiveness rising from 0.35 to 0.58, theater ratio rising
 *   from 0.45 to 0.72 over a 30-year interval) shows the constraint's
 *   functional degradation over time: as generations pass without major
 *   catastrophes, simulation becomes increasingly theatrical—its performative
 *   role (demonstrating compliance, justifying certification) grows while its
 *   actual preservation of tacit knowledge capacity declines. The suppression
 *   trajectory (0.35 → 0.54) reflects rising costs to exit: as simulation
 *   becomes more entrenched in regulatory frameworks and training
 *   infrastructure, operators and facilities face increasing barriers to
 *   investing in alternative knowledge-preservation mechanisms
 *   (apprenticeship, mentorship, near-miss learning).
 *
 * KEY AGENTS:
 *   - Field Operators: Primary victim (powerless/trapped) — certified via simulation but lacking tacit stress-response capacity; cannot exit the facility or refuse certification
 *   - Facility Management: Secondary victim (moderate/constrained) — benefit from simulation cost reduction but bear accountability for safety outcomes while regulatory capture limits their control over actual safety margins
 *   - Certification Training Industry: Primary beneficiary (institutional/arbitrage) — captures ongoing revenue from training updates, recertification cycles, and simulation scenario development
 *   - Regulatory Authority: Organized victim-beneficiary (organized/constrained) — benefits from scalable compliance metrics but bears liability for safety outcomes while the regulatory framework itself certifies simulation as sufficient
 *   - Long-Term Safety Margins: Victim (powerless/trapped) — abstract organizational capacity erodes as tacit knowledge and stress-response patterns are not transmitted across generational transitions
 *   - Simulation Efficacy Doctrine: Institutional performance (piton) — persists through inertia despite accumulated research showing simulation captures procedural competence but fails to preserve tacit judgment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as inherent features of knowledge transmission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_degradation_reading, 0.52).
domain_priors:suppression_score(hybrid_degradation_reading, 0.48).
domain_priors:theater_ratio(hybrid_degradation_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_degradation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hybrid_degradation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hybrid_degradation_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_degradation_reading, "Simulation-Maintained Competence with Generational Tacit Knowledge Decay").
narrative_ontology:topic_domain(hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_degradation_reading, 'e35710bf-673c-422e-b2fb-628e5801c82e').
narrative_ontology:cs_kernel_codification('e35710bf-673c-422e-b2fb-628e5801c82e', formalized).
narrative_ontology:cs_authority_grounding('e35710bf-673c-422e-b2fb-628e5801c82e', extraction).
narrative_ontology:cs_interpretation_layer_present('e35710bf-673c-422e-b2fb-628e5801c82e').
narrative_ontology:cs_reading_relation('e35710bf-673c-422e-b2fb-628e5801c82e', hybrid_degradation_reading__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('e35710bf-673c-422e-b2fb-628e5801c82e', hybrid_degradation_reading__catastrophe_necessity_reading, influences).
narrative_ontology:cs_reading_relation('e35710bf-673c-422e-b2fb-628e5801c82e', hybrid_degradation_reading__simulation_fidelity_threshold, coexists_with).
narrative_ontology:cs_axiom('e35710bf-673c-422e-b2fb-628e5801c82e', foundational, procedural_competence_transferability).
narrative_ontology:cs_axiom_status(procedural_competence_transferability, holdable).
narrative_ontology:cs_axiom_grounding('e35710bf-673c-422e-b2fb-628e5801c82e', procedural_competence_transferability, empirically_contingent).
narrative_ontology:cs_axiom('e35710bf-673c-422e-b2fb-628e5801c82e', foundational, tacit_knowledge_generational_decay).
narrative_ontology:cs_axiom_status(tacit_knowledge_generational_decay, holdable).
narrative_ontology:cs_axiom_grounding('e35710bf-673c-422e-b2fb-628e5801c82e', tacit_knowledge_generational_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('e35710bf-673c-422e-b2fb-628e5801c82e', simulation_competency_maintenance).
narrative_ontology:cs_drift_state('e35710bf-673c-422e-b2fb-628e5801c82e', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e35710bf-673c-422e-b2fb-628e5801c82e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, regulatory_compliance_bureaucracy).
narrative_ontology:constraint_victim(hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(hybrid_degradation_reading, operational_stress_response_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD OPERATOR (SNARE) — Operators at the facility face the full extractive burden of the constraint without benefit. Simulation-based training certifies them as competent, but tacit knowledge (equipment idiosyncrasies, stress-response patterns learned through rare events) erodes over generations. When a genuine catastrophe occurs, operators discover their actual stress-response capacity is lower than the simulation predicted. They are trapped: cannot exit the facility, cannot opt out of relying on their training certification, cannot recover lost tacit knowledge from departed colleagues. Maximum experienced extraction — the power asymmetry is between certified-but-unprepared operators and the systems they are supposed to control.
constraint_indexing:constraint_classification(hybrid_degradation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FACILITY OPERATOR / MANAGEMENT (TANGLED ROPE) — Plant operators and facility managers benefit from simulation as a cost-reduction mechanism (cheaper than catastrophe-based learning or extended apprenticeships). They also bear asymmetric extraction: they are accountable for safety outcomes while regulatory frameworks certify simulation as sufficient, shifting accountability burden while reducing their control over actual safety margins. Constrained by regulations requiring simulation-based training and by the absence of compelling alternatives. Moderately extractive because they have some agency (can invest in supplementary apprenticeship, equipment redundancy, procedural overhead) but regulatory capture makes these choices costly.
constraint_indexing:constraint_classification(hybrid_degradation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CERTIFICATION TRAINING INDUSTRY (ROPE) — Training providers benefit from simulation-based certification regimes. The constraint solves a genuine coordination problem (how to standardize competency assessment across heterogeneous facilities) while generating ongoing revenue for training updates, recertification cycles, and simulation scenario development. Arbitrage options available: can pivot to different certification domains, develop proprietary simulation systems, export training services internationally. Net beneficiary. The training industry experiences low effective extraction because they are the primary beneficiaries and face no binding exit barriers.
constraint_indexing:constraint_classification(hybrid_degradation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Regulators benefit from simulation-based certification: it provides a scalable, standardizable compliance metric (simulation scores are quantifiable and comparable across jurisdictions) and reduces the regulatory burden of verifying tacit competency. They also bear asymmetric extraction: they are accountable for safety outcomes but the regulatory framework itself certifies simulation as sufficient, creating a hidden liability when tacit decay causes real failures. Organized agents with constrained exit (cannot abandon regulation, cannot devolve safety responsibility to industry alone). Moderately extractive at the regulatory level because the organized authority has significant agency but faces political and institutional constraints.
constraint_indexing:constraint_classification(hybrid_degradation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SIMULATION EFFICACY DOCTRINE (PITON) — From an institutional/civilizational view, the doctrine that simulation adequately substitutes for real-world catastrophe-based learning has atrophied. Accumulated research (aviation human factors, nuclear operator studies, military command post exercises) shows simulation captures procedural competence but fails to preserve stress-response capacity and tacit judgment. The doctrine persists through institutional inertia: simulation infrastructure is in place, training budgets are allocated, regulatory frameworks are codified. Continuing to deploy simulation despite known limitations is largely theatrical — maintaining the appearance that the training system is functioning adequately. High theater ratio (0.65) reflects the gap between stated simulation efficacy and demonstrated degradation of tacit knowledge.
constraint_indexing:constraint_classification(hybrid_degradation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some loss of tacit knowledge across generational transitions is inherent to organizational learning: explicit procedures preserve only what can be codified; stress-response patterns emerge from embodied experience in rare, high-consequence events; generations separated by institutional stability inevitably lose first-hand encounters with failure modes. This perspective sees the constraint as an immutable property of how organizational knowledge is transmitted. However, the structural data reveals this as a false summit: the constraint benefits identifiable actors (training industry, regulatory compliance bureaucracy) and can be mitigated by choices (extending apprenticeship periods, investing in near-miss learning systems, rotating experienced operators). The 'natural law' framing naturalizes what is actually a contingent institutional arrangement optimized for cost reduction.
constraint_indexing:constraint_classification(hybrid_degradation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_degradation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_degradation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_degradation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_degradation_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The constraint extracts from field operators (who depend on certification that understates actual knowledge gaps) and from long-term safety margins (which decline as tacit knowledge is not transmitted). The beneficiaries are the training industry and regulatory bureaucracy, whose interests align in maintaining the simulation-certification regime. The measurement trajectory showing rising extractiveness over 30 years reflects the accumulation of tacit knowledge debt: as generations pass without major catastrophes, the gap between simulated and actual stress-response capacity widens, but remains hidden because certification classes are being completed on schedule. Suppression (0.48): Moderate. Operators face barriers to learning tacit knowledge outside the certified simulation regime (cost of apprenticeship, time requirements, equipment access, mentorship availability). Facilities face regulatory barriers to investing in supplementary tacit-knowledge mechanisms (simulation-sufficiency doctrine is codified; alternatives require costly regulatory exceptions). Rising suppression trajectory reflects institutional entrenchment: the longer the regime persists without obvious failure, the harder it becomes to exit. Theater ratio (0.65): High and rising. Simulation-based certification is substantially performative—it demonstrates compliance to regulators and justifies cost reduction to managers while failing to address the actual preservation of stress-response capacity. The rising trajectory reflects increasing gap between theatrical compliance and actual safety margins: after 30 years, simulation certifications continue to be issued and recertifications completed, but accumulated research shows operator cohorts with no exposure to near-misses show degraded performance under stress. The performative content (completing training cycles on time, achieving certification scores) persists while the functional content (preserving actual stress-response capacity) erodes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a perspectival gap between beneficiaries and victims. The certification training industry sees a solution (Rope) to a genuine coordination problem: how to standardize competency assessment at scale. The regulatory authority sees a pragmatic tool (Tangled Rope) that balances scalability with accountability, though it creates hidden liability. Facility management sees cost reduction (Tangled Rope) balanced against accountability exposure. Field operators see the constraint as a failure mechanism (Snare): they are certified as competent in simulations where they performed adequately, but when actual high-consequence events occur (rare events that simulation cannot replicate), they discover their actual stress-response capacity is lower than expected. The long-term safety margins are an abstract victim (Snare): no concrete agent advocates for generational knowledge preservation when the regulatory system certifies simulation as sufficient. The simulation efficacy doctrine appears as a degraded ritual (Piton) from the civilizational perspective: it persists through inertia despite accumulated evidence that procedural competence does not transfer to actual stress-response. The analytical observer's mountain classification risks naturalizing what is actually a contingent institutional arrangement: tacit knowledge decay is not an immutable law of organizational learning, but a consequence of specific cost-minimization choices in training regime design.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading (hybrid_degradation_reading) instantiates the catastrophe_proxy_sufficiency kernel with a specific claim: simulation can maintain procedural competence while tacit knowledge and stress-response capacity degrade. The kernel is contested across four distinct readings: (1) hybrid_degradation_reading (this story) — simulation as partial solution with hidden decay; (2) simulation_as_proxy_catastrophe_reading — simulation's certification naturalizes failure to prepare; (3) catastrophe_necessity_reading — catastrophic events are irreplaceable; (4) simulation_fidelity_threshold — adequacy depends on measurable fidelity standards. Each reading has a different ε, beneficiary/victim structure, and classification. This reading coexists with the proxy_catastrophe and fidelity_threshold readings (different parties hold different readings simultaneously) and influences the catastrophe_necessity reading (if hybrid degradation is real, catastrophe becomes necessary for learning that simulation cannot provide). The regulatory authority and training industry endorse the hybrid_degradation reading because it preserves the certification regime while acknowledging limits — a politically defensible position. The analytical observer risks the false summit of naturalizing the constraint as inherent to knowledge transmission, when it is actually a contingent outcome of cost-minimization choices.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is NOT RESOLVED in this reading. The founding mandate of simulation-based training is to standardize competency assessment at scale and reduce training costs. This mandate remains live: simulation does accomplish these goals. However, the secondary mandate (implicit in the regulatory framework) is to preserve organizational learning and safety margins across generational transitions. This mandate has been outlived: accumulated evidence shows simulation maintains procedural competence but fails to preserve tacit knowledge and stress-response capacity. The hybrid_degradation reading acknowledges the mandatrophy by naming it explicitly (theater_ratio of 0.65 reflects the gap between stated and actual function) but does not resolve it because the reading coexists with alternative readings that dispute whether the mandate has truly been outlived (the catastrophe_necessity reading argues that no training regime preserves tacit knowledge without real catastrophes; the fidelity_threshold reading argues that measurement and enforcement of tacit knowledge preservation is possible). The constraint's piton classification (degraded ritual maintained through inertia) is the concrete expression of unresolved mandatrophy: the training system continues to certify operators and issue recertifications despite known limitations in preserving actual safety capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_vs_procedural_sufficiency,
    'Is the gap between simulation-certified procedural competence and actual stress-response capacity a structural feature of organizational knowledge transmission, or a contingent outcome of cost-minimization choices in training regime design?',
    'Comparative analysis of organizations with different tacit-knowledge preservation investments (extended apprenticeship, shadowing, near-miss learning programs, intergenerational mentorship) vs simulation-only regimes. Measure actual vs simulated stress-response performance in high-consequence scenarios (crisis simulations with real physiological monitoring, post-incident behavioral analysis).',
    'If structural (near-universal across knowledge domains): classify constraint as mountain with high accessibility_collapse. If contingent (variance correlates with investment choices): classify as tangled_rope with higher potential for renegotiation. If contingent but path-dependent (initial cost-minimization created institutional lock-in now expensive to reverse): classify as snare with high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_vs_procedural_sufficiency, empirical, 'Whether tacit knowledge decay is structural or contingent on training regime choices').

omega_variable(
    certification_doctrine_natural_law_ambiguity,
    'Does the constraint represent a genuine natural law (tacit knowledge is inherently difficult to transmit across generations), or is it a constructed constraint where the certification industry and regulatory bureaucracy benefit from naturalizing a choice (simulation as sufficient) as inevitable?',
    'Historical genealogy of simulation adoption in safety-critical domains: trace when simulation became certified as adequate, which organizations advocated for that standard, what alternatives were foreclosed, and whether the ''natural law'' framing preceded or followed industry incentive alignment. Examine organizations that rejected the natural law framing and invested in tacit knowledge preservation despite regulatory pressure.',
    'If natural law: regulatory reliance on simulation is justified; the constraint persists regardless of stakeholder opposition. If constructed: the constraint is a false summit eligible for reclassification to tangled_rope with explicit beneficiary/victim structure. If artificially naturalized: the constraint is a snare with high suppression because the natural law framing forecloses alternative training regimes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certification_doctrine_natural_law_ambiguity, conceptual, 'Is tacit knowledge decay a natural law or a constructed constraint with beneficiaries?').

omega_variable(
    catastrophe_necessity_vs_simulation_substitution,
    'Can simulation adequately substitute for the learning that occurs through real-world near-misses and low-frequency high-consequence events, or do stress-response patterns require embodied experience with actual consequences?',
    'Empirical comparison of operator stress-response in simulated vs actual high-consequence scenarios (heart rate, decision latency, error patterns). Analysis of incident reports: do operators trained only on simulation show different failure modes in actual crisis compared to operators with exposure to near-misses or mentorship from experienced colleagues? Neuroscientific evidence on whether fear conditioning and stress-response patterns learned in simulation transfer to high-stakes real-world contexts.',
    'If simulation is adequate: the constraint is minimally extractive; training certification approaches the rope type. If simulation is inadequate: the constraint is substantially extractive; classification approaches snare for operators and tangled_rope for managers. If adequacy is intermediate and dependent on supplementary tacit learning: classification remains tangled_rope but with higher suppression reflecting the asymmetric knowledge transfer burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_simulation_substitution, empirical, 'Whether simulation can substitute for real-world learning in stress-response patterns').

omega_variable(
    kernel_reading_ambiguity,
    'Which reading of the catastrophe_proxy_sufficiency kernel is structurally true: (a) simulation maintains procedural competence while tacit knowledge degrades (hybrid_degradation_reading, this story), (b) simulation''s certification naturalizes what is actually a failure to prepare for catastrophe (simulation_as_proxy_catastrophe_reading), (c) catastrophic events are necessary and irreplaceable elements of organizational learning (catastrophe_necessity_reading), or (d) simulation adequacy depends on fidelity thresholds we can measure and enforce (simulation_fidelity_threshold)?',
    'The ambiguity is resolved through interdependencies between constraint stories. The hybrid_degradation_reading coexists with the simulation_as_proxy reading (both identify partial failure); influences the catastrophe_necessity reading (if hybrid degradation is real, then catastrophe becomes more rather than less necessary); and interfaces with the fidelity_threshold reading (if fidelity matters, then measurement of tacit knowledge preservation becomes possible). The kernel''s authority structure (regulatory frameworks and training industry incentives) currently endorses the hybrid_degradation reading because it preserves the certification system while acknowledging limits. Resolution requires cross-reading analysis in the constraint family.',
    'If hybrid_degradation is the dominant reading: the tangled_rope classification stands, and regulatory reform should focus on supplementary tacit-knowledge mechanisms. If simulation_as_proxy foreclosed hybrid_degradation: the classification would shift to snare for operators. If catastrophe_necessity forecloses both: the entire simulation regime would require rethinking. If fidelity_threshold influences the reading: measurement and enforcement of tacit knowledge preservation become enforceable regulatory requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which kernel reading is structurally true regarding simulation adequacy and tacit knowledge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_degradation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hyb_deg_tr_t0, hybrid_degradation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hyb_deg_tr_t10, hybrid_degradation_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(hyb_deg_tr_t20, hybrid_degradation_reading, theater_ratio, 20, 0.65).
narrative_ontology:measurement(hyb_deg_tr_t30, hybrid_degradation_reading, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(hyb_deg_be_t0, hybrid_degradation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hyb_deg_be_t10, hybrid_degradation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hyb_deg_be_t20, hybrid_degradation_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(hyb_deg_be_t30, hybrid_degradation_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hyb_deg_su_t0, hybrid_degradation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hyb_deg_su_t15, hybrid_degradation_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(hyb_deg_su_t30, hybrid_degradation_reading, suppression_requirement, 30, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(hybrid_degradation_reading, 0.12).
narrative_ontology:affects_constraint(hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(hybrid_degradation_reading, simulation_fidelity_threshold).
narrative_ontology:affects_constraint(hybrid_degradation_reading, training_infrastructure_lock_in).
narrative_ontology:affects_constraint(hybrid_degradation_reading, operator_stress_response_erosion).

% DUAL FORMULATION NOTE:
% The hybrid_degradation_reading is part of a constraint family decomposing the catastrophe_proxy_sufficiency kernel. Each family member (the four reading stories plus two dependent stories on infrastructure and stress-response) has its own ε, beneficiary/victim structure, and classification. They are not versions of a single constraint but distinct structural arrangements linked through the kernel they contest. Cross-reading analysis requires measuring which reading the field actually instantiates: do operators show degraded stress-response (supporting hybrid_degradation), or do they show certification-masked unpreparedness (supporting proxy_catastrophe), or is stress-response learnable only through real catastrophes (supporting necessity), or can measurement and enforcement of fidelity standards control the outcome (supporting fidelity_threshold)?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_degradation_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
