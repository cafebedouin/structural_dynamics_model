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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_degradation_reading
 *   human_readable: Hybrid Degradation: Simulation Maintains Procedural Competence While Tacit Knowledge Decays
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint describes a structural trap in high-reliability
 *   organizations where simulation-based training maintains measurable
 *   procedural competence while tacit knowledge and stress-response capacity
 *   degrade silently across generational timescales. The constraint is a
 *   tangled rope: it solves a genuine coordination problem (standardizing
 *   training, reducing costs, enabling scaling) while simultaneously
 *   extracting from long-term safety margins through a mechanism that is
 *   structurally invisible until catastrophe reveals it. The beneficiaries
 *   (certification industry, regulatory apparatus) have no incentive to
 *   measure tacit knowledge degradation because doing so would undermine the
 *   simulation-as-proxy narrative that justifies their business models and
 *   compliance frameworks. The victims (operators, long-term safety margins,
 *   tacit knowledge commons) bear the cost of this degradation but cannot
 *   exit the certification regime. The constraint is one reading of a
 *   contested kernel about whether simulation is a sufficient proxy for
 *   catastrophe prevention.
 *
 * KEY AGENTS:
 *   - Certification Training Industry: Primary beneficiary (institutional/arbitrage) — captures recurring revenue from simulation-based training mandates; has exit options (can shift to new technologies, expand domains)
 *   - Regulatory Compliance Apparatus: Secondary beneficiary (institutional/arbitrage) — benefits from measurable, auditable compliance metrics; has exit options (can adjust standards, adopt new technologies)
 *   - Operators: Primary victim (moderate/constrained) — gain procedural competence but lose access to stress-response development; constrained by certification mandates; face deferred extraction risk in real catastrophes
 *   - Tacit Knowledge Commons: Victim (powerless/trapped) — abstract collective good that cannot exit or organize; bears full cost of knowledge degradation; no mechanism for detecting or recovering lost expertise
 *   - Long-Term Safety Margins: Victim (powerless/trapped) — abstract safety buffer that decays silently; no self-correction mechanism; extraction deferred until catastrophe
 *   - Training Organizations: Mixed position (moderate/constrained) — benefit from simulation access and standardized curricula; constrained by regulatory mandates; lose pedagogical depth
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as a genuine tangled rope with real coordination function and real extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_degradation_reading, 0.58).
domain_priors:suppression_score(hybrid_degradation_reading, 0.62).
domain_priors:theater_ratio(hybrid_degradation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_degradation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hybrid_degradation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hybrid_degradation_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_degradation_reading, "Hybrid Degradation: Simulation Maintains Procedural Competence While Tacit Knowledge Decays").
narrative_ontology:topic_domain(hybrid_degradation_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_degradation_reading, '454b2175-88c5-4cba-bceb-0b6217192f8e').
narrative_ontology:cs_kernel_codification('454b2175-88c5-4cba-bceb-0b6217192f8e', distributed).
narrative_ontology:cs_authority_grounding('454b2175-88c5-4cba-bceb-0b6217192f8e', extraction).
narrative_ontology:cs_interpretation_layer_present('454b2175-88c5-4cba-bceb-0b6217192f8e').
narrative_ontology:cs_reading_relation('454b2175-88c5-4cba-bceb-0b6217192f8e', hybrid_degradation_reading__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('454b2175-88c5-4cba-bceb-0b6217192f8e', hybrid_degradation_reading__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('454b2175-88c5-4cba-bceb-0b6217192f8e', hybrid_degradation_reading__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('454b2175-88c5-4cba-bceb-0b6217192f8e', foundational, simulation_maintains_procedural_competence).
narrative_ontology:cs_axiom_status(simulation_maintains_procedural_competence, holdable).
narrative_ontology:cs_axiom_grounding('454b2175-88c5-4cba-bceb-0b6217192f8e', simulation_maintains_procedural_competence, empirically_contingent).
narrative_ontology:cs_axiom('454b2175-88c5-4cba-bceb-0b6217192f8e', foundational, tacit_knowledge_degrades_without_real_catastrophes).
narrative_ontology:cs_axiom_status(tacit_knowledge_degrades_without_real_catastrophes, holdable).
narrative_ontology:cs_axiom_grounding('454b2175-88c5-4cba-bceb-0b6217192f8e', tacit_knowledge_degrades_without_real_catastrophes, empirically_contingent).
narrative_ontology:cs_axiom('454b2175-88c5-4cba-bceb-0b6217192f8e', secondary, stress_response_capacity_requires_embodied_experience).
narrative_ontology:cs_axiom_status(stress_response_capacity_requires_embodied_experience, holdable).
narrative_ontology:cs_axiom_grounding('454b2175-88c5-4cba-bceb-0b6217192f8e', stress_response_capacity_requires_embodied_experience, empirically_contingent).
narrative_ontology:cs_reference_frame('454b2175-88c5-4cba-bceb-0b6217192f8e', simulation_as_sufficient_proxy).
narrative_ontology:cs_drift_state('454b2175-88c5-4cba-bceb-0b6217192f8e', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('454b2175-88c5-4cba-bceb-0b6217192f8e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, regulatory_compliance_apparatus).
narrative_ontology:constraint_victim(hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(hybrid_degradation_reading, tacit_knowledge_transmission).
narrative_ontology:constraint_victim(hybrid_degradation_reading, stress_response_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, operators).
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, training_organizations).
narrative_ontology:constraint_victim(hybrid_degradation_reading, operators).
narrative_ontology:constraint_victim(hybrid_degradation_reading, training_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers simulation-based training standards. Captures recurring revenue from certification mandates. Has exit options: can shift to new simulation technologies, expand into adjacent domains, adjust curriculum offerings. Experiences the constraint as pure coordination — simulation standardizes training, reduces costs, enables scaling. No incentive to measure tacit knowledge degradation because doing so would undermine the simulation-as-proxy narrative.
narrative_ontology:constraint_stakeholder(hybrid_degradation_reading, certification_training_industry, agenda_setter,
    institutional, immediate, arbitrage, global).

% Enforces simulation-based certification requirements. Benefits from measurable, auditable compliance metrics. Has exit options: can adjust certification standards, adopt new simulation technologies, modify regulatory frameworks. Experiences the constraint as pure coordination — simulation provides standardized, verifiable compliance. No structural incentive to measure tacit knowledge degradation.
narrative_ontology:constraint_stakeholder(hybrid_degradation_reading, regulatory_compliance_apparatus, agenda_setter,
    institutional, immediate, arbitrage, global).

% Undergo simulation-based certification training. Gain procedural competence and standardized credentials. Lose access to stress-response development through real-world experience. Constrained by certification mandates — cannot exit without losing employment. Face deferred extraction risk: in real catastrophes, lack embodied intuition and pattern recognition that simulation cannot teach. Mixed position: benefit from procedural competence, pay through lost stress-response capacity.
narrative_ontology:constraint_stakeholder(hybrid_degradation_reading, operators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hybrid_degradation_reading, operators, beneficiary).

% Abstract collective good representing embodied expertise, intuition, and pattern recognition developed through real-world experience. Degrades silently across generational handoffs as operators trained via simulation-only lose access to stress-response development. No mechanism for detecting or recovering lost tacit knowledge. Trapped: cannot exit the simulation-substitution trap. Bears full cost of knowledge degradation.
narrative_ontology:constraint_stakeholder(hybrid_degradation_reading, tacit_knowledge_commons, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(hybrid_degradation_reading, tacit_knowledge_commons).

% Abstract safety buffer representing the difference between actual operator capacity and catastrophe threshold. Decays silently as tacit knowledge degrades across generational handoffs. No self-correction mechanism: procedural competence metrics show no decline, masking the safety deficit. Trapped: cannot exit the certification regime. Extraction deferred until catastrophe reveals the deficit.
narrative_ontology:constraint_stakeholder(hybrid_degradation_reading, long_term_safety_margins, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(hybrid_degradation_reading, long_term_safety_margins).

% Deliver simulation-based training under regulatory mandates. Benefit from simulation access (lower cost than real-world training, standardized curricula, measurable competency gates). Constrained by regulatory requirements and operator demand. Lose pedagogical depth: cannot teach stress-response capacity through simulation alone. Mixed position: gain revenue stability and standardized curricula, pay through reduced training effectiveness.
narrative_ontology:constraint_stakeholder(hybrid_degradation_reading, training_organizations, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hybrid_degradation_reading, training_organizations, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardize operator training across organizations and jurisdictions, reduce training costs, enable scaling of certification programs, provide measurable and auditable competency gates.
% TRANSFER_FUNCTION: Simulation-based training transfers procedural knowledge (what operators know) from training organizations to operators. Certification credentials transfer from operators to employers and regulators. Revenue transfers from operators/employers to certification industry. Tacit knowledge and stress-response capacity transfer away from operators — they lose access to embodied expertise development.
% ABSENT_VOICES: Safety researchers and operators who have experienced real catastrophes are largely absent from the design of certification standards. Their voices would emphasize the importance of stress-response capacity and tacit knowledge. Operators in crisis conditions are absent from the regulatory process — they cannot advocate for training regimes that develop embodied expertise.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight (simulation-based certification mandates were eliminated), the world would rearrange significantly. Training organizations would need to develop real-world training capacity. Operators would gain access to stress-response development. Regulatory frameworks would need to shift to measuring tacit knowledge and stress-response capacity. The certification industry would lose recurring revenue. However, the rearrangement would be costly and disruptive — organizations have invested in simulation infrastructure, curricula are standardized around simulation, and regulatory frameworks are built on simulation-based metrics.
% FOUNDING_PROBLEM: High-reliability organizations needed to scale operator training across multiple sites and jurisdictions while maintaining consistent competency standards. Real-world training was expensive, location-dependent, and difficult to standardize. Simulation offered a solution: lower cost, standardized curricula, measurable competency gates, scalable across organizations.
% FOUNDING_PROBLEM_CORROBORATION: Senior operators and safety researchers (outside the certification industry) attest that the original scaling problem has been solved. Modern organizations have sufficient training capacity. The persistence of simulation-based certification is now driven by beneficiary incentives (certification industry revenue, regulatory compliance metrics) rather than by the original coordination problem.
narrative_ontology:disappearance_verdict(hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hybrid_degradation_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TACIT KNOWLEDGE COMMONS (SNARE) — Cannot exit the simulation-substitution trap. Stress-response capacity and embodied expertise degrade silently across generational handoffs. No mechanism for detecting or recovering lost tacit knowledge. Maximum extraction: the commons bears full cost of degradation while certification metrics show no decline.
constraint_indexing:constraint_classification(hybrid_degradation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPERATORS IN CRISIS CONDITIONS (SNARE) — Trapped by certification systems that validate procedural knowledge but not stress-response capacity. When real catastrophes occur, operators lack the embodied intuition and pattern recognition that simulation cannot teach. No exit from the certification regime; no alternative pathway to develop tacit knowledge. Extraction is deferred until catastrophe, then catastrophic.
constraint_indexing:constraint_classification(hybrid_degradation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: TRAINING ORGANIZATIONS AND OPERATORS (TANGLED ROPE) — Constrained by regulatory mandates requiring simulation-based certification. Also benefit from simulation access (lower cost than real-world training, standardized curricula, measurable competency gates). Moderate extraction: the constraint both enables and constrains their work. Operators gain procedural competence but lose access to stress-response development. Training organizations gain revenue stability but lose pedagogical depth.
constraint_indexing:constraint_classification(hybrid_degradation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CERTIFICATION AND TRAINING INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: simulation-based certification creates recurring revenue streams, standardized curricula, and measurable compliance. Net beneficiary with arbitrage options (can shift to new simulation technologies, expand into adjacent domains). Extraction runs toward this agent; they have agency and exit options.
constraint_indexing:constraint_classification(hybrid_degradation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE APPARATUS (ROPE) — Secondary beneficiary. Experiences the constraint as coordination: simulation-based certification provides measurable, auditable compliance metrics. Regulators can verify training completion, standardize curricula across operators, and document due diligence. Net beneficiary with arbitrage options (can adjust certification standards, adopt new simulation technologies). Extraction runs toward this agent.
constraint_indexing:constraint_classification(hybrid_degradation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REAL-WORLD CATASTROPHE PREVENTION MANDATE (PITON) — The original mandate (prevent catastrophes through operator competence) persists as institutional theater. Simulation-based certification is maintained as the primary competence-building mechanism despite evidence that procedural knowledge alone is insufficient for stress-response capacity. The mandate is degraded: certification metrics show competence while actual safety margins decay. Piton classification derives from the theater gate — the constraint maintains the appearance of catastrophe prevention while the actual mechanism (tacit knowledge transmission) atrophies.
constraint_indexing:constraint_classification(hybrid_degradation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits genuine coordination (simulation standardizes training, reduces costs, enables scaling) alongside genuine extraction (tacit knowledge and stress-response capacity degrade silently, creating hidden safety deficits that manifest only in catastrophes). The constraint is not a false summit — the coordination function is real. But the extraction mechanism is also real and structurally embedded: the beneficiaries (certification industry, regulators) have no incentive to measure or report tacit knowledge degradation because doing so would undermine the simulation-as-proxy narrative.
constraint_indexing:constraint_classification(hybrid_degradation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): Moderate-high and rising. The constraint solves a genuine coordination problem (simulation standardizes training, reduces costs, enables scaling) but extracts through a hidden mechanism (tacit knowledge degradation). The extractiveness is not as severe as a pure snare (0.75+) because the coordination function is real and valuable. But it is higher than a pure rope (0.15) because the extraction mechanism is structurally embedded and invisible. The rising trajectory (0.35 → 0.68 over 30 years) reflects that tacit knowledge degradation accumulates across generational handoffs — each cohort trained via simulation-only loses more embodied expertise than the previous cohort, compounding the deficit. Suppression (0.62): Moderate-high. Significant barriers to detecting and addressing the constraint include: (1) procedural competence metrics show no decline, masking tacit knowledge loss; (2) catastrophes are rare, so the extraction mechanism remains latent; (3) beneficiaries have structural incentives not to measure tacit knowledge; (4) operators cannot exit the certification regime without losing employment. Theater ratio (0.68): High and rising. Simulation-based certification is substantially performative: it measures procedural knowledge (what operators know) but not stress-response capacity (how operators perform under extreme stress). The theater increases over time as simulation technologies improve at measuring procedural competence while tacit knowledge degradation accelerates. The constraint maintains the appearance of competence while the actual mechanism (embodied expertise transmission) atrophies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival gap between beneficiaries and victims. The certification industry and regulators see pure coordination (Rope) — simulation standardizes training, reduces costs, enables scaling. Operators see mixed coordination and extraction (Tangled Rope) — they gain procedural competence but lose stress-response development. The tacit knowledge commons and long-term safety margins see pure extraction (Snare) — they bear the full cost of degradation with no benefit. The real-world catastrophe prevention mandate sees its own degradation (Piton) — the original mandate persists as institutional theater while the actual mechanism (tacit knowledge transmission) atrophies. The analytical observer sees the constraint as a genuine tangled rope with real coordination function and real extraction mechanism — the perspectival gap reveals that the beneficiaries' rope classification is partial (they see the coordination benefit but not the extraction cost) while the victims' snare classification is also partial (they see the extraction but not the coordination benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the extraction flow. Beneficiaries (certification industry, regulators) with arbitrage options experience low or negative effective extraction — they collect from the constraint. Victims (operators, tacit knowledge commons, safety margins) with trapped or constrained exit experience high effective extraction — they bear the cost. Operators with constrained exit (can exit but at career cost) experience moderate extraction. The analytical observer with analytical exit experiences the constraint as a genuine tangled rope — the coordination function is real (simulation does standardize training and reduce costs) but the extraction mechanism is also real (tacit knowledge degrades silently). The directionality derivation chain produces d values that reflect this structure: beneficiaries get low d (0.1-0.3), victims get high d (0.7-0.9), mixed actors get moderate d (0.4-0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the original mandate (prevent catastrophes through operator competence) has outlived its function as a guide to actual safety. Simulation-based certification maintains the appearance of competence while the actual mechanism (tacit knowledge transmission) atrophies. The mandate persists as institutional theater because the beneficiaries (certification industry, regulators) have structural incentives to maintain the simulation-as-proxy narrative. The constraint is not resolved mandatrophy — the mandate still drives institutional behavior — but it is degraded mandatrophy where the mandate's original purpose (catastrophe prevention) is no longer served by the mechanism (simulation-based training). The analytical observer's tangled rope classification reveals this: the constraint solves a coordination problem (standardizing training) but extracts through a hidden mechanism (tacit knowledge degradation) that undermines the original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_measurability,
    'Can tacit knowledge and stress-response capacity be measured independently of catastrophe outcomes, or are they inherently latent until crisis reveals them?',
    'Longitudinal studies comparing operators trained via simulation vs. mixed simulation/real-world training; measurement of stress-response capacity via high-fidelity scenario testing with physiological markers; post-incident analysis of operator performance in real catastrophes',
    'If measurable: the constraint''s extraction becomes visible and can be quantified. If inherently latent: the constraint''s extraction is structurally undetectable until catastrophe, making it a pure snare from the safety-margins perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_measurability, empirical, 'Whether tacit knowledge degradation is measurable before catastrophe').

omega_variable(
    simulation_fidelity_ceiling,
    'Is there a fidelity threshold above which simulation can transmit stress-response capacity, or is embodied experience in real-world conditions a necessary component?',
    'Comparative analysis of operator performance in real catastrophes across cohorts trained at different simulation fidelity levels; neurobiological studies of stress-response learning in simulated vs. real-world conditions; longitudinal tracking of operators transitioning from simulation-only to real-world operations',
    'If threshold exists and is achievable: simulation can be improved to reduce extraction. If embodied experience is necessary: the constraint is structurally irreducible and the extraction is permanent unless real-world training is restored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Whether simulation fidelity can transmit stress-response capacity').

omega_variable(
    generational_knowledge_loss_rate,
    'What is the rate of tacit knowledge loss per generational handoff (20-30 years) in simulation-only training regimes, and at what loss rate does catastrophe risk become unacceptable?',
    'Historical analysis of operator cohorts across multiple generational transitions; measurement of procedural competence vs. stress-response capacity across cohorts; correlation between knowledge-loss rate and near-miss or catastrophe frequency',
    'If loss rate is low (<5% per generation): extraction is manageable and the constraint remains tangled rope. If loss rate is high (>20% per generation): extraction becomes severe and the constraint approaches snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_loss_rate, empirical, 'Rate of tacit knowledge loss across generational handoffs').

omega_variable(
    kernel_reading_ambiguity,
    'Is simulation a sufficient proxy for catastrophe prevention (simulation_as_proxy_catastrophe_reading), or does catastrophe prevention require real-world experience (catastrophe_necessity_reading), or is the truth a hybrid where simulation maintains procedural competence while tacit knowledge decays (this reading)?',
    'Post-incident analysis of catastrophes: did they occur in operators trained via simulation-only? Did they occur in operators with mixed training? What was the distribution of procedural vs. stress-response failures? Longitudinal comparison of safety records across training regimes.',
    'If simulation is sufficient: this reading is foreclosed and the constraint is rope. If catastrophe requires real-world experience: this reading coexists with catastrophe_necessity_reading as competing frameworks. If hybrid is correct: this reading is the accurate structural description and the other readings are partial framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether simulation is a sufficient proxy for catastrophe prevention or whether hybrid degradation is the accurate structural description').

omega_variable(
    beneficiary_incentive_alignment,
    'Do the beneficiaries (certification industry, regulators) have structural incentives to measure and report tacit knowledge degradation, or do their revenue/compliance models depend on not measuring it?',
    'Analysis of certification standards and regulatory frameworks: do they include metrics for tacit knowledge or stress-response capacity? Historical review of regulatory changes: have standards been tightened or loosened over time? Interview analysis of industry actors: what would happen to their business models if tacit knowledge degradation were measured and reported?',
    'If beneficiaries have incentives to measure: the constraint''s extraction may be self-correcting. If beneficiaries have incentives NOT to measure: the constraint is structurally extractive and will persist until external pressure (catastrophe) forces change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_incentive_alignment, empirical, 'Whether beneficiaries have incentives to measure tacit knowledge degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_degradation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_deg_tr_t0, hybrid_degradation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hybrid_deg_tr_t10, hybrid_degradation_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(hybrid_deg_tr_t20, hybrid_degradation_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement(hybrid_deg_tr_t30, hybrid_degradation_reading, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(hybrid_deg_be_t0, hybrid_degradation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hybrid_deg_be_t10, hybrid_degradation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hybrid_deg_be_t20, hybrid_degradation_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hybrid_deg_be_t30, hybrid_degradation_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_deg_su_t0, hybrid_degradation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hybrid_deg_su_t10, hybrid_degradation_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(hybrid_deg_su_t20, hybrid_degradation_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(hybrid_deg_su_t30, hybrid_degradation_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The hybrid_degradation_reading is one reading of the contested kernel catastrophe_proxy_sufficiency. The sibling readings (simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading, simulation_fidelity_threshold) are structurally distinct constraints with different ε values and beneficiary/victim structures. The hybrid_degradation_reading coexists with these siblings as competing frameworks held by different parties in the high-reliability organization domain. The network edges represent the structural relationships between readings: this reading influences the others by providing empirical evidence of the hybrid mechanism (procedural competence maintained, tacit knowledge degraded).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_degradation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
