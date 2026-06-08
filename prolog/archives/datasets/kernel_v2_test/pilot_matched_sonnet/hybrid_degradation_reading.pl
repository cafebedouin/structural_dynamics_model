% ============================================================================
% CONSTRAINT STORY: hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_degradation_reading
 *   human_readable: Hybrid Degradation in Catastrophe-Free Safety Training
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The hybrid degradation reading of the catastrophe_proxy_sufficiency
 *   kernel holds that simulation-based safety training maintains procedural
 *   competence (the visible, measurable component) while tacit knowledge and
 *   stress-response capacity degrade invisibly over generational timescales.
 *   This reading sees the constraint as a tangled rope: genuine coordination
 *   function (maintaining baseline competence without exposing personnel to
 *   real catastrophes) coexisting with structural extraction (hidden erosion
 *   of safety margins that only manifests when a real catastrophe occurs).
 *   The certification industry benefits from ongoing training revenue;
 *   long-term safety margins bear the cost. The degradation is structurally
 *   hidden because it operates at generational timescales (25+ years) while
 *   organizational decision-making operates at biographical timescales
 *   (career horizons of 5-15 years). Theater ratio (0.58) reflects that
 *   simulation hours have become partly performative: regulatory requirements
 *   continue to increase even as simulation fidelity has plateaued, and
 *   compliance auditors recognize that hour-counting has replaced
 *   effectiveness measurement. This reading coexists with the
 *   simulation_as_proxy_catastrophe_reading (which holds that sufficient
 *   simulation fidelity eliminates the degradation) and the
 *   catastrophe_necessity_reading (which holds that real catastrophes are
 *   irreplaceable for maintaining organizational resilience). The three
 *   readings are held by different institutional actors with different
 *   structural relationships to the constraint.
 *
 * KEY AGENTS:
 *   - Certification Industry: Primary beneficiary (institutional/arbitrage) — captures ongoing revenue from simulation-based recertification cycles; experiences constraint as coordination
 *   - Simulation Technology Vendors: Secondary beneficiary (institutional/arbitrage) — benefits from regulatory mandates requiring simulation hours
 *   - Compliance Auditors: Tertiary beneficiary (institutional/constrained) — maintains employment through audit cycles, but recognizes performative nature of hour-counting
 *   - Long-Term Safety Margins: Primary victim (powerless/trapped) — abstract collective good that cannot exit or organize; degradation invisible at biographical timescales
 *   - Tacit Knowledge Transmission: Secondary victim (powerless/trapped) — intergenerational knowledge transfer mechanism that atrophies without real-catastrophe experience
 *   - Stress-Response Capacity: Tertiary victim (powerless/trapped) — organizational ability to handle novel high-stress scenarios degrades without real exposure
 *   - Safety Training Personnel: Mixed position (moderate/constrained) — experience both coordination benefit (career structure, standardized protocols) and extraction (awareness of simulation limitations)
 *   - HRO Research Community: Organized agents (organized/mobile) — developing next-generation methodologies with sunset logic (augmented reality stress inoculation, synthetic catastrophe generation)
 *   - Legacy Regulatory Framework: Institutional actor (institutional/constrained) — maintains performative hour requirements through inertia (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_degradation_reading, 0.48).
domain_priors:suppression_score(hybrid_degradation_reading, 0.62).
domain_priors:theater_ratio(hybrid_degradation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_degradation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hybrid_degradation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hybrid_degradation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_degradation_reading, "Hybrid Degradation in Catastrophe-Free Safety Training").
narrative_ontology:topic_domain(hybrid_degradation_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_degradation_reading, '180479f4-ee14-4b97-ab09-329f81df36a7').
narrative_ontology:cs_kernel_codification('180479f4-ee14-4b97-ab09-329f81df36a7', distributed).
narrative_ontology:cs_authority_grounding('180479f4-ee14-4b97-ab09-329f81df36a7', distributed).
narrative_ontology:cs_reading_relation('180479f4-ee14-4b97-ab09-329f81df36a7', hybrid_degradation_reading__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('180479f4-ee14-4b97-ab09-329f81df36a7', hybrid_degradation_reading__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('180479f4-ee14-4b97-ab09-329f81df36a7', hybrid_degradation_reading__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('180479f4-ee14-4b97-ab09-329f81df36a7', foundational, tacit_knowledge_requires_real_stress).
narrative_ontology:cs_axiom_status(tacit_knowledge_requires_real_stress, holdable).
narrative_ontology:cs_axiom_grounding('180479f4-ee14-4b97-ab09-329f81df36a7', tacit_knowledge_requires_real_stress, empirically_contingent).
narrative_ontology:cs_axiom('180479f4-ee14-4b97-ab09-329f81df36a7', foundational, procedural_competence_simulation_sufficient).
narrative_ontology:cs_axiom_status(procedural_competence_simulation_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('180479f4-ee14-4b97-ab09-329f81df36a7', procedural_competence_simulation_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('180479f4-ee14-4b97-ab09-329f81df36a7', secondary, generational_degradation_invisible_biographically).
narrative_ontology:cs_axiom_status(generational_degradation_invisible_biographically, holdable).
narrative_ontology:cs_axiom_grounding('180479f4-ee14-4b97-ab09-329f81df36a7', generational_degradation_invisible_biographically, empirically_contingent).
narrative_ontology:cs_reference_frame('180479f4-ee14-4b97-ab09-329f81df36a7', real_catastrophe_baseline).
narrative_ontology:cs_drift_state('180479f4-ee14-4b97-ab09-329f81df36a7', contemporary_simulation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('180479f4-ee14-4b97-ab09-329f81df36a7', '').
narrative_ontology:cs_kernel_id(hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, certification_industry).
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(hybrid_degradation_reading, compliance_auditors).
narrative_ontology:constraint_victim(hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(hybrid_degradation_reading, tacit_knowledge_transmission).
narrative_ontology:constraint_victim(hybrid_degradation_reading, stress_response_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LONG-TERM SAFETY MARGINS (SNARE) — The abstract collective good of organizational resilience cannot exit the degradation dynamic. Trapped at generational timescale because the decay is invisible within biographical horizons. Maximum extraction: the constraint extracts safety capacity while appearing to maintain it.
constraint_indexing:constraint_classification(hybrid_degradation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY TRAINING PERSONNEL (TANGLED ROPE) — Constrained by regulatory requirements and organizational mandates. Experience genuine coordination benefit (standardized training protocols, career structure) alongside extraction (awareness that simulation cannot fully substitute for real experience, but no alternative pathway exists). Mixed experience: the system both enables their work and limits its effectiveness.
constraint_indexing:constraint_classification(hybrid_degradation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CERTIFICATION INDUSTRY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: simulation-based certification solves the legitimate problem of maintaining workforce competence without exposing personnel to real catastrophes. Captures ongoing revenue from recertification cycles. Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(hybrid_degradation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: HRO RESEARCH COMMUNITY (SCAFFOLD) — Organized researchers developing next-generation training methodologies see current simulation-based approaches as transitional. Active work on augmented reality stress inoculation, cross-organizational knowledge transfer protocols, and synthetic catastrophe generation suggests a sunset: hybrid approaches combining simulation with controlled real-world stressors. Estimated timeline: 15-25 years for new methodologies to achieve regulatory acceptance.
constraint_indexing:constraint_classification(hybrid_degradation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REGULATORY FRAMEWORK (PITON) — Compliance auditors recognize that simulation-hour requirements have become largely performative. The original function (ensuring competence through practice) has atrophied as simulation fidelity plateaued while regulatory hour requirements continued to increase. Maintained through institutional inertia and liability management rather than demonstrated effectiveness.
constraint_indexing:constraint_classification(hybrid_degradation_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint exhibits genuine coordination function (maintaining baseline procedural competence without catastrophic training costs) alongside structural extraction (hidden degradation of tacit knowledge and stress-response capacity that only manifests at generational timescales). The coordination is real; the extraction is also real. This is the claimed type.
constraint_indexing:constraint_classification(hybrid_degradation_reading, tangled_rope,
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
 *   Extractiveness (0.48): Moderate-high. The certification industry and simulation vendors extract ongoing revenue while long-term safety margins degrade invisibly. The extraction is substantial but not maximal because the coordination function is genuine: simulation does maintain procedural competence, preventing the immediate safety collapse that would occur with no training at all. The value reflects that roughly half of the constraint's operation is extractive (hidden degradation) and half is coordinative (maintained baseline competence). Suppression (0.62): Moderate-high and increasing. Regulatory mandates create high barriers to alternative approaches. Organizations cannot opt out of simulation-based training without losing certification. The suppression has increased over the interval as regulatory requirements have become more prescriptive and simulation-hour minimums have risen. Real-catastrophe-based training is legally and ethically prohibited in most safety-critical domains. Career risk for safety personnel who question simulation sufficiency is substantial. Theater ratio (0.58): Moderate-high and increasing. Simulation hour requirements have become partly performative as simulation fidelity plateaued while regulatory minimums continued to rise. Compliance auditors count hours rather than assess effectiveness. The theater has increased over the interval as the gap between regulatory requirements and demonstrated training effectiveness has widened. However, theater is not total: simulation does provide some genuine training value, particularly for procedural competence.
 *
 * PERSPECTIVAL GAP:
 *   The certification industry sees pure coordination (Rope): simulation solves the legitimate problem of maintaining competence without catastrophic training costs. Safety training personnel see mixed coordination and extraction (Tangled Rope): the system both enables their work and limits its effectiveness. Long-term safety margins see pure extraction (Snare): the constraint extracts safety capacity invisibly over generational timescales. The HRO research community sees a temporary problem with a sunset (Scaffold): next-generation methodologies will address the degradation. The legacy regulatory framework sees its own degraded ritual (Piton): hour-counting has replaced effectiveness measurement. The analytical observer sees tangled rope at civilizational scale: genuine coordination coexisting with structural extraction. The perspectival gap is diagnostic: beneficiaries with immediate time horizons see coordination; victims with generational time horizons see extraction; organized agents with mobile exit see a solvable problem; the analytical observer sees both functions operating simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification industry, simulation vendors, and compliance auditors are beneficiaries with arbitrage or constrained exit options. They experience low effective extraction (negative chi for certification industry, near-zero for vendors, low-positive for auditors who recognize the performative nature but depend on the system). Safety training personnel are in a mixed position: moderate power, constrained exit, appearing in both beneficiary (career structure) and victim (awareness of limitations) categories. The engine will derive moderate positive d, reflecting their ambiguous structural position. Long-term safety margins, tacit knowledge transmission, and stress-response capacity are powerless victims with trapped exit options at generational timescales. They experience maximum extraction: the constraint extracts safety capacity while appearing to maintain it, and the extraction is invisible until catastrophic failure reveals it. The HRO research community has organized power and mobile exit options, experiencing low extraction because they see a pathway out (next-generation methodologies). The legacy regulatory framework has institutional power but constrained exit (cannot easily revise established hour requirements), experiencing moderate extraction as it maintains a partly performative system.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is not a compromise classification but a structural reality: the coordination function (maintaining procedural competence) and the extraction mechanism (hidden degradation of tacit knowledge) are both genuinely present and operate simultaneously. The constraint is not 'partly rope and partly snare' — it is fully both, from different perspectives and at different timescales. The certification industry's rope experience is their genuine structural reality (they are net beneficiaries). The safety margins' snare experience is also genuine structural reality (they are net victims). The analytical observer's tangled rope classification captures that both functions are real and that the constraint requires active enforcement (regulatory mandates) to persist in this hybrid state. Without enforcement, organizations would either revert to real-catastrophe-based learning (if legally permitted) or accept visible competence degradation (if simulation were voluntary). The enforcement maintains the hybrid: visible competence without visible catastrophes, invisible degradation without visible failures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tacit_knowledge_measurement,
    'Can tacit knowledge degradation be measured before catastrophic failure reveals it, or is the degradation only detectable retrospectively?',
    'Development of proxy metrics for tacit knowledge retention: decision-making speed under novel conditions, pattern recognition in ambiguous scenarios, cross-generational knowledge transfer success rates. Longitudinal studies comparing organizations with recent real-catastrophe experience vs. simulation-only training.',
    'If measurable prospectively: the constraint becomes manageable through monitoring and intervention (shifts toward Rope from more perspectives). If only retrospectively detectable: the extraction mechanism is structurally hidden until failure (confirms Snare from safety margins perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_measurement, empirical, 'Whether tacit knowledge degradation can be prospectively measured').

omega_variable(
    simulation_fidelity_ceiling,
    'Has simulation technology reached a fundamental fidelity ceiling, or are current limitations merely engineering problems awaiting solution?',
    'Analysis of simulation improvement trajectories over past 30 years; identification of theoretical vs. practical barriers to stress-response replication; comparison with other domains where simulation has successfully substituted for real experience (aviation, surgery).',
    'If fundamental ceiling: the hybrid degradation is inherent to simulation-based training (supports this reading''s core claim). If engineering problem: sufficient investment could eliminate the degradation mechanism (supports simulation_as_proxy_catastrophe_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Whether simulation fidelity limitations are fundamental or surmountable').

omega_variable(
    generational_threshold_ambiguity,
    'At what generational distance does tacit knowledge degradation become safety-critical? One generation (25 years)? Two? Three?',
    'Historical analysis of high-reliability organizations: correlation between time-since-last-real-catastrophe and subsequent failure rates; identification of inflection points where degradation becomes measurable in safety outcomes.',
    'If threshold < 25 years: many current organizations are already in degraded state (urgent intervention required). If threshold > 75 years: the constraint is less extractive than assessed (more time for corrective mechanisms to operate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_threshold_ambiguity, empirical, 'Generational timescale at which degradation becomes safety-critical').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of the catastrophe_proxy_sufficiency kernel, where different parties hold structurally distinct positions on whether simulation can substitute for real catastrophes?',
    'Cross-reading analysis: does the simulation_as_proxy_catastrophe_reading (simulation as sufficient proxy) produce a structurally different constraint with different beneficiaries and different ε? Does the catastrophe_necessity_reading (real catastrophes as irreplaceable) produce yet another distinct constraint? If yes, these are sibling readings of a contested kernel. If no, they are merely different opinions about the same constraint.',
    'If genuine kernel: the disagreement is located in foundational axioms about knowledge transmission and stress-response development, not in empirical facts. Different readings will persist as live positions held by different institutional actors (certification industry vs. HRO researchers vs. safety-critical operators). If not a kernel: the disagreement is empirical and resolvable through better measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this is one reading of a contested kernel or an empirical disagreement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_degradation_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_deg_tr_t0, hybrid_degradation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hybrid_deg_tr_t8, hybrid_degradation_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(hybrid_deg_tr_t16, hybrid_degradation_reading, theater_ratio, 16, 0.52).
narrative_ontology:measurement(hybrid_deg_tr_t24, hybrid_degradation_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(hybrid_deg_be_t0, hybrid_degradation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hybrid_deg_be_t8, hybrid_degradation_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(hybrid_deg_be_t16, hybrid_degradation_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(hybrid_deg_be_t24, hybrid_degradation_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_deg_su_t0, hybrid_degradation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hybrid_deg_su_t8, hybrid_degradation_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(hybrid_deg_su_t16, hybrid_degradation_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(hybrid_deg_su_t24, hybrid_degradation_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_degradation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hybrid_degradation_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(hybrid_degradation_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(hybrid_degradation_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The catastrophe_proxy_sufficiency kernel decomposes into at least four structurally distinct readings with different ε values, different beneficiary/victim structures, and different institutional advocates. The hybrid_degradation_reading (this constraint) has moderate-high extractiveness (0.48) reflecting hidden degradation alongside genuine coordination. The simulation_as_proxy_catastrophe_reading has low extractiveness (genuine coordination with engineering challenges but no structural extraction). The catastrophe_necessity_reading has high extractiveness (simulation as pure theater masking competence collapse). The simulation_fidelity_threshold reading has variable extractiveness depending on whether current technology is above or below the threshold. These are not different measurements of the same constraint — they are different constraints instantiated by different institutional commitments to different axioms about knowledge transmission and stress-response development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
