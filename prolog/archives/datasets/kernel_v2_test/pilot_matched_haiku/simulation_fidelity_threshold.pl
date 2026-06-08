% ============================================================================
% CONSTRAINT STORY: simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_fidelity_threshold, []).

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
 *   constraint_id: simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   Competence retention in high-reliability organizations (nuclear plants,
 *   commercial aviation, military operations) depends on operators
 *   maintaining skills in domains where real-world catastrophe is too costly
 *   to use as a training mechanism. The fidelity threshold constraint solves
 *   a genuine coordination problem: how to ensure operators maintain
 *   competence without exposing them to actual catastrophic risk. This
 *   reading asserts that simulation crossing a fidelity threshold IS
 *   SUFFICIENT for competence retention — the threshold is a coordination
 *   mechanism that vendors, regulators, and organizations use to define what
 *   'competent' means. However, the threshold is technology-dependent: as
 *   simulation technology improves, the threshold can shift, and vendors have
 *   structural incentives to set thresholds high enough to maximize their
 *   market. The constraint exhibits Rope classification from the vendor and
 *   analytical perspectives (genuine coordination), Tangled Rope from the
 *   regulatory and organizational perspectives (mixed coordination and
 *   extraction), Snare from the frontline operator perspective (trapped in a
 *   system they cannot control), and Piton from the certification ritual
 *   perspective (increasingly performative). The extractiveness has increased
 *   over the interval (0.35 → 0.52) as vendors have invested in
 *   higher-fidelity simulation and regulators have raised threshold
 *   standards, creating a ratchet effect where operators must continuously
 *   invest in training to maintain certification. Theater ratio has also
 *   increased (0.22 → 0.45) as the certification process has become more
 *   elaborate and the causal link between fidelity threshold crossing and
 *   actual competence retention has become more contested.
 *
 * KEY AGENTS:
 *   - Frontline Operator: Primary victim (powerless/trapped) — must maintain competence through expensive, vendor-controlled simulation; cannot exit the requirement or access alternatives
 *   - Simulation Technology Vendor: Primary beneficiary (institutional/arbitrage) — benefits from fidelity threshold as a coordination mechanism that creates legitimate demand; has agency to improve technology and set standards
 *   - Regulatory Authority: Secondary beneficiary (organized/constrained) — coordinates safety requirements through the threshold; benefits from measurable standard but constrained by technology-dependence
 *   - Training Infrastructure Operator: Secondary beneficiary (moderate/constrained) — operates simulation facilities; benefits from threshold as justification for capital investment
 *   - Safety-Critical Organization: Mixed beneficiary/victim (powerful/mobile) — coordinates safety requirements but also constrained by technology-dependence and regulatory requirements
 *   - Certification Ritual: Institutional actor (institutional/arbitrage) — maintains performative compliance process; theater ratio increasing as causal claim becomes contested
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function but also sees technology-dependence creating extraction risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_fidelity_threshold, 0.52).
domain_priors:suppression_score(simulation_fidelity_threshold, 0.48).
domain_priors:theater_ratio(simulation_fidelity_threshold, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_fidelity_threshold, extractiveness, 0.52).
narrative_ontology:constraint_metric(simulation_fidelity_threshold, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(simulation_fidelity_threshold, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(simulation_fidelity_threshold, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_fidelity_threshold, 'b372d9d3-dac2-46d6-9061-b2cdb896c170').
narrative_ontology:cs_kernel_codification('b372d9d3-dac2-46d6-9061-b2cdb896c170', formalized).
narrative_ontology:cs_authority_grounding('b372d9d3-dac2-46d6-9061-b2cdb896c170', extraction).
narrative_ontology:cs_interpretation_layer_present('b372d9d3-dac2-46d6-9061-b2cdb896c170').
narrative_ontology:cs_reading_relation('b372d9d3-dac2-46d6-9061-b2cdb896c170', simulation_fidelity_threshold__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('b372d9d3-dac2-46d6-9061-b2cdb896c170', simulation_fidelity_threshold__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b372d9d3-dac2-46d6-9061-b2cdb896c170', simulation_fidelity_threshold__hybrid_degradation_reading, influences).
narrative_ontology:cs_axiom('b372d9d3-dac2-46d6-9061-b2cdb896c170', foundational, simulation_fidelity_sufficiency).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('b372d9d3-dac2-46d6-9061-b2cdb896c170', simulation_fidelity_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('b372d9d3-dac2-46d6-9061-b2cdb896c170', secondary, technology_dependent_threshold_legitimacy).
narrative_ontology:cs_axiom_status(technology_dependent_threshold_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b372d9d3-dac2-46d6-9061-b2cdb896c170', technology_dependent_threshold_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('b372d9d3-dac2-46d6-9061-b2cdb896c170', simulation_as_competence_proxy).
narrative_ontology:cs_drift_state('b372d9d3-dac2-46d6-9061-b2cdb896c170', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b372d9d3-dac2-46d6-9061-b2cdb896c170', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(simulation_fidelity_threshold, training_infrastructure_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped in a system where competence retention depends on access to high-fidelity simulation that is expensive, geographically concentrated, and controlled by vendors. Cannot exit the requirement to maintain competence; cannot afford or access the simulation infrastructure independently. Experiences the threshold as an immutable gate: below threshold = incompetence, above threshold = competence. No agency in setting the threshold or accessing the technology.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SIMULATION TECHNOLOGY VENDOR (ROPE) — Benefits from the fidelity threshold as a coordination mechanism: the threshold creates legitimate demand for their products and services. The vendor experiences the constraint as solving a genuine coordination problem — how to ensure operators maintain competence in high-stakes domains. The threshold is technology-dependent, which means vendors have arbitrage options: they can invest in fidelity improvements, migrate customers to new platforms, or establish themselves as the standard-setter. Net beneficiary with agency.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (TANGLED ROPE) — Coordinates genuine safety requirements (operators must maintain competence) while extracting legitimacy and control through the fidelity threshold mechanism. The authority benefits from the threshold because it provides a measurable, defensible standard for competence certification. But the authority is also constrained: the threshold is technology-dependent, meaning regulatory authority is partially delegated to vendors who control the technology. Active enforcement required to maintain the threshold as a binding standard.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRAINING INFRASTRUCTURE OPERATOR (ROPE) — Operates the simulation facilities and benefits from the fidelity threshold as a coordination mechanism that justifies capital investment in high-fidelity equipment. The operator experiences the constraint as solving the problem of how to allocate training resources efficiently. Constrained by the need to maintain fidelity standards and by vendor lock-in on simulation technology, but also benefits from the legitimacy the threshold provides for their operations.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: SAFETY-CRITICAL ORGANIZATION (TANGLED ROPE) — Nuclear plant operator, airline, or similar organization that must maintain operator competence. Coordinates genuine safety requirements through the fidelity threshold while also extracting legitimacy and control over training budgets. The organization benefits from the threshold as a defensible standard but is also constrained by the technology-dependence: fidelity improvements require capital investment, and the threshold can shift as technology advances. Mobile enough to shop between vendors or invest in alternative training methods, but constrained by regulatory requirements and safety liability.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CERTIFICATION RITUAL (PITON) — The formal competence certification process based on simulation fidelity thresholds is increasingly performative. The ritual persists through institutional inertia and regulatory requirement, but the actual mechanism by which simulation crossing a fidelity threshold produces competence retention is contested and weakly validated. Theater ratio reflects that certification procedures are maintained as compliance theater even as the underlying causal claim (fidelity → competence → safety) remains empirically uncertain.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the fidelity threshold represents a genuine coordination solution to the problem of maintaining competence in domains where real-world catastrophe is too costly to use as a training mechanism. The constraint solves a real collective-action problem: how to ensure operators maintain skills without exposing them to actual catastrophic risk. The threshold is technology-dependent, which means it is revisable and improvable — not a natural law but a contingent institutional arrangement that serves a coordination function.
constraint_indexing:constraint_classification(simulation_fidelity_threshold, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_fidelity_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_fidelity_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_fidelity_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(simulation_fidelity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(simulation_fidelity_threshold, TR),
    TR >= 0.70.

:- end_tests(simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The constraint extracts from frontline operators through the requirement to maintain expensive simulation-based competence certification. The extraction is not maximal because the constraint does solve a genuine coordination problem — operators genuinely need to maintain competence, and simulation is a legitimate (if imperfect) proxy for catastrophe exposure. However, the technology-dependence creates vendor capture risk: vendors can set thresholds high enough to maximize their market, and operators have limited ability to challenge the threshold or access alternatives. The increasing trajectory (0.35 → 0.52) reflects vendor investment in higher-fidelity simulation and regulatory ratcheting of threshold standards. Suppression (0.48): Moderate. Operators are suppressed by the requirement to maintain expensive certification, by geographic concentration of high-fidelity simulation facilities, and by regulatory barriers to alternative training methods. However, suppression is not total — some operators can access simulation, and some organizations invest in alternative training methods. The increasing trajectory reflects regulatory tightening and vendor consolidation. Theater ratio (0.38): Moderate and increasing. The certification process based on fidelity thresholds is increasingly performative because the causal link between fidelity threshold crossing and actual competence retention is contested and weakly validated. The increasing trajectory reflects the growing gap between the elaborate certification procedures and the uncertain underlying causal claim.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The vendor sees Rope (genuine coordination mechanism that creates legitimate demand). The regulatory authority sees Tangled Rope (mixed coordination and extraction, with active enforcement required). The frontline operator sees Snare (trapped in a system they cannot control). The certification ritual sees Piton (increasingly performative). The analytical observer sees Rope (genuine coordination function) but also sees the technology-dependence creating extraction risk. The perspectival gap reveals that the constraint's classification depends critically on whether one believes the fidelity threshold is a genuine coordination mechanism or a vendor-captured extraction mechanism. This is the core committer-frame ambiguity: the kernel 'catastrophe_proxy_sufficiency' has multiple readings, and this reading asserts that simulation fidelity threshold crossing IS sufficient for competence retention.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Simulation technology vendors are beneficiaries with arbitrage options (d ≈ 0.2) — they can invest in fidelity improvements, migrate customers, or establish themselves as standard-setters. Frontline operators are victims with trapped exit options (d ≈ 0.9) — they must maintain competence through vendor-controlled simulation and cannot exit the requirement. Regulatory authorities are mixed (d ≈ 0.4) — they benefit from the threshold as a measurable standard but are constrained by technology-dependence. Training infrastructure operators are beneficiaries with constrained exit options (d ≈ 0.3) — they benefit from the threshold as justification for capital investment but are constrained by vendor lock-in. Safety-critical organizations are mixed (d ≈ 0.5) — they coordinate safety requirements but are constrained by technology-dependence and regulatory requirements. The effective extraction (χ) is amplified for trapped operators and damped for beneficiaries with arbitrage options. The increasing trajectory of extractiveness reflects that the constraint's extraction mechanism is becoming more effective as vendors consolidate and regulators raise threshold standards.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between the genuine coordination function (maintaining operator competence without catastrophic risk) and the extraction mechanism (vendor capture of threshold-setting). The mandate is to maintain competence; the constraint solves this mandate through simulation fidelity thresholds. However, the technology-dependence creates a secondary extraction mechanism where vendors can set thresholds high enough to maximize their market. The constraint is classified as Rope because the primary function is coordination, but the increasing extractiveness and theater ratio suggest that the extraction mechanism is becoming more effective over time. The mandatrophy is not resolved — the constraint's mandate (maintain competence) is still live, but the extraction mechanism is becoming more salient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_causality,
    'Does crossing a simulation fidelity threshold actually cause competence retention, or does the threshold merely correlate with other factors (selection effects, training intensity, operator motivation) that produce competence?',
    'Randomized controlled trials comparing operators trained above vs. below threshold with matched training intensity and selection criteria; longitudinal tracking of competence decay rates; intervention studies where fidelity is held constant while other factors vary',
    'If causal: the threshold is a genuine coordination mechanism (Rope confirmed). If merely correlational: the threshold is a proxy that may be gamed or substituted (Snare risk increases). If confounded: the threshold is theater (Piton confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_sufficiency_causality, empirical, 'Causal relationship between fidelity threshold crossing and competence retention').

omega_variable(
    threshold_technology_dependence,
    'Is the fidelity threshold inherently technology-dependent, or can it be defined in domain-independent terms that transcend specific simulation platforms?',
    'Analysis of threshold definitions across domains (aviation, nuclear, maritime, military); assessment of whether thresholds transfer between simulation platforms or require recalibration; investigation of whether threshold is defined by measurable physical/cognitive parameters or by vendor-specific capabilities',
    'If inherently technology-dependent: vendors have structural power to set thresholds (extraction risk). If domain-independent: threshold is a genuine coordination standard (Rope confirmed). If hybrid: threshold is partially captured by vendors (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_technology_dependence, empirical, 'Whether fidelity threshold is technology-dependent or domain-independent').

omega_variable(
    catastrophe_proxy_sufficiency_reading_contest,
    'Is simulation crossing a fidelity threshold a SUFFICIENT proxy for catastrophe exposure, or merely a NECESSARY component of competence maintenance?',
    'Comparative analysis of competence retention in operators trained via simulation-only vs. simulation+real-incident exposure vs. real-incident-only; investigation of whether simulation fidelity improvements reduce the need for real catastrophe exposure or merely delay it; longitudinal tracking of whether operators trained above threshold maintain competence without real-incident exposure',
    'If simulation-only is sufficient: this reading (Rope) is correct. If simulation is merely necessary: sibling readings (Snare or Piton) are correct. If hybrid: hybrid_degradation_reading is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_proxy_sufficiency_reading_contest, conceptual, 'Whether simulation fidelity threshold is sufficient or merely necessary for competence retention').

omega_variable(
    vendor_capture_risk,
    'Does the technology-dependence of the fidelity threshold create structural incentives for vendors to set thresholds high enough to maximize their market, rather than at the level that actually produces competence?',
    'Analysis of threshold-setting processes: who sets thresholds, what incentives they face, whether thresholds have increased over time as technology has advanced; comparison of thresholds across competing vendors; investigation of whether threshold increases correlate with vendor revenue or with measured competence improvements',
    'If vendor capture is significant: the constraint is Snare or Tangled Rope (extraction via threshold inflation). If thresholds are set independently: the constraint is Rope (genuine coordination). If hybrid: the constraint is Tangled Rope (mixed coordination and extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_risk, empirical, 'Vendor capture risk in fidelity threshold setting').

omega_variable(
    reading_foreclosure_test,
    'Does the assertion that ''fidelity threshold crossing is sufficient for competence retention'' logically foreclose the sibling reading that ''only real catastrophe produces competence''?',
    'Logical analysis: if simulation-only sufficiency is true, can the catastrophe-necessity reading still be held within a single coherent framework? Or do the readings occupy genuinely incompatible epistemic positions?',
    'If foreclosed: the readings are in genuine logical conflict (forecloses relation). If coexistent: the readings represent different empirical hypotheses that could both be true or false (coexists_with relation). If one influences the other: the readings have asymmetric pressure (influences relation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Logical relationship between simulation-sufficiency and catastrophe-necessity readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_fidelity_threshold, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simfid_tr_t0, simulation_fidelity_threshold, theater_ratio, 0, 0.22).
narrative_ontology:measurement(simfid_tr_t5, simulation_fidelity_threshold, theater_ratio, 5, 0.3).
narrative_ontology:measurement(simfid_tr_t10, simulation_fidelity_threshold, theater_ratio, 10, 0.38).
narrative_ontology:measurement(simfid_tr_t15, simulation_fidelity_threshold, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(simfid_be_t0, simulation_fidelity_threshold, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(simfid_be_t5, simulation_fidelity_threshold, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(simfid_be_t10, simulation_fidelity_threshold, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(simfid_be_t15, simulation_fidelity_threshold, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(simfid_su_t0, simulation_fidelity_threshold, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(simfid_su_t5, simulation_fidelity_threshold, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(simfid_su_t10, simulation_fidelity_threshold, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(simfid_su_t15, simulation_fidelity_threshold, suppression_requirement, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_fidelity_threshold, resource_allocation).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the catastrophe_proxy_sufficiency kernel family. The sibling readings (catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading) are separate constraint stories with different ε values and classifications. This reading asserts that simulation fidelity threshold crossing IS sufficient for competence retention (Rope classification, moderate extractiveness). Sibling readings contest this and produce different classifications (Snare, Piton, or Tangled Rope). The readings are linked via network.affects_constraints because they share the same kernel and contest the same core question: Is simulation a sufficient proxy for catastrophe exposure?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simulation_fidelity_threshold, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
