% ============================================================================
% CONSTRAINT STORY: simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_as_proxy, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: simulation_as_proxy
 *   human_readable: Simulation-as-Proxy for Competence Validation
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In safety-critical domains (aviation, nuclear operations, emergency
 *   response, maritime), competence validation faces an inherent measurement
 *   problem: the most valid test — actual catastrophe — is too costly and
 *   rare to use as the basis for training decisions. Simulation emerged as a
 *   practical proxy that allows repeated, controlled testing of emergency
 *   response without deploying real catastrophe. The simulation-as-proxy
 *   reading treats this proxy as sufficient validation: if operators
 *   demonstrate competence in high-fidelity simulation drills, and
 *   organizational safety records remain acceptable, simulation certification
 *   is adequate evidence of competence. The constraint exhibits dual
 *   characteristics: it coordinates a real validation problem (how to certify
 *   competence without waiting for catastrophe) while extracting from
 *   frontline operators (imposing testing, surveillance, and career risk
 *   through simulation metrics). The theater ratio (rising from 0.45 to 0.68
 *   over 15 years) reflects increasing performativity: as simulation
 *   infrastructure matures, time and resources devoted to compliance
 *   documentation, scenario logging, and certification paperwork grow, while
 *   genuine competence development may not increase proportionally. This
 *   constraint is one reading of the kernel 'competence_exercise_validity,'
 *   which is contested across three distinct readings: the
 *   simulation-as-proxy reading (this constraint), the real_catastrophe_only
 *   reading (competence can only be proven through actual failure, a rare and
 *   costly approach), and the continuous_refresh_hybrid reading (competence
 *   requires ongoing adaptive learning cycles blending simulation and field
 *   observation). Each reading grounds its validity claim differently —
 *   empirically (simulations predict field performance), deontologically
 *   (operators deserve to be trained under real conditions), and
 *   instrumentally (continuous adaptive learning achieves competence more
 *   efficiently than one-time simulation validation).
 *
 * KEY AGENTS:
 *   - Regulatory Authority: Primary beneficiary (institutional/arbitrage) — gains standardized competence metrics, documented compliance, reduced audit burden. Can revise standards or shift validation modality without exit cost.
 *   - Frontline Operators: Primary victim (powerless/trapped) — subject to mandatory simulation drills, career risk from poor scores, no exit from certification regime. Competence judgment imposed regardless of real-world readiness.
 *   - Organization Risk Management: Secondary beneficiary (institutional/arbitrage) — gains documented compliance, liability protection, quantified competence data for insurance and regulatory purposes.
 *   - Operations Managers: Mixed position (moderate/constrained) — constrained by requirement to maintain simulation-certified crews and log training hours, but benefit from operational data and learning opportunities in controlled drills.
 *   - Competence Assurance Coalition: Organized alternative (organized/constrained) — sees simulation-as-proxy as temporary, working toward maturer systems (continuous digital monitoring, just-in-time training) that will sunset the simulation regime.
 *   - Certification Infrastructure: Institutional persistence (institutional/arbitrage) — training centers, simulation facilities, and credentialing bodies maintain the regime through inertia and constituency interests, not because simulation remains the most effective validation method.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_as_proxy, 0.52).
domain_priors:suppression_score(simulation_as_proxy, 0.58).
domain_priors:theater_ratio(simulation_as_proxy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_as_proxy, extractiveness, 0.52).
narrative_ontology:constraint_metric(simulation_as_proxy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(simulation_as_proxy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(simulation_as_proxy, "Simulation-as-Proxy for Competence Validation").
narrative_ontology:topic_domain(simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_as_proxy, 'a68f9ab5-7d6d-4be3-92b4-a8dad988c149').
narrative_ontology:cs_created_at('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', '').
narrative_ontology:cs_kernel_codification('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', formalized).
narrative_ontology:cs_authority_grounding('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', extraction).
narrative_ontology:cs_interpretation_layer_present('a68f9ab5-7d6d-4be3-92b4-a8dad988c149').
narrative_ontology:cs_kernel_id(simulation_as_proxy, competence_exercise_validity).
narrative_ontology:cs_reading_relation('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', real_catastrophe_only, coexists_with).
narrative_ontology:cs_reading_relation('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_axiom('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', foundational, simulation_fidelity_sufficient).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', simulation_fidelity_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', foundational, regulatory_compliance_adequate).
narrative_ontology:cs_axiom_status(regulatory_compliance_adequate, holdable).
narrative_ontology:cs_axiom_grounding('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', regulatory_compliance_adequate, conventional).
narrative_ontology:cs_reference_frame('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', simulation_sufficiency_framework).
narrative_ontology:cs_drift_state('a68f9ab5-7d6d-4be3-92b4-a8dad988c149', contemporary_monitoring_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_as_proxy, regulatory_authorities).
narrative_ontology:constraint_beneficiary(simulation_as_proxy, organization_risk_management).
narrative_ontology:constraint_victim(simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(simulation_as_proxy, actual_system_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped by mandatory simulation drills that validate competence on paper while actual emergency response remains untested. Cannot exit the certification regime. Bears full cost if simulation-validated competence fails catastrophically in real conditions. No appeal from the validation framework.
constraint_indexing:constraint_classification(simulation_as_proxy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPERATIONS MANAGER (TANGLED ROPE) — Constrained by regulatory requirement to maintain simulation-certified crews and document competence metrics. Also benefits: simulation drills provide controlled learning environment, performance data, and documented compliance reducing personal liability. Mixed extraction and coordination — required to run drills but gains operational intelligence from them.
constraint_indexing:constraint_classification(simulation_as_proxy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Benefits from simulation-as-proxy as a coordination mechanism: enables standardized competence validation, documenting compliance, and managing accountability without resource-intensive field audits. Experiences the constraint as efficient coordination — simulation metrics solve the verification problem at reasonable cost. Net beneficiary with multiple exit options (can revise standards, shift metrics, etc).
constraint_indexing:constraint_classification(simulation_as_proxy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPETENCE ASSURANCE COALITION (SCAFFOLD) — Organized actors (accreditation bodies, safety networks, operator associations) recognize simulation-as-proxy as a temporary solution with an explicit sunset: continuous field observation, real-time digital monitoring, and just-in-time adaptive training are building alternatives. Coalition sees the gap between simulation validation and actual readiness as a time-limited problem. Effective extraction is low because organized parties have agency and see an exit path (mature monitoring systems within 15-20 years).
constraint_indexing:constraint_classification(simulation_as_proxy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CERTIFICATION INFRASTRUCTURE (PITON) — Simulation-based competence certification persists through institutional inertia despite erosion of actual validity. The infrastructure (training centers, simulation facilities, certification exams) was built to solve a real problem (validating competence without constant field deployment). But the infrastructure now persists because it exists and generates constituencies, not because it reliably predicts field readiness. Theater ratio (0.68) reflects that much simulation activity is performative compliance ritual — logging hours, passing scenario benchmarks, documenting training — rather than genuine competence development.
constraint_indexing:constraint_classification(simulation_as_proxy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, some gap between controlled training and real-world performance is an inherent feature of how competence develops: the complexity of live catastrophe always exceeds simulation parameters, so simulation cannot fully prepare operators for the unknown. This perspective sees simulation-as-proxy as an immutable property of learning under uncertainty. However, the structural data contradicts the mountain gate — identifiable beneficiaries and extraction mechanisms reveal this is a contingent institutional arrangement naturalized as law.
constraint_indexing:constraint_classification(simulation_as_proxy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_as_proxy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_as_proxy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_as_proxy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(simulation_as_proxy, TR),
    TR >= 0.70.

:- end_tests(simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from operators through mandatory participation, surveillance of performance metrics, and career consequences of poor simulation scores. But extraction is not maximum because simulation drills also provide genuine learning opportunities and skill development — the coordination function is real, not pure facade. The extractiveness reflects that operators bear disproportionate cost (time, career risk, compliance burden) while beneficiaries (regulators, organizations) gain validation and liability reduction. Suppression (0.58): Moderate-high. Operators cannot exit the regime — simulation certification is mandatory for employment in safety-critical roles. But suppression is not total — operators can achieve high scores and advance careers; the regime does not prevent all success. The suppression reflects barriers to exit (career consequences, mandatory certification) and limits on alternative validation pathways (operators cannot choose continuous field observation or peer-based assessment as substitutes). Theater ratio (0.68): High and rising. Over the 15-year interval, simulation activity has increasingly become compliance ritual rather than pure competence development. This reflects growth in documentation requirements, standardized scenario benchmarks, and certification paperwork — activities that validate regulatory compliance more than operational readiness. The rising theater ratio indicates that as simulation infrastructure matures, institutional inertia and bureaucratic requirements consume increasing resources relative to actual skill development.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. The regulatory authority experiences the constraint as pure coordination (Rope) — simulation solves the verification problem elegantly. The frontline operator experiences it as extraction (Snare) — mandatory drills with career consequences that may not predict real competence. The operations manager experiences mixed coordination and extraction (Tangled Rope) — required to enforce drills but gains learning data. The organized coalition sees a temporary problem with a sunset (Scaffold) — maturing monitoring technology will eventually replace simulation-based validation. The certification infrastructure sees its own degraded ritual (Piton) — simulation persists through inertia, not because it works better than alternatives. The analytical observer risks naturalizing the proxy as inherent to learning (Mountain) — assuming some gap between training and real-world performance is inevitable. But the beneficiary/victim declarations reveal this is not law of nature but contingent institutional arrangement: identifiable actors benefit from simulation-as-proxy validation, and their interests in maintaining the regime can be traced.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities and risk management organizations have low directionality (d ≈ 0.15–0.25) because they are primary beneficiaries with arbitrage options — they can change validation standards, shift metrics, or migrate to alternative modalities without cost. Frontline operators have high directionality (d ≈ 0.88–0.95) because they are victims with trapped exit options — they cannot exit the certification regime and bear full cost of mandatory participation. Operations managers occupy middle ground (d ≈ 0.55–0.65) because they experience both coordination benefits (learning from drills, operational intelligence) and extraction costs (compliance burden, requirement to maintain certified crews). The organized coalition has low-to-moderate directionality (d ≈ 0.42–0.58) because they are organized agents with constrained but real exit options — they can build alternative validation systems and transition away from simulation dependence, though transition requires coordination and resources.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by explicitly routing the reading selection (proxy vs. catastrophe vs. hybrid) through omega variables and cs_structure axioms rather than embedding the dispute in the base classification. The constraint's tangled-rope classification reflects its structure in the proxy reading: genuine coordination (operators need validation pathway; regulators need compliance documentation) mixed with extraction (operators bear testing burden; regulators gain liability reduction). If the real_catastrophe reading were adopted, extractiveness would rise (no legitimate coordination function; pure extraction disguised as learning) and the constraint would reclassify as Snare. If the continuous-refresh reading were adopted, extractiveness would drop (coordination function is genuine and addresses all parties' learning needs) and the constraint might reclassify as improved Rope. Mandatrophy is resolved by: (1) acknowledging that the reading choice is a commitment-system question, not a technical classification question; (2) routing the contested axioms (whether simulation suffices) to omega variables with type_class='conceptual' and type_class='empirical'; (3) declaring reading_relations that show the three readings coexist as live positions without logical foreclosure of each other (each has internal consistency); (4) documenting drift_state to show whether the proxy reading's authority is stable or eroding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_boundary,
    'What fidelity threshold makes simulation a reliable predictor of real-world competence? Is any fixed-parameter simulation adequate, or does real-world readiness require unbounded parameter complexity?',
    'Comparative analysis of operators with high simulation scores but poor field performance vs. operators with lower scores but strong real-world outcomes. Identification of specific failure modes that simulation missed.',
    'If fixed-parameter simulation suffices: simulation-as-proxy is a legitimate coordination mechanism (classification shifts toward Rope). If unbounded complexity is required: simulation is inherently inadequate cover story for extraction (classification shifts toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, empirical, 'Fidelity threshold at which simulation becomes reliable predictor of real competence').

omega_variable(
    proxy_vs_coordinate_kernel_reading,
    'This is one reading of the contested kernel ''competence_exercise_validity''. What distinguishes the simulation-as-proxy reading from its siblings (real_catastrophe_only, continuous_refresh_hybrid)?',
    'Explicit comparison of foundational axioms: does simulation validation suffice (proxy reading) or must competence be tested in real catastrophic conditions (real_catastrophe reading) or through continuous adaptive refreshing (hybrid reading)? Examine the grounding type of each reading''s core claim.',
    'If simulation-as-proxy axiom holds: regulation can rely on simulation metrics. If overridden: regulatory framework must shift to continuous monitoring or acceptance of higher real-world failure rates as proof of readiness (real_catastrophe reading) or continuous refresh cycles (hybrid reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proxy_vs_coordinate_kernel_reading, conceptual, 'Whether simulation validation is sufficient for competence judgment (kernel reading distinction)').

omega_variable(
    extraction_mechanism_vs_coordination_cost,
    'Is the simulation regime''s extraction (suppression of operator autonomy, career risk for poor scores, regulatory overhead) a necessary coordination cost or an extractive mechanism layered onto coordination?',
    'Counterfactual analysis: could equivalent competence validation be achieved with lower suppression (e.g., peer-based continuous assessment, lightweight field observation)? Comparison of suppression levels across different regulatory regimes using different validation modalities.',
    'If extraction exceeds minimal coordination cost: the constraint is Tangled Rope or Snare (extraction + some coordination). If extraction equals coordination cost: constraint is pure Rope (coordination with acceptable overhead).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_vs_coordination_cost, empirical, 'Whether simulation regime''s suppression is coordination cost or extractive overhead').

omega_variable(
    regulatory_authority_capture,
    'Does the regulatory authority''s dependence on simulation-as-proxy metrics create incentive to maintain the regime even if field data contradicts its validity? Is the authority captured by the infrastructure it created?',
    'Historical analysis of regulatory responses to documented failures: do regulators revise simulation standards when operators with high scores fail catastrophically, or do they add more simulation requirements? Examination of regulatory turnover, institutional identity dependence, and career paths of standard-setting bodies.',
    'If regulatory authority is captured: the constraint persists despite validity erosion, and piton classification is confirmed. If authority revises standards in response to evidence: regulation maintains genuine coordination function, and classification remains Rope/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_capture, empirical, 'Whether regulatory authority is captured by simulation infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_as_proxy, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simprox_tr_t0, simulation_as_proxy, theater_ratio, 0, 0.45).
narrative_ontology:measurement(simprox_tr_t8, simulation_as_proxy, theater_ratio, 8, 0.58).
narrative_ontology:measurement(simprox_tr_t15, simulation_as_proxy, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(simprox_be_t0, simulation_as_proxy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(simprox_be_t8, simulation_as_proxy, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(simprox_be_t15, simulation_as_proxy, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_as_proxy, enforcement_mechanism).
narrative_ontology:affects_constraint(simulation_as_proxy, competence_decay_recognition).
narrative_ontology:affects_constraint(simulation_as_proxy, catastrophe_frequency_measurement).

% DUAL FORMULATION NOTE:
% The simulation-as-proxy constraint is upstream of domain-specific competence constraints (pilot certification, nuclear operator readiness, emergency responder training) but represents a distinct structural choice: how to validate competence without deploying catastrophe. The related constraints 'competence_decay_recognition' and 'catastrophe_frequency_measurement' depend on this reading's validity. If the proxy reading is challenged (omega 'proxy_vs_coordinate_kernel_reading' resolved against proxy), downstream constraints would be affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
