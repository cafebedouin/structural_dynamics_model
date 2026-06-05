% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__actuarial_risk_acceptance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__actuarial_risk_acceptance, []).

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
 *   constraint_id: rogers_commission_findings__actuarial_risk_acceptance
 *   human_readable: Rogers Commission Actuarial Risk Acceptance Framework
 *   domain: aerospace_safety/regulatory_governance/organizational_decision_making
 *
 * SUMMARY:
 *   The Rogers Commission (1986) found that the Space Shuttle Challenger
 *   disaster resulted from organizational failures to act on known O-ring
 *   thermal performance degradation. The Commission's core recommendation was
 *   that future missions proceed only when 'risk is acceptable to informed
 *   decision-makers'—establishing an actuarial framework for crew safety.
 *   This story instantiates ONE READING of the Rogers findings: the
 *   actuarial_risk_acceptance reading, where documented failure probability
 *   enables transparent trade-offs between mission value and crew safety.
 *   This reading sits alongside two contested siblings: the
 *   engineering_absolute_threshold reading (some failure modes must be
 *   excluded categorically) and the management_compliance_narrative reading
 *   (risk quantification serves organizational legitimacy rather than genuine
 *   decision-making). The actuarial reading commits to the core assertion
 *   that quantifying failure probability and securing informed authorization
 *   is a legitimate way to proceed with high-risk operations. Extractiveness
 *   has risen from 0.35 (early post-Challenger era) to 0.58 (contemporary
 *   practice) as the actuarial framework became institutionalized and theater
 *   ratios increased. Suppression has similarly risen as career consequences
 *   of declining documented-risk missions have become explicit. The
 *   constraint exhibits hybrid coordination-extraction: genuine coordination
 *   function (transparent risk communication enables better decisions)
 *   coupled with asymmetric extraction (crew must accept quantified risk or
 *   exit operationally; program leadership retains authorization authority).
 *
 * KEY AGENTS:
 *   - Astronaut crew: Primary victim (powerless/trapped) — cannot exit documented-risk missions without career consequences; experience maximum suppression and extraction
 *   - Safety engineering community: Organized victim (organized/constrained) — benefit from program continuation but subordinated to management authorization decisions; constrained exit (can lobby but cannot prevent)
 *   - Mission planners and program leadership: Primary beneficiary (institutional/arbitrage) — gain decisional authority and program continuity; arbitrage between risk and mission value
 *   - Post-Challenger safety reform movement: Organized beneficiary (powerful/mobile) — external epistemic community with exit options; views framework as temporary scaffold toward higher absolute safety standards
 *   - Quantification and risk assessment machinery: Institutional actor (institutional/arbitrage) — the actuarial process itself; enables legitimacy generation
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional framework as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, 0.58).
domain_priors:suppression_score(rogers_commission_findings__actuarial_risk_acceptance, 0.72).
domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, extractiveness, 0.58).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__actuarial_risk_acceptance, "Rogers Commission Actuarial Risk Acceptance Framework").
narrative_ontology:topic_domain(rogers_commission_findings__actuarial_risk_acceptance, "aerospace_safety/regulatory_governance/organizational_decision_making").

domain_priors:requires_active_enforcement(rogers_commission_findings__actuarial_risk_acceptance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__actuarial_risk_acceptance, '73f1b55c-91f9-4e14-82eb-2a5e861f99ff').
narrative_ontology:cs_kernel_codification('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', formalized).
narrative_ontology:cs_authority_grounding('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', extraction).
narrative_ontology:cs_interpretation_layer_present('73f1b55c-91f9-4e14-82eb-2a5e861f99ff').
narrative_ontology:cs_reading_relation('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', rogers_commission_findings__engineering_absolute_threshold, coexists_with).
narrative_ontology:cs_reading_relation('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', rogers_commission_findings__management_compliance_narrative, influences).
narrative_ontology:cs_axiom('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', foundational, quantified_probability_enables_legitimate_authorization).
narrative_ontology:cs_axiom_status(quantified_probability_enables_legitimate_authorization, holdable).
narrative_ontology:cs_axiom_grounding('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', quantified_probability_enables_legitimate_authorization, instrumental).
narrative_ontology:cs_axiom('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', secondary, crew_acceptance_constitutes_informed_consent).
narrative_ontology:cs_axiom_status(crew_acceptance_constitutes_informed_consent, overridden).
narrative_ontology:cs_axiom_grounding('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', crew_acceptance_constitutes_informed_consent, deontological).
narrative_ontology:cs_reference_frame('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', documented_risk_authorization_standard).
narrative_ontology:cs_drift_state('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', contemporary_normalized_actuarialism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('73f1b55c-91f9-4e14-82eb-2a5e861f99ff', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, mission_planners).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__actuarial_risk_acceptance, aerospace_program_leadership).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, crew_safety_absolute_standard).
narrative_ontology:constraint_victim(rogers_commission_findings__actuarial_risk_acceptance, categorical_safety_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASTRONAUT AS STATISTICAL SUBJECT (SNARE) — Crew members are transformed into probability distributions. They cannot exit: accepting the mission requires accepting documented failure probability. No opt-out mechanism. High suppression: declining to fly a documented-risk mission ends career, triggers reputational damage, removes peer from operational status. Extraction is severe because the actuarial framing legitimizes exposure that a categorical safety standard would forbid.
constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY ENGINEERING COMMUNITY (TANGLED ROPE) — Engineers benefit from the constraint (it legitimizes continued operations and enables program continuation they rely on) AND bear costs (actuarial framework subordinates their categorical safety recommendations to management decision-making). Organized but constrained: they can lobby for stricter bounds but cannot prevent operations deemed acceptable by leadership under the actuarial framework. Active enforcement required: someone must decide which probability threshold is acceptable, creating institutional authority asymmetry.
constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRAM LEADERSHIP (ROPE) — Mission planners experience the constraint as pure coordination: quantifying risk enables transparent decision-making and legitimate authorization. They can arbitrage between mission urgency and safety bounds—the actuarial framework is their tool for making explicit trade-offs that preserve program continuity. Net beneficiary: the framework empowers them to say 'acceptable risk' instead of accepting unsafe or canceling missions.
constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-CHALLENGER SAFETY REFORM (SCAFFOLD) — External organized actors (ASAP/Boisjoly epistemic community, international aerospace safety forums) view actuarial risk acceptance as a temporary framework: improved materials, redundancy design, and probabilistic risk assessment maturation are building pathways to lower absolute failure rates, reducing the need for high-probability-acceptance operations. Sunset clause: as absolute safety improves, acceptable failure probability thresholds should drop. Theater is moderate because quantification itself (risk communication requirement) is genuine, even if decision-making remains opaque.
constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE QUANTIFICATION RITUAL (PITON) — The actuarial machinery itself (probability assessment, risk matrices, acceptance documentation) has become largely performative. Organizations conduct formal risk quantification to generate legitimate-appearing decision records, but the underlying probability estimates rest on sparse failure data (one O-ring failure mode among billions of flights) and engineering judgment disguised as calculation. The theater lies in the appearance of precision: calling something 'an 0.04 failure probability' creates an illusion of measurement that masks deep uncertainty. Theater ratio is elevated (0.65) because the quantification process produces artifacts of legitimacy rather than confidence.
constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From a civilizational-universal view, human spaceflight is inherently probabilistic: no vehicle can be made perfectly safe, and all complex systems exhibit failure modes. Therefore, some threshold of documented acceptable probability is logically necessary to conduct operations. This perspective risks naturalizing what is actually a contingent institutional choice — treating 'we must have a decision framework' as if it means 'actuarial framing is the only or inevitable framework.' False summit candidate: the engine will identify beneficiaries and reveal that the 'necessity' framing conceals a legitimacy claim that benefits specific actors.
constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_findings__actuarial_risk_acceptance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__actuarial_risk_acceptance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rogers_commission_findings__actuarial_risk_acceptance, TR),
    TR >= 0.70.

:- end_tests(rogers_commission_findings__actuarial_risk_acceptance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The actuarial framework enables mission authorization under documented-risk conditions that would be forbidden under categorical safety standards. Program leadership extracts decisional authority (they decide what probability is acceptable); crew extracts the legitimacy of continued program operation but loses the choice to decline. The asymmetry reflects that crew accepts risk under documented conditions while leadership retains the power to define 'acceptable.' Extractiveness has increased over the interval as the framework became normalized: early post-Challenger era (t=0) maintained higher categorical safety aspiration (extractiveness 0.35); by t=10, actuarial framing had become standard operating procedure (0.58). Suppression (0.72): High. Crew members face irreversible consequences: accepting documented failure probability is required for mission participation; declining is career-ending. Safety engineers face suppression through institutional override: their recommendations can be superseded by management judgment that probability is 'acceptable.' The quantification machinery itself suppresses dissent by creating an appearance of objective decision-making. Suppression has risen as the framework became institutionalized and career consequences explicit. Theater ratio (0.65): Moderately high, reflecting that risk quantification is both genuine (enables explicit trade-off discussion) and performative (probability estimates rest on sparse data and engineering judgment, creating illusion of precision). The theater has increased over the interval as organizations learned to use quantification for legitimacy generation rather than decision-improvement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the widest perspectival divergence in the Rogers readings. The program leadership sees a coordination mechanism (Rope) enabling legitimate decisions; the astronaut crew sees a snare that transforms them into statistical subjects while stripping them of exit authority. The safety engineering community occupies the tangled middle (Tangled Rope)—they benefit from program continuation but are subordinated by the framework. The piton perspective reveals that the quantification machinery has become largely theatrical: probability estimates are presented as measurement but represent organized judgment. The scaffold perspective sees actuarial acceptance as temporary—improved absolute safety should progressively lower acceptable failure probability thresholds. The mountain perspective risks naturalizing what is contingent: treating 'some decision framework is necessary' as if it means 'actuarial framing is necessary.' The false summit detector will identify this as naturalization of institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's position relative to the risk-acceptance extraction flow. Program leadership (institutional/arbitrage) has low d: they benefit from the framework and can exit any specific decision by refusing authorization. Crew (powerless/trapped) has high d: they are forced to accept quantified risk or exit operationally (career death). Safety engineers (organized/constrained) occupy middle d: they have some lobbying power but cannot prevent authorization. The sigmoid f(d) amplifies extraction for trapped agents (d→1.0, f(d)→1.42) and suppresses it for arbitrage beneficiaries (d→0.05, f(d)→-0.12). Scope modifier σ(S) is slightly elevated (σ_national ≈ 1.0, σ_global ≈ 1.2) because spaceflight authority is nationally concentrated but globally consequential. The framework's authority to define 'acceptable probability' flows toward program leadership (low d, low f(d)) and away from crew (high d, high f(d)): chi = ε × f(d) × σ(S) produces asymmetric experienced extractiveness that parallels the structural power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The actuarial reading resolves its mandatrophy by demonstrating that the tangled rope classification (hybrid coordination-extraction) is stable across all high-power perspectives: program leadership sees coordination (Rope) when viewed narrowly, but the organizing power structure reveals extraction from crew. The scaffold perspective stabilizes the classification by showing that actuarial acceptance is not permanently extractive—improved absolute safety reduces acceptable probability thresholds. The piton perspective reveals that the quantification machinery has become largely performative, increasing theater without increasing real safety (theater ratio 0.45→0.65). The snare perspective (crew) is irreducible: from the crew's structural position, extraction is total. The constraint is legitimately classified as tangled rope at the institutional/program level because genuine coordination function (transparent risk communication) exists alongside asymmetric extraction (authorization authority concentration). The mandatrophy is resolved by recognizing that both functions are present and neither is removable without losing the constraint's entire structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_estimation_epistemic_gap,
    'Can crew failure probability be estimated with confidence sufficient to ground irreversible life-or-death decisions when failure datasets are sparse and engineering judgment heavily weights estimates?',
    'Bayesian analysis of O-ring failure model post-Challenger: calibration of predicted vs actual failure rates across subsequent missions; comparison of pre-flight estimates to post-flight validation; assessment of tail-event prediction accuracy',
    'If estimable with high confidence: actuarial framework is sound epistemic basis for decision. If not: actuarial precision is theater, and risk acceptance is a legitimacy ritual masking organizational judgment. Classification remains tangled_rope either way, but the extraction mechanism shifts from justified trade-off to illegitimate concealment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probability_estimation_epistemic_gap, empirical, 'Whether crew failure probability can be estimated with decision-grade confidence').

omega_variable(
    informed_consent_enforcement_gap,
    'Does documented risk acceptance by mission planners constitute informed consent by crew when crew members cannot veto mission authorization?',
    'Analysis of crew briefing content, veto authority, and career consequences of declining documented-risk missions; comparison to medical informed consent standards; survey of crew perception of decision authority vs decision communication',
    'If true consent: framework legitimately distributes decision authority. If not: actuarial frame is a mechanism for converting crew choice into organizational directive. Affects whether snare classification persists or crew gains constrained/mobile exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_enforcement_gap, empirical, 'Whether documented risk acceptance constitutes meaningful informed consent').

omega_variable(
    categorical_vs_actuarial_foreclosure,
    'Does commitment to actuarial risk acceptance logically foreclose the engineering_absolute_threshold reading (that some failure modes must be excluded categorically regardless of probability bounds)?',
    'Logical analysis: can both readings coexist in a single decision framework, or does accepting actuarial frames require rejecting categorical exclusions? Case study: post-Challenger O-ring redundancy — did this represent achievement of categorical safety or merely reduction of acceptable probability?',
    'If forecloses: this reading and engineering_absolute_threshold are contradictory; framework cannot hold both. If coexists: both readings remain live within different institutional constituencies. Determines reading_relations structure in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_actuarial_foreclosure, conceptual, 'Logical relationship between actuarial and categorical safety framings').

omega_variable(
    management_narrative_cooptation,
    'To what extent does the management_compliance_narrative reading (risk acceptance as organizational legitimacy theater) represent a distinct structural claim versus a cynical reinterpretation of the actuarial reading?',
    'Discourse analysis of NASA post-Challenger documentation: examine whether risk quantification served decision-making (actuarial reading) or legitimacy generation (compliance narrative reading); interview organizational historians about decision-making authority structure; analyze decision reversal patterns (decisions that should have been blocked by quantified risk but weren''t)',
    'If distinct constraint: write separate story with different ε (higher, reflecting pure legitimacy rather than hybrid coordination-extraction). If reinterpretation: management_compliance_narrative is not a sibling reading but a contestation of this reading''s authenticity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_narrative_cooptation, conceptual, 'Whether management_compliance_narrative is a distinct reading or critique').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__actuarial_risk_acceptance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_actuarial_theater_t0, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rogers_actuarial_theater_t5, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 5, 0.58).
narrative_ontology:measurement(rogers_actuarial_theater_t10, rogers_commission_findings__actuarial_risk_acceptance, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(rogers_actuarial_extractiveness_t0, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rogers_actuarial_extractiveness_t5, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(rogers_actuarial_extractiveness_t10, rogers_commission_findings__actuarial_risk_acceptance, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rogers_actuarial_suppression_t0, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(rogers_actuarial_suppression_t5, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(rogers_actuarial_suppression_t10, rogers_commission_findings__actuarial_risk_acceptance, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__actuarial_risk_acceptance, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__actuarial_risk_acceptance, rogers_commission_findings__management_compliance_narrative).

% DUAL FORMULATION NOTE:
% The Rogers Commission findings decompose into three distinct constraints with different ε values: (1) actuarial_risk_acceptance (ε=0.58, Tangled Rope) — quantified probability enables authorization; (2) engineering_absolute_threshold (ε=0.32, Rope) — some failure modes must be excluded categorically; (3) management_compliance_narrative (ε=0.75, Snare) — quantification serves organizational legitimacy rather than decision-making. These are not perspectives on one constraint but structurally distinct claims with different beneficiary/victim structures, authority relationships, and extractiveness profiles. All three are readings of the same kernel (Rogers findings) but instantiate different structural commitments. Network links enable contamination propagation analysis: if the actuarial reading's epistemic basis is undermined, the compliance narrative reading becomes more salient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__actuarial_risk_acceptance, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
