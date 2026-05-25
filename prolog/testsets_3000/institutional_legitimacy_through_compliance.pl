% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_through_compliance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_through_compliance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_legitimacy_through_compliance
 *   human_readable: Institutional Legitimacy Through Compliance
 *   domain: institutional_governance/organizational_behavior
 *
 * SUMMARY:
 *   Institutional legitimacy through compliance creates a structural
 *   extraction mechanism where regulatory adherence becomes the primary
 *   signal of institutional trustworthiness. This constraint operates across
 *   all institutional contexts—corporate governance, healthcare, education,
 *   law enforcement—where external stakeholders (regulators, auditors,
 *   insurers, public) assess institutional legitimacy primarily through
 *   documentation and compliance metrics rather than through direct
 *   observation of outcomes. The constraint exhibits a classic tangled rope
 *   structure: genuine coordination function (institutions do need
 *   standardized practices to communicate trustworthiness and enable
 *   multi-stakeholder systems to function) coexists with asymmetric
 *   extraction (frontline operators bear compliance burden while
 *   institutional leadership captures legitimacy benefits). The theater ratio
 *   has increased over the measurement interval (from 0.42 to 0.68) as
 *   compliance systems have accumulated without corresponding simplification,
 *   indicating that performative compliance activity now dominates functional
 *   verification.
 *
 * KEY AGENTS:
 *   - Frontline Operators: Primary victims (powerless/trapped) — healthcare workers, teachers, service providers who bear documentation and procedural burden; no exit within their professional context
 *   - Institutional Leadership: Primary beneficiaries (institutional/arbitrage) — executives, boards, senior administrators who capture legitimacy value from compliance signaling while remaining insulated from compliance burden
 *   - Middle Managers: Secondary actors (moderate/constrained) — supervisors, department heads who experience both coordination function and extraction pressure; unable to exit due to career specialization
 *   - Compliance Infrastructure: Secondary beneficiary (institutional/arbitrage) — compliance departments, audit functions, regulatory consultants whose institutional existence depends on maintaining compliance requirement complexity
 *   - Reform Coalition: Organized agents (organized/constrained) — labor unions, professional associations, consumer advocates pushing for proportional compliance and outcome-based alternatives
 *   - Regulatory Bodies: External actors (institutional/analytical) — government agencies, standard-setters that design and enforce compliance regimes; often experience regulatory capture
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional need for legitimacy verification as inherent requirement rather than as contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_through_compliance, 0.58).
domain_priors:suppression_score(institutional_legitimacy_through_compliance, 0.65).
domain_priors:theater_ratio(institutional_legitimacy_through_compliance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_through_compliance, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_through_compliance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_legitimacy_through_compliance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_through_compliance, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_through_compliance, "Institutional Legitimacy Through Compliance").
narrative_ontology:topic_domain(institutional_legitimacy_through_compliance, "institutional_governance/organizational_behavior").

domain_priors:requires_active_enforcement(institutional_legitimacy_through_compliance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_through_compliance, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_through_compliance, compliance_infrastructure).
narrative_ontology:constraint_victim(institutional_legitimacy_through_compliance, frontline_operators).
narrative_ontology:constraint_victim(institutional_legitimacy_through_compliance, organizational_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped within compliance regimes that extract labor and autonomy. Operators bear costs of regulatory adherence, documentation burden, and liability exposure while institutional leadership captures legitimacy benefits. No meaningful exit option within the professional context they inhabit.
constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Constrained by both upward accountability to leadership and downward pressure on operators. Experiences genuine coordination function (aligning local action with institutional standards) alongside asymmetric extraction (forced to pass compliance burden downward while reporting upward only sanitized success metrics). Limited exit due to industry consolidation and career specialization.
constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Experiences compliance as pure coordination: it solves the legitimacy problem and enables institutional actors to participate in regulatory ecosystems. Leadership can exit (relocate, change industries, or lobby for regulatory modification) without bearing primary cost of compliance. Net beneficiary of the constraint.
constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized labor, professional associations, and regulatory reformers see compliance burden as a temporary coordination failure with a sunset. Evidence-based policy, automation of routine compliance tasks, and pressure for proportional rules are building exit pathways. Theater ratio has declined as transparency and outcome-focus replace box-checking. High agency due to organized mobilization.
constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY COMPLIANCE APPARATUS (PITON) — Institutional actors tasked with compliance implementation often recognize their own function as substantially degraded. Compliance departments persist through administrative inertia, maintaining reporting structures and audit trails that exceed their functional necessity. Theater ratio (0.68) reflects that much compliance activity is performative ritual rather than effective risk mitigation. The apparatus continues because removing it would require institutional disruption.
constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational perspective, some compliance burden is structurally inherent to institutional governance: coordination requires verification, and verification requires documentation. This perspective risks naturalizing as immutable law what is actually a contingent balance between regulatory stringency and institutional burden. The engine's false summit detector will flag this as naturalization rather than genuine necessity.
constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_through_compliance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_through_compliance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_through_compliance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_through_compliance, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_through_compliance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts labor (compliance work), autonomy (standardized procedures replacing professional judgment), and time (documentation burden) from frontline operators, with benefits concentrating in leadership and compliance infrastructure. The value reflects that some extraction is justified coordination cost—institutions do need to signal trustworthiness—but a substantial portion exceeds what functional coordination requires. The upward trajectory (0.35→0.58 over the interval) indicates extraction has accumulated as compliance requirements have layered without simplification, suggesting this is not optimization drift but institutional rent-seeking. Suppression (0.65): High. Multiple barriers prevent exit: professional licensing requirements tie operators to specific institutional contexts; economic dependence on stable employment; career specialization makes sector-switching costly; regulatory barriers make opt-out impossible. Suppression includes both structural (economic/legal) and internalized (professional identity) components. Theater ratio (0.68): High and rising. Traditional compliance (audits, certifications, documentation reviews) is substantially performative. Auditors cannot directly verify service quality or outcome effectiveness from compliance records; instead they assess whether institutions have documented appropriate processes. Documentation compliance has decoupled from outcome effectiveness in many domains (healthcare readmission rates, educational achievement, corporate ethics failures all show poor correlation with compliance scores). The rising theater ratio indicates the gap has widened—as compliance systems have accumulated, performative activity has grown faster than functional verification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence across power positions. Leadership and compliance infrastructure perceive a rope (coordination mechanism enabling institutional function). Reform coalitions perceive a scaffold with sunset (temporary problem being solved by automation and outcome-focus alternatives). Middle managers perceive tangled rope (genuine coordination mixed with extraction pressure they're forced to transmit). Frontline operators perceive a snare (pure extraction with no exit). The legacy compliance apparatus perceives itself as piton (degraded ritual maintained through inertia). The analytical observer risks perceiving mountain (compliance is inherently necessary for institutional governance). This spectrum reveals that the constraint's classification depends entirely on structural position—same institutional system is simultaneously rope, scaffold, tangled rope, snare, piton, and mountain depending on the observer's power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline operators are victims (d ≈ 0.92): structurally mobile (could change careers) but trapped by economic dependency and professional identity fusion. Leadership are beneficiaries (d ≈ 0.08): arbitrage exit (can relocate, lobby, or change industries) combined with institutional power creates near-zero experienced extraction—they control the system. Middle managers occupy intermediate position (d ≈ 0.55): constrained exit (career opportunity narrowing if they leave) with mixed victim/beneficiary status—they both implement compliance (extraction flow toward operators) and report upward (pressure from leadership). Compliance infrastructure (d ≈ 0.10): beneficiaries with arbitrage options (compliance specialists can relocate internationally, shift to adjacent fields, or lobby for regulatory change). Reform coalitions (d ≈ 0.60): organized victims with constrained exit (union members can strike/advocate but not fully exit the employment relationship). The directionality cascade reveals the systemic structure: extraction flows from powerless trapped agents through moderate constrained agents to institutional beneficiaries, with compliance infrastructure maintaining the apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The constraint resolves the mandatrophy by showing that all six types are legitimate readings from different structural positions within the same institutional system. The question is not 'is compliance a rope or a snare?' but 'whose structural position are we measuring from?' For institutional leadership, it is genuinely a rope—a coordination mechanism solving the legitimacy problem. For frontline operators, it is genuinely a snare—extraction with no exit. The middle manager sees both (tangled rope). The reform coalition sees a temporary problem (scaffold). The compliance apparatus sees its own degradation (piton). The analytical observer must resist the temptation to naturalize the system as a mountain—institutional legitimacy verification is a contingent design choice, not an immutable law. The constraint's actual extractiveness (0.58, rising) and theater ratio (0.68, rising) confirm that institutional leadership is using compliance signaling to extract legitimacy value that exceeds functional coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_functionality_threshold,
    'At what point does compliance burden exceed its verification function and become pure extraction?',
    'Comparative analysis of risk mitigation outcomes across regulatory regimes with varying compliance stringency; measurement of false positives and negatives in compliance systems vs actual institutional failures',
    'If most institutional failures are non-compliance-related: high compliance burden is extraction masquerading as coordination. If failures are primarily compliance-correlated: burden may be justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_functionality_threshold, empirical, 'Threshold between legitimate verification burden and extractive over-compliance').

omega_variable(
    theater_vs_functional_compliance_ratio,
    'What proportion of compliance activity is genuinely functional risk mitigation vs performative box-checking?',
    'Process analysis of compliance workflows; tracking of compliance-flagged issues vs actual resolved problems; post-failure audit of whether failed institutions had compliance sign-off',
    'If >60% is theater: snare classification confirmed for operators; piton confirmed for apparatus. If <30% is theater: rope classification gains credibility across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_functional_compliance_ratio, empirical, 'Functional vs performative ratio in compliance regimes').

omega_variable(
    regulatory_capture_in_compliance_design,
    'Do institutional leadership and compliance infrastructure co-design systems that serve leadership interests under the guise of standardization?',
    'Historical analysis of compliance rule changes; examination of who benefits from specific enforcement patterns; tracking of regulatory capture indicators (revolving door, industry-friendly rule design)',
    'If strong capture evidence: leadership extraction is deliberate and systemic (snare deepens into institutional conspiracy). If weak capture: compliance burden is unintended side effect of legitimate coordination attempt (tangled rope justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_in_compliance_design, empirical, 'Degree of regulatory capture in compliance architecture').

omega_variable(
    autonomy_loss_reversibility,
    'When compliance requirements are removed, do operators recover prior adaptive capacity or remain degraded?',
    'Longitudinal study of institutions that reduce compliance burden; measurement of innovation rates, decision-making speed, and adaptive response time before/after deregulation',
    'If degradation is reversible: suppression is reversible (operators can exit when constraints are removed). If irreversible: suppression has internalized and become internalized autonomy loss, raising structural barriers to exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_loss_reversibility, empirical, 'Reversibility of compliance-induced autonomy loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_through_compliance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_legitimacy_through_compliance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(inst_tr_t5, institutional_legitimacy_through_compliance, theater_ratio, 5, 0.55).
narrative_ontology:measurement(inst_tr_t10, institutional_legitimacy_through_compliance, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_legitimacy_through_compliance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inst_be_t5, institutional_legitimacy_through_compliance, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(inst_be_t10, institutional_legitimacy_through_compliance, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_through_compliance, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_through_compliance, professional_autonomy_degradation).
narrative_ontology:affects_constraint(institutional_legitimacy_through_compliance, regulatory_capture_dynamics).
narrative_ontology:affects_constraint(institutional_legitimacy_through_compliance, bureaucratic_theater_accumulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_through_compliance, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
