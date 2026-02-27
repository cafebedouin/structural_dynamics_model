% ============================================================================
% CONSTRAINT STORY: rogers_commission_institutional_analysis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_institutional_analysis, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rogers_commission_institutional_analysis
 *   human_readable: The Silent Safety Silo (NASA Decision-Making Pre-Challenger)
 *   domain: political/technological
 *
 * SUMMARY:
 *   The Rogers Commission investigation into the Challenger disaster revealed
 *   a structural constraint in NASA decision-making: critical technical data
 *   regarding O-ring thermal erosion risks was systematically filtered before
 *   reaching senior leadership. This was not a single failure event but an
 *   institutional architecture that created asymmetric risk distribution.
 *   Engineers at Morton Thiokol identified the thermal vulnerability and
 *   raised concerns; their data was suppressed at mid-level management
 *   review. Senior leadership did not receive information that would have
 *   changed the launch decision. The crew had no knowledge of the extent of
 *   known risks. The public was kept uninformed. The silo was maintained
 *   through a combination of organizational hierarchy, contractual structure,
 *   career incentives, and schedule pressure — creating a pure extraction
 *   mechanism where the beneficiary (NASA schedule authority) captured the
 *   benefits of launch timing while the costs (crew risk, epistemic
 *   integrity, public trust) were borne by powerless parties. The constraint
 *   exhibits Snare classification from multiple perspectives and represents a
 *   diagnostic case of how organizational structure can suppress critical
 *   information in safety-critical decisions.
 *
 * KEY AGENTS:
 *   - Shuttle Crew (Challenger 51-L): Primary victim (powerless/trapped) — no knowledge of known risks, no exit option
 *   - Morton Thiokol Field Engineers: Secondary victim (moderate/constrained) — identified thermal risk, suppressed by organizational hierarchy and contract dependency
 *   - NASA Mid-Level Management (Flight Readiness Review): Institutional actor (institutional/constrained) — performed risk assessment theater while filtering critical data upward
 *   - NASA Senior Leadership: Primary beneficiary (institutional/arbitrage) — benefited from schedule continuity, insulated from safety data that would have complicated decision-making
 *   - Mission Safety Epistemic Commons: Abstract victim (powerless/trapped) — systematic suppression of transparent safety assessment; no voice, full cost
 *   - American Public: Secondary victim (moderate/constrained) — unable to assess risk independently; bore reputational cost without being party to decision-making
 *   - Rogers Commission (Post-Disaster): Analytical observer (analytical/analytical) — revealed the silo structure and its consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_institutional_analysis, 0.58).
domain_priors:suppression_score(rogers_commission_institutional_analysis, 0.72).
domain_priors:theater_ratio(rogers_commission_institutional_analysis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, extractiveness, 0.58).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_institutional_analysis, snare).
narrative_ontology:human_readable(rogers_commission_institutional_analysis, "The Silent Safety Silo (NASA Decision-Making Pre-Challenger)").
narrative_ontology:topic_domain(rogers_commission_institutional_analysis, "political/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_institutional_analysis, nasa_schedule_enforcement_authority).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, shuttle_crew).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, mission_safety_epistemic_commons).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, public_trust_in_nasa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SHUTTLE CREW (SNARE) — Trapped within a system where critical safety data about O-ring erosion was systematically filtered. No exit option; exposure to risk was not a choice but a structural fact of their assigned mission. d≈0.98, f(d)≈1.45, σ=1.0 → χ≈0.84.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD ENGINEERS AND SAFETY ADVOCATES (SNARE) — Engineers at Morton Thiokol who identified O-ring thermal limits were constrained by organizational hierarchy, contractual dependency, and career risk. Suppression of their technical data was structural, not accidental. d≈0.88, f(d)≈1.25, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-LEVEL NASA MANAGEMENT (PITON) — Managers at the flight readiness review level performed risk assessment theater while actual technical constraints were known but not propagated upward. The filtering mechanism was structurally inertial — organizational boundaries that persisted through habit and contractual structure, not through active enforcement. theater_ratio=0.68 satisfies piton gate. d≈0.55, f(d)≈0.73, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NASA SENIOR LEADERSHIP (ROPE) — Upper management experienced the constraint as coordination: maintaining launch schedule, managing political pressure, and securing budget continuity. The silo structure appeared to be a necessary organizational partition (dividing concerns, maintaining focus, reducing decision noise). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; experienced as coordination mechanism.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MISSION SAFETY EPISTEMIC COMMONS (SNARE) — The abstract collective good of transparent safety assessment was systematically suppressed. No agent advocates for it; it bears the full cost of the silo without voice or exit. Theater_ratio=0.68 reflects that the safety review process appeared rigorous but was systemically blind. d≈0.97, f(d)≈1.43, σ=1.0 → χ≈0.83.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC TRUST IN NASA (SNARE) — The public's ability to assess mission safety was constrained by information asymmetry. Citizens could not exit the space program but were constrained from making informed consent. The silo created asymmetric risk: public bore reputational and legitimacy cost without being party to actual risk assessment. d≈0.85, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, this is a pure extraction mechanism dressed in organizational necessity language. The silo is not immutable; it is a structural choice that filters information asymmetrically to protect the beneficiary (schedule authority) at the cost of the victims (crew, safety commons, public trust). The constraint persists through institutional inertia and contractual structure, not through natural law.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_institutional_analysis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_institutional_analysis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rogers_commission_institutional_analysis, TR),
    TR >= 0.70.

:- end_tests(rogers_commission_institutional_analysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts schedule reliability and political continuity for NASA leadership by filtering out information that would complicate decisions. Over the 10-year pre-Challenger interval, this extraction mechanism intensified as launch cadence increased and schedule pressure became the dominant performance metric. The extraction is not trivial (0.35 → 0.58 trajectory) because critical safety information was systematically withheld. Suppression (0.72): High. The silo structure created multiple barriers: organizational hierarchy that discouraged upward escalation of bad news, contractual dependency (Morton Thiokol's business relationship with NASA), career risk for engineers who challenged consensus, and normalization of delay-averse decision-making that made safety data inconvenient. Theater ratio (0.68): Moderate-high, increasing. Flight readiness reviews performed the rituals of safety assessment (formal processes, documented reviews, sign-offs) while systematically missing critical technical data. The theater increased over time as the silo's invisibility made the review process appear comprehensive while actually becoming more selective in what data it allowed upward. The theater is not maximal (0.68 vs piton threshold 0.70) because some legitimate technical review did occur; it was the filtering mechanism, not the absence of review.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits complete perspectival fragmentation. NASA senior leadership experienced the silo as coordination (Rope) — a necessary organizational partition that allowed them to focus on feasibility assessment and schedule management without being overwhelmed by technical noise. Mid-level management experienced it as inertial theater (Piton) — performing risk assessment review while the organizational structure made it impossible for critical data to reach them or to propagate further. Engineers experienced it as pure extraction (Snare) — their technical findings were systematically suppressed through hierarchy and career incentives. The crew experienced it as lethal constraint (Snare) — trapped in a system where known risks were not communicated. The public experienced it as asymmetric risk (Snare) — constrained from independent risk assessment while bearing reputational and safety consequences. The mission safety epistemic commons experienced it as complete suppression (Snare) — transparent safety assessment was systematically prevented. From the analytical perspective, this is a pure Snare dressed in organizational language: the silo is not inevitable or natural, but a contingent structural choice that asymmetrically benefits schedule authority at the cost of safety.
 *
 * DIRECTIONALITY LOGIC:
 *   Shuttle crew: Victim + trapped → d≈0.98, f(d)≈1.45. Maximum extraction. No knowledge, no choice, full risk. Field engineers: Victim + constrained → d≈0.88, f(d)≈1.25. High extraction. Could theoretically escalate but organizational and financial barriers made escalation prohibitively costly (contract with NASA, career risk, precedent of suppressing bad news). Mid-level management: Constrained but not victimized → d≈0.55, f(d)≈0.73. Mixed position: they were constrained by organizational structure but also participated in the filtering mechanism. NASA senior leadership: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Could access information through alternative channels (direct technical briefings) but organizational structure made such channels exceptional. Public/epistemic commons: Victim + trapped → d≈0.85-0.97, f(d)≈1.18-1.43. High to maximum extraction. No exit, no information, full cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that information filtering in safety-critical organizations is not coordination (where both parties benefit from reducing noise) but extraction (where one party benefits from suppressing information). The silo could have been framed as a coordination mechanism — 'managers need focus, engineers need to solve technical problems, hierarchies reduce decision noise' — but the asymmetric distribution of risk (beneficiary insulated from consequences; victims bear full risk without knowledge) transforms this into pure Snare. The Rogers Commission resolved the mandatrophy by showing that the silo existed specifically to maintain schedule credibility despite known safety risks. Engineers could have escalated; the structure prevented it. Leaders could have requested critical data; the structure kept it invisible. The crew could have been informed; the structure withheld information. Each suppression was individually defensible (reducing noise, maintaining hierarchy, respecting organizational boundaries) but collectively formed an extraction mechanism. The constraint is not ambiguous between Rope and Snare — it is unambiguously Snare from every perspective except the beneficiary's, and even the beneficiary's Rope experience requires treating the silo as a legitimate coordination partition rather than as an information control device.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    schedule_pressure_causality,
    'To what degree did launch schedule pressure causally drive the filtering mechanism, versus the silo existing independent of schedule dynamics?',
    'Historical reconstruction of internal NASA decision sequences; comparison of filter intensity against schedule pressure curves; counterfactual analysis of whether the silo would have existed absent launch cadence pressure',
    'If causally driven by schedule: constraint is Snare with extractive purpose (higher ε, higher suppression). If independent: silo is Piton (institutional inertia). If mixed: classification remains Snare but mandate_force analysis must account for two separate mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(schedule_pressure_causality, empirical, 'Degree to which schedule pressure caused the filtering mechanism').

omega_variable(
    field_engineer_agency_boundaries,
    'At what decision level could field engineers have effectively escalated O-ring concerns to circumvent the silo, and why was this boundary enforced?',
    'Reconstruction of available escalation channels; analysis of incentive structures (career risk, contract dependency) that made escalation prohibitively costly; identification of which decision points had alternative institutional paths',
    'If escalation was structurally impossible: suppression ≥0.80 (cage classification). If technically possible but incentivized against: suppression=0.72 (snare). This affects whether victims had any theoretical exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_engineer_agency_boundaries, empirical, 'Whether field engineers had escalation channels to bypass silo').

omega_variable(
    information_asymmetry_intentionality,
    'Was the silo an intentional information control structure, or a consequence of compartmentalization for legitimate organizational reasons that happened to create asymmetric risk?',
    'Analysis of decision logs and testimony regarding whether leaders explicitly chose filtering versus whether filtering emerged as an unintended consequence of organizational design; examination of what information would have flowed absent the silo boundaries',
    'If intentional: constraint is active Snare with institutional malice (ε≥0.62, suppression≥0.75). If unintended but foreseeable: Snare with negligence (ε≈0.58, suppression≈0.72). If genuinely unanticipated: classification may degrade to Tangled Rope (mixed coordination failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_intentionality, conceptual, 'Whether the silo was intentionally designed or emerged unintentionally').

omega_variable(
    post_rogers_structural_change,
    'Did institutional changes after Rogers Commission actually eliminate the silo mechanism, or did they layer new oversight theater atop the same filtering structure?',
    'Analysis of NASA decision processes post-1986; tracking of safety data propagation pathways; comparison of pre- and post-reform technical communication networks; audit of whether critical safety data now reaches decision-makers or remains filtered',
    'If eliminated: the constraint is historical (ended). If persisted with new theater: constraint evolved from Snare to Piton (performance persists, function degraded). If unchanged: current constraint is still active Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_rogers_structural_change, empirical, 'Whether post-Rogers reforms actually eliminated the silo or added theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_institutional_analysis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_tr_t0, rogers_commission_institutional_analysis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rogers_tr_t5, rogers_commission_institutional_analysis, theater_ratio, 5, 0.55).
narrative_ontology:measurement(rogers_tr_t10, rogers_commission_institutional_analysis, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(rogers_be_t0, rogers_commission_institutional_analysis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rogers_be_t5, rogers_commission_institutional_analysis, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rogers_be_t10, rogers_commission_institutional_analysis, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_institutional_analysis, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_institutional_analysis, nasa_schedule_metrics_supremacy).
narrative_ontology:affects_constraint(rogers_commission_institutional_analysis, organizational_hierarchy_risk_filtering).
narrative_ontology:affects_constraint(rogers_commission_institutional_analysis, contractor_dependency_information_asymmetry).

% DUAL FORMULATION NOTE:
% The Silent Safety Silo is downstream of deeper structural constraints: the institutional priority of schedule metrics over safety metrics, the organizational hierarchy that filters risk information, and the contractor dependency that makes escalation costly. Each upstream constraint has its own ε value reflecting its structural basis; the silo has ε=0.58 reflecting the intensity of information suppression and the beneficiary's extraction of schedule reliability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
