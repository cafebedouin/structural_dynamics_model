% ============================================================================
% CONSTRAINT STORY: challenger_thermal_protection_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_challenger_thermal_protection_failure, []).

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
 *   constraint_id: challenger_thermal_protection_failure
 *   human_readable: Challenger Thermal Protection System Failure and Organizational Extraction
 *   domain: aerospace/organizational_failure
 *
 * SUMMARY:
 *   The Space Shuttle Challenger disaster on January 28, 1986, resulted from
 *   O-ring thermal protection failure in the right solid rocket booster
 *   during launch from Kennedy Space Center. The structural constraint was
 *   not the physics of O-ring material—that was well-understood—but the
 *   organizational suppression of known thermal vulnerability data that
 *   prevented the constraint from becoming part of the launch decision.
 *   Morton Thiokol engineers had documented that O-ring resilience degraded
 *   below 53°F; temperatures at launch were 36°F. This information was known
 *   to engineering teams but systematically excluded from the formal launch
 *   authorization chain through hierarchical override and decision-authority
 *   fragmentation. The constraint operated through extractive suppression:
 *   crew and public bore the catastrophic cost while organizational actors
 *   (NASA management, contractor leadership) maintained schedule and
 *   preserved institutional credibility through the decision-suppression
 *   mechanism. The failure killed seven astronauts and demonstrated that the
 *   constraint was not an immutable feature of space exploration but a
 *   contingent organizational dysfunction—a Snare, not a Mountain.
 *
 * KEY AGENTS:
 *   - Astronaut Crew (Challenger): Primary victims (powerless/trapped) — seven astronauts with no practical ability to exit mission once assigned; bore maximum extraction cost
 *   - Morton Thiokol Engineers: Secondary victims (moderate/constrained) — possessed thermal risk data but faced career penalty for escalating concerns; constrained by organizational hierarchy
 *   - NASA Leadership (Marshall Space Flight Center, Kennedy Space Center): Primary beneficiary (institutional/arbitrage) — maintained schedule, preserved appearance of program success, extracted legitimacy from continued operations
 *   - NASA/Contractor Hierarchy: Institutional actors (institutional/arbitrage) — benefited from schedule maintenance and avoided admitting known thermal vulnerability
 *   - Public Trust in Space Program: Tertiary victim (powerless/trapped) — could not exit; bore damage to institutional legitimacy and public confidence
 *   - Engineering Integrity Culture: Diffuse victim (powerless/trapped) — the constraint extracted from the broader culture by demonstrating that technical dissent could be overridden by administrative authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(challenger_thermal_protection_failure, 0.88).
domain_priors:suppression_score(challenger_thermal_protection_failure, 0.92).
domain_priors:theater_ratio(challenger_thermal_protection_failure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(challenger_thermal_protection_failure, extractiveness, 0.88).
narrative_ontology:constraint_metric(challenger_thermal_protection_failure, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(challenger_thermal_protection_failure, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(challenger_thermal_protection_failure, snare).
narrative_ontology:human_readable(challenger_thermal_protection_failure, "Challenger Thermal Protection System Failure and Organizational Extraction").
narrative_ontology:topic_domain(challenger_thermal_protection_failure, "aerospace/organizational_failure").

% --- Structural relationships ---
narrative_ontology:constraint_victim(challenger_thermal_protection_failure, astronaut_crew).
narrative_ontology:constraint_victim(challenger_thermal_protection_failure, space_program_public_trust).
narrative_ontology:constraint_victim(challenger_thermal_protection_failure, engineering_integrity_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASTRONAUT CREW (SNARE) — Trapped by mission assignment; no practical exit option once committed to flight. Bears maximum extraction cost (loss of life). The constraint operates through suppression of dissenting information — engineers' concerns about O-ring performance in cold temperatures were systematized out of the decision chain. The crew had no access to complete technical data about known risks.
constraint_indexing:constraint_classification(challenger_thermal_protection_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC TRUST IN SPACE EXPLORATION (SNARE) — Cannot exit; bears the cost of legitimacy damage and erosion of confidence in institutional competence. The constraint extracts public legitimacy and confidence through catastrophic failure traceable to organizational dysfunction rather than technical limits.
constraint_indexing:constraint_classification(challenger_thermal_protection_failure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ENGINEERING COMMUNITY (TANGLED ROPE) — Constrained by organizational hierarchy and career risk. Engineers genuine coordination function: preventing catastrophic failure through rigorous analysis. But the constraint embeds asymmetric extraction: dissenting engineers face career penalty; their warnings are suppressed by management pressure to maintain launch schedule. Both coordination (safety analysis) and extraction (silencing dissent) present.
constraint_indexing:constraint_classification(challenger_thermal_protection_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NASA MANAGEMENT AND CONTRACTOR HIERARCHY (ROPE) — Experiences the constraint as coordination: launch readiness certification, schedule maintenance, safety protocol enforcement. Net beneficiary from maintained schedule and appearance of control. Extraction flows toward this agent through cost-shifting (risk externalized to crew and public). Theater reflects the performative safety review process that bypassed actual technical concerns.
constraint_indexing:constraint_classification(challenger_thermal_protection_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SPACE RACE INSTITUTION (PITON) — At civilizational scale, the space race itself had become vestigial by 1986. The Shuttle program was marketed as a routine, economical vehicle but was operationally complex, unreliable, and expensive. The constraint reflects institutional inertia: continued launch schedule despite technical risk because the institution's identity and funding depended on projecting success. Theater_ratio high because much of the safety review process was performative — designed to appear rigorous rather than to actually halt launches when risks were unacceptable.
constraint_indexing:constraint_classification(challenger_thermal_protection_failure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (APPARENT MOUNTAIN, FALSE SUMMIT) — From a universal/analytical perspective, one might argue that thermal protection failure is an inherent property of complex aerospace systems: no material perfect seals at extreme temperatures, risk is irreducible. However, this naturalizes what was actually a contingent organizational choice: the constraint that failed was not the physics of O-rings but the organizational suppression of known thermal vulnerability data. The 'mountain' framing is a false summit masking extractive institutional dysfunction.
constraint_indexing:constraint_classification(challenger_thermal_protection_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(challenger_thermal_protection_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(challenger_thermal_protection_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(challenger_thermal_protection_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(challenger_thermal_protection_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(challenger_thermal_protection_failure, TR),
    TR >= 0.70.

:- end_tests(challenger_thermal_protection_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The constraint extracted from the crew directly (loss of life) and from the public (legitimacy damage). The measurement trajectory shows increasing extractiveness leading up to the catastrophe—as launch approaches, the pressure to suppress dissenting concerns intensified. Final value reflects the maximal cost extraction at the decision point. Suppression (0.92): Extreme. The constraint operated through systematic suppression of known thermal risk data. Engineers had documented the risk; management hierarchies prevented this information from reaching formal decision-making authority. Suppression took multiple forms: organizational silence protocols, hierarchical override of technical dissent, fragmentation of information routing. Theater ratio (0.65): Moderately high and increasing. The safety review process that approved Challenger's launch was substantially performative. Engineers' concerns were documented but not formally presented to the Flight Readiness Review. Management assertions of safety satisfied the authorization ritual without engaging technical substance. Theater increased as launch approached because maintaining schedule required appearing to have conducted rigorous safety review while actually circumventing it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival gap. The astronaut crew perceived imminent, unavoidable catastrophe within minutes of launch failure—a Snare at biographical/trapped. NASA management perceived routine mission operations—a Rope at institutional/arbitrage. Engineers perceived both coordination (preventing catastrophe through analysis) and suppression (their warnings ignored)—Tangled Rope at moderate/constrained. The public perceived institutional competence—until failure revealed Snare. The analytical observer risks seeing physics-determined risk (Mountain) but discovers organizational dysfunction (Snare). The full perspectival spectrum is present because a single organizational decision (suppress thermal data) appears as inevitable coordination to beneficiaries, as catastrophic extraction to victims, and as institutional inertia to long-term observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural positions and exit options. Crew: d ≈ 1.0 (trapped victims with no alternative). Engineers with concerns: d ≈ 0.85 (mobile structurally but constrained by career risk and hierarchical authority). Management: d ≈ 0.20 (beneficiaries with arbitrage options—could have delayed launch without personal consequence, chosen not to). Public/program trust: d ≈ 0.95 (trapped, no exit from institutional dependency). The high d values for trapped agents combine with institutional suppression to generate maximal effective extractiveness χ. Management's low d reflects their beneficiary status—they bear minimal cost of the suppression mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint classifies as Snare from victims' perspectives (crew, public, engineers) and as Rope from beneficiaries' perspectives (NASA management). The contradiction is resolved by recognizing that both classifications are correct—they describe the same constraint from structurally different positions. The beneficiary experiences coordination (safety review, schedule management); the victim experiences extraction (risk externalized, dissent suppressed). The post-hoc classification is unambiguous Snare: extractiveness > 0.46, suppression ≥ 0.60, χ ≥ 0.66, minimal coordination benefit, existence depends on suppressing information. The historical record makes clear that the organizational choice to suppress thermal data was not inevitable—alternative choices were available to management—which means the constraint was maintained through active enforcement of the suppression mechanism, not through natural law. Mandatrophy resolved: this is a Snare, and the mandatrophy signal (beneficiary sees coordination, victim sees extraction) is diagnostic of why it became classified as Snare rather than ambiguously tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_suppression_mechanism,
    'Was the suppression of engineering warnings a deliberate cover-up or emergent organizational dysfunction?',
    'Documentary evidence from internal NASA communications, deposition testimony, and organizational decision logs during the pre-launch period. Examination of whether suppression was centralized directive or distributed incentive structure.',
    'If deliberate: criminal liability and institutional capture. If emergent: reveals structural incentive misalignment requiring cultural reform. Classification remains Snare either way, but remediation differs fundamentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_suppression_mechanism, empirical, 'Whether suppression of engineering data was deliberate or emergent').

omega_variable(
    thermal_risk_knowability,
    'Was O-ring thermal vulnerability genuinely unknown to leadership or was the knowledge systematically excluded from decision authority?',
    'Timeline of thermal performance data availability; mapping of who possessed data vs who participated in launch decision; analysis of information routing in the organizational hierarchy.',
    'If unknown: reduces institutional culpability and suggests technical limits. If knowable but excluded: demonstrates extractive organizational structure where information flow is suppressed to maintain schedule. Snare classification confirmed either way, but the source of the constraint shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(thermal_risk_knowability, empirical, 'Was O-ring thermal risk knowable to decision-makers').

omega_variable(
    schedule_pressure_causality,
    'Did schedule pressure (external constraint) directly cause suppression of safety concerns, or was suppression rooted in organizational culture independent of schedule?',
    'Comparative analysis with other NASA programs and aerospace contractors facing similar schedule pressure; examination of whether organizations with identical external constraints produced different safety cultures; interviews with decision-makers about causal attribution.',
    'If schedule pressure directly causal: constraint is partly external (transportation of an extractive institutional environment). If cultural: constraint is endogenous to organizational dysfunction. Theater_ratio interpretation depends on whether the constraint is a temporary response to external pressure or an entrenched institutional norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(schedule_pressure_causality, empirical, 'Whether schedule pressure directly caused suppression or reflected deeper cultural dysfunction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(challenger_thermal_protection_failure, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chtp_tr_t0, challenger_thermal_protection_failure, theater_ratio, 0, 0.45).
narrative_ontology:measurement(chtp_tr_t2, challenger_thermal_protection_failure, theater_ratio, 2, 0.58).
narrative_ontology:measurement(chtp_tr_t4, challenger_thermal_protection_failure, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(chtp_be_t0, challenger_thermal_protection_failure, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(chtp_be_t2, challenger_thermal_protection_failure, base_extractiveness, 2, 0.72).
narrative_ontology:measurement(chtp_be_t4, challenger_thermal_protection_failure, base_extractiveness, 4, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(challenger_thermal_protection_failure, enforcement_mechanism).
narrative_ontology:affects_constraint(challenger_thermal_protection_failure, columbia_thermal_protection_system_degradation).
narrative_ontology:affects_constraint(challenger_thermal_protection_failure, organizational_silence_protocols).

% DUAL FORMULATION NOTE:
% The Challenger constraint family includes the immediate O-ring thermal protection physics (Mountain: material limits are immutable) and the organizational suppression structure (Snare: the decision to suppress known data was contingent). These are distinct constraints with different ε values. The physics constraint ε ≈ 0.05 (irreducible thermal physics). The organizational suppression constraint ε ≈ 0.88 (contingent choice with catastrophic extraction). The historical disaster arose from the coupling between these two constraints: organizational dysfunction turned a known technical vulnerability into a catastrophic failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(challenger_thermal_protection_failure, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
