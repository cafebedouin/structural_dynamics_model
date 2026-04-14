% ============================================================================
% CONSTRAINT STORY: police_accountability_vacuum
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_police_accountability_vacuum, []).

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
 *   constraint_id: police_accountability_vacuum
 *   human_readable: Police Accountability Vacuum
 *   domain: governance/law_enforcement/criminal_justice
 *
 * SUMMARY:
 *   The police accountability vacuum represents a structural constraint where
 *   mechanisms ostensibly designed to address misconduct—internal affairs,
 *   civilian complaint review boards, qualified immunity doctrine, union
 *   grievance procedures—function primarily to suppress alternatives to
 *   police authority rather than to coordinate legitimate enforcement with
 *   civilian oversight. The constraint exhibits the full DR typology across
 *   observer positions: abuse victims experience pure extraction (Snare);
 *   police administration experiences coordination solving (Rope); reform
 *   advocates experience mixed coordination-extraction (Tangled Rope);
 *   oversight bodies exhibit degraded function (Piton); and the analytical
 *   observer risks naturalizing the vacancy as inherent to policing (false
 *   Mountain). Base extractiveness has risen from 0.45 to 0.68 across the
 *   interval, while theater ratio has increased from 0.35 to 0.65, indicating
 *   that reform gestures are accumulating as performative overlay without
 *   corresponding reduction in the accountability gap itself—a classic
 *   Goodhart pattern where the metric (number of oversight bodies, complaint
 *   procedures, reform initiatives) substitutes for the actual function
 *   (holding misconduct to account). Suppression (0.72) reflects both
 *   material barriers to remedy (qualified immunity, union protections,
 *   discovery limitations) and epistemic suppression (reframing systemic
 *   problems as isolated officer failures, delegitimizing victims through
 *   credibility deficits, controlling the narrative through police media
 *   relations).
 *
 * KEY AGENTS:
 *   - Civilian Targets of Police Abuse: Primary victims (powerless/trapped/national scope) — face retaliation risk, credibility deficits, civil remedies that are inadequate, and jurisdictional confinement. Trapped at multiple levels: cannot safely report, cannot exit jurisdiction, cannot obtain meaningful remedy.
 *   - Community Trust (Civic Legitimacy): Structural victim (powerless/trapped/generational) — the legitimacy of law enforcement as a civic institution erodes across generations when misconduct is unaccountable. Communities cannot exit the enforcement relationship. Progressive degradation of rule-of-law perception represents extraction of institutional trust itself.
 *   - Law Enforcement Administration: Net beneficiary (institutional/arbitrage/national) — accountability vacuum enables operational autonomy, officer protection, institutional reputation management, and discretionary resource allocation. Can exit by reforming but opts to maintain the constraint because it serves internal coordination and external protection simultaneously.
 *   - Police Union Leadership: Secondary beneficiary (organized/arbitrage/national) — grievance procedures that overturn misconduct findings, contract protections that shield officers from discipline, collective power to resist external accountability. Uses accountability mechanisms as adversarial process rather than truth-seeking.
 *   - Police Reform Advocates: Secondary victim/constrained beneficiary (moderate/constrained/regional) — faces political opposition, media framing, union resistance, and fear of 'soft on crime' backlash. Also benefits from reform as career/advocacy platform. Mixed extraction: genuine coordination function (identifying real problems) coupled with asymmetric extraction from institutional resistance.
 *   - Civilian Oversight Bodies: Institutional actor exhibiting piton dynamics (institutional/arbitrage/national) — exist as theater: review procedures, complaint mechanisms, investigation protocols that generate appearance of accountability without structural power. Limited authority, access to records controlled by police, no enforcement mechanism.
 *   - Analytical Observer: Civilizational perspective at risk of false summit (analytical/analytical/universal) — naturalizes accountability vacuum as necessary to police function rather than recognizing it as contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(police_accountability_vacuum, 0.68).
domain_priors:suppression_score(police_accountability_vacuum, 0.72).
domain_priors:theater_ratio(police_accountability_vacuum, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(police_accountability_vacuum, extractiveness, 0.68).
narrative_ontology:constraint_metric(police_accountability_vacuum, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(police_accountability_vacuum, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(police_accountability_vacuum, snare).
narrative_ontology:human_readable(police_accountability_vacuum, "Police Accountability Vacuum").
narrative_ontology:topic_domain(police_accountability_vacuum, "governance/law_enforcement/criminal_justice").

domain_priors:requires_active_enforcement(police_accountability_vacuum).

% --- Structural relationships ---
narrative_ontology:constraint_victim(police_accountability_vacuum, civilian_targets_of_abuse).
narrative_ontology:constraint_victim(police_accountability_vacuum, community_trust).
narrative_ontology:constraint_victim(police_accountability_vacuum, rule_of_law_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABUSE VICTIM (SNARE) — Civilian target of police misconduct has no structural exit. Cannot leave the jurisdiction without abandoning livelihood/family. Reporting mechanisms are controlled by the enforcement apparatus itself. Face retaliation risk, credibility deficits in court, and minimal remedy. Maximum experienced extraction — trapped by jurisdiction and by control of accountability mechanisms.
constraint_indexing:constraint_classification(police_accountability_vacuum, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY TRUST (SNARE, GENERATIONAL) — The civic commitment to rule of law and police legitimacy is progressively degraded by unaccountable misconduct. Communities cannot exit the relationship with law enforcement. The trust relationship deteriorates across generations — generational time horizon reveals the extraction of institutional legitimacy itself. Suppression maintained through reframing accountability gaps as individual officer failures rather than structural constraints.
constraint_indexing:constraint_classification(police_accountability_vacuum, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT ADMINISTRATION (ROPE) — Experiences the constraint as coordination mechanism for internal order and operational autonomy. Accountability vacuum enables rapid decision-making, officer morale maintenance, and protection of institutional reputation. Sees the constraint as solving a coordination problem: how to maintain unit cohesion and public confidence while preserving officer discretion. Net beneficiary through arbitrage — can exit regulatory capture by reforming, but opts not to.
constraint_indexing:constraint_classification(police_accountability_vacuum, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICE REFORM ADVOCATES (TANGLED ROPE) — Constrained by political power of law enforcement unions, media framing, and fear of 'soft on crime' backlash. Also benefit from accountability mechanisms as a framework for career advancement (civil rights law, policy work, advocacy platforms). Mixed experience: genuine coordination function (fixing real problems) coupled with asymmetric extraction from police institutions that resist reform while activists bear reputational cost.
constraint_indexing:constraint_classification(police_accountability_vacuum, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CIVILIAN OVERSIGHT BODIES (PITON) — Nominally independent review boards, internal affairs divisions, and civilian complaint mechanisms exist but have degraded function. Review processes are performative: limited investigative authority, access to records controlled by police, no power to compel discipline, and minimal teeth in recommended penalties. Theater ratio (0.65) reflects that accountability theater persists through institutional inertia rather than functional capacity. The constraint maintains itself through the existence of oversight structures that generate appearance of accountability without structural power.
constraint_indexing:constraint_classification(police_accountability_vacuum, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN, FALSE SUMMIT) — Risk of naturalizing the accountability vacuum as inherent to law enforcement: 'Police need operational discretion.' 'Accountability mechanisms always lag enforcement action.' 'Institutional loyalty is necessary for unit cohesion.' These framings appear as natural constraints on police governance but are actually contingent institutional choices. The mountain classification here is a false summit — the engine's structural analysis reveals this perspective naturalizes extractive arrangements.
constraint_indexing:constraint_classification(police_accountability_vacuum, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(police_accountability_vacuum_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(police_accountability_vacuum, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(police_accountability_vacuum, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(police_accountability_vacuum, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(police_accountability_vacuum, TR),
    TR >= 0.70.

:- end_tests(police_accountability_vacuum_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High, reflecting that the accountability vacuum enables systematic extraction from abuse victims and communities through multiple mechanisms: (1) immunity from civil liability (qualified immunity), (2) union protection from discipline, (3) control of investigation process by police institutions, (4) retaliation risk, (5) credibility deficits assigned to civilian complainants. The rise from 0.45 to 0.68 across the interval reflects not the closure of the vacuum but its deepening through accumulating institutional protections and narrative capture. Suppression (0.72): High. Multiple mechanisms suppress accountability: qualified immunity doctrine, union grievance procedures that reverse findings, limited investigative authority of civilian boards, discovery limitations, media control, retaliation risk, and epistemic suppression (reframing systemic problems as isolated officer misconduct). Theater ratio (0.65): Substantial and rising. The interval shows accumulating reform measures—new oversight boards, de-escalation training, use-of-force policies, complaint procedures—that generate appearance of accountability while misconduct rates remain stable or rising. This is diagnostic of theater accumulation: institutional theater substitutes for actual accountability, creating Goodhart drift where the metric (number of policies) diverges from the outcome (actual reduction of misconduct and victim remedy).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. Abuse victims see Snare: pure extraction with no coordination benefit and no exit. Police administration sees Rope: coordination mechanism solving legitimate problems of internal order and discretion protection. Reform advocates see Tangled Rope: genuine coordination function (identifying and addressing real misconduct) coupled with asymmetric extraction from institutional resistance. Oversight bodies exhibit Piton: degraded function, performative processes, maintained through inertia. Communities experience Snare at generational scale: the institutional legitimacy necessary for rule of law is progressively extracted through unaccountable misconduct. The analytical observer risks Mountain by naturalizing the vacancy as inherent to police authority. The perspectival gap reveals that 'accountability mechanisms' mean opposite things to different observers: to victims, they mean suppression (controlled processes that never produce remedy); to police, they mean coordination (structures that enable discretion); to reform advocates, they mean genuine but asymmetric function (real coordination coupled with real resistance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Abuse victims are trapped at high d (~0.95): no exit options, full cost-bearing, no arbitrage available. Experienced extractiveness is maximum. Police administration is at low d (~0.10): institution controls the constraint, benefits from arbitrage (can exit by reforming but opts not to), experiences negative extractiveness (coordination benefit). Reform advocates are at moderate-high d (~0.68): constrained by institutional opposition, bear reputational cost, but also benefit from advocacy platform. Communities are at high d (~0.90) at generational horizon: cannot exit the enforcement relationship, bear costs of eroded legitimacy across generations. The piton perspective (institutional/arbitrage) derives low d (~0.15) for oversight bodies that nominally benefit from the appearance of oversight authority, though their actual function is degraded. The mountain perspective (analytical/analytical) lacks meaningful d because it naturalizes the constraint, treating it as a feature of policing rather than a structural extraction mechanism—this is precisely where false summit detection fires.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint produces structural extraction that persists despite reform theater accumulation. The classical mandatrophy error would be: 'We have civilian oversight boards and complaint procedures—therefore accountability is functioning—therefore the constraint is Rope (coordination), not Snare (extraction).' The false resolution naturalizes institutional theater as institutional function. The actual mandatrophy resolution shows that: (1) the rising theater_ratio (0.35 → 0.65) indicates metric substitution (number of procedures substitutes for actual accountability), (2) the rising base_extractiveness (0.45 → 0.68) despite reform measures indicates that extraction and theater are moving together (more oversight appearance correlates with worse outcomes), (3) the perspectival gap is not a measurement problem but a structural reality—the same processes that appear as coordination to police appear as extraction suppression to victims, (4) the snare classification holds because the constraint's existence relies on suppressing the alternative (independent civilian authority with enforcement power), and (5) the institutional theater (piton perspective) is the mechanism that sustains the snare by providing appearance of accountability without substance. The mandatrophy is dissolved by recognizing that theater accumulation is itself part of the extraction mechanism—each new oversight procedure that lacks enforcement power sustains the vacuum by making the victim's plea that 'the system is responding' false but durable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qualified_immunity_necessity,
    'Does qualified immunity represent a structurally necessary protection for discretionary law enforcement or a mechanism enabling extraction through suppression of accountability?',
    'Comparative analysis of jurisdictions with vs without qualified immunity analogs; empirical correlation between immunity doctrine and misconduct rates; cross-national data on officer safety vs accountability',
    'If necessary: extractiveness drops to 0.35, classification becomes Tangled Rope. If extraction mechanism: extractiveness confirmed at 0.68, Snare classification holds. Impact determines whether the constraint is fundamental to law enforcement or contingent to U.S. legal architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qualified_immunity_necessity, empirical, 'Whether qualified immunity is structurally necessary or extractively enabling').

omega_variable(
    union_power_countervailing,
    'Does police union power represent a countervailing force against civilian authority erosion or a structural mechanism protecting the accountability vacuum?',
    'Analysis of disciplinary outcomes in union vs non-union jurisdictions; cases where union grievance procedures reversed sustained misconduct findings; comparative institutional analysis of union veto power over reform',
    'If countervailing: moderate beneficiary benefit (unions provide job security for falsely accused officers), suppression moderates. If protective mechanism: suppression and extraction are reinforced by union power. Determines whether union participation in reform is viable or contradictory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_power_countervailing, empirical, 'Whether police unions counteract or enable accountability vacuum').

omega_variable(
    community_exit_capacity,
    'Can communities with high police misconduct rates actually exit or form alternative enforcement mechanisms, or is the exit capacity sufficiently constrained that trapped status is appropriate for generational time horizons?',
    'Case studies of communities attempting alternative security models (community patrols, unarmed responders, mutual aid); analysis of jurisdictional mobility barriers (housing markets, family ties, economic opportunity); cost-benefit analysis of relocation vs enduring misconduct',
    'If exit is possible: exit_options for community may upgrade from trapped to constrained, altering classification and experienced extraction. If exit is illusory: trapped status confirmed, snare classification solidified across generational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_exit_capacity, empirical, 'Whether communities can exit accountability vacuum or are structurally trapped').

omega_variable(
    reform_authenticity_erosion,
    'Do police reform measures actually reduce misconduct and accountability gaps or do they primarily function as theater that sustains the vacuum by providing appearance of change?',
    'Longitudinal analysis of misconduct rates before/after major reform initiatives; comparison of rhetoric vs outcomes in use-of-force policy, de-escalation training, and discipline procedures; measurement of civilian complaint resolution rates and remedy adequacy',
    'If reforms are authentic: theater_ratio should decline to 0.35 over time, constraint degrades toward Rope or Scaffold. If reforms are theater: theater_ratio increases or plateaus, constraint hardens into Piton or Snare. Determines whether reform movements are structural or captured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_authenticity_erosion, empirical, 'Whether police reforms reduce misconduct or constitute institutional theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(police_accountability_vacuum, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(policeacct_tr_t0, police_accountability_vacuum, theater_ratio, 0, 0.35).
narrative_ontology:measurement(policeacct_tr_t10, police_accountability_vacuum, theater_ratio, 10, 0.52).
narrative_ontology:measurement(policeacct_tr_t20, police_accountability_vacuum, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(policeacct_be_t0, police_accountability_vacuum, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(policeacct_be_t10, police_accountability_vacuum, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(policeacct_be_t20, police_accountability_vacuum, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(police_accountability_vacuum, enforcement_mechanism).
narrative_ontology:affects_constraint(police_accountability_vacuum, qualified_immunity_doctrine).
narrative_ontology:affects_constraint(police_accountability_vacuum, police_union_grievance_procedures).
narrative_ontology:affects_constraint(police_accountability_vacuum, civilian_complaint_suppression).

% DUAL FORMULATION NOTE:
% The police accountability vacuum is a constraint family decomposing into: (1) qualified_immunity_doctrine (ε=0.62, provides liability shield), (2) police_union_grievance_procedures (ε=0.55, reverses sustained findings), (3) civilian_complaint_suppression (ε=0.48, controls investigation). The vacuum story represents the integrated constraint; disaggregation enables analysis of which components drive the extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
