% ============================================================================
% CONSTRAINT STORY: quellcrist_falconer_justice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quellcrist_falconer_justice, []).

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
 *   constraint_id: quellcrist_falconer_justice
 *   human_readable: The Machinery of Justice (Quellist)
 *   domain: political/criminal_justice
 *
 * SUMMARY:
 *   The machinery of justice, as understood by Quellcrist Falconer's
 *   analysis, is a constraint that extracts compliance, wealth, time, and
 *   dignity from the powerless under the rhetorical cover of coordination
 *   toward social order. The system claims to protect rights and establish
 *   predictable dispute resolution, but structural features — resource
 *   asymmetry between prosecution and defense, poverty-driven plea
 *   bargaining, bail systems targeting the poor, overcriminalization of
 *   survival activities, and bureaucratic complexity — systematize extraction
 *   from those least able to resist. The constraint operates across multiple
 *   institutional levels: police enforcement (selective prosecution),
 *   prosecutorial discretion (leverage for plea), court procedures
 *   (complexity as barrier), correctional systems (debt peonage through fines
 *   and fees), and reentry barriers (permanent legal disabilities).
 *   Quellcrist's framing emphasizes that the machinery is not a Bug but a
 *   Feature — it extracts precisely because it is designed to, camouflaged by
 *   legitimacy rhetoric. The increasing theater ratio (0.65) reflects the
 *   growing decoupling of procedural ritual from actual justice outcomes.
 *
 * KEY AGENTS:
 *   - Defendants Without Resources: Primary victims (powerless/trapped) — bear maximum extraction; exit blocked by bail systems and procedural complexity
 *   - Marginalized Communities: Secondary victims (moderate/constrained) — over-policed, under-protected, asymmetrically prosecuted; geographic and economic barriers to exit
 *   - State Enforcement Bodies: Primary beneficiaries (institutional/arbitrage) — capture plea bargains, fines, civil asset forfeiture, and compliance; exit through case dismissal or non-prosecution
 *   - Institutional Legal Apparatus: Primary beneficiaries (institutional/arbitrage) — preserve jurisdictional power, resource dependency, and professional monopolies; arbitrage through case selection and discretion
 *   - Reform Coalitions: Secondary actors (organized/constrained) — observe and resist pure extraction; constrained exit through system capture and incrementalism
 *   - Legal Profession: Tertiary actors (institutional/arbitrage) — maintain theater through procedural complexity, licensing barriers, and continuing education; profit from defendant desperation
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees machinery as designed extraction subordinating justice to bureaucratic extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quellcrist_falconer_justice, 0.68).
domain_priors:suppression_score(quellcrist_falconer_justice, 0.72).
domain_priors:theater_ratio(quellcrist_falconer_justice, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quellcrist_falconer_justice, extractiveness, 0.68).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quellcrist_falconer_justice, snare).
narrative_ontology:human_readable(quellcrist_falconer_justice, "The Machinery of Justice (Quellist)").
narrative_ontology:topic_domain(quellcrist_falconer_justice, "political/criminal_justice").

domain_priors:requires_active_enforcement(quellcrist_falconer_justice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quellcrist_falconer_justice, institutional_legal_apparatus).
narrative_ontology:constraint_beneficiary(quellcrist_falconer_justice, state_enforcement_bodies).
narrative_ontology:constraint_victim(quellcrist_falconer_justice, defendants_without_resources).
narrative_ontology:constraint_victim(quellcrist_falconer_justice, marginalized_communities).
narrative_ontology:constraint_victim(quellcrist_falconer_justice, accused_individuals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The accused without financial resources faces the full machinery: legal costs are prohibitive, public defenders are overburdened, bail systems extract desperation, and the procedural complexity itself becomes an extraction mechanism. The defendant is trapped within the system — any attempt to exit through flight, non-compliance, or informal settlement increases enforcement. Maximum structural extraction.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Communities over-policed and under-protected experience the machinery as both extraction and (theoretically) coordination. The justice system claims to provide order and protection, but enforcement is asymmetric: minor infractions are prosecuted aggressively while harms within the community go unsolved. Exit is constrained by geography, economic dependency, and the threat of arrest.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Courts and judges experience the machinery as a coordination mechanism: establishing dispute resolution procedures, enforcing property rights, and maintaining legal predictability. The institutional view sees the system as functional coordination despite acknowledged inefficiency. Judges have substantial arbitrage options — case selection, sentencing discretion, case prioritization — that insulate them from the full constraints the system imposes on defendants.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Legal aid organizations, prisoner advocates, and reform coalitions observe pure extraction obscured by coordination rhetoric. They see the system as designed to extract compliance, wealth (through fines, court fees, bail bonds), and time from the powerless while preserving institutional legitimacy through appearance of fairness. The machinery subordinates justice to bureaucratic efficiency. Organized groups have constrained exit — they can push for reform but cannot escape the system's fundamental logic.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Law as a profession increasingly maintains theater rather than justice: bar associations preserve licensing barriers, continuing education requirements, and fee structures that protect legal monopolies rather than improve outcomes. The procedural apparatus (motions, discovery, appeals) has become decoupled from its original function of ensuring fairness. Theater ratio high — much of the machinery is performative ritual divorced from actual justice determination.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a universal perspective, the machinery of justice is a snare: it extracts compliance, resources, and dignity from the powerless through the threat of escalating coercion. The system claims to coordinate social order and protect rights, but structural features (poverty-driven plea bargaining, resource asymmetries, bureaucratic rationalization) guarantee systematic extraction from the vulnerable. The machinery operates as designed — the design itself is the constraint.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quellcrist_falconer_justice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quellcrist_falconer_justice, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quellcrist_falconer_justice, TR),
    TR >= 0.70.

:- end_tests(quellcrist_falconer_justice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The machinery systematically transfers wealth (bail, fines, court fees, restitution), time (incarceration, pretrial detention, legal proceedings), compliance (through threat of escalation), and dignity (through criminal record, surveillance, and permanent legal disabilities) from the powerless to the state and legal profession. The extraction is not incidental — it is structurally necessary to the system's operation. Poverty-driven plea bargaining, bail systems, and court fees are not bugs but mechanisms that extract and subordinate. Suppression (0.72): High. Exit options are severely restricted: plea bargains extract confession under risk asymmetry, bail systems extract wealth or freedom, procedural complexity creates dependency on professionals, and criminal records create permanent barriers. Alternatives (community justice, informal resolution) are suppressed through legal monopolies and criminalization of exit strategies. Theater ratio (0.65): Moderate-high and rising. Procedural rituals (trials, motions, appeals) are increasingly decoupled from justice outcomes — outcomes are determined by resource availability and prosecutorial leverage before trial procedures commence. The machinery maintains legitimacy through spectacle (public trials, judicial robes, legal rhetoric) while actual justice is subordinated to extraction efficiency.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark and reveals the machinery's fundamental asymmetry. The institutional legal apparatus experiences coordination (Rope) — establishing order, processing disputes, maintaining legitimacy. The analytical observer sees pure extraction (Snare) — the same system, described identically, but from the position of those it subordinates. The marginalized community sees hybrid extraction with some coordination (Tangled Rope) — the system offers theoretical protection but asymmetrically enforces it. The impoverished defendant sees maximum extraction (Snare) — no coordination benefit, no exit option. The reform coalition sees degraded ritual (Piton) — the machinery persists through institutional inertia despite its failure to deliver justice. The professional legal system (Piton) sees its own procedures as increasingly theater. This perspectival gap is not a measurement ambiguity — it reflects genuine structural inequality. The machinery operates as designed: it coordinates order for the powerful while extracting from the powerless. The gap between 'justice system' rhetoric and 'extraction mechanism' reality is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position within the extraction flow. The powerless defendant (d ≈ 0.95) bears maximum extraction with zero exit capacity — trapped by bail, plea pressure, and procedural complexity. The institutional apparatus (d ≈ 0.05) benefits from the extraction and maintains arbitrage options (discretion in prosecution, case selection, sentencing). Marginalized communities (d ≈ 0.75) are caught between over-policing and under-protection — partially trapped, partially targeted. The organized reform coalition (d ≈ 0.80) sees the extraction clearly but remains constrained within the system — they can advocate but not escape or fundamentally reshape. The analytical observer (d ≈ 1.0) from a civilizational perspective sees the machinery as designed extraction without redemptive coordination function. Beneficiary declarations (state enforcement, institutional legal apparatus) and victim declarations (powerless defendants, marginalized communities) are unambiguous — the extraction flow is explicitly from bottom to top, from weak to institutional.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The machinery of justice is NOT a hidden coordination mechanism misconstrued as snare, and is NOT a snare misconstrued as coordination. It is structurally a snare — it extracts from the powerless and subordinates justice to bureaucratic efficiency. The mandatrophy resolution requires rejecting the false middle: the system is not a hybrid (tangled rope) or temporary (scaffold) or degraded (piton). It is a pure extraction mechanism operating with institutional legitimacy. The theoretical coordination function (dispute resolution, rights protection, social order) is real but systematically skewed toward institutional power. For the powerless, the coordination is null — they experience only extraction and suppression. The machinery resolves the mandatrophy by being honest: it is a snare for defendants without resources, a rope for those with institutional protection, and a piton (degraded theater) for the legal profession itself. All perspectives converge on extraction, differing only in intensity. The reform movement's constrained option to work within the system is not an exit — it is a contained alternative, itself extracted into legitimacy service. Mandatrophy fully resolved at extractiveness 0.68.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plea_bargain_coercion_threshold,
    'At what conviction probability does the risk-asymmetry in plea bargaining cease to be coordination (defendant accepting modest penalty to avoid trial risk) and become pure extraction (defendant coerced into confessing to crimes they may not have committed)?',
    'Empirical analysis of plea rates by conviction likelihood; psychological studies of rational choice under threat; innocence project data on exonerations of plea-convicted defendants',
    'If threshold is low (< 60% conviction probability): plea systems extract confessions from the innocent. If threshold is high (> 85%): plea systems remain rational coordination even for marginal cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plea_bargain_coercion_threshold, empirical, 'Threshold separating rational plea coordination from coercive extraction').

omega_variable(
    bail_system_function,
    'Does the bail system function as a coordination mechanism ensuring defendant appearance (rational purpose) or as a wealth extraction mechanism punishing pre-conviction poverty?',
    'Comparative analysis of bail policy jurisdictions; correlation between bail amounts and defendant flight risk; data on bail nonpayment as proxy for poverty vs predictive risk; jurisdictions with risk-based release showing appearance rates',
    'If coordination: bail is Rope from multiple perspectives. If extraction: bail is primary extraction mechanism within the snare, and should be isolated as a separate constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bail_system_function, empirical, 'Whether bail functions as coordination or wealth extraction').

omega_variable(
    public_defender_adequacy,
    'Does public defender availability constitute genuine legal representation (satisfying coordination function) or theater masking inadequate defense (satisfying suppression via legitimacy)?',
    'Case outcome analysis controlling for defense quality; comparison of conviction rates public defender vs private counsel for identical charges; documentation of caseload ratios and time-per-case allocation',
    'If adequate: defendant protection is real, reducing experienced extraction for powerless agents. If theatrical: public defenders function as legitimacy apparatus, enabling snare operation without visible coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_defender_adequacy, empirical, 'Whether public defenders provide adequate representation or theater').

omega_variable(
    system_alternative_feasibility,
    'Is the current machinery of justice structurally necessary for social order, or is it a contingent institutional arrangement replaceable by alternative dispute resolution, restorative justice, or community-based systems?',
    'Analysis of jurisdictions with alternative justice models; comparative outcome data on safety, restitution, recidivism; feasibility studies of scaling alternatives; historical precedent analysis',
    'If necessary: machinery is partially Mountain (irreducible constraint). If contingent: machinery is pure Snare (designed extraction). Changes fundamental classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(system_alternative_feasibility, conceptual, 'Whether machinery of justice is structurally necessary or contingently extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quellcrist_falconer_justice, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quelj_tr_t0, quellcrist_falconer_justice, theater_ratio, 0, 0.48).
narrative_ontology:measurement(quelj_tr_t25, quellcrist_falconer_justice, theater_ratio, 25, 0.58).
narrative_ontology:measurement(quelj_tr_t50, quellcrist_falconer_justice, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(quelj_be_t0, quellcrist_falconer_justice, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(quelj_be_t25, quellcrist_falconer_justice, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(quelj_be_t50, quellcrist_falconer_justice, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quellcrist_falconer_justice, enforcement_mechanism).
narrative_ontology:affects_constraint(quellcrist_falconer_justice, mass_incarceration_regime).
narrative_ontology:affects_constraint(quellcrist_falconer_justice, plea_bargain_coercion).
narrative_ontology:affects_constraint(quellcrist_falconer_justice, bail_system_wealth_extraction).
narrative_ontology:affects_constraint(quellcrist_falconer_justice, policing_discretion_subordination).

% DUAL FORMULATION NOTE:
% The machinery of justice decomposes into four related snares: enforcement (police discretion), prosecutorial leverage (plea bargains), extraction mechanisms (bail and fines), and institutional subordination (sentencing disparities). Each has distinct epsilon and beneficiary structures but shares the parent constraint's suppression and theater. This story represents the macro-level constraint; network edges link to micro-level mechanisms that comprise the machinery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
