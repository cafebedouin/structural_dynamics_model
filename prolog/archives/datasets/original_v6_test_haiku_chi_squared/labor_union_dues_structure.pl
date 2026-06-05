% ============================================================================
% CONSTRAINT STORY: labor_union_dues_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_union_dues_structure, []).

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
 *   constraint_id: labor_union_dues_structure
 *   human_readable: Mandatory Union Dues-for-Grievance Mechanism
 *   domain: economic/political
 *
 * SUMMARY:
 *   The mandatory union dues structure exists to solve a classic collective
 *   action problem: individual workers cannot credibly organize grievance
 *   mechanisms without guaranteed funding, and voluntary contribution systems
 *   collapse due to free-riding. However, the same institutional mechanism
 *   that solves coordination also enables extraction: union leadership
 *   captures bargaining rent, workers cannot exit without losing grievance
 *   access, and the dues-for-benefits structure suppresses individual wage
 *   negotiation. The constraint thus exhibits a structural hybrid — it is
 *   genuinely coordinating AND genuinely extractive depending on the
 *   observer's position. The increasing theater ratio (0.32 → 0.48 over 70
 *   years) reflects that as labor markets changed and union density declined,
 *   grievance processing became increasingly ritualistic in stable sectors
 *   while dues collection infrastructure persisted. The extractiveness
 *   increase (0.35 → 0.52) reflects the accumulation of secondary extraction
 *   layers: union political spending, administrative overhead, and benefits
 *   for retirees funded by current workers. The constraint bridges the
 *   analytical divide between defenders (who emphasize collective action
 *   necessity) and critics (who emphasize extraction and suppression of
 *   individual negotiation).
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — forced into dues payments to access grievance mechanism; cannot exit without losing protection or employment
 *   - Stable Union Members: Mixed (moderate/constrained) — benefit from grievance access and wage floors but pay mandatory dues; constrained exit due to union shop agreements
 *   - Union Leadership: Primary beneficiary (organized/constrained) — captures dues income and bargaining authority; constrained by democratic accountability and legal restrictions
 *   - Non-Union Employers: Secondary victim (powerful/mobile) — face competitive wage floor imposed by union bargaining; can relocate or hire non-union labor
 *   - State Labor Apparatus: Institutional enforcer (institutional/arbitrage) — mandates union security agreements in some jurisdictions; enables grievance appeals and labor board review
 *   - Declining Union Sectors: Piton actor (institutional/constrained) — maintains dues collection infrastructure through inertia despite shrinking membership and changing labor market structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_union_dues_structure, 0.52).
domain_priors:suppression_score(labor_union_dues_structure, 0.65).
domain_priors:theater_ratio(labor_union_dues_structure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_union_dues_structure, extractiveness, 0.52).
narrative_ontology:constraint_metric(labor_union_dues_structure, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_union_dues_structure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_union_dues_structure, tangled_rope).
narrative_ontology:human_readable(labor_union_dues_structure, "Mandatory Union Dues-for-Grievance Mechanism").
narrative_ontology:topic_domain(labor_union_dues_structure, "economic/political").

domain_priors:requires_active_enforcement(labor_union_dues_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_union_dues_structure, union_leadership).
narrative_ontology:constraint_beneficiary(labor_union_dues_structure, protected_workers).
narrative_ontology:constraint_beneficiary(labor_union_dues_structure, collective_bargaining_capacity).
narrative_ontology:constraint_victim(labor_union_dues_structure, precarious_workers).
narrative_ontology:constraint_victim(labor_union_dues_structure, non_union_competitors).
narrative_ontology:constraint_victim(labor_union_dues_structure, individual_wage_negotiation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by employment conditions; must pay dues to access grievance mechanism; no practical exit if employer union-secured. Cannot negotiate individually. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(labor_union_dues_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STABLE UNION MEMBER (TANGLED ROPE) — Constrained exit (switching jobs costs union benefits); genuine coordination benefit (grievance access, bargaining power). Pays dues but receives protection and voice. d≈0.60, f(d)≈0.72, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(labor_union_dues_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNION LEADERSHIP (ROPE) — Experiences dues as coordinating mechanism for collective action. Constrained by democratic accountability to membership. Beneficiary via bargaining capacity and dues income, but genuine coordination function (grievance processing, negotiation). d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16.
constraint_indexing:constraint_classification(labor_union_dues_structure, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-UNION EMPLOYER (SNARE) — Sees union dues as extraction cost imposed on competitors in union-secured sectors. Mobile (can relocate or hire non-union). Victim of union coordination mechanism; compressed by wage floors and grievance costs in union shops. d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(labor_union_dues_structure, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE LABOR APPARATUS (TANGLED ROPE) — Institutional actor with arbitrage (can adjust legal framework). Genuine coordination interest (stable labor relations, reduced strike frequency). Also extracts via licensing, regulation, and labor board authority. Mandates union security agreements in some jurisdictions; enables grievance appeals. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.21.
constraint_indexing:constraint_classification(labor_union_dues_structure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY UNION BUREAUCRACY (PITON) — In sectors with declining membership, union dues structure persists through inertia. Theater ratio = 0.48 reflects that grievance processing has become ritualistic in stable workplaces; real disputes are fewer but dues collection infrastructure unchanged. d≈0.25, f(d)≈0.08, σ=0.9 → χ≈0.04.
constraint_indexing:constraint_classification(labor_union_dues_structure, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COLLECTIVE ACTION VIEW (MOUNTAIN) — From civilizational scale, the mandatory dues mechanism solves Olson's collective action problem: individual workers cannot credibly organize without a mandatory fee structure to ensure non-shirking. The bottleneck (free-rider prevention) appears immutable. However, base extractiveness=0.52 and suppression=0.65 contradict the mountain gate (ε ≤ 0.25, suppression ≤ 0.05). Engine flags as false summit; the constraint is contingent institutional design, not natural law.
constraint_indexing:constraint_classification(labor_union_dues_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_union_dues_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_union_dues_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_union_dues_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_union_dues_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_union_dues_structure, TR),
    TR >= 0.70.

:- end_tests(labor_union_dues_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The dues structure extracts mandatory fees from workers to fund a system they cannot exit. However, the extraction is not as severe as a pure Snare because the coordination function is genuine — workers do receive grievance access and the mechanism does protect against unilateral employer retaliation. The value reflects that extraction and coordination are mixed. Suppression (0.65): Moderate-high. Significant barriers to individual wage negotiation include legal restrictions on individual contracting in union shops, social enforcement against free-riding, and exclusion from grievance mechanisms for non-participants. However, suppression is not total — workers can still change employers (mobile exit) and growing non-union sectors provide alternatives. Theater ratio (0.48): Moderate. Grievance processing retains functional value in active disputes (genuine protection), but in stable workplaces the system becomes ritualistic. The ratio reflects this mixed state — not purely performative (like a Piton) but not purely functional either. Claimed type: Tangled Rope. The constraint has BOTH genuine coordination (grievance mechanism solves collective action problem) AND asymmetric extraction (union leadership and dues structure suppress individual negotiation and capture bargaining rent). Active enforcement is required (union security agreements enforced by State or employer). This matches the Tangled Rope gate exactly.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the maximal perspectival gap between beneficiary and victim. The union leadership and stable members see a coordination solution (Rope or Tangled Rope) — they experience genuine protection and collective voice. The precarious worker sees extraction (Snare) — mandatory dues for grievance access they may never use, with no alternative. The non-union employer sees a competitive disadvantage (Snare) — facing wage floors imposed by union bargaining. The state apparatus sees a labor stability mechanism (Tangled Rope) — reduces strike frequency and enables dispute resolution. The declining union sector sees inertial bureaucracy (Piton) — dues collection persists despite falling membership and relevance. The analytical observer tempted to see natural law (Mountain) — collective action requires mandatory funding — but the structural data (ε=0.52, suppression=0.65) reveals this as a contingent institutional choice, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit without cost. Stable union members: Mixed (benefit + constrained) → d≈0.60, f(d)≈0.72. Significant but partial extraction; genuine benefit offsets cost. Union leadership: Beneficiary + constrained → d≈0.35, f(d)≈0.30. Moderate extraction; constrained by democratic membership and legal requirements. Non-union employer: Victim + mobile → d≈0.78, f(d)≈1.08. High extraction; faces wage floor but can exit via relocation. State apparatus: Institutional + arbitrage → d≈0.40, f(d)≈0.40. Moderate-low extraction; arbitrage capacity and dual interest (stability + labor rights). Legacy bureaucracy: Piton gate driven by theater, not directionality. The engine derives d=0.25 from institutional/constrained defaults, producing low χ and piton classification (theater ≥ 0.70 triggers type override).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy via the Tangled Rope type, which explicitly combines coordination and extraction. The classical error is labeling it a 'pure collective good' (Mountain or Rope) when it is actually a 'legitimate coordination with asymmetric rent' (Tangled Rope). The mandatory dues structure genuinely solves the free-rider problem in worker organizing (coordination), but it simultaneously creates a mechanism for union leadership to extract rent and suppress individual wage negotiation (extraction). Neither view is false; both are structural features. The constraint is not being mislabeled as Rope when it should be Snare — it is legitimately both. The mandatrophy resolution lies in rejecting the binary (coordination XOR extraction) and accepting the hybrid (coordination AND extraction). The perspectival gap (Snare from precarious workers, Rope from stable members, Tangled Rope from analytical view) reflects that the same structure produces fundamentally different experienced constraints depending on one's exit options and beneficiary status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_rider_threshold_empirical,
    'What fraction of workforce must voluntarily fund grievance mechanisms before the system collapses due to free-riding?',
    'Historical data from open-shop contexts and right-to-work jurisdictions; measurement of grievance system viability vs union participation rates',
    'If threshold > 60%: mandatory dues may be extractive rent-seeking disguised as collective action. If threshold < 20%: mandatory structure is genuinely necessary to prevent free-rider collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_rider_threshold_empirical, empirical, 'Empirical free-rider threshold for grievance system sustainability').

omega_variable(
    alternative_funding_mechanisms,
    'Could grievance access be funded via sliding-scale voluntary contributions, per-use fees, or employer-paid mechanisms without mandatory collective extraction?',
    'Comparative analysis of non-union grievance systems (ombudsman, legal aid, arbitration); cost-benefit of alternative funding models in sectors with declining union density',
    'If viable alternatives exist: mandatory dues structure is choice, not necessity → classification shifts toward Snare for more perspectives. If alternatives fail: mandatory structure is structural requirement → classification stabilizes as Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_mechanisms, empirical, 'Viability of alternative grievance funding mechanisms').

omega_variable(
    extraction_disguised_as_coordination,
    'To what degree do union leadership incentives bias grievance processing or contract terms toward extraction rather than protection?',
    'Analysis of grievance win rates vs union leadership compensation structures; comparison of grievance outcomes when membership has democratic control vs when leadership has autonomy',
    'If leader incentives dominate: dues structure is extraction mechanism with thin coordination legitimacy → Snare from more perspectives. If membership control dominates: coordination function is genuine → Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_disguised_as_coordination, conceptual, 'Whether union incentive structures prioritize protection or extraction').

omega_variable(
    mandate_legal_enforceability,
    'Does the legal enforceability of union security agreements (Taft-Hartley / right-to-work variation) determine whether the dues structure is extractive?',
    'Comparative legal analysis; empirical measurement of dues extraction and worker exit in jurisdictions with vs without mandatory union security; measurement of grievance accessibility in voluntary-contribution contexts',
    'If legal mandate is primary: constraint is State-enforced Snare from worker perspective. If worker demand for grievance access is primary: constraint is Tangled Rope reflecting genuine coordination need.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_legal_enforceability, conceptual, 'Role of legal mandate in enforcing dues structure vs organic demand for grievance protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_union_dues_structure, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lud_tr_t0, labor_union_dues_structure, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lud_tr_t35, labor_union_dues_structure, theater_ratio, 35, 0.4).
narrative_ontology:measurement(lud_tr_t70, labor_union_dues_structure, theater_ratio, 70, 0.48).

% Extraction over time
narrative_ontology:measurement(lud_be_t0, labor_union_dues_structure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lud_be_t35, labor_union_dues_structure, base_extractiveness, 35, 0.48).
narrative_ontology:measurement(lud_be_t70, labor_union_dues_structure, base_extractiveness, 70, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_union_dues_structure, resource_allocation).
narrative_ontology:affects_constraint(labor_union_dues_structure, individual_wage_negotiation_suppression).
narrative_ontology:affects_constraint(labor_union_dues_structure, union_leadership_rent_capture).
narrative_ontology:affects_constraint(labor_union_dues_structure, workplace_grievance_accessibility).

% DUAL FORMULATION NOTE:
% The mandatory dues structure is downstream of collective action theory (why mandatory structures are necessary to prevent free-riding) but represents a distinct constraint on individual worker autonomy and wage negotiation. Related constraints: individual_wage_negotiation_suppression (ε≈0.48, how union shop agreements prevent individual contracting) and union_leadership_rent_capture (ε≈0.58, how leadership incentives bias bargaining toward rent extraction). The dues structure couples all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_union_dues_structure, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
