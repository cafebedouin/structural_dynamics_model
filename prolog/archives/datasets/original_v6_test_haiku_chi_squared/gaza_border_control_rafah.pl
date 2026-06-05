% ============================================================================
% CONSTRAINT STORY: gaza_border_control_rafah
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_border_control_rafah, []).

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
 *   constraint_id: gaza_border_control_rafah
 *   human_readable: Control regime over the Gaza-Egypt (Rafah) border crossing
 *   domain: geopolitical/border_control/humanitarian_access
 *
 * SUMMARY:
 *   The Rafah border crossing represents a structural constraint on movement
 *   and humanitarian access for 2 million Gaza civilians. The crossing
 *   functions as the primary exit point for the Gaza Strip, with the Israeli
 *   border sealed to civilian passage and the Mediterranean coast
 *   inaccessible. Control over the crossing is exercised through coordination
 *   between Israeli security authorities (who determine closure decisions)
 *   and Egyptian officials (who operate gates). The constraint exhibits high
 *   suppression (no viable alternatives exist) and high extractiveness
 *   (closure periods create humanitarian emergencies: blocked medical access,
 *   family separation, supply shortages). Extractiveness has increased over
 *   the measured interval as closures have become longer and more frequent,
 *   and theater ratio has risen as humanitarian justifications have become
 *   more formulaic despite operational patterns suggesting political/security
 *   discretion. The constraint classifies as Snare from the perspective of
 *   trapped populations, Egyptian state (constrained actor), humanitarian
 *   organizations, and analytical observers. No perspective yields Rope
 *   classification because the crossing's function (mere passage, not complex
 *   coordination) does not justify the suppression levels. The Israeli
 *   security establishment benefits from the extraction mechanism (maintains
 *   control, generates deterrence, leverages closure as political tool) but
 *   is not victimized by the constraint, creating a perspectival anomaly:
 *   snare classification correctly captures that this IS extraction-based
 *   coercion, but some perspectives of the snare (the beneficiaries) do not
 *   experience victimization. This reveals the precision of the DR system:
 *   snare classifies by extraction structure (high ε, high suppression,
 *   minimal coordination benefit), not by whether all parties within it are
 *   equally harmed.
 *
 * KEY AGENTS:
 *   - Gaza Civilian Population: Primary victim (powerless/trapped) — 2 million residents with no alternative exit routes; bear full humanitarian costs of closure periods
 *   - Israeli Security Establishment: Primary beneficiary and enforcer (powerful/mobile, but strategically rejects exit) — controls closure decisions, extracts deterrence and political leverage; maintains regime through military enforcement
 *   - Egyptian Government: Constrained institutional actor (institutional/constrained) — operates crossing gates but faces Israeli coordination requirements; extracts rents and security control; pays costs through refugee burden and legitimacy damage
 *   - Humanitarian Organizations: Secondary victim (moderate/constrained) — depend on crossing for operations; forced to choose between access continuity and operational presence during closures
 *   - Third-Country Nationals: Acute victim (powerless/trapped) — stranded individuals with zero negotiating power; subject to regime's discretionary closure decisions
 *   - Analytical Observer: Civilizational/structural perspective (analytical/analytical) — views crossing control as pure extraction with minimal coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_border_control_rafah, 0.68).
domain_priors:suppression_score(gaza_border_control_rafah, 0.72).
domain_priors:theater_ratio(gaza_border_control_rafah, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_border_control_rafah, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_border_control_rafah, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gaza_border_control_rafah, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_border_control_rafah, snare).
narrative_ontology:human_readable(gaza_border_control_rafah, "Control regime over the Gaza-Egypt (Rafah) border crossing").
narrative_ontology:topic_domain(gaza_border_control_rafah, "geopolitical/border_control/humanitarian_access").

domain_priors:requires_active_enforcement(gaza_border_control_rafah).

% --- Structural relationships ---
narrative_ontology:constraint_victim(gaza_border_control_rafah, gaza_civilian_population).
narrative_ontology:constraint_victim(gaza_border_control_rafah, humanitarian_organizations).
narrative_ontology:constraint_victim(gaza_border_control_rafah, palestinian_medical_patients).
narrative_ontology:constraint_victim(gaza_border_control_rafah, trapped_third_country_nationals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAZA CIVILIAN POPULATION (SNARE) — 2 million residents with no alternative exit routes (Israeli border sealed, sea access blocked). Closure periods create humanitarian crises: medical emergencies cannot reach Egyptian hospitals, students cannot access universities, families cannot reunite. Trapped exit + victim status → d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.86. Pure extraction: bear full costs of closure with zero compensation or voice.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATIONS (SNARE) — NGOs operating in Gaza depend on the crossing for staff rotation, supply delivery, and evacuation of urgent cases. Constrained exit: organizations can theoretically relocate operations, but doing so abandons their beneficiaries. Career and funding dependencies mean most staff cannot leave. Closure periods force operational suspension. d≈0.80, f(d)≈1.20, σ=0.9 → χ≈0.70. High extraction: organizations forced to choose between operational continuity and humanitarian access.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SECURITY ESTABLISHMENT (SNARE) — Primary enforcer of closure regime. Maintains operational closure authority despite international pressure. Mobile exit (could open the crossing unilaterally) is strategically rejected as incompatible with security doctrine. Maintains closure as extraction mechanism: generates deterrence value, maintains civilian population pressure, controls information flow, creates leverage over both Gaza and Egypt. d≈0.15, f(d)≈-0.01, σ=0.9 → χ≈-0.01. Structural position: beneficiary of extraction (controls exit valve); classification as Snare reflects that the extraction is coercive and suppresses alternatives, not that the security establishment is victimized. NOTE: This perspective reveals the paradox of the snare classification — snare captures that the constraint IS extraction-based (high suppression, high extractiveness), not that all parties within it are victims.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EGYPTIAN GOVERNMENT (TANGLED ROPE) — Operates the crossing gates on Egypt's side but faces severe constraints: Israeli security coordination requirements, own domestic security concerns (Sinai militants use crossing), economic pressure from tourism/trade disruption, humanitarian obligations under international law. Constrained exit: cannot unilaterally open without Israeli coordination (de facto veto). Coordination function: manages humanitarian flow, processes asylum seekers, maintains infrastructure. Extraction: tolls, visa fees, currency exchange control. Benefits from controlled crossing as revenue source and security tool; pays costs through refugee settlement burdens, economic disruption, and legitimacy damage from humanitarian crises. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.62. Mixed: genuine coordination necessity (must process flows somehow) coupled with asymmetric extraction (controls the valve, extracts rents).
constraint_indexing:constraint_classification(gaza_border_control_rafah, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRAPPED THIRD-COUNTRY NATIONALS (SNARE) — Foreigners stranded in Gaza (students, workers, aid staff) cannot exit via Israel, have limited consular support, depend entirely on Rafah crossing operational status. Multiple closure periods have trapped individuals for months. Zero negotiating power, zero alternatives. d≈0.98, f(d)≈1.44, σ=0.9 → χ≈0.88. Maximal extraction: zero agency, total dependence on regime's discretion.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a structural analytical position, the Rafah crossing control regime is a pure extraction mechanism: high suppression (no alternatives exist), high extractiveness (closure directly inflicts humanitarian costs), minimal coordination benefit (crossing's function is mere passage, not complex coordination). All classical beneficiaries (Israeli security, Egyptian state) are beneficiaries through extraction, not through genuine coordination. Theater ratio (0.58) reflects moderate performative justification: humanitarian protocols exist but are frequently violated; security rationales are stated but often contradicted by operational patterns (closures during non-crisis periods). The analytical classification is Snare because the structure exhibits no coordinate benefit that would justify the suppression — only asymmetric power and coercion. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_border_control_rafah_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_border_control_rafah, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_border_control_rafah, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_border_control_rafah, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gaza_border_control_rafah_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Closure periods directly inflict humanitarian costs on trapped populations with zero compensation or negotiation. The extractiveness has increased over the interval (0.52→0.68) as closures have become longer and more frequent, reflecting increasing use of the crossing as a control mechanism rather than functional infrastructure. The value reflects that extraction is the primary function, not a byproduct. Suppression (0.72): High. No viable alternatives exist: Israeli border is sealed to civilian passage; Mediterranean is blockaded; underground tunnels are periodically destroyed; no sea corridors function at scale. Trapped populations have zero agency in whether the crossing opens. Suppressiveness reflects that the constraint operates through elimination of choice, not through choice-distorting incentives. Theater ratio (0.58): Moderate. Humanitarian protocols exist (vulnerable categories prioritized, medical referrals processed) but are frequently suspended during closures or undermined by operational patterns (closures without stated security rationale; delays that contradict stated capacity constraints). The theater has increased over the interval (0.42→0.58) as justifications have become more routine and operational violations more frequent — indicating that performative legitimation is becoming more important as actual security justification weakens. Claimed type: Snare. Classification holds from all victim-perspective viewpoints. No beneficiary perspective yields Rope because there is no genuine coordination function — the crossing's role is mere passage, not complex coordination requiring suppression. The Israeli beneficiary's interest is in control and deterrence, not in managing collective goods.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The trapped Gaza civilian sees maximal extraction (Snare, χ≈0.86). The humanitarian organization sees constrained operation under crisis conditions (Snare, χ≈0.70). The Egyptian state sees mixed coordination necessity and extraction opportunity (Tangled Rope, χ≈0.62). The Israeli security establishment sees itself as beneficiary of a control mechanism (beneficiary position within Snare structure, d≈0.15, negative χ). The analytical observer sees pure extraction with minimal coordination function (Snare, χ≈0.78). The unique feature: Israeli beneficiary classification does not contradict Snare classification. Snare classification is structural (high ε, high suppression, minimal coordination benefit); it does not depend on whether all parties within it are victimized. The Israeli position is beneficiary from extraction, not beneficiary from coordination. This is the precise distinction that DR enables: distinguishing extraction-based control from coordination-based governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Gaza civilian population: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction magnitude. Humanitarian organizations: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction but with some agency (organizations can theoretically relocate; most choose not to). Israeli security establishment: Beneficiary + mobile (but strategically rejects exit) → d≈0.15, f(d)≈-0.01. Negative effective extraction from their perspective (they benefit from the mechanism). The apparent paradox is resolved by noting that d reflects structural position relative to the constraint, not absolute power. Israeli security establishment is a beneficiary of extraction (extraction flows to them), so their d is low despite their absolute power. Trapped populations are victims of extraction, so their d is high despite their absolute powerlessness. Egyptian government: Mixed (victim of refugee burden + beneficiary of rent extraction) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction: genuine costs (humanitarian obligations, refugee settlement) coupled with genuine benefits (security tool, revenue extraction). Analytical observer: analytical exit → d≈0.72, f(d)≈1.15. Standard analytical derivation from canonical fallback.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_threshold,
    'What fraction of closures are structurally necessary for legitimate security concerns vs. discretionary use of closure as political leverage?',
    'Temporal correlation analysis: closure timing vs. reported security incidents vs. political events (Israeli elections, Gaza political dynamics, Egyptian bilateral tensions). Comparison to other border crossing closure patterns globally.',
    'If >70% necessary: constraint may degrade from Snare toward Tangled Rope (justified mixed mechanism). If <30% necessary: confirms pure extraction classification (Snare). If 30-70%: remains Snare but with structural ambiguity about closure justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_threshold, empirical, 'Fraction of closures driven by security necessity vs. political discretion').

omega_variable(
    humanitarian_alternative_feasibility,
    'Could alternative humanitarian access routes (sea corridors, northern crossings, underground infrastructure) functionally replace Rafah crossing at scale?',
    'Engineering analysis of capacity constraints; political negotiation feasibility; cost-benefit comparison to Rafah opening. Historical cases of alternative crossing development (during Rafah closures).',
    'If technically feasible and politically achievable: removes ''no alternatives'' justification; recasts as Tangled Rope (mixed coordination/extraction with viable exit). If infeasible: confirms trap status (Snare classification holds).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_alternative_feasibility, empirical, 'Feasibility of alternative humanitarian access routes').

omega_variable(
    extraction_mechanism_beneficiary_chain,
    'Who actually captures value from Rafah closure? Does extraction flow to Israeli security establishment, Egyptian state revenue, arms dealers, or private security contractors?',
    'Financial tracking of closure-related revenue flows (visa fees, humanitarian aid contracts, security contracts, remittance timing correlations). Institutional analysis of who controls closure decisions vs. who benefits from them.',
    'If extraction is widely distributed: may indicate coordination failure (multiple actors with veto power) rather than single-actor extraction. If concentrated: confirms snare (single beneficiary extracting from trapped population).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism_beneficiary_chain, empirical, 'Identification of actual beneficiaries in the extraction chain').

omega_variable(
    civilian_exit_demand_elasticity,
    'How does closure duration/frequency affect Gaza civilian population''s adaptive capacity (home-based education, telemedicine, internal economy adaptation)?',
    'Longitudinal data on Gaza economic metrics, school enrollment, healthcare outcomes during varying closure regimes. Comparison to other isolated populations (Crimea, North Korea sanctions).',
    'If high elasticity (populations adapt quickly): constraint becomes background condition (Piton). If low elasticity (repeated harm, no adaptation): extraction persists in full force (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_exit_demand_elasticity, empirical, 'Civilian population''s adaptive capacity to sustained closures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_border_control_rafah, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rafah_tr_t0, gaza_border_control_rafah, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rafah_tr_t10, gaza_border_control_rafah, theater_ratio, 10, 0.5).
narrative_ontology:measurement(rafah_tr_t20, gaza_border_control_rafah, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rafah_be_t0, gaza_border_control_rafah, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(rafah_be_t10, gaza_border_control_rafah, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(rafah_be_t20, gaza_border_control_rafah, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_border_control_rafah, global_infrastructure).
narrative_ontology:affects_constraint(gaza_border_control_rafah, gaza_israeli_blockade_total).
narrative_ontology:affects_constraint(gaza_border_control_rafah, palestinian_humanitarian_access_system).
narrative_ontology:affects_constraint(gaza_border_control_rafah, egypt_israel_security_coordination).

% DUAL FORMULATION NOTE:
% The Rafah crossing control regime is downstream of the broader Gaza blockade (which determines why Rafah is the only exit) and upstream of the humanitarian access system (which depends on Rafah for critical supply flows). The constraint exhibits higher extractiveness than the broader blockade because the crossing functions as a control valve that can be weaponized; the blockade is the structural precondition. Network decomposition: (1) total blockade (ε=0.55, Snare) establishes the trap; (2) Rafah control (ε=0.68, Snare) weaponizes it; (3) humanitarian access system (ε=0.52, Tangled Rope) attempts to mitigate through humanitarian protocols. All three are distinct constraints with different ε values reflecting their different structural functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_border_control_rafah, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
