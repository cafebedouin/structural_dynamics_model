% ============================================================================
% CONSTRAINT STORY: gaza_border_control_rafah
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: geopolitical/humanitarian_access
 *
 * SUMMARY:
 *   The Rafah border crossing represents the primary exit point for the Gaza
 *   Strip's 2+ million residents to Egypt. Control of this crossing operates
 *   as a structural constraint that exhibits pure extraction characteristics
 *   (snare) from the perspective of trapped civilians, while appearing as
 *   coordination mechanism with extraction to the Israeli and Egyptian
 *   governments. The constraint combines absolute suppression (no
 *   alternatives when closed) with high extractiveness (political leverage,
 *   fee collection, demographic control). The theater ratio reflects that
 *   border closure is often justified through security rhetoric but lacks
 *   proportionate relationship to actual security incidents. Extractiveness
 *   has increased over the 23-year interval (from 0.42 to 0.68) as the
 *   constraint has shifted from occasional closure to systematic restriction
 *   of civilian movement during political crises. The constraint is sustained
 *   through active enforcement by both Israeli military (territorial control)
 *   and Egyptian security apparatus (border management), making it neither a
 *   natural law nor a temporary scaffold, but a persistent institutional
 *   snare.
 *
 * KEY AGENTS:
 *   - Gaza Civilian Population: Primary victim (powerless/trapped) — 2+ million residents with no exit options; bears full cost of closures including medical, educational, and economic harms
 *   - Humanitarian Organizations: Secondary victim (moderate/constrained) — inability to deliver aid when crossing closes; dependent on political permission for operations
 *   - Egyptian Government: Primary beneficiary-enforcer (institutional/arbitrage) — controls crossing from Egypt side; extracts political leverage and revenues; can choose opening/closure
 *   - Israeli Government: Secondary beneficiary-enforcer (organized/constrained) — maintains indirect control through occupation; uses closure for population control and security leverage; cannot exit without strategic consequence
 *   - International Legal Framework: Degraded institution (institutional/arbitrage) — nominally guarantees freedom of movement but lacks enforcement mechanism; persists performatively
 *   - Analytical Observer: Sees pure extraction structure (analytical/analytical) — no coordination benefit justifies the asymmetric coercion; structure is unambiguous snare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_border_control_rafah, 0.68).
domain_priors:suppression_score(gaza_border_control_rafah, 0.85).
domain_priors:theater_ratio(gaza_border_control_rafah, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_border_control_rafah, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_border_control_rafah, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gaza_border_control_rafah, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_border_control_rafah, snare).
narrative_ontology:human_readable(gaza_border_control_rafah, "Control regime over the Gaza-Egypt (Rafah) border crossing").
narrative_ontology:topic_domain(gaza_border_control_rafah, "geopolitical/humanitarian_access").

domain_priors:requires_active_enforcement(gaza_border_control_rafah).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_border_control_rafah, egyptian_government).
narrative_ontology:constraint_beneficiary(gaza_border_control_rafah, israeli_government).
narrative_ontology:constraint_victim(gaza_border_control_rafah, gaza_civilian_population).
narrative_ontology:constraint_victim(gaza_border_control_rafah, humanitarian_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAZA CIVILIANS (SNARE) — Trapped without exit options. Cannot leave Gaza except through Rafah crossing, which is controlled and frequently closed. Bears full cost of closure: no access to medical treatment abroad, educational opportunities, family reunification, or economic livelihood. Maximum extraction with no alternatives or negotiating power. High suppression: military and security apparatus on both sides of crossing creates barriers to exit.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATIONS (SNARE) — Constrained by government approvals and operational closures. Cannot sustain aid delivery when crossing closes; staff mobility restricted. Face extraction through: requirement to cooperate with security screening, acceptance of operational constraints, and dependence on political permission for program delivery. Moderate power and constrained (not trapped) exit options reflect that organizations can operate in alternative locations, but doing so requires abandoning Gaza beneficiaries.
constraint_indexing:constraint_classification(gaza_border_control_rafah, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EGYPTIAN GOVERNMENT (TANGLED ROPE) — Controls Rafah from Egypt side. Experiences constraint as hybrid coordination-extraction. Coordination function: manages orderly flow, prevents security threats, maintains border integrity. Extraction function: leverages border control for political concessions from Hamas, charges fees for crossing, restricts Palestinian labor/trade movement. Net beneficiary but also benefits from coordination that prevents chaos. Arbitrage exit reflects discretion to close or open crossing based on political calculations.
constraint_indexing:constraint_classification(gaza_border_control_rafah, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ISRAELI GOVERNMENT (TANGLED ROPE) — Controls Rafah indirectly through military occupation and coordination with Egypt. Experiences constraint as coordination (security management, population control) layered with extraction (leverage over Gaza, restriction of Palestinian movement). Cannot simply exit (would lose security control); instead maintains presence through intermittent enforcement and periodic escalation. Constrained exit reflects that withdrawal requires negotiated settlement, not unilateral choice.
constraint_indexing:constraint_classification(gaza_border_control_rafah, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL FRAMEWORK (PITON) — UN conventions and international humanitarian law ostensibly guarantee freedom of movement and civilian protection. Border closure violates these norms but persists through institutional inertia and lack of enforcement mechanism. The international framework is performative: it condemns closures in statements while lacking capacity to enforce opening. Theater ratio reflects gap between legal principle and operational reality. The constraint persists not because law requires it but because enforcement is absent.
constraint_indexing:constraint_classification(gaza_border_control_rafah, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a global/civilizational view, this constraint exhibits pure extraction with maximum suppression. Closure is absolute when in effect (no alternatives); has no coordination benefit that would justify extraction (no genuine security need requires closing a border to civilians seeking medical treatment). The structure is pure coercion: power difference + no exit + no coordination justification = snare. No false summit here — the analytical view agrees with the powerless agent's view.
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

test(piton_threshold) :-
    domain_priors:theater_ratio(gaza_border_control_rafah, TR),
    TR >= 0.70.

:- end_tests(gaza_border_control_rafah_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The crossing closure extracts multiple forms of value from trapped population: political leverage over Hamas, demonstration of control, prevention of Palestinian economic integration with Egypt, and management of demographic pressure. The value is asymmetric — Gaza population bears costs (medical access, livelihood, family reunion) while Israeli/Egyptian actors gain control and leverage. The metric reflects historical increase: early 2000s closures were intermittent (0.42); by 2010s, restrictions were systematic (0.58); by 2023, closure had become extended/indefinite (0.68). Suppression (0.85): Extreme. When the crossing is closed, there are no alternatives: Kerem Shalom (Israeli-controlled) is not available for most civilians; sea routes are blocked; tunnels are unreliable and dangerous. This is not constrained movement — it is trapped movement. Suppression remains near maximum even when crossing is nominally open because opening is contingent and can be revoked. Theater ratio (0.58): Moderate. Security justifications for closure exist but are not proportionate. Closures often coincide with political events, not security incidents. International statements condemn closure while lacking enforcement capacity. The performative gap has grown over time as the rhetoric has become more systematic but less connected to operational security needs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a major perspectival gap between the trapped beneficiaries of closure (Israeli/Egyptian governments) and its victims (Gaza population and humanitarian organizations). Israeli and Egyptian governments experience the constraint as coordinated management with extractive benefit — they maintain order and gain political leverage. Gaza civilians experience it as pure snare — absolute suppression with no benefit. The analytical observer's view aligns with the victim's view: there is no coordination benefit that justifies the asymmetry, making this unambiguous snare rather than tangled_rope. The piton perspective (international legal framework) is subordinate — it declares principles that are ignored in practice. The perspectival gap reveals that this constraint is not a mixed coordination-extraction (which would show some victims perceiving benefit or some beneficiaries perceiving obligation); it is asymmetric extraction presented through coordination rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Gaza civilians derive d ≈ 0.95 (trapped victims with no exit) producing maximum experienced extraction. Humanitarian organizations derive d ≈ 0.70 (constrained victims who can operate elsewhere but would abandon beneficiaries) producing high but not maximal extraction. Egyptian government derives d ≈ 0.10 (beneficiary with arbitrage exit options) producing negative or minimal experienced extraction — they benefit from the constraint's existence. Israeli government derives d ≈ 0.35 (beneficiary constrained by strategic considerations; cannot fully arbitrage away from this constraint without security consequence) producing moderate extraction in their direction. The directionality overrides are not needed — the structural data (beneficiary/victim status + exit options) derives the correct d values directly.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY FULLY RESOLVED: This constraint is unambiguous snare across the relevant perspectives. There is no coordination function that would justify the extraction — no genuine security benefit that requires closing a border to civilians seeking medical treatment, family reunion, or livelihood. The security rhetoric is performative cover for extractive control. The constraint does not risk being mislabeled as coordination (Rope) because there is no real coordination benefit. It does not risk being mislabeled as temporary (Scaffold) because there is no credible sunset mechanism — the regime is sustained precisely because it extracts value for the enforcers. The piton perspective (international law) is degraded precisely because the constraint violates stated norms. The mountain perspective is a false summit — closure is not inherent to border management or geopolitics; it is contingent policy choice by enforcers. Mandatrophy resolution confirms: this is pure extraction (snare) enabled by power asymmetry + trapped population + no coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_threshold,
    'What level of security threat justifies border closure for civilian populations, and does the claimed threat meet that threshold?',
    'Comparative analysis of closure frequency vs security incidents; examination of whether closures correlate with actual threats or political events; assessment of whether same-magnitude threats at other borders trigger similar restrictions',
    'If security justification is genuine: constraint may reclassify as scaffold (temporary security measure with exit path). If security claims are pretextual: confirms snare classification and reveals pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_threshold, empirical, 'Whether security justifications for closure are proportionate to actual threats').

omega_variable(
    extraction_beneficiary_identification,
    'Who materially benefits from Rafah closure, and what is the magnitude of their benefit relative to civilian cost?',
    'Financial analysis of fees/levies collected; political analysis of leverage gains from closure; comparison of Israeli/Egyptian institutional benefit against humanitarian harm metrics',
    'If closure benefits are minimal and distributed: might reclassify as tangled_rope with more symmetric extraction. If benefits concentrate in Israeli/Egyptian actors: confirms snare structure with asymmetric extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_identification, empirical, 'Quantification of who benefits from border restrictions and by how much').

omega_variable(
    alternative_exit_sufficiency,
    'Do alternative routes (Kerem Shalom crossing, sea routes, underground tunnels) provide meaningful exit options for trapped population?',
    'Operational data on throughput, access criteria, and reliability of alternatives; assessment of whether alternatives are available to majority of population or only privileged groups',
    'If alternatives are robust: exit_options upgrade from trapped to constrained, potentially reclassifying some perspectives. If alternatives are restricted/unreliable: confirms trap status and strengthens snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_exit_sufficiency, empirical, 'Whether alternative routes provide genuine exit options for civilian population').

omega_variable(
    enforcement_mechanism_sustainability,
    'How sustainable is the two-state enforcement regime (Israel + Egypt coordination) given divergent political interests?',
    'Historical analysis of coordination breakdowns; examination of incentive misalignment between Israeli security goals and Egyptian domestic politics; assessment of whether regime requires active US/international enforcement support',
    'If enforcement is fragile: might introduce scaffold perspective with real sunset as regimes realign. If enforcement is stable: confirms persistent snare with high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_sustainability, conceptual, 'Structural sustainability of the coordination between Israeli and Egyptian enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_border_control_rafah, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rafah_theater_2000, gaza_border_control_rafah, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rafah_theater_2010, gaza_border_control_rafah, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rafah_theater_2023, gaza_border_control_rafah, theater_ratio, 23, 0.58).

% Extraction over time
narrative_ontology:measurement(rafah_extractiveness_2000, gaza_border_control_rafah, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rafah_extractiveness_2010, gaza_border_control_rafah, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rafah_extractiveness_2023, gaza_border_control_rafah, base_extractiveness, 23, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_border_control_rafah, enforcement_mechanism).
narrative_ontology:affects_constraint(gaza_border_control_rafah, palestinian_labor_market_integration).
narrative_ontology:affects_constraint(gaza_border_control_rafah, gaza_medical_access_egypt_route).
narrative_ontology:affects_constraint(gaza_border_control_rafah, humanitarian_aid_delivery_gaza).

% DUAL FORMULATION NOTE:
% This constraint is distinct from but causally upstream of specific harms (medical access, labor integration, aid delivery). Each downstream constraint experiences extraction pressure through the gating mechanism of Rafah closure. The network structure reveals how a single control point (the crossing) propagates extractive effects across multiple institutional domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
