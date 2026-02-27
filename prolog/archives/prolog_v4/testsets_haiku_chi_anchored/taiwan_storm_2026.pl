% ============================================================================
% CONSTRAINT STORY: taiwan_storm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_storm_2026, []).

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
 *   constraint_id: taiwan_storm_2026
 *   human_readable: The 2026 Taiwan "Perfect Storm" Geopolitical Convergence
 *   domain: geopolitical/political
 *
 * SUMMARY:
 *   The 2026-2027 Taiwan scenario represents a convergence of three
 *   structural factors creating a heightened window of geopolitical risk: (1)
 *   a potential legacy deadline for Xi Jinping's consolidation of power and
 *   regional control before 2027 succession dynamics or term limits become
 *   relevant; (2) temporary perceived U.S. strategic distraction or weakening
 *   (post-2024 election uncertainty, domestic polarization, competing
 *   commitments in Ukraine/Middle East); (3) Taiwan's political cycle and
 *   potential internal divisions that could invite coercive diplomacy or
 *   military pressure. The constraint is fundamentally a snare—an extraction
 *   mechanism where Beijing can exploit the coordination failure between
 *   Washington and Taipei while the international rules-based order (UN,
 *   international law) remains performative rather than enforcing. Taiwan
 *   faces maximal suppression (0.72): military options are catastrophic,
 *   unilateral diplomatic options are ineffective without U.S. backing, and
 *   the window itself is the constraint—waiting beyond 2027 changes the terms
 *   entirely. Theater ratio (0.58) reflects that public rhetoric about
 *   'peaceful resolution' and 'status quo maintenance' masks real military
 *   preparations and the credible possibility of force. The extractiveness
 *   has increased from 0.45 to 0.68 over the 2024-2026 interval as both
 *   Beijing's capabilities have matured and Washington's signal clarity has
 *   lagged.
 *
 * KEY AGENTS:
 *   - Taiwan's Political Leadership: Primary victim (powerless/trapped) — caught in convergence with no unilateral exit; dependent on U.S. commitment which is uncertain
 *   - China's Central Leadership (Beijing): Primary beneficiary (institutional/arbitrage) — captures regional consolidation advantage during window; experiences constraint as coordination mechanism, not extraction
 *   - U.S. Strategic Leadership: Secondary actor (powerful/constrained) — bound by prior commitments and domestic political uncertainty; constrained by reputational costs of ambiguity
 *   - Regional States (Japan, South Korea, Philippines): Secondary victims (moderate/constrained) — dependent on U.S. security guarantees; exposed to spillover risk; cannot guarantee independent deterrence
 *   - International Rules-Based Order (UN, International Law): Institutional theater (institutional/arbitrage) — formally neutral but functionally powerless; P5 veto blocks enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as real structural convergence, not naturalized inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_storm_2026, 0.68).
domain_priors:suppression_score(taiwan_storm_2026, 0.72).
domain_priors:theater_ratio(taiwan_storm_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_storm_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(taiwan_storm_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_storm_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_storm_2026, snare).
narrative_ontology:human_readable(taiwan_storm_2026, "The 2026 Taiwan \"Perfect Storm\" Geopolitical Convergence").
narrative_ontology:topic_domain(taiwan_storm_2026, "geopolitical/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_storm_2026, china_leadership).
narrative_ontology:constraint_beneficiary(taiwan_storm_2026, strategic_opportunists).
narrative_ontology:constraint_victim(taiwan_storm_2026, taiwan_sovereignty).
narrative_ontology:constraint_victim(taiwan_storm_2026, regional_stability).
narrative_ontology:constraint_victim(taiwan_storm_2026, us_strategic_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN'S POLITICAL LEADERSHIP (SNARE) — Trapped within the convergence window. No unilateral exit; military options are catastrophic; diplomatic options require U.S. backing which is itself uncertain. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.64.
constraint_indexing:constraint_classification(taiwan_storm_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: U.S. STRATEGIC LEADERSHIP (TANGLED_ROPE) — Constrained by prior commitments (Taiwan Relations Act, implicit deterrent signaling) and domestic political uncertainty (2024-2026 transition, Senate/House turnover). Experiences both coordination function (deterrent credibility) and extraction (reputational cost, military escalation risk). d≈0.58, f(d)≈0.72, σ=1.2 → χ≈0.54.
constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINA'S LEADERSHIP (ROPE) — Beijing experiences the convergence as a coordination mechanism: consolidating control over Taiwan without full military escalation maximizes domestic legitimacy (centenial CCP and legacy timelines) while minimizing costs of international sanctions or prolonged conflict. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Net beneficiary position.
constraint_indexing:constraint_classification(taiwan_storm_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL STATES (SNARE) — Japan, South Korea, and Philippines are constrained by military dependence on U.S. security guarantees and exposure to spillover risk. Cannot unilaterally guarantee deterrent credibility; limited independent exit options. d≈0.80, f(d)≈1.20, σ=1.1 → χ≈0.62.
constraint_indexing:constraint_classification(taiwan_storm_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL RULES-BASED ORDER (PITON) — UN, international law, freedom of navigation norms are largely performative in Taiwan scenario. Theater_ratio=0.58 reflects that formal institutional responses (Security Council, International Court) are blocked by P5 veto; actual enforcement is delegated to military deterrence. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(taiwan_storm_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED_ROPE) — From civilizational scale, the convergence is a coordination problem masking extraction: Beijing coordinates consolidation of its regional order while the U.S. and allies extract legitimacy costs and reputational damage. The constraint is neither immutable (outcomes still contingent on decisions 2024-2027) nor purely coordination (asymmetric benefits and costs). d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(taiwan_storm_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_storm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_storm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_storm_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_storm_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_storm_2026, TR),
    TR >= 0.70.

:- end_tests(taiwan_storm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Beijing captures substantial asymmetric advantage from the convergence: consolidated regional control, enhanced legitimacy domestically (legacy consolidation), and reputational damage to U.S. deterrence credibility. The extraction escalated from 0.45 to 0.68 as the window has approached—this reflects real accumulation of Beijing's options and Taiwan's constrained alternatives. Suppression (0.72): Very high. Taiwan faces multiple suppression mechanisms: (1) military inferiority in contested scenarios (air, sea denial); (2) economic dependence on mainland markets; (3) dependence on external (U.S.) deterrence; (4) internal political divisions that Beijing can exploit (cross-strait voters, business interests); (5) cost of unilateral military buildup exceeds defensive benefit. Theater ratio (0.58): Moderate-high. Public discourse emphasizes 'peaceful resolution' and 'status quo' but masks real military preparations (PLA exercise intensification, Taiwan air defense procurement, U.S. carrier operations). The theater has increased from 0.42 to 0.58 as the convergence has approached—political rhetoric has become more performative as military reality has become more ominous.
 *
 * PERSPECTIVAL GAP:
 *   Taiwan's political leadership sees a snare: constrained, no unilateral exit, trapped in window. China's leadership sees a rope: the convergence is solving a coordination problem (consolidating regional order) with minimal extraction cost relative to benefit. The U.S. sees tangled rope: commitment to deterrence (coordination function) while facing reputational and military costs (extraction). Regional states see snare: dependent on U.S. credibility which is itself uncertain; spillover exposure without agency. The international rules-based order sees piton: formal institutional responses are blocked; actual enforcement is delegated to military deterrence which is the constraint itself. The analytical observer sees tangled rope: the convergence is coordination (consolidation of regional hierarchy) extracting legitimacy and reputational costs from the order being consolidated. The perspectival gap reveals that no single actor experiences this as a pure coordination problem—Beijing alone captures net coordination benefit; all others bear net extraction costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Taiwan's leadership: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction direction—no exit options. China's leadership: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction from their perspective; net beneficiary. U.S. leadership: Victim (reputational cost, military escalation risk) + constrained (bound by prior commitments) → d≈0.58, f(d)≈0.72. Moderate-high extraction direction; U.S. experiences costs but retains some agency through commitment clarity. Regional states: Victims (spillover exposure) + constrained (dependent on U.S.) → d≈0.80, f(d)≈1.20. High extraction direction; constrained options limit agency. International order: Neither primary beneficiary nor victim; institutional arbitrage → d≈0.10, f(d)≈-0.08. Theater classification comes from performative institutional responses (UN statements, international law invocation) that mask lack of enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint must be classified as snare, not mistaken for coordinate action problem or natural geopolitical law. The mandatrophy emerges in two forms: (1) Is the convergence inherent to great-power competition or contingent on specific actors' choices? (2) Does Beijing's interest in legacy consolidation represent genuine coordination (shared interest in regional stability) or pure extraction (unilateral advantage-taking)? The snare classification resolves both: the convergence is contingent (choices matter, outcomes uncertain), but Beijing's position enables extraction (regional consolidation) while suppressing alternatives (Taiwan's options, international enforcement). The theater ratio (0.58) reflects that public framing of 'peaceful resolution' naturalizes what is actually an extraction mechanism—this is the mandatrophy: mistaking constrained coordination for voluntary coordination. The analytical observer must avoid naturalizing the convergence as inevitable great-power behavior—the constraint is real only because of specific decisions (U.S. signaling failures, Beijing's timeline urgency, Taiwan's isolation). Different choices 2024-2026 could shift the constraint from snare toward tangled_rope (if U.S. signals clarity) or rope (if Beijing deprioritizes Taiwan). The snare classification holds only if the current trajectory (ambiguity, urgency, isolation) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    xi_legacy_urgency_magnitude,
    'How urgent is Xi Jinping''s personal interest in resolving Taiwan before 2027? Does it dominate strategic decision-making or is it one of several factors?',
    'Analysis of CCP leadership messaging 2024-2026; comparison of Taiwan policy emphasis vs other legacy goals (economic targets, technology self-sufficiency, environmental restoration); behavioral indicators from military posture changes',
    'If legacy urgency is dominant: snare classification holds globally (d>0.80 for all perspectives). If legacy is secondary: classification shifts to tangled_rope for most perspectives (d≈0.55-0.65), reducing effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(xi_legacy_urgency_magnitude, empirical, 'Degree to which Xi''s personal legacy timeline drives Taiwan policy').

omega_variable(
    us_commitment_credibility_gap,
    'Will U.S. political transition 2024-2026 result in clear commitment signals to Taiwan and regional allies, or will ambiguity persist?',
    'Tracking of U.S. official statements, military deployments, military aid packages to Taiwan 2024-2026; assessment of consistency across administrations; allied perception surveys (Japan, Korea, Australia on U.S. credibility)',
    'If U.S. clarity achieved: regional states'' exit_options upgrade from constrained→mobile; perspectives shift from snare toward rope (d<0.60). If ambiguity persists: snare holds (d>0.75).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_credibility_gap, empirical, 'Whether U.S. signals resolve commitment ambiguity').

omega_variable(
    military_capability_surprise_threshold,
    'Are there hidden asymmetries in PLA capabilities (amphibious assault, air superiority, sea denial) that would shift the military balance below currently assessed thresholds?',
    'Intelligence assessments; comparison of declared vs inferred capabilities; conflict simulations (RAND, defense think tanks) updated with new capability data; Taiwan military procurement outcomes',
    'If PLA hidden advantage is substantial: Taiwan''s exit_options collapse to fully_trapped; classification becomes pure snare (d≈0.98) from all non-beneficiary perspectives. If capabilities match assessments: current snare classifications hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_capability_surprise_threshold, empirical, 'Whether hidden PLA capabilities alter military balance assessment').

omega_variable(
    economic_interdependence_constraint_effect,
    'Does China-U.S. economic interdependence (semiconductors, supply chains, debt markets) create mutual extraction or mutual deterrence in the Taiwan scenario?',
    'Scenario analysis of semiconductor supply disruption; modeling of financial contagion (Chinese debt, U.S. markets); tracking of decoupling progress 2024-2026 (critical tech restrictions, supply chain relocation)',
    'If interdependence creates mutual extraction: all perspectives recognize mutual snare (d→0.50 for both sides, symmetric high extraction). If decoupling advances: extraction becomes asymmetric (Beijing has lower exit cost than U.S.), snare persists with high d for U.S.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_interdependence_constraint_effect, empirical, 'Whether economic interdependence enables mutual deterrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_storm_2026, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiwan_tr_t0, taiwan_storm_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(taiwan_tr_t1, taiwan_storm_2026, theater_ratio, 1, 0.5).
narrative_ontology:measurement(taiwan_tr_t2, taiwan_storm_2026, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(taiwan_be_t0, taiwan_storm_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(taiwan_be_t1, taiwan_storm_2026, base_extractiveness, 1, 0.52).
narrative_ontology:measurement(taiwan_be_t2, taiwan_storm_2026, base_extractiveness, 2, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_storm_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(taiwan_storm_2026, semiconductor_supply_chain_vulnerability).
narrative_ontology:affects_constraint(taiwan_storm_2026, us_china_strategic_competition).
narrative_ontology:affects_constraint(taiwan_storm_2026, regional_deterrence_credibility).

% DUAL FORMULATION NOTE:
% The Taiwan convergence is downstream of three independent constraints: (1) U.S.-China competition (which creates incentive asymmetry), (2) Taiwan's geopolitical isolation (which creates suppression), and (3) Xi's legacy timeline (which creates urgency). Each has its own ε; the convergence generates a composite snare by alignment. Decomposition: semantic clarity requires separate stories for Xi's personal incentive structure (biographical timeline constraint, moderate ε) and Beijing's strategic interest in Taiwan consolidation (civilizational regional hierarchy constraint, higher ε). The 2026 perfect storm story unifies these as a temporal intersection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_storm_2026, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
