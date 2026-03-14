% ============================================================================
% CONSTRAINT STORY: houthi_proxy_escalation_logic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_houthi_proxy_escalation_logic, []).

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
 *   constraint_id: houthi_proxy_escalation_logic
 *   human_readable: Houthi Proxy Escalation Logic
 *   domain: geopolitical/military/state_capture
 *
 * SUMMARY:
 *   The Houthi proxy escalation constraint operates as an asymmetric
 *   extraction mechanism embedded within a nominal coordination framework.
 *   Iran strategically coordinates Houthi military capabilities to achieve
 *   geopolitical objectives (pressure on Saudi/UAE, disruption of regional
 *   shipping, demonstration of reach into global commerce) while distributing
 *   costs across Yemen's civilian population and global maritime networks.
 *   The constraint exhibits characteristics of both pure extraction (Snare —
 *   from the perspective of trapped civilian populations) and hybrid
 *   coordination-extraction (Tangled Rope — from the perspective of Houthi
 *   leadership and Iran). The escalation logic is self-reinforcing: each
 *   Houthi strike (enabled by Iranian weapons/intelligence) triggers
 *   coalition responses, which are then cited as justification for further
 *   Iranian provision and Houthi escalation. The theater component has
 *   increased over the measurement interval as military operations have
 *   become ritualized (daily drone/missile strikes against commercial
 *   shipping, predictable coalition air strikes against Houthi positions)
 *   while strategic objectives remain unchanged. The constraint is not a
 *   temporary coordination problem with a natural sunset — it is a locked-in
 *   extraction mechanism sustained by the combination of Iranian strategic
 *   interest and Houthi organizational survival dependence.
 *
 * KEY AGENTS:
 *   - Iran Strategic Command: Primary beneficiary (institutional/arbitrage) — extracts geopolitical leverage, regional destabilization, and forward military presence at minimal direct cost. Controls technology supply, intelligence provision, tactical coordination parameters.
 *   - Houthi Leadership: Secondary beneficiary/primary victim hybrid (organized/constrained) — benefits from organizational survival, legitimacy, military capability provision; constrained by operational dependence on Iranian resources and inability to exit without organizational collapse. Subject to active enforcement via Iranian targeting parameters and resource conditioning.
 *   - Yemeni Civilian Population: Primary victim (powerless/trapped) — bears full cost of infrastructure destruction, economic collapse, humanitarian crisis, and regional instability. No mechanism for exit, negotiation, or self-determination. No organizational capacity to counter constraint.
 *   - Global Shipping/Commerce Networks: Secondary victim (moderate/constrained) — face escalating operational costs (rerouting, insurance, delays) and suppressed by geographic chokepoint vulnerability. Can partially exit through alternative routes but at permanent cost premium.
 *   - US/Saudi/UAE Coalition: Secondary actor (powerful/mobile) — nominally coordinating for security but experiencing extractive military industrial momentum. Can exit (negotiate, withdraw) but face sunk costs and strategic credibility constraints.
 *   - Regional State System: Institutional degradation (institutional/arbitrage) — UN, international maritime law, sovereignty norms serve as performative theater masking operational inability to enforce constraints on proxy escalation. Persist through inertia despite functional failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(houthi_proxy_escalation_logic, 0.68).
domain_priors:suppression_score(houthi_proxy_escalation_logic, 0.72).
domain_priors:theater_ratio(houthi_proxy_escalation_logic, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(houthi_proxy_escalation_logic, extractiveness, 0.68).
narrative_ontology:constraint_metric(houthi_proxy_escalation_logic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(houthi_proxy_escalation_logic, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(houthi_proxy_escalation_logic, tangled_rope).
narrative_ontology:human_readable(houthi_proxy_escalation_logic, "Houthi Proxy Escalation Logic").
narrative_ontology:topic_domain(houthi_proxy_escalation_logic, "geopolitical/military/state_capture").

domain_priors:requires_active_enforcement(houthi_proxy_escalation_logic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(houthi_proxy_escalation_logic, iran_strategic_interests).
narrative_ontology:constraint_beneficiary(houthi_proxy_escalation_logic, houthi_organizational_survival).
narrative_ontology:constraint_victim(houthi_proxy_escalation_logic, yemen_civilian_population).
narrative_ontology:constraint_victim(houthi_proxy_escalation_logic, regional_maritime_stability).
narrative_ontology:constraint_victim(houthi_proxy_escalation_logic, global_shipping_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Yemeni population bears the full cost of proxy escalation — economic collapse, infrastructure destruction, and humanitarian crisis — with no mechanism for exit or negotiation. Trapped by geography, lacking external protection, and incapable of organizing counterforce. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Houthis experience dual-function constraint: genuine coordination of internal military/political structure PLUS asymmetric extraction from Iranian patronage. Benefits from military aid and strategic recognition; constrained by operational dependence on Iranian resources and targeting parameters. Can theoretically exit (historical non-proxy phase) but at extreme cost to organizational survival. Active enforcement: Iranian weapon transfers, tactical coordination, drone/missile technology provision.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Iran experiences the constraint as pure coordination with asymmetric benefit flow toward Iran. Coordinates proxy escalation to achieve multiple objectives (pressure on Saudi/UAE, disruption of regional shipping, demonstration of extended reach) with low direct cost to Iranian territory or military. Can arbitrage between escalation intensity and deniability. Net beneficiary extracting from regional instability.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Global commerce faces escalating extraction risk from expanding Houthi targeting parameters (Red Sea chokepoint control, extension to Indian Ocean). High suppression: rerouting costs, insurance premiums, operational delays. Some agency through defense systems and route variation, but trapped by geographic necessity of Suez/Red Sea passage. Escalation logic forces adaptation costs that compound without solution mechanism.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Coalition experiences mixed coordination and extraction: genuine security coordination against Iranian proxy expansion PLUS extractive military industrial demand (sustained weapons sales, extended deployment justification). Can exit (tactical withdrawal, negotiations) but faces sunk costs and institutional momentum. Constrained by regional alliance management and strategic credibility requirements. Theater component increases as military operations become ritualized containment rather than decisive action.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The state-based security architecture (UN, international law, naval law enforcement) is substantially degraded as constraint enforcement mechanism. The UN Security Council is paralyzed by permanent member veto; international maritime law enforcement is performative; sovereignty norms are circumvented by proxy arrangements. The institutional framework persists through inertia (declarations, maritime patrols, sanctions regimes) with low functional capacity to constrain escalation. Theater-dominant: the appearance of international order masks operational reality of uncontrolled proxy acceleration.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational view, some analysts frame proxy escalation as a natural consequence of great power competition and regional power vacuums — an immutable structural feature of multipolar competition. However, this naturalizes what is actually a contingent institutional choice: proxy escalation is incentivized by the specific architecture of deniability, the structure of arms markets, and the breakdown of interstate verification norms. The mountain classification is a false summit masking contingent institutional arrangements as structural inevitability.
constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(houthi_proxy_escalation_logic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(houthi_proxy_escalation_logic, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(houthi_proxy_escalation_logic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(houthi_proxy_escalation_logic, TR),
    TR >= 0.70.

:- end_tests(houthi_proxy_escalation_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Measurement interval shows sustained escalation from 2015 (ε=0.35) to 2024 (ε=0.68). The extraction flow is directional: Iran gains geopolitical leverage and regional influence while distributing costs to Yemen, commercial shipping, and regional stability. The escalation is not responsive to coalition pressure (contradicting deterrence theory) but appears driven by Iran's evaluation of strategic benefit relative to consequences. The constraint exhibits positive feedback: each Houthi strike increases coalition presence, which Iran uses to justify further escalation, which increases shipping disruption, which increases global attention and strategic importance of the theater. Suppression (0.72): High and structural. Yemeni population is geographically trapped and organizationally incapable of resistance. Global shipping is suppressed by chokepoint geography and asymmetric targeting (Houthi strikes can occur anywhere in the region; defense is distributed and reactive). Houthi leadership is suppressed by organizational dependence on Iranian resources — exit would mean organizational collapse. Coalition is suppressed by strategic credibility requirements and alliance management costs. Theater ratio (0.58): Moderate and rising. Military operations have become increasingly ritualized: daily Houthi drone/missile strikes against known shipping targets; predictable coalition air strikes against known Houthi positions; international declarations without enforcement mechanisms. The theater has increased over the interval as the operational tempo persists without clear strategic progress — the constraint is sustained more by institutional momentum than by functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer risks naturalizing the escalation as an inevitable feature of great power competition (Mountain perspective) — viewing proxy dynamics as immutable structural consequences of multipolar competition. However, the structural data reveals this as a false summit. The constraint is contingent on specific institutional arrangements: the asymmetry of deniability (Iran's strikes are attributed to Houthis), the structure of arms markets (weapons available for Iranian provision), the breakdown of verification norms (no mechanism to trace weapons to suppliers), and the incentive structure of proxy escalation (Iran extracts benefit at low direct cost). These are policy-contingent, not structural necessities. Alternative institutional arrangements — mandatory weapons tracking, attribution frameworks, direct consequence mechanisms for sponsor states — could alter the constraint's character fundamentally.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is derived from their structural position in the escalation logic. Iran, as beneficiary with arbitrage exit options (can escalate or de-escalate, absorbs minimal consequences), derives low d (≈0.05) and experiences negative χ — the constraint benefits them. Houthis, as organized actors constrained by dependence, derive moderate d (≈0.55) and experience moderate χ — they benefit from capability provision but are trapped in the extraction relationship. Yemen's powerless trapped population derives maximum d (≈0.95) and experiences maximum χ — all extraction with no benefit. Global shipping, as moderate actors constrained by geography, derives high d (≈0.75) and experiences high χ — significant extraction with limited exit options. Coalition, as powerful actors with mobile exit options, derives lower d (≈0.50) and experiences moderate χ — they can theoretically exit but face credibility costs that constrain the exit option's practical exercise.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying that 'escalation' can be simultaneously a coordination problem (from Iran's strategic perspective) and pure extraction (from Yemen's perspective). The mandatrophy resolution shows that no single type is correct — the constraint exhibits all types from different structural positions. However, the analytical observer's task is to identify which type dominates the constraint's structural character: Is it primarily coordination (suggesting negotiable settlement) or primarily extraction (suggesting intractable conflict)? The evidence points toward extraction-dominant: escalation persists despite stated willingness to negotiate; costs are asymmetrically distributed; termination mechanisms are absent; and the constraint is sustained by institutional momentum rather than functional necessity. This classification rules out the false summit (mountain) and suggests the constraint is Tangled Rope trending toward Snare — initially appeared as coordination but has revealed itself as increasingly extractive as the cycle has progressed and termination options have collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    escalation_termination_mechanism,
    'What credible mechanism terminates the Houthi escalation cycle once initiated?',
    'Historical pattern analysis of proxy escalations (2015-present); structural examination of Iranian strategic calculus around exit thresholds; analysis of successful de-escalation dynamics in similar conflicts',
    'If no credible termination mechanism exists: escalation is open-ended extraction masked as strategic competition. If termination threshold exists: classify as temporary scaffold with defined sunset. Current evidence suggests open-ended extraction (snare/tangled_rope dominant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_termination_mechanism, empirical, 'Existence and credibility of escalation termination mechanism').

omega_variable(
    houthi_genuine_autonomy,
    'To what degree are Houthi targeting decisions independent Iranian choices vs. proxy execution of Iranian strategy?',
    'Pattern analysis of Houthi strikes relative to Iranian strategic announcements; examination of capability gaps (technology, intelligence provision, resource constraints); interviews with defected Houthi operational commanders',
    'If high Houthi autonomy: constraint is less extraction, more hybrid coordination between semi-independent agents. If low autonomy: constraint is primarily Iranian extraction using Houthi organizational capacity. Current evidence suggests low-to-moderate autonomy with increasing Iranian tactical control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(houthi_genuine_autonomy, empirical, 'Degree of Houthi operational autonomy vs Iranian directional control').

omega_variable(
    shipping_alternative_viability,
    'Are alternative maritime routes (Cape of Good Hope, northern passages) functionally equivalent to Red Sea transit, or do they introduce sufficient cost/delay to constitute genuine suppression?',
    'Comparative analysis of transit time, insurance cost, fuel consumption, geopolitical risk across routes; shipping industry cost-benefit analysis for rerouting decisions',
    'If routes are genuinely equivalent: suppression is temporary (within 5 years rerouting completes, escape option expands). If routes introduce permanent cost premium: suppression persists, extraction mechanism locked in place.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shipping_alternative_viability, empirical, 'Whether alternative maritime routes provide functional escape from Red Sea chokepoint').

omega_variable(
    coalition_incentive_alignment,
    'Do US/Saudi/UAE coalition members share identical incentives for proxy containment, or do divergent interests create coalition fragmentation risk?',
    'Analysis of divergent strategic objectives (Saudi Arabia regional dominance vs US Iran containment vs UAE economic interests in ports/trade); historical precedent for coalition sustainability under proxy pressure',
    'If incentives diverge: coalition becomes unstable, enabling Iranian strategic exploitation through differential pressure application. If aligned: coalition can sustain containment pressure long-term, capping escalation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_incentive_alignment, empirical, 'Alignment of coalition member strategic incentives').

omega_variable(
    iranian_threshold_for_escalation_cost,
    'At what level of US/coalition military response does Iran perceive escalation costs as exceeding strategic benefit?',
    'Game-theoretic modeling of Iranian decision calculus; historical analysis of Iranian responses to direct strikes (2019 drone/missile attacks, leadership assassinations); comparative analysis with other proxy theaters (Syria, Iraq, Lebanon)',
    'If threshold is very high: escalation can proceed far before Iran perceives costs as intolerable. If threshold is moderate: credible deterrence against further escalation is possible. Current trajectory suggests moving beyond Iranian cost tolerance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iranian_threshold_for_escalation_cost, empirical, 'Iranian threshold cost tolerance for proxy escalation consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(houthi_proxy_escalation_logic, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(houthi_theater_2015, houthi_proxy_escalation_logic, theater_ratio, 0, 0.42).
narrative_ontology:measurement(houthi_theater_2019, houthi_proxy_escalation_logic, theater_ratio, 4, 0.5).
narrative_ontology:measurement(houthi_theater_2024, houthi_proxy_escalation_logic, theater_ratio, 9, 0.58).
narrative_ontology:measurement(houthi_theater_2017, houthi_proxy_escalation_logic, theater_ratio, 2, 0.47).

% Extraction over time
narrative_ontology:measurement(houthi_extract_2015, houthi_proxy_escalation_logic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(houthi_extract_2019, houthi_proxy_escalation_logic, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(houthi_extract_2024, houthi_proxy_escalation_logic, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(houthi_extract_2017, houthi_proxy_escalation_logic, base_extractiveness, 2, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(houthi_proxy_escalation_logic, enforcement_mechanism).
narrative_ontology:affects_constraint(houthi_proxy_escalation_logic, red_sea_chokepoint_control).
narrative_ontology:affects_constraint(houthi_proxy_escalation_logic, iran_regional_proxy_system).
narrative_ontology:affects_constraint(houthi_proxy_escalation_logic, gulf_state_security_dilemma).
narrative_ontology:affects_constraint(houthi_proxy_escalation_logic, global_supply_chain_fragility).

% DUAL FORMULATION NOTE:
% The Houthi escalation logic is downstream of the broader Iranian regional proxy system (constraint family: iran_regional_proxy_system includes Syria, Iraq, Lebanon, Yemen proxies). Each proxy theater has distinct ε values reflecting local conditions, but all are coordinated through central Iranian strategic command. The Houthi constraint (ε=0.68) exhibits higher extractiveness than the Iraq/Syria proxies (ε≈0.45-0.55) due to geographic distance from Iran enabling deniability and lower consequence perception. Decomposition: Houthi escalation logic (this constraint) represents the specific proxy relationship; the broader Iranian regional strategy represents a meta-constraint that affects all proxy theaters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(houthi_proxy_escalation_logic, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
