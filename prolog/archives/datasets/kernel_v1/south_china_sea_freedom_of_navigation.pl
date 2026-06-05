% ============================================================================
% CONSTRAINT STORY: south_china_sea_freedom_of_navigation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_china_sea_freedom_of_navigation, []).

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
 *   constraint_id: south_china_sea_freedom_of_navigation
 *   human_readable: South China Sea Freedom of Navigation Constraint
 *   domain: geopolitical/maritime_law
 *
 * SUMMARY:
 *   The South China Sea freedom of navigation constraint represents a
 *   geopolitical hybrid: genuine coordination mechanisms (UNCLOS framework,
 *   ASEAN dispute resolution, Code of Conduct negotiations) coexist with
 *   asymmetric extraction mechanisms (military dominance through artificial
 *   island militarization, Nine-Dash Line claims, and capacity to interdict
 *   smaller states' maritime activities). The constraint exhibits different
 *   classification from each major stakeholder. China experiences the
 *   framework as enabling regional coordination under its leadership (Rope
 *   perspective). Extra-regional powers see it as a coordination challenge
 *   with multiple solution pathways (Scaffold perspective). Smaller claimant
 *   states face maximum constraint with no exit options (Snare perspective).
 *   International law provides the shared normative architecture but exhibits
 *   degraded enforcement mechanisms (Piton perspective). The core tension:
 *   whether militarization can be constrained through negotiated agreements
 *   (Code of Conduct path) or has already locked the region into permanent
 *   Chinese dominance (snare outcome for smaller states). Extractiveness has
 *   risen from 0.32 to 0.58 over the interval as island militarization has
 *   accelerated; suppression requirements have risen from 0.48 to 0.68 as
 *   military capability gaps have widened; theater ratio has risen from 0.52
 *   to 0.65 as ritual performance (FONOPs, legal briefs, Code of Conduct
 *   negotiations) has increased alongside actual military construction.
 *
 * KEY AGENTS:
 *   - China: Primary beneficiary (institutional/arbitrage) — expands regional dominance through militarization; can walk away from international constraints at acceptable cost (sanctions, escalation)
 *   - Smaller Claimant States (Vietnam, Philippines, Malaysia): Primary victims (powerless/trapped) — face military dominance and cannot exit regional politics or economy; bear maximum constraint
 *   - ASEAN Consensus Mechanism: Secondary institutional actor (organized/constrained) — provides coordination framework but China has embedded veto power through unanimity requirement
 *   - Extra-Regional Powers (US, Japan, Australia, Allies): Powerful institutional actor (powerful/mobile) — conduct FONOPs to assert UNCLOS compliance; have multiple exit pathways and alliance options
 *   - International Law & UNCLOS Regime: Institutional framework (institutional/arbitrage) — provides shared normative language but enforcement is degraded; all parties invoke UNCLOS while violating its spirit
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as hybrid coordination-extraction system with genuine but asymmetric benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_freedom_of_navigation, 0.58).
domain_priors:suppression_score(south_china_sea_freedom_of_navigation, 0.68).
domain_priors:theater_ratio(south_china_sea_freedom_of_navigation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_freedom_of_navigation, extractiveness, 0.58).
narrative_ontology:constraint_metric(south_china_sea_freedom_of_navigation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(south_china_sea_freedom_of_navigation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_china_sea_freedom_of_navigation, tangled_rope).
narrative_ontology:human_readable(south_china_sea_freedom_of_navigation, "South China Sea Freedom of Navigation Constraint").
narrative_ontology:topic_domain(south_china_sea_freedom_of_navigation, "geopolitical/maritime_law").

domain_priors:requires_active_enforcement(south_china_sea_freedom_of_navigation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_china_sea_freedom_of_navigation, china_regional_dominance).
narrative_ontology:constraint_beneficiary(south_china_sea_freedom_of_navigation, island_militarization_beneficiaries).
narrative_ontology:constraint_victim(south_china_sea_freedom_of_navigation, extra_regional_navigational_freedom).
narrative_ontology:constraint_victim(south_china_sea_freedom_of_navigation, smaller_claimant_states).
narrative_ontology:constraint_victim(south_china_sea_freedom_of_navigation, international_law_compliance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER CLAIMANT STATES (SNARE) — Vietnam, Philippines, Malaysia face maximum extraction without viable exit. Cannot abandon overlapping claims (national sovereignty), cannot match military capability, cannot exit regional economy. The constraint extracts compliance with Chinese interests through threat of seizure or military confrontation. Zero degrees of freedom.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN CONSENSUS MECHANISM (TANGLED ROPE) — Coordination function: ASEAN provides dispute resolution framework and collective voice. Extraction function: China's dominance is embedded in the structure itself—consensus requires unanimity, blocking any genuinely constraining agreement. Member states coordinate on non-binding Code of Conduct while China extracts de facto veto power. Constrained exit: states need regional stability more than confrontation; cannot simply leave ASEAN.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINA'S REGIONAL HEGEMONIC POSITION (ROPE) — China experiences the constraint as coordination: the Nine-Dash Line and island militarization coordinate regional order under Chinese leadership. UNCLOS and international law become coordination mechanisms that China can selectively invoke. High arbitrage capacity—China can walk away from international regime obligations at acceptable cost (escalate, absorb sanctions, build alternative economic ties). This perspective experiences the constraint as enabling rather than constraining.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: EXTRA-REGIONAL POWERS (SCAFFOLD) — FONOPs constitute a temporary coordination mechanism asserting UNCLOS compliance and preventing solidification of Chinese monopoly. Low effective extraction because extra-regional powers have mobility (can adjust operational tempo, form new partnerships, deepen ties with regional states). The constraint is experienced as a coordination challenge with multiple solution pathways and exit options. Implicit sunset: if the Code of Conduct materializes into genuine constraints on militarization, the scaffolding architecture shifts.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LAW & UNCLOS REGIME (PITON) — The constraint exhibits high theater: UNCLOS provides a shared normative framework (peaceful resolution, freedom of navigation, exclusive economic zones) that all parties invoke, yet the regime's enforcement mechanism is degraded. FONOPs are performed assertions of UNCLOS compliance; China's militarization is performed assertion of sovereignty. Both sides conduct theater without genuinely subordinating interests to the legal order. The regime persists through institutional inertia (alternative frameworks lack legitimacy) despite low functional enforcement.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, the constraint coordinates regional power distribution while extracting compliance from weaker states. International law provides the coordination function (shared dispute-resolution framework, common language for claims); China's military dominance provides the extraction mechanism (capacity to enforce preferred outcomes). The constraint is neither pure coordination nor pure extraction—it is hybrid, with genuine coordination benefits alongside asymmetric extraction.
constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_freedom_of_navigation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_china_sea_freedom_of_navigation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_china_sea_freedom_of_navigation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_china_sea_freedom_of_navigation, TR),
    TR >= 0.70.

:- end_tests(south_china_sea_freedom_of_navigation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. China extracts regional hegemony and smaller states' compliance, but not total extraction—smaller states retain some agency in alliance formation and diplomatic maneuvering. The extracted value is substantial (control of shipping lanes, geostrategic position, veto power in regional decisions) but not maximal (FONOPs persist, Code of Conduct negotiations continue, economic interdependencies constrain unilateral dominance). Suppression (0.68): High. Smaller states face military dominance with escalating costs to exit. Equipment gaps, alliance credibility questions, and economic interdependencies raise suppression above the tangled_rope floor. Theater ratio (0.65): Moderate-high. FONOPs are ritual assertions of UNCLOS compliance with limited effect on Chinese behavior. Code of Conduct negotiations are performative exercises (10+ years of talks, no binding agreement). Military buildup is real, but legal briefs and diplomatic statements constitute substantial theater. The theater ratio has risen as ritual performance accelerates while actual behavioral change stalls.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a massive perspectival gap reflecting stark power asymmetries. China's rope perspective (coordination under regional hegemony) contrasts sharply with smaller states' snare perspective (extraction without exit). ASEAN's tangled_rope experience (coordination function degraded by veto power) sits between these extremes. Extra-regional powers' scaffold perspective (mobile actors with exit options) depends on alliance credibility that smaller states cannot assume. International law's piton perspective (shared framework with degraded enforcement) describes the shared normative language that all parties invoke while violating in practice. The analytical observer's tangled_rope classification (genuine coordination alongside asymmetric extraction) reflects that the constraint is neither pure law nor pure power—it is hybrid, unstable, and vulnerable to either negotiated settlement or military escalation depending on which mechanism (coordination or extraction) stabilizes first.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the constraint. China as institutional beneficiary with arbitrage exit options occupies d ≈ 0.05-0.15 (low d, can walk away without fatal cost). Smaller states as powerless victims with trapped exit occupy d ≈ 0.95 (maximum d, cannot walk away). ASEAN as organized but constrained occupies d ≈ 0.55-0.65 (moderate d, high cost to exit but some agency). Extra-regional powers as powerful and mobile occupy d ≈ 0.40-0.50 (moderate d, capability to escalate or de-escalate). International law regime as institutional actor with arbitrage capacity occupies d ≈ 0.10-0.20 (low d, can be selectively invoked). The analytical observer at civilizational scale occupies d ≈ 0.72 (high analytical extraction—seeing structure that no single actor fully perceives). Directionality derivation confirms that extraction flows from smaller states to China, with substantial performance (FONOPs, legal argument) maintaining the illusion of coordination while enforcing asymmetric outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint's extractiveness (0.58) sits in the tangled_rope band (0.40-0.90), and it exhibits genuine coordination (UNCLOS, ASEAN, Code of Conduct negotiations) alongside asymmetric extraction (militarization, dominance, veto power). The classification is not mandatrophous—the tangled_rope type correctly captures the hybrid structure. The mandatrophy trap would arise if we classified it as pure rope (ignoring the asymmetric extraction) or pure snare (ignoring the coordination function). The tangled_rope classification avoids both errors by acknowledging that genuine coordination benefits coexist with genuine extraction harms, both measurable and both real. The omega variables (Code of Conduct credibility, militarization reversibility, FONOP effectiveness) represent the uncertainty: which mechanism will stabilize the constraint—the coordination function hardening into a binding treaty, or the extraction mechanism locking in through completed militarization? The constraint's future classification depends on how these omegas resolve: successful Code of Conduct → rope or scaffold; failed Code of Conduct → snare (for smaller states) or piton (for international law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militarization_reversibility,
    'Are the artificial islands and military installations reversible features, or have they created permanent structural facts that cannot be undone through diplomacy?',
    'Analysis of island fortification durability, sunk costs in military infrastructure, and integration into Chinese strategic doctrine. Scenario modeling of cost-benefit of demilitarization.',
    'If reversible: constraint could shift to rope (coordination-centered) via demilitarization agreement. If irreversible: constraint is locked into snare for smaller states indefinitely, raising likelihood of military conflict rather than negotiated settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militarization_reversibility, empirical, 'Reversibility of military installations on artificial islands').

omega_variable(
    fonop_effectiveness_paradox,
    'Do FONOPs constrain Chinese expansionism or performatively reinforce the constraint by treating it as legitimate (i.e., does the FONOP ritual validate Chinese authority by submitting to it)?',
    'Historical analysis of FONOP frequency vs. Chinese behavior changes; comparison of pre-FONOP and post-FONOP militarization rates; assessment of whether FONOPs have ever altered Chinese claims or island development.',
    'If FONOPs constrain: extra-regional power perspective is rope/scaffold (functional coordination). If FONOPs performatively validate: extra-regional power perspective is piton (ritualized assertion without substance), and the constraint hardens into snare for smaller states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fonop_effectiveness_paradox, empirical, 'Whether FONOPs functionally constrain or performatively validate Chinese claims').

omega_variable(
    code_of_conduct_credibility,
    'Is the Code of Conduct sunset pathway (ASEAN/China negotiating binding constraints) structurally viable, or is it performative theater obscuring permanent Chinese dominance?',
    'Analysis of CoC negotiation timeline vs. militarization acceleration; enforcement mechanism design; comparison with past failed regional arms-control frameworks.',
    'If viable: scaffold classification is correct, and genuine sunset is possible. If theater: CoC becomes another piton—shared legal language masking extraction—and the constraint locks into snare/tangled_rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_of_conduct_credibility, empirical, 'Viability of Code of Conduct as constraint sunset mechanism').

omega_variable(
    smaller_state_coalition_possibility,
    'Could smaller claimant states (Vietnam, Philippines, Malaysia) achieve exit through military coalition or alliance deepening, or is their powerless classification structural rather than contingent?',
    'Scenario modeling of military balance with allied support; assessment of whether alliance commitment is credible (would extra-regional powers actually defend against Chinese aggression?); correlation of alliance depth with risk-taking by smaller states.',
    'If coalition is viable: powerless perspective could shift to moderate/organized (constrained rather than trapped). If alliance commitment is doubtful: powerless classification is structural, and smaller states face indefinite snare unless regime change occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smaller_state_coalition_possibility, empirical, 'Structural viability of coalition among smaller claimant states').

omega_variable(
    economic_extraction_mechanism,
    'Beyond military dominance, does Chinese control of SCS shipping lanes extract economic rents through tolling, insurance, or supply-chain favoritism?',
    'Economic data on shipping route diversification, toll/insurance costs, trade flow changes post-militarization, preferential trade terms for states accepting Chinese dominance.',
    'If economic extraction is significant: the constraint has dual extraction mechanisms (military + economic), raising severity and locking smaller states into snare more deeply. If marginal: extraction is primarily military/political rather than economic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_extraction_mechanism, empirical, 'Economic extraction through control of shipping lanes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_china_sea_freedom_of_navigation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_fonop_tr_t0, south_china_sea_freedom_of_navigation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(scs_fonop_tr_t5, south_china_sea_freedom_of_navigation, theater_ratio, 5, 0.59).
narrative_ontology:measurement(scs_fonop_tr_t10, south_china_sea_freedom_of_navigation, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(scs_fonop_be_t0, south_china_sea_freedom_of_navigation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(scs_fonop_be_t5, south_china_sea_freedom_of_navigation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(scs_fonop_be_t10, south_china_sea_freedom_of_navigation, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(scs_fonop_su_t0, south_china_sea_freedom_of_navigation, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(scs_fonop_su_t5, south_china_sea_freedom_of_navigation, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(scs_fonop_su_t10, south_china_sea_freedom_of_navigation, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_china_sea_freedom_of_navigation, enforcement_mechanism).
narrative_ontology:affects_constraint(south_china_sea_freedom_of_navigation, first_island_chain_strategic_depth).
narrative_ontology:affects_constraint(south_china_sea_freedom_of_navigation, east_asian_power_balance).
narrative_ontology:affects_constraint(south_china_sea_freedom_of_navigation, regional_trade_route_security).

% DUAL FORMULATION NOTE:
% The South China Sea freedom of navigation constraint is downstream of Chinese strategic ambitions (island militarization, Nine-Dash Line enforcement) and upstream of broader first-island-chain balance, regional power distribution, and global shipping security. The constraint family decomposes into militarization dynamics (extraction-heavy, extractiveness ≈ 0.65), legal regime dynamics (performance-heavy, theater_ratio ≈ 0.75), and economic dynamics (coordination-heavy, extractiveness ≈ 0.35). Each story has distinct ε but shares the common mechanism of Chinese regional dominance assertion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(south_china_sea_freedom_of_navigation, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
