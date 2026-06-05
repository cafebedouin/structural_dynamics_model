% ============================================================================
% CONSTRAINT STORY: hormuz_leverage_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hormuz_leverage_paradox, []).

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
 *   constraint_id: hormuz_leverage_paradox
 *   human_readable: Strait of Hormuz Closure Leverage Paradox
 *   domain: international_relations/military_strategy/energy_security
 *
 * SUMMARY:
 *   The Strait of Hormuz closure paradox represents a structurally stable
 *   deterrence equilibrium where Iran's highest-impact retaliation option is
 *   rendered unusable by the certainty of regime-ending US military response.
 *   Approximately 21 million barrels per day of petroleum and petroleum
 *   products flow through the 21-mile-wide strait at its narrowest point,
 *   representing roughly 21% of global oil consumption and 30% of
 *   seaborne-traded oil. Iran possesses the technical capability to disrupt
 *   this flow through mine-laying, anti-ship missiles, and small-boat swarm
 *   tactics, but any sustained closure would trigger overwhelming US military
 *   retaliation targeting not just Iranian naval assets but regime
 *   infrastructure. This creates a paradox: the option exists and provides
 *   some deterrence value (preventing regional rivals from assuming Iran has
 *   no escalation options), but its use is synonymous with regime suicide,
 *   making it unusable except in scenarios where the regime has already
 *   concluded it faces existential threat. The constraint has evolved from a
 *   genuine mutual deterrence mechanism (1990s-2000s) toward increasing
 *   theater: Iranian statements about closure capability are performative
 *   signals rather than credible threats, US naval presence is justified by a
 *   threat that cannot be actualized, and international maritime law
 *   enforcement is maintained ritually while actual security depends on
 *   military power projection. The constraint coordinates global energy
 *   security (genuine rope function) while extracting from Iranian strategic
 *   autonomy (snare function from Iranian perspective) and imposing costs on
 *   regional actors (tangled rope function from GCC perspective).
 *
 * KEY AGENTS:
 *   - Iranian Strategic Autonomy: Primary victim (powerless/trapped) — highest-impact option is unusable except as regime suicide; no exit from strategic bind
 *   - US Fifth Fleet: Primary beneficiary (institutional/arbitrage) — paradox justifies permanent forward deployment and budget priority; can redeploy if threat diminishes
 *   - Global Oil Importers: Beneficiary (institutional/mobile) — benefit from reliable transit and price signals incentivizing diversification; can shift sourcing over time
 *   - Regional US Allies (GCC): Mixed position (organized/constrained) — benefit from security umbrella but bear militarization costs and Iranian proxy pressure; cannot exit without facing Iran directly
 *   - Regional Stability: Abstract victim (moderate/constrained) — coordination against worst-case escalation but persistent crisis risk and economic costs
 *   - International Maritime Law: Institutional actor (institutional/arbitrage) — legal framework maintained theatrically but enforcement depends on US military power, not norms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and asymmetric extraction; constraint's claimed type
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hormuz_leverage_paradox, 0.48).
domain_priors:suppression_score(hormuz_leverage_paradox, 0.62).
domain_priors:theater_ratio(hormuz_leverage_paradox, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hormuz_leverage_paradox, extractiveness, 0.48).
narrative_ontology:constraint_metric(hormuz_leverage_paradox, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hormuz_leverage_paradox, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hormuz_leverage_paradox, tangled_rope).
narrative_ontology:human_readable(hormuz_leverage_paradox, "Strait of Hormuz Closure Leverage Paradox").
narrative_ontology:topic_domain(hormuz_leverage_paradox, "international_relations/military_strategy/energy_security").

domain_priors:requires_active_enforcement(hormuz_leverage_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hormuz_leverage_paradox, us_fifth_fleet).
narrative_ontology:constraint_beneficiary(hormuz_leverage_paradox, global_oil_importers).
narrative_ontology:constraint_beneficiary(hormuz_leverage_paradox, regional_us_allies).
narrative_ontology:constraint_victim(hormuz_leverage_paradox, iranian_strategic_autonomy).
narrative_ontology:constraint_victim(hormuz_leverage_paradox, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN STRATEGIC AUTONOMY (SNARE) — Trapped in a deterrence framework where the highest-impact retaliation option (Hormuz closure) is unusable except as regime suicide. No exit from the strategic bind: conventional military inferiority prevents direct confrontation, nuclear program draws sanctions and sabotage, proxy networks invite retaliation, and the strait closure option exists only as theater. Maximum extraction: the constraint removes Iran's most valuable strategic asset by making its use synonymous with regime termination.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL STABILITY (TANGLED ROPE) — Constrained by the paradox that credible Iranian closure capability maintains some deterrence balance (preventing adventurism by regional rivals) but the same capability creates permanent crisis risk. Mixed extraction: the constraint coordinates against worst-case escalation (genuine rope function) but extracts from regional actors through persistent insurance costs, elevated military postures, and economic uncertainty. The coordination function is real but asymmetrically distributed.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US FIFTH FLEET (ROPE) — Primary beneficiary experiencing the constraint as coordination. The paradox justifies permanent forward deployment, secures host-nation basing agreements, and maintains budget priority for naval assets. The Iranian threat that cannot be used is the perfect threat from a force-posture perspective: serious enough to justify presence, contained enough to be manageable. Arbitrage exit options: can redeploy assets if threat diminishes or reframe mission if strategic priorities shift.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL OIL IMPORTERS (ROPE) — Benefit from the constraint's coordination function: the paradox keeps Hormuz open while maintaining price signals that incentivize supply diversification and strategic reserves. Mobile exit options: can shift sourcing to non-Hormuz suppliers (US shale, West African, North Sea) over generational timescales. Experience low effective extraction because the insurance and volatility costs are manageable relative to the coordination benefit of reliable transit.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL US ALLIES (TANGLED ROPE) — Organized actors (Saudi Arabia, UAE, Kuwait) with constrained exit. Benefit from US security umbrella and Iranian containment (coordination function) but bear costs of permanent militarization, vulnerability to Iranian proxy attacks, and economic exposure to oil price shocks. Cannot exit the security dependency without facing Iranian pressure directly. Mixed extraction: genuine security coordination layered with asymmetric burden-sharing and loss of strategic autonomy.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL MARITIME LAW (PITON) — The formal legal framework (UNCLOS freedom of navigation, international waters doctrine) is maintained theatrically but has atrophied in functional terms. Enforcement depends entirely on US naval power projection, not on legal norms or multilateral institutions. The legal ritual persists through inertia: states invoke UNCLOS in diplomatic statements, but the actual mechanism preventing closure is military deterrence, not law. High theater ratio: the constraint's legal framing is performative.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global analytical perspective, the constraint exhibits both genuine coordination (prevents worst-case closure, maintains energy security for global economy) and asymmetric extraction (removes Iranian strategic options, concentrates costs on regional actors, justifies permanent US force posture). The paradox is structurally stable: Iran cannot use the option without regime termination, but cannot abandon the option without losing deterrence credibility. This is the constraint's claimed type and the basis for classification.
constraint_indexing:constraint_classification(hormuz_leverage_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hormuz_leverage_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hormuz_leverage_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hormuz_leverage_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hormuz_leverage_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hormuz_leverage_paradox, TR),
    TR >= 0.70.

:- end_tests(hormuz_leverage_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, now crossing the tangled_rope threshold. The constraint removes Iran's most valuable strategic asset by making its use synonymous with regime termination, which is significant extraction from Iranian strategic autonomy. The constraint also provides genuine coordination benefits (global energy security, mutual deterrence preventing adventurism), but the extraction is substantial enough to firmly establish tangled_rope classification. The value reflects that extraction is real and significant — Iran's highest-impact option is neutralized — while coordination function remains genuine. Suppression (0.62): Moderate-high. Iran faces significant barriers to alternative strategic postures: conventional military inferiority prevents direct confrontation with US/allies, nuclear program development draws sanctions and sabotage, proxy networks invite retaliation, and the strait closure option exists only as theater. The regime has limited strategic options and faces persistent external pressure. The value reflects substantial constraint on Iranian strategic autonomy with limited exit paths. Theater ratio (0.58): Moderate, below piton threshold. Multiple theatrical elements: Iranian statements about closure capability are performative signals rather than credible threats (leadership knows use equals regime termination), US naval presence is justified by a threat that cannot be actualized without triggering the response that makes it suicidal, international maritime law is invoked ritually while actual enforcement depends on military power, and insurance markets price a risk that both sides have strong incentives to prevent. The theater has increased over the interval as the paradox has become more widely understood and the performative aspects more prominent, but remains below piton threshold because genuine coordination function persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — the Hormuz closure paradox — appears as pure extraction (snare) from the Iranian perspective, mixed coordination-extraction (tangled_rope) from the regional stability and GCC perspectives, pure coordination (rope) from the US Fifth Fleet and global oil importer perspectives, and degraded ritual (piton) from the international maritime law perspective. The Iranian regime experiences maximum extraction: their highest-impact option is neutralized by making its use synonymous with regime termination. The US Fifth Fleet experiences coordination: the paradox justifies their mission and force structure. Global oil importers experience coordination: reliable transit with manageable insurance costs. GCC states experience mixed extraction: security benefits layered with dependency costs. The analytical observer sees tangled_rope: genuine mutual deterrence function (coordination) layered with asymmetric extraction (Iranian options removed, regional costs imposed, US posture justified). The perspectival gap is not a disagreement about facts but a structural consequence of different positions relative to the extraction flow and coordination benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Iranian strategic autonomy is the primary victim with trapped exit options, yielding high directionality (d ≈ 0.95) and maximum experienced extraction. The constraint removes Iran's highest-impact option by making its use regime-ending. US Fifth Fleet and global oil importers are primary beneficiaries with arbitrage/mobile exit options, yielding low directionality (d ≈ 0.05-0.15) and low or negative experienced extraction — they benefit from the coordination function (reliable transit, justified force posture) without bearing significant costs. Regional US allies (GCC states) occupy a mixed position: they benefit from the security umbrella (beneficiary status) but are constrained by dependency and bear militarization costs (victim characteristics), with constrained exit options. This yields moderate directionality (d ≈ 0.50) and moderate experienced extraction, producing the tangled_rope classification from their perspective. Regional stability as an abstract collective good is a victim with constrained exit, yielding moderate-high directionality. The analytical observer sees the full structure: genuine coordination function (preventing worst-case closure, maintaining energy security) layered with asymmetric extraction (Iranian strategic autonomy removed, costs concentrated on regional actors, US force posture justified by unusable threat).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the classification depends on the observer's structural position. From the Iranian perspective, the constraint is a snare: the highest-impact retaliation option is rendered unusable except as regime suicide, with no exit from the strategic bind. From the US Fifth Fleet perspective, the constraint is a rope: it coordinates global energy security while justifying forward deployment. From the GCC perspective, the constraint is a tangled_rope: genuine security coordination layered with dependency costs and loss of autonomy. From the analytical perspective, the constraint is a tangled_rope: the paradox exhibits both genuine coordination function (mutual deterrence, energy security) and asymmetric extraction (Iranian strategic autonomy removed, costs concentrated regionally, US force posture justified by unusable threat). The mandatrophy is resolved by recognizing that all classifications are legitimate perspectival readings of the same structural data. The constraint is not 'really' a rope or 'really' a snare — it is a presheaf over the observation site, with different fibers at different positions. The analytical classification (tangled_rope) reflects the view that sees both the coordination function and the asymmetric extraction, which is the constraint's claimed type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    closure_capability_credibility,
    'Does Iran retain genuine technical capability to close Hormuz for a sustained period (weeks to months), or has US/allied countermeasure development degraded this to a temporary disruption capability (days)?',
    'Classified military assessments of Iranian mine-laying capacity, anti-ship missile effectiveness against modern countermeasures, and US/allied mine-clearing and air superiority timelines. Observable proxies: insurance market pricing of closure risk, tanker routing decisions, US Navy mine countermeasure force structure.',
    'If capability degraded to temporary disruption: constraint shifts toward piton (theater of deterrence with atrophied function). If sustained closure remains feasible: constraint remains tangled_rope with genuine coordination function (mutual deterrence) alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_capability_credibility, empirical, 'Whether Iranian closure capability remains credible or has degraded to theater').

omega_variable(
    alternative_export_routes_sufficiency,
    'Can alternative oil export routes (pipelines to Mediterranean, Red Sea terminals, overland to Central Asia) handle sufficient volume to make Hormuz closure economically survivable for Gulf producers?',
    'Infrastructure capacity analysis: existing pipeline throughput, expansion timelines, capital costs, and political feasibility of route diversification. Compare to current Hormuz transit volumes (21 million barrels/day, ~21% of global petroleum liquids consumption).',
    'If alternatives sufficient: Gulf states gain exit options, reducing their victim status and shifting constraint toward rope from their perspective. If alternatives insufficient: Gulf states remain trapped in Hormuz dependency, maintaining tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_export_routes_sufficiency, empirical, 'Whether alternative export routes provide genuine exit from Hormuz dependency').

omega_variable(
    regime_survival_calculus_threshold,
    'At what threshold of external pressure (sanctions severity, military strikes on nuclear facilities, proxy network degradation) does the Iranian regime calculate that Hormuz closure becomes rational despite guaranteed massive retaliation?',
    'Historical analysis of regime decision-making under extreme pressure; game-theoretic modeling of regime survival vs national survival trade-offs; intelligence assessments of leadership risk tolerance and internal factional dynamics.',
    'If threshold is reachable through sanctions/strikes: constraint is less stable than assumed, and the ''unusable option'' may become usable under specific conditions, shifting toward snare from Iranian perspective (trapped with no good options). If threshold is unreachable short of invasion: constraint remains stable tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_survival_calculus_threshold, conceptual, 'Whether external pressure could make Hormuz closure rational for Iranian regime').

omega_variable(
    energy_transition_timeline,
    'Does the global energy transition away from fossil fuels occur on a timeline (pre-2040) that reduces Hormuz strategic importance before the constraint''s other structural features change?',
    'Energy transition modeling: EV adoption rates, renewable capacity additions, oil demand peak timing, and geopolitical implications of reduced Gulf oil dependency. Cross-reference with infrastructure lock-in and capital cycle timelines.',
    'If transition pre-2040: constraint has implicit sunset clause (scaffold characteristics emerge), as Hormuz becomes less critical and Iranian leverage naturally diminishes. If transition post-2050: constraint persists as tangled_rope through mid-century.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_transition_timeline, empirical, 'Whether energy transition provides natural sunset for Hormuz strategic importance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hormuz_leverage_paradox, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hormuz_theater_1990, hormuz_leverage_paradox, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hormuz_theater_2000, hormuz_leverage_paradox, theater_ratio, 10, 0.46).
narrative_ontology:measurement(hormuz_theater_2010, hormuz_leverage_paradox, theater_ratio, 20, 0.52).
narrative_ontology:measurement(hormuz_theater_2025, hormuz_leverage_paradox, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(hormuz_extract_1990, hormuz_leverage_paradox, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hormuz_extract_2000, hormuz_leverage_paradox, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(hormuz_extract_2010, hormuz_leverage_paradox, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(hormuz_extract_2025, hormuz_leverage_paradox, base_extractiveness, 35, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hormuz_leverage_paradox, global_infrastructure).
narrative_ontology:affects_constraint(hormuz_leverage_paradox, iranian_nuclear_program_leverage).
narrative_ontology:affects_constraint(hormuz_leverage_paradox, gcc_security_dependency).
narrative_ontology:affects_constraint(hormuz_leverage_paradox, us_middle_east_force_posture).

% DUAL FORMULATION NOTE:
% The Hormuz leverage paradox is structurally linked to Iranian nuclear program leverage (alternative strategic option when Hormuz option is neutralized), GCC security dependency (regional actors' reliance on US umbrella given Iranian threat), and US Middle East force posture (forward deployment justified by Hormuz transit security mission). Each represents a distinct constraint with its own extractiveness value, but they form a coupled system where changes in one affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hormuz_leverage_paradox, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
