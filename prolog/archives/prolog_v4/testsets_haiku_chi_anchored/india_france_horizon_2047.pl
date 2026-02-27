% ============================================================================
% CONSTRAINT STORY: india_france_horizon_2047
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_france_horizon_2047, []).

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
 *   constraint_id: india_france_horizon_2047
 *   human_readable: India-France "Horizon 2047" Strategic Partnership
 *   domain: geopolitical/defense/technology
 *
 * SUMMARY:
 *   The India-France 'Horizon 2047' Strategic Partnership represents a
 *   comprehensive alignment on defense, space, nuclear energy, artificial
 *   intelligence, and critical technologies, with the explicit horizon of
 *   2047 (India's centennial of independence and a symbolic date in Indian
 *   strategic planning). This constraint exhibits the hallmark of
 *   tangled_rope: it combines genuine coordination functions (France-India
 *   collaboration on space launch, defense technology, nuclear fuel cycles)
 *   with asymmetric extraction mechanisms (polarization of South Asian
 *   regional dynamics, lock-in of technology ecosystems, reduction of
 *   strategic autonomy for smaller states). The theater_ratio (0.58) reflects
 *   that significant diplomatic rhetoric surrounds the partnership (summits,
 *   ceremonial declarations, invocation of 'civilizational ties') while the
 *   actual operational content is concentrated in defense and technology
 *   agreements. The partnership creates a structural dilemma: India gains
 *   defense deterrence capability and technological access but surrenders
 *   non-aligned positioning; France gains Indian Ocean presence and
 *   technology partnerships but becomes implicated in regional polarization;
 *   smaller South Asian states face a trap (constrained exit, no viable
 *   alternatives). The constraint's extractiveness has grown from 0.28 to
 *   0.52 over the measurement interval (0-10 years), indicating that initial
 *   optimism about mutual benefit ('rope' framing) has given way to clearer
 *   asymmetries ('snare' for smaller states, 'piton' for NAM legacy). The
 *   strategic intent (2047 horizon) naturalizes a geopolitical trajectory,
 *   risking false mountain classification.
 *
 * KEY AGENTS:
 *   - French Strategic Establishment: Institutional/arbitrage — benefits from Indian Ocean access, space launch partnerships, counterweight to China; high exit optionality
 *   - Indian Strategic Establishment: Organized/constrained — gains defense deterrence and technology access but constrained by China threat and Pakistan hostility; uses partnership to extract commitment costs from non-aligned states
 *   - Non-Aligned Movement States (NAM & Global South): Powerless/trapped — lose neutrality option, face polarization pressure; constrained by economic/security dependency; no meaningful exit
 *   - South Asian Regional Powers (Pakistan, Bangladesh): Moderate/constrained — caught in security dilemma; constrained by regional imbalance but gain some spillover benefits from technology standards
 *   - Technology Standards Bodies (IETF, IEEE, Open Architecture Coalitions): Organized/mobile — see bilateral lock-in as temporary constraint; building interoperable alternatives with genuine exit pathways
 *   - Non-Aligned Movement Institutional Legacy: Institutional/arbitrage — persists rhetorically but functions only performatively; actual capacity atrophied; piton classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_france_horizon_2047, 0.52).
domain_priors:suppression_score(india_france_horizon_2047, 0.65).
domain_priors:theater_ratio(india_france_horizon_2047, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_france_horizon_2047, extractiveness, 0.52).
narrative_ontology:constraint_metric(india_france_horizon_2047, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(india_france_horizon_2047, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_france_horizon_2047, tangled_rope).
narrative_ontology:human_readable(india_france_horizon_2047, "India-France \"Horizon 2047\" Strategic Partnership").
narrative_ontology:topic_domain(india_france_horizon_2047, "geopolitical/defense/technology").

domain_priors:requires_active_enforcement(india_france_horizon_2047).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, french_defense_industrial_complex).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, indian_strategic_autonomy_aspirants).
narrative_ontology:constraint_beneficiary(india_france_horizon_2047, bilateral_diplomatic_leverage).
narrative_ontology:constraint_victim(india_france_horizon_2047, non_aligned_movement_principles).
narrative_ontology:constraint_victim(india_france_horizon_2047, regional_balance_of_power_systems).
narrative_ontology:constraint_victim(india_france_horizon_2047, competing_technology_blocs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED MOVEMENT STATES (SNARE) — Smaller South Asian and African nations, historically positioned outside Cold War blocs, face a new trap: the India-France partnership reinforces a two-pole system (French-backed India vs China-backed alignment), eliminating neutrality options. No exit: these states cannot afford isolation. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.64.
constraint_indexing:constraint_classification(india_france_horizon_2047, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SOUTH ASIAN REGIONAL POWERS (TANGLED ROPE) — Constrained by security dilemma with India; cannot exit regional dynamics but gain some coordination benefit through technology spillover and infrastructure partnerships if included indirectly. Experience both extraction (security disadvantage) and coordination (access to French technology standards). d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FRENCH STRATEGIC INTERESTS (ROPE) — France experiences Horizon 2047 as pure coordination: it gains access to Indian Ocean naval assets, space launch capabilities, nuclear fuel cycle partnerships, and counterweight to China without direct military exposure. Exit option (arbitrage): France can redirect Indo-Pacific strategy to other partners. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(india_france_horizon_2047, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIAN STRATEGIC ESTABLISHMENT (TANGLED ROPE) — Organized actors (MEA, defense ministry, space agency) see dual function: coordination with France on technology and defense (rope benefit) AND extraction from non-aligned states through polarization (snare mechanism). India is constrained by geopolitical reality (China threat, Pakistan hostility) but also extracts commitment costs from smaller neighbors. d≈0.45, f(d)≈0.42, σ=0.9 → χ≈0.22.
constraint_indexing:constraint_classification(india_france_horizon_2047, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NON-ALIGNED MOVEMENT INSTITUTIONAL LEGACY (PITON) — The NAM, once a structural force in geopolitics, persists as theatrical invocation (NAM summits, rhetorical references to 'strategic autonomy') while its actual function has atrophied. Horizon 2047 represents the institutional inertia of NAM language masking alignment with France. theater_ratio=0.58 reflects that NAM still functions performatively (summits, declarations) but has lost structural capacity. d≈0.12, f(d)≈0.00, σ=1.0 → χ≈0.00.
constraint_indexing:constraint_classification(india_france_horizon_2047, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TECH STANDARDS & OPEN ARCHITECTURE (SCAFFOLD) — Global coalitions (IETF, IEEE, open-source AI governance) see Horizon 2047 as a temporary constraint: bilateral technology ecosystems lock in vendor relationships, but interoperable standards (5G/6G openness, AI model portability) are building alternatives. Exit path: multi-vendor supply chains. Sunset: 15-20 years as open standards mature. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.19.
constraint_indexing:constraint_classification(india_france_horizon_2047, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN CLAIM) — Some analysts frame Horizon 2047 as a natural law: 'rising powers must align with existing powers; bipolarity is inevitable; non-alignment is a historical artifact.' However, base extractiveness (0.52) and suppression (0.65) exceed mountain thresholds. This is a false summit: the framing naturalizes contingent power dynamics as immutable. Engine detects: emerges_naturally=false when ε>0.25. d≈0.70, f(d)≈1.13, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(india_france_horizon_2047, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_france_horizon_2047_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_france_horizon_2047, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_france_horizon_2047, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_france_horizon_2047, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_france_horizon_2047, TR),
    TR >= 0.70.

:- end_tests(india_france_horizon_2047_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The partnership contains genuine coordination elements (space, defense, nuclear) that benefit both parties, but it extracts commitment costs from smaller states through polarization pressure and lock-in of technology ecosystems. The trajectory (0.28→0.52 over interval) shows that initial mutual-benefit framing has given way to clearer asymmetries — the apparent 'strategic autonomy' gains for India come partly at the expense of real autonomy options for non-aligned states. Suppression (0.65): High. Smaller states face real barriers to exit: economic dependency, security dependency, lack of technology alternatives, and diplomatic isolation costs if they resist. The 2047 horizon naturalizes a 25-year commitment, making short-term exit prohibitive. Theater ratio (0.58): Moderate-high. The partnership is heavily ceremonial (civilizational ties, leadership summits, symbolic 2047 horizon) but contains real operational content in defense and space. The rise from 0.35 to 0.58 reflects increased rhetorical (vs actual) emphasis as implementation faces technical obstacles and cost pressures.
 *
 * PERSPECTIVAL GAP:
 *   French institutional perspective sees rope (pure coordination: space launch, defense, technology partnerships without military entanglement). Indian strategic establishment sees tangled_rope (coordination with France + extraction of commitment costs from smaller states). Non-aligned states see snare (trapped in polarization with no exit). South Asian regional powers see tangled_rope (constrained by India's partnership but also benefit from technology spillover). Technology standards bodies see scaffold (temporary lock-in with genuine exit pathways through interoperable standards). NAM legacy sees piton (rhetorical persistence masking functional atrophy). Analytical observer risks seeing mountain ('bipolarity is inevitable, alignment is a law of power politics') but structural data (ε=0.52, suppression=0.65) contradicts this — the constraint is contingent, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   French establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Indian establishment: Beneficiary (relative to non-alignment) + organized + constrained (by security threats) → d≈0.45, f(d)≈0.42. Mixed experience. Non-aligned states: Victim + powerless + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. South Asian regional powers: Victim + moderate + constrained → d≈0.68, f(d)≈1.05. High extraction. Tech standards bodies: Victim (to lock-in) + organized + mobile → d≈0.35, f(d)≈0.30. Moderate extraction, but genuine exit path. NAM legacy: Neither beneficiary nor victim; institutional inertia → d≈0.12, f(d)≈0.00. Piton due to theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying the distinction between French-Indian coordination (genuine rope) and the constraint's systemic extraction from non-aligned states (snare). The mandatrophy arises from the question: 'Is this partnership coordination or extraction?' The answer is BOTH, at different scales. France-India relationship is coordination (rope). India-smaller states relationship is extraction (snare). The partnership exhibits tangled_rope structure: it requires active enforcement (ongoing alliance management, technology transfer agreements, military exercises), contains both coordination (space, defense innovation) and asymmetric extraction (polarization, lock-in), and exhibits rising theater as rhetorical emphasis outpaces operational gains. The false mountain (analytical observer's 'bipolarity is inevitable') is detected by the structural data exceeding mountain thresholds — the constraint is contingent geopolitical choice, not natural law. The sunset clause (implicit in 'Horizon 2047' temporal framing) is aspirational, not structural — there is no automatic mechanism to dissolve the partnership; the 2047 date is symbolic, not operational.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_extraction_intent,
    'Is France using Horizon 2047 as genuine coordination with India, or as a mechanism to extract strategic advantage (naval basing, space access, technology transfer terms favorable to France)?',
    'Analysis of treaty asymmetries: technology transfer rates, defense manufacturing licensing terms, space launch cost structures, nuclear fuel pricing; comparison with France-EU partnership terms',
    'If coordination-dominant: rope perspective strengthens; tangled_rope weakens. If extraction-dominant: snare and tangled_rope perspectives confirmed; rope perspective is France''s self-serving framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_extraction_intent, empirical, 'Degree to which France extracts strategic advantage vs mutual coordination').

omega_variable(
    indian_autonomy_preservation,
    'Does Horizon 2047 enhance Indian strategic autonomy (independent deterrent, diversified partnerships) or reduce it (lock-in to French technology ecosystem, reduced negotiating leverage with other powers)?',
    'Tracking India''s technology diversification post-2047; analysis of lock-in costs in defense procurement; comparison of India''s leverage in France partnership vs China threat dynamic',
    'If autonomy-enhancing: tangled_rope confirmed with moderate beneficiary component. If autonomy-reducing: snare elements strengthen; India''s constrained exit becomes apparent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indian_autonomy_preservation, empirical, 'Whether partnership enhances or reduces Indian strategic autonomy').

omega_variable(
    non_aligned_movement_resilience,
    'Can the NAM or successor institutions (BRICS, Global South coalitions) regenerate as functional structures, or is bipolarity permanently trapping smaller states?',
    'Longitudinal analysis of Global South coordination capacity; measurement of structural alternatives to India-France-like partnerships; emergence of new bloc-resistant technologies (distributed AI, satellite constellation independence)',
    'If resilience possible: snare classification weakens; scaffold and rope elements emerge. If permanent trap: snare deepens; powersless agent status becomes structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_aligned_movement_resilience, conceptual, 'Whether non-aligned alternatives can rebuild structural capacity').

omega_variable(
    technology_lock_in_reversibility,
    'Are the technology ecosystems locked in by Horizon 2047 (French fighter jets, nuclear vendors, space launch chains) reversible, or do switching costs become prohibitive?',
    'Cost analysis of technology transitions; historical comparison with previous defense partnerships (India-Soviet, India-Israel); tracking of indigenous alternatives (Tejas fighter, Chandrayaan, PSLV autonomy)',
    'If reversible: scaffold and constrained perspectives correct. If irreversible: snare elements strengthen; extraction becomes permanent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_lock_in_reversibility, empirical, 'Whether technology lock-in is reversible or permanent').

omega_variable(
    china_bloc_formation_necessity,
    'Is India-France alignment necessary because China is actively forming a hostile bloc, or does Horizon 2047 ITSELF CREATE the bloc formation logic, converting potential neutrality into structural hostility?',
    'Counterfactual analysis: comparison of China''s trade/tech behavior toward non-aligned vs explicitly India-France-aligned states; measurement of bloc formation rate before and after Horizon 2047 declaration',
    'If China-driven: tangled_rope justified as defensive. If Horizon 2047-driven: the partnership becomes self-fulfilling prophecy; rope coordination becomes snare extraction of smaller states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_bloc_formation_necessity, conceptual, 'Whether bipolarity is China-driven necessity or Horizon 2047-created').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_france_horizon_2047, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ifh2047_tr_t0, india_france_horizon_2047, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ifh2047_tr_t5, india_france_horizon_2047, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ifh2047_tr_t10, india_france_horizon_2047, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ifh2047_be_t0, india_france_horizon_2047, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ifh2047_be_t5, india_france_horizon_2047, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(ifh2047_be_t10, india_france_horizon_2047, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_france_horizon_2047, enforcement_mechanism).
narrative_ontology:affects_constraint(india_france_horizon_2047, south_asian_security_dilemma).
narrative_ontology:affects_constraint(india_france_horizon_2047, non_aligned_movement_degradation).
narrative_ontology:affects_constraint(india_france_horizon_2047, indo_pacific_strategic_competition).
narrative_ontology:affects_constraint(india_france_horizon_2047, technology_ecosystem_lock_in).

% DUAL FORMULATION NOTE:
% Horizon 2047 decomposes into three structurally distinct constraints: (1) France-India bilateral coordination (ε≈0.15, rope), (2) India's extraction from non-aligned states (ε≈0.65, snare), (3) technology ecosystem lock-in with reversibility uncertainty (ε≈0.48, tangled_rope). This story aggregates all three; each could be separately analyzed with its own ε. The current decomposition treats the partnership as a unified constraint because the three mechanisms are causally coupled — France-India coordination enables India's extraction from others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_france_horizon_2047, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
