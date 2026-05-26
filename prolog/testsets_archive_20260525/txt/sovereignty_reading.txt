% ============================================================================
% CONSTRAINT STORY: sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sovereignty_reading
 *   human_readable: Border Authority via Territorial Sovereignty (Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   The sovereignty reading instantiates one interpretation of the contested
 *   border-legitimacy kernel. Under this reading, states derive legitimate
 *   authority to exclude from the principle of territorial sovereignty — the
 *   claim that a bounded political community has the right to control access
 *   to its territory and allocate membership. This reading frames border
 *   enforcement as coordination (managing shared resources, maintaining
 *   social stability, enabling collective self-determination) rather than as
 *   extraction. The constraint exhibits tangled_rope structure: genuine
 *   coordination benefits coexist with asymmetric extraction of mobility from
 *   non-citizens. Excluded migrants (the powerless/trapped perspective)
 *   experience pure snare — absolute prohibition enforced by violence with
 *   zero perceived coordination benefit. The sovereign state
 *   (institutional/arbitrage) experiences rope — border coordination without
 *   experienced extraction. The analytical observer at civilizational scope
 *   risks classifying sovereignty as mountain (immutable principle), but the
 *   structural data reveals this as a false summit: sovereignty is a recently
 *   constructed institutional arrangement (<400 years old) that benefits
 *   identifiable actors (state elites, citizen-privilege holders) and is
 *   maintained through active enforcement and suppression mechanisms. The
 *   measurement trajectory shows rising extractiveness (0.42 → 0.68 over 200
 *   time units) and slightly rising theater ratio (0.28 → 0.38), indicating
 *   that the 'coordination' framing is becoming increasingly performative
 *   while actual extraction from excluded populations intensifies. This
 *   reading is explicitly ONE instantiation of the contested kernel;
 *   alternative readings (freedom-of-movement, humanitarian-obligation) would
 *   emphasize different structural features and produce different
 *   extractiveness values and victim sets.
 *
 * KEY AGENTS:
 *   - Sovereign State Authority: Primary beneficiary (institutional/arbitrage) — maintains territorial control, allocates membership, enforces collective provisioning
 *   - Citizenship Privilege Holders: Primary beneficiary (powerful/arbitrage or organized/mobile) — enjoy unimpeded movement within territory and carry passport authority recognized globally
 *   - Excluded Migrants: Primary victim (powerless/trapped) — face absolute barrier to entry; no exit from exclusion except by abandoning origin place or accepting permanent displacement
 *   - Border-Adjacent Population: Secondary victim (moderate/constrained) — constrained by high crossing costs; also benefit from labor-market protection and cultural preservation
 *   - Transnational Migrant Networks: Organized agents (organized/mobile) — perceive border as temporary problem with sunset horizon; build alternative pathways and generational strategies
 *   - International Legal System: Institutional actor (institutional/arbitrage) — maintains performative justification for sovereignty through legal architecture (UN Charter, Westphalian principles) while actual coordination happens through bilateral negotiation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing sovereignty as immutable when it is contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereignty_reading, 0.58).
domain_priors:suppression_score(sovereignty_reading, 0.72).
domain_priors:theater_ratio(sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sovereignty_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(sovereignty_reading, "Border Authority via Territorial Sovereignty (Sovereignty Reading)").
narrative_ontology:topic_domain(sovereignty_reading, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(sovereignty_reading, fixed_text).
narrative_ontology:cs_authority_grounding(sovereignty_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(sovereignty_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereignty_reading, state_territorial_control).
narrative_ontology:constraint_beneficiary(sovereignty_reading, citizenship_privilege_holders).
narrative_ontology:constraint_victim(sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(sovereignty_reading, family_separation_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute barrier to entry enforced by violence; no exit from exclusion except by abandoning place of origin or accepting indefinite displacement. Suppression is maximal: legal prohibition, physical barriers, deportation mechanisms, and threat of violence create irreducible constraint. No perceived coordination benefit; pure extraction of mobility from non-citizens to benefit the state.
constraint_indexing:constraint_classification(sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BORDER-ADJACENT POPULATION (TANGLED ROPE) — Constrained by proximity and high crossing costs; also benefits from border enforcement (protection from labor competition, resource scarcity reduction, social stability maintenance). Experiences both extraction (restricted mobility, visa barriers) and coordination (shared border resource allocation, cultural preservation norms). High suppression offset by genuine coordination function.
constraint_indexing:constraint_classification(sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVEREIGN STATE AUTHORITY (ROPE) — Experiences border as pure coordination mechanism: manages migration flows, allocates public goods, maintains demographic stability, enforces contract terms for entry. State has full arbitrage capacity (can redefine borders, change entry criteria, negotiate bilateral agreements). Sees extraction as legitimate cost of coordination — the 'taking' of tax revenue from residents or restriction of citizen mobility is framed as necessary contribution to collective provisioning.
constraint_indexing:constraint_classification(sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSNATIONAL MIGRANT NETWORK (SCAFFOLD) — Organized agents (diaspora networks, remittance corridors, migration brokers) perceive border enforcement as a temporary coordination problem with a sunset: as economic integration deepens and labor markets globalize, border control mechanisms become increasingly costly and unsustainable. This perspective sees the sovereignty-based exclusion as a vestigial constraint that will erode through market and network pressure. Low effective extraction because the network has agency, alternative pathways, and generational optimism about regime change.
constraint_indexing:constraint_classification(sovereignty_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL SYSTEM (PITON) — The formal international legal order (UN Charter, Westphalian sovereignty, passport system) maintains border enforcement as a performative ritual: the legal architecture declares sovereignty while the functional coordination happens through bilateral agreements, trade regimes, and informal labor mobility. The theater ratio is high because the legal system expends enormous institutional energy justifying sovereignty as immutable while actual border regimes are highly negotiable. The legal system has become the inertial mechanism that sustains border enforcement even as its functional necessity decays.
constraint_indexing:constraint_classification(sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, territorial sovereignty and the right to exclude are presented as immutable properties of the international system: borders are treated as logical consequences of collective self-determination and resource stewardship. This perspective views border enforcement as an irreducible feature of any decentralized political order. However, the structural data (identifiable beneficiaries, active enforcement, high suppression against powerless agents) contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts mobility from non-citizens and returns it asymmetrically to citizens. The value reflects that extraction is real and measurable (visa barriers, deportation mechanisms, restricted labor market access) but partially offset by coordination benefits that accrue to the broader included population (resource allocation, public goods provisioning, social stability). The measurement trajectory shows rising extractiveness over 200 time units (0.42 → 0.68), indicating that the coordination function is degrading while pure extraction intensifies — likely due to accumulating migration pressures and hardening border enforcement. Suppression (0.72): High. Barriers to exit from exclusion are nearly absolute: legal prohibition, physical walls, deportation enforcement, visa restrictions, and threat of violence create compounded suppression. An excluded migrant cannot unilaterally choose inclusion; their only exit is accepting permanent exclusion or risking illegal crossing (which adds legal jeopardy). However, suppression is not complete (0.95+) because some migration pathways exist (legal immigration channels, asylum claims, remittance-based transnational residence), and these keep the classification at tangled_rope rather than pure snare. Theater ratio (0.38): Moderate-low. The sovereignty reading emphasizes actual coordination function (resource management, collective governance) rather than performative justification. However, the measurement trajectory shows rising theater (0.28 → 0.38), suggesting that as material coordination rationales weaken (globalization, labor market integration), the legal-philosophical justification (sovereignty as immutable natural right) becomes increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across observation contexts. The excluded migrant sees pure snare (extraction without coordination benefit, maximum suppression). The state authority sees rope (coordination mechanism, voluntary participation through citizenship framing). The analytical observer at civilizational scope risks seeing mountain (sovereignty as immutable law of political organization). The transnational migrant network sees scaffold (temporary problem with generational sunset as market integration deepens). The international legal system sees piton (formal legal architecture maintains the constraint through inertia, but functional necessity is declining). The border-adjacent population sees tangled rope (mixed coordination and extraction). No single classification dominates because the constraint's function is genuinely contested: does it coordinate public provisioning and enable collective self-determination (state's perspective), or does it extract and exclude (migrant's perspective)? The perspectival gap reflects deeper structural ambiguity about what borders actually do — the kernel is under-determined by the evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereignty reading constructs directionality through four structural axes: (1) Beneficiary status: state and citizen-privilege holders clearly benefit from exclusion (they control territory, allocate membership, extract labor-market protection). (2) Victim status: excluded migrants and border-adjacent populations bear costs (restricted mobility, family separation, labor market subordination). (3) Exit options: the state has arbitrage capacity (can redefine borders, negotiate agreements, adjust enforcement); excluded migrants are trapped (no unilateral escape from legal prohibition). (4) Power asymmetry: state capacity to enforce exclusion is institutional/organized; migrant resistance is distributed and resource-constrained. These factors combine to create high directionality values (d) for excluded migrants (d ≈ 0.92, maximum targeting) and low d for state actors (d ≈ 0.08, maximum beneficiary status). The chi formula scales these directionalities by scope: the global scope (σ=1.2) amplifies the extraction experienced by migrants and the benefit enjoyed by states. From the migrant perspective, effective extraction reaches χ ≈ 0.95 (snare threshold); from the state perspective, χ ≈ -0.15 (negative extraction, meaning net benefit). The tangled_rope classification at the moderate/constrained perspective reflects that some agents (border-adjacent populations, low-wage workers competing for jobs) experience mixed directionality: they are beneficiaries of labor-market protection but victims of reduced social mobility and increased labor supply from irregular migrants.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading resolves mandatrophy by accepting that a contested kernel will produce different classifications from different perspectives. The mandate (constraint classification) cannot be uniform when the kernel itself — what borders are for — is contested. The state and analytical observer see coordination (rope or mountain); the migrant sees extraction (snare); the moderate actor sees both (tangled rope). Rather than forcing a single type, the framework preserves the perspectival multiplicity and routes disagreement through omega variables: which reading of the kernel is correct? Does coordination require exclusion? Is sovereignty immutable or contingent? These are the actual structural questions; classification types are their shadows. The rising extractiveness trajectory (0.42 → 0.68) suggests that the 'coordination benefit' justification is weakening over time — the constraint is becoming a snare with increasingly thin coordination cover. This is the diagnostic signal the measurements provide: if the coordination function were stable and genuine, extractiveness should not accumulate; rising extractiveness indicates that legitimizing rationales are becoming theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_kernel_contest,
    'Is the sovereignty reading the correct interpretation of the border-legitimacy kernel, or do freedom-of-movement or humanitarian-obligation readings better capture what borders actually do?',
    'Historical and comparative analysis: which reading best explains border regimes'' actual function across time and geography? Does sovereignty protect collective goods (the reading''s claim) or primarily serve elite extraction and labor discipline (alternative readings)? What happens to border effectiveness when sovereignty framing is weakened?',
    'If sovereignty reading is dominant: border enforcement is legitimate constraint (Rope/Tangled Rope from most perspectives). If humanitarian or freedom readings are dominant: border enforcement is snare (extraction mechanism). If readings are incommensurable: no single reading can stabilize the classification, and the framework must model border-legitimacy as an unstable contested kernel with multiple valid instantiations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_kernel_contest, conceptual, 'Which reading of border-legitimacy kernel is structurally correct').

omega_variable(
    collective_self_determination_mechanism,
    'Does territorial sovereignty actually enable collective self-determination for the included population, or does it primarily enable exit control for state elites?',
    'Comparative analysis of border regimes with and without sovereignty framing; measurement of whether border restrictions correlate with (a) improved provision of public goods, (b) enhanced democratic participation, or (c) increased state capacity for extraction. Does relaxation of borders measurably decrease citizens'' collective agency?',
    'If sovereignty enables self-determination: extractiveness should be lower (0.35–0.45, shifting toward Rope). If sovereignty primarily enables extraction: extractiveness should be higher (0.65–0.75, shifting toward Snare). Current value (0.58) assumes mixed function; empirical resolution would move classification significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_self_determination_mechanism, empirical, 'Whether sovereignty enables collective self-determination or state extraction').

omega_variable(
    alternative_coordination_mechanisms,
    'Can public goods (education, healthcare, infrastructure, social stability) be provided without territorial exclusion, or does exclusion prove necessary for collective provisioning?',
    'Case studies of open-border or high-mobility regimes (EU freedom of movement, historical frontier regions without state enforcement); comparison of public goods provision under high-exclusion vs low-exclusion regimes; analysis of whether externalities cited as justifying borders (labor competition, fiscal burden) materialize empirically.',
    'If public goods require exclusion: the coordination function is real, tangled_rope classification confirmed. If public goods provision is orthogonal to exclusion: the ''coordination'' is theater, and the constraint reclassifies as snare with high theater ratio. This is the crux of whether suppression (0.72) reflects legitimate collective defense or illegitimate coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Whether public goods provision requires territorial exclusion').

omega_variable(
    false_summit_sovereignty,
    'Is sovereignty an immutable principle (mountain), or is the ''natural law'' framing a cover story that permits extractive border enforcement?',
    'Historical documentation of sovereignty''s contingency: the Westphalian system is <400 years old; borders have been radically redrawn and renegotiated throughout human history; many contemporary borders were drawn by colonial powers without indigenous consent. If sovereignty is contingent and recently constructed, the mountain classification is a false summit.',
    'If sovereignty is contingent: the analytical observer''s mountain is reclassified to tangled_rope or snare. The ''natural law'' framing is revealed as theater that naturalizes power relations. The constraint is not immutable but actively maintained through institutional work. This would shift the engine''s false summit detector into high confidence mode.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_sovereignty, conceptual, 'Whether sovereignty is immutable principle or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereignty_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_theater_t0, sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sov_theater_t100, sovereignty_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(sov_theater_t200, sovereignty_reading, theater_ratio, 200, 0.38).

% Extraction over time
narrative_ontology:measurement(sov_extract_t0, sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sov_extract_t100, sovereignty_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(sov_extract_t200, sovereignty_reading, base_extractiveness, 200, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(sovereignty_reading, 0.25).
narrative_ontology:affects_constraint(sovereignty_reading, freedom_of_movement_reading).
narrative_ontology:affects_constraint(sovereignty_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% SOVEREIGNTY-READING is one member of the BORDER-LEGITIMACY kernel family. Each reading instantiates the same contested kernel but produces different extractiveness values and victim sets. SOVEREIGNTY-READING (this file) emphasizes coordination function and treats exclusion as partially legitimate (ε=0.58, Tangled Rope). FREEDOM-OF-MOVEMENT-READING treats exclusion as pure extraction (ε≈0.78, Snare). HUMANITARIAN-OBLIGATION-READING treats exclusion as temporarily legitimate but ethically constrained (ε≈0.65, Scaffold with humanitarian sunset). All three stories share the same base structural commitment (borders exist and enforce exclusion) but interpret the legitimacy and function of that commitment differently. Authors and analysts must declare which reading they are adopting; no single reading can be treated as canonical without resolving the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereignty_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
