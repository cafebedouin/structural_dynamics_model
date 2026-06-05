% ============================================================================
% CONSTRAINT STORY: beijing_military_modernization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beijing_military_modernization, []).

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
 *   constraint_id: beijing_military_modernization
 *   human_readable: Beijing Military Modernization and Great Power Strategic Asymmetry
 *   domain: geopolitical/military/strategic
 *
 * SUMMARY:
 *   Beijing's military modernization over the past two decades represents a
 *   structural constraint that generates asymmetric costs and benefits across
 *   the Indo-Pacific strategic landscape. The constraint is fundamentally a
 *   coordination problem with embedded extraction: modernization requires
 *   integrated military-industrial action (coordination function), but it
 *   simultaneously narrows security options for smaller neighboring states
 *   and increases military expenditure across the region (extraction effect).
 *   The constraint exhibits all six classification types from different
 *   structural positions, illustrating how the same geopolitical phenomenon
 *   can be analyzed as pure extraction (from the perspective of island states
 *   facing asymmetric deterrence), mixed coordination-extraction (from the
 *   perspective of the US alliance system), pure coordination (from Beijing's
 *   institutional modernization apparatus), temporary institutional
 *   management (from multilateral organizations), degraded strategic
 *   competition logic (from Cold War deterrence paradigms), and naturalized
 *   power transition law (from civilizational analysis). The extractiveness
 *   value (0.58) reflects moderate-high structural extraction: Beijing
 *   captures strategic benefits and regional deference during the
 *   modernization window, neighboring states incur defense costs and
 *   strategic vulnerability, the US alliance system bears reassurance
 *   expenditure, and the international institutional order absorbs management
 *   costs. The suppression value (0.65) reflects significant barriers to
 *   autonomous security strategies for smaller states, though not absolute —
 *   some states maintain strategic ambiguity and develop coalitional
 *   responses. Theater ratio (0.48) indicates that strategic discourse
 *   (deterrence stability, peer competition, power transition management) has
 *   moderate performative content but genuine operational constraints drive
 *   actual military decisions.
 *
 * KEY AGENTS:
 *   - Beijing Strategic Apparatus (PRC Military-Industrial Complex): Primary beneficiary (institutional/arbitrage) — captures regional power projection, strategic status, and peer-competitor positioning
 *   - Smaller Regional States (Philippines, Vietnam, smaller ASEAN): Primary victims (powerless/trapped) — face asymmetric military capabilities, narrowed security options, forced strategic dependence on larger powers
 *   - United States Alliance System: Secondary beneficiary/constrained actor (powerful/constrained) — gains from allied presence and deterrent credibility but bears costs of reassurance, forward deployment, and alliance management
 *   - Democratic Defense Establishments (Taiwan, Japan, Australia): Secondary victims (institutional/constrained) — benefit from security coordination but locked into defense spending escalation and strategic vulnerability
 *   - ASEAN and Multilateral Institutions: Organized intermediary (organized/constrained) — attempt to manage modernization outcomes through institutional restraint but have limited enforcement capacity
 *   - Cold War Strategic Competition Paradigm: Institutional inertia actor (institutional/arbitrage) — persists as organizing logic despite degraded relevance, sustains escalatory framing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing power transition as immutable law, obscuring contingent policy choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beijing_military_modernization, 0.58).
domain_priors:suppression_score(beijing_military_modernization, 0.65).
domain_priors:theater_ratio(beijing_military_modernization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beijing_military_modernization, extractiveness, 0.58).
narrative_ontology:constraint_metric(beijing_military_modernization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(beijing_military_modernization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beijing_military_modernization, tangled_rope).
narrative_ontology:human_readable(beijing_military_modernization, "Beijing Military Modernization and Great Power Strategic Asymmetry").
narrative_ontology:topic_domain(beijing_military_modernization, "geopolitical/military/strategic").

domain_priors:requires_active_enforcement(beijing_military_modernization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beijing_military_modernization, prc_military_industrial_complex).
narrative_ontology:constraint_beneficiary(beijing_military_modernization, prc_regional_power_projection).
narrative_ontology:constraint_beneficiary(beijing_military_modernization, beijing_central_authority).
narrative_ontology:constraint_victim(beijing_military_modernization, neighboring_states_security).
narrative_ontology:constraint_victim(beijing_military_modernization, international_stability_commons).
narrative_ontology:constraint_victim(beijing_military_modernization, global_maritime_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL SECURITY DILEMMA (SNARE) — Smaller neighboring states (Philippines, Vietnam, Taiwan) face asymmetric military capabilities with no exit option. Cannot credibly deter through conventional force. Suppression is high: military modernization narrows the security window, forcing dependent allies to accept asymmetric arrangements (basing, military aid conditionality). No internal coordination function; pure extraction of strategic concessions.
constraint_indexing:constraint_classification(beijing_military_modernization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: US STRATEGIC ALLIANCE SYSTEM (TANGLED ROPE) — US experiences genuine coordination benefit (allied presence, logistics networks, intelligence sharing, interoperability standards) alongside extraction costs (military deployment requirements, forward base maintenance, alliance reassurance expenditure). Can exit bilaterally but constrained by system-wide implications — abandoning one ally destabilizes the entire East Asian order. Asymmetric: allies bear disproportionate vulnerability.
constraint_indexing:constraint_classification(beijing_military_modernization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: BEIJING STRATEGIC MODERNIZATION (ROPE) — From the institutional perspective driving modernization, the constraint is pure coordination: building integrated deterrent capabilities, projecting power into contested domains (South China Sea, Taiwan Strait), and establishing peer-competitor status requires coordinated military-industrial, diplomatic, and economic action. This perspective benefits directly from the modernization; experiences the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(beijing_military_modernization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL INSTITUTIONAL ORDER (SCAFFOLD) — Organized multilateral actors (ASEAN, UN mechanisms, international law frameworks) see the military modernization as a temporary disruption to which institutional restraint mechanisms apply: SCS Code of Conduct negotiations, freedom of navigation operations, dispute resolution under UNCLOS. These mechanisms have sunset clauses built in — they are designed to manage the modernization period and theoretically sunset into normalized peer-state interaction once capabilities stabilize. Theater is moderate — institutions perform reassurance functions that have real but limited constraint on action.
constraint_indexing:constraint_classification(beijing_military_modernization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR DETERRENCE PARADIGM (PITON) — The underlying strategic logic treating military modernization as deterrent escalation follows Cold War structure (mutual assured destruction, stability through strength) despite radically different context (integrated global economy, cyber interdependence, climate coupling). The paradigm persists through institutional inertia — military planning, doctrinal thinking, and force structure decisions are organized around it — but its functional relevance has degraded. Theater_ratio is high: nuclear deterrence rhetoric and strategic stability discourse continue without corresponding operational reality in an era of asymmetric threats, hybrid warfare, and economic coupling.
constraint_indexing:constraint_classification(beijing_military_modernization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC DEFENSE ESTABLISHMENT (TANGLED ROPE) — Taiwan, Japan, Australia experience genuine coordination benefit (collective defense posture, interoperability, information sharing) alongside extraction (force structure lock-in, defense spending escalation, strategic vulnerability to US pivot or treaty violation). Can exit at extreme cost only — coordination benefits and security dependency are fused. Asymmetric in that Beijing's military modernization drives the agenda while democratic states respond.
constraint_indexing:constraint_classification(beijing_military_modernization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / POWER TRANSITION VIEW (MOUNTAIN) — From civilizational/universal scope, power transitions between great powers are treated as immutable structural features of international politics: the rising power must modernize to achieve peer status, the declining power must manage decline, conflict arises from mutual strategic uncertainty. However, this perspective risks naturalizing contingent choices (modernization strategy, pace, risk tolerance) as inevitable laws. The engine's false summit detection applies — this is contingent institutional behavior framed as natural law.
constraint_indexing:constraint_classification(beijing_military_modernization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beijing_military_modernization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beijing_military_modernization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beijing_military_modernization, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beijing_military_modernization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(beijing_military_modernization, TR),
    TR >= 0.70.

:- end_tests(beijing_military_modernization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Beijing's military modernization captures genuine strategic benefits (regional power projection, deterrent capability, peer status) that constitute real extraction from smaller states facing asymmetric capabilities. The value reflects that extraction is not total coercion (smaller states retain some autonomy and can pursue coalitional responses) but is significant and structural. The trajectory from 0.32 to 0.58 over 20 years reflects accumulating capability asymmetry and narrowing security options for victims. Suppression (0.65): Moderate-high. Smaller states face genuine barriers to autonomous security strategies — military budget constraints, technological gaps, and geographic vulnerability create real suppression. However, suppression is not absolute — some states pursue strategic ambiguity (Vietnam), develop coalitional responses (Quad), or maintain non-alignment (Indonesia, Thailand). The value reflects constrained rather than trapped-level suppression. Theater ratio (0.48): Moderate. Strategic discourse around peer competition and power transition has performative elements (deterrence stability rhetoric, strategic competition framing, doctrine alignment), but actual military modernization decisions are driven by genuine force projection goals, not pure theater. The moderate ratio reflects that strategic language shapes perception and constrains policy options without being entirely disconnected from operational reality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is driven by structural position relative to the military asymmetry. Beijing's own institutional apparatus sees coordination — the genuine challenges of integrating military-industrial capacity, achieving peer-competitor status, and managing force deployment require coordinated action. Smaller regional states see extraction — they face narrowed security options and forced strategic dependence with minimal reciprocal benefit. The US alliance system occupies the middle: genuine coordination (collective deterrence) overlaid with asymmetric burden distribution (allies bear vulnerability, US bears reassurance costs). The democratic defense establishments (Taiwan, Japan, Australia) are most tightly bound — they experience genuine security coordination benefits fused with forced lock-in to defense escalation. The multilateral institutional perspective sees a temporary coordination problem with institutional solutions — the Scaffold classification. The Cold War deterrence paradigm classification as Piton reveals institutional inertia: the logic of nuclear deterrence and strategic stability through strength persists in doctrinal thinking despite radically different operational context (cyber coupling, economic interdependence, hybrid threats). The analytical observer at civilizational scope risks the most distortion: power transitions look like natural laws of international politics, but this naturalizes contingent choices (modernization pace, strategic intent, risk tolerance) that could be structured differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position in the extraction flow. Beijing's institutional apparatus (beneficiary + arbitrage) derives low d (~0.15) — they capture strategic benefits and have unilateral exit options (can slow modernization or redirect capabilities). Smaller regional states (victims + trapped) derive high d (~0.95) — they bear asymmetric military vulnerability and have no exit option (geographic and historical constraints are immovable). The US alliance system (mixed beneficiary/victim + constrained) derives moderate d (~0.50-0.55) — they benefit from allied presence but are constrained by alliance commitments and cannot exit without systemic implications. Democratic defense establishments (victims + constrained) derive high-moderate d (~0.70-0.75) — they bear defense costs and strategic vulnerability but retain exit options at extreme cost. Multilateral institutions (secondary actors + constrained) derive moderate d (~0.55) — they have some agency in designing restraint mechanisms but limited enforcement capacity. The Cold War paradigm (inertial institutional actor + arbitrage) derives moderate d (~0.45) — it sustains doctrinal consensus without direct extraction benefit, indicating institutional persistence rather than beneficiary position. These d values feed into the chi formula through f(d), with scope modifiers reflecting that modernization operates at both regional (smaller states, immediate effects) and global (US alliance, civilizational implications) scales.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination from asymmetric extraction at different structural levels. The Rope perspective (Beijing's own institutional apparatus) is legitimate — military modernization genuinely requires coordinated industrial action. The Snare perspective (smaller regional states) is legitimate — those states face pure extraction with minimal coordination benefit. The Tangled Rope perspectives (US alliance system, democratic defense establishments) are legitimate — real coordination (collective deterrence) overlays real extraction (asymmetric burden distribution and vulnerability). The Scaffold perspective (multilateral institutions) is legitimate — institutions genuinely coordinate restraint mechanisms with sunset logic (Code of Conduct, UNCLOS dispute resolution). The Piton perspective (Cold War paradigm) identifies inertial degradation — deterrence logic persists in doctrine without matching operational reality. The Mountain perspective (analytical civilizational view) is diagnosed as a false summit — power transitions appear immutable when actually driven by contingent choices. The mandatrophy resolution shows that these are not competing claims but distinct structural positions: each perspective correctly identifies the constraint from its vantage point, and the perspectival presheaf is the complete answer. The constraint simultaneously is coordination-for-some and extraction-for-others, depending on structural position in the force distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modernization_intent_ambiguity,
    'Is Beijing''s military modernization intended for regional hegemony, deterrent defense, or status-matching competition with the US?',
    'Content analysis of official doctrine; comparison of stated capabilities goals vs doctrinal requirements for each strategic posture; assessment of force structure decisions against capability thresholds for different objectives',
    'If hegemony-seeking: classification shifts toward Snare from more perspectives. If deterrent: Rope/Tangled Rope more dominant. If status-matching: Scaffold with longer sunset timeline. Strategic intent determines whether suppression is intentional coercion or structural byproduct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernization_intent_ambiguity, conceptual, 'Ambiguity between hegemonic, deterrent, and competitive intent driving modernization').

omega_variable(
    peer_competition_sustainability,
    'Can sustained peer military competition with the US be maintained without triggering mutual escalation or economic decoupling collapse?',
    'Econometric modeling of defense spending vs GDP growth; assessment of technological innovation cycles required for sustained peer competition; analysis of economic coupling effects (supply chain dependencies, debt dynamics, trade interdependencies)',
    'If sustainable: extraction persists indefinitely (Snare from victim perspectives). If unsustainable: the constraint has natural sunset as economic pressures force either cooperation or conflict. Determines whether Scaffold perspective is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_competition_sustainability, empirical, 'Whether sustained peer military competition is economically and technologically sustainable').

omega_variable(
    asymmetry_lock_in,
    'Does regional military asymmetry create permanent structural dependence that prevents smaller states from developing autonomous security strategies?',
    'Historical analysis of smaller power options in asymmetric security environments; assessment of potential alternative security arrangements (regional coalitions, strategic ambiguity, internal development); measurement of capability gaps required for autonomy',
    'If permanent lock-in: suppression is structural and high (0.65+). If temporary: modernization creates window for adjustment and smaller states can adapt. Determines whether victims classify as trapped or constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_lock_in, empirical, 'Whether military asymmetry creates permanent structural dependence').

omega_variable(
    institutional_order_resilience,
    'Do multilateral institutions (ASEAN+, UNCLOS, freedom of navigation) have sufficient enforcement capacity to constrain military modernization outcomes?',
    'Assessment of past institutional effectiveness in constraining power transitions; analysis of enforcement mechanisms vs potential enforcement costs; evaluation of institutional design for managing peer competition',
    'If resilient: Scaffold perspective validated, sunset mechanisms work. If fragile: institutions are theater (Piton), no actual constraint. Affects whether meaningful coordination exists beyond the military asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_order_resilience, empirical, 'Institutional capacity to constrain military modernization outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beijing_military_modernization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beij_tr_t0, beijing_military_modernization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(beij_tr_t10, beijing_military_modernization, theater_ratio, 10, 0.45).
narrative_ontology:measurement(beij_tr_t20, beijing_military_modernization, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(beij_be_t0, beijing_military_modernization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(beij_be_t10, beijing_military_modernization, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(beij_be_t20, beijing_military_modernization, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beijing_military_modernization, enforcement_mechanism).
narrative_ontology:affects_constraint(beijing_military_modernization, taiwan_strait_security_dilemma).
narrative_ontology:affects_constraint(beijing_military_modernization, south_china_sea_territorial_disputes).
narrative_ontology:affects_constraint(beijing_military_modernization, us_china_strategic_competition).
narrative_ontology:affects_constraint(beijing_military_modernization, asean_cohesion_maintenance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beijing_military_modernization, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
