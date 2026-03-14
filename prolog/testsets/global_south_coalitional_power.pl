% ============================================================================
% CONSTRAINT STORY: global_south_coalitional_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_south_coalitional_power, []).

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
 *   constraint_id: global_south_coalitional_power
 *   human_readable: Global South Coalitional Power in International Institutions
 *   domain: geopolitical/institutional
 *
 * SUMMARY:
 *   The Global South coalition in international institutions represents a
 *   paradoxical structural arrangement: it generates real coordination
 *   benefits while simultaneously enabling systematic extraction of smaller
 *   and weaker members by regional powers and institutional apparatus
 *   bureaucracies. Individual southern states face a coordination problem —
 *   alone, they are marginalized in global negotiations; collectively, they
 *   amplify voice. But the coalition mechanism creates new extractive layers:
 *   larger southern states capture coalition positions to extract concessions
 *   from smaller members; the institutional apparatus captures development
 *   resources meant for member benefit; members sacrifice agenda autonomy for
 *   the promise of amplified voice that remains subordinate to northern
 *   preferences. The constraint has degraded significantly over 30 years:
 *   extractiveness has risen from 0.35 (1995, peak NAM/G77 functional
 *   capacity) to 0.61 (2025, ossified institutional apparatus), while theater
 *   has risen from 0.30 to 0.68, indicating that the apparatus now operates
 *   largely through performative ritual. The structure itself remains tangled
 *   rope: genuine coordination function persists (coalition members do
 *   extract real benefits in specific domains like climate and trade), but
 *   extraction has grown as institutional inertia and regional power
 *   asymmetries have hardened.
 *
 * KEY AGENTS:
 *   - Individual Southern States (powerless/trapped) — small and medium-sized Global South nations; benefit from coalition voice but sacrifice agenda autonomy; trapped by lack of unilateral leverage
 *   - Regional Power Brokers (organized/constrained) — India, Brazil, Nigeria, Indonesia; experience genuine tangled rope: gain coalition voice amplification but lose bilateral arbitrage capacity, constrained by peer pressure to maintain unity
 *   - Global South Institutional Apparatus (institutional/arbitrage) — G77 Secretariat, ALBA technical committees, AU structures; maintain permanent funding, staffing, legitimacy through coalition reproduction; arbitrage options via parallel northern funding streams
 *   - Transiting Middle Powers (powerful/mobile) — Vietnam, Morocco, Kenya, Bangladesh; use coalition as temporary scaffolding for capacity building; mobile enough to shift alignment as development trajectory changes
 *   - Post-Colonial Institutional Framework (institutional/arbitrage) — legacy institutional structures carrying forward colonial-era division of labor; maintain ceremonial functions through inertia; high theater, minimal functional extraction capacity
 *   - Analytical Observer (analytical/analytical) — global systemic view revealing entanglement of coordination and extraction across all timeframes and power levels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_south_coalitional_power, 0.58).
domain_priors:suppression_score(global_south_coalitional_power, 0.68).
domain_priors:theater_ratio(global_south_coalitional_power, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_south_coalitional_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_south_coalitional_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(global_south_coalitional_power, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_south_coalitional_power, tangled_rope).
narrative_ontology:human_readable(global_south_coalitional_power, "Global South Coalitional Power in International Institutions").
narrative_ontology:topic_domain(global_south_coalitional_power, "geopolitical/institutional").

domain_priors:requires_active_enforcement(global_south_coalitional_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_south_coalitional_power, global_south_coalition_members).
narrative_ontology:constraint_beneficiary(global_south_coalitional_power, southern_coordination_capacity).
narrative_ontology:constraint_victim(global_south_coalitional_power, individual_southern_states).
narrative_ontology:constraint_victim(global_south_coalitional_power, southern_agenda_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED SOUTHERN STATE (SNARE) — A small or medium southern state faces impossible choice: join the coalition and cede agenda autonomy to maintain seat at the table, or exit and lose voice entirely. Exit from coalitional structure means accepting marginalization in global negotiations. Material barriers to unilateral effectiveness (lack of capital, military, technological leverage) trap the agent within the coalition framework despite experiencing extraction through agenda subordination.
constraint_indexing:constraint_classification(global_south_coalitional_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWER BROKER (TANGLED ROPE) — Larger regional powers (India, Indonesia, Nigeria, Brazil) experience genuine coordination benefits from the coalition: unified voting blocks increase negotiating leverage, shared position development amplifies voice. But they also bear extraction: subordination of regional interests to pan-southern alignment, loss of bilateral arbitrage capacity, constraint on defection to northern coalitions for side-deals. High structural complexity — coordination and extraction are genuinely entangled.
constraint_indexing:constraint_classification(global_south_coalitional_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: GLOBAL SOUTH INSTITUTIONAL APPARATUS (ROPE) — The coalition apparatus itself (G77+China, ALBA, African Union, ASEAN) benefits from member loyalty: it reproduces its own legitimacy, funding, staffing. The apparatus is a genuine coordination mechanism — it enables south-south dialogue, capacity sharing, collective position development. From the apparatus perspective, the constraint is pure coordination with minimal extraction. The apparatus has arbitrage options: it can access northern funding streams, secure permanent institutional seats, develop parallel technical committees.
constraint_indexing:constraint_classification(global_south_coalitional_power, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSITING MIDDLE POWER (SCAFFOLD) — Emerging powers (Vietnam, Morocco, Kenya) use the southern coalition as a temporary platform for capacity building and agenda access. The coalition has structural sunset: as these states develop economic/military capacity, the coordination necessity declines and extraction becomes visible. They see the constraint as temporary scaffolding — useful now for manufacturing access and development financing, but obsolete as autonomous leverage grows. Theater remains moderate because the coordination function is real, even if temporary.
constraint_indexing:constraint_classification(global_south_coalitional_power, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-COLONIAL INSTITUTIONAL FRAMEWORK (PITON) — From the longest timeframe, the southern coalition apparatus shows high theater and degraded function. Once-vibrant structures (NAM — Non-Aligned Movement — had real coordination capacity; G77 coordinated UNCTAD victories) have ossified into performative ritual. Member states attend conferences, deliver speeches affirming solidarity, but extract little material benefit. The institutions persist through inertia: they provide conference venues, funding flows to bureaucrats, legitimacy to state delegations. But their ability to bend global rules or defend member interests has atrophied. Theater ratio high; actual effective extraction from members minimal because the apparatus no longer extracts anything of value.
constraint_indexing:constraint_classification(global_south_coalitional_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From maximum distance and time, the constraint exhibits genuine tangled rope structure: the coalition creates real coordination benefits (unified purchasing power, shared research access, collective advocacy) AND systematic extraction (smaller states sacrifice autonomy for voice that remains subordinate in global rules; leadership states capture coalition positions to extract concessions from members; institutional apparatus captures resources meant for development). Neither function eliminates the other — they are structurally entangled. The suppression is real (exit costs are high; material alternatives limited) and enforcement is active (coalition peer pressure, ostracism threats, institutional dependency). The base extractiveness of 0.58 reflects the mixed function.
constraint_indexing:constraint_classification(global_south_coalitional_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_south_coalitional_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_south_coalitional_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_south_coalitional_power, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_south_coalitional_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_south_coalitional_power, TR),
    TR >= 0.70.

:- end_tests(global_south_coalitional_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint has asymmetric extraction flowing from smaller members toward the apparatus and regional powers. The 0.58 value reflects that the extraction is neither total (members do gain negotiating voice) nor minimal (apparatus overhead and regional exploitation are substantial). The upward trajectory over 30 years (0.35→0.61) indicates institutional degradation — as the coalition's functional capacity declined, extraction mechanisms became more visible and less justified by coordination benefits. Suppression (0.68): High. Members face substantial barriers to exit: unilateral action produces negligible outcomes; alternative coalitions (regional or ideological) are incomplete substitutes; defection triggers peer pressure and reduced institutional support for domestic programs. The suppression is both structural (material leverage imbalance) and institutional (coalition peer discipline). Theater (0.55): Moderate-high and rising. The coalition produces real coordination in specific domains (climate negotiations, WTO agricultural advocacy) but operates largely through performative ritual in others (sovereignty declarations, solidarity conferences). The rising theater trajectory (0.30→0.68) indicates shift toward ceremonial function as the apparatus has decoupled from effective policy influence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence: the isolated state sees snare (pure extraction with trapped exit); the regional power sees tangled rope (mixed coordination and extraction with constrained exit); the apparatus sees rope (pure coordination with arbitrage options); the transitioning power sees scaffold (temporary extraction with mobile exit); the civilizational view sees piton (degraded institution with ossified theater); the analytical observer resolves the gap by recognizing that all six types are genuine readings of different structural positions within the same constraint. The gap reveals that 'the Global South coalition' is not a monolithic actor but a presheaf of differential extraction relationships. Any narrative that treats the coalition as uniformly beneficial (perspective 3 — apparatus view) or uniformly harmful (perspective 1 — isolated state view) naturalizes the perspective of a specific structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary dramatically across perspectives due to structural position within the coalition hierarchy. Isolated southern states occupy d ≈ 0.92 (nearly pure targets): they bear extraction costs with minimal exit options and minimal decision-making power within coalition. Regional power brokers occupy d ≈ 0.55 (symmetric): they both benefit (amplified voice, coordination capacity, institutional seat) and bear costs (agenda subordination, constrained bilateral options, apparatus maintenance burden). The apparatus itself occupies d ≈ 0.10 (strong beneficiary): staffing, funding, legitimacy flow toward the apparatus; it has maximal exit options and minimal extraction pressure. This directionality gradient is the mechanism driving the tangled rope classification: without the asymmetry, all agents would experience the same constraint type; the asymmetry is precisely what creates both coordination function AND extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint resolves mandatrophy by decomposing the coalition into its constituent structural relationships. There is no single 'correct' classification — the apparatus genuinely experiences coordination (rope), the isolated state genuinely experiences extraction (snare), and the regional power genuinely experiences both (tangled rope). The analytics do not lie: each perspective's classification is locally accurate given that agent's structural position. The mandatrophy appears as a question only when treating 'the Global South coalition' as a black box. Disaggregate it: the institutional apparatus is rope; the member states' relationships to the apparatus is tangled rope for large states and snare for small states; the apparatus's post-colonial legacy component is piton. The constraint family has true structure, not ambiguity. The analytical frame's task is to prevent false universalization — recognizing that the coalition is extractive from the powerless perspective is not a bug in the framework; it is a diagnostic signal that the coordination narrative (perspective 3) is naturalizing the structural position of institutional beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_voice_fungibility,
    'Does individual voice within a coalition framework represent more or less effective advocacy than exit and unilateral positioning?',
    'Comparative policy outcome analysis: track WTO negotiations, climate agreements, trade deals with coalition vs non-coalition member positions; measure policy change attributable to coalition advocacy vs unilateral action',
    'If coalition voice produces better outcomes: tangled rope classification confirmed — extraction is cost of coordination benefit. If unilateral positions outperform: extraction dominates, should reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_voice_fungibility, empirical, 'Whether coalition voice produces more effective advocacy than unilateral alternatives').

omega_variable(
    extraction_visibility_asymmetry,
    'Do powerful regional states within the coalition consciously exploit smaller members through position-setting, or is the asymmetry an emergent artifact of capacity differences?',
    'Documentary analysis of coalition position-setting processes; interviews with state delegates on perceived influence; comparison of stated positions vs voting patterns across countries by power rank',
    'If conscious exploitation: snare component is intentional, reclassify toward higher extractiveness. If emergent asymmetry: tangled rope confirmed, extraction is byproduct of unequal capacities, not design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_visibility_asymmetry, empirical, 'Whether extraction within coalition is intentional or emergent').

omega_variable(
    institutional_apparatus_rent_capture,
    'What proportion of coalition institutional funding actually reaches member state development vs flows to bureaucratic overhead and apparatus maintenance?',
    'Financial audit of G77 Secretariat, ALBA development fund, AU technical bodies; track funding flows from member contributions through apparatus to actual development projects',
    'If overhead > 30%: apparatus is primarily extractive rent-seeking (high piton signature). If overhead < 10%: apparatus functions as legitimate coordination infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_apparatus_rent_capture, empirical, 'Rent capture within coalition institutional apparatus').

omega_variable(
    coalitional_constraint_vs_capacity_constraint,
    'How much of southern state subordination derives from coalition discipline vs from genuine capacity limitations in implementing independent strategies?',
    'Counterfactual analysis: model what unilateral southern actions could achieve with current state capacities; compare actual coalition positions to unilateral capacity ceiling; examine rare unilateral defections for outcome differences',
    'If capacity is primary constraint: coalition is rescue mechanism (high rope signal). If coalition actively blocks better strategies: extraction dominates (high snare signal).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalitional_constraint_vs_capacity_constraint, conceptual, 'Attribution of subordination to coalition discipline vs capacity limitations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_south_coalitional_power, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gs_coal_tr_t0, global_south_coalitional_power, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gs_coal_tr_t10, global_south_coalitional_power, theater_ratio, 10, 0.42).
narrative_ontology:measurement(gs_coal_tr_t20, global_south_coalitional_power, theater_ratio, 20, 0.55).
narrative_ontology:measurement(gs_coal_tr_t30, global_south_coalitional_power, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(gs_coal_be_t0, global_south_coalitional_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gs_coal_be_t10, global_south_coalitional_power, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gs_coal_be_t20, global_south_coalitional_power, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gs_coal_be_t30, global_south_coalitional_power, base_extractiveness, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_south_coalitional_power, enforcement_mechanism).
narrative_ontology:affects_constraint(global_south_coalitional_power, global_north_institutional_dominance).
narrative_ontology:affects_constraint(global_south_coalitional_power, southern_state_development_sovereignty).
narrative_ontology:affects_constraint(global_south_coalitional_power, multilateral_institution_reform).

% DUAL FORMULATION NOTE:
% The Global South coalition can be decomposed into two structurally distinct constraints: (1) coalition_coordination_mechanism (ε≈0.25, Rope) — the genuine problem-solving function of unified southern voice in global forums; (2) apparatus_extraction_rent (ε≈0.72, Snare) — the institutional apparatus's capture of member contributions and transformation of development resources into bureaucratic overhead. The integrated story (ε=0.58, Tangled Rope) represents their entanglement. Networks track how improvements in coordination reduce apparatus power (north-to-south dominance decreases) and how apparatus degradation (increasing piton signal) threatens the coordination function that justified the apparatus's existence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_south_coalitional_power, organized, 0.52).
constraint_indexing:directionality_override(global_south_coalitional_power, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
