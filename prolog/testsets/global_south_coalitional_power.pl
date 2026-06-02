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
 *   The global south coalition in international institutions embodies a
 *   structural paradox: it solves a genuine coordination problem for
 *   marginalized states while simultaneously creating a mechanism for
 *   systematic extraction by regional powers and institutional bureaucracies.
 *   Individual southern states face a prisoner's dilemma — alone, they are
 *   systematically sidelined in UN bodies, trade negotiations, and climate
 *   forums; together, they amplify collective voice and negotiating leverage.
 *   Yet the coalition itself has become a machine for extracting labor,
 *   resources, and policy concessions from smaller members to benefit
 *   regional hegemons and administrative apparatus. The constraint has
 *   evolved over 30 years: it emerged as a genuine coordination response to
 *   northern institutional dominance (low extractiveness, ~0.28), but has
 *   accumulated bureaucratic complexity, regional power concentration, and
 *   suppressive discipline mechanisms (high theater, high suppression by year
 *   30). The measurement trajectory shows classic tangled_rope degradation:
 *   real coordination function persists (true at t=0, still largely true at
 *   t=30), but the machinery that delivers it increasingly extracts from its
 *   own members.
 *
 * KEY AGENTS:
 *   - Smaller southern states (powerless/trapped): Bear full extraction cost while receiving minimal protective benefit. Exit is prohibitive due to isolation risk. Primary victims.
 *   - Medium-power southern states (moderate/constrained): Brazil, Nigeria, Indonesia, Mexico. Provide much of the negotiation labor and coalition maintenance effort. Benefit from amplified voice but constrained by internal coalition discipline. Asymmetric burden distribution.
 *   - Regional hegemons (institutional/arbitrage): India, South Africa, Egypt. Benefit from leadership position and disproportionate voice amplification. Can arbitrage to alternative forums. Primary beneficiaries.
 *   - Coalition bureaucratic apparatus (institutional/constrained): UN Group of 77 secretariat, rotating coalition chairs, technical working groups. Benefit from institutional legitimacy and resource allocation. Extract through mandate expansion and gatekeeping.
 *   - Northern institutional system (institutional/arbitrage): UN, World Bank, WTO. Officially recognize coalitions but fragmentarily engage (selective acceptance of coalition positions based on alignment with northern interest). Increasingly replace coalition authority with bilateral arrangements.
 *   - Analytical observer: Sees full structural complexity — genuine coordination function alongside systematic internal extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_south_coalitional_power, 0.55).
domain_priors:suppression_score(global_south_coalitional_power, 0.62).
domain_priors:theater_ratio(global_south_coalitional_power, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_south_coalitional_power, extractiveness, 0.55).
narrative_ontology:constraint_metric(global_south_coalitional_power, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_south_coalitional_power, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_south_coalitional_power, tangled_rope).
narrative_ontology:human_readable(global_south_coalitional_power, "Global South Coalitional Power in International Institutions").
narrative_ontology:topic_domain(global_south_coalitional_power, "geopolitical/institutional").

domain_priors:requires_active_enforcement(global_south_coalitional_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_south_coalitional_power, regional_power_states).
narrative_ontology:constraint_beneficiary(global_south_coalitional_power, institutional_bureaucracy).
narrative_ontology:constraint_beneficiary(global_south_coalitional_power, coalition_administrative_apparatus).
narrative_ontology:constraint_victim(global_south_coalitional_power, smaller_southern_states).
narrative_ontology:constraint_victim(global_south_coalitional_power, weak_peripheral_members).
narrative_ontology:constraint_victim(global_south_coalitional_power, collective_southern_interest).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAK PERIPHERAL MEMBER (SNARE) — A small southern state faces crushing isolation if it exits the coalition. Alone, it is systematically marginalized in global institutions; within the coalition, it subsidizes regional power dominance while receiving minimal protective benefit. Exit costs are prohibitive: loss of bloc voting leverage, exclusion from coalition diplomatic channels, vulnerability to bilateral pressure from stronger states and northern powers. High suppression, high extraction, minimal coordination benefit flowing to this agent.
constraint_indexing:constraint_classification(global_south_coalitional_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MEDIUM-POWER SOUTHERN STATE (TANGLED ROPE) — Medium powers (Brazil, Nigeria, Indonesia, etc.) experience genuine coordination benefit: coalition bloc voting amplifies their voice in UN bodies, climate negotiations, and trade forums. Simultaneously, they bear disproportionate costs: providing resources and diplomatic labor to maintain coalition cohesion, suppressing national interest conflicts to preserve alignment, and absorbing negotiating labor that benefits all members equally. Constrained by coalition discipline and reputation costs of defection, but with sufficient capacity to negotiate internally. Mixed genuine coordination and asymmetric extraction.
constraint_indexing:constraint_classification(global_south_coalitional_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL HEGEMON (ROPE) — India, South Africa, Mexico, and similar states experience the coalition as pure coordination with net benefit flow toward them. They extract disproportionate voice and leadership position (caucus chair, negotiation spokesperson) while providing genuine coordination infrastructure (convening capacity, diplomatic networks, resource leverage). Exit costs are minimal — they can arbitrage to ad-hoc coalitions, bilateral leverage, or northern forums if needed. The constraint appears to them as legitimate coordination mechanism.
constraint_indexing:constraint_classification(global_south_coalitional_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COALITION BUREAUCRATIC APPARATUS (TANGLED ROPE) — Permanent secretariats, rotating chairs, technical working groups, and coordination offices benefit from the coalition's existence (budget, staff positions, institutional legitimacy). They coordinate genuine information sharing and negotiating strategy alignment among members. Simultaneously, they extract through mandate expansion, procedural complexity that increases member dependency, and gatekeeping of coalition-wide communications. Constrained by coalition member autonomy but with significant institutional inertia. Genuine function alongside embedded extraction.
constraint_indexing:constraint_classification(global_south_coalitional_power, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COORDINATION FRAME (ROPE) — From a structural perspective, the coalition solves a genuine collective action problem: individually fragmented southern states gain measurable amplification in global institutions. Empirical evidence shows coalition bloc voting produces negotiation outcomes (climate agreements, trade positions, UN resolutions) that isolated states could not achieve. The constraint appears as coordination mechanism with low suppression and low base extractiveness when viewed at the system level. This perspective risks naturalizing the internal extraction as necessary coordination cost.
constraint_indexing:constraint_classification(global_south_coalitional_power, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NORTHERN INSTITUTIONAL OBSERVER (PITON) — Northern states and multilateral institutions officially recognize and facilitate southern coalitions (UNGA procedures, UN Group of 77, BRICS, AU coordination) as legitimate representation mechanisms. Yet the institutional support is increasingly performative — northern states selectively engage with coalition positions (accepting them when alignment serves northern interests, ignoring them when inconvenient) and have invested in fragmenting coalition coherence through bilateral agreements and competitive funding. The constraint persists through inertia: coalitional representation is institutionalized in UN procedures and norm, but its actual power to alter outcomes has eroded.
constraint_indexing:constraint_classification(global_south_coalitional_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective focusing on structural asymmetries in global power, the coalition constraint might appear as an immutable natural law: weaker states will always be extracted by stronger states and institutional hierarchies because power asymmetry is fundamental. Coalitions are merely the form this extraction takes. However, this perspective risks naturalizing what is actually a contingent institutional arrangement subject to decomposition and redesign. The structural data reveals this as a false summit: the coalition's extractiveness is contingent on specific institutional design choices, not an immutable law.
constraint_indexing:constraint_classification(global_south_coalitional_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_south_coalitional_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_south_coalitional_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_south_coalitional_power, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Base extractiveness (0.55): Moderate-high at measurement end. Started at 0.28 (genuine coordination, minimal extraction) but accumulated to 0.55 by year 30 as bureaucratic complexity, regional power concentration, and suppressive voting discipline developed. The trajectory reflects how coordination mechanisms degrade toward extraction over institutional lifespan. Suppression (0.62): High. Multiple suppression mechanisms operate: coalition voting discipline (members constrain national interest voting to maintain bloc alignment), exit barriers (isolation without coalition protection), reputation costs (defection is costly and visible), and institutional gatekeeping (coalition leadership controls access to coalition resources and forums). These are structural barriers rather than coercive force, but collectively produce high suppression. Theater ratio (0.58): Moderate-high. Coalition procedures and meetings (summits, ministerial committees, working groups) perform representational function but deliver diminishing material outcomes. Many coalition activities are procedural coordination without corresponding negotiation leverage — southern states show up as bloc, vote together, but northern states and multilateral institutions increasingly bypass coalition positions when inconvenient. Theater has increased over interval as coordination effectiveness has declined relative to bureaucratic complexity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across institutional positions. A regional hegemon (India, Brazil) experiences the coalition as pure coordination mechanism (Rope) — they obtain amplified voice and leadership platform with minimal cost. A weak peripheral state (Lesotho, Mauritius, small island state) experiences it as a snare — trapped by isolation risk, extracting value for the benefit of larger states. A medium power (Nigeria, Indonesia) experiences genuine mixed coordination-extraction (Tangled Rope) — they benefit from amplified voice but bear disproportionate coordination burden. The coalition bureaucracy experiences it as beneficial institutional apparatus (Tangled Rope for them, though Piton or Snare for members). The northern institutional system (UN, World Bank) views it as increasingly performative (Piton) — officially recognized, procedurally accommodated, but strategically bypassed. The analytical observer avoiding false naturalization sees the constraint as contingent institutional arrangement subject to redesign, not an immutable hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural position relative to extraction flow. Regional hegemons: beneficiary status + arbitrage exit options → low d (d ≈ 0.15) → negative f(d) → they experience extraction reducing their utility (the constraint subsidizes them). Smaller states: victim status + trapped exit → high d (d ≈ 0.95) → high f(d) ≈ 1.42 → they experience maximum effective extraction. Medium powers: both beneficiary (amplified voice) and victim (coordination burden) status + constrained exit → mixed d (d ≈ 0.55) → moderate f(d) ≈ 0.75 → they experience moderate extraction. Coalition bureaucracy: beneficiary (institutional legitimacy, budget, staff) + constrained exit → low d → they experience extraction reducing their burden. The derived directionality flow is consistent with the constraint classification: regional hegemons perceive rope (low extraction), smaller states perceive snare (high extraction), medium powers perceive tangled_rope (mixed), analytical observer sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival decomposition. The constraint exhibits no single-type classification — it genuinely is different things to different structural positions. A false resolution would attempt to force one type (e.g., 'it's really a Tangled Rope at the analytical level'). The true resolution is that Tangled Rope is the system-level classification, but the system contains embedded Rope (for hegemons), Snare (for smaller states), and Piton (for bureaucratic apparatus) perspectives. The mandatrophy manifests as the conflicting experiences reported by coalition members: hegemons claim 'this is successful coordination,' while smaller states claim 'this extracts more than it protects.' Both are structurally correct from their respective positions. The constraint is well-classified as Tangled Rope at the analytical level because the entire system combines genuine coordination function (states do achieve collective leverage they couldn't achieve individually) with systematic asymmetric extraction (extraction flows disproportionately toward hegemons and bureaucratic apparatus, away from smaller members). No higher-level natural law or coordinate universality resolves this — it is a contingent institutional design with embedded extraction dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_bloc_effectiveness,
    'Does global south coalitional voting leverage actually produce material concessions in multilateral negotiations, or is it primarily symbolic representation?',
    'Comparative analysis of outcomes: UN resolutions on contested issues where south bloc voted together vs. outcomes when bloc fractured; measurement of voting alignment vs. policy concessions; control for temporal effects and underlying power shifts',
    'If effective: coalition generates genuine coordination benefit justifying tangled_rope classification. If symbolic only: coordination function is theater, and classification shifts toward snare for all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_bloc_effectiveness, empirical, 'Whether coalition bloc voting produces material concessions').

omega_variable(
    extraction_magnitude_by_state_size,
    'How does member state size, population, and institutional capacity correlate with extraction burden within coalitions? Do medium powers systematically extract from smaller states, or does burden distribute equally across coalition membership?',
    'Analysis of coalition voting discipline burden (negotiation labor, cost of suppressing national interest conflicts); correlation with state capacity metrics; measurement of negotiation benefits allocated by coalition leadership to different member tiers',
    'If extraction scales with state size: confirms snare classification for smaller members and tangled_rope for medium powers. If burden distributes equally: coalition approaches pure rope and smaller members benefit proportionally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_by_state_size, empirical, 'Correlation between state size and extraction burden in coalitions').

omega_variable(
    institutional_substitution_pressure,
    'Are northern-backed bilateral and sub-regional arrangements systematically fragmenting the global south coalition, or are such arrangements complementary to broader coalition membership?',
    'Network analysis of southern state participation in competing forums (bilateral trade agreements, fragmented regional coalitions, northern-led initiatives); measurement of coalition voting discipline erosion over time; tracking of coalition member defections to alternative arrangements',
    'If fragmenting: coalition suppression may be eroding as institutional alternatives reduce exit costs. If complementary: coalition remains high-suppression constraint with stable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_substitution_pressure, empirical, 'Whether northern arrangements fragment or complement global south coalitions').

omega_variable(
    identity_locked_coalition_commitment,
    'Do smaller coalition members remain committed to coalition participation primarily due to material calculation (exit costs) or due to identity fusion (self-concept as ''Global South'' member, institutional identity tied to coalition membership)?',
    'Qualitative analysis of state messaging and diplomatic framing; interviews with coalition negotiators; measurement of coalition commitment during periods when bilateral alternatives appeared more advantageous; analysis of post-exit trajectories when states do leave coalitions',
    'If identity-locked: exit costs include psychological and identity dissolution costs beyond material barriers, increasing effective suppression. If material-cost only: suppression can be reduced by lowering exit barriers through institutional innovation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_coalition_commitment, conceptual, 'Whether coalition commitment is identity-fused or material-cost driven').

omega_variable(
    bureaucratic_mandate_expansion,
    'Do coalition secretariats and administrative bodies systematically expand their mandates beyond member-directed coordination, creating institutional extraction independent of member agency?',
    'Comparative analysis of secretariat budget growth, staffing expansion, and mandate scope across coalition lifecycle; member satisfaction surveys regarding whether bureaucratic functions serve or constrain coalition interests; measurement of decision-lag time (how long coordination decisions take as bureaucratic complexity increases)',
    'If significant mandate expansion: bureaucratic extraction is real and measurable, supporting piton classification view of institutional apparatus. If mandates remain controlled: bureaucratic apparatus serves pure coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bureaucratic_mandate_expansion, empirical, 'Whether coalition bureaucracies expand mandates beyond member direction').

omega_variable(
    extraction_vs_amplification_boundary,
    'At what point does the coalition constraint transition from ''extraction with coordination benefit'' (tangled_rope) to ''extraction theater with minimal amplification'' (piton or snare)? What measurable threshold distinguishes legitimate internal organization from dysfunctional extraction?',
    'Longitudinal measurement of voice amplification per member (voting power before and after coalition membership) against extraction cost per member (negotiation labor, constrained voting freedom); identification of the inflection point where marginal cost exceeds marginal amplification benefit; comparison with non-coalition southern states'' institutional access',
    'If threshold is sharp and recent: coalition may be transitioning from tangled_rope toward piton. If threshold is stable and in past: coalition is stabilized in tangled_rope form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_amplification_boundary, empirical, 'Threshold distinguishing coordination from extraction in coalition dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_south_coalitional_power, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gscp_tr_t0, global_south_coalitional_power, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gscp_tr_t15, global_south_coalitional_power, theater_ratio, 15, 0.48).
narrative_ontology:measurement(gscp_tr_t30, global_south_coalitional_power, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(gscp_be_t0, global_south_coalitional_power, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gscp_be_t15, global_south_coalitional_power, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(gscp_be_t30, global_south_coalitional_power, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gscp_su_t0, global_south_coalitional_power, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gscp_su_t15, global_south_coalitional_power, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(gscp_su_t30, global_south_coalitional_power, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_south_coalitional_power, resource_allocation).
narrative_ontology:affects_constraint(global_south_coalitional_power, un_voting_procedures_structural_bias).
narrative_ontology:affects_constraint(global_south_coalitional_power, multilateral_development_bank_governance).
narrative_ontology:affects_constraint(global_south_coalitional_power, climate_negotiation_coalitional_dynamics).
narrative_ontology:affects_constraint(global_south_coalitional_power, trade_coalition_formation_incentives).

% DUAL FORMULATION NOTE:
% The global south coalitional power constraint is upstream of multiple domain-specific coalition stories (climate, trade, development finance). This constraint describes the general institutional form; downstream constraints instantiate domain-specific manifestations with their own extractiveness values. The network link captures how degradation of general coalitional power structure creates pressure on domain-specific coalitions to fragment or intensify extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_south_coalitional_power, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
