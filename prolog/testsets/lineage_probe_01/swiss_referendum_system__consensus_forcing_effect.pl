% ============================================================================
% CONSTRAINT STORY: swiss_referendum_system__consensus_forcing_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_swiss_referendum_consensus_forcing, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: swiss_referendum_system__consensus_forcing_effect
 *   human_readable: Swiss Referendum System: Consensus Forcing via Veto Threat
 *   domain: political/comparative/direct_democracy
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'swiss_referendum_system': the CONSENSUS_FORCING_EFFECT reading. The
 *   Swiss political system exhibits a structural paradox: direct-democracy
 *   mechanisms (referendum and initiative) designed to protect minorities and
 *   popular sovereignty have produced an effect opposite to majoritarian
 *   competition — they have forced permanent consensus governance. Any
 *   political force that achieves sufficient size (historically ~5% voter
 *   support, 100,000 signatures for initiative) can threaten to sink
 *   legislation via referendum, creating incentive for established parties to
 *   bring them inside coalition negotiations rather than risk repeated veto
 *   campaigns. This reading argues that the referendum threat mechanism is
 *   the primary driver of Swiss consensus norms: without the veto threat, the
 *   system would converge toward majoritarian alternation as in Westminster
 *   democracies. The 'magic formula' (proportional coalition distribution) is
 *   not a discovered natural law but a contingent institutional response to
 *   referendum veto capacity. This reading COEXISTS with two sibling
 *   readings: (1) MINORITY_RIGHTS_TENSION — which argues that
 *   direct-democracy mechanisms can vote restrictions INTO the constitution
 *   (minaret ban) and thus collide with minority rights protection; (2)
 *   POPULAR_INITIATIVE_ENGINE — which argues the initiative is an
 *   agenda-setting wildcard held by citizen movements, independent of party
 *   consensus. This reading concentrates on the suppression of majoritarian
 *   alternation as the defining extractive effect; the siblings emphasize
 *   orthogonal dimensions (rights collision, agenda power).
 *
 * KEY AGENTS:
 *   - Consensus Coalition Core (SVP, CVP, SPS, FDP until 2015): Institutional beneficiaries of the veto-suppression system — permanent inclusion regardless of electoral performance, guaranteed policy influence through coalition membership.
 *   - Ascending Minority Parties (SVP 1970s-80s, Greens 1990s, SVP escalation 2000s): Moderate-power victim-beneficiaries — face extraction costs (policy compromise, co-optation) to gain inclusion but benefit from guaranteed governance participation rather than alternating opposition exclusion.
 *   - Excluded Political Forces (radical leftists, libertarians, ethnic minority movements): Powerless agents facing the worst position — significant voter support but forced to choose between subordinate coalition role or repeated veto campaigns with high legitimacy costs.
 *   - Voter Movements and Initiative Campaigns: Organized agents with scaffold-like structural role — can mobilize outside coalition structures to change agenda, but faces co-optation as movement grows into party that must negotiate coalition terms.
 *   - Federal Council (Seven-member executive): Institutional performer of consensus ritual — presents coalition arithmetic as natural law while serving as legitimating symbol for the underlying veto-threat mechanism.
 *   - Alternating Opposition (Historical Westminster-model imaginary): Victim set: the system structurally suppresses the possibility of clean governing majorities and opposition rotations that other democracies take for granted.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(swiss_referendum_system__consensus_forcing_effect, 0.38).
domain_priors:suppression_score(swiss_referendum_system__consensus_forcing_effect, 0.42).
domain_priors:theater_ratio(swiss_referendum_system__consensus_forcing_effect, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(swiss_referendum_system__consensus_forcing_effect, extractiveness, 0.38).
narrative_ontology:constraint_metric(swiss_referendum_system__consensus_forcing_effect, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(swiss_referendum_system__consensus_forcing_effect, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(swiss_referendum_system__consensus_forcing_effect, tangled_rope).
narrative_ontology:human_readable(swiss_referendum_system__consensus_forcing_effect, "Swiss Referendum System: Consensus Forcing via Veto Threat").
narrative_ontology:topic_domain(swiss_referendum_system__consensus_forcing_effect, "political/comparative/direct_democracy").

domain_priors:requires_active_enforcement(swiss_referendum_system__consensus_forcing_effect).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(swiss_referendum_system__consensus_forcing_effect, '37d83baa-3591-4717-9b7c-2394c1687c77').
narrative_ontology:cs_kernel_codification('37d83baa-3591-4717-9b7c-2394c1687c77', formalized).
narrative_ontology:cs_authority_grounding('37d83baa-3591-4717-9b7c-2394c1687c77', lineage).
narrative_ontology:cs_interpretation_layer_present('37d83baa-3591-4717-9b7c-2394c1687c77').
narrative_ontology:cs_reading_relation('37d83baa-3591-4717-9b7c-2394c1687c77', swiss_referendum_system__minority_rights_tension, coexists_with).
narrative_ontology:cs_reading_relation('37d83baa-3591-4717-9b7c-2394c1687c77', swiss_referendum_system__popular_initiative_engine, influences).
narrative_ontology:cs_axiom('37d83baa-3591-4717-9b7c-2394c1687c77', foundational, referendum_veto_forces_consensus).
narrative_ontology:cs_axiom_status(referendum_veto_forces_consensus, holdable).
narrative_ontology:cs_axiom_grounding('37d83baa-3591-4717-9b7c-2394c1687c77', referendum_veto_forces_consensus, empirically_contingent).
narrative_ontology:cs_axiom('37d83baa-3591-4717-9b7c-2394c1687c77', foundational, majoritarian_alternation_structurally_suppressed).
narrative_ontology:cs_axiom_status(majoritarian_alternation_structurally_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('37d83baa-3591-4717-9b7c-2394c1687c77', majoritarian_alternation_structurally_suppressed, deontological).
narrative_ontology:cs_reference_frame('37d83baa-3591-4717-9b7c-2394c1687c77', direct_democracy_veto_equilibrium).
narrative_ontology:cs_drift_state('37d83baa-3591-4717-9b7c-2394c1687c77', contemporary_formula_rigidification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37d83baa-3591-4717-9b7c-2394c1687c77', '').
narrative_ontology:cs_kernel_id(swiss_referendum_system__consensus_forcing_effect, swiss_referendum_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(swiss_referendum_system__consensus_forcing_effect, permanent_coalition_parties).
narrative_ontology:constraint_beneficiary(swiss_referendum_system__consensus_forcing_effect, consensus_participants).
narrative_ontology:constraint_victim(swiss_referendum_system__consensus_forcing_effect, majoritarian_exclusion_prevention).
narrative_ontology:constraint_victim(swiss_referendum_system__consensus_forcing_effect, alternating_opposition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MINORITY FACING REFERENDUM VETO (SNARE) — Any political force that wins significant support (roughly 5%+ of voters, 100k signatures) but is excluded from coalition negotiations faces the worst position: they can sink legislation via referendum threat but cannot participate in governance. They are trapped in a binary: accept subordinate role in coalition or deploy veto power repeatedly, which is costly and delegitimizes their veto mechanism. Maximum suppression of alternative governance pathways (only two options, both costly).
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSENSUS COALITION CORE (ROPE) — The traditional 'magic formula' parties (SVP, CVP, SPS, FDP until 2015) experience the constraint as pure coordination. The referendum threat from outside forces creates alignment incentives: coalition members trade policy positions to keep excluded parties from veto power. This is coordination with asymmetric benefit distribution, but genuine coordination function — prevents policy gridlock that would result from repeated referendum campaigns. Beneficiary in the positive sense: gets to govern.
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NEWLY ASCENDING PARTY NEGOTIATING INCLUSION (TANGLED ROPE) — Parties that grow large enough to threaten coalition stability (SVP in 1970s-80s, Greens in 1990s, FDP's loss of coalition position in 2015) face mixed dynamics. They gain real policy influence and governance participation (rope benefit) but experience extraction through the consensus requirement — must compromise foundational positions to stay in coalition. The threat mechanism that brought them to the table persists: if they break consensus too severely, they face exclusion and delegitimization. Mixed coordination (keeping them inside prevents veto campaigns) and extraction (they must moderate policy positions).
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED COALITION PARTNER (TANGLED ROPE, POWERFUL MOBILE) — SVP as dominant coalition member by 2000s, with sufficient electoral strength to credibly threaten exit: experiences both coordination benefits (influence over federal policy, guaranteed cabinet seats) and extraction costs (must maintain internal party discipline to preserve coalition, loses ideological clarity, absorbs electoral losses when coalition is unpopular). Mobile in the sense that SVP has demonstrated capacity to threaten and execute exit (partially withdrew cooperation 2007-2015), but constrained by the system's design: full exit means returning to veto-threat powerlessness.
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED REFERENDUM MOVEMENTS (SCAFFOLD) — Direct-democracy instruments (referendum and initiative) provide an exit pathway from consensus discipline: movements can mobilize voter coalitions independently of parliamentary coalition structures. Swiss People's Party deployment of initiatives in 1990s (asylum, immigration, minarets) demonstrates that the referendum is a structural escape hatch from consensus stagnation. This is a sunset dynamic: as populist movements mature and gain political power, the scaffold is dismantled (party co-optation, inclusion in coalition) or hardened (if they remain excluded, scaffold persists as permanent alternative). Theater low because referendum function is substantive, not performative.
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL COUNCIL INSTITUTIONAL PERFORMANCE (PITON) — The Swiss Federal Council itself is substantially performative: the 'magic formula' distribution (originally 2-2-2 SVP-CVP-SPS-FDP, reformed 2015 to 2-2-2-1-1) is presented as a natural law of proportional governance. In reality, the formula's institutional inertia obscures the referendum threat mechanism that drives it. The council's consensus ritual — collective government responsibility, unanimous cabinet communications — appears as immutable institutional design but functions primarily to smooth over the underlying veto-threat coordination. Theater high because much council activity is ceremonial coalition management rather than policy innovation.
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — INSTITUTIONAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal perspective, consensus governance is an immutable property of small, pluralistic democracies with veto-capable minorities: any system that gives minorities referendum power MUST evolve consensus norms or face permanent gridlock. This perspective sees the consensus-forcing effect as a natural law of political systems, not a contingent institutional arrangement. However, the structural data reveals false summit dynamics: the referendum threat is a humanly-designed mechanism, not a law of nature, and its extractive effects on alternating opposition are measurable and contingent.
constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swiss_referendum_system__consensus_forcing_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swiss_referendum_system__consensus_forcing_effect, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(swiss_referendum_system__consensus_forcing_effect, TR),
    TR >= 0.70.

:- end_tests(swiss_referendum_system__consensus_forcing_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. The consensus-forcing system produces real extraction effects — it suppresses majoritarian winner-take-all governance, forces permanent coalition compromise, and converts opposition veto power into co-government co-optation. However, extraction is not severe because: (1) the mechanism preserves real political power for ascending minorities (they gain cabinet seats and policy influence, not merely symbolic inclusion); (2) alternatives for excluded parties exist (referendum campaigns, though costly); (3) the system produces policy stability valued by many constituencies. Historical trajectory shows rising extractiveness: at 1950 (early consensus norms, before SVP mobilization), base extractiveness was ~0.25 (loose coalition arrangements, little suppression of majoritarian norms). By 1975 (SVP threatening coalition stability), extractiveness rose to 0.38 as consensus norms hardened. By 2000 (formula fully entrenched), extractiveness ~0.42 (system now explicitly enforces proportionality against winner-take-all logic). Suppression (0.42): Moderate-high. The referendum veto threat creates suppression of majoritarian norms: parties cannot operate as winner-take-all majorities; opposition parties cannot credibly threaten clean electoral alternation; ascending parties face binary choice (consensus co-optation or veto legitimacy costs). However, suppression is not total because: (1) referendum campaigns are genuinely deployable (not pure theater); (2) parties can threaten and execute partial coalition exits (SVP demonstrated this 2007-2015); (3) voter movements retain agenda-setting capacity outside coalition. Theater ratio (0.35): Low-moderate. The consensus system has some genuine coordination function — it prevents policy gridlock from repeated referendum campaigns and distributes power in proportion to electoral strength. But theater has increased over time: the 'magic formula' is increasingly presented as institutional necessity rather than contingent arrangement; Federal Council collective responsibility rituals are substantially ceremonial; consensus discipline increasingly functions to hide underlying distributive conflicts rather than resolve them. The rising theater trajectory (0.25 → 0.32 → 0.35) reflects increasing institutionalization and ritualization of what began as a functional coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The consensus-forcing system generates maximum perspectival variation. From the powerless excluded minority (trapped), it appears as a snare: they have veto power but using it repeatedly delegitimizes them, while accepting coalition terms means policy compromise. From the consensus coalition core (institutional arbitrage), it appears as pure rope: coordination mechanism that prevents external veto interference. From ascending parties (moderate constrained), it appears as tangled rope: real power and policy influence (rope benefit) but extraction through forced compromise and discipline. From established coalition members (powerful mobile), it appears as tangled rope with more agency: they can credibly threaten exit and have demonstrated willingness to execute partial withdrawals. From organized referendum movements (organized constrained), it appears as scaffold: referendum instruments provide exit pathways from consensus discipline, though these pathways erode as movements are co-opted into parties. From the Federal Council (institutional arbitrage), it appears as piton: the institution is substantially performative, maintaining consensus ritual that obscures the underlying veto mechanism. From the analytical observer (civilizational), it risibly appears as mountain: an immutable law of direct-democracy systems. The false summit detection reveals this last perspective as naturalizing a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to the veto-threat mechanism and consensus extraction. Consensus coalition core parties (institutional arbitrage): beneficiaries get low d (~0.15) because they benefit from the mechanism and have arbitrage (exit) options — if they compromise too severely, they can threaten coalition withdrawal. Ascending parties (moderate constrained): victims of co-optation pressures get moderate-high d (~0.58) because the mechanism extracts policy compromise as cost of coalition inclusion, and exit options are constrained (leaving means referendum veto-threat powerlessness). Excluded minorities (powerless trapped): face maximum d (~0.92) because they bear full cost of the veto mechanism — they cannot credibly deploy veto without delegitimization and cannot exit to alternative governance. Referendum movements (organized constrained): face moderate d (~0.65) because they have structural exit option (mobilize outside coalition) but face co-optation pressure as they grow. Federal Council (institutional arbitrage): low d despite institutional role because they perform consensus ritual but do not directly extract from it — beneficiary parties extract, not the institution itself. The analytical observer (analytical analytical): canonical d ~0.73 per legacy table. No directionality overrides needed; derivation chain captures structural positions accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival decomposition. The core claim — 'referendum threat forces consensus' — is not falsified by alternate perspectives; it is SPECIFIED by them. The beneficiary coalition core sees rope (coordination function genuine). The ascending parties see tangled rope (mix of coordination and extraction). The excluded minorities see snare (veto threat without co-optation option). The organized movements see scaffold (referendum escape hatch with sunset as parties are co-opted). The Federal Council sees piton (consensus ritual increasingly performative as formula rigidifies). The analytical observer must reject the mountain classification (not a natural law) and see tangled rope or snare depending on whether one emphasizes the coordination benefits of consensus or the suppression of majoritarian alternation. The mandatrophy dissolves because all six types are real; the constraint exhibits them from different positions. The question 'Is the system a coordination mechanism or an extraction mechanism?' has no single answer — it is both, and the balance shifts across observer positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_extraction_measurement,
    'Is the cost imposed on ascending minority parties (policy compromise, co-optation) extractive in nature, or a legitimate coordination price?',
    'Comparative analysis of policy position shifts pre- and post-coalition inclusion; measurement of ideal-point movement relative to voting base; survey data on party member alienation.',
    'If extraction: extractiveness should be ≥0.50, and classification shifts toward snare from more perspectives. If coordination: extractiveness stable at current ~0.38, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_extraction_measurement, empirical, 'Whether consensus participation imposes extractive costs on minority parties').

omega_variable(
    veto_threat_enforcement_mechanism,
    'Is the referendum veto threat genuinely credible and deployable, or mostly symbolic given high costs of repeated campaigns?',
    'Historical frequency of actual referendum campaigns post-exclusion; cost analysis of signature collection and campaign infrastructure; success rate of excluded parties'' referendum challenges vs coalition-negotiated outcomes.',
    'If highly credible: suppression remains moderate (alternatives exist). If mostly symbolic: suppression increases (excluded parties forced into coalition despite veto threat cost), classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_threat_enforcement_mechanism, empirical, 'Credibility and deployability of referendum veto threat').

omega_variable(
    alternating_opposition_foreclosure,
    'Does the consensus system permanently foreclose alternating governments (winner-take-all opposition rotation), or merely suppress majoritarian norms while preserving potential for future alternation?',
    'Comparison with Westminster alternating-government systems; measurement of opposition time outside coalition; analysis of whether parties could realistically exit consensus and form alternative coalitions.',
    'If permanently foreclosed: consensus system is an irreversible extraction constraint (snare for opposition in perpetuity). If suppressed but potentially reversible: tangled_rope classification holds; alternation remains theoretically possible under changed conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternating_opposition_foreclosure, conceptual, 'Whether consensus permanently forecloses alternating opposition or merely suppresses it').

omega_variable(
    reading_sibling_contest_natural_law_vs_constructed,
    'Is the consensus-forcing effect a natural law of direct-democracy systems with veto capability, or a specific institutional arrangement contingent on how referendum mechanisms are designed and used?',
    'Cross-national comparison: do other countries with referendum power (Denmark, Ireland) converge on consensus norms? Historical counterfactual: would Swiss consensus have evolved if referendum accessibility were stricter or easier? Alternative scenario analysis: what consensus norms would emerge under different referendum signature thresholds?',
    'This is the core reading ambiguity routed through omega per Rule 2. If natural law: this reading''s core claim (referendum threat forces consensus) is a discovered structural necessity, and sibling reading minority_rights_tension becomes foreclosed (rights cannot protect against inevitable consensus). If constructed: this reading is one institutional arrangement among alternatives, and sibling readings coexist as equally live institutional choices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_sibling_contest_natural_law_vs_constructed, conceptual, 'Natural law vs. contingent institutional arrangement for consensus-forcing effect (kernel contest signature)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(swiss_referendum_system__consensus_forcing_effect, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swiss_ref_1950_theater, swiss_referendum_system__consensus_forcing_effect, theater_ratio, 0, 0.25).
narrative_ontology:measurement(swiss_ref_1980_theater, swiss_referendum_system__consensus_forcing_effect, theater_ratio, 20, 0.32).
narrative_ontology:measurement(swiss_ref_2000_theater, swiss_referendum_system__consensus_forcing_effect, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(swiss_ref_1950_base_extract, swiss_referendum_system__consensus_forcing_effect, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(swiss_ref_1975_base_extract, swiss_referendum_system__consensus_forcing_effect, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(swiss_ref_2000_base_extract, swiss_referendum_system__consensus_forcing_effect, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(swiss_ref_1950_suppression, swiss_referendum_system__consensus_forcing_effect, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(swiss_ref_1975_suppression, swiss_referendum_system__consensus_forcing_effect, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(swiss_ref_2000_suppression, swiss_referendum_system__consensus_forcing_effect, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(swiss_referendum_system__consensus_forcing_effect, enforcement_mechanism).
narrative_ontology:affects_constraint(swiss_referendum_system__consensus_forcing_effect, swiss_referendum_system__minority_rights_tension).
narrative_ontology:affects_constraint(swiss_referendum_system__consensus_forcing_effect, swiss_referendum_system__popular_initiative_engine).

% DUAL FORMULATION NOTE:
% The swiss_referendum_system kernel contains three structurally distinct constraints with different ε values and victim sets. CONSENSUS_FORCING_EFFECT (this story, ε≈0.38) models how veto threats suppress majoritarian alternation. MINORITY_RIGHTS_TENSION (sibling, ε≈0.55) models how direct-democracy mechanisms enable majoritarian voting on minority protections. POPULAR_INITIATIVE_ENGINE (sibling, ε≈0.30) models how initiatives distribute agenda-setting power to citizen movements outside coalition control. All three operate within the same institutional frame but emphasize orthogonal structural features. This story does not subsume the siblings — they are genuinely separate constraint mechanisms that coexist within the Swiss system. The network links indicate structural influence: this story (veto suppression of alternation) influences the siblings (rights protection becomes negotiated within consensus framework; initiative agenda power is partially captured as veto threat).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(swiss_referendum_system__consensus_forcing_effect, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
