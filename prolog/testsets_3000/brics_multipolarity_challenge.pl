% ============================================================================
% CONSTRAINT STORY: brics_multipolarity_challenge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brics_multipolarity_challenge, []).

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
 *   constraint_id: brics_multipolarity_challenge
 *   human_readable: BRICS Multipolarity Challenge: Coordination vs. Extraction in Alternative Global Order
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   The BRICS multipolarity challenge represents a structural tension between
 *   coordinating a genuinely multipolar international order and enabling
 *   extraction through new hierarchies within that order. BRICS emerged as
 *   coordination mechanism for developing nations to counterbalance Western
 *   institutional dominance (IMF, World Bank, UN Security Council) while
 *   creating alternative institutional pathways (New Development Bank,
 *   alternative payment systems, unified voting blocs). However, the same
 *   coordination mechanisms that enable multipolarity also enable new forms
 *   of extraction: China's dominance over member economies through resource
 *   demands and capital control, geopolitical alignment demands from larger
 *   members toward smaller ones, and conditionality through alternative
 *   financing that mirrors IMF structural adjustment logic. The constraint
 *   exhibits classical tangled rope structure at the core (BRICS institutions
 *   coordinate genuine transition away from Western hegemony while enabling
 *   extraction among members) with snare dynamics at the periphery (smallest
 *   developing nations trapped between Western and BRICS coercion) and
 *   scaffold aspirations (if alternative institutions mature, extraction
 *   should decline). The theater ratio (0.64) reflects that BRICS maintains
 *   high performative content — unity statements, summit declarations,
 *   institutional legitimacy-building — while institutional functionality
 *   remains contested. The extractiveness (0.58) reflects moderate but real
 *   asymmetry in benefits: dominant members (China) extract from smaller
 *   members while coordination benefits are genuine but unevenly distributed.
 *
 * KEY AGENTS:
 *   - China: Dominant BRICS power (institutional/arbitrage) — primary beneficiary through resource access, capital dominance, Belt-and-Road financing integration
 *   - India & Brazil: Rising regional powers (powerful/mobile) — benefit from institutional voice and multipolarity but constrained by Chinese dominance and regional vulnerability
 *   - South Africa: Mid-tier member (moderate/constrained) — experiences genuine coordination benefits alongside extraction pressure; most exposed to institutional fragility
 *   - Smaller developing economies: Excluded or peripheral members (powerless/trapped) — dependent on either Western or BRICS patronage with no autonomous path
 *   - Non-aligned institutional network: Organized stakeholders (organized/constrained) — African Union, ASEAN, regional development banks viewing BRICS as transitional scaffold
 *   - Western institutional order: Legacy hegemonic system (institutional/arbitrage) — views BRICS as threatening but also maintains dominance through institutional inertia
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — assesses whether multipolarity represents genuine institutional shift or reconfigured extraction hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brics_multipolarity_challenge, 0.58).
domain_priors:suppression_score(brics_multipolarity_challenge, 0.52).
domain_priors:theater_ratio(brics_multipolarity_challenge, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brics_multipolarity_challenge, extractiveness, 0.58).
narrative_ontology:constraint_metric(brics_multipolarity_challenge, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(brics_multipolarity_challenge, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brics_multipolarity_challenge, tangled_rope).
narrative_ontology:human_readable(brics_multipolarity_challenge, "BRICS Multipolarity Challenge: Coordination vs. Extraction in Alternative Global Order").
narrative_ontology:topic_domain(brics_multipolarity_challenge, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(brics_multipolarity_challenge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brics_multipolarity_challenge, emerging_market_governments).
narrative_ontology:constraint_beneficiary(brics_multipolarity_challenge, non_aligned_movement_states).
narrative_ontology:constraint_beneficiary(brics_multipolarity_challenge, brics_institutional_apparatus).
narrative_ontology:constraint_victim(brics_multipolarity_challenge, smaller_developing_economies).
narrative_ontology:constraint_victim(brics_multipolarity_challenge, brics_coalition_unity).
narrative_ontology:constraint_victim(brics_multipolarity_challenge, global_institutional_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED DEVELOPING NATION (SNARE) — Small developing states dependent on either Western institutional frameworks (IMF, World Bank) or BRICS patronage have no genuine exit. Trapped between coercive alternatives with no autonomous path. Faces extraction through debt mechanisms or geopolitical alignment demands masquerading as coordination.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER BRICS MEMBER (TANGLED ROPE) — States like South Africa or India within BRICS experience genuine coordination benefits (market access, alternative financing, institutional voice) alongside asymmetric extraction by dominant members (China's resource extraction, geopolitical demands, capital dominance). Constrained by capital requirements and regional vulnerability; benefits genuine but extraction is real.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMINANT BRICS POWER (ROPE) — China experiences BRICS primarily as coordination mechanism for managing multipolarity and securing resource access. Net beneficiary position with arbitrage options (can exit or degrade BRICS if constraints become unfavorable). Views the constraint as solving coordination problem: managing competing powers without hegemonic collapse.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RISING REGIONAL POWER (TANGLED ROPE) — India and Brazil benefit from BRICS coordination (counterweight to Western dominance, institutional legitimacy) but experience extraction from larger players and constraints on autonomous regional action. Mobile enough to adjust alliances but committed to multipolarity. Experience genuine mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-ALIGNED INSTITUTIONAL NETWORK (SCAFFOLD) — Organized actors (African Union, ASEAN, regional development banks) see BRICS as a temporary institutional scaffold enabling transition from Western-dominated to genuinely multipolar order. View the constraint as having sunset logic: as alternative institutions mature and coordination norms stabilize, the extractive aspects should decline. Theater is high but declining as legitimacy builds.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WESTERN INSTITUTIONAL ORDER (PITON) — Traditional institutions (IMF, World Bank, UN Security Council) view BRICS as a challenge to legitimacy but also as a performance opportunity. Retain institutional inertia and governance power while appearing to adapt. The constraint is degraded — BRICS coordination is high-theater legitimacy-building masking persistent Western institutional dominance. Maintained through institutional momentum rather than functional necessity.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The multipolarity challenge simultaneously coordinates genuine transition away from hegemonic order (real coordination function) AND enables new extractive hierarchies among members (China's resource demands, capital dominance, security sphere extension). The constraint is neither pure coordination nor pure extraction but a hybrid containing elements of both.
constraint_indexing:constraint_classification(brics_multipolarity_challenge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brics_multipolarity_challenge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brics_multipolarity_challenge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brics_multipolarity_challenge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brics_multipolarity_challenge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brics_multipolarity_challenge, TR),
    TR >= 0.70.

:- end_tests(brics_multipolarity_challenge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The BRICS coordination provides genuine institutional alternative (New Development Bank lending, alternative payment systems, unified voting mechanisms) but subordinates smaller members to larger ones through capital concentration, resource demands, and geopolitical alignment requirements. The extraction is real but tempered by genuine coordination function — distinguish from snare extraction (0.80+) which provides minimal coordination. The measurement trajectory shows rising extractiveness (0.42→0.62) as BRICS matures and China consolidates dominance within the coalition. Suppression (0.52): Moderate. Barriers to exit include: capital dependency on BRICS institutions, geopolitical vulnerability to coalition leaders, reputational cost of defection from non-aligned movement, but also meaningful alternatives (regional institutions, market diversification). Not total suppression but significant friction. Theater ratio (0.64): High and rising. BRICS summit declarations, institutional legitimacy-building, and unified voting statements contain high performative content. Actual institutional capacity (policy coordination, enforcement mechanisms, dispute resolution) lags far behind the ceremonial claims. Rising theater trajectory reflects that as real institutional development slows, performative content increases to maintain coalition appearance.
 *
 * PERSPECTIVAL GAP:
 *   The deepest gap opens between China's rope classification (coordination problem solved) and powerless small states' snare classification (trapped between coercive alternatives). This gap reveals the constraint's essential ambiguity: BRICS genuinely coordinated multipolarity transition but did so by creating new extraction hierarchies. For China, the constraint is a success — coordination achieved. For small states, the constraint is a trap — coerced participation in order that delivers minimal benefits. The middle perspectives (India, Brazil, South Africa) see the gap clearly: they benefit from multipolarity but bear disproportionate coordination costs. The scaffold perspective (non-aligned institutions) is aspirational — if alternative institutions mature on schedule, the extraction aspects should decline and theater ratio should drop. The piton perspective (Western order) is diagnostic of institutional brittleness — BRICS appears as challenge but Western dominance persists because alternative institutions remain immature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from agent structural position relative to extraction: China as dominant beneficiary derives d ≈ 0.05 (arbitrage exit, beneficiary status → low d → negative χ). India/Brazil as powerful beneficiaries with some extraction vulnerability derive d ≈ 0.35 (mobile exit, mixed beneficiary-victim → moderate d → moderate χ). South Africa as moderate member with asymmetric costs derives d ≈ 0.60 (constrained exit, victim status from larger members → high d → high χ). Small powerless states derive d ≈ 0.92 (trapped exit, victim status → very high d → maximum χ). Non-aligned coalition derives d ≈ 0.45 (constrained exit, mixed beneficiary-victim → moderate d → moderate χ). Western order derives d ≈ 0.20 (arbitrage exit, beneficiary from institutional inertia but threatened by alternatives → low d). The analytical observer derives d ≈ 0.72 (analytical exit, observes full structure → moderate-high d reflecting the hybrid nature).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that BRICS is genuinely BOTH coordination mechanism AND extraction apparatus. The traditional mandatrophy question ('Is BRICS coordination or extraction?') has a false dichotomy answer: it is tangled rope, containing both functions with asymmetric distribution. China experiences it as rope (pure coordination, high benefit). Powerless states experience it as snare (pure extraction, no benefit). Mid-tier members and analytical observers see both functions operating simultaneously. The mandatrophy resolves through perspectival decomposition: all classifications are correct relative to their observer's structural position. No single type 'is' the answer. The presheaf of classifications across all perspectives is the answer. This is the canonical resolution pattern for tangled rope constraints: they appear as rope from beneficiary perspectives, snare from victim perspectives, and tangled rope from mixed or analytical perspectives. The analytical observer's tangled rope classification is the base constraint description — the helicopter view that encompasses all perspectival readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    multipolar_equilibrium_stability,
    'Is the multipolar order structure stable, or will BRICS coordination collapse into bipolar competition or reconverge toward hegemonic order?',
    'Long-term institutional stability analysis; monitoring of BRICS institutional capacity growth relative to capability divergence among members; analysis of whether exit barriers increase or decrease over 10-20 year horizon',
    'If stable multipolar equilibrium: scaffold perspective confirmed — real institutional maturation with sunset logic. If collapsed: entire BRICS coordination was snare/tangled rope extraction mechanism masquerading as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multipolar_equilibrium_stability, empirical, 'Whether multipolar order proves institutionally stable').

omega_variable(
    extractive_mechanism_centrality,
    'Is extraction through BRICS (debt mechanisms, geopolitical alignment demands, market access control) a primary driver of member participation or a side effect of coordination?',
    'Game-theoretic analysis of payoff matrices for member states under BRICS participation vs alternatives; tracking of debt-to-BRICS-loan ratios; analysis of whether coalition members exit when extraction exceeds coordination benefits',
    'If primary: BRICS is snare/tangled rope with coordination function secondary. If side effect: genuine coordination with acceptable extraction overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_mechanism_centrality, empirical, 'Whether extraction is primary driver or secondary effect').

omega_variable(
    institutional_maturation_timeline,
    'On what timeline do alternative BRICS institutions (New Development Bank, BRICS currency initiatives, parallel payment systems) achieve functional parity with Western institutions?',
    'Capacity analysis of New Development Bank loan volume vs World Bank; market share analysis of alternative payment systems; institutional independence assessment of BRICS institutions vs member state control',
    'If maturation is fast (5-10 years): scaffold sunset is real. If slow (20+ years): scaffold is aspirational and members remain trapped in longer coordination period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_maturation_timeline, empirical, 'Timeline for institutional alternative maturation').

omega_variable(
    member_state_coalition_durability,
    'Do BRICS members maintain unified positions on key geopolitical issues, or does coalition fragment along regional/ideological lines?',
    'UN voting alignment analysis; tracking of public statements on major geopolitical events; monitoring of bilateral trade flows and investment patterns between members',
    'If unified: BRICS has real coordination function and represents genuine multipolar power. If fragmented: constraint is extraction mechanism disguised as coordination; members bear costs of coalition maintenance for minimal voice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_state_coalition_durability, empirical, 'Whether BRICS coalition maintains strategic unity').

omega_variable(
    narrative_naturalization_risk,
    'Is multipolarity framed as inevitable/natural consequence of development (mountain perspective), or is it recognized as contingent institutional design choice requiring active maintenance?',
    'Discourse analysis of BRICS rhetoric and academic commentary; tracking of whether multipolarity is treated as self-sustaining equilibrium or as dependent on institutional coordination',
    'If naturalized: members may underestimate brittleness and face sudden collapse. If recognized as contingent: members can actively reinforce institutional foundations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_naturalization_risk, conceptual, 'Risk of naturalizing multipolarity as inevitable rather than maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brics_multipolarity_challenge, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brics_tr_t0, brics_multipolarity_challenge, theater_ratio, 0, 0.48).
narrative_ontology:measurement(brics_tr_t5, brics_multipolarity_challenge, theater_ratio, 5, 0.58).
narrative_ontology:measurement(brics_tr_t10, brics_multipolarity_challenge, theater_ratio, 10, 0.64).
narrative_ontology:measurement(brics_tr_t15, brics_multipolarity_challenge, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(brics_be_t0, brics_multipolarity_challenge, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(brics_be_t5, brics_multipolarity_challenge, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(brics_be_t10, brics_multipolarity_challenge, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(brics_be_t15, brics_multipolarity_challenge, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brics_multipolarity_challenge, enforcement_mechanism).
narrative_ontology:affects_constraint(brics_multipolarity_challenge, imf_structural_adjustment).
narrative_ontology:affects_constraint(brics_multipolarity_challenge, world_bank_lending_conditionality).
narrative_ontology:affects_constraint(brics_multipolarity_challenge, un_security_council_legitimacy).
narrative_ontology:affects_constraint(brics_multipolarity_challenge, semiconductore_supply_chain_multipolarity).

% DUAL FORMULATION NOTE:
% BRICS multipolarity decomposes into two structurally distinct constraints: (1) alternative_institutional_development (ε≈0.25, rope: genuine coordination for building new institutions) and (2) BRICS_extraction_hierarchy (ε≈0.65, snare/tangled rope: asymmetric benefits distribution within coalition). The higher-extractiveness story (this file) is downstream of institutional maturation; the lower-extractiveness story focuses on coordination function. Both stories link via network.affects_constraints to show family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brics_multipolarity_challenge, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
