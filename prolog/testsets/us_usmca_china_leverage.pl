% ============================================================================
% CONSTRAINT STORY: us_usmca_china_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_usmca_china_leverage, []).

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
 *   constraint_id: us_usmca_china_leverage
 *   human_readable: US leveraging of USMCA ratification to constrain Canadian foreign policy on China
 *   domain: geopolitical/trade
 *
 * SUMMARY:
 *   The USMCA leverage constraint represents asymmetric extraction by the US
 *   over Canadian foreign policy autonomy, mediated through trade agreement
 *   ratification conditionality. During 2017-2020, the Trump administration
 *   explicitly linked USMCA ratification timing and terms to Canadian
 *   alignment on China policy — specifically, exclusion of Huawei from 5G
 *   infrastructure, detention of Chinese executives at US request, and
 *   broader positioning against China in technology competition. Canada faced
 *   a structural dilemma: USMCA ratification was economically essential
 *   (representing ~25% of Canadian trade), but ratification required
 *   accepting constraints on independent China policy. The constraint
 *   exhibits genuine coordination function (USMCA does coordinate North
 *   American trade, reduce tariff barriers, and integrate supply chains)
 *   alongside asymmetric extraction (US retains veto over ratification timing
 *   and can weaponize it). The theater ratio is moderate (~0.48) because the
 *   leverage operates through explicit political pressure rather than
 *   institutional procedure — the formal USMCA text contains no China policy
 *   provisions, so enforcement relies on raw political signaling, not
 *   procedural theater. However, the constraint shows increasing theater over
 *   time as USMCA becomes institutionalized and formal dispute resolution
 *   mechanisms remain unused.
 *
 * KEY AGENTS:
 *   - US Administration (Trump executive): Primary beneficiary (institutional/arbitrage) — extracts China policy alignment with minimal cost, can walk away from leverage if interests change
 *   - Canadian Federal Government: Primary victim (moderate/trapped) — must choose between ratification (with policy constraints) or rejection (with trade devastation)
 *   - Canadian Export Industries (auto, agriculture, pharmaceuticals): Secondary victim (powerful/constrained) — benefit from market access but constrained by policy requirements they didn't negotiate
 *   - Canadian Export Associations and Business Councils: Organized secondary victim (organized/constrained) — can lobby but cannot override federal policy or US leverage
 *   - US Strategic Community (State Department, CSIS, military): Beneficiary (institutional/arbitrage) — sees USMCA ratification as coordinating North American anti-China alignment
 *   - USMCA Institutional Framework: Piton observer (institutional/arbitrage) — formal structures (dispute panels, rules of origin) are performative; actual leverage operates outside text
 *   - Alternative Trade Coalition (CPTPP, IPEF, AUKUS): Counter-power (organized/mobile) — represents exit path with sunset logic as alternatives develop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_usmca_china_leverage, 0.58).
domain_priors:suppression_score(us_usmca_china_leverage, 0.72).
domain_priors:theater_ratio(us_usmca_china_leverage, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_usmca_china_leverage, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_usmca_china_leverage, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_usmca_china_leverage, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_usmca_china_leverage, tangled_rope).
narrative_ontology:human_readable(us_usmca_china_leverage, "US leveraging of USMCA ratification to constrain Canadian foreign policy on China").
narrative_ontology:topic_domain(us_usmca_china_leverage, "geopolitical/trade").

domain_priors:requires_active_enforcement(us_usmca_china_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_usmca_china_leverage, us_strategic_position).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_policy_autonomy).
narrative_ontology:constraint_victim(us_usmca_china_leverage, canadian_trade_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANADIAN POLICY MAKERS (SNARE) — Trapped between ratification dependency and sovereignty constraints. USMCA ratification is essential for Canadian trade access; rejection would trigger severe economic consequences. Yet acceptance binds policy on China to US preferences. Cannot exit without bearing massive trade costs; suppression is structural and enforced through economic leverage.
constraint_indexing:constraint_classification(us_usmca_china_leverage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CANADIAN EXPORT INDUSTRIES (TANGLED ROPE) — Benefit from USMCA market access and supply chain integration, but constrained by policy requirements. The coordination function (trade rules, tariff reduction) is genuine; the extraction (US veto over China policy) is asymmetric. Constrained exit: leaving USMCA is possible but economically devastating for auto, agriculture, tech sectors.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US STRATEGIC COORDINATING ELITE (ROPE) — Experiences the constraint as pure coordination: aligning North American responses to China's trade and technology practices. From this view, USMCA + policy alignment = efficient collective action without significant enforcement cost. Beneficiary: arbitrage options enable exit without penalty (can walk away from USMCA lever if China threat recedes). Net positive from coordination logic.
constraint_indexing:constraint_classification(us_usmca_china_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CANADIAN BUSINESS ASSOCIATIONS (TANGLED ROPE) — Organized actors with moderate agency. Benefit from market access but face China policy constraints. Can lobby and negotiate terms but cannot unilaterally exit. Extractiveness is real but negotiable; suppression moderate because business interests can voice objections through diplomatic channels.
constraint_indexing:constraint_classification(us_usmca_china_leverage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: NAFTA/USMCA INSTITUTIONAL FRAMEWORK (PITON) — The trade agreement itself is increasingly performative as a coordination mechanism. Theater ratio high: formal dispute resolution procedures (Chapter 19, Panel XX) are rarely invoked; actual power operates through political leverage outside the institutional text. The agreement persists through inertia and lock-in, not because the formal mechanism effectively coordinates trade. The US leverage works because USMCA ratification is essential, not because Chapter 31 process is functional.
constraint_indexing:constraint_classification(us_usmca_china_leverage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL ALIGNMENT COUNTER-COALITION (SCAFFOLD) — International coordination mechanisms (CPTPP, AUKUS-adjacent, Indo-Pacific Economic Framework) represent alternative pathways that reduce USMCA leverage by diversifying trade partners and security alignment. Canada's potential participation in these frameworks creates an exit option with sunset logic: as alternative supply chains mature (2025-2035 horizon), the leverage cost of USMCA dependency declines. Low experienced extraction because this coalition has mobility and sees an exit path.
constraint_indexing:constraint_classification(us_usmca_china_leverage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL REALISM (MOUNTAIN) — From a civilizational perspective, geopolitical leverage through trade dependency is an immutable constraint of international anarchy: states with market power extract concessions from trade-dependent neighbors; this is structural to the system. No state can exit without bearing costs; suppression is inherent to power asymmetry. However, the structural data contradicts this: the leverage is contingent on USMCA ratification and on China threat salience. Different trade architecture or reduced China tensions would dissolve the lever.
constraint_indexing:constraint_classification(us_usmca_china_leverage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_usmca_china_leverage_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_usmca_china_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_usmca_china_leverage, TR),
    TR >= 0.70.

:- end_tests(us_usmca_china_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The US captures China policy alignment without compensating Canada through additional market access or exemptions. However, the extraction is not maximal (~0.66+) because the trade benefits of USMCA are genuine and asymmetrically favor both parties relative to pre-agreement baseline. The extraction is the *marginal* cost of accepting China policy constraints. Suppression (0.72): High. Canada faces severe costs for refusal: rejection of USMCA triggers immediate tariff exposure, supply chain disruption, and economic downturn. Alternatives (bilateral renegotiation, other trade agreements) are slow and incomplete. Career and political costs for Canadian leaders refusing ratification are severe. Theater ratio (0.48): Moderate. The leverage operates through explicit political communication and ratification conditionality, not institutional procedure. The constraint is transparent (everyone knows the US is conditioning ratification), so theater is lower than institutional mechanisms. However, the theater is increasing: USMCA is becoming routine, and future leverage will depend more on ambiguous institutional precedent than explicit pressure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximum perspectival disagreement. From the US perspective (beneficiary/arbitrage), it's pure coordination: aligning allies on a strategic threat with minimal coercion — they see rope. From the Canadian policy perspective (victim/trapped), it's a snare: inescapable extraction with no alternatives. From the business perspective (mixed/constrained), it's tangled rope: benefits from trade access but constrained by policy requirements. From the institutional view (piton), it's a degraded mechanism: formal USMCA structures are irrelevant; real leverage operates through political signaling outside the agreement. From the counter-coalition view (scaffold), it's temporary: alternative trade architectures are creating exit options with sunset logic. From the civilizational analytical view (mountain), it risks naturalization: framing power asymmetry in trade as inherent structural realism rather than contingent on specific agreements and administrations. The gap is maximal because the constraint's extractiveness depends entirely on USMCA's necessity — change the trade architecture or China threat salience, and the constraint dissolves.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the constraint. The US (institutional/arbitrage) has high exit options — it can withdraw leverage or walk away from USMCA if other priorities emerge. This derives d ≈ 0.15-0.20 (beneficiary + arbitrage exit) → negative f(d) ≈ -0.01 → very low experienced extraction. Canadian policy makers (moderate/trapped) have low exit options — they must ratify to survive economically, yet ratification locks them into constraints. This derives d ≈ 0.85-0.95 (victim + trapped exit) → high f(d) ≈ 1.15-1.42 → high experienced extraction (χ ≈ 0.75+). Canadian exporters (powerful/constrained) occupy middle ground: they benefit from market access but are constrained by policy requirements negotiated without their input. This derives d ≈ 0.60-0.65 (mixed beneficiary-victim + constrained exit) → moderate f(d) ≈ 0.85-1.00 → moderate experienced extraction (χ ≈ 0.49-0.58). The piton perspective (institutional/arbitrage) has no vector into the extraction: it's an observer of the institutional arrangement, not a participant experiencing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy through the tangled rope classification because it satisfies all three gates: (1) Genuine coordination function: USMCA does reduce tariffs, integrate supply chains, and align trade rules — not theater, but functional coordination. (2) Asymmetric extraction: US extracts China policy alignment that benefits its strategic position more than Canada's. (3) Active enforcement: US explicitly conditions ratification timing and terms on Canadian policy alignment — enforcement is transparent and sustained. The constraint is NOT a snare because the coordination benefits are real (not zero-sum), and the extraction is mediated through a genuine trade agreement, not purely through coercion. The constraint is NOT a rope because the extraction is severe (~0.58) and beneficiaries/victims are clearly asymmetric. Tangled rope correctly captures the hybrid: real coordination in trade terms, real extraction in policy autonomy, and hybrid enforcement (both institutional procedure and political pressure). The piton perspective is a separate analytical finding: USMCA's formal institutions (dispute panels, Chapter 31 mechanisms) are increasingly performative as actual leverage operates through political signaling. But the constraint as a whole is tangled rope, not piton, because the coordination function remains primary and the extraction is active, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_commitment_credibility,
    'Would the US actually enforce USMCA withdrawal if Canada resisted China policy pressure, or is the threat primarily rhetorical?',
    'Historical analysis of US follow-through on trade threats; comparison of stated ratification conditions to actual implementation; analysis of Trump administration trade-war outcomes with other partners',
    'If credible: suppression is high (~0.72) and constraint is genuine snare from Canadian view. If rhetorical: suppression is lower (~0.45) and constraint weakens to tangled rope for all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_credibility, empirical, 'Credibility of US USMCA enforcement threats').

omega_variable(
    china_threat_salience_decay,
    'As China threat perception declines or trade friction eases (through negotiation or change of administrations), does the US leverage mechanism automatically weaken?',
    'Tracking of US threat rhetoric, trade tensions, and geopolitical realignment; analysis of how China-focused justifications for USMCA leverage track with actual bilateral US-China relations',
    'If threat salience drives leverage: constraint is contingent and extractiveness should decline with threat perception (extractiveness now ~0.58 but potentially ~0.25 if threat recedes). If leverage persists independent of threat: constraint is more structural (extractiveness remains ~0.58).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_threat_salience_decay, empirical, 'Whether constraint leverage decays as China threat perception changes').

omega_variable(
    alternative_market_development,
    'Can Canada develop sufficient alternative trade relationships (CPTPP, bilateral with EU/India) to reduce USMCA dependency below the pain threshold that makes US leverage effective?',
    'Long-term trade diversification modeling; assessment of whether non-US trade can reach 40%+ of Canadian exports within 15-year horizon; comparison with Mexico''s analogous dependency ratios',
    'If achievable: Canada''s exit options improve from trapped to mobile/arbitrage over time (scaffold perspective gains structural ground). If not achievable: trap persists and suppression remains high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_market_development, empirical, 'Feasibility of Canadian trade dependency reduction').

omega_variable(
    china_policy_alignment_scope,
    'Is the US leverage limited to explicit anti-China policies (Huawei bans, 5G restrictions, foreign investment screening) or does it extend to broader geopolitical alignment (Taiwan, South China Sea, QUAD-style positioning)?',
    'Textual analysis of US pressure communications; tracking of which specific Canadian policy reversals correlate with USMCA ratification timeline; comparison of pressure scope across different administrations',
    'If limited to tech/trade: extractiveness is moderate (~0.58 current). If extended to full strategic alignment: extractiveness increases toward ~0.70 (high snare). If vague/diffuse: theater ratio increases (piton threat increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(china_policy_alignment_scope, conceptual, 'Scope and specificity of US China policy pressure on Canada').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_usmca_china_leverage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usmca_tr_t0, us_usmca_china_leverage, theater_ratio, 0, 0.38).
narrative_ontology:measurement(usmca_tr_t3, us_usmca_china_leverage, theater_ratio, 3, 0.44).
narrative_ontology:measurement(usmca_tr_t6, us_usmca_china_leverage, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(usmca_be_t0, us_usmca_china_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usmca_be_t3, us_usmca_china_leverage, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(usmca_be_t6, us_usmca_china_leverage, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_usmca_china_leverage, enforcement_mechanism).
narrative_ontology:affects_constraint(us_usmca_china_leverage, us_china_tech_competition).
narrative_ontology:affects_constraint(us_usmca_china_leverage, canadian_huawei_5g_policy).
narrative_ontology:affects_constraint(us_usmca_china_leverage, north_american_supply_chain_coupling).

% DUAL FORMULATION NOTE:
% This constraint is part of a broader constraint family around US-China competition and its spillovers into allied state autonomy. The upstream constraint (us_china_tech_competition, ε ≈ 0.15, Mountain) represents the fundamental US-China rivalry as an immutable structural fact. The present constraint (us_usmca_china_leverage, ε = 0.58, Tangled Rope) represents how US leverage over trade agreements amplifies that rivalry into extraction from intermediate states. Downstream constraints (canadian_huawei_5g_policy, ε ≈ 0.62, Snare; us_tech_supply_chain_restriction, ε ≈ 0.48, Tangled Rope) represent specific policy outcomes driven by the leverage mechanism. The present constraint mediates upstream rivalry into downstream policy constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_usmca_china_leverage, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
