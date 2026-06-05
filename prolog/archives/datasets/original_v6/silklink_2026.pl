% ============================================================================
% CONSTRAINT STORY: silklink_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silklink_2026, []).

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
 *   constraint_id: silklink_2026
 *   human_readable: SilkLink Syria-Saudi Telecom Project
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The SilkLink Syria-Saudi Telecom Project represents a $1B infrastructure
 *   investment by Saudi Telecom Company in post-conflict Syria. On its
 *   surface, it is presented as coordination — enabling critical
 *   telecommunications infrastructure in a country with degraded capacity.
 *   However, the project exhibits clear asymmetric extraction: STC captures
 *   monopoly rents on telecom services, gains data access and geopolitical
 *   leverage for Saudi interests, and creates long-term infrastructure
 *   lock-in that constrains Syrian state autonomy and consumer choice. The
 *   constraint is not pure extraction (Snare) nor pure coordination (Rope),
 *   but a durable hybrid (Tangled Rope) where both coordination and
 *   extraction are structurally necessary — Syria needs the infrastructure,
 *   but STC's monopoly position ensures persistent extraction. The theater
 *   ratio (0.58) reflects that the project is presented in regulatory and
 *   development language (investment, infrastructure, regional integration)
 *   that obscures its asymmetric extraction mechanisms (monopoly pricing,
 *   data extraction, geopolitical leverage). The extractiveness trajectory
 *   (0.35 → 0.55 over 12 months) shows increasing extraction as STC's
 *   infrastructure becomes entrenched and switching costs for users rise.
 *
 * KEY AGENTS:
 *   - Saudi Telecom Company (STC): Primary beneficiary (institutional/arbitrage) — captures monopoly rents, data access, geopolitical leverage
 *   - Syrian State / Government: Structured victim + junior beneficiary (organized/constrained) — benefits from infrastructure coverage but becomes dependent on STC and loses regulatory autonomy
 *   - Syrian Telecom Users: Primary victims (powerless/trapped) — bear extraction costs through monopoly pricing, data access, service lock-in with no exit options
 *   - Domestic Syrian Telecom Competitors: Secondary victims (moderate/constrained) — face barriers to competing with STC's scale and Saudi backing
 *   - International Open Internet Advocates: Perspective-holders (moderate/mobile) — see temporary extraction with eventual sunset via satellite alternatives
 *   - International Regulatory Bodies: Institutional perspective-holders (institutional/arbitrage) — have nominal authority but low enforcement capacity in Syrian geopolitical context
 *   - Saudi State: Structural beneficiary (institutional/arbitrage) — gains geopolitical leverage through STC infrastructure control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silklink_2026, 0.55).
domain_priors:suppression_score(silklink_2026, 0.65).
domain_priors:theater_ratio(silklink_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silklink_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(silklink_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(silklink_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silklink_2026, tangled_rope).
narrative_ontology:human_readable(silklink_2026, "SilkLink Syria-Saudi Telecom Project").
narrative_ontology:topic_domain(silklink_2026, "technological/economic").

domain_priors:requires_active_enforcement(silklink_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silklink_2026, saudi_telecom_company).
narrative_ontology:constraint_beneficiary(silklink_2026, saudi_state_interests).
narrative_ontology:constraint_victim(silklink_2026, syrian_telecommunications_independence).
narrative_ontology:constraint_victim(silklink_2026, syrian_consumer_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYRIAN TELECOM USERS (SNARE) — Syrian consumers and businesses lack exit options from the SilkLink infrastructure once deployed. Infrastructure lock-in prevents alternative telecom pathways. STC controls pricing, data access, and service quality with minimal competitive pressure. Users bear extraction costs (data extraction, pricing power, service monopoly) with no appeal mechanism. Maximum experienced extraction due to trapped exit and asymmetric power.
constraint_indexing:constraint_classification(silklink_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SYRIAN TELECOM COMPETITORS (SNARE) — Domestic Syrian telecom operators (if any) face barriers to competing with STC's $1B infrastructure advantage. STC's Saudi backing and scale create extraction through market consolidation. Constrained exit options (cannot easily build parallel infrastructure); high suppression of competitive alternatives. Moderate power but minimal leverage against institutional actor.
constraint_indexing:constraint_classification(silklink_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAUDI TELECOM COMPANY (ROPE) — STC benefits from infrastructure deployment as coordination mechanism: enabling telecom coverage across Syria solves a collective action problem (lack of capital, technical capacity). STC experiences the constraint as coordination with net benefit. Arbitrage exit option: STC can monetize the asset or sell stakes. Institutional power and clear beneficiary status produce low effective extraction from STC's perspective.
constraint_indexing:constraint_classification(silklink_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SYRIAN STATE (TANGLED ROPE) — The Syrian government benefits from infrastructure coverage and foreign investment (coordination function), but also becomes dependent on STC for critical telecommunications infrastructure (extraction function). Constrained exit: once built, the infrastructure is difficult to reclaim without major economic cost. Organized actor but with significant structural vulnerability. Active enforcement required to maintain STC's monopoly position through licensing and regulatory capture.
constraint_indexing:constraint_classification(silklink_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL OPEN INTERNET ADVOCATES (SCAFFOLD) — Global actors promoting open internet standards, net neutrality, and alternative infrastructure (decentralized networks, community broadband) view the SilkLink as a temporary extractive monopoly that will eventually be bypassed by technological alternatives (satellite internet, mesh networks, blockchain-based telecom). Lower effective extraction due to mobile exit options and belief in sunset mechanism. Theater is moderate — regulatory compliance performance obscures underlying monopoly extraction.
constraint_indexing:constraint_classification(silklink_2026, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL REGULATORY FRAMEWORKS (PITON) — Global telecom regulations (ITU, WTO telecom commitments) theoretically require non-discriminatory market access and consumer protection, but enforcement is largely performative in Syria's geopolitical context. International bodies have arbitrage options (threaten sanctions, revoke recognition) but rarely exercise them over infrastructure monopolies. The regulatory apparatus is degraded — it persists through institutional inertia (treaty obligations, compliance theater) rather than functional enforcement.
constraint_indexing:constraint_classification(silklink_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the SilkLink combines genuine coordination (enabling telecom infrastructure in economically constrained region) with clear asymmetric extraction (STC monopoly, data access, pricing power, geopolitical leverage for Saudi interests). Both functions are structural and persistent. The constraint is not temporary (scaffold) or natural law (mountain) — it is a durable hybrid designed to extract while appearing to coordinate.
constraint_indexing:constraint_classification(silklink_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silklink_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silklink_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silklink_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silklink_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(silklink_2026, TR),
    TR >= 0.70.

:- end_tests(silklink_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. STC captures monopoly rents on telecom services in a market with limited competition and high switching costs. The extraction is not total (suppression prevents complete control), but is substantial and growing. Initial value of 0.35 reflects pre-deployment state when SilkLink was announced but not yet operational; trajectory to 0.55 reflects increasing extraction as infrastructure becomes entrenched and user lock-in rises. The value is not higher (0.70+) because Syrian regulatory capacity (however limited) still exists in theory, and alternative technologies (satellite internet) provide eventual exit pathways. Suppression (0.65): Moderate-high. Barriers to exit include infrastructure lock-in (once deployed, difficult to bypass), lack of alternative providers (capital constraints in Syria prevent competing infrastructure), regulatory capture (STC gains favorable terms), and geopolitical constraints (Saudi backing reduces Syrian government's willingness to regulate harshly). However, suppression is not total because alternative technologies exist (satellite, mesh networks) and international pressure on monopoly practices can theoretically reduce costs. Theater ratio (0.58): Moderate-high. The project is marketed as infrastructure development and regional integration (coordination language), while extraction mechanisms (monopoly pricing, data access, geopolitical leverage) are obscured by regulatory compliance theater and investment metrics. Theater is not higher (0.70+) because the project's extractive function is relatively visible to analysts; it is not lower because official rhetoric genuinely emphasizes coordination benefits.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits severe perspectival divergence. STC sees coordination (Rope) — they are solving a genuine infrastructure problem with fair market returns. The Syrian state sees mixed coordination and extraction (Tangled Rope) — it needs the infrastructure but also perceives loss of autonomy. Syrian consumers see pure extraction (Snare) — monopoly pricing, data access, and service lock-in with no choice. International actors see durable extraction (Tangled Rope) — the hybrid is not temporary. This gap is not due to measurement disagreement but structural reality: different agents occupy different positions relative to the extraction mechanism, and their experienced costs and benefits differ accordingly. The state's mid-position (organized actor, constrained but not trapped exit) produces the most balanced perception (Tangled Rope); the beneficiary's privileged position produces Rope; the powerless users' trapped position produces Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position — whether they benefit or bear costs, and what exit options they have. STC, as beneficiary with arbitrage options, derives low d (≈0.05-0.15) → low effective extraction in their perception. The Syrian state, as organized actor with constrained exit and mixed beneficiary/victim status, derives moderate d (≈0.50) → moderate effective extraction in their perception. Syrian users, as powerless agents with trapped exit and victim status, derive high d (≈0.85-0.95) → high effective extraction in their perception. The engine's sigmoid function f(d) amplifies these differences, producing the perspectival gap: STC sees Rope (negative/low chi), state sees Tangled Rope (moderate chi), users see Snare (high chi). The suppression value (0.65) is unscaled by directionality — it is a structural property of the constraint, not observer-relative.
 *
 * MANDATROPHY ANALYSIS:
 *   The SilkLink constraint is NOT a case of mislabeled pure coordination as extraction (Rope misclassified as Snare). The tangled rope classification is correct: the project genuinely coordinates (enables infrastructure), genuinely extracts (monopoly rents, data access, geopolitical leverage), and requires active enforcement (regulatory capture, Saudi backing) to sustain both functions. The mandatrophy is resolved by showing that both functions are structural, not confused. The state is not a 'powerless victim' (which would elevate it to Snare) — it is an organized actor with constrained but non-zero agency, and the coordination benefit (infrastructure) is real. STC is not engaged in pure predation — they are investing capital and technical capacity in a region that lacks private sector capacity. The constraint is stable as Tangled Rope because neither the coordination nor the extraction can be removed without changing the fundamental structure. The extraction cannot be eliminated (monopoly is intrinsic to large-scale infrastructure in capital-constrained regions), and the coordination cannot be eliminated (infrastructure is genuinely needed). Removing either would require external intervention (international regulation, competing infrastructure investment, technological substitution) — hence the omega variables on geopolitical leverage and satellite timeline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_depth,
    'Does the Syrian government retain genuine regulatory authority over STC operations, or has regulatory capture eliminated this authority entirely?',
    'Analysis of regulatory filings, service terms, pricing controls, and Syrian regulatory body independence; comparison to other foreign-operated telecom monopolies in post-conflict states',
    'If capture is total: snare classification from Syrian government perspective. If partial authority remains: tangled rope is correct. This determines whether the Syrian state is victim or junior beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth and completeness of regulatory capture by STC').

omega_variable(
    satellite_internet_substitution_timeline,
    'What timeline for satellite internet (Starlink, Amazon Kuiper, etc.) in Syria would make SilkLink ground infrastructure obsolete or non-essential?',
    'Technical analysis of satellite coverage maps, latency requirements for telecom/data services, cost trajectories, and Syrian government policy stance on competing technologies',
    'If timeline < 5 years: scaffold perspective is structural (genuine sunset). If > 15 years: SilkLink lock-in is long-term; classification remains snare/tangled rope. If indefinite: ground infrastructure remains essential despite alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(satellite_internet_substitution_timeline, empirical, 'Timeline for technological substitution of ground telecom').

omega_variable(
    geopolitical_leverage_asymmetry,
    'Does STC''s infrastructure control constitute a primary mechanism for Saudi geopolitical influence over Syrian state decisions?',
    'Historical analysis of Syrian telecommunications policy changes following SilkLink deployment; signals intelligence and diplomatic cable analysis where available; comparison to other infrastructure-based geopolitical leverage mechanisms',
    'If yes: extraction function is primarily geopolitical (snare classification stable). If no: extraction is primarily economic monopoly rent (tangled rope may upgrade toward rope). This determines whether the constraint is security-driven or market-driven.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_leverage_asymmetry, empirical, 'Degree to which STC infrastructure serves geopolitical control mechanism').

omega_variable(
    data_sovereignty_extraction,
    'To what extent does STC control and extract value from data flows through the SilkLink infrastructure?',
    'Review of data residency requirements, encryption standards, metadata access terms, and comparison to international data protection standards; analysis of STC''s commercial data products from Syrian markets',
    'If comprehensive data extraction: suppression and extractiveness scores increase toward 0.70+. If limited (by Syrian law or technical controls): both metrics decrease. This determines whether the constraint is primarily physical infrastructure extraction or digital surveillance capitalism extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_sovereignty_extraction, empirical, 'Data sovereignty and extraction through SilkLink').

omega_variable(
    saudi_state_backend_enforcement,
    'Does the Saudi state directly enforce STC''s monopoly position through diplomatic, economic, or military pressure on Syria?',
    'Diplomatic cable leaks, public statements by Syrian and Saudi officials, pattern analysis of STC policy enforcement, and third-party reports on Saudi leverage in Syria',
    'If yes: requires_active_enforcement is confirmed; suppression increases. If no or minimal: STC monopoly is primarily economic, not state-backed; suppression and active enforcement flags may lower. This determines whether the constraint is a corporate extraction mechanism or state coercion apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(saudi_state_backend_enforcement, empirical, 'Saudi state direct enforcement of STC monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silklink_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silk_tr_t0, silklink_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(silk_tr_t6, silklink_2026, theater_ratio, 6, 0.52).
narrative_ontology:measurement(silk_tr_t12, silklink_2026, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(silk_be_t0, silklink_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(silk_be_t6, silklink_2026, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(silk_be_t12, silklink_2026, base_extractiveness, 12, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silklink_2026, global_infrastructure).
narrative_ontology:affects_constraint(silklink_2026, saudi_economic_dominance_region).
narrative_ontology:affects_constraint(silklink_2026, syrian_state_capacity_recovery).
narrative_ontology:affects_constraint(silklink_2026, middle_east_telecom_competition).

% DUAL FORMULATION NOTE:
% The SilkLink constraint can be decomposed into distinct structural claims: (1) telecom infrastructure coordination in capital-constrained region (ε ≈ 0.15, Rope), (2) STC monopoly extraction through pricing/data (ε ≈ 0.55, Snare/Tangled Rope), (3) geopolitical leverage mechanism for Saudi interests (ε ≈ 0.65, Snare). The current story treats the hybrid as a single constraint (Tangled Rope, ε=0.55) because the infrastructure and extraction are inseparable — you cannot extract without the infrastructure, and the infrastructure in this geopolitical context inherently enables extraction. Alternative decomposition would require tracking separate extraction mechanisms (economic vs. geopolitical), which may be justified in future analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(silklink_2026, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
