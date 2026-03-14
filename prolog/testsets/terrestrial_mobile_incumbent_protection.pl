% ============================================================================
% CONSTRAINT STORY: terrestrial_mobile_incumbent_protection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_terrestrial_mobile_incumbent_protection, []).

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
 *   constraint_id: terrestrial_mobile_incumbent_protection
 *   human_readable: Terrestrial Mobile Incumbent Protection via Regulatory Capture
 *   domain: telecommunications/regulatory_capture
 *
 * SUMMARY:
 *   Terrestrial mobile incumbent protection via regulatory capture represents
 *   a structurally rich constraint that exhibits the full range of DR
 *   classification types. The US Federal Communications Commission maintains
 *   a spectrum licensing and auction system that formally presents itself as
 *   competitive but structurally advantages the three dominant carriers
 *   (AT&T, Verizon, T-Mobile). This constraint combines genuine coordination
 *   functions (spectrum allocation requires rule-based delineation; network
 *   standardization enables interoperability) with asymmetric extraction
 *   (incumbents capture rent through pricing, spectrum hoarding, and barrier
 *   creation). The constraint's theater ratio (0.55) reflects that FCC
 *   auction rules and competitive frameworks are performative: auctions are
 *   designed to appear competitive while preserving incumbent market share
 *   through rules that favor leveraged bidders. The extractiveness trajectory
 *   (0.35 → 0.58 over a decade) shows how regulatory protections have
 *   intensified as spectrum scarcity has increased and incumbents have
 *   invested heavily in lobbying.
 *
 * KEY AGENTS:
 *   - Incumbent Carriers (AT&T, Verizon, T-Mobile): Primary beneficiary (institutional/arbitrage) — capture spectrum rents, set pricing, control infrastructure access
 *   - New Market Entrants (failed attempts: MetroPCS pre-acquisition, Lightsquared, etc.): Primary victim (powerless/trapped) — face multi-billion-dollar barriers, regulatory approval delays, spectrum cost premiums
 *   - Spectrum Efficiency (collective good): Victim (powerless/trapped) — incumbents hold spectrum without fully utilizing it (hoarding dynamic); spectrum is scarcer than it should be
 *   - Consumer Choice (collective good): Victim (powerless/trapped) — duopoly/oligopoly pricing; reduced service quality innovation due to incumbent complacency
 *   - International Competitors (EU/Asian carriers): Secondary victim (moderate/constrained) — have alternatives (exit to other jurisdictions) but face extraction in US market
 *   - Device Manufacturers (Apple, Qualcomm): Secondary agent (moderate/constrained) — benefit from incumbent infrastructure but face licensing fees and carrier control
 *   - Open Spectrum Coalition (NGO advocates): Organized agent (organized/constrained) — sees incumbent protection as temporary problem with policy solutions
 *   - FCC Regulatory Agency: Institutional actor (institutional/mobile) — maintains performative framework; sees own mandate as degraded but continues ritual
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terrestrial_mobile_incumbent_protection, 0.58).
domain_priors:suppression_score(terrestrial_mobile_incumbent_protection, 0.68).
domain_priors:theater_ratio(terrestrial_mobile_incumbent_protection, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terrestrial_mobile_incumbent_protection, extractiveness, 0.58).
narrative_ontology:constraint_metric(terrestrial_mobile_incumbent_protection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(terrestrial_mobile_incumbent_protection, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(terrestrial_mobile_incumbent_protection, tangled_rope).
narrative_ontology:human_readable(terrestrial_mobile_incumbent_protection, "Terrestrial Mobile Incumbent Protection via Regulatory Capture").
narrative_ontology:topic_domain(terrestrial_mobile_incumbent_protection, "telecommunications/regulatory_capture").

domain_priors:requires_active_enforcement(terrestrial_mobile_incumbent_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(terrestrial_mobile_incumbent_protection, incumbent_carriers).
narrative_ontology:constraint_victim(terrestrial_mobile_incumbent_protection, new_market_entrants).
narrative_ontology:constraint_victim(terrestrial_mobile_incumbent_protection, spectrum_efficiency).
narrative_ontology:constraint_victim(terrestrial_mobile_incumbent_protection, consumer_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL MARKET ENTRANT (SNARE) — New carriers face multi-billion-dollar licensing fees, spectrum auctions designed to preserve incumbent market share, infrastructure deployment barriers (tower access, right-of-way), and regulatory approval delays lasting 3-7 years. No meaningful exit option exists short of abandoning entry entirely. Maximum experienced extraction: must pay oligopolistic rents to use spectrum incumbents control.
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERNATIONAL COMPETITOR (TANGLED ROPE) — Foreign carriers have alternative markets where they can invest and scale. They benefit from fragmented regulatory regimes (can forum-shop between jurisdictions) but face significant entry barriers in the US market. Exit cost is high (capital write-off) but not insurmountable (relocation to EU, Asia). Experiences mixed coordination (spectrum standardization enables global operations) and asymmetric extraction (must pay US incumbent premium to access US customers).
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT CARRIER (ROPE) — Experiences the constraint as coordination: spectrum licenses, tower infrastructure, equipment standards are genuine coordination goods. The regulatory framework coordinates 50+ state actors, multiple frequency bands, and international standards. Incumbents see themselves as solving coordination problems; their arbitrage option (relocate operations to another geography) is theoretically available but practically irrelevant due to network effects and sunk capital. Net beneficiary of the constraint.
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPEN SPECTRUM COALITION (SCAFFOLD) — Organized advocates (Public Knowledge, Citizens Communications) see incumbent protection as a temporary market structure flaw. They push for spectrum auctions that favor small carriers, dynamic spectrum sharing, and unlicensed band expansion. These reforms have a visible sunset: once dynamic spectrum and open protocols mature (estimated 10-15 years), spectrum access becomes less scarce, incumbent moat weakens. Current extraction is tolerable because the coalition perceives a clear policy pathway to exit.
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCY (FCC) (PITON) — The FCC maintains spectrum auction processes and licensing frameworks that formally appear competitive but structurally advantage incumbents. The rules are theater: bidding processes allow incumbents to leverage sunk capital and superior financing; rules that appear pro-competition (net neutrality, tower access) coexist with rules that entrench incumbents (spectrum concentration limits that grandfathered existing holdings). The FCC sees its own mandate as degraded — balancing competition and investment incentives — and maintains the ritual framework through bureaucratic inertia.
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: DEVICE MANUFACTURER (TANGLED ROPE) — Manufacturers benefit from spectrum standards and large installed base created by incumbents (coordination function). But they also face extraction: licensing fees for patent pools, reduced negotiating power when carriers control network access (content throttling, zero-rating). Exit cost is moderate (can sell globally but lose US market share); exit is constrained by network effects, not by legal barriers. Genuine hybrid: coordination + asymmetric extraction.
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a physics/information-theory perspective, spectrum is inherently scarce (electromagnetic property), and some allocation mechanism must exist. Interference between transmitters is a physical law. Coordination costs (licensing, delineation of frequency bands) are inherent to any multi-user spectrum system. From this perspective, incumbent protection appears as an immutable law: scarcity requires allocation, allocation requires rules, rules require coordination, coordination requires someone to benefit. But this naturalizes what is actually a contingent institutional arrangement: spectrum scarcity is real, but allocation via oligopolistic auction (vs. dynamic spectrum sharing, unlicensed bands, or commons-based approaches) is a policy choice.
constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terrestrial_mobile_incumbent_protection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terrestrial_mobile_incumbent_protection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(terrestrial_mobile_incumbent_protection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(terrestrial_mobile_incumbent_protection, TR),
    TR >= 0.70.

:- end_tests(terrestrial_mobile_incumbent_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Incumbents capture substantial economic rent through spectrum control, infrastructure barriers, and regulatory pricing. The rent is not as extreme as pure monopoly (0.80+) because some new entry occurs (Metro by T-Mobile, MVNO agreements) and international competition constrains pricing at the margin. However, extractiveness has been rising (0.35 to 0.58 over a decade) as spectrum scarcity increased and incumbents consolidated lobbying power. Suppression (0.68): High. Significant barriers include: (1) Spectrum licensing costs ($500M-$10B per major auction), (2) Infrastructure deployment (tower access, right-of-way negotiations), (3) Regulatory approval timelines (2-7 years), (4) Equipment standardization costs (must support multiple bands), (5) Financing barriers (risk premium for new entrants), (6) Regulatory capture (rules designed to favor incumbents). The suppression is structural (some barriers are real costs of coordination) and institutional (some are rules that could be changed). Theater ratio (0.55): Moderate-high. FCC competitive frameworks are substantially performative: auction rules appear competitive but use bidding structures that favor leveraged incumbents, spectrum concentration caps are grandfathered (incumbent holdings exempt), competitive bidding requirements coexist with facilities-based exemptions (incumbents use themselves as 'competitors'), and investment commitments are monitored without binding enforcement. The theater has increased as auctions have become more complex and rule-making more technical, making competitive intent harder to verify.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The incumbent carrier sees pure coordination (Rope) — spectrum allocation is a genuine problem they are helping solve. The open spectrum coalition sees a temporary problem with policy solutions (Scaffold) — unlicensed spectrum expansion and dynamic sharing will eventually replace the incumbent moat. The FCC sees its own degraded ritual (Piton) — competitive frameworks that no longer deliver competition, maintained through bureaucratic inertia. The entrant sees pure extraction (Snare) — no meaningful exit option exists. The device manufacturer sees mixed coordination and extraction (Tangled Rope) — real benefits from standardization, real costs from carrier gatekeeping. The international competitor sees mixed extraction and exit options (Tangled Rope) — significant extraction in the US market but ability to relocate globally. The civilizational analytical observer risks seeing spectrum scarcity as natural law (Mountain) — but the structural data reveals this as a false summit: scarcity is physical reality, but allocation via oligopolistic auction (vs. commons-based spectrum, dynamic sharing, unlicensed bands) is a policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain operates as follows: Incumbent carriers hold beneficiary status + arbitrage-level exit options (can theoretically relocate but practically cannot due to sunk capital and network effects). Derived d ≈ 0.05-0.15 (full beneficiary range). Entrants hold victim status + trapped exit options (multi-billion-dollar sunk cost barriers). Derived d ≈ 0.90-0.98 (full target range). International competitors hold victim status (in US market) + constrained exit (can relocate to EU/Asia, but at high cost). Derived d ≈ 0.65-0.75 (moderate-to-high target range). Device manufacturers hold mixed status (beneficiary from infrastructure standardization, victim from carrier pricing power) + constrained exit (cannot abandon US market). Derived d ≈ 0.55-0.60 (mixed range). The FCC as institutional actor with mobile exit (could theoretically apply rules differently but faces political capture and institutional inertia) derives d ≈ 0.25-0.35 (moderate beneficiary: maintains the status quo).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that incumbent protection is genuinely both coordination AND extraction. The coordination function is real: spectrum allocation does require rules, delineation, and some form of delegation. Network standardization does create efficiency gains. Infrastructure investment does require some predictability and profit incentive. The extraction is also real: incumbents capture far more economic rent than necessary to provide these coordination functions. The constraint could be restructured with lower extractiveness (unlicensed spectrum, dynamic sharing, reduced auction costs) without losing coordination benefits. The mandatrophy is resolved by recognizing that the constraint belongs to the tangled_rope category precisely because both functions are genuine, neither can be removed without destroying the constraint entirely, and the task of governance is to shift the ratio: maximize coordination, minimize unnecessary extraction. This is not a false positive where a name-only extractive constraint hides as coordination, nor a false negative where genuine coordination is misread as pure extraction. The benchmark is not perfect fairness but structural functionality: can the constraint be redesigned to deliver equivalent or better coordination with lower asymmetric extraction? Yes — scaffold technologies (dynamic spectrum, unlicensed bands, commons protocols) suggest that extractiveness could be reduced to 0.25-0.35 without eliminating coordination. This confirms that current extractiveness (0.58) includes policy-contingent rent (0.25-0.33 reducible range) not structural necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spectrum_commons_viability,
    'Can dynamic spectrum sharing and unlicensed band protocols achieve capacity and quality-of-service parity with licensed incumbent networks within a 10-15 year horizon?',
    'Empirical testing of dynamic spectrum protocols (TV white space, CBRS, future 6GHz unlicensed); comparative analysis of capacity, latency, reliability across licensed vs unlicensed deployments; adoption data for commons-based spectrum',
    'If viable: the scaffold perspective is materially correct, extractiveness drops to 0.25-0.35 (transition to pure coordination), and the constraint has a genuine sunset. If not viable: incumbent protection is structurally necessary, the snare classification becomes mountain (scarcity constraint), and extractiveness remains high indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectrum_commons_viability, empirical, 'Viability of dynamic spectrum sharing as incumbent moat replacement').

omega_variable(
    regulatory_capture_mechanism,
    'Is incumbent protection primarily a capture mechanism (regulators captured by incumbent lobbying) or a structural consequence (scarcity requires allocation, allocation requires delegation to efficient actors)?',
    'Comparative regulatory analysis across jurisdictions with different capture intensity (US vs. EU vs. India); analysis of auction rules before and after lobbying pressure; measurement of regulatory distance between legislated intent and implemented rules',
    'If capture: the constraint''s extractiveness is partly a function of incumbent power, reforms addressing capture could lower extractiveness to 0.35-0.45. If structural: extractiveness reflects genuine scarcity costs, and reforms would shift the burden without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether incumbent protection is regulatory capture or structural necessity').

omega_variable(
    sunk_capital_lock_in,
    'How much of incumbent moat strength is due to sunk infrastructure capital (towers, spectrum, equipment) vs. regulatory rules (licensing requirements, auction design)?',
    'Counterfactual analysis: estimate entrant competitive position under different regulatory rules but same sunk capital distribution; analyze historical entry periods with less regulatory protection',
    'If primarily sunk capital: reforms cannot eliminate moat (physical reality), but can reduce regulatory premium; extractiveness floor remains at 0.35-0.40. If primarily regulatory: reforms could significantly weaken moat; extractiveness could drop to 0.20-0.30.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunk_capital_lock_in, empirical, 'Sunk capital vs regulatory rule contribution to incumbent moat').

omega_variable(
    investment_incentive_dependency,
    'Would reducing incumbent protection (lowering extractiveness, increasing entry) materially harm network investment and coverage expansion?',
    'Comparison of investment rates and coverage expansion across jurisdictions with different incumbent protection regimes (US high-protection vs. EU medium-protection vs. India low-protection); time-series analysis of investment before/after major reforms',
    'If investment dependent on high margins: reforms must be gradual (scaffold rather than sharp sunset) to avoid network degradation. If investment is robust to lower margins: reforms can proceed faster, confirming scaffold sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_incentive_dependency, empirical, 'Network investment dependency on incumbent margin preservation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terrestrial_mobile_incumbent_protection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tmip_tr_t0, terrestrial_mobile_incumbent_protection, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tmip_tr_t5, terrestrial_mobile_incumbent_protection, theater_ratio, 5, 0.48).
narrative_ontology:measurement(tmip_tr_t10, terrestrial_mobile_incumbent_protection, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(tmip_be_t0, terrestrial_mobile_incumbent_protection, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tmip_be_t5, terrestrial_mobile_incumbent_protection, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(tmip_be_t10, terrestrial_mobile_incumbent_protection, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terrestrial_mobile_incumbent_protection, global_infrastructure).
narrative_ontology:affects_constraint(terrestrial_mobile_incumbent_protection, spectrum_scarcity).
narrative_ontology:affects_constraint(terrestrial_mobile_incumbent_protection, net_neutrality_enforcement).
narrative_ontology:affects_constraint(terrestrial_mobile_incumbent_protection, tower_access_regulation).

% DUAL FORMULATION NOTE:
% Incumbent protection is downstream of spectrum scarcity (physical constraint) but represents a distinct institutional/regulatory constraint. The upstream scarcity constraint has ε ≈ 0.10 (mountain: electromagnetic interference is inherent). The incumbent protection constraint has ε ≈ 0.58 (tangled_rope: the *allocation mechanism* for scarce spectrum is policy-contingent). Decomposition clarifies that scarcity is inevitable; protectionism is not.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(terrestrial_mobile_incumbent_protection, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
