% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary — Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint models the 'embedded liberalism' reading of NAFTA's
 *   jurisdictional boundary (Chapter 11 investment + Chapter 19 dispute
 *   settlement + side agreements on labor/environment). The reading holds
 *   that trade agreement text creates a framework for market access while
 *   preserving legitimate domestic policy space for environmental, labor, and
 *   health regulation — provided measures are non-discriminatory and not
 *   disguised restrictions. The structural reality: partial jurisdictional
 *   overlap where regulatory agencies retain defensive authority within a
 *   contested 'legitimate objectives' boundary, but face moderate extraction
 *   through ISDS litigation costs and regulatory chill. This is one reading
 *   of a three-way kernel contest.
 *
 * KEY AGENTS:
 *   - export_oriented_corporations: Primary beneficiary (institutional/arbitrage) — gain market access certainty and ISDS enforcement
 *   - trade_lawyers_arbitrators: Secondary beneficiary (organized/mobile) — capture recurring dispute resolution rents
 *   - investment_treaty_practitioners: Secondary beneficiary (professional/mobile) — career and revenue stream from treaty interpretation
 *   - domestic_environmental_regulators: Primary target (organized/constrained) — bear litigation costs and regulatory chill
 *   - labor_standards_enforcement_agencies: Primary target (organized/constrained) — face ISDS challenges to labor protections
 *   - subnational_governments_states_provinces: Target (powerless/constrained) — bound by federal treaty but lack voice in negotiation
 *   - affected_communities_near_pollution_sources: Target (powerless/trapped) — bear environmental harm when regulation is chilled
 *   - arbitral_tribunals: Agenda_setter (institutional/analytical) — adjudicate the legitimate objectives boundary
 *   - domestic_courts: Observer (institutional/analytical) — apply treaty as supreme law per constitutional hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.38).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary — Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '48197fd7-c1be-4e16-817d-391d5fad1ec0').
narrative_ontology:cs_kernel_codification('48197fd7-c1be-4e16-817d-391d5fad1ec0', formalized).
narrative_ontology:cs_authority_grounding('48197fd7-c1be-4e16-817d-391d5fad1ec0', lineage).
narrative_ontology:cs_interpretation_layer_present('48197fd7-c1be-4e16-817d-391d5fad1ec0').
narrative_ontology:cs_reading_relation('48197fd7-c1be-4e16-817d-391d5fad1ec0', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('48197fd7-c1be-4e16-817d-391d5fad1ec0', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('48197fd7-c1be-4e16-817d-391d5fad1ec0', foundational, legitimate_objectives_exception_preserves_regulatory_space).
narrative_ontology:cs_axiom_status(legitimate_objectives_exception_preserves_regulatory_space, holdable).
narrative_ontology:cs_axiom_grounding('48197fd7-c1be-4e16-817d-391d5fad1ec0', legitimate_objectives_exception_preserves_regulatory_space, conventional).
narrative_ontology:cs_axiom('48197fd7-c1be-4e16-817d-391d5fad1ec0', foundational, non_discrimination_as_boundary_condition).
narrative_ontology:cs_axiom_status(non_discrimination_as_boundary_condition, holdable).
narrative_ontology:cs_axiom_grounding('48197fd7-c1be-4e16-817d-391d5fad1ec0', non_discrimination_as_boundary_condition, conventional).
narrative_ontology:cs_reference_frame('48197fd7-c1be-4e16-817d-391d5fad1ec0', embedded_liberalism_compromise_1947).
narrative_ontology:cs_drift_state('48197fd7-c1be-4e16-817d-391d5fad1ec0', post_nafta_chapter11_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('48197fd7-c1be-4e16-817d-391d5fad1ec0', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_lawyers_arbitrators).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, investment_treaty_practitioners).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_environmental_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_standards_enforcement_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, subnational_governments_states_provinces).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, affected_communities_near_pollution_sources).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_principle).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_autonomy_within_legitimate_objectives).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, embedded_liberalism_compromise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Multinational firms in manufacturing, energy, mining, and services that use NAFTA Chapter 11 to lock in market access and challenge environmental, health, and zoning regulations that affect expected profits. They initiate ISDS claims when domestic regulation reduces investment value. Their capital mobility gives them arbitrage-grade exit — they can relocate production, shift supply chains, or threaten to do so. They capture the primary gains: stable investment rules, compensation for regulatory change, and deterrence of future regulation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% The specialized bar and arbitrator corps that has grown around investment treaty arbitration. They earn fees from representing investors and states, and arbitrators are paid per case. Their professional identity and revenue depend on the ISDS system's continuation and expansion. They have mobile exit — they can shift to other treaty regimes (ICSID, ICC, UNCITRAL) or commercial arbitration — but their specialized capital is tied to the investment treaty regime.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_lawyers_arbitrators, beneficiary,
    organized, biographical, mobile, global).

% Academics, consultants, NGO specialists, and government legal advisors whose careers revolve around investment treaty interpretation. They benefit from the regime's complexity and contestation — more disputes mean more demand for expertise. Mobile exit within the international law field, but their specific human capital is regime-specific.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, investment_treaty_practitioners, beneficiary,
    organized, biographical, mobile, global).

% Federal and state/provincial agencies (EPA, Environment Canada, SEMARNAT, state DEQs) that must defend environmental regulations against ISDS claims. They bear the direct litigation costs (millions per case), the opportunity cost of diverted enforcement resources, and the regulatory chill — measures not proposed or weakened due to ISDS risk. Their exit is constrained: they are legally bound to implement treaty obligations and cannot opt out of defending challenged regulations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_environmental_regulators, payer,
    organized, biographical, constrained, national).

% Agencies enforcing labor standards (OSHA, NLRB, Canadian provincial labour ministries, Mexican STPS) that face ISDS challenges when labor protections affect investor returns. Same structural position as environmental regulators: bear defense costs, experience chill, constrained exit. The NAALC side agreement provides a weaker, state-to-state track that does not displace ISDS exposure.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_standards_enforcement_agencies, payer,
    organized, biographical, constrained, national).

% U.S. states, Canadian provinces, Mexican states bound by NAFTA obligations negotiated at federal level. They have no formal standing in ISDS proceedings (which are federal-state) but their laws are challenged and they bear implementation costs. Their exit is constrained — they cannot withdraw from the treaty and have limited influence on federal trade policy. They are the 'forgotten federalism' layer in the jurisdictional boundary.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, subnational_governments_states_provinces, payer,
    powerless, biographical, constrained, regional).

% Communities (often Indigenous, low-income, communities of color) located near industrial facilities whose pollution is regulated by the very measures vulnerable to ISDS challenge. When regulation is chilled or rolled back due to investment treaty risk, they bear the health and environmental costs directly. Their exit is trapped — they cannot relocate easily, and their political voice is minimal in treaty interpretation. They are not parties to the treaty but are structurally positioned as its ultimate victims.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, affected_communities_near_pollution_sources, payer,
    powerless, biographical, trapped, local).

% Ad hoc tribunals (ICSID, UNCITRAL, NAFTA Chapter 11) that interpret the treaty's jurisdictional boundary — defining 'investment', 'expropriation', 'fair and equitable treatment', and the 'legitimate objectives' / 'non-discriminatory' exceptions. Their rulings create the de facto boundary. They are not elected, not structurally accountable to affected communities, and their reasoning becomes precedent for future tribunals. They set the agenda for what counts as legitimate regulation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, arbitral_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% National supreme courts (U.S. Supreme Court, Supreme Court of Canada, Mexican SCJN) that apply the treaty as supreme law under their constitutional hierarchies. They review domestic implementing legislation and occasionally ISDS awards for enforcement. They observe the jurisdictional boundary from within the domestic legal order but cannot modify the treaty itself. Their role is interpretive and enforcement-oriented, not agenda-setting for the treaty's evolution.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a predictable, rules-based framework for cross-border investment and trade across three asymmetric economies, reducing the risk of arbitrary expropriation and discriminatory treatment. Provides a dispute resolution mechanism (ISDS) that substitutes for diplomatic protection and domestic court bias. The side agreements (NAALC, NAAEC) create parallel cooperation tracks for labor and environment.
% TRANSFER_FUNCTION: Moves litigation costs, regulatory risk, and occasional damages awards from domestic regulators (and ultimately taxpayers and affected communities) to investors and their legal representatives. Moves regulatory autonomy at the margin from subnational and national governments to arbitral tribunals. The transfer is asymmetric: investors gain enforceable rights; regulators bear defense costs and chill.
% ABSENT_VOICES: Affected communities (especially Indigenous and environmental justice communities) are structurally excluded from ISDS proceedings — no standing, no notice, no participation. Subnational governments have no formal voice in treaty interpretation despite bearing implementation costs. Future generations who inherit the regulatory constraints locked in by treaty precedent. These voices would object to the asymmetry but are not in the room.
% DISAPPEARANCE_RATIONALE: If the embedded liberalism reading vanished overnight, the jurisdictional boundary would be contested outright between capital_supremacy and sovereignty_primacy readings. Investors would lose the 'legitimate objectives' defense framework but keep ISDS access; regulators would lose the non-discrimination safe harbor but gain political space to assert sovereignty. The USMCA renegotiation shows the world rearranges: Chapter 11 gutted for US-Canada, reformed for US-Mexico, new labor/environment enforcement. The constraint's disappearance triggers institutional reorganization.
% FOUNDING_PROBLEM: Post-WWII trade regime needed to reconcile two imperatives: (1) open markets and investment flows for growth, and (2) domestic policy space for full employment, social protection, and environmental stewardship. The embedded liberalism compromise (Ruggie 1982) was the answer: international rules for market access, domestic autonomy for legitimate regulation. NAFTA (1994) extended this to investment via Chapter 11, with side agreements for labor/environment.
% FOUNDING_PROBLEM_CORROBORATION: The embedded liberalism framing is corroborated by Ruggie (1982), Kahler (2013), and the original NAFTA negotiating record (USTR archives, Canadian and Mexican government documents). The 'contested' status is corroborated by: (a) investor-state claimants and tribunals who read the treaty as stronger investor protection (capital_supremacy Reading); (b) labor/environmental NGOs and Global South governments who read it as sovereignty erosion (sovereignty_primacy Reading); (c) the USMCA renegotiation itself, which restructured the bargain because the founding problem's balance had broken down. No single party's self-assertion settles it.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the real but moderate transfer: investors gain enforceable expectations and occasional damages; regulators lose autonomy at the margin and bear defense costs. Suppression (0.38) is moderate — the constraint does not forbid regulation outright but raises its cost and creates chill. Theater ratio (0.28) captures the growing performative invocation of 'legitimate objectives' while the boundary contracts in practice. Accessibility collapse (0.45) is partial — alternatives (carve-outs, renegotiation, withdrawal) exist but are politically costly. Resistance (0.55) is significant — states have pushed back via renegotiation (USMCA), withdrawal threats, and carve-out proposals. The claimed_type tangled_rope fits: genuine coordination (market access framework) + asymmetric extraction (ISDS costs borne by regulators, benefits captured by investors).
 *
 * PERSPECTIVAL GAP:
 *   From the investor/arbitrator seat: the constraint is a rope — predictable rules, mutual gains from trade, legitimate disputes resolved neutrally. From the regulator/community seat: the constraint is a snare — asymmetric exposure, regulatory chill, extraction via litigation asymmetry. From the analytical seat: the constraint is a tangled_rope — both coordination and extraction are structurally real and inseparable in the same mechanism. The engine computes this divergence from the declared beneficiaries/victims and their exit/power profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented corporations and investment treaty practitioners are declared beneficiaries — they collect the gains of market access certainty and dispute resolution rents. Their exit options are arbitrage/mobile (capital can relocate, practitioners can shift forums). Domestic regulators, labor agencies, subnational governments, and affected communities are declared victims — they bear the litigation costs, regulatory chill, and environmental/health harms when regulation is deterred. Their exit options are constrained (bound by treaty) to trapped (communities cannot exit the pollution). Arbitral tribunals are agenda_setters — they define the legitimate objectives boundary. The directionality derivation from these structural positions yields the per-seat χ amplification the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII trade regime balancing market access with domestic policy space) remains live but contested. The constraint has not resolved its mandatrophy — the coordination function (trade liberalization) persists but the extraction function (ISDS asymmetry) has grown. The embedded liberalism reading itself is the attempted resolution: rebalancing toward coordination. Whether it succeeds is the contested_status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is this constraint a distinct reading of the nafta_jurisdictional_boundary kernel, or a separate constraint with its own ε?',
    'Test ε-invariance: if measuring the constraint via investor-state dispute outcomes vs. regulatory autonomy preservation yields different ε, the readings are structurally distinct constraints. The ε-invariance principle requires separate stories for each reading with distinct ε values.',
    'If this is one reading of a contested kernel, the other readings (capital_supremacy_reading, sovereignty_primacy_reading) must be authored as separate constraint stories with their own ε values and linked via network.affects_constraints. If it is a standalone constraint, the kernel framing is analytical overlay only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the embedded liberalism reading is a distinct constraint instantiation from the kernel').

omega_variable(
    legitimate_objectives_boundary_ambiguity,
    'Where does the ''legitimate objectives'' boundary actually lie — is it a stable coordination line or an expanding extraction zone?',
    'Track arbitration outcomes over time: if the scope of measures deemed ''necessary'' or ''non-discriminatory'' contracts while investor claims expand, the boundary functions as extraction. If the boundary holds and regulatory space is genuinely preserved, it functions as coordination.',
    'Boundary stability determines whether the constraint is genuinely tangled_rope (coordination + extraction) or drifts toward snare (extraction masked by coordination language). Affects claimed_type validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_ambiguity, empirical, 'Whether the legitimate objectives exception functions as genuine regulatory space or extraction cover').

omega_variable(
    litigation_cost_as_extraction_vector,
    'Is the moderate extraction through litigation costs a feature of genuine dispute resolution or a structural barrier that chills legitimate regulation?',
    'Compare regulatory chill metrics (measures not enacted due to ISDS threat) against actual dispute filings. If chill exceeds filings by wide margin, litigation cost is an extraction vector, not a coordination cost.',
    'If litigation costs primarily chill regulation rather than resolve disputes, the constraint''s extraction is higher than the dispute count suggests — effective extraction amplified for regulators who face asymmetric cost of defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_cost_as_extraction_vector, empirical, 'Whether ISDS litigation costs function as regulatory chill mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_el_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(nafta_el_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(nafta_el_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(nafta_el_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(nafta_el_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(nafta_el_tr_t2023, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(nafta_el_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.25).
narrative_ontology:measurement(nafta_el_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(nafta_el_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.36).
narrative_ontology:measurement(nafta_el_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.39).
narrative_ontology:measurement(nafta_el_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.41).
narrative_ontology:measurement(nafta_el_be_t2023, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nafta_el_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.22).
narrative_ontology:measurement(nafta_el_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(nafta_el_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.33).
narrative_ontology:measurement(nafta_el_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(nafta_el_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement(nafta_el_su_t2023, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2023, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.12).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, usmca_investment_chapter_reform).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, icsid_reform_process).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, ceta_investment_court_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nafta_jurisdictional_boundary kernel. The capital_supremacy_reading has higher ε (stronger investor rights, weaker regulatory space). The sovereignty_primacy_reading has lower ε (stronger regulatory space, weaker investor enforcement). All three share the same treaty text but instantiate different constraints with different beneficiary/victim structures and extraction profiles. The embedded_liberalism_reading is the negotiated middle — the only one that acknowledges both coordination and extraction as structurally real.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, institutional, 0.35).
constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, organized, 0.65).
constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
