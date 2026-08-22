% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading (Policy Space, S&D, Technology Transfer as Core Commitments)
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   This story instantiates the developmental reading of the WTO treaty
 *   framework kernel: policy space for development, special-and-differential
 *   (S&D) treatment, and technology transfer obligations are read as
 *   permanent, equal-status structural commitments recognizing that member
 *   states entered the multilateral trading system from radically asymmetric
 *   starting conditions — not as temporary transitional exceptions to a
 *   fundamentally symmetric liberalization obligation. Under this reading,
 *   tariff flexibility, subsidy space, and compulsory licensing authority for
 *   developing and least-developed members are core treaty commitments, and
 *   multinational IP holders' exclusivity is legitimately subordinate to
 *   development policy space. The sibling market_access_reading (a separate
 *   constraint) reads the same textual kernel as establishing trade
 *   liberalization as the primary, symmetric, universal obligation, with S&D
 *   as time-limited transitional accommodation destined to sunset as states
 *   graduate. The two readings diverge sharply on ε: this developmental
 *   reading holds a moderate ε (0.38) reflecting real, contested extraction
 *   from IP holders and export sectors through enforced technology transfer
 *   and tolerated policy space, while a market-access reading of the same
 *   text would likely show a very different extraction profile centered on
 *   developing-state market barriers as the extractive object. These are not
 *   two measurements of one constraint — they are two different constraints
 *   sharing a textual kernel, linked structurally rather than merged.
 *
 * KEY AGENTS:
 *   - global_south_member_states: Primary beneficiary/agenda-setter (moderate/constrained) — invokes and defends S&D and technology transfer as permanent commitments
 *   - least_developed_countries: Most dependent beneficiary (powerless/trapped) — relies on flexibilities it has least capacity to enforce
 *   - multinational_ip_rights_holders: Primary payer (organized/constrained) — bears compulsory licensing and technology transfer costs
 *   - developed_state_export_sectors: Secondary payer (powerful/mobile) — absorbs continued market access barriers legitimated by this reading
 *   - wto_dispute_settlement_body: Agenda-setting adjudicator (institutional/analytical) — determines whether the accommodation is enforceable in practice
 *   - developed_state_governments: Excluded from this reading's frame (institutional/mobile) — hold the sibling market-access reading, appearing here only as resistance pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.38).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.42).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading (Policy Space, S&D, Technology Transfer as Core Commitments)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, 'a543a23f-08c8-4e9a-8d30-614f36946fe9').
narrative_ontology:cs_kernel_codification('a543a23f-08c8-4e9a-8d30-614f36946fe9', fixed_text).
narrative_ontology:cs_authority_grounding('a543a23f-08c8-4e9a-8d30-614f36946fe9', distributed).
narrative_ontology:cs_reading_relation('a543a23f-08c8-4e9a-8d30-614f36946fe9', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('a543a23f-08c8-4e9a-8d30-614f36946fe9', foundational, asymmetric_starting_conditions_warrant_permanent_accommodation).
narrative_ontology:cs_axiom_status(asymmetric_starting_conditions_warrant_permanent_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('a543a23f-08c8-4e9a-8d30-614f36946fe9', asymmetric_starting_conditions_warrant_permanent_accommodation, empirically_contingent).
narrative_ontology:cs_axiom('a543a23f-08c8-4e9a-8d30-614f36946fe9', secondary, ip_exclusivity_subordinate_to_development_policy_space).
narrative_ontology:cs_axiom_status(ip_exclusivity_subordinate_to_development_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('a543a23f-08c8-4e9a-8d30-614f36946fe9', ip_exclusivity_subordinate_to_development_policy_space, instrumental).
narrative_ontology:cs_reference_frame('a543a23f-08c8-4e9a-8d30-614f36946fe9', gatt_special_and_differential_treatment_origins).
narrative_ontology:cs_drift_state('a543a23f-08c8-4e9a-8d30-614f36946fe9', post_appellate_body_collapse, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a543a23f-08c8-4e9a-8d30-614f36946fe9', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, global_south_member_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_state_export_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_state_governments).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, special_and_differential_treatment_doctrine).
narrative_ontology:constraint_vindicates(wto_treaty_framework__developmental_reading, common_but_differentiated_responsibility_in_trade).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and invoke S&D provisions to retain tariff flexibility, subsidy space, and compulsory licensing authority they read as permanent structural accommodation for asymmetric starting conditions, not temporary charity. They push in committee and dispute settlement to keep policy space open and to enforce technology transfer commitments against reluctant counterparties. Exit from the WTO framework entirely would cost market access, so their leverage is exercised inside the system rather than by threatening to leave it.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, global_south_member_states, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, global_south_member_states, agenda_setter).

% Depend most heavily on S&D flexibilities and technology transfer commitments because they lack the administrative and industrial capacity to compete on symmetric terms. They have the least capacity to litigate disputes or monitor compliance by developed-country counterparties, so the accommodation exists on paper more reliably than it is enforced in their favor.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_countries, beneficiary,
    powerless, generational, trapped, global).

% Domestic manufacturing and agricultural sectors in developing states that the policy-space reading is designed to shield from premature full liberalization, giving them time and protected space to reach competitive scale before facing symmetric tariff and subsidy discipline.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, infant_industries, beneficiary,
    powerless, biographical, trapped, national).

% Hold patents and proprietary technology that compulsory licensing authority and technology transfer obligations require them to share or license below preferred market terms when a developing member invokes the flexibilities. They lobby home governments to narrow S&D scope and to treat these obligations as exceptional rather than core, since the developmental reading treats their exclusivity as subordinate to development policy space.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders, payer,
    organized, generational, constrained, global).

% Face continued tariff and non-tariff barriers in developing-country markets that, under the developmental reading, are treaty-legitimate rather than transitional violations to be phased out. They can diversify markets or shift production, giving them real if costly exit relative to the trapped beneficiary states.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_state_export_sectors, payer,
    powerful, biographical, mobile, global).

% Adjudicates disputes over whether specific measures fall within legitimate S&D policy space or violate underlying non-discrimination obligations. Its rulings determine in practice whether the developmental reading's promises are enforceable commitments or aspirational language, and its own authority has been degraded by the collapse of the Appellate Body, weakening enforcement of the very accommodations this reading treats as core.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Formally parties to the treaty but structurally resist the developmental reading in practice — pushing graduation criteria, contesting self-designation as developing, and slow-walking technology transfer commitments. Their preferred reading (market access as symmetric obligation) is excluded from this story's frame by construction, since this story instantiates only the developmental reading; their objections surface here as resistance pressure rather than as an alternative constraint.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_state_governments, excluded,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developed_state_governments, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__developmental_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_treaty_framework__developmental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared multilateral framework in which states at very different levels of industrial and institutional development can commit to trade rules without requiring immediate symmetric liberalization — allowing lower-capacity states to participate in and benefit from the trading system while building the capacity to compete on more equal terms later.
% TRANSFER_FUNCTION: Moves policy space (tariff flexibility, subsidy latitude, compulsory licensing authority) and technology (through transfer obligations) toward Global South states and away from the full market-access entitlements and IP exclusivity multinational rights holders and developed exporters would otherwise hold under a symmetric reading of the same treaty text.
% ABSENT_VOICES: Domestic constituencies within developed states who bear diffuse costs from constrained market access (import-competing consumers benefit, but export-sector workers and firms bear losses) rarely appear directly in WTO negotiating rooms, which are dominated by state delegations and organized industry lobbies; multinational rights holders are present but developing-country civil society and smallholder producers who would benefit from stronger technology transfer enforcement are typically absent from the rooms where compliance is actually negotiated.
% DISAPPEARANCE_RATIONALE: If the developmental reading's accommodations vanished overnight — S&D treated as fully expired, compulsory licensing authority withdrawn, technology transfer obligations voided — developing states would lose negotiated protections for infant industries and access to patented technology on preferential terms; several would face immediate exposure to disputes over subsidies and tariffs currently shielded, and industrial policy space that currently exists as treaty-protected would have to be renegotiated bilaterally from a weaker position or abandoned.
% FOUNDING_PROBLEM: Post-colonial and newly industrializing states entering GATT/WTO negotiations faced radically asymmetric starting conditions relative to established industrial economies — the founding problem was how to build a rules-based multilateral trading system without freezing that asymmetry permanently into treaty obligations that only the already-industrialized could meet.
% FOUNDING_PROBLEM_CORROBORATION: UNCTAD and G77 negotiating positions, along with a substantial development-economics literature (Chang, Wade, and others on 'kicking away the ladder'), corroborate that the asymmetric-starting-conditions problem remains live and that many currently industrialized states used exactly these tools during their own development. Developed-state trade ministries and industry associations, situated inside the benefiting-from-market-access camp rather than outside it, dispute that the problem remains live for states now classified as advanced developing economies — this dispute is the axis the market_access_reading occupies, and this story does not adjudicate it, only records that corroboration outside the developmental camp exists in the scholarly and G77 record.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).
:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38, rising slowly from 0.22 at founding) because the developmental reading's core mechanisms — tariff flexibility and compulsory licensing — represent real, bounded transfers of value and policy latitude away from IP holders and toward developing states, not unlimited extraction; the value transferred is significant but the flexibilities are themselves negotiated and periodically contested rather than open-ended. Suppression (0.42) reflects the enforcement machinery needed to make technology transfer obligations bite against resistant counterparties, but it sits well below levels seen in pure extraction constraints because member states retain real voice in negotiating rounds. Theater ratio rises over the interval (0.20 to 0.40) tracking the widely documented gap between S&D provisions as formally declared (over 150 provisions across WTO agreements) and S&D provisions as actually enforced or operationalized — much of the accommodation architecture functions more as declaratory commitment than binding, monitored obligation, especially after the Appellate Body's effective collapse in 2019 removed a key enforcement backstop.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South member states and least-developed countries are declared beneficiaries because the developmental reading's flexibilities exist structurally for their benefit — but their exit options differ sharply (constrained vs. trapped), which the engine's directionality derivation should reflect as LDCs sitting nearer full-beneficiary intensity of need despite having the least actual capacity to secure the benefit in practice. Multinational IP rights holders and developed export sectors are declared victims/payers because technology transfer obligations and tolerated policy space directly constrain their exclusivity and market access relative to a symmetric baseline — but their mobile/constrained exit options put real distance between them and a trapped target, which is why this reading's ε stays moderate rather than high.
 *
 * MANDATROPHY ANALYSIS:
 *   The developmental reading resists mislabeling as pure extraction because it retains a genuine, documented coordination function: without some accommodation for asymmetric starting conditions, the founding problem (bringing radically unequal economies into one rules-based system) has no solution other than either excluding low-capacity states or freezing them into permanent subordination — both of which the S&D architecture was built to avoid. It resists mislabeling as pure coordination (rope) because the technology transfer and compulsory licensing mechanisms impose real, identifiable costs on multinational rights holders that are not merely incidental — they are the deliberate mechanism by which the coordination is achieved, which is exactly the tangled_rope signature: genuine coordination function plus asymmetric extraction through the same structure, requiring active enforcement (dispute settlement, negotiating pressure) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_permanence_vs_transition_ambiguity,
    'Is S&D treatment a permanent structural accommodation recognizing irreducible asymmetry between member economies, or a temporary transitional mechanism whose proper endpoint is graduation to full symmetric obligations?',
    'This is the central live dispute between the developmental_reading and the market_access_reading of the same kernel; it is not resolvable by data internal to either reading, since each reading''s own framework treats the answer as definitionally settled. External resolution would require either a negotiated multilateral redefinition of graduation criteria (a political act) or long-run empirical tracking of whether states that graduate from S&D subsequently converge to comparable competitive positions with legacy developed states.',
    'If the permanence framing is correct, this constraint''s classification as tangled_rope with a stable moderate epsilon is durable. If the transitional framing is correct instead, this reading''s high beneficiary count and low victim-side justification would erode over time as graduation proceeds, pushing the constraint toward scaffold (sunset-bound coordination) rather than tangled_rope — a different constraint, not a recomputation of this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sd_permanence_vs_transition_ambiguity, conceptual, 'Whether S&D/policy-space commitments are structurally permanent or transitional — the axis the two sibling readings are built on.').

omega_variable(
    technology_transfer_enforceability,
    'Are technology transfer obligations under agreements like TRIPS Article 66.2 genuinely enforceable core commitments, or largely declaratory ''best endeavors'' language with negligible compliance consequence?',
    'Empirical review of actual technology transfer flows attributable to treaty obligation (as opposed to ordinary FDI and licensing) versus the volume of formal reporting and declared commitments; compare LDC-specific compliance reports against independent technology-flow data.',
    'If largely declaratory, the theater_ratio for this reading is understated and the coordination function is weaker than authored, pushing the classification toward piton-adjacent territory for this specific mechanism even while other S&D provisions remain substantively binding. If substantively enforced, the tangled_rope classification with moderate epsilon is well supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_enforceability, empirical, 'Whether technology transfer commitments are substantively binding or largely declaratory.').

omega_variable(
    self_designation_beneficiary_scope,
    'Should states that self-designate as ''developing'' but possess advanced industrial capacity (a live dispute predating and outlasting the 2019 US challenge to self-designation) be counted within this reading''s beneficiary class, or does their inclusion dilute the accommodation''s targeting toward the states with genuine asymmetric starting conditions?',
    'Comparative analysis of per-capita industrial capacity, export sophistication, and negotiating leverage across self-designated developing members, benchmarked against the founding problem''s original target population.',
    'A broad beneficiary class (including advanced developing economies) would show extraction spread across many payer relationships with weaker individual justification per case; a narrow beneficiary class limited to LDCs and low-capacity states would show more concentrated, better-justified extraction consistent with the founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_designation_beneficiary_scope, empirical, 'Whether the beneficiary class as currently practiced matches the founding problem''s target population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__developmental_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(wto__tr_t2000, wto_treaty_framework__developmental_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(wto__tr_t2005, wto_treaty_framework__developmental_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(wto__tr_t2010, wto_treaty_framework__developmental_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(wto__tr_t2015, wto_treaty_framework__developmental_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(wto__tr_t2020, wto_treaty_framework__developmental_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(wto__tr_t2024, wto_treaty_framework__developmental_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__developmental_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(wto__be_t2000, wto_treaty_framework__developmental_reading, base_extractiveness, 2000, 0.27).
narrative_ontology:measurement(wto__be_t2005, wto_treaty_framework__developmental_reading, base_extractiveness, 2005, 0.31).
narrative_ontology:measurement(wto__be_t2010, wto_treaty_framework__developmental_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(wto__be_t2015, wto_treaty_framework__developmental_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(wto__be_t2020, wto_treaty_framework__developmental_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(wto__be_t2024, wto_treaty_framework__developmental_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__developmental_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(wto__su_t2000, wto_treaty_framework__developmental_reading, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement(wto__su_t2005, wto_treaty_framework__developmental_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(wto__su_t2010, wto_treaty_framework__developmental_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(wto__su_t2015, wto_treaty_framework__developmental_reading, suppression_requirement, 2015, 0.39).
narrative_ontology:measurement(wto__su_t2020, wto_treaty_framework__developmental_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(wto__su_t2024, wto_treaty_framework__developmental_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).

% DUAL FORMULATION NOTE:
% This constraint and wto_treaty_framework__market_access_reading are sibling readings of a single contested textual kernel (the WTO treaty framework, including GATT/GATS/TRIPS and their S&D provisions). They are NOT two measurements of one constraint — per the epsilon-invariance principle, each reading fixes a different beneficiary/victim structure and a different epsilon from the same text, so they are authored as two separate constraint stories linked here rather than as one story with a measurement parameter. The developmental_reading (this file) treats S&D and technology transfer as permanent, equal-status commitments with moderate epsilon concentrated on IP holders and export sectors. The market_access_reading treats S&D as a temporary transitional exception to a primarily symmetric liberalization obligation, and would author a different epsilon concentrated on the market-access costs borne by exporters into protected developing markets. Structural pressure runs both directions: aggressive graduation-criteria litigation under the market_access_reading directly narrows this reading's beneficiary class over time, while enforcement wins for technology transfer under this reading raise the political cost of graduation pressure under the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
