% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO TRIPS Dispute Settlement Binding Interpretive Authority
 *   domain: International Trade Law / Public Health Policy / Intellectual Property Regime
 *
 * SUMMARY:
 *   WTO dispute panels interpret the TRIPS agreement through the Dispute
 *   Settlement Understanding, issuing reports that become binding unless
 *   rejected by consensus. Over time, panel precedent has accumulated to
 *   narrow the scope of compulsory licensing and parallel import
 *   flexibilities, while authorized trade retaliation enforces compliance.
 *   Since the Appellate Body ceased functioning in 2019, panel reports are
 *   less reviewable and powerful members increasingly supplement multilateral
 *   rulings with bilateral pressure. This constraint is the meta-mechanism of
 *   binding interpretive authority itself â one reading of the contested
 *   TRIPS kernel â which locks in substantive readings through precedent
 *   and retaliation. It claims to be genuine coordination (rules-based
 *   dispute resolution) but operates with substantial asymmetric extraction:
 *   developed countries and innovator industries capture the precedential
 *   benefits, while developing countries and generic producers bear the
 *   retaliatory and regulatory costs. The claimed type is tangled_rope,
 *   acknowledging both the real coordination function and the asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - wto_dispute_settlement_body: Primary agenda-setter (institutional/analytical) â administers panel process and retaliation authorization
 *   - developed_wto_members: Primary beneficiary (institutional/mobile) â initiates disputes and captures precedential lock-in
 *   - developing_country_wto_members: Primary target (institutional/constrained) â bears retaliation and lost regulatory autonomy
 *   - innovator_pharmaceutical_sector: Secondary beneficiary (organized/mobile) â captures economic rents from narrowed flexibilities
 *   - generic_medicine_producers: Secondary target (moderate/constrained) â loses market access from adverse rulings
 *   - public_health_advocates: Excluded voice (organized/constrained) â no standing in state-to-state proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.78).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO TRIPS Dispute Settlement Binding Interpretive Authority").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "International Trade Law / Public Health Policy / Intellectual Property Regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '5a63a553-637c-4583-adcf-8773da02fa3a').
narrative_ontology:cs_kernel_codification('5a63a553-637c-4583-adcf-8773da02fa3a', formalized).
narrative_ontology:cs_authority_grounding('5a63a553-637c-4583-adcf-8773da02fa3a', lineage).
narrative_ontology:cs_interpretation_layer_present('5a63a553-637c-4583-adcf-8773da02fa3a').
narrative_ontology:cs_reading_relation('5a63a553-637c-4583-adcf-8773da02fa3a', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('5a63a553-637c-4583-adcf-8773da02fa3a', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('5a63a553-637c-4583-adcf-8773da02fa3a', foundational, dispute_settlement_binding_authority).
narrative_ontology:cs_axiom_status(dispute_settlement_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('5a63a553-637c-4583-adcf-8773da02fa3a', dispute_settlement_binding_authority, conventional).
narrative_ontology:cs_axiom('5a63a553-637c-4583-adcf-8773da02fa3a', foundational, trade_retaliation_for_ip_enforcement).
narrative_ontology:cs_axiom_status(trade_retaliation_for_ip_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('5a63a553-637c-4583-adcf-8773da02fa3a', trade_retaliation_for_ip_enforcement, conventional).
narrative_ontology:cs_reference_frame('5a63a553-637c-4583-adcf-8773da02fa3a', multilateral_reviewed_adjudication).
narrative_ontology:cs_drift_state('5a63a553-637c-4583-adcf-8773da02fa3a', post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5a63a553-637c-4583-adcf-8773da02fa3a', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_wto_members).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, innovator_pharmaceutical_sector).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_wto_members).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_medicine_producers).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dsu_article_23_compliance).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, panel_report_precedential_weight).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Understanding on Rules and Procedures Governing the Settlement of Disputes. Convenes ad hoc panels to interpret TRIPS provisions, circulates panel reports to the Dispute Settlement Body for adoption, and oversees the authorization of retaliatory measures when compliance is not achieved. Since 2019 it has operated without a functioning Appellate Body.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% File complaints against developing country members alleging inadequate TRIPS compliance. Benefit from panel reports that interpret TRIPS obligations narrowly and authorize suspension of concessions against non-compliant members. Can also pursue bilateral and plurilateral pressure outside the WTO when multilateral outcomes are slow.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_wto_members, beneficiary,
    institutional, generational, mobile, global).

% Defend their domestic IP laws and public health measures before panels. Face authorized trade retaliation if found non-compliant, including suspension of concessions in unrelated sectors. Must either amend domestic law to comply or absorb retaliatory costs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_wto_members, payer,
    institutional, generational, constrained, global).

% Provides technical assistance and legal analysis to support complaints that protect patent terms and data exclusivity. Benefits from precedential interpretations that limit compulsory licensing and parallel importation. Lobbies for strong enforcement through national trade agencies.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, innovator_pharmaceutical_sector, beneficiary,
    organized, generational, mobile, global).

% Manufacture affordable medicines under compulsory licenses or exceptions permitted by domestic law. Lose export markets and domestic production authorization when panel rulings invalidate those measures. Must reformulate or exit markets affected by adverse rulings.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_medicine_producers, payer,
    moderate, biographical, constrained, global).

% Campaign for broad interpretation of TRIPS flexibilities on behalf of access to medicines. Submit amicus briefs to panels, which panels are not required to consider. Have no standing in state-to-state proceedings and no vote in DSB deliberations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, diffuse).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces unilateral trade pressure with a rules-based adjudication mechanism for resolving disputes over TRIPS compliance, aiming to stabilize expectations and prevent trade wars over intellectual property standards.
% TRANSFER_FUNCTION: Moves interpretive authority over TRIPS text from autonomous member state reading to binding panel reports backed by authorized cross-retaliation; transfers regulatory flexibility from developing country respondents to precedential constraints that favor strong intellectual property protection.
% ABSENT_VOICES: Public health advocates, affected patient populations, and generic medicine producers lack standing in state-to-state dispute proceedings; their arguments enter only through amicus curiae briefs that panels routinely disregard, and least-developed countries often lack technical capacity to participate meaningfully.
% DISAPPEARANCE_RATIONALE: Without binding panel authority and retaliation enforcement, TRIPS interpretations would revert to unilateral state reading and bilateral pressure; accumulated precedent locking in strong exclusivity would lose force; the global pharmaceutical access landscape would reorganize around regional and bilateral IP standards rather than multilateral precedent.
% FOUNDING_PROBLEM: International IP conflicts prior to TRIPS were resolved through unilateral economic coercion and trade retaliation by powerful states, creating instability and asymmetry that disadvantaged smaller trading partners and prevented coordinated global standards.
% FOUNDING_PROBLEM_CORROBORATION: The WTO Secretariat and developed country members attest the mechanism remains necessary to prevent unilateralism. Developing country members, public health NGOs, and independent trade law scholarship document that bilateral power has substantially substituted for multilateral adjudication after the Appellate Body collapse, undermining the original function.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because panel precedent progressively narrows the public health flexibility reading of TRIPS, transferring regulatory autonomy to a precedential structure that favors strong exclusivity. Suppression (0.78) is higher still: the mechanism depends on the credible threat of cross-sectoral trade retaliation, which compels compliance even when the underlying interpretation is contested. Theater ratio (0.45) reflects the post-Appellate Body dynamic in which bilateral power increasingly substitutes for multilateral legal process, making the panel proceedings more performative. Accessibility collapse (0.68) is high because alternatives to compliance (sustained non-compliance under retaliation) are economically prohibitive for most developing members. Resistance (0.55) captures ongoing efforts to use flexibilities despite rulings, the blocking of AB appointments, and the proliferation of bilateral workarounds.
 *
 * PERSPECTIVAL GAP:
 *   From the developed country complainant seat, the constraint is a necessary enforcement mechanism that prevents free-riding on IP standards and ensures treaty compliance. From the developing country respondent seat, it is a coercive interpretive lock-in that prevents legitimate use of public health flexibilities. The engine computes this divergence from the same structural data; the authored claim (tangled_rope) names the hybrid reality rather than either seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed country members and innovator pharmaceutical industries are declared beneficiaries: they collect the value of locked-in high-protection interpretations and have mobile or arbitrage-grade exit (bilateral alternatives). Developing country members and generic producers are declared victims: they pay through retaliatory costs and lost regulatory space, with constrained or trapped exit. The WTO DSB sits near symmetric as administrator. Public health advocates are excluded from the mechanism entirely. Directionality derives from these structural positions: low d for beneficiaries, high d for targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the genuine coordination function (preventing unilateral trade wars over IP) from the extractive overlay (asymmetric precedent accumulation backed by retaliation). A pure rope reading would ignore the victim set and the active enforcement asymmetry; a pure snare reading would ignore that some dispute resolution is preferable to unilateral coercion. The founding problem (unilateral trade pressure) was real, but the arrangement has drifted: the Appellate Body collapse and bilateral substitution suggest the coordination function is decaying while the extraction remains. This drift is captured in the theater_ratio trajectory and the drift_state authority_erosion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'How does the dispute settlement interpretive authority reading relate to the substantive strong exclusivity and public health flexibility readings of the same TRIPS kernel?',
    'Corpus-level comparison of the three sibling constraints to verify structural non-equivalence and measure cross-index coupling.',
    'Confirms that epsilon-invariance is maintained: the meta-constraint of interpretive authority is structurally distinct from the substantive IP standards it interprets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer frame location of this reading within the TRIPS kernel').

omega_variable(
    appellate_body_collapse_authority,
    'Does the Appellate Body collapse structurally degrade the legitimacy of panel authority, or does it merely shift review into informal channels?',
    'Empirical tracking of compliance rates with panel reports pre- and post-2019, and comparison of bilateral versus multilateral enforcement frequency.',
    'If legitimacy is severely degraded, the constraint''s suppression may be increasingly theatrical; if informal channels maintain review, the coordination function persists in altered form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_collapse_authority, empirical, 'Impact of AB collapse on panel authority legitimacy').

omega_variable(
    bilateral_substitution_or_decay,
    'Does the post-Appellate Body shift to bilateral power dynamics represent a decay of this multilateral constraint, or an intensification of its extractive function through alternative channels?',
    'Comparative analysis of IP enforcement actions: count and economic impact of WTO-authorized retaliation versus bilateral and plurilateral pressure such as USTR 301 actions and trade agreement IP chapters.',
    'If bilateral dynamics dominate, the multilateral constraint may be a piton (atrophied function, theatrical maintenance); if multilateral retaliation remains primary despite AB collapse, it is a tangled rope with weakened review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_substitution_or_decay, empirical, 'Whether bilateral substitution indicates constraint decay or extraction intensification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0, 0.12).
narrative_ontology:measurement(trip_tr_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 5, 0.16).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 10, 0.22).
narrative_ontology:measurement(trip_tr_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 15, 0.28).
narrative_ontology:measurement(trip_tr_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 20, 0.35).
narrative_ontology:measurement(trip_tr_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 25, 0.4).
narrative_ontology:measurement(trip_tr_t29, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 29, 0.45).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(trip_be_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(trip_be_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(trip_be_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(trip_be_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(trip_be_t29, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 29, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(trip_su_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(trip_su_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(trip_su_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(trip_su_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(trip_su_t29, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 29, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the trips_agreement_interpretive_kernel family, decomposed per the epsilon-invariance principle. It models the meta-constraint of dispute settlement authority, while siblings model the substantive strong exclusivity and public health flexibility readings of the TRIPS text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
