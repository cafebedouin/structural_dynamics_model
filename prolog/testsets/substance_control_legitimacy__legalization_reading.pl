% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Adult Autonomy with Third-Party Harm Prevention
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint models the legalization reading of substance control
 *   authority: competent adults hold autonomy over personal substance use;
 *   state authority derives legitimacy from preventing third-party harms
 *   (impaired driving, secondhand exposure, youth access, public health
 *   emergencies) rather than from criminalizing possession. This reading sits
 *   in a contested kernel alongside prohibition_reading (state authority
 *   grounded in moral duty to prevent all use through criminalization) and
 *   harm_reduction_reading (state authority grounded in minimizing aggregate
 *   harm through public health measures without criminalization). The
 *   legalization reading claims autonomy as foundational and locates the
 *   constraint's persistence in enforcement of harm-prevention boundaries.
 *   Key structural delta from prohibition: users move from victim to
 *   beneficiary set; third-party harm bearers become the primary victim set;
 *   legal market operators emerge as beneficiaries capturing economic rent;
 *   criminal justice apparatus experiences institutional identity crisis as
 *   caseload shifts from criminal enforcement to regulatory compliance. The
 *   constraint is claimed as tangled_rope (real coordination function +
 *   asymmetric extraction) and the metrics reflect both coordination benefits
 *   (decriminalization, product safety, market clarity) and extraction costs
 *   (market operator markup, regulatory burden, enforcement asymmetry
 *   favoring operators over harm-bearers).
 *
 * KEY AGENTS:
 *   - adult_user_constituency: Moves from prohibition-era victim set to legalization-era beneficiary; gains legal access and elimination of criminalization risk, but subject to regulatory compliance and market pricing.
 *   - legal_market_operators: Primary beneficiaries; capture economic rents through licensed monopoly/oligopoly; shape regulatory boundaries through lobbying.
 *   - third_party_harm_bearers: Primary victims under legalization reading; bear costs of impaired driving, secondhand exposure, youth access enforcement, and public health burden without direct consumption benefit.
 *   - regulation_and_enforcement_apparatus: Agenda-setter; administers harm-prevention framework and compliance monitoring; carries significant operational burden that did not exist under prohibition.
 *   - criminal_justice_apparatus_remnant: Identity-locked payer; institutional legitimacy rooted in prohibition-era framing; experiences caseload reduction and authority erosion; cannot easily exit because career specialization and organizational culture remain prohibition-focused.
 *   - public_health_advocates: Constrained payer; must advocate within legalization framework against market operators' commercial incentives to expand use and potency.
 *   - prohibition_adherent_constituency: Structurally excluded from legalization reading; their moral-duty-grounded authority claim is foreclosed by the autonomy axiom.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.68).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.52).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Adult Autonomy with Third-Party Harm Prevention").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '3d7effc1-4e03-44f5-bc4e-0897a34b17d3').
narrative_ontology:cs_kernel_codification('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', formalized).
narrative_ontology:cs_authority_grounding('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', distributed).
narrative_ontology:cs_reading_relation('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', substance_control_legitimacy__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', foundational, adult_autonomy_over_personal_substance_use).
narrative_ontology:cs_axiom_status(adult_autonomy_over_personal_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', adult_autonomy_over_personal_substance_use, deontological).
narrative_ontology:cs_axiom('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', foundational, state_authority_limited_to_third_party_harm_prevention).
narrative_ontology:cs_axiom_status(state_authority_limited_to_third_party_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', state_authority_limited_to_third_party_harm_prevention, deontological).
narrative_ontology:cs_reference_frame('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', autonomous_adult_personal_choice_with_harm_boundaries).
narrative_ontology:cs_drift_state('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', contemporary_market_consolidation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d7effc1-4e03-44f5-bc4e-0897a34b17d3', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_market_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adult_users_accessing_substance).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_bearers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, enforcement_personnel_burden).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (at legalization onset, when coordination benefits dominate) to 0.68 (at interval end, as market operators consolidate pricing power and regulatory barriers entrench). The trajectory reflects market maturation: initial legalization establishes coordination gains (safety, decriminalization, predictability) but over 25 years, competitive consolidation and marketing escalation (higher potency, targeted advertising, youth-adjacent positioning) amplify extraction above coordination costs. Suppression_requirement rises modestly (0.38 to 0.52) because harm-prevention enforcement requires active investment in impaired-driving enforcement, youth-access monitoring, and compliance auditing — this is genuinely necessary suppression, not pure coercion. Theater_ratio rises (0.25 to 0.41) because regulatory agencies increasingly perform public-safety theater (potency-limit declarations, packaging warnings) that does not proportionally reduce harms as market operators find regulatory arbitrage. Accessibility_collapse is moderate (0.62): legalization opens the primary market but creates strong regulatory barriers and pricing that re-entrench alternatives (black markets for potent unregulated products, cross-border purchases). Resistance is high (0.71) because third-party harm bearers (families with youth access concerns, drivers affected by impaired operators, medical professionals treating addiction) mount continuous resistance through advocacy, litigation, and local enforcement — the constraint's persistence depends on active suppression of that resistance via regulatory preemption and commercial marketing.
 *
 * PERSPECTIVAL GAP:
 *   From the legal market operator and adult user seats: the arrangement is genuine coordination solving a real problem (decriminalization, product safety, market clarity) — they should compute a beneficiary reading, possibly rope-grade. From the third-party harm bearer and criminal-justice seats: the same arrangement is extraction — they bear concentrated costs (enforcement burden, harm exposure) while operators capture benefits (rent collection) — they should compute snare or high-extraction tangled_rope. The engine computes this from directionality: operators and users sit as beneficiaries (d near 0); harm bearers sit as victims (d near 1); regulation apparatus sits as beneficiary-agenda-setter with enforcement burden (d mid-range). The perspectival divergence is structural and reflects real seat conflict, not measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal market operators (institutional power, arbitrage exit): beneficiary directionality (d ~0.1), collect economic rent from legalization; beneficiary_agenda_setter role gives them high influence over regulatory shape, so d stays low despite enforcement requirements. Adult users (organized power, arbitrage exit): beneficiary directionality (d ~0.15), gain decriminalization and market access, can exit by relocating to other legalized jurisdictions; arbitrage exit significantly reduces target pressure. Third-party harm bearers (powerless, constrained exit): victim directionality (d ~0.9), bear enforcement burden and harm exposure with no corresponding benefit, cannot exit the shared environment where harms occur. Regulation apparatus (institutional power, analytical exit): mixed position (d ~0.45), agenda-setter role tempts toward lower d but enforcement burden and conflict with operators over harm-prevention stringency pulls toward victim directionality; analytical exit (could redesign the system) keeps d mid-range rather than high. Criminal-justice remnant (organized power, identity-locked exit): payer directionality (d ~0.75), bears caseload reduction and authority erosion; identity_locked exit means institutional identity is fused with prohibition-era authority and cannot easily shift; makes this seat a persistent victim.
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem (criminalization harms: incarceration, disparity, black markets) is contested whether it remains live or is substantially solved. If founding_problem_status = dead (harm substantially reduced), but disappearance_verdict = world_rearranges (the arrangement's persistence depends on active enforcement), the system exhibits mandatrophy: the authority that persists claims to solve a problem it no longer solves, and persists by rent collection instead. The mismatch (dead + world_rearranges) is a zombie indicator. Countervailing: if founding_problem_status = contested (which is the authored position), mandatrophy is not triggered — the framework permits both readings (problem is live, problem is substantially solved) to coexist without falsification. This reading stops short of asserting mandatrophy because the evidence genuinely supports both the harm-reduction claim and the market-extraction claim; they are not mutually exclusive in the legalization framework. The tension belongs in omega variables, not in a mandatrophy verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    third_party_harm_measurability,
    'Are the primary third-party harms (impaired driving, secondhand exposure, youth access, public health emergency load) measurable with sufficient precision to operationalize a harm-prevention boundary, or does ''harm'' remain essentially contested and subject to regulatory capture?',
    'Longitudinal epidemiological data from legalization jurisdictions; rigorous measurement of impaired-driving incident rates, secondhand exposure exposure levels, youth-use trend data, and emergency-care burden; comparison to jurisdiction-specific harm-prevention enforcement capacity.',
    'If harms are measurable and enforcement capacity tracks harm magnitude, the legalization constraint is sustainable as a genuine harm-prevention framework (moves toward rope). If harms are unmeasurable or enforcement capacity is decoupled from harm magnitude (regulatory capture or market pressure reducing enforcement stringency), the constraint drifts toward snare (operators extract while claiming harm prevention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_measurability, empirical, 'Whether the harm-prevention boundary can be operationalized and maintained against market pressure.').

omega_variable(
    third_party_harm_vs_personal_autonomy_conflict,
    'When third-party harms and personal autonomy conflict (e.g., secondhand cannabis smoke in shared residential settings, high-potency products increasing emergency-room load), which value dominates in practice? Is the harm-prevention boundary enforced symmetrically or does it erode in favor of operator and user interests?',
    'Case law and regulatory enforcement patterns; analysis of disputes between neighbors over secondhand exposure, between emergency-care providers and operators over potency limits, between youth-access advocates and commercial interests; measurement of regulatory agency budget allocation and enforcement frequency.',
    'If harm-prevention boundary is maintained symmetrically, the constraint remains tangled_rope with genuine coordination. If enforcement erodes in favor of operators (potency limits unenforced, secondhand exposure unregulated, youth-access gaps tolerated), the constraint drifts toward snare — the autonomy axiom becomes cover for operator extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_vs_personal_autonomy_conflict, empirical, 'Whether the harm-prevention boundary is a genuine constraint or an asymmetric enforcement vector favoring operators.').

omega_variable(
    market_operator_consolidation_and_rent_capture,
    'Does legalization enable dispersed small operators (craft producers, independent retailers) or does it consolidate into a few large operators extracting substantial economic rent through vertical integration and potency escalation?',
    'Market structure data (Herfindahl index over time, price tracking, potency trends), operator profitability data, and regulatory capture analysis (regulatory board composition, campaign contributions, revolving-door employment patterns).',
    'Consolidation with rent capture increases extractiveness above coordination costs (snare-drift). Dispersed competitive markets with price discipline and potency regulation maintain rope-grade equilibrium. The trajectory (0.48 → 0.68 extractiveness in the measurement series) suggests consolidation is occurring, but the mechanism is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_operator_consolidation_and_rent_capture, empirical, 'Whether market structure maintains competition or consolidates extraction.').

omega_variable(
    criminal_justice_identity_lock_escape_velocity,
    'Can criminal-justice-apparatus institutional actors exit identity-lock (institutional commitment to prohibition-era framing of their authority) and genuinely embrace regulatory-enforcement roles, or does institutional identity fusion create path-dependent resistance that undermines harm-prevention enforcement?',
    'Observational study of jurisdiction transitions: do law enforcement agencies successfully retrain for compliance monitoring and impaired-driving enforcement, or do they resist regulatory work and lobby to restore criminal penalties? Track career pathways and organizational budget allocations.',
    'If identity-lock can be overcome, the apparatus becomes a genuine regulatory partner with d dropping toward neutral (0.45). If identity-lock persists, the apparatus becomes a persistent victim/dissenting voice whose interests diverge from harm prevention, creating institutional conflict that destabilizes the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(criminal_justice_identity_lock_escape_velocity, empirical, 'Whether institutional identity-lock prevents criminal-justice apparatus from genuinely transitioning to regulatory enforcement.').

omega_variable(
    autonomy_axiom_cross_framework_contestation,
    'The legalization reading grounds authority in autonomy principle (competent adults decide for themselves; state authority limited to third-party harm prevention). Can this axiom coexist with prohibition reading''s axiom (state authority derives from moral duty to prevent harm regardless of autonomy) in the same jurisdiction, or do they truly foreclose each other in practice?',
    'Jurisdictional analysis: do legalization and prohibition coexist (federal vs. state, different jurisdictions, different substances) without fundamental contradiction, or does one inevitably try to eliminate the other? Examine legal/regulatory conflicts and institutional stability.',
    'If axioms coexist peacefully, readings are genuinely coexisting_with (both live, different parties). If they conflict, one axiom forecloses the other and the readings are in logical conflict (not mere disagreement but mutual exclusion). This affects the kernel''s stability and whether legalization can sustainably coexist with prohibition in the same polity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_axiom_cross_framework_contestation, conceptual, 'Whether autonomy and moral-duty axioms can coexist in a single authority framework.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'The suppression score (0.52) reflects enforcement of harm-prevention boundaries. Is this suppression structural (external barriers, regulatory penalties, enforcement costs) or partially internalized (users/operators internalize harm-prevention norms through education and regulatory legitimacy)?',
    'Post-regulation shift analysis: do users and operators comply with harm-prevention rules because external enforcement is present, or because norms have shifted? Test by varying enforcement intensity or authority legitimacy and observing compliance patterns.',
    'If suppression is structural, the constraint requires continuous enforcement investment; if internalized, suppression can gradually decrease as norms shift (more stable). If partially internalized, the breakdown ratio matters — a high structural component suggests extraction fragility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural enforcement or internalized norm adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scl_leg_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(scl_leg_tr_t5, substance_control_legitimacy__legalization_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(scl_leg_tr_t10, substance_control_legitimacy__legalization_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(scl_leg_tr_t15, substance_control_legitimacy__legalization_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(scl_leg_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(scl_leg_tr_t25, substance_control_legitimacy__legalization_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(scl_leg_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(scl_leg_be_t5, substance_control_legitimacy__legalization_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(scl_leg_be_t10, substance_control_legitimacy__legalization_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(scl_leg_be_t15, substance_control_legitimacy__legalization_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(scl_leg_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(scl_leg_be_t25, substance_control_legitimacy__legalization_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(scl_leg_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(scl_leg_su_t5, substance_control_legitimacy__legalization_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(scl_leg_su_t10, substance_control_legitimacy__legalization_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(scl_leg_su_t15, substance_control_legitimacy__legalization_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(scl_leg_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(scl_leg_su_t25, substance_control_legitimacy__legalization_reading, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% substance_control_legitimacy is a contested kernel decomposing into three structurally distinct constraint stories with different ε values, beneficiary/victim structures, and authority-grounding claims. This story instantiates legalization_reading; sibling stories are prohibition_reading and harm_reduction_reading. All three readings coexist in public discourse across different jurisdictions and policy coalitions. The ε values differ substantially: legalization_reading (this one) has moderate-high extractiveness (0.68) due to market operator consolidation competing with genuine harm-prevention benefits; prohibition_reading has lower extractiveness (concentrated enforcement burden but no market operator extraction) but higher suppression (criminal penalties); harm_reduction_reading has moderate extractiveness (harm-prevention mechanisms without criminalization, but public-health bureaucracy builds rents). The readings are linked by network.affects_constraints because they share the same contested kernel and compete for institutional adoption — legalization influences harm_reduction by shifting authority grounding from moral duty to autonomy principle, and coexists with prohibition across different political coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
