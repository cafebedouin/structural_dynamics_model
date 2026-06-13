% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty in Federal Compact (Confederation Model)
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   Under the compact-federalism reading, the Canadian federation (or any
 *   analogous confederation) is understood as a negotiated compact among
 *   sovereign provinces, not a federal constitution that subordinates
 *   provinces to federal authority. Provinces retain residual sovereignty —
 *   anything not explicitly delegated to the federal government remains
 *   theirs, including natural resources (s.92A), education, healthcare, and
 *   labor regulation. Exit from the federation is possible but negotiable
 *   under duress: a province cannot unilaterally leave, but can extract
 *   concessions from the federal government by threatening to do so. This
 *   reading grounds legitimacy in the original 1867 compact (Confederation)
 *   rather than in constitutional subordination. The constraint operates as a
 *   tangled rope: it coordinates interprovincial equalization and common
 *   standards (coordination function) while simultaneously extracting
 *   authority from federal government and constraining resource-poor
 *   provinces (asymmetric extraction). The measured extractiveness reflects
 *   the growing divergence between the compact's founding coordination
 *   function (interprovincial trade, common labor rules, unified defense) and
 *   its contemporary operation (resource-rich provinces use sovereignty
 *   threat to extract federal concessions; federal climate action is blocked
 *   by provincial veto; interprovincial labor mobility remains fragmented).
 *   The claim-to-metric gap is structural to the reading itself: compact
 *   federalism CLAIMS to be a genuine coordination mechanism (rope), but the
 *   authored metrics reveal it operates with substantial extraction
 *   (tangled_rope). This is the diagnostic divergence the engine is designed
 *   to detect.
 *
 * KEY AGENTS:
 *   - provincial_governments: institutional agenda-setters, collectively benefit from retained sovereignty and equalization transfers
 *   - resource_rich_provinces: powerful beneficiaries, use resource wealth and exit threat to dominate interprovincial negotiation
 *   - federal_government: constrained institutional agenda-setter, operates conditionally on provincial consent
 *   - resource_poor_provinces: moderate-power victims, trapped into equalization dependence and constrained labor mobility
 *   - federal_labor_mobility_seekers: powerless victims, experience provincial barriers despite supposed federal labor market
 *   - indigenous_nations: excluded institutional actors, bearing extraction costs from provincial resource extraction without decision voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.48).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.52).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.48).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty in Federal Compact (Confederation Model)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '768b8e43-f82c-4059-b0b1-2084c16e1214').
narrative_ontology:cs_kernel_codification('768b8e43-f82c-4059-b0b1-2084c16e1214', fixed_text).
narrative_ontology:cs_authority_grounding('768b8e43-f82c-4059-b0b1-2084c16e1214', lineage).
narrative_ontology:cs_interpretation_layer_present('768b8e43-f82c-4059-b0b1-2084c16e1214').
narrative_ontology:cs_reading_relation('768b8e43-f82c-4059-b0b1-2084c16e1214', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('768b8e43-f82c-4059-b0b1-2084c16e1214', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('768b8e43-f82c-4059-b0b1-2084c16e1214', foundational, confederation_compact_foundational_sovereignty).
narrative_ontology:cs_axiom_status(confederation_compact_foundational_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('768b8e43-f82c-4059-b0b1-2084c16e1214', confederation_compact_foundational_sovereignty, conventional).
narrative_ontology:cs_axiom('768b8e43-f82c-4059-b0b1-2084c16e1214', secondary, residual_provincial_authority_non_delegated_powers).
narrative_ontology:cs_axiom_status(residual_provincial_authority_non_delegated_powers, holdable).
narrative_ontology:cs_axiom_grounding('768b8e43-f82c-4059-b0b1-2084c16e1214', residual_provincial_authority_non_delegated_powers, conventional).
narrative_ontology:cs_reference_frame('768b8e43-f82c-4059-b0b1-2084c16e1214', confederation_original_compact).
narrative_ontology:cs_drift_state('768b8e43-f82c-4059-b0b1-2084c16e1214', contemporary_resource_abundance_climate_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('768b8e43-f82c-4059-b0b1-2084c16e1214', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, resource_poor_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_labor_mobility_seekers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.32) in the early interval because the founding compact's original coordination functions (trade, defense, currency) were new and substantial — provinces genuinely benefited from unified markets and federal military capacity. Over 40 years it rises monotonically to 0.48, flattening after year 25: this trajectory models the degradation of the founding coordination functions (continental trade integration is mature, federal defense is taken for granted) while the extraction mechanisms (provincial resource rents protected by sovereignty claim, federal climate policy blocked by provincial veto, labor mobility restricted by provincial barriers) remain active and harden. The theater ratio rises from 0.28 to 0.41 at the same flattening point, indicating that by year 25, a growing share of provincial sovereignty invocation is performative — defending resource rents under the language of autonomy, rather than exercising genuine residual authority. Suppression starts at 0.38 (moderate passive barriers: credential non-recognition, healthcare portability rules, federal labor-mobility proposals that provinces reject) and rises to 0.52 (active federal enforcement machinery developed to manage interprovincial disputes: constitutional court precedent, federal spending conditions, threatened federal carbon pricing that provinces must negotiate against). The one shared time grid ensures every metric is measured at every examined point; the projection basis switches from observed to projected at year 30 to reflect uncertainty in long-term compact stability.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial-government agenda-setter seat (particularly resource-rich provinces), the compact is a genuine exercise in retained sovereignty: they are using legitimate residual authority to manage resources and policy within their boundaries, and negotiating with the federal government from a position of consent and exit leverage. They experience the constraint as a rope — it coordinates equalization and federal standards while allowing them autonomy. From the federal-government seat, the compact appears as a tangled rope with problematic extraction: federal authority is conditional on provincial agreement, which resource-rich provinces exploit to block climate action and federal labor-market harmonization. Federal actors experience the constraint's enforcement machinery (constitutional limits on unilateral federal action, the spending power doctrine, negotiated federalism) as a suppression mechanism keeping them from solving collective problems (climate, labor mobility) that individual provinces cannot. From resource-poor provinces and labor-mobility seekers, the compact is clearly extractive: they bear the cost of federal compliance with provincial preferences (limited climate action, fragmented labor markets) without the leverage to change the arrangement. These divergent perspectives arise from structural asymmetry in exit options (resource-rich provinces mobile/arbitrage, resource-poor provinces trapped), not from disagreement on the facts. The engine computes per-seat classification from power + exit + beneficiary/victim data; this perspectival gap is the structural material those computations operate on.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial governments as a class are beneficiaries (they set the agenda, they collect rents, they negotiate equalization upward) and therefore should compute low directionality (d near 0.0, full beneficiary). Resource-rich provinces within that class have the highest bargaining power (exit leverage, resource wealth, arbitrage options) and should compute lower d than resource-poor provinces. The federal government is the primary target of extraction — its authority is constrained, its climate and labor-mobility agenda blocked — so it should compute high d (near 1.0, full target). Resource-poor provinces are secondary targets: they pay equalization contributions, they lose labor-market mobility, but they also benefit from equalization transfers (mixed position, d near 0.5). Federal labor-mobility seekers are powerless targets with no exit options (trapped), so they should compute very high d. Indigenous nations are excluded, identity-locked, and bearing extraction costs without voice — extremely high d. No overrides are necessary; the beneficiary/victim declarations and power/exit data produce accurate directionality derivations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy (a constraint whose original function has atrophied but persists theatrically). The compact's original founding functions (interprovincial trade, defense coordination, unified currency, labor mobility) remain materially important — provinces still benefit from trade, federal defense remains a public good, and the currency is unified. What HAS atrophied is the compact's ability to coordinate on NEW collective problems (climate, interprovincial environmental spillovers, labor-market adaptation to automation). The constraint persists not because of institutional inertia, but because the arrangement still delivers asymmetric benefits to resource-rich provinces who have exit leverage. If the constraint were a piton, we would expect theater_ratio to be high (0.7+) with no real coordination function and low measured extraction (since pitons persist by diffusion, not by active beneficiary defense). Instead, the constraint shows moderate theater (0.41) with genuine coordination residue and active extraction (0.48) by concentrated beneficiaries (resource-rich provinces) against concentrated targets (federal government, resource-poor provinces). This is tangled rope, not piton: extraction is explicit, enforcement is active, and the beneficiary class knows exactly who is paying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence_vs_obsolescence,
    'Is the original 1867 founding problem (coordinating four colonies into a continent-spanning economic and political union) still live, or has it been substantially solved such that the compact''s persistence is now best explained by resource-rich provincial rent extraction rather than coordination necessity?',
    'Counterfactual institutional analysis: what problems would arise if the compact dissolved tomorrow and were replaced with (a) a unitary federal state, or (b) three or four sovereign nations? If interprovincial trade, labor mobility, and defense would function as well or better, the founding problem is dead. If serious coordination problems would emerge (market fragmentation, labor-market failures, defense gaps), the founding problem is live.',
    'If the founding problem is dead, the constraint shifts toward snare (pure extraction with coordination cover story); if live, it remains tangled rope. If dead, base_extractiveness should be revised upward and theater_ratio downward in a piton direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_vs_obsolescence, conceptual, 'Whether the compact''s original coordination necessity persists or the arrangement is now sustained primarily by concentrated beneficiary interest in resource-rich provinces.').

omega_variable(
    provincial_sovereignty_as_natural_or_constructed,
    'Is ''provincial sovereignty'' as described in the compact-federalism reading a natural or self-evident fact that emerges from the 1867 bargain, or a constructed narrative that serves resource-rich provincial interests by claiming natural status?',
    'Historical analysis of the 1867 Confederation discussions: did the founders explicitly assert provincial sovereignty as residual and negotiable, or was this reading developed later (post-1900, post-1982 resource-sovereignty amendments) as a tool for provincial advantage in resource disputes?',
    'If natural/original, the reading''s legitimacy is grounded in historical intent; if constructed, the constraint is a false-summit candidate masquerading as natural law. If constructed, the compact-federalism reading should carry an additional omega documenting the false-summit ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provincial_sovereignty_as_natural_or_constructed, conceptual, 'Whether provincial sovereignty is an intrinsic feature of the original compact or a retroactively asserted reading developed to serve provincial advantage in resource extraction disputes.').

omega_variable(
    exit_threat_credibility_under_climate_crisis,
    'As climate change makes resource extraction (oil, gas) increasingly costly and socially disfavored, does the exit threat (separation threat) that resource-rich provinces use to extract federal concessions remain credible, or will it decay?',
    'Observation over the next 10–15 years: if separation rhetoric persists despite declining resource revenue and fossil-fuel phase-out pressure, the exit threat is rhetorical (theater); if separation rhetoric declines as resources become less valuable, the exit threat was instrumentally tied to resource rents (not fundamental sovereignty claim).',
    'If exit threat decays, suppression_requirement should fall, base_extractiveness should fall, and the constraint should shift from tangled_rope toward rope (reduced asymmetric extraction as the leverage basis disappears). If exit threat hardens despite declining resource value, it indicates a deeper sovereignty claim not tied to rents (more fundamental than compact-federalism would predict).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_threat_credibility_under_climate_crisis, empirical, 'Whether provincial exit threats are grounded in material resource wealth and will decay with resource devaluation, or represent deeper sovereignty claims that will persist.').

omega_variable(
    federal_climate_authority_legitimacy_contest,
    'Do federal climate authorities (environmental agencies, climate-policy bureaucrats) have legitimate grounds to override the compact-federalism reading''s requirement for provincial consent on emissions policy, or is provincial consent structurally required by the reading''s own logic?',
    'Examination of the 1867 compact''s original allocation of authority: was environmental/climate regulation implicitly federal (under ''peace, order, good government'' residual federal power) or implicitly provincial (under property and civil rights)? Does the reading''s own textual basis support or deny federal unilateral climate authority?',
    'If federal authority over climate is supported by the compact''s own logic, then federal climate policy blocking is a violation of the compact (not a legitimate provincial exercise), and the constraint''s extraction mechanism is illegitimate even within the reading. If provincial consent is structurally required, then federal climate blockage is a feature of the compact, not a bug.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_climate_authority_legitimacy_contest, conceptual, 'Whether provincial climate-policy veto is a legitimate expression of the compact-federalism reading''s allocation of authority, or whether the reading''s own logic supports federal unilateral climate authority.').

omega_variable(
    indigenous_nation_exclusion_legitimacy,
    'Is the exclusion of indigenous nations from the confederation''s decision structure a permanent feature of the compact-federalism reading, or does the reading''s own logic (original sovereignty retained by the parties to the compact) implicate indigenous nations as original sovereigns whose sovereignty was never ceded?',
    'Historical and legal analysis of indigenous sovereignty: were indigenous nations parties to the 1867 compact (Confederation)? Do indigenous nations retain claims to territorial sovereignty that predate the compact? If yes to both, does the compact-federalism reading logically require recognition of indigenous nations as original sovereigns with veto rights equivalent to provinces?',
    'If indigenous nations are implicated as original sovereigns by the reading''s own logic, then their exclusion is a contradiction within the reading (false-summit candidate). If excluded by design, the reading is inherently contestable as a justification for federal structure. Either way, indigenous-nation recognition would alter the constraint''s stakeholder map and directionality computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_nation_exclusion_legitimacy, conceptual, 'Whether the compact-federalism reading''s logic of retained sovereignty applies to indigenous nations as original sovereigns, making their exclusion a logical contradiction within the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_sov_compact_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.28).
narrative_ontology:measurement(prov_sov_compact_tr_t5, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 5, 0.31).
narrative_ontology:measurement(prov_sov_compact_tr_t10, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 10, 0.34).
narrative_ontology:measurement(prov_sov_compact_tr_t15, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 15, 0.37).
narrative_ontology:measurement(prov_sov_compact_tr_t20, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 20, 0.39).
narrative_ontology:measurement(prov_sov_compact_tr_t25, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 25, 0.4).
narrative_ontology:measurement(prov_sov_compact_tr_t30, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 30, 0.41).
narrative_ontology:measurement(prov_sov_compact_tr_t35, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 35, 0.41).
narrative_ontology:measurement(prov_sov_compact_tr_t40, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(prov_sov_compact_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(prov_sov_compact_be_t5, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 5, 0.37).
narrative_ontology:measurement(prov_sov_compact_be_t10, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(prov_sov_compact_be_t15, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(prov_sov_compact_be_t20, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(prov_sov_compact_be_t25, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(prov_sov_compact_be_t30, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(prov_sov_compact_be_t35, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 35, 0.48).
narrative_ontology:measurement(prov_sov_compact_be_t40, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(prov_sov_compact_su_t0, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(prov_sov_compact_su_t5, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(prov_sov_compact_su_t10, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(prov_sov_compact_su_t15, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(prov_sov_compact_su_t20, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(prov_sov_compact_su_t25, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(prov_sov_compact_su_t30, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(prov_sov_compact_su_t35, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(prov_sov_compact_su_t40, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, federal_spending_power__equalization_distribution).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_environmental_authority__resource_extraction_override).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, labor_mobility_interprovincial__credential_recognition_barriers).

% DUAL FORMULATION NOTE:
% The provincial_sovereignty_boundary kernel decomposes into three readings with distinct ε and beneficiary structures: (1) compact_federalism (this constraint) — federal authority conditional on provincial consent; ε=0.48 (substantial extraction by resource-rich provinces); (2) constitutional_subordination — federal authority supreme, provinces are constitutional creatures; ε=0.25 (minimal extraction if federal authority is exercised); (3) resource_sovereignty_primacy — provincial resource control grounds absolute territorial sovereignty; ε=0.62 (maximum extraction by resource-rich provinces over federal climate/environmental authority). These readings coexist in Canadian federalism jurisprudence — they are held by different institutional actors simultaneously (resource-rich provinces prefer reading 3 or 1, federal government prefers reading 2, courts oscillate). Link all three constraint files via network.affects_constraints to enable contamination analysis of the kernel: if one reading's legitimacy degrades (e.g., if resource sovereignty primacy is foreclosed by international climate agreements), predict effects on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
