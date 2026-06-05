% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: Federation Membership Obligations (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   The integration-primary reading of federation membership obligations
 *   holds that free movement of persons is constitutive of EU citizenship and
 *   single market functioning, requiring member states to subordinate their
 *   welfare boundaries to mobility rights. Under this reading, workers from
 *   any EU member state must have full access to the social insurance and
 *   redistributive welfare systems of receiving states, not on grounds of
 *   contribution or residency history but on grounds of citizenship status
 *   within the federation. This constraint exhibits the tangled-rope
 *   structure: it coordinates labor market matching and removes search
 *   frictions (genuine coordination function) while simultaneously extracting
 *   from displaced local labor and welfare system fiscal capacity (asymmetric
 *   extraction). The integration-primary reading is one of three competing
 *   institutional framings of the same contested kernel:
 *   member_sovereignty_primary asserts that national welfare states retain
 *   closure authority, and selective_solidarity proposes tiering welfare
 *   access by contribution history. The three readings coexist as live
 *   positions within different member states and EU institutions, with no
 *   institutional settlement foreclosing any of them. The constraint's
 *   evolution shows increasing suppression and extractiveness as ECJ case law
 *   (Carpenter, Bidar, Dano, Mengesha progression) expands mobility rights
 *   while member states lack fiscal mechanisms to absorb costs. Theater ratio
 *   reflects the performative maintenance of national welfare sovereignty by
 *   administrative structures that no longer control actual eligibility
 *   gates.
 *
 * KEY AGENTS:
 *   - Mobile EU Workers: Primary beneficiary (institutional/arbitrage) — access to full welfare services in receiving states, labor market mobility without skill credential friction, supranational citizenship status
 *   - Displaced Local Labor: Primary victim (powerless/trapped) — wage suppression, skill devaluation, labor market restructuring without adjustment support or exit options
 *   - Receiving State Welfare System: Secondary beneficiary and victim (powerful/constrained) — coordinates social insurance while bearing fiscal costs of expanded beneficiary set; constrained by ECJ authority to exclude citizens of member states
 *   - Receiving State Employers: Beneficiary (powerful/mobile) — access to flexible labor, wage moderation benefits, skill sourcing; mobile exit via relocation or non-EU recruitment
 *   - EU Adjustment Funding Coalitions: Organized agents (organized/constrained) — building adjustment transition pathways and welfare harmonization mechanisms; see sunset potential
 *   - National Welfare State Institutions: Institutional actors (institutional/constrained) — maintain appearance of national welfare sovereignty while experiencing progressive authority erosion via case law
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangement (ECJ doctrine, lack of welfare harmonization) as logical requirement of federation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.48).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.62).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.48).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "Federation Membership Obligations (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, 'a600e2cc-fce0-4731-8d9b-a422421d7e4e').
narrative_ontology:cs_kernel_codification('a600e2cc-fce0-4731-8d9b-a422421d7e4e', formalized).
narrative_ontology:cs_authority_grounding('a600e2cc-fce0-4731-8d9b-a422421d7e4e', extraction).
narrative_ontology:cs_interpretation_layer_present('a600e2cc-fce0-4731-8d9b-a422421d7e4e').
narrative_ontology:cs_reading_relation('a600e2cc-fce0-4731-8d9b-a422421d7e4e', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('a600e2cc-fce0-4731-8d9b-a422421d7e4e', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('a600e2cc-fce0-4731-8d9b-a422421d7e4e', foundational, free_movement_constitutive_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('a600e2cc-fce0-4731-8d9b-a422421d7e4e', free_movement_constitutive_citizenship, deontological).
narrative_ontology:cs_axiom('a600e2cc-fce0-4731-8d9b-a422421d7e4e', foundational, welfare_boundary_subordinate_to_integration).
narrative_ontology:cs_axiom_status(welfare_boundary_subordinate_to_integration, holdable).
narrative_ontology:cs_axiom_grounding('a600e2cc-fce0-4731-8d9b-a422421d7e4e', welfare_boundary_subordinate_to_integration, conventional).
narrative_ontology:cs_reference_frame('a600e2cc-fce0-4731-8d9b-a422421d7e4e', constitutional_federation_model).
narrative_ontology:cs_drift_state('a600e2cc-fce0-4731-8d9b-a422421d7e4e', contemporary_welfare_state_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a600e2cc-fce0-4731-8d9b-a422421d7e4e', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, supranational_integration_project).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, welfare_system_fiscal_capacity).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_social_contract_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED LOCAL LABOR (SNARE) — Cannot exit the labor market restructuring imposed by free movement. Bears full adjustment costs (wage suppression, skill devaluation, relocation pressure) with no compensation mechanism or exit alternative. No alternative labor markets available locally; organized collective action is suppressed by skill heterogeneity and precarity. Maximum extractive experience — the constraint extracts their labor market position and transfers it to mobile EU workers without reciprocal benefit.
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MOBILE EU WORKERS (ROPE) — Experience the constraint as pure coordination mechanism. Free movement rules enable labor market matching, reduce search frictions, and solve collective action problems around skill portability and credential recognition. Benefits from access to welfare services in receiving states as a coordination dividend (healthcare, family benefits, unemployment insurance). Perceived as solving genuine cooperation problems, not as extraction. Arbitrage exit option (can relocate to other member states if receiving state becomes unattractive).
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RECEIVING STATE EMPLOYERS (TANGLED ROPE) — Benefit from access to mobile labor (wage moderation, skill flexibility, reduced training costs) while also bearing some adjustment costs (wage competition in some sectors, skill mismatches). Genuinely participate in coordination (hiring, training, workplace integration) while extracting labor cost benefits. Mobile exit option: can shift production to other member states or recruit from non-EU sources. Effective extraction is substantial but not maximal because exit capacity is real and some coordination benefits (labor market stability) flow back.
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: RECEIVING STATE WELFARE SYSTEM (TANGLED ROPE) — Coordinates redistribution and social insurance (genuine function) while extracting from fiscal sustainability through expanded beneficiary set without equivalent revenue expansion. Mobile workers contribute through payroll taxes but often have lower lifetime contribution profiles and higher utilization of working-age benefits (child allowances, unemployment). Constrained exit: cannot exclude EU citizens without violating Treaty commitments and facing ECJ sanctions; structural adjustment is the only path. Effective extraction is significant: the welfare system bears the cost of non-contributory benefit access while the integration project captures the political benefit of mobility rights.
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EU ADJUSTMENT FUNDING COALITIONS (SCAFFOLD) — Organized agents (regional development funds, labor transition programs, EU Commission mobility initiatives) see the constraint as temporary coordination failure with a sunset: phased welfare harmonization, transitional funding for displaced workers, and labor market adjustment programs are building alternatives to unmanaged free movement. Low effective extraction because these coalitions have agency and see an exit path through welfare convergence and adjustment financing. Sunset mechanism: as welfare systems harmonize and labor market adjustment capacity builds, the asymmetry declines. Estimated sunset: 15-25 years if solidarity mechanisms fully operationalize.
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATIONAL WELFARE STATE INSTITUTIONS (PITON) — The Bismarckian welfare model (linked to national employment and contribution history) persists institutionally despite structural incompatibility with free movement of persons. National welfare administrators go through the motions of administering benefits, but the legitimating principle (national social contract, earned entitlement) has been hollowed out by supranational mobility rights. Institutions maintain the appearance of national welfare sovereignty while ECJ case law progressively erodes gatekeeping authority. Theater ratio reflects this performative maintenance: member states appear to control welfare eligibility while the constraint systematically narrows their actual authority. High institutional inertia — welfare bureaucracies continue operating as if national closure is possible, even as individual case law accumulates.
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this constraint appears as an immutable property of federal systems: mobility is incompatible with closed welfare boundaries, and federal membership inherently requires welfare boundary subordination to citizenship rights. The naturalizing framing: 'This is what federation means — states that want to join can't have exclusive welfare closure.' However, the structural data contradicts this naturalization. The constraint is produced by specific institutional choices (ECJ authority expansion, Treaty Article 45 interpretation, lack of welfare harmonization mechanisms), not by logical necessity. The false summit reveals that 'federation requires this' naturalizes what is actually contingent institutional path-dependency.
constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_obligations__integration_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federation_membership_obligations__integration_primary, TR),
    TR >= 0.70.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The integration-primary reading produces systematic extraction from displaced local labor (wage suppression, skill devaluation) and from welfare system fiscal sustainability (expanded beneficiary set, non-contributory access). However, the extraction is not total (Snare threshold ≥0.46) because genuine coordination benefits exist (labor market matching, skill portability, reduced hiring frictions) and some adjustment mechanisms operate (EU solidarity funds, regional development programs). The measurement trajectory (0.32 → 0.48 over 20 years) reflects ECJ case law expanding mobility rights faster than welfare harmonization mechanisms can build. Suppression (0.62): Moderate-high. Significant barriers prevent local labor and welfare systems from opposing the constraint: ECJ supremacy doctrine, treaty commitments, political costs of explicit discrimination against EU citizens, collective action problems among displaced workers. Suppression is not total because some opposition operates (national parliamentary resistance, selective non-compliance with case law, political backlash in high-mobility receiving states). Theater ratio (0.55): Moderate. The constraint involves both genuine coordination (labor market integration, skill recognition) and performative maintenance (national welfare systems administering benefits while formal authority over eligibility erodes; member states negotiating welfare clauses while ECJ narrowly interprets them). The theater ratio reflects that approximately 55% of the constraint's operation is institutional maintenance of national welfare forms that no longer carry substantive control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a wide perspectival gap across the observation site. Mobile EU workers perceive pure coordination (Rope) — the constraint solves their labor market search problem and creates access to portable social insurance. Receiving state employers perceive mixed coordination and extraction (Tangled Rope) — they benefit from labor access and cost moderation while bearing some adjustment costs. Welfare systems perceive mixed function and fiscal extraction (Tangled Rope) — the constraint coordinates redistribution while expanding the beneficiary set beyond the contributory base. Adjustment coalitions perceive temporary coordination failure with sunset potential (Scaffold) — they see adjustment programs and welfare harmonization as pathways to exit. National welfare institutions perceive degraded institutional form (Piton) — they maintain the appearance of welfare sovereignty while experiencing progressive authority erosion. Displaced local labor perceive pure extraction (Snare) — they bear adjustment costs with no compensation and no exit. The analytical observer risks naturalizing the constraint as a logical requirement of federation (Mountain) — but the structural data reveals this as a false summit, since the constraint is produced by specific institutional choices (ECJ authority doctrine, lack of welfare harmonization mechanisms) rather than by logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed. The derivation chain produces appropriate d values from the declared beneficiary/victim relationships and exit options: mobile workers (beneficiary + arbitrage) → low d → negative chi; displaced labor (victim + trapped) → high d → high chi; welfare systems (victim + constrained) → high d → high chi despite nominal beneficiary status; employers (beneficiary + mobile) → moderate d → moderate chi. The perspectival gap is fully explained by the structural differentiation of exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is resolved through the reading contest itself. The constraint classifies as Tangled Rope under the integration-primary reading because it genuinely coordinates labor market matching while simultaneously extracting from displaced labor and welfare system fiscal capacity. The apparent tension between coordination and extraction functions is not a classification error but an accurate reflection of the reading's structural commitments: mobility rights are asserted as both coordination mechanism (solving labor market matching problems) and as fundamental citizenship entitlements (requiring welfare access regardless of contribution). A different reading (member_sovereignty_primary) would classify the same structural phenomenon as Snare (extraction masked by coordination rhetoric) or as Rope with severe welfare costs. Selective_solidarity would classify it as Scaffold with tiered sunset mechanisms. The mandatrophy is not resolved by finding the 'true' classification but by recognizing that the three readings instantiate genuinely different commitments to the same kernel, and the classification follows from the reading chosen. This constraint demonstrates that mandatrophy resolution sometimes means accepting that multiple readings are coherent — the question is not which classification is correct but which institutional reading will govern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_harmonization_feasibility,
    'Is upward welfare harmonization structurally achievable within the current EU institutional framework, or does path-dependency lock member states into asymmetric contribution profiles?',
    'Longitudinal analysis of welfare spending convergence, fiscal capacity assessments, and political willingness to transfer from high-welfare to low-welfare states over 10-20 year periods',
    'If harmonization is feasible: the scaffold perspective''s sunset is real, and the constraint is temporary. If path-dependent lock is structural: the constraint becomes permanent, reclassifying as snare from the welfare system perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_harmonization_feasibility, empirical, 'Whether welfare harmonization is structurally achievable').

omega_variable(
    ecj_authority_trajectory,
    'Will ECJ case law continue to expand mobility rights over national welfare gates, or will political backlash (expressed through treaty amendment or structural non-compliance) arrest the trajectory?',
    'Analysis of ECJ case law velocity and scope expansion; correlation with member state compliance rates and explicit legal pushback; assessment of political feasibility for treaty revision or judicial review limitations',
    'If trajectory continues: integration-primary reading remains locked in. If arrested: member_sovereignty_primary reading gains structural plausibility, and the kernel contest resolves toward selective_solidarity compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_authority_trajectory, empirical, 'Direction and sustainability of ECJ mobility rights expansion').

omega_variable(
    substitution_vs_displacement_empirics,
    'Do mobile EU workers substitute for local labor (filling vacancies, enabling growth) or displace local labor (taking positions at lower wage offers, suppressing local wages)?',
    'Econometric analysis of local wage effects, employment displacement, and sectoral composition changes in high-mobility receiving states; comparison of wage trends in sectors with high vs low EU worker penetration',
    'If primarily substitution: constraint is coordination (Rope becomes primary classification). If primarily displacement: constraint is extraction (Snare classification confirmed for displaced labor perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_vs_displacement_empirics, empirical, 'Whether mobile workers substitute for or displace local labor').

omega_variable(
    reading_contest_institutional_basis,
    'Which reading of the federation_membership_obligations kernel is institutionally dominant: integration-primary (this reading), member_sovereignty_primary, or selective_solidarity?',
    'Analysis of ECJ doctrine (Carpenter, Bidar, Dano, Mengesha case law progression); comparison of treaty language (Article 45, Article 21) with national constitutional commitments; assessment of member state implementation variance',
    'This omega documents the constitutional ambiguity at the kernel level. The three readings (integration-primary, member_sovereignty_primary, selective_solidarity) each find support in treaty text and case law but no single reading is textually mandated. The engine computes which reading is foreclosed, coexists, or influences based on structural consistency. This omega records the absence of a determinate institutional anchor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_institutional_basis, conceptual, 'Which reading of federation membership obligations is institutionally dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_integ_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fed_integ_tr_t10, federation_membership_obligations__integration_primary, theater_ratio, 10, 0.52).
narrative_ontology:measurement(fed_integ_tr_t20, federation_membership_obligations__integration_primary, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(fed_integ_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fed_integ_be_t10, federation_membership_obligations__integration_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(fed_integ_be_t20, federation_membership_obligations__integration_primary, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fed_integ_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fed_integ_su_t10, federation_membership_obligations__integration_primary, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fed_integ_su_t20, federation_membership_obligations__integration_primary, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, welfare_state_fiscal_sustainability).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, labor_market_segmentation_by_migrant_status).

% DUAL FORMULATION NOTE:
% The federation_membership_obligations kernel has three structurally distinct constraint readings: (1) integration_primary (this file) — free movement is constitutive, welfare boundaries must yield; (2) member_sovereignty_primary — national welfare closure authority is retained; (3) selective_solidarity — welfare access is tiered by contribution history. Each reading has its own ε, its own beneficiary/victim structure, and its own classification type. They are not three perspectives on one constraint but three separate constraints instantiated from the same contested institutional kernel. All three must be modeled as separate stories linked via network.affects_constraints to enable the engine to detect the reading contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
