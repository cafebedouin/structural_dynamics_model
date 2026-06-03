% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement as Constitutive of Single Market (Integration Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The integration-primary reading of federation membership treats free
 *   movement of workers as a constitutive feature of the single market — not
 *   merely a policy tool but a foundational principle such that restrictions
 *   are presumptively illegitimate unless narrowly justified by genuine
 *   public policy exceptions. This constraint operates at the federation
 *   level, governing member states' authority to regulate labor market access
 *   and residence. From the perspective of mobile workers and transnational
 *   firms, free movement functions as pure coordination enabling efficient
 *   allocation of labor and capital. From the perspective of local labor
 *   markets and national welfare systems, the same constraint operates as
 *   extraction: downward pressure on wages, fiscal strain on social benefits,
 *   and suppression of local protective regulation that would be standard
 *   within a sovereign nation-state. The measurement trajectory shows
 *   increasing extractiveness (0.38 → 0.52) and suppression (0.55 → 0.68)
 *   over a 10-year interval, indicating that the integration-primary reading
 *   has become more institutionally entrenched and that accommodations to
 *   local labor markets and welfare systems have been narrowed rather than
 *   expanded. The low theater ratio (0.30–0.35) indicates that the constraint
 *   functions through substantive legal rules and institutional enforcement,
 *   not performative ritual — federation authorities genuinely prevent member
 *   states from restricting free movement, and member states genuinely comply
 *   or face legal consequences.
 *
 * KEY AGENTS:
 *   - Mobile Workers: Primary beneficiary (powerful/mobile) — gain access to expanded labor markets and arbitrage opportunities across federation member states
 *   - Transnational Firms: Secondary beneficiary (powerful/arbitrage) — benefit from labor cost differentials and cross-border hiring without regulatory friction
 *   - Local Labor Markets: Primary victim (powerless/trapped) — cannot restrict in-migration; bear wage compression and employment displacement costs with no meaningful exit option
 *   - National Welfare Systems: Secondary victim (institutional/constrained) — face fiscal pressure from in-migration; constrained by federation law from means-testing or residency-based restrictions
 *   - Member States: Mixed institutional position (institutional/constrained) — benefit from single market but constrained by integration-primary rules; can appeal to narrow exceptions but face high legal burden
 *   - Labor Unions: Organized victim with limited agency (organized/constrained) — have capacity to organize workers but suppressed from using free movement restrictions as leverage; wage-setting power eroded by expanded labor supply
 *   - Federation Authority: Institutional coordinator (institutional/arbitrage) — primary beneficiary of rule-making power; coordinates single market integration; high arbitrage option to revise rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.52).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.68).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement as Constitutive of Single Market (Integration Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '29ad117f-4230-4dbd-8254-247ff05fc3e8').
narrative_ontology:cs_kernel_codification('29ad117f-4230-4dbd-8254-247ff05fc3e8', formalized).
narrative_ontology:cs_authority_grounding('29ad117f-4230-4dbd-8254-247ff05fc3e8', extraction).
narrative_ontology:cs_interpretation_layer_present('29ad117f-4230-4dbd-8254-247ff05fc3e8').
narrative_ontology:cs_reading_relation('29ad117f-4230-4dbd-8254-247ff05fc3e8', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('29ad117f-4230-4dbd-8254-247ff05fc3e8', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('29ad117f-4230-4dbd-8254-247ff05fc3e8', foundational, free_movement_constitutive_of_market).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_market, holdable).
narrative_ontology:cs_axiom_grounding('29ad117f-4230-4dbd-8254-247ff05fc3e8', free_movement_constitutive_of_market, instrumental).
narrative_ontology:cs_axiom('29ad117f-4230-4dbd-8254-247ff05fc3e8', foundational, restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('29ad117f-4230-4dbd-8254-247ff05fc3e8', restrictions_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('29ad117f-4230-4dbd-8254-247ff05fc3e8', supranational_labor_integration_framework).
narrative_ontology:cs_drift_state('29ad117f-4230-4dbd-8254-247ff05fc3e8', contemporary_post_2008, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('29ad117f-4230-4dbd-8254-247ff05fc3e8', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, transnational_firms).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, integrated_capital_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, subnational_regulatory_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE WORKER (ROPE) — Experiences free movement as pure coordination benefit. Can access labor markets across federation member states without restriction; wages and opportunity set expand. Exit is real (can choose where to work or reside). Benefits exceed costs — this agent is the canonical beneficiary. Perceives the constraint as coordination enabling arbitrage among local opportunities.
constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 2: LOCAL LABOR MARKET (SNARE) — Cannot restrict in-migration without violating federation rules; bears full cost of wage compression and employment displacement. No meaningful exit option — the constraint is imposed externally by federation membership. Suppression is structural: state restrictions on free movement are presumptively illegitimate and face high legal burden. The local labor market has no advocate within the integration-primary framework and cannot organize collective protection.
constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MEMBER STATE (TANGLED ROPE) — Constrained by federation rules (exit cost is federation exit, which is existential/political suicide for most states). Benefits from the single market integration: capital flows, access to larger labor supply, economies of scale. Harms from free movement: fiscal pressure on welfare, labor market disruption, political backlash. The state experiences genuine coordination (single market benefits) alongside asymmetric extraction (local adjustment costs pushed downward to subnational actors and vulnerable workers). Has partial agency through 'public policy' exceptions but they are narrowly construed.
constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION (TANGLED ROPE) — Organized capacity to resist extraction through collective action (strikes, political pressure) but constrained by federation legal structure that treats union restrictions on free movement as themselves illegitimate restraints on competition. Union benefits from single market scale but loses wage-setting power in segmented labor market. Has agency but insufficient leverage — high suppression of alternative coordination mechanisms (wage floors, sectoral agreements that might discriminate by origin).
constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERATION AUTHORITY (ROPE) — Primary coordinator of single market integration. Benefits from rule-setting power and political capital from market liberalization. Experiences the constraint as coordination mechanism: free movement rules enable capital, labor, and goods to allocate efficiently across federation. High arbitrage option — can revise rules through institutional process. Does not experience extraction because authority derives benefit from the constraint itself.
constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, economic integration and labor mobility are treated as natural or inevitable consequences of market organization and federation membership. Restrictions appear as market distortions or artificial barriers. This perspective risks naturalizing what is actually a contingent institutional reading: free movement as constitutive IS a choice, not a law of nature. The constraint appears immutable because integration-primary framers define any restriction as illegitimate 'by definition,' but this is performative, not descriptive.
constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_treaty__integration_primary, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Mobile workers and transnational firms capture clear benefits — access to expanded labor markets, wage arbitrage, hiring flexibility. Local labor markets and welfare systems bear proportional costs — wage pressure, fiscal strain, regulatory constraint. The asymmetry is real but not total: member states retain some agency through exceptions and can build complementary policies (education, retraining, sectoral support). The trajectory toward 0.52 reflects increasing entrenchment of integration-primary rules and decreasing accommodation for local adjustment. Suppression (0.68): High. Member states are structurally prevented from using standard labor market and welfare protections (apprenticeship requirements, wage floors, sectoral collective agreements) if they discriminate by origin or effectively restrict free movement. The 'public policy exception' is narrow and rarely succeeds. Suppression increased over the interval as case law narrowed exception scope. Theater ratio (0.35): Low. The integration-primary constraint operates through substantive institutional rules and legal enforcement, not performative ritual. Federation courts actively prevent member state restrictions; member states comply or defect. No significant theater accumulation, indicating the constraint's function is substantive rather than maintaining legitimacy through appearance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a sharp perspectival divide. Mobile workers and federation authorities see coordination (rope) — the constraint solves the problem of allocating labor and capital efficiently across federation space. Local labor markets see pure extraction (snare) — the constraint is imposed externally and prevents self-protection. Member states see mixed coordination and extraction (tangled_rope) — they benefit from single market scale but bear adjustment costs and lose regulatory authority. Organized workers (labor unions) see tangled_rope with insufficient agency — they benefit from market scale but lack power to influence the constraint's terms. The analytical observer risks seeing natural law (mountain) — market integration and labor mobility treated as inevitable — but the structural data reveals this as naturalization of a contested institutional reading. The perspectival gap indicates that the integration-primary reading has genuine beneficiaries (mobile workers, transnational firms, integrated capital) and genuine victims (local labor markets, subnational workers, national welfare systems) — the constraint is not coordination pure and simple, but rather coordination for some combined with extraction from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality and experienced extractiveness depends on the agent's power level, exit options, and beneficiary/victim status. Mobile workers (powerful/mobile/beneficiary) experience low directionality (d ≈ 0.15) — they are the targets of the coordination and have exit options within the framework (can move, can return home, can access transnational opportunities). Local labor markets (powerless/trapped/victim) experience high directionality (d ≈ 0.95) — they are the targets of extraction and have zero meaningful exit option. Member states (institutional/constrained/mixed) experience moderate directionality (d ≈ 0.55) — they are both beneficiaries (single market gains) and victims (regulatory constraint), and their exit option is federation exit (existential cost but not zero). Suppression is a structural property — member states cannot use standard protective measures without violating integration-primary rules. This suppression is not dependent on directionality or power; it is a feature of the constraint's design.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_system_vulnerability_threshold,
    'At what level of in-migration does welfare system fiscal pressure become unsustainable, and who bears adjustment costs?',
    'Time-series analysis of welfare expenditure relative to in-migration flows; identification of breakpoint or saturation point; tracking of policy adjustments (means-testing, residency requirements, benefit reductions)',
    'If threshold is high (>15% in-migration over decade): welfare systems demonstrate robustness; extraction is lower than current assessment. If threshold is low (<5%): current extractiveness underestimated; victim set includes welfare beneficiaries as primary targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_vulnerability_threshold, empirical, 'Fiscal sustainability threshold for welfare systems under free movement').

omega_variable(
    labor_market_segmentation_persistence,
    'Does free movement eliminate or entrench labor market segmentation (native vs migrant workers, primary vs secondary sectors)?',
    'Longitudinal wage gap analysis by origin; sector concentration tracking; comparison of segmentation under integration-primary vs subsidiarity-balance jurisdictions',
    'If segmentation increases: free movement creates dual labor market (snare at local level confirmed). If segmentation decreases: integration-primary achieves stated goal (constraint better characterized as rope than tangled_rope for local labor markets).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_segmentation_persistence, empirical, 'Whether free movement eliminates or entrenches labor market segmentation').

omega_variable(
    restrictive_exception_scope_drift,
    'Do ''public policy'' and ''public security'' exceptions to free movement grow or shrink over time, and who controls their definition?',
    'Case law analysis of exception invocations and success rates; comparative study of member states'' exception usage; tracking of whether exceptions are narrowly construed or permissively applied',
    'If exceptions narrow: integration-primary suppression is structural and durable. If exceptions expand: member states are carving de facto subsidiarity into case law; the reading is drifting toward subsidiarity_balance in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restrictive_exception_scope_drift, empirical, 'Trajectory of exceptions to free movement rules and their scope').

omega_variable(
    reading_kernel_contest,
    'Is the integration-primary reading the dominant institutional framing, or is it contested by equally powerful sovereignty-primary or subsidiarity-balance readings?',
    'Analysis of jurisprudence frequency, legislative intent statements, enforcement priorities, and institutional authority statements; comparison of which reading is used to justify policy decisions',
    'If integration-primary is dominant: the constraint''s extractiveness reflects institutional power concentration (current assessment 0.52 is accurate). If equally contested: suppression is lower (member states retain real exit options through alternative readings); extractiveness should be 0.35–0.40 (tangled_rope downgrading toward rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Institutional dominance of integration-primary reading vs sibling readings').

omega_variable(
    committer_reading_choice,
    'This constraint instantiates the integration-primary reading of the federation_membership_treaty kernel. What structural features distinguish this reading from sovereignty-primary and subsidiarity-balance, and could an alternative reading produce a different constraint entirely?',
    'Comparison of extracted beneficiary/victim sets, suppression profiles, and perspective classifications across the three readings. If sovereignty-primary reading is instantiated: local labor markets shift to beneficiary position (retained control over their markets); mobile workers shift to constrained/victim position. If subsidiarity-balance is instantiated: suppression lowers and multiple perspectives classify as rope or scaffold.',
    'The three readings are structurally distinct constraints with different ε values, not observational variants of one constraint. integration-primary ε ≈ 0.52 (tangled_rope). sovereignty-primary ε ≈ 0.15 (rope favoring local protection). subsidiarity-balance ε ≈ 0.30 (rope with active coordination across scales). Each reading is legitimate within its own framework; none forecloses the others completely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_choice, conceptual, 'Kernel reading identity: integration-primary vs sibling readings (sovereignty-primary, subsidiarity-balance)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_int_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fed_int_tr_t5, federation_membership_treaty__integration_primary, theater_ratio, 5, 0.32).
narrative_ontology:measurement(fed_int_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(fed_int_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fed_int_be_t5, federation_membership_treaty__integration_primary, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(fed_int_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fed_int_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fed_int_su_t5, federation_membership_treaty__integration_primary, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(fed_int_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, transnational_labor_arbitrage).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, national_welfare_fiscal_burden).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel decomposes into three distinct constraints, each instantiating a different reading. integration_primary (this constraint, ε ≈ 0.52, tangled_rope) treats free movement as constitutive and suppresses national restrictions. sovereignty_primary (sibling constraint, ε ≈ 0.15, rope) treats free movement as conditional and preserves state authority. subsidiarity_balance (sibling constraint, ε ≈ 0.30, rope) treats free movement as real but bounded. Each reading has its own beneficiary/victim structure, suppression profile, and perspective distribution. The three readings coexist in actual federation jurisprudence and politics but compete for institutional dominance. The measurement trajectory (extractiveness rising 0.38 → 0.52) reflects integration-primary institutional ascendancy over the 10-year interval.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
