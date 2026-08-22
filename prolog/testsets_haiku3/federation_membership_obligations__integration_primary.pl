% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Integration Primary: Free Movement Overrides Welfare Closure
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the EU federation-membership
 *   kernel: free movement is constitutive of EU citizenship and single market
 *   functioning; member state welfare boundaries must yield to mobility
 *   rights. Under this reading, a mobile EU citizen has a right to full labor
 *   market access and equal welfare treatment in any member state,
 *   enforceable by supranational courts against member state legislatures.
 *   The reading generates a clear beneficiary set (mobile workers,
 *   integration advocates, supranational institutions) and a clear victim set
 *   (displaced local labor, welfare-system administrators managing cost
 *   pressures, low-skill incumbent workers). The constraint is actively
 *   enforced through ECJ case law (Advocates General opinions, preliminary
 *   rulings that preempt national law) and is contested by member state
 *   governments and labor constituencies. It is one of three sibling readings
 *   of the same kernel (member_sovereignty_primary and selective_solidarity
 *   represent alternative framings); this story instantiates the
 *   integration-primary reading only, with omegas documenting the contest.
 *
 * KEY AGENTS:
 *   - Mobile EU workers: primary beneficiary; gain full welfare access and labor market entry in any member state; high exit optionality (arbitrage-capable across labor markets)
 *   - Displaced local labor: primary victim; face wage competition and unemployment in receiving states; trapped by geography and identity (low spatial/economic mobility)
 *   - Welfare system administrators: institutional payer; must expand benefits while managing fiscal pressures; formally in agenda-setter role but substantively constrained by ECJ rulings
 *   - European Court of Justice: agenda-setter; expands its authority via integration-maximizing case law that subordinates welfare closure to free movement rights
 *   - Supranational integration constituency: secondary beneficiary; Commission, Parliament, transnational advocacy; benefits from each expansion of integration principles
 *   - Member state governments: excluded from beneficiary calculus but mediators of conflict; represent electoral constituents bearing costs but constrained by EU law obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.54).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Integration Primary: Free Movement Overrides Welfare Closure").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '05efe518-39e8-48b9-8805-687bb30eaae9').
narrative_ontology:cs_kernel_codification('05efe518-39e8-48b9-8805-687bb30eaae9', fixed_text).
narrative_ontology:cs_authority_grounding('05efe518-39e8-48b9-8805-687bb30eaae9', extraction).
narrative_ontology:cs_interpretation_layer_present('05efe518-39e8-48b9-8805-687bb30eaae9').
narrative_ontology:cs_reading_relation('05efe518-39e8-48b9-8805-687bb30eaae9', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('05efe518-39e8-48b9-8805-687bb30eaae9', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('05efe518-39e8-48b9-8805-687bb30eaae9', foundational, free_movement_constitutive_of_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('05efe518-39e8-48b9-8805-687bb30eaae9', free_movement_constitutive_of_citizenship, deontological).
narrative_ontology:cs_axiom('05efe518-39e8-48b9-8805-687bb30eaae9', foundational, integration_primacy_over_welfare_closure).
narrative_ontology:cs_axiom_status(integration_primacy_over_welfare_closure, holdable).
narrative_ontology:cs_axiom_grounding('05efe518-39e8-48b9-8805-687bb30eaae9', integration_primacy_over_welfare_closure, instrumental).
narrative_ontology:cs_reference_frame('05efe518-39e8-48b9-8805-687bb30eaae9', single_market_integration_framework).
narrative_ontology:cs_drift_state('05efe518-39e8-48b9-8805-687bb30eaae9', contemporary_fiscal_constraint_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05efe518-39e8-48b9-8805-687bb30eaae9', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, supranational_integration_constituency).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, welfare_system_administrators).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, low_skill_incumbent_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_state_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers from any EU member state have the right to work, reside, and access full welfare benefits in any other member state without permit restrictions. They gain access to higher-wage labor markets and equal social protection; their exit option from their home state is structurally enforced by law. This reading places them in the full beneficiary set of receiving states' welfare systems from arrival.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, arbitrage, global).

% Incumbent workers in receiving states, especially in lower-skill occupations, face wage competition and employment displacement when mobile EU labor enters the labor market at scale. They bear the adjustment cost — downward wage pressure, reduced job availability, or forced retraining — but have limited exit options (immobile, tied to home communities, home-country employment may be worse). They do not benefit from the mobility rights that displace them.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, trapped, national).

% Member state governments and welfare bureaucracies must extend full benefits to mobile workers while maintaining fiscal sustainability and managing political pressure from displaced constituencies. They cannot close welfare borders without violating EU law; their enforcement role is to administer a system whose boundaries they did not choose and cannot revise unilaterally. Some use agenda-setter status to shape implementation details (residency verification, benefit access timing) within the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, welfare_system_administrators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, welfare_system_administrators, agenda_setter).

% Concentrated in sectors most exposed to mobile labor (agriculture, hospitality, construction, care work). Face both wage competition and welfare-access congestion (longer waiting times, reduced per-capita benefit allocation if funding is fixed). Trapped by local identity (roots in community, family, home-language capital), unable to exit to better labor markets or states where they might re-skill. Bear a disproportionate share of adjustment costs relative to their power to resist.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, low_skill_incumbent_workers, payer,
    powerless, biographical, identity_locked, national).

% European Commission, ECJ, Parliament, and transnational civil society organizations advocating for integration. They benefit from each expansion of free movement rights (it advances the integration project, strengthens supranational authority, and vindicates the founding principles of EU citizenship). They can set the framing through case law, legislative initiative, and discourse dominance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, supranational_integration_constituency, beneficiary,
    institutional, civilizational, arbitrage, global).

% Represent the electoral constituencies that bear the adjustment costs (displaced labor, local taxpayers) but are bound by EU law obligations. They formally participate in EU lawmaking but are structurally overruled on welfare-closure questions by ECJ case law that interprets free movement rights as superior to welfare sovereignty. They are excluded from the reading's beneficiary/victim calculus because they are not the seated parties — but they mediate the conflict between mobile workers and local constituencies.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, observer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, excluded).

% Interprets EU law and has consistently ruled that free movement and non-discrimination rights take precedence over member state welfare-closure claims. Expands its own authority with each ruling that subordinates national welfare boundaries to supranational integration principles. Operates with no direct electoral accountability to local constituencies bearing costs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, european_court_of_justice, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Must fund welfare expansion to include mobile workers without corresponding tax base growth (mobile workers may have lower-than-average tax contribution initially, or contribute to lower-tax jurisdictions). Bear the fiscal cost indirectly through benefit reduction, tax increases, or service degradation for incumbent populations. Politically organized but constitutionally unable to exclude non-residents from benefits.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_taxpayers, payer,
    organized, biographical, constrained, national).

% In lower-wage member states, workers gain access to higher-wage labor markets abroad and can exit worse-paid home employment. They benefit from remittances and outside opportunities but also lose workers from home labor markets and tax bases. The net effect is constituency-specific: some regions lose young workers and economic dynamism; others gain capital flows.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, origin_state_labor_constituencies, observer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, supranational_integration_constituency).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes labor market fragmentation by eliminating borders to worker mobility and establishing equal treatment across member states. This allows efficient allocation of labor across geographies, reduces transaction costs of cross-border work, and enables the single market's functioning by permitting workers to follow economic opportunities without permits.
% TRANSFER_FUNCTION: Transfers welfare access rights from residence-based or contribution-based closure to membership-based entitlement. Mobile workers gain full beneficiary status in receiving states' welfare systems upon arrival; displaced local workers and receiving-state taxpayers bear the cost of expanded welfare populations without proportional fiscal capacity growth. The transfer is mediated through supranational courts enforcing integration principles over national welfare boundaries.
% ABSENT_VOICES: Workers in origin states most exposed to brain drain; low-skill local workers in receiving states who have no seat at EU-level decision-making (they are excluded from ECJ proceedings, have limited EU parliamentary representation, and are overruled by member state executives constrained by EU law); welfare-dependent non-mobile populations in receiving states who do not migrate but do experience congestion and reduced per-capita benefit access. These constituencies would argue for tiered welfare access, labor-impact assessments, and receiving-state border discretion, but are structurally absent from the supranational decision-making apparatus that enforces integration primacy.
% DISAPPEARANCE_RATIONALE: If this constraint (free movement as constitutive of EU citizenship, with welfare boundaries yielding to mobility rights) disappeared overnight, member states would rapidly reimpose welfare-access restrictions on non-residents, labor markets would re-fragment along national borders, social policy would revert to citizenship-based closure, and the single market would lose a foundational principle. The EU project itself would fundamentally reorient toward intergovernmentalism rather than integration primacy. Member states would reorganize their welfare systems around residence and contribution history, not EU citizenship. The shift would be profound and contested.
% FOUNDING_PROBLEM: Post-WWII economic integration required removing labor market barriers to enable efficient resource allocation and prevent nationalist competition that had driven conflict. The Common Market (later Single Market) logic: free movement of workers is as essential as free movement of capital and goods. Economic integration requires mobility to work; welfare harmonization should follow, not precede, mobility rights.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and ECJ attest the founding problem remains live: labor market segmentation and welfare closure would undermine the single market. Economically oriented scholars and integration advocates corroborate this reading. However, member state governments, labor unions in receiving states, and social-policy analysts argue the founding problem has been substantially solved (European labor markets have functionally integrated; welfare fragmentation is now the problem, not market closure) and the arrangement persists as a transfer mechanism overriding fiscal sustainability and democratic closure. Legislative hearings in receiving states and academic literature from outside the supranational beneficiary circle support the contested/obsolescence reading.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1992) to 0.68 (2024) because the constraint's scope and enforcement expand: migration volumes grow, ECJ case law becomes more expansive (Maastricht→Amsterdam→Nice→Lisbon trajectory of integration-favorable rulings), and member states' capacity to manage welfare boundaries independently erodes. Suppression requirement also rises (0.28→0.54) because the constraint's persistence depends on actively suppressing member state boundary-setting authority through legal override; without ECJ enforcement, welfare closure would re-emerge rapidly. Theater ratio is low-to-moderate (0.08→0.22) because the constraint has genuine coordination function (labor market integration) but an increasing share of enforcement activity is devoted to defending welfare-access expansion against member state and local labor resistance — the functional surplus is real but the extractive element is growing. The time grid is shared across all three metrics; measurements are observed at historical intervals where data on migration flows, ECJ rulings, and welfare-access patterns are available.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (ECJ, Commission), this is a triumph of law over nationalist boundary-drawing; from the victim seats (displaced labor, welfare administrators), it is coercive closure of their ability to manage their own institutions and labor markets. These are not reconcilable perspectives — they rest on different premises about sovereignty, justice, and the purpose of welfare states. The constraint FORCES a choice: integration primacy or welfare closure. This reading chooses integration primacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers are at d≈0.2 (full beneficiary: gain welfare access, unrestricted labor mobility, equal treatment — the constraint subsidizes them). Supranational integration constituency is at d≈0.15 (beneficiary: expanded authority, institutional power, vindication of founding principles). Displaced local labor is at d≈0.85 (full target: bear wage pressure, adjustment costs, no exit, no voice). Welfare administrators are at d≈0.72 (high target: must implement expansion they did not choose, under revenue constraints, against electoral pressure). Receiving-state taxpayers are at d≈0.68 (target: fund expansion without proportional capacity growth). These directionalities are derived from beneficiary/victim declarations + exit options; no override is needed. The asymmetry is structural: the constraint benefits those with arbitrage-capable exit and institutional power; it extracts from those with trapped or identity-locked exit and powerless position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor market barriers preventing efficient allocation) is contested as to whether it remains live. Economic integration has advanced substantially since 1992; the EU labor market is more integrated than most member states' internal labor markets were in the post-war period. Yet the constraint's enforcement has intensified (suppression requirement rising, theater ratio rising), which is consistent with a mandate that has become partially obsolete but is maintained by institutional inertia and supranational constituencies that benefit from it. The measurement series shows extraction accumulating while the founding-problem status shifts from 'live' (1992–2008) to 'contested' (2008–2024). This is the signature of a constraint whose functional justification has eroded but whose institutional enforcement has hardened — a candidate for mandatrophy. However, the constraint is not wholly obsolete: labor market fragmentation persists at the margins, and integration advocates argue the problem is structural (will re-emerge if constraint weakens). The classification as tangled_rope (rather than snare) reflects that genuine coordination function remains, but with asymmetric extraction and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Has the post-1992 expansion of EU labor market integration and cross-border work substantially solved the founding problem (labor market barriers preventing efficient allocation), or does the problem persist at a structural level that requires continued enforcement of free movement overrides?',
    'Counterfactual analysis: if member states were permitted to impose selective welfare closures and labor-market protections, would labor fragmentation re-emerge, or has integration become self-sustaining? Proxy: examine whether labor mobility persists or accelerates if enforcement is relaxed (natural experiments from UK post-Brexit, or member state legal challenges).',
    'If the founding problem is substantially solved and enforcement has persisted due to supranational constituency interest, the constraint reclassifies toward piton (performative maintenance of obsolete rule). If the problem remains structural, the constraint remains tangled_rope with justified enforcement. The measurement series (rising suppression_requirement) suggests the former, but the judgment is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s founding coordination problem remains live or has been substantially solved.').

omega_variable(
    welfare_closure_alternative_feasibility,
    'Could member states maintain welfare-system sustainability while respecting free movement rights through alternative mechanisms (e.g., tiered benefits by contribution history, short-term welfare exclusions for new arrivals, return-of-contribution rules) that do not require full welfare-boundary closure?',
    'Comparative analysis of member state welfare models and ECJ rulings on welfare-access conditions. Test whether Selective_Solidarity reading (tiered by contribution) could satisfy both mobile workers and member state fiscal concerns, or whether ECJ would overrule such compromises as discriminatory.',
    'If alternatives exist that ECJ permits, the integration-primary reading''s claim to uniqueness is weakened; the constraint becomes one point on a spectrum rather than a necessary enforcement. If ECJ systematically forecloses alternatives, the reading''s enforced primacy is confirmed, but at the cost of appearing impositioned rather than justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_closure_alternative_feasibility, conceptual, 'Whether welfare-closure alternatives exist that could satisfy both mobility and fiscal sustainability without full integration-primary enforcement.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.54) structural (legal barriers, ECJ enforcement, fiscal incapacity) or internalized (member state elites have adopted integration-primary beliefs and no longer resist closure override, even at electoral cost)?',
    'Trajectory analysis: if member state governments were permitted to close welfare borders without ECJ override, would they do so immediately, or have integration narratives become hegemonic such that closure would face internal resistance? Proxy: examine member state government positions in ECJ preliminary rulings (do they argue for closure, or have they internalized integration logic).',
    'If suppression is structural, relaxing ECJ enforcement would rapidly restore welfare closure. If internalized, the constraint might persist even with legal permission to override, because the premises that justify closure have been replaced. This affects the constraint''s classification: pure structural suppression = tangled_rope; internalized suppression = partial shift toward rope (coordination by adoption rather than coercion).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the constraint''s suppression of member state closure authority is structural or internalized.').

omega_variable(
    displacement_cost_distribution,
    'Are displacement costs (wage pressure, unemployment, welfare congestion) concentrated in specific regions and industries, or distributed broadly across member states?',
    'Regional labor market analysis and welfare-access data by receiving state and sector. Identify hotspot states/sectors where displacement is severe vs. dispersed states where effects are marginal.',
    'Concentrated costs amplify victim-seat resistance and make the constraint appear more extractive from the local perspective (high d for those seats). Distributed costs make the constraint appear more like a genuine coordination with incidental spillover. The visibility and political salience of the constraint depend on whether costs are concentrated or diffuse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_cost_distribution, empirical, 'Geographic and sectoral concentration of displacement costs from mobile worker influx.').

omega_variable(
    supranational_constituency_dependency,
    'Does the persistence of the integration-primary reading depend on the supranational constituency''s institutional power (ECJ, Commission) continuously enforcing it through case law and political pressure, or has integration primacy become self-sustaining through elite consensus and international norm diffusion?',
    'Counterfactual: if ECJ rulings shifted toward member_sovereignty_primary tomorrow, would integration-primary practices persist among member states and mobile workers, or would they collapse? Proxy: examine whether integrationist norms have penetrated member state administration and civil society.',
    'If persistence depends on continuous supranational enforcement, the constraint is vulnerable to institutional change and remains tangled_rope. If integration primacy has become self-sustaining through norm adoption, the constraint approaches rope (coordination by shared belief rather than coercion). The measurement series (rising theater_ratio) suggests the former: increasing proportion of enforcement is devoted to maintaining the rule against member state resistance, rather than facilitating coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_constituency_dependency, empirical, 'Whether integration-primary reading persists by institutional enforcement or has become self-sustaining through norm adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_obligations__integration_primary, theater_ratio, 1992, 0.08).
narrative_ontology:measurement_basis(fede_tr_t1992, observed).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__integration_primary, theater_ratio, 2000, 0.11).
narrative_ontology:measurement_basis(fede_tr_t2000, observed).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_obligations__integration_primary, theater_ratio, 2008, 0.15).
narrative_ontology:measurement_basis(fede_tr_t2008, observed).
narrative_ontology:measurement(fede_tr_t2015, federation_membership_obligations__integration_primary, theater_ratio, 2015, 0.19).
narrative_ontology:measurement_basis(fede_tr_t2015, observed).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__integration_primary, theater_ratio, 2020, 0.21).
narrative_ontology:measurement_basis(fede_tr_t2020, observed).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__integration_primary, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(fede_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_obligations__integration_primary, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement_basis(fede_be_t1992, observed).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__integration_primary, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(fede_be_t2000, observed).
narrative_ontology:measurement(fede_be_t2008, federation_membership_obligations__integration_primary, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement_basis(fede_be_t2008, observed).
narrative_ontology:measurement(fede_be_t2015, federation_membership_obligations__integration_primary, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement_basis(fede_be_t2015, observed).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__integration_primary, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(fede_be_t2020, observed).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__integration_primary, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(fede_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_obligations__integration_primary, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement_basis(fede_su_t1992, observed).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__integration_primary, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement_basis(fede_su_t2000, observed).
narrative_ontology:measurement(fede_su_t2008, federation_membership_obligations__integration_primary, suppression_requirement, 2008, 0.41).
narrative_ontology:measurement_basis(fede_su_t2008, observed).
narrative_ontology:measurement(fede_su_t2015, federation_membership_obligations__integration_primary, suppression_requirement, 2015, 0.49).
narrative_ontology:measurement_basis(fede_su_t2015, observed).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__integration_primary, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(fede_su_t2020, observed).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__integration_primary, suppression_requirement, 2024, 0.54).
narrative_ontology:measurement_basis(fede_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_labor_market_fragmentation).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, national_welfare_state_boundary_closure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_obligations kernel. The integration_primary reading asserts free movement overrides welfare closure; the member_sovereignty_primary reading asserts welfare closure authority overrides free movement (forecast: should structurally foreclose integration-primary within a single framework, but empirically coexists as different jurisdictions adopt different readings); the selective_solidarity reading asserts tiered welfare access by contribution (forecast: influences both siblings, creating pressure toward compromise but without logically foreclosing either). All three stories share the kernel but have distinct ε values, distinct beneficiary/victim structures, and distinct classifications. They are linked via network.affects_constraints to enable constraint-family analysis and to model how a single contested kernel instantiates multiple constraints depending on which reading is politically dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
