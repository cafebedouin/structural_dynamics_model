% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement Rights and Labor Mobility (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The integration reading of the federation membership kernel asserts that
 *   free movement is a fundamental right constitutive of EU citizenship and
 *   single-market completion, and that supranational authority (the ECJ)
 *   interprets scope expansively to maximize labor mobility and equal
 *   treatment. Under this reading, restrictions on work, residence, or
 *   welfare access that are based on nationality or sending-state origin are
 *   prima facie illegitimate. Member states retain formal sovereignty but are
 *   bound by ECJ interpretation of what the fundamental right encompasses.
 *   The constraint operationalizes through three mechanisms: (1) ECJ rulings
 *   that override national labor-market protections and welfare eligibility
 *   thresholds; (2) enforcement actions against member states that attempt to
 *   restrict free movement; (3) the normative claim that 'equal treatment'
 *   requires ignoring sending-state labor-market conditions or
 *   receiving-state fiscal capacity. The structural delta from the sibling
 *   member_sovereignty_reading is stark: this reading places displaced local
 *   labor and receiving-state welfare systems squarely in the victim set,
 *   externalizes sending-state brain drain as an uncompensated cost, and
 *   treats national labor-market protections as illegitimate obstacles rather
 *   than legitimate social coordination. The sibling
 *   welfare_coordination_reading differs by asserting that free movement
 *   should operate through coordination of national welfare systems
 *   (anti-social-dumping enforcement) rather than supranational harmonization
 *   and equal-treatment doctrine.
 *
 * KEY AGENTS:
 *   - mobile_workers: beneficiaries with mobile exit (high freedom to relocate)
 *   - receiving_state_employers: beneficiaries from labor-pool expansion
 *   - supranational_authority (ECJ): agenda-setter interpreting and enforcing the constraint
 *   - displaced_native_workers: powerless victims trapped in local labor markets
 *   - receiving_state_welfare_systems: institutional payers absorbing costs without compensation
 *   - sending_state_labor_markets: moderate-power payers experiencing brain drain
 *   - receiving_state_governments: dual agents — enforcers of ECJ rulings and absorbers of fiscal cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.72).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement Rights and Labor Mobility (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '951d3610-1052-4d9c-9f61-ed0c2043da6e').
narrative_ontology:cs_kernel_codification('951d3610-1052-4d9c-9f61-ed0c2043da6e', fixed_text).
narrative_ontology:cs_authority_grounding('951d3610-1052-4d9c-9f61-ed0c2043da6e', lineage).
narrative_ontology:cs_interpretation_layer_present('951d3610-1052-4d9c-9f61-ed0c2043da6e').
narrative_ontology:cs_reading_relation('951d3610-1052-4d9c-9f61-ed0c2043da6e', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('951d3610-1052-4d9c-9f61-ed0c2043da6e', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('951d3610-1052-4d9c-9f61-ed0c2043da6e', foundational, free_movement_supranational_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_supranational_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('951d3610-1052-4d9c-9f61-ed0c2043da6e', free_movement_supranational_fundamental_right, deontological).
narrative_ontology:cs_axiom('951d3610-1052-4d9c-9f61-ed0c2043da6e', foundational, labor_market_integration_maximizes_efficiency).
narrative_ontology:cs_axiom_status(labor_market_integration_maximizes_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('951d3610-1052-4d9c-9f61-ed0c2043da6e', labor_market_integration_maximizes_efficiency, empirically_contingent).
narrative_ontology:cs_axiom('951d3610-1052-4d9c-9f61-ed0c2043da6e', secondary, ecj_interprets_scope_expansively).
narrative_ontology:cs_axiom_status(ecj_interprets_scope_expansively, holdable).
narrative_ontology:cs_axiom_grounding('951d3610-1052-4d9c-9f61-ed0c2043da6e', ecj_interprets_scope_expansively, conventional).
narrative_ontology:cs_reference_frame('951d3610-1052-4d9c-9f61-ed0c2043da6e', supranational_market_integration_supremacy).
narrative_ontology:cs_drift_state('951d3610-1052-4d9c-9f61-ed0c2043da6e', contemporary_post_2015_migration_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('951d3610-1052-4d9c-9f61-ed0c2043da6e', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, supranational_authority).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_native_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_labor_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, supranational_market_integration_supremacy).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, economic_mobility_as_fundamental_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU citizens and third-country family members exercising free movement rights across member states. They gain legal right to work, reside, and access social benefits in receiving states on equal terms with citizens. The ECJ interprets these rights expansively, including access to welfare benefits, family reunification, and labor protections. Exit is easy: they can move freely between member states or return home.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_workers, beneficiary,
    moderate, biographical, mobile, global).

% Access an expanded labor pool from across the EU without legal barriers to hiring or employing migrant workers. Labor costs often compress as competition increases. They benefit from removing hiring restrictions, nationality-based minimum wages, or quota systems. Exit is operational: they can relocate operations or simply hire locally when labor conditions suit.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_employers, beneficiary,
    organized, biographical, mobile, national).

% The ECJ and EU Commission interpret and enforce free movement rights, overriding national labor market protections, welfare eligibility rules, and residence requirements where they conflict with the integration reading. They set the scope of the fundamental right expansively and issue binding rulings that member states must comply with. They collect institutional authority and legitimacy from the founding treaties and from being the arbiters of what 'fundamental right' means in the EU context.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, supranational_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Workers in receiving states whose labor market position deteriorates due to increased competition from free movement. They face wage pressure, longer unemployment spells, or exclusion from jobs that historically offered stable middle-class paths. They have no legal recourse to restrict entry or to demand ECJ review; their only exit is retraining, geographic relocation within the state, or accepting lower wages. Exit options are tightly constrained by local economic structure and family ties.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_native_workers, payer,
    powerless, biographical, trapped, local).

% Bear fiscal costs when mobile workers access unemployment benefits, child allowances, housing assistance, and healthcare immediately upon arrival, without prior contribution history. The ECJ rules that equal treatment (not residence duration) governs eligibility, and that member states cannot impose contribution thresholds that disfavor migrants. Welfare budgets absorb these costs without fiscal compensation from sending states or EU central budget. Exit is legal but costly: tightening eligibility rules may trigger ECJ infringement proceedings or require EU legislative negotiation to change the framework.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    powerful, generational, constrained, national).

% Experience brain drain and loss of working-age population, particularly of skilled and educated workers who migrate to higher-wage receiving states. Labor shortages emerge in certain sectors. Remittances flow out. Tax revenues decline as the productive workforce shrinks. Sending states cannot legally restrict out-migration or impose return requirements. Exit from the constraint is limited: they can negotiate side-payments (not available under current rules) or exit EU membership itself (extremely costly).
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_labor_markets, payer,
    moderate, generational, constrained, national).

% Legal structures (apprenticeship systems, union wage-setting, minimum wage regimes, skill certification) that historically protected native labor market entrants. The integration reading subordinates these to free movement rights; ECJ rulings override or hollow out the protections where they conflict with equal treatment. They are not a party but a category of rules and institutions whose legitimacy is displaced by the constraint's operation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_labor_market_protections, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__integration_reading, national_labor_market_protections).

% Must implement ECJ rulings and enforce free movement rights while managing welfare cost impacts and labor market pressure on native workers. They lose the power to set nationality-based employment preferences or welfare eligibility but retain formal sovereignty over most welfare design. They are both enforcers of the supranational constraint and absorbers of its fiscal and political costs. Exit is hard: leaving the EU is the only full exit; partial exit (opting out of free movement) is not available to EU members.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, receiving_state_governments, payer).

% Cannot restrict out-migration or impose return requirements; lose tax revenue and human capital. They are formally equal in the ECJ's framework but have less structural interest in defending the integration reading since their labor markets export workers rather than import them. They sit between EU institutional pressure to enforce free movement and domestic political pressure from workers who benefit from remittances and from communities that prefer local control of migration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_governments, agenda_setter,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, sending_state_governments, payer).

% The Court of Justice of the European Union interprets the scope and limits of free movement rights through case law. It is formally neutral but its decisional record shows consistent expansion of free movement protections and equal treatment principles. The court functions as an analytical seat with authority over the interpretation frame, not as a party collecting benefits.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_judicial_body, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, supranational_authority).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables labor market integration across member states by removing borders to work and residence: a worker can move from lower-wage to higher-wage regions without legal barriers. Employers access larger labor pools. Welfare systems operate on a principle of equal treatment. Firms can relocate operations or hire from across the EU without nationality-based restrictions.
% TRANSFER_FUNCTION: Moves labor from lower-wage sending states to higher-wage receiving states; moves welfare costs (unemployment, family, housing benefits) from receiving-state taxpayers to welfare systems; externalizes labor-market adjustment costs onto displaced native workers and sending-state governments. The supranational authority collects institutional power and legitimacy from being the arbiter of these rights.
% ABSENT_VOICES: Displaced native workers and their representatives have no seat at the ECJ's table; sending-state labor unions and sending-state governments are present in formal structures but structurally outnumbered in the court's interpretation bias. Voices calling for welfare-state protection or labor-market restriction are excluded from the core decision-making on what the 'fundamental right' means.
% DISAPPEARANCE_RATIONALE: If free movement rights and their ECJ enforcement disappeared overnight, member states would immediately impose labor-market protections, nationality-based hiring preferences, and welfare eligibility thresholds. Cross-border labor flows would drop sharply. Receiving-state welfare budgets would contract. Sending states would stabilize their labor supplies. The entire EU labor market structure would shift from integration-as-supremacy to coordination-among-sovereigns.
% FOUNDING_PROBLEM: Fragmented labor markets prevented efficient resource allocation in the early EU; workers could not move freely across borders due to visa restrictions, work permits, and nationality-based hiring rules. This created dead-weight loss: workers willing to move at higher wages could not, and employers could not access larger talent pools.
% FOUNDING_PROBLEM_CORROBORATION: The ECJ and EU Commission attest the founding problem remains live and cite ongoing labor-market inefficiencies. Member state governments and independent economists attest the founding problem is substantially solved and the constraint now operates primarily as extraction and supranational agenda-setting rather than genuine coordination. Labor unions and displacement studies from receiving states corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end, rising from 0.38 at 1993) because the constraint transfers labor-market adjustment costs, welfare expenditures, and human capital without equivalent compensation or fiscal transfer. The measuring time grid is shared across all three metrics; every metric is authored at every examined year (1993, 2000, 2008, 2015, 2020, 2023). The rising trajectory reflects the ECJ's consistent expansion of free movement scope and member state compliance, making exit and alternative coordination increasingly costly. Suppression is high (0.72) because the constraint's persistence depends on ECJ willingness to override national labor-market protections, on member state compliance with ECJ rulings, and on the absence of any legal route for displaced workers or sending states to appeal the framework. Theater ratio rises from 0.22 to 0.41, indicating that while the coordination function was real in 1993 (removing visa/work-permit barriers), by 2023 a growing share of the constraint's operation defends welfare-access supremacy and overrides local labor-market preferences rather than solving a live coordination problem. Accessibility collapse is moderate-to-high (0.62): once the ECJ's interpretation is clear, alternatives (national labor-market design, welfare eligibility thresholds, skill certification) are legally foreclosed for member states, though not for workers themselves (who retain exit). Resistance is high (0.71) because multiple constituencies — displaced worker movements, receiving-state labor unions, some member state governments, sending-state governments — mount real political opposition to the constraint, though the ECJ's structural position insulates it from that pressure.
 *
 * PERSPECTIVAL GAP:
 *   The supranational authority and mobile workers' seats compute this as pure coordination; the displaced native workers and receiving-state welfare systems compute it as pure extraction. The receiving-state governments and sending-state governments occupy the gap: they enforce the constraint under ECJ mandate while absorbing political costs. From the beneficiary seats (mobile workers, employers, ECJ authority), the constraint solves genuine labor-market inefficiency and instantiates a fundamental right; from the victim seats (displaced workers, welfare systems), it is leveraging the appearance of coordination to extract labor-market adjustment costs and welfare expenses without consent or compensation. The engine computes per-seat classifications from the same structural data; this perspectival gap is the measurement the divergence exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers sit at d ≈ 0.2–0.3 (beneficiaries with exit options; they gain labor-market access and equal treatment rights; their only cost is competition with similarly mobile workers, but they can always relocate). Receiving-state employers sit at d ≈ 0.25–0.35 (beneficiaries; they gain labor-pool access; their cost is that wages may compete away some of the gain, but labor remains relatively cheap and abundant). The supranational authority sits at d ≈ 0.15–0.25 (beneficiary in terms of institutional power and interpretive authority; it collects legitimacy from being the arbiter; its cost is limited to political pressure from member states, which it can largely resist through judicial authority). Displaced native workers sit at d ≈ 0.75–0.90 (strong targets; they bear wage pressure, unemployment risk, and exclusion from jobs; their exit is constrained by local economic ties and retraining costs; the supranational authority's rulings override their interests without consulting them). Receiving-state welfare systems sit at d ≈ 0.70–0.80 (strong targets; they absorb immediate costs from welfare access; they cannot legally restrict eligibility; their exit is political and extremely costly). Sending-state labor markets sit at d ≈ 0.60–0.70 (targets; they lose workers and tax revenue; their exit is limited to EU-level negotiation or departure). These directionalities are derived from the beneficiary/victim declarations and exit-option asymmetries: beneficiaries have mobile or arbitrage-grade exit; victims have trapped or identity-locked exit in the local context. No directionality overrides are needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The integration reading classifies as tangled_rope (genuine coordination function + asymmetric extraction + active enforcement). This prevents misclassification as pure snare (which would require no beneficiaries or benefits) or as pure rope (which would require symmetry or near-symmetry in costs and benefits). The founding-problem status is contested: the ECJ and Commission attest the problem of fragmented labor markets is live; displaced worker advocates and some member state governments attest it is solved and the constraint has become extraction. The disappearance verdict is world_rearranges: if the constraint vanished, member states would immediately reimpose labor-market protections and welfare restrictions. This alignment (contested status + rearranges verdict) is the mandatrophy flag the omega variables should investigate — the constraint persists despite a substantial portion of the polity contesting its founding function. The theater_ratio rise from 0.22 to 0.41 is the secondary indicator: the proportion of enforcement activity defending welfare-access supremacy and equal-treatment doctrine (rather than removing border barriers) has grown, suggesting functional drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_decomposition,
    'What is the size of the genuine coordination benefit (labor-market efficiency gain from removing borders) versus the extraction component (welfare costs, labor-market adjustment costs, brain drain) in the measured extractiveness value?',
    'Counterfactual economic analysis: model EU labor-market outcomes under (a) fragmented national markets with current EU productivity levels, versus (b) integrated markets. Compare actual welfare flows to what optimal coordination would require. Decompose the benefit stream and the cost stream.',
    'If coordination benefits are small (< 0.15) and extraction is large (> 0.60), the constraint reclassifies as snare with a thin coordination pretext. If coordination benefits are large (> 0.35) and extraction moderate, the tangled_rope classification holds. The decomposition determines whether the constraint is defensible as coordination-with-cost or indefensible as extraction-with-pretext.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_decomposition, empirical, 'Proportion of measured extractiveness attributable to genuine coordination benefit versus net transfer and cost externalization.').

omega_variable(
    reading_contest_structural_identity,
    'Is the contest between the integration_reading, member_sovereignty_reading, and welfare_coordination_reading a matter of genuinely incommensurable frameworks (three different ways of carving up the same constraint), or are they three distinct constraints masquerading as one kernel?',
    'Apply the ε-invariance principle: if the three readings yield significantly different ε values (differing by > 0.25) for the ''same'' constraint, and the ε differences arise from different observables (what counts as a benefit, what counts as a cost), then the readings are actually three different constraints, not three readings of one kernel. Verify by checking whether the readings make contradictory claims about the same causal mechanism or about different mechanisms.',
    'If the readings are genuinely distinct constraints, they should be authored as three separate files (already planned), but they are NOT readings of a single kernel — the kernel concept would be misapplied. If they are readings of one kernel, the divergent ε values are reading-indexed (different endorsing parties see different referents as the ''constraint''), and the kernel is a genuinely contested commitment. This determines the kernel_id categorization: valid kernel (one commitment, multiple readings) versus constraint family (multiple commitments, one domain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_structural_identity, conceptual, 'Whether the three readings constitute a single contested kernel or a decomposed constraint family.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (ECJ authority blocking national-level alternatives, member states unable to exit without EU departure) or internalized (member state governments have internalized the integration reading''s legitimacy and no longer contest the framework)?',
    'Examine member state legislative history and public messaging: do governments actively resist ECJ rulings and seek to find legal loopholes (structural suppression), or do they comply readily and frame the constraint as legitimate (internalized)? Post-exit suppression trajectory: if a member state were to leave the EU, would suppression persist (suggesting internalization) or would it immediately lift (suggesting structural)?',
    'If suppression is structural, the constraint''s persistence depends on active ECJ enforcement and member state compliance under duress; escape is possible via EU exit. If suppression is internalized, member state governments have fused their identity with the constraint and escape would require cognitive reframing, not just legal change — the constraint is more durable. The distinction affects the stability forecast: structural suppression can be disrupted by political change; internalized suppression is harder to disrupt without trauma.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates through external ECJ enforcement or through member state internalization of the integration reading''s legitimacy.').

omega_variable(
    displaced_workers_coalition_potential,
    'Do displaced native workers in receiving states have the organizational capacity and cross-border coordination to mount political opposition sufficient to alter the constraint, or is their powerlessness (power=powerless) durable?',
    'Monitor labor union organization, electoral support for parties opposing free movement, and grassroots mobilization. Check whether coalition formation across receiving states'' labor movements has occurred or is feasible. Assess political window: have receiving-state governments faced sufficient electoral pressure to negotiate welfare-state carve-outs or labor-market protections within the EU framework?',
    'If coalition power emerges, displaced workers might transit from powerless to organized, shifting directionality upward (lower d values, reduced effective extraction from their perspective) and creating pressure for member_sovereignty or welfare_coordination readings to gain ground. If powerlessness is durable, the constraint persists as a tangled_rope with high extraction from the victim seat, potentially degrading to piton (theater rises, enforcement hardens, coordination function fades).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_workers_coalition_potential, empirical, 'Whether displaced native workers can organize sufficient coalition power to challenge the integration reading''s dominance.').

omega_variable(
    axiom_overriding_empirical_challenge,
    'Has the foundational empirical axiom of the integration reading (''labor-market integration maximizes efficiency and welfare'') been substantially challenged by economic evidence since 1993?',
    'Meta-analysis of peer-reviewed economics: do studies show that free movement produces net welfare gains, neutral effects, or net losses for receiving-state native workers and sending-state labor markets? Has the ECJ acknowledged or engaged with empirical challenges to the axiom?',
    'If empirical challenges are substantial and unengaged by the ECJ, the axiom_overriding drift direction in cs_structure.drift_state gains support (magnitude=substantial or severe). This would indicate the axiom is overridden by evidence but the authority structure has not formally acknowledged the override, suggesting growing gap between the reading''s grounding and empirical reality. The engine would compute a foreclosure probability signal if status=''holdable'' + grounding_type=''empirically_contingent'' + drift=''axiom_overriding'' + magnitude=substantial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_empirical_challenge, empirical, 'Whether the foundational empirical axiom of the integration reading has been substantially challenged or refuted by economic evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_int_tr_t1993, federation_membership_kernel__integration_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement_basis(fmk_int_tr_t1993, observed).
narrative_ontology:measurement(fmk_int_tr_t2000, federation_membership_kernel__integration_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement_basis(fmk_int_tr_t2000, observed).
narrative_ontology:measurement(fmk_int_tr_t2008, federation_membership_kernel__integration_reading, theater_ratio, 2008, 0.34).
narrative_ontology:measurement_basis(fmk_int_tr_t2008, observed).
narrative_ontology:measurement(fmk_int_tr_t2015, federation_membership_kernel__integration_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(fmk_int_tr_t2015, observed).
narrative_ontology:measurement(fmk_int_tr_t2020, federation_membership_kernel__integration_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(fmk_int_tr_t2020, observed).
narrative_ontology:measurement(fmk_int_tr_t2023, federation_membership_kernel__integration_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement_basis(fmk_int_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(fmk_int_be_t1993, federation_membership_kernel__integration_reading, base_extractiveness, 1993, 0.38).
narrative_ontology:measurement_basis(fmk_int_be_t1993, observed).
narrative_ontology:measurement(fmk_int_be_t2000, federation_membership_kernel__integration_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(fmk_int_be_t2000, observed).
narrative_ontology:measurement(fmk_int_be_t2008, federation_membership_kernel__integration_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement_basis(fmk_int_be_t2008, observed).
narrative_ontology:measurement(fmk_int_be_t2015, federation_membership_kernel__integration_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement_basis(fmk_int_be_t2015, observed).
narrative_ontology:measurement(fmk_int_be_t2020, federation_membership_kernel__integration_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement_basis(fmk_int_be_t2020, observed).
narrative_ontology:measurement(fmk_int_be_t2023, federation_membership_kernel__integration_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement_basis(fmk_int_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(fmk_int_su_t1993, federation_membership_kernel__integration_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement_basis(fmk_int_su_t1993, observed).
narrative_ontology:measurement(fmk_int_su_t2000, federation_membership_kernel__integration_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement_basis(fmk_int_su_t2000, observed).
narrative_ontology:measurement(fmk_int_su_t2008, federation_membership_kernel__integration_reading, suppression_requirement, 2008, 0.63).
narrative_ontology:measurement_basis(fmk_int_su_t2008, observed).
narrative_ontology:measurement(fmk_int_su_t2015, federation_membership_kernel__integration_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(fmk_int_su_t2015, observed).
narrative_ontology:measurement(fmk_int_su_t2020, federation_membership_kernel__integration_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(fmk_int_su_t2020, observed).
narrative_ontology:measurement(fmk_int_su_t2023, federation_membership_kernel__integration_reading, suppression_requirement, 2023, 0.72).
narrative_ontology:measurement_basis(fmk_int_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% The federation_membership_kernel has been decomposed into three constraint stories, each instantiating a different reading of the contested kernel. The integration_reading (this file) asserts free movement as a fundamental supranational right with expansive ECJ interpretation. The member_sovereignty_reading asserts member-state authority to bound free movement by welfare capacity and labor-market protection. The welfare_coordination_reading asserts coordination of national welfare systems rather than supranational harmonization. These three readings cannot coexist in a single legal framework — they make contradictory claims about who has authority to define the scope of free movement and how national welfare and labor-market protections interact with the fundamental right. However, they do coexist across different political coalitions and member states. Each reading yields a distinct constraint with distinct beneficiaries, victims, and ε values. They are linked as a constraint family via this network.affects_constraints field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
