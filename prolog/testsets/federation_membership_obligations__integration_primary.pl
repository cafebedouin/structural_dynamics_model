% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement & Welfare Integration Reading
 *   domain: political_economy/federalism/welfare_state
 *
 * SUMMARY:
 *   The European Union enforces free movement of workers and EU citizenship
 *   as supranational rights that override member-state welfare-system
 *   closure. Under this reading (integration_primary), free movement is
 *   constitutive of EU citizenship itself and essential to single-market
 *   functioning; member states must grant welfare access to EU migrants on
 *   terms no less favorable than their own citizens; the ECJ enforces these
 *   rights through case law, systematically dismantling durational
 *   requirements, residency-based closures, and contribution-history
 *   discrimination. This is ONE reading of a contested kernel
 *   (federation_membership_obligations); sibling readings
 *   (member_sovereignty_primary and selective_solidarity) assert member-state
 *   closure authority or tiered access instead. This story instantiates
 *   integration_primary and models its structural extraction: mobile workers
 *   and employers capture the gains; local labor, welfare systems, and public
 *   services in receiving states bear the adjustment costs; supranational
 *   institutions (ECJ, Commission) enforce the arrangement against
 *   member-state resistance.
 *
 * KEY AGENTS:
 *   - Mobile EU workers: beneficiary, arbitrage exit, continental scope—access labor and welfare across borders
 *   - Receiving-state employers: beneficiary, powerful, mobile—expand labor supply and reduce wage pressure
 *   - Local displaced labor: victim, powerless, constrained—face wage competition and job exclusion
 *   - Member-state welfare systems: victim, institutional, constrained—absorb fiscal costs of migrant eligibility
 *   - ECJ/supranational authority: agenda-setter, institutional, analytical—enforces the reading via case law
 *   - Member-state governments: payer/excluded, powerful, trapped—bound by treaty supremacy and unable to close welfare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.61).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement & Welfare Integration Reading").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '83739001-4726-43d6-8114-96d4fabab57c').
narrative_ontology:cs_kernel_codification('83739001-4726-43d6-8114-96d4fabab57c', formalized).
narrative_ontology:cs_authority_grounding('83739001-4726-43d6-8114-96d4fabab57c', extraction).
narrative_ontology:cs_interpretation_layer_present('83739001-4726-43d6-8114-96d4fabab57c').
narrative_ontology:cs_reading_relation('83739001-4726-43d6-8114-96d4fabab57c', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('83739001-4726-43d6-8114-96d4fabab57c', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('83739001-4726-43d6-8114-96d4fabab57c', foundational, free_movement_citizenship_essence).
narrative_ontology:cs_axiom_status(free_movement_citizenship_essence, holdable).
narrative_ontology:cs_axiom_grounding('83739001-4726-43d6-8114-96d4fabab57c', free_movement_citizenship_essence, deontological).
narrative_ontology:cs_axiom('83739001-4726-43d6-8114-96d4fabab57c', foundational, welfare_closure_violates_citizenship).
narrative_ontology:cs_axiom_status(welfare_closure_violates_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('83739001-4726-43d6-8114-96d4fabab57c', welfare_closure_violates_citizenship, deontological).
narrative_ontology:cs_reference_frame('83739001-4726-43d6-8114-96d4fabab57c', supranational_citizenship_primacy).
narrative_ontology:cs_drift_state('83739001-4726-43d6-8114-96d4fabab57c', post_2004_enlargement_migration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('83739001-4726-43d6-8114-96d4fabab57c', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, service_sector_capital).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, local_displaced_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, local_public_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, free_movement_as_citizenship_essence).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, single_market_supranational_primacy).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, ecj_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU citizens with portable labor skills and professional credentials gain unrestricted access to labor markets across member states and full welfare eligibility in receiving states. They arbitrage wage and benefit differences, accessing healthcare, family allowances, unemployment insurance, and pension recognition across borders. Their options have expanded from the constraint's enforcement—they exit low-wage or depressed regions and enter high-wage or generous-welfare receiving states with legal security.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, arbitrage, continental).

% Access an expanded, mobile labor supply without protectionist barriers or licensing restrictions. They benefit from wage moderation (migrant workers accept lower compensation than local labor in some sectors), filling skills gaps, and avoiding relocation or wage pressure in tight local markets. The constraint's enforcement prevents member states from blocking or pricing this access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_employers, beneficiary,
    powerful, generational, mobile, continental).

% Local workers in low-skilled sectors (construction, care services, hospitality, agriculture) face wage pressure, job competition, and exclusion from preferred positions as mobile EU workers are legally indistinguishable from locals and willing to work at lower rates or worse conditions. They cannot restrict entry into their labor market; the constraint forces their member state to treat intra-EU migrants as citizens with full employment rights. Their recourse is retraining or exit—both costly and slow.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, local_displaced_labor, payer,
    powerless, biographical, constrained, local).

% Receiving states cannot close welfare access to EU migrants or condition it on contribution history longer than imposed on own citizens (ECJ case law enforces this). Aging EU member states with generous welfare systems face net migration inflows that increase fiscal burden on healthcare, pensions, and family benefits. They cannot adjust eligibility rules or immigration policy to manage welfare-system sustainability without violating the constraint's enforcement via ECJ rulings. Their exit options are formal withdrawal from EU (existential institutional cost) or accepting the redistribution.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Health, education, and social services in receiving states absorb demand from migrant populations without dedicated funding flows; sending-state investments in workforce training and education are captured without compensation. Public service quality and access for local populations compress where services are capacity-constrained and migration is high.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, local_public_services, payer,
    institutional, generational, constrained, regional).

% Benefit from labor-market clearing (unemployment reduction and wage moderation via emigration) and remittance flows from mobile workers. They pay through the loss of human capital (educated workers leave), reduced tax base in origin, and diffused political voice (emigrants' interests are weakly represented at home). Poorer EU states gain net-migration benefits; richer states gain net-costs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_state_governments, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, sending_state_governments, payer).

% The European Court of Justice sits as the authoritative interpreter of free-movement rights and welfare-access conditions via case law. It expands free-movement scope through successive rulings (establishing family reunification, social-benefit portability, durational non-discrimination, etc.). It enforces the constraint against member-state attempts to condition welfare on contribution history or restrict migrant access. ECJ authority depends on the constraint's persistence and expansion—it is the seat that enforces the rule and derives institutional legitimacy from that function.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj_supranational_authority, agenda_setter,
    institutional, civilizational, analytical, continental).

% Administers single-market rules and brings enforcement actions against member states that attempt welfare protectionism. Commission legitimacy rests on free-movement enforcement; it frames restrictions as illiberal and economically backward. It has constrained exit—withdrawal from single-market administration would dissolve its institutional role.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, european_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Subject to supranational enforcement of free-movement and non-discrimination rules. Receiving-state governments lose unilateral control of welfare eligibility and labor-market access; sending-state governments lose control of emigration. They theoretically set the rules via Council but are constrained by supranational courts' interpretive authority and treaty supremacy doctrine. Exit from the EU is legally available but institutionally catastrophic. They would prefer conditional free movement and welfare closure, but their preferences are overridden by supranational actors (ECJ, Commission) enforcing the integration-primary reading.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, excluded).

% Political movements opposing free movement and welfare integration gain electoral strength in receiving states (where local labor and public-service capacity feel the extraction) but lack institutional seats at the table where the constraint is enforced. They contest the constraint via electoral politics and referenda but find their member-state governments bound by treaty law and ECJ rulings. Their voices are excluded from supranational decision-making; the constraint persists despite their opposition.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, populist_opposition_coalitions, excluded,
    moderate, biographical, mobile, national).

% Measure and analyze the constraint's effects: labor-market dynamics, welfare-system fiscal pressure, wage effects on local workers, fiscal transfers between member states. They provide expert testimony in political debates but have no institutional authority to change the constraint's enforcement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, academic_and_policy_observers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, ecj_supranational_authority).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables labor mobility across member states as an essential single-market mechanism: workers move to jobs, employers access talent regardless of origin-state borders, wage and productivity equalization occurs across regions, human capital is allocated efficiently across the federation rather than trapped by national citizenship barriers.
% TRANSFER_FUNCTION: Transfers welfare access, labor-market entry rights, and portability of social insurance from member-state control to EU-level free-movement guarantee; receives constraint acceptance costs (wage pressure on local low-skilled labor, fiscal burden on receiving-state welfare systems, public-service capacity constraints) in exchange for single-market integration benefits (employer flexibility, worker opportunity, capital mobility).
% ABSENT_VOICES: Local labor unions and affected workers in low-skilled sectors are weakly organized supranationally and largely absent from the sites where the constraint's enforcement happens (ECJ, Commission). Populist parties opposing the constraint gain representation in some member states but lack supranational institutional standing. Fiscal-impact analysts and welfare-system administrators who bear the adjustment costs have advisory roles but not veto authority.
% DISAPPEARANCE_RATIONALE: If the integration-primary reading were overturned and member-state welfare closure permitted, EU labor flows would contract, wage pressure on local low-skilled workers would ease, and welfare-system fiscal burdens would shift—receiving states would redesign eligibility rules within days. The single market itself would fragment along welfare and labor-market lines; employer access to mobile EU labor would vanish; the ECJ's institutional authority would collapse. The federation's structural integration hinges on this constraint.
% FOUNDING_PROBLEM: 1950s-1970s: European economic integration requires overcoming national borders and protectionist closure of labor markets and welfare systems. Free movement was framed as essential to single-market efficiency and to the legitimacy project of 'European citizenship' as a form of supranational political community transcending nation-states.
% FOUNDING_PROBLEM_CORROBORATION: Integration architects and the ECJ affirm the problem remains live: border-based labor restrictions distort markets and undermine the EU's supranational legitimacy. Welfare-state researchers and receiving-state governments attest that the founding problem (market fragmentation) is substantially solved but the constraint persists as a source of new problems (welfare sustainability, local-labor precarity, fiscal transfers). Economists measure the efficiency gains of free movement but disagree on how to account for distributional costs. No corroboration exists from local-labor constituencies or populist opposition—they are structurally excluded from the seats where the problem is declared solved.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).

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
 *   Extractiveness (0.68 at interval end) reflects asymmetric transfer: mobile workers and employers gain welfare access and labor-market freedom; local labor and welfare systems lose control and fiscal capacity. The 8-point upward trend (0.48→0.68) models increasing migration flows and welfare-system strain post-2004 EU enlargement, plus ECJ case-law expansion of non-discrimination rules. Suppression (0.61) reflects the constraint's active enforcement: ECJ rulings block member-state welfare restrictions, Commission enforcement actions sanction closure attempts, treaty supremacy doctrine overrides national constitutional law. Member states cannot exit without existential institutional cost. Theater (0.28, rising from 0.12) captures the constraint's performative layer: free movement is legitimated as 'European citizenship' and 'market efficiency,' but an increasing share of enforcement activity (post-2015 migration pressure) defends welfare access and labor inflow against populist resistance, revealing the extraction beneath the coordination framing. The rising trajectory of suppression_requirement (0.48→0.61) models the intensification of enforcement machinery needed to overcome growing member-state and local resistance. One shared time grid: all metrics author values at every point (0,5,10,15,20,25,30,35); gaps in any metric are never created by misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the ECJ/Commission seat, the constraint is genuine coordination (efficient labor allocation, single-market integration). From member-state government seats, it is enforced transfer of welfare closure authority to supranational courts (loss of control). From local-labor seats, it is direct extraction via wage pressure and job competition. The engine computes these divergent d values from the structural data: mobile workers get d near 0.2 (beneficiary); local labor gets d near 0.85 (target); member states get d near 0.75 (strong targets, constrained exit); ECJ gets d near 0.1 (setter, not sitting in the transferred-extraction seat). The authored claim (Tangled Rope: genuine coordination + asymmetric extraction) depends on this seat divergence—the same structure is coordination TO some seats and extraction FROM others.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mobile workers, employers, receiving-state services-demand) have low d (beneficiary direction). Victims (local labor, welfare systems, member-state sovereignty) have high d (target direction). Supranational actors (ECJ, Commission) sit outside the d calculation—they are setters, not directionalized within the constraint itself. The beneficiary/victim split is structural: beneficiaries arbitrage across borders and gain welfare access they could not secure at home; victims bear adjustment costs (wage pressure, fiscal burden, lost policy authority) they cannot exit. Identity_locked elements appear for member states: their political identity is constituted through welfare-state provision and labor-market closure; ECJ enforcement forces them to reconfigure that identity (ceasing to be purely national welfare states). For local labor, the lock is economic: once skilled labor leaves for higher-wage EU states, returning is costly, creating path dependence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (market fragmentation, post-war European division) is attested as solved by 1990s single-market completion. Free movement rules remain and are enforced with increasing intensity, but the problem they were built to solve no longer exists—labor mobility is achieved, borders are open, market integration is deep. The constraint persists due to: (1) institutional inertia—ECJ's authority depends on interpreting and expanding free-movement rules; (2) beneficiary capture—mobile workers and employers benefit and have political voice in the Commission; (3) supranational legitimacy—free movement is woven into EU citizenship mythology. Local labor and welfare systems bear the costs and have insufficient voice to reframe the constraint. This is a candidate for Piton classification IF the founding problem status were 'dead'; it is instead classified Tangled Rope because coordination and extraction are still functionally distinct (labor mobility IS valuable; the question is who bears the cost). Mandatrophy is not yet resolved—the constraint's mandate could be superseded if member states successfully reassert welfare closure (selective_solidarity reading) or if EU federalism collapses entirely (member_sovereignty_primary reading). The rising theater_ratio suggests the constraint is drifting toward Piton: enforcement effort increasingly goes to defending the extraction (welfare access, labor inflow) rather than to solving the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the integration-primary reading''s founding problem (post-war market fragmentation and labor-market closure) still live, or has it been solved and the constraint now persists for institutional inertia and beneficiary capture?',
    'Comparison of labor-market integration metrics (wage convergence, labor mobility, regional unemployment equalization) and institutional authority: if the problem is solved but ECJ authority expands and welfare transfer increases, the constraint has drifted from coordination to extraction.',
    'If the problem is dead, the constraint should be reclassified from Tangled Rope (mixed coordination/extraction) to Piton (inertial extraction). Member states should be able to reassert welfare closure without violating the constraint''s essential coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s founding coordination problem persists or has been superseded.').

omega_variable(
    member_state_welfare_system_sustainability,
    'At current and projected migration flows, can EU member-state welfare systems sustainably integrate mobile EU workers without cost-shifting to local labor or service reduction?',
    'Fiscal analysis of welfare expenditure by EU member state, attribution of costs to EU-migrant beneficiary eligibility vs. other factors, and long-term sustainability modeling under different migration scenarios.',
    'If welfare systems are strained and migrant-eligibility costs are material, the extraction measure (0.68) understates the true cost transfer. If systems can absorb the costs, the constraint is more purely coordination than the story''s metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_welfare_system_sustainability, empirical, 'Whether welfare-system fiscal burden is extractive or absorbable.').

omega_variable(
    local_labor_wage_effect_causation,
    'Are measured wage declines and job-loss concentration among local low-skilled labor causally attributable to EU free-movement-enabled migration, or are they driven by automation, offshoring, and other technological/economic factors?',
    'Econometric study isolating migration''s causal effect on local wages and employment, using natural experiments (EU enlargement shocks, internal migration within otherwise-closed borders) and controlling for confounders.',
    'If migration is the primary driver, local labor is a victim of the constraint and extraction is high. If other factors dominate and migration is secondary, local labor''s victim status is weakened and the constraint''s extraction may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_labor_wage_effect_causation, empirical, 'The causal attribution of local labor''s wage/employment losses to free movement.').

omega_variable(
    reading_logical_foreclosure,
    'Does the integration_primary reading logically foreclose the member_sovereignty_primary reading, or can both be held by different parties within the same federation?',
    'Formal analysis of the two readings'' core premises: integration_primary asserts ''free movement is constitutive of citizenship; welfare closure violates citizenship''; member_sovereignty_primary asserts ''member states retain closure authority; free movement is conditional.'' Do these premises logically contradict in a single framework?',
    'If they foreclose, only one reading can survive in a stable federation—the contest is zero-sum. If they coexist, the constraint''s persistence depends on which reading''s supporters control supranational institutions, and the contest is distributive (who wins policy, not who wins truth).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_foreclosure, conceptual, 'Whether integration and sovereignty readings are logically exclusive or can coexist.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression (0.61) structural (member states are legally barred from welfare closure by treaty supremacy and ECJ enforcement) or partially internalized (member states have accepted the reading and no longer attempt closure)?',
    'Analysis of member-state behavior post-ECJ rulings: do governments attempt closure and face enforcement, or do they voluntarily comply? Post-exit compliance trajectories in case of ECJ authority collapse.',
    'If suppression is largely internalized (normative acceptance), it is lower cost to maintain and the constraint is more stable. If structural (external enforcement), the constraint depends on ECJ''s continued institutional power and is vulnerable to member-state exit or institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is external enforcement or internalized normative acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_integration_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(federation_integration_tr_t0, projected).
narrative_ontology:measurement(federation_integration_tr_t5, federation_membership_obligations__integration_primary, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(federation_integration_tr_t5, observed).
narrative_ontology:measurement(federation_integration_tr_t10, federation_membership_obligations__integration_primary, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(federation_integration_tr_t10, observed).
narrative_ontology:measurement(federation_integration_tr_t15, federation_membership_obligations__integration_primary, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(federation_integration_tr_t15, observed).
narrative_ontology:measurement(federation_integration_tr_t20, federation_membership_obligations__integration_primary, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(federation_integration_tr_t20, observed).
narrative_ontology:measurement(federation_integration_tr_t25, federation_membership_obligations__integration_primary, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(federation_integration_tr_t25, observed).
narrative_ontology:measurement(federation_integration_tr_t30, federation_membership_obligations__integration_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(federation_integration_tr_t30, observed).
narrative_ontology:measurement(federation_integration_tr_t35, federation_membership_obligations__integration_primary, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(federation_integration_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(federation_integration_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(federation_integration_be_t0, projected).
narrative_ontology:measurement(federation_integration_be_t5, federation_membership_obligations__integration_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(federation_integration_be_t5, observed).
narrative_ontology:measurement(federation_integration_be_t10, federation_membership_obligations__integration_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(federation_integration_be_t10, observed).
narrative_ontology:measurement(federation_integration_be_t15, federation_membership_obligations__integration_primary, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(federation_integration_be_t15, observed).
narrative_ontology:measurement(federation_integration_be_t20, federation_membership_obligations__integration_primary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(federation_integration_be_t20, observed).
narrative_ontology:measurement(federation_integration_be_t25, federation_membership_obligations__integration_primary, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(federation_integration_be_t25, observed).
narrative_ontology:measurement(federation_integration_be_t30, federation_membership_obligations__integration_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(federation_integration_be_t30, observed).
narrative_ontology:measurement(federation_integration_be_t35, federation_membership_obligations__integration_primary, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(federation_integration_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(federation_integration_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(federation_integration_su_t0, projected).
narrative_ontology:measurement(federation_integration_su_t5, federation_membership_obligations__integration_primary, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(federation_integration_su_t5, observed).
narrative_ontology:measurement(federation_integration_su_t10, federation_membership_obligations__integration_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(federation_integration_su_t10, observed).
narrative_ontology:measurement(federation_integration_su_t15, federation_membership_obligations__integration_primary, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(federation_integration_su_t15, observed).
narrative_ontology:measurement(federation_integration_su_t20, federation_membership_obligations__integration_primary, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(federation_integration_su_t20, observed).
narrative_ontology:measurement(federation_integration_su_t25, federation_membership_obligations__integration_primary, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(federation_integration_su_t25, observed).
narrative_ontology:measurement(federation_integration_su_t30, federation_membership_obligations__integration_primary, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(federation_integration_su_t30, observed).
narrative_ontology:measurement(federation_integration_su_t35, federation_membership_obligations__integration_primary, suppression_requirement, 35, 0.61).
narrative_ontology:measurement_basis(federation_integration_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, global_infrastructure).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.22).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, national_welfare_state_closure_norms).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, ecj_supremacy_doctrine).

% DUAL FORMULATION NOTE:
% This constraint and its siblings (member_sovereignty_primary, selective_solidarity) are three readings of a single contested kernel (federation_membership_obligations). Each reading instantiates a different constraint with a different epsilon value, beneficiary/victim structure, and type. Integration_primary claims supranational primacy over welfare closure; member_sovereignty_primary claims member-state closure authority; selective_solidarity claims tiered access by contribution. The three readings coexist across different EU institutions (Commission/ECJ vs. member-state governments vs. proposed alternative rules), produce different structural effects (welfare integration vs. closure vs. conditionality), and compete for institutional authority. This story generates integration_primary only; the others are separate constraint stories linked via network.affects_constraints and commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
