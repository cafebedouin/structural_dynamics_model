% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement via National Welfare Coordination (Welfare Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel: 'federation
 *   membership and free movement scope.' The welfare-coordination reading
 *   holds that free movement is sustainable through coordination of national
 *   welfare systems — members retain autonomy to structure welfare
 *   eligibility, provided rules are non-discriminatory; the EU enforces
 *   anti-social-dumping constraints (posted worker directives, cabotage
 *   limits) while leaving detailed welfare design to member states. This
 *   avoids both deep harmonization (politically impossible) and complete
 *   closure (incompatible with free movement). The reading faces two sibling
 *   readings: integration (free movement is a fundamental right and
 *   equal-treatment mandate, driving ECJ toward expansionist interpretation)
 *   and sovereignty (free movement rights must yield to national welfare
 *   capacity and social solidarity). The constraint you are reading is a
 *   STRATEGIC STABILIZATION POINT between those pressures, not a natural law
 *   or a temporary expedient. Its persistence depends on ECJ and Commission
 *   maintaining a narrow middle ground — which the measurement series and
 *   omegas document as contested.
 *
 * KEY AGENTS:
 *   - EU supranational authority (ECJ, Commission): sets and enforces the coordination rules; mediates between free movement and welfare autonomy
 *   - Receiving-state governments (institutional payer): bear dual cost (wage pressure + fiscal pressure); constrained to coordinate rather than unilaterally close
 *   - Sending-state governments (institutional payer): lose workers without compensation; lack formal say in free movement scope decisions
 *   - Posted workers (powerless victim): structurally trapped; 2-year exemptions create precarity and wage-undercutting; no union representation
 *   - Resident labor market workers (organized payer): wage pressure from posted workers; constrained by non-discrimination rules limiting protectionist measures
 *   - High-skill migrants (powerful beneficiary): arbitrage skills and credentials across EU; exit easily if conditions deteriorate
 *   - Service-sector employers (powerful beneficiary): access cheap posted labor without full harmonization; capture rents from wage differentials
 *   - Worker advocacy organizations (excluded observer): unions and labor NGOs lack formal seats; can lobby but not veto posted worker policies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.61).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via National Welfare Coordination (Welfare Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'd1869cfd-3d5e-443f-98bd-1ef3ca094a72').
narrative_ontology:cs_kernel_codification('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', formalized).
narrative_ontology:cs_authority_grounding('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', lineage).
narrative_ontology:cs_interpretation_layer_present('d1869cfd-3d5e-443f-98bd-1ef3ca094a72').
narrative_ontology:cs_reading_relation('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', federation_membership_kernel__integration_reading, influences).
narrative_ontology:cs_reading_relation('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', foundational, welfare_autonomy_preservable_with_free_movement).
narrative_ontology:cs_axiom_status(welfare_autonomy_preservable_with_free_movement, holdable).
narrative_ontology:cs_axiom_grounding('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', welfare_autonomy_preservable_with_free_movement, instrumental).
narrative_ontology:cs_axiom('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', foundational, anti_social_dumping_enforcement_sufficient_for_protection).
narrative_ontology:cs_axiom_status(anti_social_dumping_enforcement_sufficient_for_protection, holdable).
narrative_ontology:cs_axiom_grounding('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', anti_social_dumping_enforcement_sufficient_for_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', subsidiarity_plus_free_movement).
narrative_ontology:cs_drift_state('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', contemporary_posted_worker_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1869cfd-3d5e-443f-98bd-1ef3ca094a72', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, low_wage_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, high_human_capital_migrants).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, service_sector_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, resident_labor_market_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_welfare_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% ECJ and EU Commission enforce free movement rights and anti-social-dumping rules. They set the boundary: free movement is protected, but member states retain welfare design autonomy provided they do not discriminate on nationality. They administer posted worker directives (2-year social contribution exemptions, cabotage restrictions). Their enforcement action is heavy on rule-application, light on material redistribution to losing states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_supranational_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Bear the dual cost: posted workers undercut wages (labor market pressure); permanent migrants may access welfare systems without proportional contribution history (fiscal pressure). They retain design autonomy (can restructure welfare eligibility, raise contribution thresholds) but constrained by non-discrimination rules. The constraint forces them to coordinate welfare access with EU free movement rather than unilaterally close their systems.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments, agenda_setter).

% Lose workers without fiscal compensation: workers contribute to their home welfare system, then emigrate and contribute to receiving-state systems, hollowing out sending-state fiscal capacity. They have no seat at ECJ or EU Commission tables on how much mobility to allow. Their budgetary challenges (aging populations, emigrating youth) are not addressed by the coordination framework.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, payer,
    moderate, biographical, constrained, national).

% Sent to work in another member state under a 2-year exemption from full social contributions and tax harmonization. They undercut local wages (competitive displacement) and are excluded from receiving-state welfare access (not yet taxpayers there). They carry no exit: refusal to post means unemployment in home state; accepting post means temporary, precarious status with minimal protections. Caught between sending-state control and receiving-state labor market hostility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, trapped, national).

% Face wage pressure from posted workers (who undercut because they are exempt from full tax and social contributions) and job competition from permanent migrants. They retain formal free movement rights themselves, but exercising those rights is costly. Their unions negotiate within receiving-state regulatory boundaries that are constrained by non-discrimination rules.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, resident_labor_market_workers, payer,
    organized, biographical, constrained, national).

% Highly educated, fluent in multiple languages, with professional credentials recognized across the EU (lawyers, doctors, engineers). They benefit from free movement, low barriers to work recognition, and access to receiving-state welfare systems without delay. They exit easily if working conditions deteriorate. The constraint enables their labor arbitrage across the EU at high skill levels.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, high_human_capital_migrants, beneficiary,
    powerful, generational, arbitrage, global).

% Workers from lower-GDP member states (Poland, Romania, Bulgaria) whose labor is valuable in higher-wage labor markets (Germany, France, UK-era). The constraint opens receiving-state labor markets to them; they move for wage premium without language or credential barriers. They benefit from mobility even as posted workers in the victim category are simultaneously undercut.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, low_wage_mobile_workers, beneficiary,
    moderate, biographical, mobile, global).

% Construction, agriculture, food processing, logistics firms in receiving states hire posted workers at posted-worker wage levels, undercutting unionized domestic labor. The coordination framework (2-year exemptions, cabotage rules) structures their access to cheaper labor without full harmonization that would eliminate the cost advantage. They benefit from the difference.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, service_sector_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Interprets the Treaty and the free movement chapter; issues landmark rulings that set scope. In this reading, the ECJ is positioned as enforcer of the coordination logic (protecting free movement while allowing welfare design), not as a deep integrationist authority. The court's rulings (Bolkestein, Posted Workers Directive amendments) constrain member state discretion while nominally preserving autonomy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, european_court_of_justice, observer,
    institutional, generational, analytical, universal).

% Unions and labor NGOs in receiving states oppose the posted worker regime but lack a formal seat in the EU legislative process; they can lobby but cannot veto. They would argue for harmonized minimum wages, mandatory full social contributions, and stronger cabotage rules, but the coordination logic of this reading does not require those measures.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, worker_advocacy_organizations, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, service_sector_employers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables labor market integration across member states without abolishing national welfare states: workers move, contribute to receiving-state systems, and receive receiving-state social protections based on contribution history rather than citizenship; members retain autonomy to structure welfare systems provided rules are non-discriminatory. The real coordination problem is: how to reconcile free movement of labor with heterogeneous national welfare systems without imposing either full harmonization or full closure.
% TRANSFER_FUNCTION: Moves labor and fiscal contributions from sending to receiving states (permanent migrants pay into receiving-state systems); structures temporary labor supply through posted worker exemptions (2-year contribution holidays, wage undercutting); transfers wealth from resident wage-earners to employers and from low-GDP to high-GDP member states via labor cost differentials.
% ABSENT_VOICES: Sending-state labor market constituencies (youth, construction workers, care workers) who face domestic unemployment and population loss have no formal seat in ECJ or Council decision-making. Posted workers themselves are not represented in discussions of 2-year exemption policies; unions negotiating cabotage rules are not at the table where the exemptions are set. The voices that would argue for stronger fiscal redistribution or deeper harmonization are structurally excluded.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, labor mobility would collapse (workers could not move freely) or shift entirely to sending-state control (no EU free movement guarantee), and receiving states would likely impose unilateral welfare closures (no migrant access without years of contribution). The EU single market for labor would bifurcate into national labor markets again. Member states would lose the fiscal subsidy from high-wage zones drawing workers from low-wage zones.
% FOUNDING_PROBLEM: Post-1989 EU expansion eastward created open labor markets without harmonized wages or welfare systems; workers moved to higher-wage zones; receiving states faced fiscal pressure (welfare access) and labor market pressure (wage undercutting). The founding problem was: how to enable mobility without either complete harmonization (politically impossible, threatens national sovereignty) or complete closure (politically impossible, contradicts EU founding commitments to free movement).
% FOUNDING_PROBLEM_CORROBORATION: The Commission and ECJ attest the founding problem is partly solved: free movement is protected, welfare systems are sustainable because eligibility is tied to contribution history, not citizenship. Sending-state governments, labor unions, and economic analysts (Portes, Wadensjo) attest the founding problem persists: fiscal losses in sending states, wage pressure in receiving states, posted worker precarity are structural, not solved by the coordination logic.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.54 to 0.68 over the interval (observed to time 15, then projected flat). The rise is driven by: (1) increasing posted worker volumes as employers learn and exploit the 2-year exemption window; (2) permanent migration of lower-skilled workers into receiving states, creating dual labor market pressure; (3) fiscal pressure in receiving states from welfare access without proportional contribution history. At t=25, the curve flattens (projected) because the constraint reaches a steady state: the political settlement holding the middle ground (autonomy + coordination) becomes institutionalized; further extraction is constrained by resistance (labor unions, receiving-state governments calling for harmonization or closure). Suppression is lower (0.48→0.61) than extractiveness because the constraint is not enforced primarily through coercion; enforcement relies on ECJ's legal authority and Treaty primacy, not on police power. Theater rises to 0.42 because EU Commission rhetoric about 'social Europe' and 'posting worker protections' increasingly decouples from actual material impact: Posting Workers Directives are amended with stronger language while exemption windows remain and are widely exploited. This gap between stated protections and actual extraction is measured by theater_ratio. The one-grid rule is applied: all three metrics are authored at six shared time points (0, 5, 10, 15, 20, 25) so the engine can read them synchronously.
 *
 * PERSPECTIVAL GAP:
 *   From the EU supranational seat, the constraint is coordination: free movement is enabled while welfare systems remain solvent because eligibility is tied to contribution history. From the sending-state seat, it is extraction: workers are lost without fiscal compensation and there is no mechanism to recover the education investment sent-state governments made in those workers. From the receiving-state labor market seat (unions, resident workers), it is coercive labor supply: posted workers are mandated into the market at below-equilibrium wages and resident workers must accept wage pressure as the price of free movement. From the posted worker's seat, it is entrapment: temporary status without protections, undercutting power, no union representation. From the high-skill migrant seat, it is pure coordination: labor market access with credential recognition and full welfare protections after minimal waiting. The engine computes all four perspectives from the structural data (power, exit, beneficiary/victim, time_horizon). The claim that this is a tangled rope (mixed coordination + extraction) is the welfare reading's assertion; the metrics you are reading were authored to be consistent with that claim but are independent of it — the engine will compute whether each seat actually experiences a rope or a snare or a piton.
 *
 * DIRECTIONALITY LOGIC:
 *   d-value derivation: high-skill migrants have low d (near 0.0, beneficiaries) because they benefit from unrestricted labor market access and have arbitrage-grade exit (can move to non-EU high-wage zones). Service-sector employers have low d (0.15-0.25 range) because they benefit from posted worker access and can manage multi-state hiring (arbitrage exit). Posted workers have high d (0.85-0.95, targets) because they are trapped (no exit to domestic jobs), undercutting (wage pressure), and excluded from receiving-state welfare (immediate cost). Resident labor market workers have moderate d (0.60-0.70) because they bear wage pressure and constrained mobility but retain union organization and cannot be fully trapped (they can move as free movement applies to them too, but cost of moving is high). Receiving-state governments have moderate d (0.65-0.75) because they bear dual cost (wage + fiscal pressure) but retain welfare design autonomy and can modify eligibility rules within non-discrimination constraints. Sending-state governments have high d (0.75-0.85) because they face fiscal losses from emigration with no compensation mechanism and are structurally excluded from ECJ/Council seats determining free movement scope. No directionality overrides are needed; the derivation from beneficiary/victim + exit_options + power produces coherent d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1989 labor market integration without harmonized wages) was real and live at t=0. The constraint was built to solve it: enable mobility without forcing harmonization or closure. At t=25, the founding problem status is contested. The supranational reading (ECJ, Commission) claims the problem is solved: free movement is stable, welfare systems are sustainable. Sending-state governments and labor unions claim the problem persists: emigration losses, wage pressure, posted worker exploitation are structural, not solved. The measurement series and theater_ratio trajectory support the labor-union reading: extraction rises while the stated coordination function (free movement enabling sustainable welfare systems) does not improve. This is a live mandatrophy case: the original rationale (balanced coordination) is increasingly compromised by asymmetric extraction. The constraint persists because the EU authority structure (ECJ, Commission) maintains it through legal/institutional force, not because the coordination logic holds. The theater_ratio above 0.40 at t=25 is the marker: more enforcement energy goes into defending the coordination narrative than into actual coordination. A genuine mandatrophy resolution would require: (1) acknowledging extraction is primary and coordination is secondary, (2) restructuring to either deepen integration (supranational tax + redistribute), (3) or accept closure (members exclude migrants). The welfare reading does neither; it maintains the middle ground by theater and legal authority, not by function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_state_sustainability_ambiguity,
    'Is the coordination logic sustainable long-term, or does it mask fiscal transfers that will eventually force member states to renationalize welfare systems or exit the free movement commitment?',
    'Long-term fiscal accounting: track net contribution/extraction per member state cohort over 20+ years; model demographic aging in sending vs. receiving states; monitor welfare system solvency reports and political pressure for re-eligibilification.',
    'If coordination is unsustainable, the constraint is a temporary duct-tape over a structural incompatibility between heterogeneous welfare states and free movement. The type would shift toward piton (maintained by theater, not function) or scaffold (built to expire). If sustainable, it is a genuine tangled rope coordinating mobility while allowing autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_state_sustainability_ambiguity, empirical, 'Whether the coordination logic can persist without forcing harmonization or closure.').

omega_variable(
    posted_worker_status_legitimacy,
    'Is the posted worker 2-year exemption a legitimate temporary transition mechanism, or is it a structural exploitation loophole that persists because beneficiary employers block strengthening?',
    'Counterfactual: if the 2-year exemption were eliminated and posted workers paid full tax + social contributions, would the resulting wage levels still incentivize posting, or would employers cease posting? If they cease, the exemption was rent-extraction, not coordination cost.',
    'If the exemption is structural rent, posted workers move from payer to victim (currently dual-classified). The extraction becomes harder to justify as coordination and looks more like snare (coercive labor supply). If eliminating it stops posting entirely, some receiving-state labor market protection is lost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(posted_worker_status_legitimacy, empirical, 'Whether posted worker exemptions are coordination cost or pure rent.').

omega_variable(
    kernel_reading_contestation,
    'Does this reading (welfare coordination via national autonomy) represent a stable equilibrium of the federation membership kernel, or is it being displaced by one of the sibling readings (deep integration via supranational authority, or strengthened member sovereignty via closure)?',
    'Institutional trajectory: monitor ECJ case law trends (narrowing or expanding member state discretion), EU legislative proposals (harmonization vs. subsidiarity), and member-state exit rhetoric (as in Brexit, Orban''s sovereigntist moves). Track which reading wins cases and shapes new directives.',
    'This reading assumes ECJ and Commission enforce a stable middle ground. If ECJ moves toward expansionist interpretation of free movement (integration reading), the constraint becomes a transitional stage toward deeper harmonization. If member states assert closure (sovereignty reading), the constraint fractures and free movement collapses into national control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'The kernel contest among three readings of federation membership and free movement scope.').

omega_variable(
    sending_state_fiscal_loss_visibility,
    'Do sending states experience their emigration-driven fiscal losses as a cost imposed by the EU coordination logic, or as an internal demographic problem outside the constraint''s scope?',
    'Political framing: track whether sending-state governments frame worker emigration as an EU/free movement problem (in Council negotiations, ECJ briefs) or as a domestic labor-market adjustment. If framed as EU problem, pressure increases for fiscal redistribution or remigration incentives; if domestic, the loss becomes politically invisible to the constraint.',
    'If losses become politically visible as EU-imposed, the constraint''s stability depends on redistribution mechanisms (EU structural funds, cohesion payments) that are not part of the coordination logic itself — piton-adjacent (maintained by separate transfer, not by function). If losses stay invisible, the constraint persists by asymmetric awareness, not equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sending_state_fiscal_loss_visibility, preference, 'Whether sending-state fiscal losses are attributed to the constraint or absorbed as domestic problems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 25, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'federation_membership_kernel'. The three readings are structurally distinct constraints with different ε values, victim/beneficiary sets, and institutional mechanisms: (1) welfare_coordination_reading (this story) treats free movement as coordinated through national welfare system harmonization; ε=0.68, victims={posted_workers, resident_labor_workers, sending_state_systems}, mechanism=ECJ/Commission enforcement of subsidiarity + anti-social-dumping directives. (2) integration_reading treats free movement as a fundamental right driving supranational authority; ε would be lower (0.40-0.50), victims would be member states forced toward harmonization, beneficiaries would include supranational authority, mechanism would be expansionist ECJ interpretation. (3) member_sovereignty_reading treats free movement as bounded by national capacity; ε would be higher (0.75-0.85), victims would be migrants/employers, beneficiaries would be sending-state governments, mechanism would be member-state control over eligibility and labor standards. Each reading is a distinct strategic position in the ongoing contest over federation scope. The three stories are linked because a shift in ECJ interpretation or member-state coalition changes which reading is politically dominant. Do NOT collapse these into one story with 'observable-dependent ε' — they are three different constraints with three different stabilization points. The kernel is fixed (Treaty articles 45, 48); the readings are different instantiations of those articles with different structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
