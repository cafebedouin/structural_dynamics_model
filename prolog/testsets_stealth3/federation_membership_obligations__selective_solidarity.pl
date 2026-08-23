% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Contribution-Tiered Solidarity Gate in the Federal Union
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   Within a federal union guaranteeing free movement, access to residence
 *   security and welfare provision is conditioned on contribution history and
 *   current economic-activity status rather than on citizenship or
 *   nationality. Continuous employment purchases full tier membership;
 *   economic inactivity after three months exposes a mover to loss of
 *   residence; fragmented work records leave people formally employed yet
 *   permanently beneath the qualifying line. This file instantiates ONE
 *   reading - selective_solidarity - of the contested kernel
 *   federation_membership_obligations. The referent for epsilon is the
 *   standing tiered arrangement itself, assessed by this reading's own
 *   lights; the sibling readings (integration_primary,
 *   member_sovereignty_primary) are other constraints in other files, linked
 *   through network.affects_constraints. The claimed_type and the authored
 *   metrics are independent facts: I claim tangled_rope because I judge the
 *   arrangement to possess both a real coordination function and real
 *   asymmetric extraction; the metrics describe its observed operation
 *   without being tuned to any predicted verdict.
 *
 * KEY AGENTS:
 *   - host_state_governments: agenda setter (institutional/constrained) - draws the tier boundary, collects fiscal relief and electoral credit
 *   - host_state_welfare_bureaucracies: enforcement arm (institutional/constrained) - runs the tests that sort movers
 *   - established_contributory_workers: beneficiary-payer (moderate/mobile) - rights premium purchased with contributions
 *   - cross_border_employers: secondary beneficiary (organized/arbitrage) - flexible labor without firm-level welfare liability
 *   - economically_inactive_movers: primary target (powerless/trapped) - bears exclusion wherever they settle
 *   - short_tenure_precarious_workers: target with illusory exit (powerless/nominally mobile)
 *   - returning_nationals_foreign_records: identity-locked target (moderate/identity_locked) - reverse-discrimination class
 *   - migrant_support_ngo_networks: evidentiary observer (organized/analytical)
 *   - federation_court: adjudicative observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.62).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.58).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Contribution-Tiered Solidarity Gate in the Federal Union").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '7a4663de-93f7-49f2-bf88-cf1f5b128937').
narrative_ontology:cs_kernel_codification('7a4663de-93f7-49f2-bf88-cf1f5b128937', fixed_text).
narrative_ontology:cs_authority_grounding('7a4663de-93f7-49f2-bf88-cf1f5b128937', distributed).
narrative_ontology:cs_reading_relation('7a4663de-93f7-49f2-bf88-cf1f5b128937', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('7a4663de-93f7-49f2-bf88-cf1f5b128937', federation_membership_obligations__member_sovereignty_primary, influences).
narrative_ontology:cs_axiom('7a4663de-93f7-49f2-bf88-cf1f5b128937', foundational, entitlement_tracks_contribution_history).
narrative_ontology:cs_axiom_status(entitlement_tracks_contribution_history, holdable).
narrative_ontology:cs_axiom_grounding('7a4663de-93f7-49f2-bf88-cf1f5b128937', entitlement_tracks_contribution_history, conventional).
narrative_ontology:cs_axiom('7a4663de-93f7-49f2-bf88-cf1f5b128937', secondary, self_sufficiency_conditions_residence_security).
narrative_ontology:cs_axiom_status(self_sufficiency_conditions_residence_security, holdable).
narrative_ontology:cs_axiom_grounding('7a4663de-93f7-49f2-bf88-cf1f5b128937', self_sufficiency_conditions_residence_security, instrumental).
narrative_ontology:cs_reference_frame('7a4663de-93f7-49f2-bf88-cf1f5b128937', contribution_earned_membership).
narrative_ontology:cs_drift_state('7a4663de-93f7-49f2-bf88-cf1f5b128937', post_enlargement_austerity_period, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7a4663de-93f7-49f2-bf88-cf1f5b128937', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, established_contributory_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_governments).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_movers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, short_tenure_precarious_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, returning_nationals_foreign_records).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, established_contributory_workers).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_insurance_principle).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, habitual_residence_test_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the domestic implementation of residence and benefit rules: define habitual residence tests, decide when economically inactive arrivals may be required to leave, and report enforcement statistics upward. Gain budget relief from claims not paid and electoral credit from visible anti-abuse administration; bear the administrative cost of running the tests and the diplomatic friction when origin states object to the treatment of their nationals.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, host_state_governments, beneficiary).

% Operate the day-to-day sorting: verify contribution records, assess comprehensive sickness insurance and sufficient resources, process right-to-reside examinations, and execute removal decisions. Their caseload, staffing, and data-sharing infrastructure have grown with the tiering; their discretion narrows as standardized tests and cross-border record checks spread.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_welfare_bureaucracies, agenda_setter,
    institutional, biographical, constrained, national).

% Mobile people in continuous employment who pay payroll contributions in each host state and accumulate portable pension, healthcare, and unemployment entitlements. Their contribution record purchases full residence security and first-position access to benefits; they also finance the system they draw on, and their relative status depends on the tier boundary staying where it is.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, established_contributory_workers, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, established_contributory_workers, payer).

% Recruit across the federation's open labor market while welfare liability for their workforce follows contribution status rather than falling on the firm. Seasonal, platform, and rotational hiring models depend on workers whose qualification clocks reset with each move; the firms can relocate operations or sourcing across borders faster than any welfare rule can follow them.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, cross_border_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% People who move for family formation, reunion, study-adjacent survival work, flight from local collapse, or retirement and are not classified as economically active. After three months they must show comprehensive insurance and sufficient resources or lose residence; most benefits are closed to them regardless of need. Returning to the origin state means abandoning households, relationships, and years of life built in place; moving elsewhere in the federation reproduces the same classification wherever they land.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_movers, payer,
    powerless, biographical, trapped, continental).

% Seasonal harvest, construction, logistics, cleaning, and platform workers whose employment spells are too short or fragmented to accumulate qualifying contribution records. They work continuously yet sit beneath the tier boundary; each move to find the next job restarts the qualification period, so the mobility available to them functions as a penalty rather than an exit. Illness or a gap month converts them into the economically inactive class overnight.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, short_tenure_precarious_workers, payer,
    powerless, immediate, mobile, continental).

% Nationals who spent working decades abroad and return home to find their foreign contribution records discounted or inapplicable under domestic rules, leaving them with weaker access than recent arrivals who contributed locally. Their nationality binds them to the jurisdiction applying the stricter standard; they cannot take up another citizenship to escape the gap, and their case is politically invisible because it reads as a paradox rather than an injury.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, returning_nationals_foreign_records, payer,
    moderate, biographical, identity_locked, continental).

% Legal clinics, church-linked services, and transnational advocacy networks that document refusals, bring test cases, and publish monitoring reports on destitution and removal practices among mobile populations. They hold no vote in council negotiations but supply the evidentiary record that courts and parliaments cite.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, migrant_support_ngo_networks, observer,
    organized, generational, analytical, continental).

% The union-level judiciary that adjudicates the boundary between movement rights and national welfare competence. Its rulings have alternately expanded and conceded ground: early proportionality protections for claimants, later deference to member-state assessments that economically inactive persons can be required to leave. Each ruling re-prices the tier boundary for everyone else.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federation_court, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains politically feasible intra-federation labor mobility across welfare systems that remain nationally financed and heterogeneous: conditioning access on contribution history reassures host electorates that payment tracks contribution, which keeps borders open where unconditional access would trigger closure demands and welfare-competition races.
% TRANSFER_FUNCTION: Moves welfare-system access and residence security away from economically inactive movers, short-tenure precarious workers, and returning nationals with foreign records, and toward established contributory workers, host-state budgets, and employers running mobile labor models.
% ABSENT_VOICES: Economically inactive movers have no franchise in host-state elections and thin representation through origin-state politics; unrecognized care and household workers sit outside the very category that defines the tiers and therefore cannot even litigate their classification. Their objection - that access should track residence and need rather than payroll record - reaches council negotiations mainly through NGOs and occasional court dockets.
% DISAPPEARANCE_RATIONALE: If the contribution-tiering vanished overnight, host-state budget projections, employer staffing models built on rotational migrant labor, origin-state remittance flows, and residence patterns would all re-key simultaneously. Either federation-level fiscal compensation appears to absorb the claims shift, or member states reimpose unilateral closure within months; the current mobility equilibrium does not survive the arrangement's removal intact.
% FOUNDING_PROBLEM: Reconcile free movement of workers with nationally financed, heterogeneous welfare states without triggering a welfare-load backlash that would close the borders the federation exists to keep open.
% FOUNDING_PROBLEM_CORROBORATION: The tension is attested from outside the benefiting parties: federation court jurisprudence repeatedly names the mobility-versus-welfare-sustainability conflict even while contesting the resolution; independent academic welfare-state research documents persistent fiscal heterogeneity and intensifying demographic pressure; NGO monitoring reports document the exclusion costs on the paying side. Host-government attestation that the problem remains live is self-interested and is discounted accordingly.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: severe and concentrated for the targeted minorities (denial of subsistence-level claims, removal exposure, permanent sub-threshold status for fragmented workers), partially offset by the genuine insurance function that contributing movers receive. Suppression is 0.58 as a raw structural property - unscaled by power or scope per the framework rule: the machinery (residence verification, resource tests, removal powers, restricted exportability) closes the claiming alternative for whole classes while leaving physical movement formally open, which is precisely why the suppression lands harder than a border closure of equal headline severity. Theater ratio 0.30: habitual-residence testing performs real allocative work, but a growing enforcement share defends the boundary symbolically - crackdowns aimed at fraud rates near statistical noise, sovereignty displays for domestic audiences - hence the mild rise and late plateau in the series. Accessibility_collapse 0.60: understanding the tiering collapses most alternatives for those beneath the line (private insurance and self-funding are the residual options) but not completely, since taking qualifying employment remains a real if unevenly reachable path. Resistance 0.55: sustained NGO litigation, origin-state objections, episodic court pushback, and parliamentary dissent - real and occasionally effective (visible in the T24 softening of the suppression and extractiveness series), yet the hardening trend dominated the interval. The three measurement series share one grid (T=0,4,8,12,16,20,24); suppression_requirement is authored deliberately because the story's traced dynamic IS enforcement-capacity build-up (data-sharing infrastructure, standardized tests) followed by partial judicial normalization - not merely extraction shifting under static enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From host_state_governments the arrangement is prudent insurance design that keeps open borders politically survivable - a coordination achievement they administer and take credit for. From economically_inactive_movers and short_tenure_precarious_workers the same rules operate as conditional personhood: presence tolerated, support denied, removal threatened, with no electoral recourse anywhere. Established_contributory_workers straddle the line - they fund the pool and draw the premium, and their perception flips with employment continuity. The federation_court seat oscillates across the interval, which the measurement series registers as the late-cycle softening. The engine derives these divergent per-seat classifications from the structural data; this commentary asserts none of them as a verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for established_contributory_workers, cross_border_employers, and host_state_governments; victim declarations drive high d for the three paying classes. Three overrides correct derivations the structural data alone would get wrong: (1) short_tenure_precarious_workers derive a mid-range d from their nominal mobility, but their exit is illusory - exercising it resets the very contribution clocks the constraint measures, so mobility is taxed, not freeing; overridden to 0.90. (2) host_state_governments derive near-beneficiary d from the fiscal-gain declaration, but they bear real enforcement administration costs and welfare-politics backlash, so they are not pure collectors; overridden to 0.25. (3) established_contributory_workers derive near-zero d from the beneficiary declaration, ignoring that they pay in continuously; overridden to 0.40, slightly favor-side of symmetric. Scope amplification applies modestly at continental reach where record verification across jurisdictions is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy resolution is declared: fiscal heterogeneity and demographic pressure persist, and the firewall function remains load-bearing. The classification nonetheless guards against two mislabels. Reading the tiering as a pure snare erases the demonstrated coordination achievement - unconditional-access proposals have repeatedly collapsed politically, and the tiered design measurably kept borders open where alternatives failed. Reading it as a pure rope erases the constructed exclusions: care and household work falling outside 'economic activity,' returning nationals stranded by record discounting, and qualification periods that penalize exactly the mobility the federation celebrates are boundary-drawing choices, not actuarial necessities. The trajectory to watch is theater_ratio: the mandate has begun drifting from fiscal firewall toward status allocation, and a sustained crossing above 0.5 - enforcement defending the boundary more than administering it - would mark the point where the coordination story became cover and the arrangement should re-read toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (selective_solidarity) of the kernel federation_membership_obligations; what structurally changes if a sibling reading governs instead?',
    'Comparative counterfactual analysis across the three readings: model citizenship-based access (integration_primary) and national closure regimes (member_sovereignty_primary) against observed mobility volumes, fiscal flows, and exclusion incidence under the current tiered arrangement.',
    'Under integration_primary the tier structure collapses and epsilon for mobile persons falls toward zero while host treasuries become the bearing seat; under member_sovereignty_primary the extraction persists but its locus shifts to twenty-seven unilateral closures with higher aggregate friction. The disagreement is located in the unit of entitlement, and no dataset resolves it - it is a framing choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: which reading of membership obligations governs determines the entire beneficiary/victim topology.').

omega_variable(
    contribution_metric_construction,
    'Is contribution history a neutral actuarial measure of paid-in status, or does its construction - what counts as contribution, which work registers, which periods count - embed incumbent advantage?',
    'Audit classification rules against labor-market composition: gendered care and household work excluded from ''economic activity'', informal and platform labor generating non-qualifying records, qualification-period design relative to typical migrant job-spell lengths.',
    'If the metric is constructed rather than neutral, part of the measured tiering is boundary-drawing that benefits established contributors and employers of fragmented labor, and effective extraction exceeds the authored estimate; if robustly neutral, the current figure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contribution_metric_construction, empirical, 'Whether the sorting variable itself is impartial or encodes the sorter''s interests.').

omega_variable(
    firewall_function_separability,
    'Is the fiscal-sustainability function achievable without status-tiered exclusion - for example via time-bounded transitional assistance or federation-level fiscal equalization - or is the status hierarchy itself load-bearing for the coordination outcome?',
    'Natural experiments from jurisdictions that extended broader safety nets to mobile persons without measurable inflow surges, combined with comparative welfare-usage data controlling for wage and demographic differences.',
    'If separable, the status-hierarchy component is not required by the coordination function and the arrangement sits closer to pure extraction than authored; if inseparable, part of the burden on payers is the genuine price of open borders and the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firewall_function_separability, conceptual, 'Whether the coordination and exclusion components of the tiering can be pried apart.').

omega_variable(
    reverse_discrimination_scale,
    'How extensive is the harm class of returning nationals whose foreign contribution records are discounted or inapplicable under domestic rules?',
    'Cross-border social-security record-linkage audits quantifying returning workers whose accumulated records fail to convert, and tracking their benefit-denial rates against locally-recorded peers.',
    'Large-scale findings would widen the victim set beyond the mobile poor, sharpen the internal-parity objection, and raise effective resistance estimates; negligible findings would narrow the paying class to the economically inactive and fragmented-worker seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_discrimination_scale, empirical, 'Size of the identity-locked reverse-discrimination population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_sel_solidarity_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.14).
narrative_ontology:measurement(fed_sel_solidarity_tr_t4, federation_membership_obligations__selective_solidarity, theater_ratio, 4, 0.17).
narrative_ontology:measurement(fed_sel_solidarity_tr_t8, federation_membership_obligations__selective_solidarity, theater_ratio, 8, 0.21).
narrative_ontology:measurement(fed_sel_solidarity_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.25).
narrative_ontology:measurement(fed_sel_solidarity_tr_t16, federation_membership_obligations__selective_solidarity, theater_ratio, 16, 0.28).
narrative_ontology:measurement(fed_sel_solidarity_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.31).
narrative_ontology:measurement(fed_sel_solidarity_tr_t24, federation_membership_obligations__selective_solidarity, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(fed_sel_solidarity_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fed_sel_solidarity_be_t4, federation_membership_obligations__selective_solidarity, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(fed_sel_solidarity_be_t8, federation_membership_obligations__selective_solidarity, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(fed_sel_solidarity_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(fed_sel_solidarity_be_t16, federation_membership_obligations__selective_solidarity, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(fed_sel_solidarity_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(fed_sel_solidarity_be_t24, federation_membership_obligations__selective_solidarity, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fed_sel_solidarity_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.41).
narrative_ontology:measurement(fed_sel_solidarity_su_t4, federation_membership_obligations__selective_solidarity, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(fed_sel_solidarity_su_t8, federation_membership_obligations__selective_solidarity, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(fed_sel_solidarity_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(fed_sel_solidarity_su_t16, federation_membership_obligations__selective_solidarity, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(fed_sel_solidarity_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(fed_sel_solidarity_su_t24, federation_membership_obligations__selective_solidarity, suppression_requirement, 24, 0.58).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=24
narrative_ontology:measurement(fed_sel_solidarity_grid_01, federation_membership_obligations__selective_solidarity, accessibility_collapse(class), 0, 0.25).
narrative_ontology:measurement(fed_sel_solidarity_grid_02, federation_membership_obligations__selective_solidarity, accessibility_collapse(class), 24, 0.55).
narrative_ontology:measurement(fed_sel_solidarity_grid_03, federation_membership_obligations__selective_solidarity, accessibility_collapse(individual), 0, 0.22).
narrative_ontology:measurement(fed_sel_solidarity_grid_04, federation_membership_obligations__selective_solidarity, accessibility_collapse(individual), 24, 0.58).
narrative_ontology:measurement(fed_sel_solidarity_grid_05, federation_membership_obligations__selective_solidarity, accessibility_collapse(organizational), 0, 0.2).
narrative_ontology:measurement(fed_sel_solidarity_grid_06, federation_membership_obligations__selective_solidarity, accessibility_collapse(organizational), 24, 0.48).
narrative_ontology:measurement(fed_sel_solidarity_grid_07, federation_membership_obligations__selective_solidarity, accessibility_collapse(structural), 0, 0.3).
narrative_ontology:measurement(fed_sel_solidarity_grid_08, federation_membership_obligations__selective_solidarity, accessibility_collapse(structural), 24, 0.62).
narrative_ontology:measurement(fed_sel_solidarity_grid_09, federation_membership_obligations__selective_solidarity, resistance(class), 0, 0.15).
narrative_ontology:measurement(fed_sel_solidarity_grid_10, federation_membership_obligations__selective_solidarity, resistance(class), 24, 0.55).
narrative_ontology:measurement(fed_sel_solidarity_grid_11, federation_membership_obligations__selective_solidarity, resistance(individual), 0, 0.1).
narrative_ontology:measurement(fed_sel_solidarity_grid_12, federation_membership_obligations__selective_solidarity, resistance(individual), 24, 0.3).
narrative_ontology:measurement(fed_sel_solidarity_grid_13, federation_membership_obligations__selective_solidarity, resistance(organizational), 0, 0.25).
narrative_ontology:measurement(fed_sel_solidarity_grid_14, federation_membership_obligations__selective_solidarity, resistance(organizational), 24, 0.4).
narrative_ontology:measurement(fed_sel_solidarity_grid_15, federation_membership_obligations__selective_solidarity, resistance(structural), 0, 0.3).
narrative_ontology:measurement(fed_sel_solidarity_grid_16, federation_membership_obligations__selective_solidarity, resistance(structural), 24, 0.5).
narrative_ontology:measurement(fed_sel_solidarity_grid_17, federation_membership_obligations__selective_solidarity, stakes_inflation(class), 0, 0.3).
narrative_ontology:measurement(fed_sel_solidarity_grid_18, federation_membership_obligations__selective_solidarity, stakes_inflation(class), 24, 0.68).
narrative_ontology:measurement(fed_sel_solidarity_grid_19, federation_membership_obligations__selective_solidarity, stakes_inflation(individual), 0, 0.28).
narrative_ontology:measurement(fed_sel_solidarity_grid_20, federation_membership_obligations__selective_solidarity, stakes_inflation(individual), 24, 0.7).
narrative_ontology:measurement(fed_sel_solidarity_grid_21, federation_membership_obligations__selective_solidarity, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement(fed_sel_solidarity_grid_22, federation_membership_obligations__selective_solidarity, stakes_inflation(organizational), 24, 0.52).
narrative_ontology:measurement(fed_sel_solidarity_grid_23, federation_membership_obligations__selective_solidarity, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(fed_sel_solidarity_grid_24, federation_membership_obligations__selective_solidarity, stakes_inflation(structural), 24, 0.52).
narrative_ontology:measurement(fed_sel_solidarity_grid_25, federation_membership_obligations__selective_solidarity, suppression(class), 0, 0.2).
narrative_ontology:measurement(fed_sel_solidarity_grid_26, federation_membership_obligations__selective_solidarity, suppression(class), 24, 0.45).
narrative_ontology:measurement(fed_sel_solidarity_grid_27, federation_membership_obligations__selective_solidarity, suppression(individual), 0, 0.18).
narrative_ontology:measurement(fed_sel_solidarity_grid_28, federation_membership_obligations__selective_solidarity, suppression(individual), 24, 0.42).
narrative_ontology:measurement(fed_sel_solidarity_grid_29, federation_membership_obligations__selective_solidarity, suppression(organizational), 0, 0.3).
narrative_ontology:measurement(fed_sel_solidarity_grid_30, federation_membership_obligations__selective_solidarity, suppression(organizational), 24, 0.55).
narrative_ontology:measurement(fed_sel_solidarity_grid_31, federation_membership_obligations__selective_solidarity, suppression(structural), 0, 0.25).
narrative_ontology:measurement(fed_sel_solidarity_grid_32, federation_membership_obligations__selective_solidarity, suppression(structural), 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% The colloquial label 'federation membership obligations' covers three structurally distinct claims and is decomposed into a three-story constraint family. All three readings share one referent - the standing free-movement/welfare arrangement - and author different epsilon over it per the reading-indexed rule: integration_primary authors low epsilon (a rights-expanding arrangement with minimal extraction from movers); member_sovereignty_primary authors high epsilon for mobility-imposed externalities on national welfare compacts; this file (selective_solidarity) authors epsilon 0.62 for the contribution-tiered standing arrangement as this reading sees it. Upstream/downstream: integration_primary is the historical upstream claim cited as legitimation; this reading exerts downstream structural pressure on member_sovereignty_primary. Family members link via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerless, 0.9).
constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, institutional, 0.25).
constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, moderate, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
