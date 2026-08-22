% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Contributory Tiering of Federation Mobility and Welfare Access
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Within a federation of nationally financed contributory welfare states,
 *   this arrangement gates the welfare dimension of free movement by
 *   contribution history and current economic activity: employed mobile
 *   workers accumulate portable entitlements, while economically inactive
 *   residents face restricted access, and continued residence can depend on
 *   passing resource tests. The arrangement is presented as actuarial
 *   fairness — the pool serves those who feed it — and it does solve a real
 *   coordination problem, but the same machinery distributes the costs of
 *   mobility onto those least able to carry them: newcomers mid-vesting,
 *   informal and care workers whose labor never registers, and origin states
 *   absorbing returns. This file instantiates one reading
 *   (selective_solidarity) of the federation_membership_obligations kernel;
 *   see commentary.kernel_context and the linked sibling stories for the
 *   decomposition.
 *
 * KEY AGENTS:
 *   - host_state_welfare_administrators: Agenda-setting administrator (institutional/arbitrage) — designs and enforces the tests, collects the fiscal relief
 *   - host_state_taxpayers: Primary beneficiary (organized/constrained) — contributory pools shielded from non-record claims
 *   - vested_mobile_workers: Secondary beneficiary (moderate/constrained) — earned portable entitlements distinguish them from newcomers
 *   - newly_arrived_mobile_workers: Primary target (moderate/constrained) — pays in from day one, access lags behind
 *   - economically_inactive_migrants: Primary target (powerless/trapped) — restricted access; relocation does not escape the gate
 *   - informal_care_economy_workers: Target (powerless/trapped) — real work outside the counted contribution system
 *   - sending_member_states: Institutional target (institutional/constrained) — absorbs returned non-qualifying nationals
 *   - federal_mobility_court: Analytical observer (institutional/analytical) — adjudicates the tests' treaty compliance
 *   - migrant_family_dependents: Excluded voice (powerless/trapped) — governed by the tests, never party to them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.48).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.5).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.48).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Contributory Tiering of Federation Mobility and Welfare Access").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'cba8de78-a7ee-4d26-a08d-9bb4e20395bd').
narrative_ontology:cs_kernel_codification('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', fixed_text).
narrative_ontology:cs_authority_grounding('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', lineage).
narrative_ontology:cs_interpretation_layer_present('cba8de78-a7ee-4d26-a08d-9bb4e20395bd').
narrative_ontology:cs_reading_relation('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', federation_membership_obligations__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', foundational, entitlement_tracks_contribution_not_personhood).
narrative_ontology:cs_axiom_status(entitlement_tracks_contribution_not_personhood, holdable).
narrative_ontology:cs_axiom_grounding('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', entitlement_tracks_contribution_not_personhood, deontological).
narrative_ontology:cs_axiom('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', secondary, contributory_tiering_preserves_open_mobility).
narrative_ontology:cs_axiom_status(contributory_tiering_preserves_open_mobility, holdable).
narrative_ontology:cs_axiom_grounding('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', contributory_tiering_preserves_open_mobility, instrumental).
narrative_ontology:cs_reference_frame('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', contributive_entitlement_framework).
narrative_ontology:cs_drift_state('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', contemporary_enforcement_maturation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('cba8de78-a7ee-4d26-a08d-9bb4e20395bd', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_taxpayers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, vested_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, newly_arrived_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, informal_care_economy_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, sending_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, newly_arrived_mobile_workers).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_entitlement_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, actuarial_fairness_in_social_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the host state's social security and assistance systems. Design and apply the residence and economic-activity tests that gate access, maintain the contribution-record databases shared across the federation, and publish enforcement statistics. The tiering relieves their budgets of claims from people without local contribution records and supplies the governing narrative that the system rewards those who pay in. They can rewrite the tests and qualifying periods at will; abandoning the arrangement altogether would mean giving up the fiscal shield the tests provide.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_welfare_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Fund the contributory pools through payroll levies and experience the tiering as keeping premiums matched to their own payment histories. They carry diffuse indirect costs where migrant labor holds down prices, and their associations lobby for tighter tests during fiscal stress. Leaving the tax base means emigrating, which few ever do.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_taxpayers, beneficiary,
    organized, generational, constrained, national).

% Moved between member states years ago and completed the qualifying periods; their contribution records now travel with them and their entitlements are secure. The tiering distinguishes them favorably from newer arrivals, and their unions defend the earned-rights logic. Moving again would restart parts of the qualifying clock, so further relocation carries a real penalty.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, vested_mobile_workers, beneficiary,
    moderate, biographical, constrained, continental).

% Arrive, register, and begin paying payroll contributions immediately, but full access to several benefits opens only after qualifying periods elapse. During the gap they self-fund illness, unemployment spells, and family events, or fall back on narrowly scoped emergency coverage. They gain immediate labor-market access — the core of what brought them — which softens but does not remove the payment-access gap. Staying builds their record; returning forfeits it.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, newly_arrived_mobile_workers, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, newly_arrived_mobile_workers, beneficiary).

% Job seekers, people between contracts, accompanying partners, and early retirees who live in a member state without a current contribution record. The tests classify them as not qualifying: means-tested assistance is restricted or withheld, and continued residence can require demonstrating sufficient resources or health coverage. Every other member state applies comparable tests, so relocating does not escape the gate; returning to their origin state usually means losing the income that brought them abroad.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, biographical, trapped, continental).

% Domestic workers, carers, and platform or seasonal workers whose labor is real but generates little or no registered contribution. Residence permits are frequently tied to the very employers who keep arrangements informal, so formalizing the work risks the permit itself. On paper they look like non-contributors; in practice they work long hours outside the counted economy.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, informal_care_economy_workers, payer,
    powerless, biographical, trapped, continental).

% Origin states whose nationals depart, contribute elsewhere, and sometimes return before qualifying anywhere. They retain responsibility for citizens who come back without portable entitlements, and they press in council negotiations for recognition of pre-departure contribution periods. They cannot withdraw from free movement without leaving the union, so their recourse is negotiation and litigation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, sending_member_states, payer,
    institutional, generational, constrained, national).

% Adjudicates conflicts between mobility guarantees and national welfare-closure measures, ruling on whether residence tests, resource requirements, and exportability limits respect the founding treaties. Its case law sets the boundaries within which member states tier access. It collects nothing and pays nothing; its output is doctrine.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federal_mobility_court, observer,
    institutional, generational, analytical, continental).

% Spouses, children, and elderly parents whose access derives from the primary worker's record rather than any status of their own. They appear in the statistics as household units but had no seat when the tests were designed; their position changes whenever the worker's contract, health, or marriage does.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, migrant_family_dependents, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, host_state_welfare_administrators).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fiscal-sustainability side of mass intra-federation mobility: nationally financed contributory systems stay solvent while workers cross borders, because entitlement follows the contribution record instead of residence alone. Portable record-keeping, aggregation of periods worked in different states, and qualifying-period gates are built once, federally, instead of negotiated bilaterally state by state.
% TRANSFER_FUNCTION: Moves welfare-cost exposure away from host contributory pools onto three carriers: newly arrived workers self-fund during qualifying gaps; economically inactive residents lose access outright; sending states absorb returned nationals who never qualified anywhere. It also moves political legitimacy to host governments, who can present themselves as open to workers yet protective of the welfare pool.
% ABSENT_VOICES: Migrant family dependents and prospective movers had no seat when the tests and qualifying periods were designed; the economically inactive appear mainly as enforcement statistics or as litigants after the rules are fixed. Sending states sit in council negotiations but with less weight than host-state coalitions.
% DISAPPEARANCE_RATIONALE: Overnight repeal would expose host contributory pools to immediate claims from every resident regardless of record, forcing a choice among rapid benefit cuts, border closure, or a negotiated federal financing mechanism; mobile workers' plans, sending-state budgets, and the court's docket would all reorganize around whichever replacement emerged.
% FOUNDING_PROBLEM: When labor mobility scaled up across a federation of nationally financed contributory welfare states, every government faced the same question: who pays for a newcomer's unemployment spell, medical bills, and pension credits before they have contributed locally? Left unanswered, the choice collapses into closed borders or insolvent pools.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the federal court's own case law repeatedly frames the tension between mobility guarantees and welfare-system sustainability; sending-state submissions in council negotiations attest bearing residual costs for returned non-qualifying nationals; independent actuarial studies of cross-border social-security flows document the cost-distribution problem. Host-government attestation of the founding problem exists but is self-interested and is not counted here.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction sits near the middle (0.48) because the arrangement's core service — portable contribution accounting that keeps both mobility and national pools solvent — is real, while a widening band of its operation allocates costs by ability to resist rather than by contribution: vesting gaps that function as interest-free loans from newcomers, informal work excluded from counting, residual burdens pushed to origin states. The referent for epsilon is the standing tiered arrangement as it operates, assessed by this reading's own lights — hence not the near-zero a pure endorsement would suggest nor the high value an opposing reading would author. Suppression (0.50) is structural rather than violent: residence tests, resource requirements, and record verification make the gate binding, and no member state offers an exit from the test regime. Theater (0.31) reflects a growing share of enforcement staged for domestic audiences — highly publicized actions against marginal abuse cases — atop routine administrative function. Accessibility collapse is moderate (0.45): the citizenship-principle and national-closure alternatives remain live political programs, not collapsed options. Resistance (0.55) runs through litigation, sending-state negotiation, and migrant organizing. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream. The claimed type (tangled_rope) is my structural judgment — genuine coordination function plus asymmetric extraction under active enforcement — authored independently of these metric values; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as prudent housekeeping: tests that keep the system's promises actuarially honest. The payer seats compute something else. Among same-level actors, newly_arrived_mobile_workers and vested_mobile_workers hold identical nominal power (moderate) yet sit on opposite sides of the gate — what separates them is vesting status, which converts identical contribution payments into opposite structural positions and different exit values: the vested worker's record is an asset that penalizes further moves, while the newcomer's payments buy a clock that has barely started. Sending states are institutionally powerful yet find the costs arriving at their treasuries without their consent — power without agenda control. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Host taxpayers and vested workers are declared beneficiaries and derive low directionality — the arrangement subsidizes them (shielded pools, secured entitlements). The four declared victim groups derive high directionality, amplified for the powerless seats (economically_inactive_migrants, informal_care_economy_workers) whose trapped exit removes the damping mobility would provide, and pulled back from the full-target end for newly_arrived_mobile_workers, whose secondary beneficiary position (immediate labor-market access) moderates their d. Sending_member_states carry high directionality despite institutional power, because the cost-shift operates on their budgets regardless of their negotiating strength. The administrators sit near the beneficiary end as authors of the rule, though their exposure is reputational rather than fiscal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling mass mobility with contributory national finance — is still live, so nothing here is resolved mandatrophy; the arrangement has not outlived its function. The tangled-rope classification guards against two mislabels: reading the whole structure as pure extraction erases the portable-accounting service that keeps dozens of welfare systems solvent under open borders; reading it as pure coordination erases the named victims the same machinery produces. The temporal series shows the drift to watch: theater and suppression rising faster than extraction suggests enforcement is maturing into gatekeeping and performance — if the coordination share keeps shrinking while tests tighten, the structure slides toward snare; if the tests stop tracking actual fiscal exposure while remaining on the books, toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation_rule,
    'This constraint is the selective_solidarity reading of the federation_membership_obligations kernel — would the integration_primary or member_sovereignty_primary readings instantiate a structurally different constraint with a different victim set and classification?',
    'Generate the sibling stories and compare computed classifications; the disagreement is located in the welfare-access allocation rule — contribution record (this reading), citizenship/personhood (integration_primary), or national discretion (member_sovereignty_primary).',
    'Under integration_primary the economically_inactive_migrants victim class largely dissolves and measured extraction falls; under member_sovereignty_primary the victim set extends to mobile workers generally and suppression rises. The classification in this file holds only under the selective_solidarity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation_rule, conceptual, 'Committer structure: one of three readings of the membership-obligations kernel; sibling readings change the victim set and epsilon.').

omega_variable(
    vesting_gap_extraction_or_cost,
    'Is the gap between contribution payment and entitlement access (the qualifying period) a necessary coordination cost of portable actuarial accounting, or extractive delay imposed on newcomers?',
    'Compare the administrative and actuarial cost of shorter vesting windows against the fiscal risk they cover; exploit natural experiments where jurisdictions shortened or lengthened qualifying periods.',
    'If the gap is a necessary cost, part of the measured extraction is the price of the coordination itself (rope-side); if it is arbitrary, it is extraction layered on coordination and the structure drifts snare-ward for the newcomer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vesting_gap_extraction_or_cost, empirical, 'Whether the vesting lag is coordination price or rent.').

omega_variable(
    informal_work_contribution_boundary,
    'Does the exclusion of informal and care work from counted contribution reflect a genuine verifiability limit, or a constructed boundary that extracts from a feminized and precarious labor segment?',
    'Pilot programs crediting documented informal work, with comparison of fraud rates, administrative cost, and coverage outcomes against the standard regime.',
    'If verifiability is the binding limit, the informal_care_economy_workers victim class shrinks and the arrangement''s extraction profile narrows; if the boundary is constructed, extraction concentrates on a gendered labor segment and the classification shifts toward snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_work_contribution_boundary, empirical, 'Whether the contribution-counting boundary is technical or constructed.').

omega_variable(
    sending_state_net_flow_balance,
    'Do sending member states actually bear net residual costs for returned non-qualifying nationals, or are the flows roughly balanced by remittances and returnee human capital?',
    'Bilateral social-security flow accounting between member-state pairs, separating gross transfers from net lifetime balances.',
    'If flows balance, sending_member_states drops from the victim set and the inter-institutional extraction asymmetry narrows; if the net burden is real, institutional conflict escalates and the constraint trends snare-ward at the institutional seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_net_flow_balance, empirical, 'Whether the cost-shift to origin states is net or nominal.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of non-vested claimants structural (legal tests, documentation barriers, permit dependence) or internalized (self-exclusion driven by deservingness norms — eligible people not claiming because they believe they have not earned it)?',
    'Take-up-rate analysis against eligibility rates, plus post-reform take-up trajectories where tests were relaxed: persistent under-claiming after barrier removal indicates internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure and travels with the target after exit, raising the effective extraction computed for the powerless payer seats beyond what the legal architecture alone implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism for non-vested claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sel_solidarity_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sel_solidarity_tr_t6, federation_membership_obligations__selective_solidarity, theater_ratio, 6, 0.21).
narrative_ontology:measurement(sel_solidarity_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.24).
narrative_ontology:measurement(sel_solidarity_tr_t18, federation_membership_obligations__selective_solidarity, theater_ratio, 18, 0.27).
narrative_ontology:measurement(sel_solidarity_tr_t24, federation_membership_obligations__selective_solidarity, theater_ratio, 24, 0.29).
narrative_ontology:measurement(sel_solidarity_tr_t30, federation_membership_obligations__selective_solidarity, theater_ratio, 30, 0.31).

% Extraction over time
narrative_ontology:measurement(sel_solidarity_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(sel_solidarity_be_t6, federation_membership_obligations__selective_solidarity, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(sel_solidarity_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(sel_solidarity_be_t18, federation_membership_obligations__selective_solidarity, base_extractiveness, 18, 0.46).
narrative_ontology:measurement(sel_solidarity_be_t24, federation_membership_obligations__selective_solidarity, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(sel_solidarity_be_t30, federation_membership_obligations__selective_solidarity, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sel_solidarity_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sel_solidarity_su_t6, federation_membership_obligations__selective_solidarity, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(sel_solidarity_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(sel_solidarity_su_t18, federation_membership_obligations__selective_solidarity, suppression_requirement, 18, 0.46).
narrative_ontology:measurement(sel_solidarity_su_t24, federation_membership_obligations__selective_solidarity, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(sel_solidarity_su_t30, federation_membership_obligations__selective_solidarity, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% The colloquial label 'free movement and welfare' covers three structurally distinct allocations of one kernel (federation_membership_obligations) and is decomposed per the epsilon-invariance principle into three stories: integration_primary, member_sovereignty_primary, and this file (selective_solidarity). Each carries its own epsilon, victim set, and classification; measuring welfare access by contribution record yields a middle-band epsilon, by citizenship yields near-zero for this reading's victims, and by national discretion yields a broader victim class. integration_primary is the doctrinal upstream (treaty text all sides cite); this reading and member_sovereignty_primary are downstream political implementations that cite it selectively. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
