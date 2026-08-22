% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Fine-and-Lien Apparatus — Extraction Reading
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   Recorded covenants in common-interest communities come paired with fine
 *   schedules, violation-processing workflows, lien placement, and
 *   attorney-fee recovery, administered by elected boards and contracted
 *   management firms. Under the extraction reading instantiated here, the
 *   apparatus's operative function is revenue generation and board power
 *   consolidation: fine proliferation widens the sanctionable surface,
 *   selective enforcement concentrates on violations carrying the largest
 *   fees and on households least able to contest, expedited lien statutes
 *   convert unpaid fines into foreclosure leverage, and fee-shifting converts
 *   disputes into counsel revenue. The epsilon referent is the standing
 *   arrangement under contest — the existing covenant-plus-fine-plus-lien
 *   operation as this reading assesses it — never a reformed alternative. Per
 *   the committer frame, this file authors ONLY the extraction reading; the
 *   coordination and behavioral-control readings are separate constraint
 *   files linked through the network section. Assumptions stated: the
 *   interval models roughly 1985-2025 US common-interest community
 *   maturation; provenance commit identifiers are session-local placeholders;
 *   the scenario was sourced directly from the task brief. KEY AGENTS (by
 *   structural relationship): - board_members: agenda-setting seat
 *   (organized/constrained) — sets fine schedules, selects targets, initiates
 *   liens; also bears the dues regime it administers -
 *   property_management_firms: primary beneficiary (institutional/arbitrage)
 *   — collects management and per-violation processing fees; drafts
 *   enforcement agendas - legal_counsel: secondary beneficiary
 *   (institutional/arbitrage) — recovers statutory attorney fees from
 *   contested violations - compliant_homeowners: incidental beneficiaries
 *   (moderate/constrained) — receive maintained commons while funding the
 *   apparatus through dues - financially_vulnerable_homeowners: primary
 *   targets (powerless/trapped) — absorb fines, interest, lien costs; least
 *   able to contest - renters_via_pass_through: indirect targets and excluded
 *   voice (powerless/constrained) — bear passed-through costs without voting
 *   rights - state_legislatures: analytical observer
 *   (institutional/analytical) — weighs reform of fine caps, lien priority,
 *   and foreclosure thresholds
 *
 * KEY AGENTS:
 *   - board_members: agenda-setting seat (organized/constrained) — sets fine schedules, selects enforcement targets, initiates liens; also bears the dues regime it administers
 *   - property_management_firms: primary beneficiary (institutional/arbitrage) — collects management and per-violation processing fees; drafts enforcement agendas
 *   - legal_counsel: secondary beneficiary (institutional/arbitrage) — recovers statutory attorney fees from contested violations
 *   - compliant_homeowners: incidental beneficiaries (moderate/constrained) — receive maintained commons while funding the apparatus through dues
 *   - financially_vulnerable_homeowners: primary targets (powerless/trapped) — absorb fines, interest, lien costs; least able to contest
 *   - renters_via_pass_through: indirect targets and excluded voice (powerless/constrained) — bear passed-through costs without voting rights
 *   - state_legislatures: analytical observer (institutional/analytical) — weighs reform of fine caps, lien priority, and foreclosure thresholds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.66).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.62).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Fine-and-Lien Apparatus — Extraction Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'ea144051-9430-408d-81f0-d458bc4cbd6a').
narrative_ontology:cs_kernel_codification('ea144051-9430-408d-81f0-d458bc4cbd6a', formalized).
narrative_ontology:cs_authority_grounding('ea144051-9430-408d-81f0-d458bc4cbd6a', extraction).
narrative_ontology:cs_interpretation_layer_present('ea144051-9430-408d-81f0-d458bc4cbd6a').
narrative_ontology:cs_reading_relation('ea144051-9430-408d-81f0-d458bc4cbd6a', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea144051-9430-408d-81f0-d458bc4cbd6a', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('ea144051-9430-408d-81f0-d458bc4cbd6a', foundational, fine_apparatus_is_revenue_mechanism).
narrative_ontology:cs_axiom_status(fine_apparatus_is_revenue_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ea144051-9430-408d-81f0-d458bc4cbd6a', fine_apparatus_is_revenue_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('ea144051-9430-408d-81f0-d458bc4cbd6a', secondary, selective_enforcement_consolidates_board_power).
narrative_ontology:cs_axiom_status(selective_enforcement_consolidates_board_power, holdable).
narrative_ontology:cs_axiom_grounding('ea144051-9430-408d-81f0-d458bc4cbd6a', selective_enforcement_consolidates_board_power, empirically_contingent).
narrative_ontology:cs_reference_frame('ea144051-9430-408d-81f0-d458bc4cbd6a', developer_installed_revenue_instrument).
narrative_ontology:cs_drift_state('ea144051-9430-408d-81f0-d458bc4cbd6a', contemporary_mature_hoa_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ea144051-9430-408d-81f0-d458bc4cbd6a', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, compliant_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, compliant_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer owners elected to set the annual budget, adopt fine schedules, decide which violations to pursue, and initiate lien proceedings. They contract the management firm, approve attorney engagements, and routinely waive or reduce penalties for allies while pressing collection against others. They also pay the same dues and live under the same recorded restrictions they enforce; stepping down or selling is possible, but their standing in the community is tied to the office.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, board_members, payer).

% Contracted companies running day-to-day operations: sending violation notices, levying late charges, forwarding files to counsel, and collecting a percentage of recoveries plus per-letter and per-hearing fees. Revenue scales with enforcement volume, which gives a standing incentive to widen the sanctionable surface. They serve many communities at once and can decline or drop unprofitable contracts.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, property_management_firms, agenda_setter).

% Law firms on retainer to associations, paid from association accounts and, under fee-shifting provisions, awarded costs against owners who contest and lose. Demand letters and suits are a revenue line; the volume of contested violations tracks billable work.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    institutional, biographical, arbitrage, national).

% Owners who pay assessments on time and keep their lots within the recorded rules. They receive maintained roads, commons, and a uniformity many value, funded by the dues everyone pays. Practical alternatives are limited: the declaration runs with the deed, and amending it requires supermajority participation that annual meetings rarely reach.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, compliant_homeowners, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, compliant_homeowners, payer).

% Owners living close to the margin, for whom a few hundred dollars in fines plus interest, collection costs, and attorney fees compounds quickly into a lien that clouds title and, in states granting associations priority, can end in foreclosure. Contesting means a hearing before the same board that fined them, then court. Selling under a pending lien forfeits equity; staying means the balance grows.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, national).

% Tenants in covenant-covered homes. Landlords fold fines, assessments, and fee-driven cost increases into lease pricing, so tenants bear part of the burden while board elections are limited to owners. Moving is possible but costly in tight rental markets, and the rules bind wherever they rent within covered housing stock.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, renters_via_pass_through, excluded).

% Bodies weighing bills to cap fines, require pre-lien notices and payment plans, bar foreclosure for small balances, and regulate management contracting. They hold hearings, take testimony from owners, industry groups, and researchers, and can rewrite the statutory ground the enforcement machinery stands on.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains shared common elements (roads, roofs, pools, landscaping) and enforces recorded use restrictions across many individually owned parcels, solving free-rider and externality problems where municipal services stop at the lot line.
% TRANSFER_FUNCTION: Moves cash from fined and assessed households — disproportionately those with thin financial buffers — into association accounts administered by boards and management firms and into attorney-fee awards; moves discretionary power over neighbors' property and social standing into board hands.
% ABSENT_VOICES: Renters — bound by the rules, priced into the costs, and barred from the vote in most associations; delinquent owners, whose only formal audience is a hearing before the board that fined them; first-time buyers, who meet the fine schedule only after closing; and tenants of investor-owned units, who learn of violations only through their landlord.
% DISAPPEARANCE_RATIONALE: Overnight removal of the fine-and-lien machinery would force reorganization: common-element funding would shift to bare assessments or special levies, management contracts would be renegotiated around fixed fees, counsel retainers would lapse, and boards would lose the sanction lever that disciplines dissent — reopening municipal-service debates in many suburbs.
% FOUNDING_PROBLEM: Common-interest subdivisions needed a mechanism to fund and maintain shared infrastructure and enforce recorded restrictions where public provision ended at the parcel line.
% FOUNDING_PROBLEM_CORROBORATION: Original declaration preambles and developer disclosure packets attest the maintenance-and-restriction purpose. Outside the benefiting parties, state legislative findings, court opinions reviewing fine-to-foreclosure pipelines, and academic surveys of association finances corroborate that enforcement activity has grown disproportionate to upkeep needs and tracks revenue; industry testimony contests this and attests the upkeep problem remains live. No source outside the benefiting parties attests that revenue generation was the founding purpose — the revenue characterization rests on pattern evidence, not founding documents.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.66) because fine schedules are decoupled from harm, collection contracts pay firms a percentage of recoveries, and fee-shifting converts every contested violation into counsel revenue. Suppression (0.62) is a raw structural property, deliberately unscaled: the declaration runs with the title, membership is mandatory, several states grant association liens priority, and the foreclosure threat backs the entire collection chain. Theater (0.30) reflects hearing rituals and community-standards framing whose performative share has risen even though the underlying maintenance function is real. Accessibility collapse is moderate-low (0.48): selling, litigating, and amendment exist as alternatives but are costly, and amendment quorums are nearly unreachable in practice. Resistance (0.58) is substantial — litigation waves, state reform bills, and board-recall campaigns — which is what a defended construct, not a natural law, looks like. Claim and metrics are independent authored facts: I claim tangled_rope because I believe genuine maintenance coordination and asymmetric extraction ride the same structure; the engine computes per-seat types from the structural data, and any divergence between my claim and a computed seat type is the measurement, not an error. All three tracked metric series run on one shared time grid (t=0,8,16,24,32,40) so no metric row borrows another's end-state values. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity hardening: streamlined lien procedures, expanded self-help powers, and fee-statute amendments across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting/beneficiary seats should compute differently. From the board and management-firm positions the apparatus is administration they operate and income that scales with diligence; from the financially vulnerable owner's position it is a sanction pipeline ending in lien and possible foreclosure; from the renter's position it is costs levied without a vote; from the compliant owner's position it sits near symmetric — real upkeep received, real dues paid. The engine computes this divergence from power, exit, and directionality data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (board_members, property_management_firms, legal_counsel) derive low d — the structure subsidizes them; declared victims (financially_vulnerable_homeowners, renters_via_pass_through) derive high d. Trapped exit pushes vulnerable owners toward the full-target end; arbitrage exit keeps firms and counsel nearer the beneficiary end; renters sit slightly off full-target because physical mobility partially offsets fiscal immobility. Two overrides correct derivations the structural data alone would misread: compliant_homeowners (moderate) are declared beneficiaries, which would derive a strongly beneficiary-side d, but they fund the apparatus through the same dues-and-fine structure they benefit from — d 0.5, near symmetric. Board members (organized) as agenda-setters would derive a near-zero d, but they pay the same dues and live under the same rules; selective self-exemption keeps them net beneficiaries without making them pure ones — d 0.3.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding maintenance problem is partly live — commons do need funding — but the enforcement mandate has outgrown any proportional relation to it: fine proliferation now tracks revenue and control, not upkeep. Holding tangled_rope prevents mislabeling in both directions: a pure-extraction reading would erase the real coordination that legitimately justifies assessments, while a pure-coordination reading would erase the asymmetric extraction the fee architecture imposes on precisely the households least able to carry it. The genealogy interview carries the obsolescence verdict: founding_problem_status is contested, and the mismatch consumer — not the narrative — decides whether the arrangement persists as zombie mandate or live function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This file instantiates the extraction_reading of the hoa_covenant_scope kernel. Would instantiating coordination_reading or behavioral_control_reading instead change the structural classification?',
    'Author the sibling files against the same referent and compare computed per-seat classifications; the disagreement is located in the primary-function attribution of the fine/lien machinery.',
    'Under coordination_reading, epsilon falls toward the rope band and board power consolidation drops out of the beneficiary structure; under behavioral_control_reading, the victim set shifts toward aesthetic-rule violators and fines reframe as value-maximization instruments. This file''s high-epsilon tangled_rope profile holds only under the extraction attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: one of three readings of the hoa_covenant_scope kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    selective_enforcement_intent,
    'Is the targeting of high-fee violations and low-contest households a deliberate revenue strategy, or an artifact of administrative capacity and complaint-driven workflows?',
    'Discovery of board and management-firm correspondence, violation-ledger audits against fine schedules, and comparison of enforcement rates across comparable violations.',
    'Deliberate strategy supports the high end of epsilon and strengthens the revenue-mechanism attribution; capacity artifacts would lower attributable extraction and soften the reading toward workflow bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_intent, empirical, 'Whether selective enforcement reflects intent or workflow bias.').

omega_variable(
    renter_pass_through_incidence,
    'What share of fines, assessments, and fee-driven cost increases actually reaches tenants through rent pass-through, versus being absorbed by investor owners?',
    'Lease-level rent regressions against association fee changes in covered versus uncovered housing stock.',
    'Full pass-through confirms renters as weighted victims; partial absorption thins their weight and recenters the victim set on financially vulnerable owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renter_pass_through_incidence, empirical, 'Incidence of enforcement costs on tenants via landlord pricing.').

omega_variable(
    board_capture_direction,
    'Are boards principals consolidating their own power, or agents captured by management firms and counsel whose revenue scales with enforcement volume?',
    'Analysis of contract terms (percentage-of-collections clauses), board-election competitiveness data, and recall outcomes.',
    'Principal boards concentrate the receipt seat in the board itself; captured boards confirm management firms as the receipt seat and reframe boards as intermediaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(board_capture_direction, empirical, 'Which seat ultimately directs the enforcement agenda.').

omega_variable(
    lien_foreclosure_severity,
    'How often do association liens actually proceed to foreclosure rather than serving as collection leverage?',
    'Court records and association financial statements distinguishing liens filed, judgments obtained, and completed foreclosure sales.',
    'Frequent completed foreclosures raise the suppression estimate toward the top of its band and sharpen the target-end directionality of vulnerable owners; rare completions support a leverage-only reading with lower suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lien_foreclosure_severity, empirical, 'Severity of the lien endpoint in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__extraction_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__extraction_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(hoa__tr_t32, hoa_covenant_scope__extraction_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__extraction_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__extraction_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__extraction_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(hoa__be_t32, hoa_covenant_scope__extraction_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__extraction_reading, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__extraction_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__extraction_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__extraction_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(hoa__su_t32, hoa_covenant_scope__extraction_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__extraction_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'HOA covenants' conflates three structurally distinct claims with different epsilon values — coordination_reading (shared-maintenance coordination, low extraction), behavioral_control_reading (aesthetic conformity as value maximization, mid extraction), and extraction_reading (revenue and power consolidation, high extraction; this file). The upstream coordination claim is the one the other two cite as cover, so the family links run from the established claim toward the contested ones. Each member carries its own epsilon, beneficiary/victim structure, and classification; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, moderate, 0.5).
constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
