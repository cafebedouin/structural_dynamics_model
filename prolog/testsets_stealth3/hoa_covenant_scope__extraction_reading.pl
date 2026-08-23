% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Enforcement Regime — Extraction Reading (Revenue Generation and Board Power Consolidation)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   In common-interest communities, the recorded covenant regime — governing
 *   documents, adopted fine schedules, management contracts, and collection
 *   counsel — operates, on this reading, as a revenue-generation and
 *   board-power-consolidation mechanism riding on a genuine
 *   maintenance-coordination shell. Fine schedules proliferate faster than
 *   the rule set they price; enforcement concentrates on violations that
 *   generate fees and on owners least able to contest; balances compound
 *   through late fees and attorney-fee add-ons into liens that cloud title.
 *   This file instantiates ONLY the extraction_reading of kernel
 *   hoa_covenant_scope, per the one-reading-one-constraint discipline:
 *   epsilon's referent is the standing enforcement arrangement as this
 *   reading assesses it, the sibling readings are separate constraint files,
 *   and no averaging across readings occurs here. The claim/metric pair is
 *   independent: claimed_type tangled_rope is asserted from structure (real
 *   coordination function + asymmetric extraction + active enforcement),
 *   while the metrics describe observed operation — the engine computes
 *   per-seat classifications and owns any divergence.
 *
 * KEY AGENTS:
 *   - financially_vulnerable_homeowners: Primary target (powerless/trapped) — bears fines, compounding late fees, liens, and fee-shifted counsel costs
 *   - renters_via_pass_through: Secondary target (powerless/mobile) — absorbs pass-through costs and eviction risk with no vote or notice
 *   - board_members: Primary beneficiary and agenda-setter (organized/constrained) — consolidates discretion through selective enforcement
 *   - property_management_firms: Primary monetary beneficiary (institutional/arbitrage) — collections-linked and per-action fee streams; de facto administrator
 *   - legal_counsel: Beneficiary (institutional/arbitrage) — demand-letter, lien, and fee-shifting revenue
 *   - compliant_homeowners: Coordination-shell beneficiary (moderate/constrained) — funds the budget, uses the amenities, rarely fined
 *   - delinquent_homeowners: Excluded voice (powerless/trapped) — disenfranchised by the machinery billing them
 *   - state_legislatures: Analytical observer (institutional/analytical) — intermittent statutory caps and disclosure mandates
 *   - developer_declarants: Structural author (institutional/arbitrage) — wrote the covenant architecture during declarant control, then exited
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.66).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.62).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Enforcement Regime — Extraction Reading (Revenue Generation and Board Power Consolidation)").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'f16070c2-a25f-4953-945e-2bc72d39a796').
narrative_ontology:cs_kernel_codification('f16070c2-a25f-4953-945e-2bc72d39a796', formalized).
narrative_ontology:cs_authority_grounding('f16070c2-a25f-4953-945e-2bc72d39a796', extraction).
narrative_ontology:cs_interpretation_layer_present('f16070c2-a25f-4953-945e-2bc72d39a796').
narrative_ontology:cs_reading_relation('f16070c2-a25f-4953-945e-2bc72d39a796', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f16070c2-a25f-4953-945e-2bc72d39a796', hoa_covenant_scope__behavioral_control_reading, influences).
narrative_ontology:cs_axiom('f16070c2-a25f-4953-945e-2bc72d39a796', foundational, persistence_explained_by_gain_concentration).
narrative_ontology:cs_axiom_status(persistence_explained_by_gain_concentration, holdable).
narrative_ontology:cs_axiom_grounding('f16070c2-a25f-4953-945e-2bc72d39a796', persistence_explained_by_gain_concentration, empirically_contingent).
narrative_ontology:cs_axiom('f16070c2-a25f-4953-945e-2bc72d39a796', foundational, selective_enforcement_is_discipline_technology).
narrative_ontology:cs_axiom_status(selective_enforcement_is_discipline_technology, holdable).
narrative_ontology:cs_axiom_grounding('f16070c2-a25f-4953-945e-2bc72d39a796', selective_enforcement_is_discipline_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('f16070c2-a25f-4953-945e-2bc72d39a796', member_fiduciary_maintenance_compact).
narrative_ontology:cs_drift_state('f16070c2-a25f-4953-945e-2bc72d39a796', contemporary_professionalized_management_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f16070c2-a25f-4953-945e-2bc72d39a796', '').
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
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, developer_declarants).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, compliant_homeowners).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__extraction_reading, public_choice_capture_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unpaid volunteer directors elected from among the owners. They adopt the annual budget, write and amend the fine schedule, select the management company and association counsel, and decide which violations receive notices and which receive warnings. Service confers standing in the community, informal latitude on rules touching their own lots, and leverage over neighbors; some accept fee waivers or priority on common-area services. They live under the same documents they administer and can leave only by selling.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, constrained, local).

% Contracted to run day-to-day operations across a portfolio of associations: issuing violation notices, processing fines, tracking delinquencies, and preparing lien referrals. Compensation mixes flat management fees with per-action charges (per notice, per hearing, per resale certificate) and, in some contracts, amounts tied to collections volume. They routinely draft the fine schedules boards adopt and recommend which accounts to escalate. Losing one account costs a line item; the portfolio spreads the risk, and national trade associations lobby statehouses on the industry's behalf.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, property_management_firms, agenda_setter).

% Association attorneys paid from association funds and, where the documents allow, from amounts added to delinquent owners' balances: demand letters, lien recordings, foreclosure filings, and hearings are billed hourly or flat-rate and shifted onto the owner's account. A single firm commonly serves dozens of associations; losing one client barely registers.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    institutional, biographical, arbitrage, regional).

% Owners on fixed or hourly incomes. Violations cluster where deferred maintenance is visible — a faded fence, an unwatered lawn — and contesting a notice costs time, postage, and paperwork many cannot spare. Late fees compound on top of fines; unresolved balances become liens that cloud title, block refinancing or sale, and in some states mature into foreclosure over sums smaller than a month's rent. Leaving means clearing the balance first.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Occupy homes their landlords own inside the association. They cast no vote, attend no meetings, and receive no notices; they experience the regime as rent adjustments after assessment increases, lease non-renewals after repeated owner-side violations, and rules they learn about only by breaching them. Moving at lease end is their principal recourse.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, mobile, local).

% The majority of owners: they pay assessments on time, keep their lots within guidelines, and use the pool, clubhouse, and maintained streets. Their payments fund the budget that compensates the management company and counsel; they rarely receive notices themselves and mostly encounter enforcement as the reason the neighborhood looks kept up. Their alternatives are selling, or organizing to replace the board through elections that frequently fail to reach quorum.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, compliant_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, compliant_homeowners, payer).

% Owners behind on assessments or fines. Governing documents and many state statutes suspend their voting rights and meeting access once an account is delinquent, so the people with the most direct grievance are formally silenced inside the process that is billing them. Some owe small original sums that have multiplied through added fees.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, delinquent_homeowners, excluded,
    powerless, immediate, trapped, local).

% State bodies that periodically hear owner testimony, commission studies, and enact reforms — fine caps, notice-and-cure mandates, foreclosure restrictions, records access. Several states have acted; more have considered it; industry associations lobby throughout. Their remedies alter the enforcement environment but do not run the associations.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_legislatures, observer,
    institutional, generational, analytical, national).

% The builders who recorded the original covenants, drafted the initial rules and assessment structures, and appointed the first boards before transition to owner control. They designed the documents every later actor inherits, then exited into new developments.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, developer_declarants, beneficiary,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools dues to fund and schedule maintenance of shared assets — roads, roofs, pools, common landscaping — that individual owners cannot maintain alone, and provides a forum for resolving boundary, noise, and appearance disputes among adjacent owners.
% TRANSFER_FUNCTION: Moves money from owners — assessments, fines, late fees, interest, attorney-fee add-ons, lien and foreclosure costs — to association coffers, management firms, and counsel; and moves discretion to whoever administers enforcement, since choosing which violations to pursue concentrates power over neighbors.
% ABSENT_VOICES: Delinquent owners (disenfranchised by the very machinery billing them), renters (no vote, no notice), future buyers (bound by documents they never negotiated), and neighboring non-members affected by exterior rules. None sits in the room where fine schedules are adopted.
% DISAPPEARANCE_RATIONALE: Shared assets would lose their funding vehicle overnight — municipalities rarely readmit road or storm-water obligations quickly — and the management-and-collections industry built on association contracts would lose its revenue base. Owners would reorganize around voluntary agreements, municipal special districts, or newly drafted covenants; the fine, lien, and fee-shifting apparatus would not survive that transition in its current form.
% FOUNDING_PROBLEM: Developers needed a durable vehicle to maintain common areas and enforce standards across thousands of lots before and after sellout, and municipalities welcomed offloading infrastructure and amenity costs onto private associations; buyers accepted the recorded documents in exchange for maintained amenities and protected appearance.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning records and the land-use scholarship on common-interest development corroborate the original maintenance-and-offload problem as real. Homeowner-rights organizations and state study commissions attest that the maintenance function persists while the enforcement-and-collections apparatus has grown well beyond it. No source outside the benefiting parties attests that fine-schedule proliferation, fee-shifting counsel, or delinquency disenfranchisement was ever necessary to the founding problem.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.66 sits in the expected band for this reading: fine schedules decouple from administrative cost, attorney-fee shifting converts enforcement into a billable product, and per-action management compensation rewards violation volume. Suppression 0.62 is predominantly structural — liens cloud title, delinquency suspends voting rights, litigation exposure chills contest, and sale requires clearing the balance — with a minority internalized component (owners treating fines as normal and deserved) handled by the suppression_mechanism_mix omega; suppression is authored as a raw structural property and is NOT scaled by power or scope, unlike extractiveness. Theater 0.48: elections that fail to reach quorum, pro forma annual meetings, and community-harmony framing increasingly decorate a collections operation. Accessibility_collapse 0.5: alternatives remain partly open — self-management, reform slates, statutory caps in several states, selling out at a price — so the constraint does not present as natural law. Resistance 0.58: owner lawsuits, recall attempts, state reform waves, and investigative press. The three temporal series share one grid (t=0..30 step 6) so every metric is authored at every examined point; trajectories rise monotonically with the professionalization of association management, with no cyclical oscillation asserted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute differently. From the board and management seats the regime is a functioning service arrangement they staff and defend; from the financially_vulnerable and delinquent seats the same structure operates as compounding debt enforced against housing. Compliant homeowners sit near symmetric — real amenity value received, assessments paid, enforcement experienced as background order. Coalition potential among the powerless seats exists on paper (vulnerable owners, delinquent owners, renters share grievances) but is blocked by the regime's own mechanics: delinquency strips voting rights, renters have no franchise at all, and reform slates die at quorum. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for board_members (agenda-setting plus status and latitude gains), property_management_firms (collections-linked compensation, arbitrage-grade portfolio exit), and legal_counsel (fee-shifting revenue, many-client diversification). Victim declarations drive high directionality for financially_vulnerable_homeowners (trapped exit amplifies toward the full-target end) and renters_via_pass_through (full-cost bearing but mobile exit dampens slightly). Compliant_homeowners derive near-symmetric positioning from their dual beneficiary/payer roles. Delinquent_homeowners carry the highest structural target position of any seat — trapped, powerless, and billed — though their exclusion limits their formal surface. No directionality overrides are authored: the beneficiary/victim data plus exit options suffice for the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the regime as pure coordination (rope) would erase the identifiable payers — the compounding-balance owners and disenfranchised delinquents — and launder fee-shifting as overhead. Reading it as pure extraction (snare) would erase the genuine maintenance function that compliant homeowners visibly fund and consume, and would predict collapse that does not occur when enforcement is temporarily restrained. The founding problem (maintenance pooling) remains partially live, so the arrangement is not yet a piton; but the rising theater_ratio series marks the drift path — if maintenance were municipalized or self-managed successfully and the fine apparatus persisted, the residual would be theatrical maintenance of an extraction mandate, and the mismatch consumer (founding_problem_status x disappearance_verdict) is positioned to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the extraction_reading of kernel hoa_covenant_scope; would instantiating the coordination_reading or behavioral_control_reading instead yield a materially different epsilon and type for the same documentary kernel?',
    'Classify the sibling stories and compare: convergence on tangled_rope with differing victim sets confirms multi-force persistence; divergence to rope under the coordination reading or toward snare under the behavioral reading relocates the explanatory weight.',
    'Under the coordination reading, epsilon falls toward the coordination-cost floor and the victim set thins to non-payers of genuine cost recovery; under the behavioral reading, victims shift toward aesthetic-rule violators and suppression rises as conformity pressure. Classification of this file alone cannot settle which reading governs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the covenant kernel explains the regime''s persistence.').

omega_variable(
    selective_enforcement_intent,
    'Is selective enforcement a deliberate revenue strategy (violations targeted because they generate fees) or capacity-driven triage (boards pursue what is visible and affordable to pursue)?',
    'Compare fine yield per violation category against violation prevalence; audit board minutes for targeting decisions; use states that banned fines on specific categories as natural experiments — if revenue shifts to remaining categories rather than falling, targeting is strategic.',
    'Deliberate targeting pushes the arrangement toward snare; capacity triage keeps it tangled_rope with extraction as accumulated drift rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_intent, empirical, 'Whether enforcement concentration is strategic rent-seeking or administrative triage.').

omega_variable(
    pass_through_incidence,
    'Do landlord-owners actually shift fines and assessment increases into rents, and at what rate?',
    'Panel comparison of rents in association-covered versus comparable non-association housing, controlling for amenities and location; lease-clause surveys for explicit pass-through terms.',
    'Near-zero incidence removes renters_via_pass_through as a victim seat and narrows the payer base; full incidence roughly doubles the population bearing the regime''s costs and strengthens the case for statutory response.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pass_through_incidence, empirical, 'Whether the renter victim seat bears real incidence or only nominal exposure.').

omega_variable(
    cost_recovery_benchmark,
    'What fraction of fine, late-fee, interest, and attorney-fee charges reflects the administrative cost of enforcement versus margin?',
    'Cost accounting of the notice-hearing-lien pipeline; benchmark against municipal code-enforcement fee schedules performing analogous functions.',
    'High margin confirms the rent-seeking core of this reading; cost-level charges would reclassify much of the measured extraction as coordination overhead and pull epsilon down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_recovery_benchmark, empirical, 'How much of the charged amounts is margin rather than cost recovery.').

omega_variable(
    suppression_mechanism_mix,
    'Is the measured suppression structural (liens, disenfranchisement, litigation exposure, sale-blocking balances) or internalized (owner belief that fines are normal, deserved, and unavoidable)?',
    'Post-exit trajectory: compare owners who sold out of associations and shed compliance behavior against those who retained it; survey perceived legitimacy of fine regimes among recent arrivals versus long-term residents.',
    'An internalized share raises effective suppression above the structural measure and slows reform uptake even after statutory caps; purely structural suppression would fall quickly if statutes capped fines and restored delinquent voting rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mix, empirical, 'Structural versus internalized composition of the regime''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hoa__tr_t6, hoa_covenant_scope__extraction_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__extraction_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(hoa__tr_t18, hoa_covenant_scope__extraction_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__extraction_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hoa__be_t6, hoa_covenant_scope__extraction_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__extraction_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(hoa__be_t18, hoa_covenant_scope__extraction_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__extraction_reading, base_extractiveness, 30, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hoa__su_t6, hoa_covenant_scope__extraction_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__extraction_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(hoa__su_t18, hoa_covenant_scope__extraction_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__extraction_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__extraction_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'HOA covenants' decomposes, per the epsilon-invariance principle, into at least three structurally distinct claims about the same recorded documents: a maintenance-coordination device (low epsilon, rope-shaped), an aesthetic-conformity instrument (medium epsilon, enforcement-dependent), and a revenue-and-power-consolidation mechanism (high epsilon — this file). Each reading carries its own epsilon, beneficiaries, victims, and classification; forcing one story to span observables that yield different epsilon values would violate epsilon-invariance. The family is linked so that reform proposals (fine caps, foreclosure restrictions) and contamination (management-industry lobbying, fee-shifting norms) propagate across readings. Upstream/downstream: the coordination reading supplies the legitimating shell the extraction reading rides on; the behavioral reading supplies the rule surface the extraction machinery monetizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
