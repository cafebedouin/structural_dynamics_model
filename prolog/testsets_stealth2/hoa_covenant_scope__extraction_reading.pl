% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: HOA Covenant Enforcement Regime - Extraction Reading (Revenue Generation and Board Power Consolidation)
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   A common-interest community's recorded declaration empowers a volunteer
 *   board, backed by a management firm and association counsel, to cite
 *   violations, levy fines, stack late fees and attorney charges, and
 *   accelerate unpaid balances into liens and nonjudicial foreclosure. This
 *   story instantiates the extraction reading of the hoa_covenant_scope
 *   kernel: on this reading the enforcement apparatus operates as a revenue
 *   stream and a discretion-consolidating instrument - fine schedules
 *   proliferate because they bill, enforcement selects for owners who cannot
 *   contest, and the management-and-counsel complex bills per item so income
 *   scales with citation volume - while a genuine maintenance-coordination
 *   substrate continues underneath, which is why the structure is claimed as
 *   tangled_rope rather than snare. Per the one-reading rule, this file
 *   authors epsilon (0.66) only for the standing enforcement arrangement as
 *   the extraction reading sees it; the coordination and behavioral-control
 *   readings are separate linked stories with their own epsilon values, and
 *   nothing here hedges or averages across them. The claim and the metrics
 *   are independent authored facts: the type claim records the coexistence of
 *   coordination and extraction; the metrics record how the arrangement
 *   actually operates.
 *
 * KEY AGENTS:
 *   - - hoa_board_members: agenda-setting seat (organized / identity_locked) - administers the fine schedule, selects enforcement targets, and consolidates discretion; collects power and exemption rather than cash
 *   - - property_management_firms: primary monetary beneficiary (organized / mobile) - per-item enforcement billing makes revenue scale with citation volume
 *   - - legal_counsel: secondary monetary beneficiary (organized / mobile) - collects demand-letter, lien, and foreclosure fees charged back to cited owners
 *   - - financially_vulnerable_homeowners: primary target seat (powerless / trapped) - bears the fine-fee-lien cascade with no contest capacity
 *   - - renters_via_pass_through: indirect target seat (powerless / constrained) - absorbs costs through rent increases and arrears-driven displacement
 *   - - compliant_dues_paying_homeowners: near-symmetric seat (moderate / constrained) - funds genuine maintenance while remaining one citation away from the cascade
 *   - - well_resourced_homeowners: contesting payer seat (powerful / arbitrage) - wins against identical violations, exposing the targeting skew
 *   - - mortgage_lenders: excluded tail-risk bearer (institutional / arbitrage) - primed by super-priority assessment liens in some states, absent from governance
 *   - - state_legislators_regulators: analytical observer (institutional / analytical) - the seat from which the full structure is visible and partially correctable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.66).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.64).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Enforcement Regime - Extraction Reading (Revenue Generation and Board Power Consolidation)").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '4e41984f-3ed7-4a31-b66f-46e3b7efde14').
narrative_ontology:cs_kernel_codification('4e41984f-3ed7-4a31-b66f-46e3b7efde14', fixed_text).
narrative_ontology:cs_authority_grounding('4e41984f-3ed7-4a31-b66f-46e3b7efde14', extraction).
narrative_ontology:cs_interpretation_layer_present('4e41984f-3ed7-4a31-b66f-46e3b7efde14').
narrative_ontology:cs_reading_relation('4e41984f-3ed7-4a31-b66f-46e3b7efde14', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('4e41984f-3ed7-4a31-b66f-46e3b7efde14', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('4e41984f-3ed7-4a31-b66f-46e3b7efde14', foundational, enforcement_operates_as_revenue_mechanism).
narrative_ontology:cs_axiom_status(enforcement_operates_as_revenue_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4e41984f-3ed7-4a31-b66f-46e3b7efde14', enforcement_operates_as_revenue_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('4e41984f-3ed7-4a31-b66f-46e3b7efde14', foundational, selective_enforcement_serves_power_consolidation).
narrative_ontology:cs_axiom_status(selective_enforcement_serves_power_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('4e41984f-3ed7-4a31-b66f-46e3b7efde14', selective_enforcement_serves_power_consolidation, empirically_contingent).
narrative_ontology:cs_reference_frame('4e41984f-3ed7-4a31-b66f-46e3b7efde14', revenue_and_discretion_apparatus).
narrative_ontology:cs_drift_state('4e41984f-3ed7-4a31-b66f-46e3b7efde14', contemporary_enforcement_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4e41984f-3ed7-4a31-b66f-46e3b7efde14', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, compliant_dues_paying_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, compliant_dues_paying_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, well_resourced_homeowners).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__extraction_reading, discretionary_enforcement_privilege).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__extraction_reading, fine_substituted_assessment_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected volunteer owners who adopt the fine schedule, direct the management firm's enforcement docket, and decide which violations to pursue, settle, or waive. They own homes inside the association and are subject to the same declaration, but they control citation priorities and routinely excuse their own properties or those of allies. Long-tenured members describe the community's appearance and solvency as their personal project; stepping off the board returns them to ordinary owner standing but feels, to most, like abandoning the place they built.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    organized, biographical, identity_locked, local).

% For-profit firms contracted to run daily operations across portfolios of associations. They invoice monthly management fees plus per-item charges for violation notices, hearing scheduling, and lien-referral packages, so their revenue rises with enforcement volume. They prepare the board's violation reports, select which matters go to counsel, and can decline renewal on associations that resist their recommendations.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, property_management_firms, agenda_setter).

% Law firms specializing in association representation. They send demand letters, file assessment liens, prosecute foreclosures, and defend the association in owner suits; their fees are billed to the cited owner's account, compounding the original debt. Contingency arrangements and volume referral relationships with management firms make enforcement work a dependable practice line.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    organized, biographical, mobile, regional).

% Owner-occupants financing their homes near the margin of affordability. A routine citation becomes a four-figure balance once late fees, interest, and attorney charges attach; a recorded lien clouds title and blocks refinancing or sale; payment plans arrive conditioned on broad waivers. Selling under duress is the only reliable exit, and it usually realizes a loss.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Tenants leasing units inside the association. They never signed the declaration and cannot vote in elections, but they absorb the regime's costs: landlords raise rent after fines or special assessments, and a tenant can be displaced when the landlord's account falls into arrears and the association pursues collection against the unit.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Owners who pay assessments on time and keep their lots within standards. They receive maintained amenities, insured common areas, and enforced neighborhood standards, and their dues fund the enforcement budget that fines nominally offset. A single missed payment or unpermitted improvement moves them into the cited population, so their protected status is conditional on continued conformity.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, compliant_dues_paying_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, compliant_dues_paying_homeowners, payer).

% Owners with legal training, spare time, or savings. When cited, they request hearings, demand records, retain counsel, and frequently win dismissal or reduced amounts; some run for the board or sue the association. Their success rate against identical violations is the visible marker that outcomes track the owner, not the violation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, well_resourced_homeowners, payer,
    powerful, biographical, arbitrage, local).

% Institutions holding purchase-money liens on association units. In states permitting super-priority assessment liens, an association foreclosure can extinguish their security interest; they respond with resale-certificate requirements, lending overlays in high-HOA markets, and trade-group lobbying for statutory limits, while having no seat in the governance that generates the exposure.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, mortgage_lenders, excluded,
    institutional, generational, arbitrage, national).

% Legislative committees and state ombudsman offices receiving constituent complaints about lien cascades, fee stacking, and nonjudicial foreclosure. They commission studies, cap fine amounts, mandate uniform enforcement policies and open-record access, and restrict self-help remedies; their data arrives through complaint portals and audit reports rather than association participation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_legislators_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates maintenance and funding of shared infrastructure (roads, roofs, common areas, reserves) and sets exterior standards across hundreds of interdependent households that cannot feasibly contract bilaterally; provides a forum for neighbor-dispute resolution.
% TRANSFER_FUNCTION: Moves fine revenue, late fees, interest, attorney fees, and lien-related charges from cited and delinquent owners into association accounts that pay management-firm invoices and counsel bills; concentrates enforcement discretion over who gets cited, when, and for what, in board and management-firm hands.
% ABSENT_VOICES: Renters living under association rules without a vote; future purchasers bound by declarations they never negotiated; delinquent owners unable to attend scheduled hearings; tenants facing displacement over a landlord's account arrears. They sit outside the annual-meeting and board-election franchise that legitimates enforcement.
% DISAPPEARANCE_RATIONALE: Common-area funding would reorganize around voluntary agreements or municipal services; boards would lose the discretionary lever that disciplines dissent; management firms would lose the enforcement-billing revenue line; counsel would lose the violation-letter practice; vulnerable owners would stop facing lien cascades - the sector's governance economy visibly reorganizes.
% FOUNDING_PROBLEM: Developer-built common-interest communities needed a way to maintain shared infrastructure and protect sales values after build-out, when no single owner could maintain roads, roofs, and amenities alone and externalities between lots were unavoidable.
% FOUNDING_PROBLEM_CORROBORATION: State HOA task-force reports and academic property-law scholarship attest both halves from outside the benefiting parties: the underlying maintenance-coordination problem remains real, while enforcement practice has drifted toward revenue and discretion; published court records in lien-contestation and selective-enforcement suits supply independent evidence of the drift. No attesting source sits inside the board-management-counsel beneficiary set.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.66 for the standing enforcement arrangement as this reading sees it: fine schedules set above documented cost, mandatory attorney fees multiplying each citation, expedited nonjudicial lien and foreclosure paths, and management contracts whose per-item billing rewards citation volume. Suppression is authored at 0.64 as a raw structural property (it is not scaled by power or scope; only extractiveness is): the declaration runs with the land, so exit means selling the home, and the enforcement pipeline of automated liens, collection referrals, and nonjudicial sale operates on owners regardless of consent. Theater sits at 0.42: violation-letter counts and hearing calendars function as performance metrics on board and management dashboards, yet real maintenance and reserve funding continue underneath. Accessibility collapse is 0.35 - exits exist (sale, litigation, recall elections, state statute) but are costly, slow, and unevenly distributed, which is why this is neither a natural law nor a fully closed trap. Resistance is 0.55: a sustained litigation wave, recall attempts, and successive state reform statutes meet the machinery. The claimed type, tangled_rope, is stated independently of these metrics: a genuine coordination substrate (compulsory funding of roofs, roads, and reserves that no household can provision alone) demonstrably persists alongside the asymmetric fine-and-fee layer, and both must be named for the structure to be described honestly. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30) with every tracked metric authored at every point; all three trajectories rise together, modeling enforcement-capacity maturation (suppression_requirement) alongside accumulating rent (base_extractiveness) and growing proxy management (theater_ratio). On the receipt surface, gain_flow names property_management_firms: their contractual per-item billing makes them the recurring, systematic recipient of the enforcement stream, while counsel captures episodically per matter and boards consolidate discretion rather than cash. fixing_cost is authored cheap because uniform-enforcement policies, published capped fine schedules, and independent appeal channels already exist as off-the-shelf models in reform jurisdictions - adoption is administrative, and persistence reflects incumbent interest rather than technical difficulty.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from identical structural data. From the board seat the machinery is experienced as stewardship - the same instruments that cascade on a delinquent owner read, from the agenda-setting chair, as tools keeping the community solvent and orderly. From the management-firm and counsel seats the arrangement presents as ordinary business: invoiced services, renewing contracts, no personal exposure. From the financially vulnerable owner seat the same structure presents as an inescapable debt pump with no exit short of selling the home. The well-resourced owner seat computes a negotiable nuisance - arbitrage-grade exit and winning hearings dampen effective burden far below what the nominal schedule implies, and their experience is the pivot that exposes selective enforcement to observers. Compliant owners sit nearest symmetric: they consume the coordination goods and carry conditional exposure. The engine derives these divergences from role, power, and exit atoms; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. hoa_board_members derive near the beneficiary pole: they collect discretion and self-exemption rather than salary, and selective self-exemption holds their cost share near zero despite residing under the declaration. property_management_firms and legal_counsel derive as pure beneficiaries (d near 0.05-0.10): revenue inflow, no cost incidence. financially_vulnerable_homeowners derive near the full-target pole, amplified by trapped exit - the cascade lands on precisely the agents least able to route around it. renters_via_pass_through derive high as declared victims, moderated somewhat by lease-term mobility relative to owners. compliant_dues_paying_homeowners derive mid-range: genuine subsidy from the maintenance function, conditional exposure to the fine function. well_resourced_homeowners occupy a payer-declared position whose effective burden is pulled down by arbitrage exit; that differentiation is carried by their exit atom rather than a directionality override, and no overrides are used anywhere in this story because the declared roles plus exit options reproduce the structural relationships without correction. mortgage_lenders, seated as excluded, carry tail-risk exposure partially offset by covenant-supported collateral values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - provisioning shared infrastructure after developer exit - remains live: roofs, roads, pools, and reserves still require compulsory coordination, so this is not a mandate outliving its function and mandatrophy is not resolved. What has accreted is a second layer (fine proliferation, fee stacking, expedited liens) riding the live function. The tangled_rope claim is what prevents mislabeling in both directions: reading the regime as pure rope adopts the board-management self-description and hides the targeting skew and fee multiplication; reading it as pure snare erases the maintenance goods that vulnerable owners themselves consume and predicts a collapse the live function contradicts. The R5 interview records the founding problem as contested with corroboration from state task forces, academic scholarship, and court records - sources outside the board-management-counsel beneficiary set - so the genealogy is not self-asserted by the parties collecting from the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (extraction_reading) of the hoa_covenant_scope kernel; would instantiating coordination_reading or behavioral_control_reading over the same enforcement surface change the constraint''s epsilon, beneficiary structure, and type?',
    'Compare the three per-reading stories at the meta level: each authors its own epsilon over the fixed referent (the standing enforcement arrangement); divergence across readings is the corpus datum, not a defect to reconcile inside one file.',
    'Under coordination_reading the same surface would author low epsilon and likely a rope claim; under behavioral_control_reading intermediate epsilon keyed to conformity production; this file''s tangled_rope claim and 0.66 epsilon hold only for the extraction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-reading indexicality: classification is reading-relative; sibling readings are separate constraints.').

omega_variable(
    selective_enforcement_targeting_skew,
    'What share of enforcement actions (citations, fines, lien referrals) lands on owners lacking resources to contest, versus uniform application across violation types?',
    'Audit of association violation records cross-tabulated against contest rates and payment-plan usage; compare citation outcomes for identical violations across owner financial strata.',
    'Near-uniform application would weaken the extraction reading toward behavioral_control; strong skew toward non-contestable owners confirms the rent-seeking mechanism and pushes the computed type toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_targeting_skew, empirical, 'Targeting skew of selective enforcement.').

omega_variable(
    fine_schedule_cost_recovery_gap,
    'Do published fine schedules and attached fees track the association''s marginal enforcement cost, or systematically exceed it?',
    'Forensic separation of enforcement revenue from enforcement cost in association budgets across a sample of associations; benchmark against comparable municipal code-enforcement cost recovery.',
    'Cost-tracking fines support a coordination-cost framing (rope-side); persistent margins above cost establish rent and support this reading''s epsilon at the high end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fine_schedule_cost_recovery_gap, empirical, 'Whether fine levels are cost recovery or rent.').

omega_variable(
    pass_through_incidence_to_renters,
    'How much of the fine, fee, and special-assessment burden reaches renters through rent pass-through or displacement following landlord account arrears?',
    'Rent-series comparison in high-enforcement associations against matched low-enforcement controls; eviction-cause coding for association-arrears-driven terminations.',
    'Material pass-through would confirm renters_via_pass_through as a first-class victim seat and raise measured scope; negligible pass-through would shrink the victim set to owner-occupants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pass_through_incidence_to_renters, empirical, 'Incidence of enforcement costs on renters.').

omega_variable(
    owner_coalition_recall_potential,
    'Can powerless owners convert numbers into governing power - do recall elections, challenger slates, or class litigation actually displace extraction-oriented boards?',
    'Longitudinal study of board-recall attempts, challenger-slate success rates, and derivative-litigation outcomes in high-fine associations.',
    'Routine successful displacement would pull the computed type back toward accountable rope; blocked or failing recalls (quorum thresholds, management-firm control of proxies) would confirm suppressed-exit dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(owner_coalition_recall_potential, empirical, 'Coalition potential of the powerless owner majority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t6, hoa_covenant_scope__extraction_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(hoa__tr_t6, observed).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__extraction_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(hoa__tr_t12, observed).
narrative_ontology:measurement(hoa__tr_t18, hoa_covenant_scope__extraction_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement_basis(hoa__tr_t18, observed).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(hoa__tr_t24, observed).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__extraction_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(hoa__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t6, hoa_covenant_scope__extraction_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(hoa__be_t6, observed).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__extraction_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(hoa__be_t12, observed).
narrative_ontology:measurement(hoa__be_t18, hoa_covenant_scope__extraction_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(hoa__be_t18, observed).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(hoa__be_t24, observed).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__extraction_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(hoa__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t6, hoa_covenant_scope__extraction_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(hoa__su_t6, observed).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__extraction_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(hoa__su_t12, observed).
narrative_ontology:measurement(hoa__su_t18, hoa_covenant_scope__extraction_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement_basis(hoa__su_t18, observed).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__extraction_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(hoa__su_t24, observed).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__extraction_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(hoa__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'HOA covenants' decomposes, per the epsilon-invariance principle, into three structurally distinct claims sharing one kernel (hoa_covenant_scope): coordination_reading (upstream - the founding charter's documented purpose, highest empirical confidence), behavioral_control_reading (conformity production as value strategy), and this file, extraction_reading (epsilon 0.66, tangled_rope claim). Each member links the others. The upstream coordination claim is routinely cited as cover by the extraction layer - boards defend fine schedules in the language of maintenance funding - which is why the family edges run from the coordination story toward this one, and why this file's omega set includes the kernel-indexicality variable recording what each sibling would change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
