% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: EU Welfare Coordination Settlement: Free Movement via Coordinated National Welfare Systems with Anti-Social-Dumping Enforcement
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   The colloquial label 'EU free movement' covers three structurally
 *   distinct arrangements, and per the epsilon-invariance principle this file
 *   authors exactly one of them: the welfare-coordination reading, in which
 *   free movement runs through coordination of twenty-seven national welfare
 *   systems (portable contribution records, defined posting channels) rather
 *   than supranational harmonization, while EU-level rules police the
 *   cost-competition externalities ('social dumping') and every member state
 *   retains full welfare-design autonomy. The epsilon referent is the
 *   standing coordination arrangement itself, assessed by this reading's own
 *   lights — not the integrated-welfare arrangement the integration reading
 *   would build, and not the bounded-movement arrangement the sovereignty
 *   reading would build. Under that referent the arrangement carries a
 *   genuine, load-bearing coordination function (portability) and, through
 *   the same channels, asymmetric extraction: posting providers capture
 *   wage-and-levy differentials, posted workers contribute at home while
 *   forgoing host-system access, destination low-wage trades absorb wage
 *   pressure, third-country migrants are displaced from entry slots, and
 *   sending-state insurance pools bleed prime-age contributors without fiscal
 *   compensation. The sibling readings are separate constraints linked via
 *   network.affects_constraints; they are not averaged into this story.
 *
 * KEY AGENTS:
 *   - - eu_legislative_and_judicial_bodies: Agenda setter (institutional/constrained) — drafts, adjudicates, and defends the coordination settlement; invalidates national deviations in either direction
 *   - - national_labor_inspectorates: Secondary enforcer (institutional/constrained) — administer declarations and inspections with highly uneven capacity
 *   - - posting_service_providers: Primary beneficiary (powerful/arbitrage) — capture the wage-and-levy differential across corridors
 *   - - member_state_executives: Dual-positioned (institutional/constrained) — preserve welfare-design autonomy while destination cabinets absorb the externalities
 *   - - high_skill_mobile_professionals: Beneficiary (organized/mobile) — portability winners who bypass the posting cost structure
 *   - - posted_workers: Primary target (powerless/trapped) — contribute at home, exempt from host levies, employer-dependent throughout assignment
 *   - - host_state_low_wage_workers: Target (moderate/constrained) — absorb wage pressure in posted-worker-intensive trades under strike-limiting jurisprudence
 *   - - third_country_migrant_workers: Target (powerless/trapped) — visa-tied, displaced from entry-level slots, no intra-EU exit
 *   - - sending_state_social_insurance_funds: Target (institutional/trapped) — lose prime-age contributors permanently while receiving assignment-period contributions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.62).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination Settlement: Free Movement via Coordinated National Welfare Systems with Anti-Social-Dumping Enforcement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'bba9b043-050f-4067-aa2e-4e1186d0aa96').
narrative_ontology:cs_kernel_codification('bba9b043-050f-4067-aa2e-4e1186d0aa96', formalized).
narrative_ontology:cs_authority_grounding('bba9b043-050f-4067-aa2e-4e1186d0aa96', lineage).
narrative_ontology:cs_interpretation_layer_present('bba9b043-050f-4067-aa2e-4e1186d0aa96').
narrative_ontology:cs_reading_relation('bba9b043-050f-4067-aa2e-4e1186d0aa96', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('bba9b043-050f-4067-aa2e-4e1186d0aa96', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_axiom('bba9b043-050f-4067-aa2e-4e1186d0aa96', foundational, welfare_design_autonomy_under_common_rules).
narrative_ontology:cs_axiom_status(welfare_design_autonomy_under_common_rules, holdable).
narrative_ontology:cs_axiom_grounding('bba9b043-050f-4067-aa2e-4e1186d0aa96', welfare_design_autonomy_under_common_rules, conventional).
narrative_ontology:cs_axiom('bba9b043-050f-4067-aa2e-4e1186d0aa96', foundational, antidumping_parity_without_harmonization).
narrative_ontology:cs_axiom_status(antidumping_parity_without_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('bba9b043-050f-4067-aa2e-4e1186d0aa96', antidumping_parity_without_harmonization, instrumental).
narrative_ontology:cs_reference_frame('bba9b043-050f-4067-aa2e-4e1186d0aa96', coordination_preserving_welfare_autonomy).
narrative_ontology:cs_drift_state('bba9b043-050f-4067-aa2e-4e1186d0aa96', contemporary_post_2018_revision, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bba9b043-050f-4067-aa2e-4e1186d0aa96', '2026-08-03T09:14:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posting_service_providers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, high_skill_mobile_professionals).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, member_state_executives).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, third_country_migrant_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_social_insurance_funds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, member_state_executives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and adopts the social-security coordination regulation and the posted-worker directives; the Court adjudicates disputes and has invalidated national attempts to restrict posting or shield cross-border strike action. Defends the settlement in which every state keeps designing its own welfare system while common rules govern cross-border contribution records and service provision.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_legislative_and_judicial_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Process posting declarations, run joint cross-border inspection campaigns, and audit subcontracting chains for compliance with host pay rules. Inspection capacity differs widely across states; several rely mostly on document checks rather than site visits, and enforcement intensity tracks domestic political attention.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, national_labor_inspectorates, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, national_labor_inspectorates, observer).

% Supply labor across borders through temporary-agency chains and layered subcontractors. Save a large share of wage and social-levy cost relative to local hiring by routing workers through lower-cost home systems, with a multi-year window in which assigned workers are exempt from host-state contributions. Shift posting corridors quickly when one pair of states tightens rules.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posting_service_providers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Negotiate and ratify the directives; every government keeps full control of its welfare design and holds a veto over harmonization. Destination-state cabinets additionally absorb the domestic political fallout of visible posting crews and labor-market complaints, while all governments depend on the settlement for the functioning of the services market.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, member_state_executives, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, member_state_executives, payer).

% Build careers across member states with pensions, healthcare entitlements, and recognized qualifications that follow them under the coordination rules. They aggregate contribution periods across systems and rarely encounter the posting regime's cost side.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, high_skill_mobile_professionals, beneficiary,
    organized, biographical, mobile, continental).

% Hold contracts routed through agencies and stacked subcontractors. They contribute to their home country's insurance accounts while being exempt from host-state levies during assignment, typically cannot access host-state benefits, and depend on the employer for housing, transport, and continued assignment. Distance from home inspectors, language barriers, and fear of losing the assignment narrow complaint channels.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, trapped, continental).

% Work in construction, agriculture, logistics, meat processing, and care alongside posted crews engaged under different contribution and sometimes different pay arrangements. Wage growth in the affected trades lags the rest of the labor market, and union attempts at cross-border solidarity action have run into legal limits on strikes directed against service providers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_low_wage_workers, payer,
    moderate, biographical, constrained, national).

% Hold visas tied to specific employers and lack intra-EU mobility rights. They compete for the same entry-level slots in destination labor markets where posted crews can staff a site within weeks, and unlike EU mobile workers they cannot relocate between member states to escape a bad employer.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, third_country_migrant_workers, payer,
    powerless, biographical, trapped, national).

% Register steady outflows of prime-age contributors to permanent emigration while collecting assignment-abroad contributions for posted workers whose lifetime residence, consumption, and eventual old-age cost profile sit increasingly elsewhere. No compensatory fiscal transfer accompanies the population loss; the funds remain bound by the coordination regulation they cannot unilaterally revise.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_social_insurance_funds, payer,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, posting_service_providers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the portability problem: common rules let contribution periods, pension rights, and healthcare entitlements accumulated in one member state be counted in another, eliminating double contribution and coverage gaps for anyone whose working life crosses borders, and giving service firms a defined legal channel to deliver contracted work in another jurisdiction.
% TRANSFER_FUNCTION: Moves labor and service delivery across borders; moves contribution streams from destination-country employers into home-country insurance accounts for posted workers; moves wage-bill savings from destination labor markets into posting firms' margins; leaves the underlying welfare systems themselves untouched and nationally owned.
% ABSENT_VOICES: Posted workers held no seat in the directive negotiations — employer federations and governments spoke for labor-market effects. Destination-state sectoral unions' full day-one parity position was defeated in the 2018 revision and sits outside the operative settlement. Sending-state regions losing population have no institutional channel in the negotiation beyond their national capitals, which balance the emigration losses against export and remittance interests.
% DISAPPEARANCE_RATIONALE: Overnight removal would freeze tens of millions of accrued contribution records with no aggregation rule, collapse cross-border service contracting (construction seasons, installation and maintenance networks in neighboring states become unstaffable), dissolve the posting industry, and force a patchwork of bilateral treaties; wage structures in border trades, the staffing models of service firms, and migration flows within the union would all reorganize.
% FOUNDING_PROBLEM: Workers moving among early member states lost accrued social-security rights at each border: systems designed for sedentary populations produced double contribution and coverage gaps, blocking the labor mobility the common market required.
% FOUNDING_PROBLEM_CORROBORATION: The regulatory preambles (Regulation 1408/71, Regulation 883/2004) and successive Commission social-policy documents state the portability problem directly; comparative welfare-state scholarship and OECD social-protection analyses, sources outside the posting industry and independent of any single government, corroborate that twenty-seven distinct welfare designs still require coordination rules for cross-border lives to function.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   The claimed type is tangled_rope on structural grounds independent of the metrics: the arrangement possesses a genuine coordination function (portability across twenty-seven systems — remove it and cross-border working lives break) and simultaneously routes identifiable extraction through the same channels (posting cost-competition, uncompensated contributor outflows). The metrics are authored as descriptive facts: extractiveness 0.62 because the differential captured is large but concentrated in specific corridors and sectors rather than universal; suppression 0.70 because persistence depends on actively foreclosing alternatives — harmonization proposals die in unanimity, national protective deviations are struck down by the Court, and cross-border strike solidarity faces legal limits. Theater 0.30 reflects a mixed enforcement reality: the 1996 directive ran for nearly two decades largely on paper declarations, producing a theater hump peaking around the period of maximum posting growth, before the 2014 enforcement directive and 2018 revision funded joint inspections and extended equal treatment. Accessibility collapse is moderate (0.40): alternatives remain live — bilateral arrangements, renewed harmonization proposals, non-posting hiring, remote work — so understanding the constraint does not close the option space. Resistance 0.60 reflects two decades of litigation, union campaigns, and parliamentary battles. Suppression_requirement is tracked temporally because this story's narrative genuinely traces enforcement-capacity change: the machinery visibly hardened from paper-declaration administration to funded joint inspection after 2014/2018. All three series share one time grid (points 0, 6, 12, 18, 24, 30), and each series' endpoint equals the corresponding scalar in base_properties. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope inside the engine.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the posting provider's position the arrangement is an engineered opportunity: legal certainty plus a structural cost edge it did not create but captures. From the posted worker's position the same structure is confinement: a contract chain, a levy exemption that reads as exclusion from the host safety net, and dependence on the employer for lodging, transport, and continued income. From the destination-state executive's position it is a tolerable externality managed by periodic enforcement tightening. From the sending-state fund's position it is a slow hemorrhage dressed as contribution inflow. From the EU judicial seat it is a completed internal market operating as designed. None of these is authored as the truth; the engine computes per-seat classifications from power, exit options, and directionality, and the divergence between the provider seat and the posted-worker seat computed from this structural data is the measurement the story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for posting_service_providers (pure gainer, arbitrage-grade corridor mobility) and high_skill_mobile_professionals (gainer, fully mobile). Victim declarations drive high directionality for posted_workers (trapped, employer-dependent — near the full-target end), host_state_low_wage_workers (constrained, absorbing wage pressure), and third_country_migrant_workers (trapped, displaced). Two explicit overrides correct derivations the class-level structural data flattens: member_state_executives derive a strongly beneficiary-side d from their autonomy interest, but the class mixes destination cabinets bearing real enforcement and labor-market costs with sending cabinets gaining export relief — overridden to 0.30 to reflect the mixed relationship the homogeneous class label hides. sending_state_social_insurance_funds appear in the victim set, but assignment-period contribution inflows are a genuine partial subsidy — overridden downward to 0.65 so the arithmetic registers the offset rather than reading them as full targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — portability across distinct welfare designs — is still live: twenty-seven systems still require coordination for any cross-border working life, so the mandate is not exhausted and no mandatrophy resolution is declared. The tangled-rope classification earns its keep in both directions. Reading the arrangement as pure coordination would erase the posting victims the same channels produce and launder cost-competition as friction; reading it as pure extraction would erase the portability function without which mobile workers lose accrued rights at every border, and would misattribute to malice what is partly the unavoidable price of pluralism. Keeping both load-bearing explains the enforcement pattern too: the constraint must be actively defended (rising suppression_requirement) precisely because its extraction generates the resistance that threatens its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_stability,
    'Is the welfare-coordination settlement a stable terminal reading of the federation_membership_kernel, or a transitional configuration drifting toward the integration reading (supranational harmonization) or the sovereignty reading (national bounding)?',
    'Track the Court''s interpretation trajectory and the direction of successive directive revisions over the next decade: continued expansion of equal-treatment scope signals integration convergence; successful national opt-outs or exclusion powers signal sovereignty convergence.',
    'Integration convergence would migrate victims toward beneficiary status and pull epsilon toward rope; sovereignty convergence would concentrate exclusion costs on mobile workers and pull the arrangement toward snare for that seat. This story''s classification holds only for the coordination reading''s current instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel, and its classification is indexical to that reading.').

omega_variable(
    posted_worker_contribution_offset,
    'Do posted workers'' home-account contributions, plus eventual home-system benefits, approximately net out the host-system access they forgo during assignment?',
    'Longitudinal administrative microdata linking individual contribution histories across systems to realized benefit receipts after return.',
    'If strongly net-negative, the authored epsilon understates the burden on the posted_worker seat; if roughly neutral, the posting harm concentrates in wage and working-condition terms rather than welfare-access terms, redistributing extraction across the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posted_worker_contribution_offset, empirical, 'Whether the levy-exemption structure transfers or merely relocates welfare contributions for posted workers.').

omega_variable(
    wage_pressure_attribution,
    'How much of destination-state low-wage stagnation in posted-worker-intensive sectors is attributable to posting cost-competition versus automation, broader immigration, and sectoral decline?',
    'Sectoral difference-in-differences around posting-corridor openings and closures, exploiting the timing of enforcement tightenings in specific state pairs.',
    'Misattribution would distort the victim-set boundaries: overstating posting''s share inflates the host-labor-market extraction term, understating it hides a real transfer channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_pressure_attribution, empirical, 'Attribution of destination labor-market damage between posting and confounders.').

omega_variable(
    enforcement_durability,
    'Is the post-2018 enforcement buildout (joint inspections, extended equal treatment, subcontractor liability) a durable capacity shift or a political-attention cycle that will decay?',
    'Inspection-intensity and sanction-rate series five-plus years past the revision, controlling for headline cases.',
    'Decay would confirm the theater hump as recurring — declarative enforcement returns, extraction re-accumulates — and date a future piton-flavored drift; durability would fix the current suppression_requirement trajectory as structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_durability, empirical, 'Whether the measured enforcement intensification persists or is episodic.').

omega_variable(
    autonomy_dumping_separability,
    'Is preservation of national welfare-design autonomy structurally separable from the cost competition it licenses, or does autonomy over contribution levels inherently generate dumping pressure wherever labor can be routed through cheaper systems?',
    'Compare sectors where host collective agreements apply erga omnes to posted workers (construction in several destination states) against uncovered sectors: if parity closes the wage gap where applied, the dumping driver is coverage, not autonomy; if gaps persist, autonomy itself is implicated.',
    'If inseparable, the reading''s own axiom — fair movement compatible with full welfare autonomy — fails internally, and the arrangement trends toward pure extraction on the posting channel; if separable, targeted coverage extension can defuse the victim set without touching the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_dumping_separability, conceptual, 'Whether the coordination reading''s two commitments (autonomy, anti-dumping) are jointly satisfiable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_welfare_coord_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fmk_welfare_coord_tr_t6, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(fmk_welfare_coord_tr_t12, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(fmk_welfare_coord_tr_t18, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(fmk_welfare_coord_tr_t24, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(fmk_welfare_coord_tr_t30, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(fmk_welfare_coord_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fmk_welfare_coord_be_t6, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(fmk_welfare_coord_be_t12, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(fmk_welfare_coord_be_t18, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(fmk_welfare_coord_be_t24, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(fmk_welfare_coord_be_t30, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fmk_welfare_coord_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fmk_welfare_coord_su_t6, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(fmk_welfare_coord_su_t12, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(fmk_welfare_coord_su_t18, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(fmk_welfare_coord_su_t24, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(fmk_welfare_coord_su_t30, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'EU free movement' decomposes into three structurally distinct constraint stories per the epsilon-invariance principle. This file is the welfare_coordination_reading (epsilon referent: the standing coordination arrangement — coordination without harmonization, enforced anti-dumping, preserved welfare autonomy; epsilon 0.62, tangled_rope). The integration_reading treats movement as constitutive citizenship with supranationally expanded equal treatment — its epsilon reflects extraction from national welfare solidarities rather than from mobile workers. The member_sovereignty_reading bounds movement by national capacity — its victim set centers on excluded or deterred mobile workers. The upstream/downstream gradient runs from the treaty-level mobility guarantee through this coordination settlement to the sibling readings' contested remedies: this settlement's demonstrated EU-level enforcement capability is precisely what changes the legitimacy conditions of the sovereignty reading's exclusion demands, and the coordination settlement's incompleteness is what fuels the integration reading's harmonization claims. Each story carries its own epsilon, beneficiaries, and victims; averaging across the family would fabricate a constraint no party holds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, institutional, 0.3).
constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
