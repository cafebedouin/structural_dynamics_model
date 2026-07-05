% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Coordination-Not-Harmonization Regime for Free Movement and Social Security
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story instantiates the welfare_coordination_reading of the
 *   federation_membership_kernel: free movement is operationalized through
 *   cross-border coordination of national social security entitlements
 *   (Regulation 883/2004 and its predecessors) rather than through harmonized
 *   EU-level welfare standards. The Commission enforces anti-social-dumping
 *   rules (the Posted Workers Directive and its 2018/2020 revisions) as a
 *   patch on this architecture, without touching member states' underlying
 *   welfare design autonomy — pension systems, minimum wage mechanisms, and
 *   sectoral bargaining remain wholly national. The structural delta from the
 *   sibling readings: this reading generates a posted-worker victim class
 *   through the two-year social levy exemption and cabotage wage
 *   undercutting, places dual pressure on receiving-state labor markets
 *   (posted-worker undercutting plus permanent-migrant displacement), and
 *   leaves sending states with no fiscal compensation mechanism for
 *   outward-migrating labor. This is a distinct constraint from the
 *   integration_reading (which asserts ECJ-driven expansive harmonization of
 *   free movement rights) and from the member_sovereignty_reading (which
 *   asserts member states retain bounded authority to exclude economically
 *   inactive migrants) — those are different claims about different
 *   institutional mechanisms and are authored as separate sibling
 *   constraints, not folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.52).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Coordination-Not-Harmonization Regime for Free Movement and Social Security").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '0980007f-69b3-45d9-a03b-c429d1083043').
narrative_ontology:cs_kernel_codification('0980007f-69b3-45d9-a03b-c429d1083043', formalized).
narrative_ontology:cs_authority_grounding('0980007f-69b3-45d9-a03b-c429d1083043', practice).
narrative_ontology:cs_interpretation_layer_present('0980007f-69b3-45d9-a03b-c429d1083043').
narrative_ontology:cs_reading_relation('0980007f-69b3-45d9-a03b-c429d1083043', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('0980007f-69b3-45d9-a03b-c429d1083043', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('0980007f-69b3-45d9-a03b-c429d1083043', foundational, welfare_design_competence_stays_national).
narrative_ontology:cs_axiom_status(welfare_design_competence_stays_national, holdable).
narrative_ontology:cs_axiom_grounding('0980007f-69b3-45d9-a03b-c429d1083043', welfare_design_competence_stays_national, conventional).
narrative_ontology:cs_axiom('0980007f-69b3-45d9-a03b-c429d1083043', secondary, portability_not_uniformity_satisfies_free_movement).
narrative_ontology:cs_axiom_status(portability_not_uniformity_satisfies_free_movement, holdable).
narrative_ontology:cs_axiom_grounding('0980007f-69b3-45d9-a03b-c429d1083043', portability_not_uniformity_satisfies_free_movement, instrumental).
narrative_ontology:cs_reference_frame('0980007f-69b3-45d9-a03b-c429d1083043', subsidiarity_bounded_coordination_1971_regime).
narrative_ontology:cs_drift_state('0980007f-69b3-45d9-a03b-c429d1083043', post_2018_directive_revision_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0980007f-69b3-45d9-a03b-c429d1083043', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posting_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, high_wage_member_state_treasuries).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, single_market_service_exporters).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_domestic_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_labor_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, cabotage_hauliers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, cabotage_hauliers).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, coordination_not_harmonization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Post workers from lower-wage member states to perform services in higher-wage states while paying social contributions at the sending state's (lower) rate for up to two years under the A1 certificate regime. This wage-and-levy differential is the entire commercial basis for their competitive advantage on cross-border contracts; they can relocate their posting corridors if any single route is closed.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posting_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% Work in the receiving state under receiving-state minimum wage floor rules but sending-state social contribution rates, often via letterbox subcontractors. Take home less effective protection than either domestic workers in the receiving state or resident workers in the sending state; housing, contract enforcement, and health coverage frequently default to whichever jurisdiction is administratively convenient for the employer. Exit means returning to lower-wage sending-state work; leverage against the posting firm is minimal.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, constrained, continental).

% Compete for construction, transport, and services work against posted labor priced below full domestic labor cost. Cannot easily move sectors or regions fast enough to escape wage compression in affected trades; unions can contest but cannot unilaterally close the wage gap because the exemption is set at EU regulatory level.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Lose working-age labor to posting and permanent outward migration without receiving fiscal transfers proportional to the lost tax base and lost future social contributions; retain the cost of raising, educating, and eventually caring for an aging population that a mobile cohort has left. No mechanism exists to bill the receiving state or the posting employer for this loss.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_labor_markets, payer,
    moderate, generational, trapped, national).

% Receive an inflow of working-age labor whose training costs were borne elsewhere, while their own welfare architecture (pension design, minimum wage structures, sectoral bargaining) remains untouched by EU-level harmonization requirements. Negotiate directive text (Posted Workers Directive revisions, coordination regulations) in Council, giving them substantial control over how tightly the anti-dumping rules bind.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, high_wage_member_state_treasuries, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, high_wage_member_state_treasuries, agenda_setter).

% Drafts and enforces coordination regulations (Regulation 883/2004 and successors) and anti-social-dumping directives, adjudicating infringement actions against member states while explicitly declining to propose harmonized minimum social contribution floors, since welfare design competence is reserved to member states under the treaties. Administers the compromise rather than resolving its underlying tension.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, european_commission, agenda_setter,
    institutional, civilizational, analytical, continental).

% Road haulage firms from lower-cost member states perform cabotage runs within higher-cost states, undercutting local carriers on price while facing tightening cabotage-limit and driver-return enforcement. Benefit from the wage gap but are themselves squeezed as enforcement of return-to-base rules and minimum wage application intensifies.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, cabotage_hauliers, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, cabotage_hauliers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, posting_employers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a genuine collective-action failure: without any cross-border social security coordination, workers moving between member states would lose pension credits, health coverage, and unemployment entitlements every time they crossed a border, making free movement practically unusable. The regulation coordinates entitlement portability without requiring any state to redesign its domestic welfare architecture.
% TRANSFER_FUNCTION: Moves labor cost advantage from receiving-state domestic workers and posted workers themselves to posting employers and cabotage operators, and moves fiscal capacity (trained workers, future contributors) from sending states to receiving-state treasuries and employers, without any compensating transfer running the other way.
% ABSENT_VOICES: Posted workers themselves are rarely represented in the negotiations that set contribution-exemption periods and enforcement thresholds — their unions, where they exist, are sending-state unions with weak standing in receiving-state bargaining, and receiving-state unions represent domestic rather than posted labor. Sending-state regional governments losing working-age population have no formal seat in setting EU-level coordination rules.
% DISAPPEARANCE_RATIONALE: If the coordination-not-harmonization compromise collapsed overnight — either toward full harmonization or toward re-nationalized social security systems — posting-based labor arbitrage would end or intensify dramatically. Posting employers would lose their commercial model under harmonization, or receiving-state domestic wages would collapse further under re-fragmentation with no coordination floor at all. Millions of current cross-border employment relationships depend on the coordination architecture continuing to exist in its current partial form.
% FOUNDING_PROBLEM: Free movement of workers, a founding single-market freedom, was unusable in practice because national social security systems were mutually incompatible — a worker crossing a border could lose pension years, health coverage, and family benefits entirely. The coordination regulations were built to make cross-border work administratively survivable without requiring any state to give up control of its own welfare state design.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and high-wage-state treasuries attest the founding problem — portability — remains live and the regulation still functions as intended. Independent labor economists (e.g. ETUI and CEPS cross-border labor mobility studies) and sending-state government submissions to Council attest that the coordination framework has been substantially repurposed: the portability problem it was built to solve is largely solved for genuine mobile citizens, while the posting-and-exemption machinery layered on top now functions primarily as a wage-arbitrage channel unrelated to the original portability rationale.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) and suppression (0.52) reflect that the posting-and-coordination architecture functions as genuine coordination (portability of pensions and health entitlements is real and valuable) layered with a substantial extractive component (the two-year contribution-exemption window and enforcement gaps that let posting employers and cabotage operators price below domestic labor cost without commensurate contribution). Theater ratio (0.31, rising over the interval) captures growing enforcement activity — A1 certificate audits, cabotage return-to-base checks — that is real but increasingly performative relative to the scale of continued undercutting; enforcement capacity has not kept pace with posting volume growth. All three temporal series share one time grid (1996–2024) reflecting successive EU enlargements and directive revisions (2004, 2014, 2018 revisions to the Posted Workers Directive) that both tightened and complicated the anti-dumping enforcement layer.
 *
 * DIRECTIONALITY LOGIC:
 *   Posting employers and high-wage treasuries sit near the beneficiary end: they capture wage-cost advantage or an untrained-cost-free labor inflow respectively, and both have strong exit/mobility options relative to the constraint (employers can shift posting corridors; treasuries set the negotiating terms in Council). Posted workers sit near the target end: trapped between two jurisdictions' partial protections, minimal individual bargaining power, weak union standing in the receiving state. Receiving-state domestic workers and sending-state labor markets are structural payers without direct capture of any of the gains — the classic tangled-rope signature of a genuine coordination function (pension/health portability) riding alongside asymmetric extraction (wage-cost arbitrage) that requires active Commission/ECJ enforcement to sustain in its current partial form.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — administrative unusability of free movement without social security coordination — is substantially solved for the archetypal mobile EU citizen; genuine cross-border retirees and permanent migrants receive real portability benefit. But the posting-and-exemption machinery that grew up around the coordination framework has drifted from that founding function toward an active wage-arbitrage channel serving posting employers and cabotage operators, a function the original 1971/2004 coordination regulations were never built to provide. This is precisely the seat-divergence the classification exists to surface: from the Commission's seat, the regime is functioning coordination; from the posted-worker and receiving-state-domestic-worker seats, the same rules operate as engineered wage suppression. Neither seat is wrong about its own experience — the tangled_rope classification holds both readings without collapsing one into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_harmonization_kernel_framing,
    'Is the coordination-not-harmonization architecture the correct reading of what the free-movement kernel requires, or is it a historically contingent political compromise that the integration_reading and member_sovereignty_reading each claim should be displaced — one toward supranational harmonization, one toward restored national exclusion authority?',
    'Track ECJ jurisprudence trajectory (expanding vs. contracting equal-treatment obligations for mobile workers) and Council negotiating outcomes on Posted Workers Directive revisions over successive multi-annual financial framework cycles; convergence toward either sibling reading over time would be the resolving signal.',
    'If the integration_reading''s expansive equal-treatment logic prevails structurally, the posted-worker victim class in this reading shrinks and the constraint drifts toward rope; if the member_sovereignty_reading''s exclusion-authority logic prevails, the beneficiary set (posting employers, cabotage operators) loses its commercial basis and the constraint could collapse toward scaffold-with-sunset as posting is phased out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_harmonization_kernel_framing, conceptual, 'Which sibling reading of the federation_membership_kernel the coordination architecture is actually converging toward.').

omega_variable(
    posting_exemption_naturalness,
    'Is the two-year social contribution exemption for posted workers a necessary transitional feature of cross-border service provision, or is it a captured carve-out maintained by organized posting-employer and cabotage lobbying against fiscal-equalization reform?',
    'Compare Commission technical justifications for the exemption period against documented lobbying positions from employer federations (e.g., road haulage and construction sector associations) in the 2018/2020 directive revision consultations; examine whether any member state has proposed shortening the window and what coalition blocked it.',
    'If the exemption period is lobbying-captured rather than technically necessary, the extractiveness component of this reading is understated and the constraint sits closer to snare than tangled_rope at the posting-employer/posted-worker dyad specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posting_exemption_naturalness, empirical, 'Whether the contribution-exemption window reflects genuine transition cost or captured extraction.').

omega_variable(
    sending_state_fiscal_compensation_absence,
    'Is the absence of any fiscal transfer mechanism compensating sending states for outward labor migration a structural oversight correctable within the coordination framework, or is it definitionally excluded by the coordination-not-harmonization premise itself (since fiscal transfers would require harmonized fiscal competence the reading explicitly withholds from Brussels)?',
    'Examine whether any Commission proposal (e.g. cohesion fund reallocation tied to net emigration) has been formally tabled and rejected, versus never seriously proposed because it falls outside the reading''s own competence boundary.',
    'If structurally excluded by the reading''s own logic, sending-state labor market losses are a permanent, undeclared feature of this specific reading rather than a fixable gap — strengthening the case that sending_state_labor_markets are victims of the reading''s architecture itself, not merely an implementation failure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_fiscal_compensation_absence, conceptual, 'Whether sending-state fiscal loss is fixable within welfare_coordination_reading or is definitionally excluded by its coordination-not-harmonization premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1996, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1996, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement_basis(fede_tr_t1996, observed).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2004, 0.16).
narrative_ontology:measurement_basis(fede_tr_t2004, observed).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement_basis(fede_tr_t2010, observed).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement_basis(fede_tr_t2016, observed).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement_basis(fede_tr_t2020, observed).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(fede_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1996, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1996, 0.28).
narrative_ontology:measurement_basis(fede_be_t1996, observed).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2004, 0.38).
narrative_ontology:measurement_basis(fede_be_t2004, observed).
narrative_ontology:measurement(fede_be_t2010, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement_basis(fede_be_t2010, observed).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2016, 0.53).
narrative_ontology:measurement_basis(fede_be_t2016, observed).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(fede_be_t2020, observed).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(fede_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1996, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1996, 0.3).
narrative_ontology:measurement_basis(fede_su_t1996, observed).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2004, 0.35).
narrative_ontology:measurement_basis(fede_su_t2004, observed).
narrative_ontology:measurement(fede_su_t2010, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement_basis(fede_su_t2010, observed).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2016, 0.47).
narrative_ontology:measurement_basis(fede_su_t2016, observed).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(fede_su_t2020, observed).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(fede_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of federation_membership_kernel, decomposed per the ε-invariance principle: the natural-language concept 'EU free movement' conflates a judicial-expansion claim (integration_reading), a national-exclusion-authority claim (member_sovereignty_reading), and this coordination-architecture claim (welfare_coordination_reading), each with a distinct ε, distinct beneficiary/victim structure, and distinct classification. All three link to each other via affects_constraints rather than being merged into a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
