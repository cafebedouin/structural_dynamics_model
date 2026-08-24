% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: EU Free Movement as Expansive ECJ Integration Right
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story models the integration_reading of the
 *   federation_membership_kernel: the EU Treaty's free movement provisions
 *   (TFEU Arts. 21, 45-48) as interpreted by the ECJ to establish free
 *   movement as a fundamental right constitutive of EU citizenship, with
 *   expansive scope covering not only worker access but equal treatment in
 *   social advantages and, increasingly, access to social assistance for
 *   economically inactive citizens. The ECJ's case law (Martinez Sala,
 *   Baumbast, Dano, Alimanovic) progressively extends the personal and
 *   material scope, overriding national labor market protections (posted
 *   workers directive tension, minimum wage coordination) and imposing
 *   welfare costs on receiving states without fiscal compensation. Sending
 *   states externalize brain drain costs. The coordination function (single
 *   market completion through labor mobility) coexists with asymmetric
 *   extraction (displaced local labor, unfunded welfare mandates, human
 *   capital depletion).
 *
 * KEY AGENTS:
 *   - mobile_eu_workers: Primary beneficiary (organized/constrained) — gains labor market access and equal treatment
 *   - displaced_local_labor: Primary victim (moderate/trapped) — bears wage pressure and displacement without exit
 *   - receiving_state_welfare_authorities: Victim/agenda_setter (institutional/constrained) — bears unfunded mandate, administers compliance
 *   - sending_state_authorities: Victim/observer (institutional/constrained) — loses human capital, gains remittances/cohesion funds
 *   - ecj_eu_institutions: Agenda_setter (institutional/arbitrage) — interprets scope, no direct cost-bearing
 *   - national_courts: Observer/payer (organized/constrained) — implements ECJ rulings, bears legitimacy costs
 *   - eu_integration_project: Beneficiary (institutional/civilizational) — advances federal integration through rights expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.72).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Expansive ECJ Integration Right").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '861d563f-73d7-451f-bc44-e25e6bcdf69f').
narrative_ontology:cs_kernel_codification('861d563f-73d7-451f-bc44-e25e6bcdf69f', formalized).
narrative_ontology:cs_authority_grounding('861d563f-73d7-451f-bc44-e25e6bcdf69f', lineage).
narrative_ontology:cs_interpretation_layer_present('861d563f-73d7-451f-bc44-e25e6bcdf69f').
narrative_ontology:cs_reading_relation('861d563f-73d7-451f-bc44-e25e6bcdf69f', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('861d563f-73d7-451f-bc44-e25e6bcdf69f', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('861d563f-73d7-451f-bc44-e25e6bcdf69f', foundational, free_movement_as_constitutive_citizenship_right).
narrative_ontology:cs_axiom_status(free_movement_as_constitutive_citizenship_right, holdable).
narrative_ontology:cs_axiom_grounding('861d563f-73d7-451f-bc44-e25e6bcdf69f', free_movement_as_constitutive_citizenship_right, deontological).
narrative_ontology:cs_axiom('861d563f-73d7-451f-bc44-e25e6bcdf69f', foundational, ecj_expansive_interpretation_legitimate).
narrative_ontology:cs_axiom_status(ecj_expansive_interpretation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('861d563f-73d7-451f-bc44-e25e6bcdf69f', ecj_expansive_interpretation_legitimate, conventional).
narrative_ontology:cs_reference_frame('861d563f-73d7-451f-bc44-e25e6bcdf69f', treaty_of_rome_functional_worker_mobility).
narrative_ontology:cs_drift_state('861d563f-73d7-451f-bc44-e25e6bcdf69f', post_2004_enlargements_citizenship_directive, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('861d563f-73d7-451f-bc44-e25e6bcdf69f', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, employers_seeking_eu_labor).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_integration_project).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_brain_drain).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_authorities).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_authorities).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, national_courts).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, eu_citizenship_as_fundamental_status).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, single_market_completion_through_labor_mobility).
narrative_ontology:constraint_vindicates(federation_membership_kernel__integration_reading, ecj_interpretive_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU citizens exercising free movement for work, study, or family. Gain access to 27 labor markets, equal treatment in employment conditions, and (per ECJ case law) increasing access to social advantages. Exit is constrained: leaving the EU means losing the right; moving within the EU is the exercise of the right, not exit from it. Organized through EU-level unions and mobility platforms but individually mobile.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    organized, biographical, constrained, continental).

% Workers in receiving regions/sectors facing wage pressure and displacement from mobile EU labor. No effective exit: cannot leave the national labor market without severe cost; EU law forecloses national protective measures (posting restrictions, minimum wage enforcement on posted workers). Collective bargaining coverage erodes where posted workers undercut local rates. Trapped in the sense that the constraint's suppression (ECJ overriding national protections) removes their collective exit options.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    moderate, biographical, trapped, national).

% National, regional, and local administrations bearing costs of social assistance, healthcare, and housing for mobile EU citizens (including economically inactive per Martinez Sala/Baumbast line). No fiscal transfer mechanism internalizes these costs — EU budget is ~1% GNI, no free-movement-specific fund. Constrained exit: treaty obligation binds; non-compliance triggers infringement proceedings. Dual role: they administer the constraint (agenda_setter for implementation) but bear its unfunded costs (payer).
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, receiving_state_welfare_authorities, agenda_setter).

% Member states experiencing net emigration of skilled/educated workers (brain drain). Gain remittances and EU cohesion fund allocations (structural funds, recovery fund) but these do not fully offset human capital loss and demographic decline. Constrained exit: EU membership benefits (single market access, funds, political influence) outweigh brain drain costs. Observer role in ECJ jurisprudence — affected by rulings but not direct parties.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, sending_state_authorities, observer).

% European Court of Justice (interpretive authority), Commission (enforcement), Parliament (legislative). ECJ expands scope through preliminary rulings; Commission enforces via infringement; Parliament amends directives (e.g., Posted Workers Directive revision). Arbitrage-grade exit: institutional positions are not personally costly; the institutions gain legitimacy and competency from rights expansion. No direct cost-bearing from the constraint's extraction channels.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj_eu_institutions, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% National constitutional and supreme courts implementing ECJ preliminary rulings. Bear legitimacy costs when rulings override national welfare/solidarity provisions (e.g., German Constitutional Court PSPP/EU law supremacy tensions). Constrained exit: EU law supremacy doctrine binds; non-compliance risks institutional crisis. Observer on the merits (apply law), payer of institutional legitimacy costs.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_courts, observer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, national_courts, payer).

% The federal integration project as an abstract institutional beneficiary. Free movement expansion deepens EU citizenship, weakens national sovereignty barriers, and advances 'ever closer union.' No direct cost-bearing; gains institutional depth and legal integration. Arbitrage-grade exit: the project's success is measured by rights expansion itself.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_integration_project, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Third-country nationals excluded from EU free movement rights. Would object to the two-tier mobility regime but have no standing in EU citizenship framework. Their exclusion is structural: the constraint's coordination function is explicitly for EU citizens. Trapped in national immigration regimes with no EU-level exit.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, non_eu_migrants, excluded,
    powerless, biographical, trapped, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Completes the single market by removing barriers to labor mobility across 27 member states, enabling efficient allocation of human capital, arbitrage of regional unemployment disparities, and mutual recognition of professional qualifications. Solves the collective action problem of 27 sovereign labor markets fragmenting the European economic space.
% TRANSFER_FUNCTION: Moves labor market access and equal treatment rights from national sovereignty to EU citizenship status. Transfers welfare costs from mobile citizens to receiving-state taxpayers (no fiscal compensation). Transfers human capital from sending to receiving regions (brain drain). Transfers regulatory authority from national labor protections to ECJ interpretive supremacy.
% ABSENT_VOICES: Non-EU migrants (excluded from citizenship rights), future generations in sending regions (demographic decline), economically inactive EU citizens in receiving states (bear political backlash without voice), posted workers in precarious conditions (captured by home-state social security, host-state labor law gaps). They are structurally excluded: the kernel defines its constituency as EU citizens; non-citizens have no standing; future generations have no vote; posted workers are governed by home-state law.
% DISAPPEARANCE_RATIONALE: If the integration_reading vanished overnight (ECJ reverted to pre-1990s worker-only scope, national welfare residency requirements restored, posting directive enforced strictly), receiving states would immediately reduce welfare access for EU migrants; national labor protections would reassert; sending states would retain more skilled workers; the EU citizenship project would lose its most expansive right. The single market would persist but with national labor market segmentation restored.
% FOUNDING_PROBLEM: Post-war European integration required labor mobility to reconstruct economies, balance regional disparities, and bind Germany into a peaceful European order. The 1957 Treaty of Rome established free movement of workers as a functional single market provision, not a citizenship right. The founding problem was economic integration through factor mobility.
% FOUNDING_PROBLEM_CORROBORATION: The integration_reading's beneficiaries (ECJ, Commission, mobile workers) attest the founding problem is live: labor mobility remains incomplete, posting abuses persist, social rights gaps undermine mobility. The member_sovereignty_reading's proponents (national governments, trade unions in high-wage states, constitutional courts) attest the founding problem is substantially solved for workers and the current expansion serves integrationist ideology, not market completion. Academic literature (Davies, Fenwick, Cremona) corroborates the contested status: the Treaty's teleology is disputed between functional and constitutional readings.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the asymmetric cost distribution: receiving states absorb welfare costs for mobile EU citizens without fiscal transfers; displaced local labor faces wage competition without compensation mechanisms; sending states lose educated workforce. Suppression (0.72) is high because national labor protections (collective bargaining, minimum wages, posting restrictions) are overridden by ECJ rulings (Laval, Viking, Ruffert) and the Posted Workers Directive enforcement gap. Theater ratio (0.28) is moderate: the coordination function (single market labor mobility) is genuine and operational, but a growing share of ECJ jurisprudence serves rights-expansion rather than market-integration. Accessibility collapse (0.65) reflects that alternatives (national labor market closure, welfare residency requirements) are legally foreclosed by EU law supremacy. Resistance (0.58) is significant: political backlash (Brexit, Eastern European resistance to posting directive reform, welfare tourism narratives) but channeled through EU institutional processes rather than exit.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural: mobile_eu_workers experience the constraint as rope (genuine coordination, net benefit, mobile exit). Displaced_local_labor experiences it as snare (extraction, no exit, suppressed alternatives). Receiving_state_welfare_authorities experience it as tangled_rope (coordination mandate with unfunded extraction). ECJ/EU_institutions experience it as scaffold (transitional integration mechanism, but no sunset). The engine computes these per-seat types from the structural data; the claimed_type (tangled_rope) reflects the constraint's aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: mobile_eu_workers (direct rights holders), employers_seeking_eu_labor (labor supply access), eu_integration_project (institutional beneficiary of rights expansion). Victims declared: displaced_local_labor (wage/displacement costs, trapped exit), receiving_state_welfare_systems (unfunded mandate, constrained exit via EU law), sending_state_brain_drain (human capital loss, no compensation). The ECJ is the agenda_setter with arbitrage-grade exit (institutional position, no personal cost). National courts are payers implementing rulings. Directionality derives from beneficiary/victim structure plus exit options: mobile workers have constrained exit (can move but lose EU rights if leave EU); displaced labor is trapped (no EU-level exit); receiving states are constrained (treaty-bound); sending states are constrained (EU membership benefit outweighs brain drain cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war reconstruction labor mobility, single market completion) is contested: the coordination function remains live for labor market integration, but the welfare dimension has expanded beyond the founding mandate. The integration_reading treats the welfare expansion as inherent to citizenship; the member_sovereignty_reading treats it as mandatrophy. The constraint is not resolved mandatrophy because the coordination function (labor mobility) is still live and the extraction (welfare costs) is structurally embedded in the rights architecture. Theater ratio rising suggests drift toward piton if coordination function atrophies further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the integration_reading a structurally distinct constraint from the member_sovereignty_reading and welfare_coordination_reading, or are they observable-dependent views of one constraint?',
    'Test ε-invariance: if measuring the constraint via ''mobile worker rights'' yields low extraction but measuring via ''receiving state welfare cost'' yields high extraction, the label ''free movement'' covers multiple constraints. Decompose per DP-001.',
    'If ε differs across measurement bases, this JSON must split into multiple constraint stories linked by network.affects_constraints. Current authoring assumes single ε for this reading''s structural referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Commitment kernel decomposition: whether the three declared readings instantiate three constraints or one constraint with observer-dependent classification.').

omega_variable(
    fiscal_compensation_gap,
    'Does the absence of a fiscal transfer mechanism for receiving-state welfare costs constitute a structural extraction channel or a coordination gap awaiting political resolution?',
    'Track whether EU-level fiscal capacity (own resources, borrowing authority) evolves to internalize cross-border welfare costs. If it does not after multiple treaty revisions, the gap is structural extraction.',
    'If structural, the constraint is tangled_rope with permanent asymmetric extraction. If resolvable, the extraction is transitional and the constraint trends toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_compensation_gap, empirical, 'Whether welfare cost externalization is a designed feature or an unresolved coordination problem.').

omega_variable(
    brain_drain_externality,
    'Is sending-state human capital depletion an externality the integration_reading internalizes (through remittances, return migration, EU cohesion funds) or a net extractive loss?',
    'Longitudinal cohort analysis of sending regions: compare GDP per capita trajectories of high-emigration vs. low-emigration NUTS-2 regions controlling for cohesion fund allocation.',
    'If net extractive, sending_state_brain_drain is a victim seat with high χ. If internalized, the victim declaration is overstated and the constraint trends toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_externality, empirical, 'Whether the brain drain victim seat experiences net extraction or net coordination benefit over the biographical horizon.').

omega_variable(
    ecj_interpretive_drift,
    'Has the ECJ''s expansive interpretation drifted from the Treaty''s original labor-mobility coordination function into a general social-rights harmonization mandate?',
    'Code ECJ free movement judgments 1990-2025 by scope: (a) worker access to labor market, (b) equal treatment in employment conditions, (c) social assistance access for economically inactive. Track share of (c) over time.',
    'If drift toward (c) is substantial and acknowledged, the constraint''s claimed_type shifts from tangled_rope (coordination+extraction) toward snare (extraction under coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_interpretive_drift, conceptual, 'Whether the supranational authority''s interpretive expansion has exceeded the coordination function''s boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_int_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(fmk_int_tr_t0, observed).
narrative_ontology:measurement(fmk_int_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(fmk_int_tr_t10, observed).
narrative_ontology:measurement(fmk_int_tr_t20, federation_membership_kernel__integration_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(fmk_int_tr_t20, observed).
narrative_ontology:measurement(fmk_int_tr_t30, federation_membership_kernel__integration_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(fmk_int_tr_t30, observed).
narrative_ontology:measurement(fmk_int_tr_t40, federation_membership_kernel__integration_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(fmk_int_tr_t40, observed).
narrative_ontology:measurement(fmk_int_tr_t50, federation_membership_kernel__integration_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement_basis(fmk_int_tr_t50, observed).
narrative_ontology:measurement(fmk_int_tr_t60, federation_membership_kernel__integration_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(fmk_int_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(fmk_int_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(fmk_int_be_t0, observed).
narrative_ontology:measurement(fmk_int_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(fmk_int_be_t10, observed).
narrative_ontology:measurement(fmk_int_be_t20, federation_membership_kernel__integration_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(fmk_int_be_t20, observed).
narrative_ontology:measurement(fmk_int_be_t30, federation_membership_kernel__integration_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(fmk_int_be_t30, observed).
narrative_ontology:measurement(fmk_int_be_t40, federation_membership_kernel__integration_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(fmk_int_be_t40, observed).
narrative_ontology:measurement(fmk_int_be_t50, federation_membership_kernel__integration_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(fmk_int_be_t50, observed).
narrative_ontology:measurement(fmk_int_be_t60, federation_membership_kernel__integration_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(fmk_int_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(fmk_int_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(fmk_int_su_t0, observed).
narrative_ontology:measurement(fmk_int_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(fmk_int_su_t10, observed).
narrative_ontology:measurement(fmk_int_su_t20, federation_membership_kernel__integration_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(fmk_int_su_t20, observed).
narrative_ontology:measurement(fmk_int_su_t30, federation_membership_kernel__integration_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(fmk_int_su_t30, observed).
narrative_ontology:measurement(fmk_int_su_t40, federation_membership_kernel__integration_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(fmk_int_su_t40, observed).
narrative_ontology:measurement(fmk_int_su_t50, federation_membership_kernel__integration_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(fmk_int_su_t50, observed).
narrative_ontology:measurement(fmk_int_su_t60, federation_membership_kernel__integration_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(fmk_int_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__integration_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, posted_workers_directive_enforcement).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_cohesion_policy_transfers).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_citizenship_rights_expansion).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the federation_membership_kernel. The integration_reading claims free movement as expansive fundamental right (high extraction on welfare dimension). The member_sovereignty_reading claims bounded right with national welfare protection (lower extraction, higher national suppression). The welfare_coordination_reading claims coordination without harmonization (lowest extraction, coordination-dominant). Their ε values differ substantially: integration_reading ε≈0.68 (welfare costs externalized), member_sovereignty_reading ε≈0.35 (national welfare protected), welfare_coordination_reading ε≈0.22 (coordination cost only). They are linked via affects_constraints forming a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, organized, 0.35).
constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
