% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Member State Labor Market Protection (Sovereignty-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-primary reading of the
 *   federation membership treaty kernel. Under this reading, member states
 *   retain unilateral authority to restrict labor mobility and condition
 *   welfare access on national criteria. Free movement is conditional on each
 *   state's consent and can be revoked or constrained to protect labor
 *   markets and welfare systems. The sovereignty-primary reading privileges
 *   democratic control, fiscal autonomy, and national negotiated labor
 *   standards over supranational coordination of labor mobility. This reading
 *   is contested: the integration-primary reading treats free movement as
 *   constitutive of the single market; the subsidiarity-balance reading seeks
 *   to preserve both mobility and legitimate national interests through
 *   proportionality review. The constraint story focuses on the
 *   sovereignty-primary reading's structural operation, not on the
 *   adjudication between readings.
 *
 * KEY AGENTS:
 *   - member_state_governments (agenda-setter): set and enforce labor market conditions, welfare eligibility, and mobility restrictions via unilateral authority
 *   - national_labor_market_incumbents (beneficiary): wage protection through restricted supply of foreign workers
 *   - welfare_system_administrators (beneficiary): fiscal autonomy and ability to condition access on residence/contribution
 *   - mobile_workers (payer/target): restricted access to labor markets outside home state, identity-locked via aspiration for cross-border opportunity
 *   - frontier_region_businesses (payer): constrained labor supply from adjacent regions, forced to offer higher wages or reduce operations
 *   - integration_doctrine_advocates (excluded, non-agent): supranational institutions and legal scholars advancing freer movement are not in the decision loop
 *   - federation_supranational_courts (observer): narrow review scope; defer to member state labor policy under sovereignty-primary reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.71).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Member State Labor Market Protection (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '76d59235-0d8e-4c5b-87db-7715be7e3ae2').
narrative_ontology:cs_kernel_codification('76d59235-0d8e-4c5b-87db-7715be7e3ae2', formalized).
narrative_ontology:cs_authority_grounding('76d59235-0d8e-4c5b-87db-7715be7e3ae2', lineage).
narrative_ontology:cs_interpretation_layer_present('76d59235-0d8e-4c5b-87db-7715be7e3ae2').
narrative_ontology:cs_reading_relation('76d59235-0d8e-4c5b-87db-7715be7e3ae2', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('76d59235-0d8e-4c5b-87db-7715be7e3ae2', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('76d59235-0d8e-4c5b-87db-7715be7e3ae2', foundational, member_state_labor_market_authority_primacy).
narrative_ontology:cs_axiom_status(member_state_labor_market_authority_primacy, holdable).
narrative_ontology:cs_axiom_grounding('76d59235-0d8e-4c5b-87db-7715be7e3ae2', member_state_labor_market_authority_primacy, deontological).
narrative_ontology:cs_axiom('76d59235-0d8e-4c5b-87db-7715be7e3ae2', foundational, labor_market_arbitrage_threat_empirical).
narrative_ontology:cs_axiom_status(labor_market_arbitrage_threat_empirical, holdable).
narrative_ontology:cs_axiom_grounding('76d59235-0d8e-4c5b-87db-7715be7e3ae2', labor_market_arbitrage_threat_empirical, empirically_contingent).
narrative_ontology:cs_reference_frame('76d59235-0d8e-4c5b-87db-7715be7e3ae2', national_labor_market_protective_sovereignty).
narrative_ontology:cs_drift_state('76d59235-0d8e-4c5b-87db-7715be7e3ae2', contemporary_integration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76d59235-0d8e-4c5b-87db-7715be7e3ae2', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_market_incumbents).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_system_administrators).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, frontier_region_businesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the sovereignty-primary reading through unilateral authority over labor market rules, welfare eligibility, and immigration policy. They author the constraint and maintain enforcement machinery. They justify restrictions as protecting democratic control and fiscal sustainability. Their exit option is high-cost (renegotiating federation terms or exit) but exists; they are not trapped by the constraint they author.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Domestic workers and trade unions benefit from labor supply restriction: wages are defended by the constraint, occupational licensing standards persist, and employment protections negotiated with domestic employers are not undercut by foreign entrants. They have constrained exit—they must work within their home labor market or leave the federation entirely. They collectively support the sovereignty-primary reading and lobby member states to maintain restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_market_incumbents, beneficiary,
    organized, biographical, constrained, national).

% National governments operating welfare systems benefit from the ability to condition access on residence, citizenship, or contribution history. They maintain fiscal autonomy and can design eligibility rules to match revenue bases. They have moderate exit options—they can liberalize access (changing policy without leaving the federation) or exit (though at cost). They support the sovereignty-primary reading to preserve budgetary control.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, welfare_system_administrators, beneficiary,
    institutional, generational, mobile, national).

% Face restricted access to labor markets outside their home state. They cannot easily exercise their aspiration to work across the federation; access is conditional and revocable. Their exit option is identity-locked: leaving the federation means abandoning the identity as mobile workers seeking opportunity within the federation system. They are not direct parties to the rulemaking; the constraint is imposed on them by member states' unilateral authority. Their resistance to the constraint is high (they actively seek to challenge or evade restrictions) but institutionally powerless.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, biographical, identity_locked, global).

% Operate in border regions where labor supply depends on cross-border mobility. The sovereignty-primary reading restricts their access to talent pools: they must pay higher wages to attract domestic labor or accept reduced operations. They have constrained exit—they can relocate their operations (high cost) or lobby member states for exemptions (low probability of success). They are partially excluded from the rulemaking process; frontier business interests are weaker than member state or incumbent labor interests.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, frontier_region_businesses, payer,
    moderate, biographical, constrained, regional).

% Legal scholars, competition authorities, and supranational institutions advancing the integration-primary reading are excluded from the rulemaking process under the sovereignty-primary reading. They argue that restrictions incompatible with a functioning single market and that member states cannot veto integration unilaterally. Their institutional voice is marginalized because the sovereignty-primary reading preserves state veto authority.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, integration_doctrine_advocates, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, integration_doctrine_advocates).

% Review labor mobility restrictions for compliance with treaty and proportionality standards. Under the sovereignty-primary reading, their scope is narrow: they defer to member state judgments about labor market and welfare policy. They observe the constraint's operation but have limited power to harmonize or override national rules. Their analytical seat is not extracted from or subsidized by the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_supranational_courts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables member states to operate distinct, locally-legitimated labor and welfare systems without external pressure to harmonize: labor law, occupational licensing, social insurance, and employment protection remain under democratic control within each polity. Solves a genuine collective-action problem: if one state could not restrict labor access, its labor standards and welfare commitments would be undercut by arbitrage from lower-regulation neighbors.
% TRANSFER_FUNCTION: Transfers labor-market opportunity from mobile workers (restricted access) to national labor market incumbents (wage protection) and to member state governments (fiscal and regulatory autonomy). The transfer operates through border controls, work permit requirements, residency conditions, and welfare eligibility restrictions that exclude or restrict non-citizens.
% ABSENT_VOICES: Mobile workers themselves have no seat in setting the rules that restrict them; they are subordinated targets, not agenda-setters. Supranational institutions advancing the integration-primary reading are excluded from membership in labor policy decisions. Frontier region businesses are partially excluded; their interests are weaker than incumbent labor or member state interests. Federation-level worker organizations lack the veto power member states possess.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary constraint vanished, member states would lose unilateral authority to restrict labor mobility; a stronger free-movement regime would emerge (either integration-primary dominance or subsidiarity-balance compromise). Labor would flow across borders until wage and welfare arbitrage was exhausted. National labor standards would face competitive pressure; some states might exit the federation or renegotiate terms. Frontier businesses would regain access to cross-border labor pools, reducing their wage costs. Mobile workers would gain unrestricted access to labor markets across the federation. The federation would shift toward deeper labor-market integration.
% FOUNDING_PROBLEM: Early federations faced member states' fear that free movement would enable labor arbitrage and threaten locally-legitimated labor protections: wealthy states would attract workers while exporting unemployment; low-regulation states would undercut high-regulation states' labor standards; welfare systems would face fiscal strain from in-migration of benefit-seekers. Member states demanded explicit authority to defend labor markets and welfare systems as the price of federation membership.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and domestic labor unions attest the founding problem is live: labor arbitrage remains a threat to negotiated employment protections and welfare sustainability. They cite instances of in-migration pressures and competition for low-wage jobs. Integration-primary advocates and supranational courts argue the problem is overstated: they point to successful labor mobility in some federation regions without catastrophic wage or welfare effects. Economic analyses show mixed evidence: some states have experienced welfare in-migration during high-growth periods, but labor-standards convergence toward lower levels is debated. Federation-wide labor movement organizations provide no unified corroboration—they are internally divided between protecting home-country jobs and supporting worker mobility.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the transfer of labor-market access from mobile workers to protected incumbents: the constraint produces a measurable wage advantage for domestic workers and welfare budget stabilization. Suppression (0.71) is substantial because the constraint's persistence depends on active enforcement via border controls, work permit gatekeeping, and welfare eligibility verification—not on participant preference. Theater (0.42, rising over time) indicates growing reliance on justification-narrative: member states increasingly frame restrictions in terms of 'labor market protection' and 'welfare sustainability' while the actual enforcement focuses on controlling worker flows. The measurement series show extractiveness plateauing at t=28 (states reach fiscal equilibrium) and theater rising through t=35 (justification-narrative required as political pressure increases). Suppression stabilizes at t=28 (enforcement machinery reaches full maturity). All metrics are authored on a single time grid so each appears at every measurement point.
 *
 * PERSPECTIVAL GAP:
 *   From the member state government and national labor incumbent seats, the sovereignty-primary reading is genuine coordination: it protects democratic control and prevents race-to-the-bottom labor standards. From the mobile worker seat, it is enforced extraction: access to opportunity is revoked at borders. From the frontier business seat, it is extractive constraint: labor supply is artificially restricted, forcing them to bear higher wage costs. The engine computes per-seat directionality from the structural data: member states and incumbents get low d (beneficiaries, arbitrage-grade exit—they can always exit the federation, though at high cost); mobile workers get high d (targets, identity-locked—leaving the federation means abandoning their identity as mobile workers seeking opportunity). Frontier businesses sit intermediate: they have moderate power but constrained exit (high commitment to regional supply chains).
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments: d ≈ 0.2 (beneficiaries, set the rules, possess arbitrage exit—can renegotiate federation terms or exit, though at civilizational cost). National labor incumbents: d ≈ 0.3 (beneficiaries, protected supply, constrained exit—they must work in their home state but are defended against competition). Welfare administrators: d ≈ 0.25 (beneficiaries, maintain fiscal autonomy). Mobile workers: d ≈ 0.85 (targets, trapped by identity—leaving means abandoning the aspiration that defines their economic agency; staying means accepting subordination). Frontier businesses: d ≈ 0.65 (payers, moderate power but constrained exit—cannot easily relocate supply chains but can lobby for rule changes). Supranational courts: d ≈ 0.5 (analytical seat, no directionality; observer role does not accrue extraction or subsidy). The directionality profile supports tangled_rope classification: genuine coordination function (labor market stability, welfare sustainability) paired with asymmetric extraction (mobile workers' opportunity is transferred to incumbents).
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-primary reading invokes a founding problem—member states' fear of labor arbitrage and welfare in-migration—that is contested in current federation discourse. Integration-primary advocates argue the problem is overstated and that restrictions block legitimate worker mobility; they claim the founding problem is dead or substantially solved. Subsidiarity-balance advocates argue it is live but can be managed through proportionality review rather than unilateral state veto. Under the sovereignty-primary reading, the mandate is preserved unambiguously: member states retain authority, and the constraint persists by their explicit endorsement. Mandatrophy would arise if the founding problem were universally acknowledged as dead (say, through empirical demonstration that labor arbitrage does not occur) AND member states continued to enforce restrictions out of institutional inertia rather than deliberate choice. Currently, mandatrophy is not present: the reading remains actively contested, and member states defend the constraint as a live protection of legitimate interests. The rising theater_ratio (0.25 → 0.42) indicates growing performative character: as empirical pressure mounts (integration-primary advocates cite labor mobility benefits), justification-narrative thickens, but the foundational authority structure remains intact. A true mandatrophy signature would show high theater_ratio (0.6+) combined with evidence that enforcement continues despite universal acknowledgment that the founding problem is gone—we do not see that yet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_arbitrage_empirical_scope,
    'How much labor mobility actually occurs across federation borders when restrictions are eased, and does it produce the wage and welfare pressures member states predict?',
    'Comparative analysis of labor flows before and after easing restrictions in particular sectors or member states; econometric studies of wage convergence and welfare in-migration following liberalization events.',
    'If labor arbitrage is minimal or welfare in-migration is non-existent, the founding problem dissolves and the sovereignty-primary reading loses empirical support; the constraint would become mandatrophic. If arbitrage is substantial, the reading''s protective logic is validated and the constraint''s persistence is justified. Current evidence is mixed and contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_arbitrage_empirical_scope, empirical, 'The magnitude of labor arbitrage and welfare migration effects.').

omega_variable(
    sovereignty_vs_integration_foreclosure,
    'Do the sovereignty-primary and integration-primary readings logically foreclose each other within a single federation framework, or can both be held by different parties simultaneously?',
    'Institutional analysis of coexistence: have jurisdictions that adopt integration-primary rules remained in federation with those maintaining sovereignty-primary authority, and if so, what institutional mechanisms mediate the contradiction?',
    'If the readings foreclose each other, federation coherence requires choosing one; if they coexist institutively (through subsidiarity or dual regimes), the constraint is contextual rather than universal. This determines whether the sovereignty-primary reading can remain stable or must eventually yield to integration pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_foreclosure, conceptual, 'Logical compatibility of sovereignty-primary and integration-primary readings in a single framework.').

omega_variable(
    identity_lock_mechanism_suppression,
    'For mobile workers under the sovereignty-primary reading, is the measured suppression primarily structural (legal barriers, permit systems, border controls) or primarily internalized (workers have internalized the norm that their aspiration is illegitimate, or have adopted the reading''s framing that restrictions are justified)?',
    'Post-easing trajectory: if restrictions are removed and mobile workers'' labor participation rises sharply, suppression was primarily structural; if participation remains low or workers do not migrate despite open borders, suppression is at least partly internalized. Survey data on worker beliefs about legitimacy of restrictions.',
    'If internalized, the effective suppression persists even after barriers are removed, and workers carry the constraint with them; the constraint''s true grip is stronger than the legal measure suggests. If structural, removing barriers would promptly unlock labor flows. Current state: unclear, likely mixed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether suppression of mobile workers under sovereignty-primary reading is structural or internalized.').

omega_variable(
    welfare_system_fiscal_design_dependence,
    'Are welfare-system administrators genuinely dependent on the ability to restrict access to sustain fiscal equilibrium, or do they invoke worker migration as a cover story for protecting benefit levels against democratic pressure to reduce spending?',
    'Comparative welfare-system analysis: do member states that open borders to labor mobility also liberalize welfare access, or do they maintain exclusionary eligibility? Fiscal impact analysis of actual vs. predicted in-migration effects on welfare budgets.',
    'If genuinely dependent, the beneficiary status of welfare administrators is structurally justified; if invoked as cover story, the constraint is more purely extractive (protecting incumbent worker wages and job security) and the welfare justification is theater. This affects classification: strong welfare-fiscal justification supports tangled_rope; weak justification moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_fiscal_design_dependence, empirical, 'Whether welfare administrators'' fiscal concerns are genuine constraints or post-hoc justifications for labor restrictions.').

omega_variable(
    kernel_reading_contention_location,
    'What is the precise structural location of disagreement between the sovereignty-primary and integration-primary readings? Is it the empirical premise (does labor arbitrage occur?), the normative premise (is labor arbitrage bad?), or the institutional premise (which body decides when restrictions are justified)?',
    'Textual and jurisprudential analysis of how integration and sovereignty advocates cite treaty language and prior decisions; explicit articulation of where they disagree on the founding problem''s scope and persistence.',
    'If disagreement is empirical, it is resolvable by evidence; if normative, it requires value judgment; if institutional, it is resolvable by amending federation governance. This determines whether the sibling readings can ever converge or are permanently incompatible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention_location, conceptual, 'The structural locus of disagreement between sovereignty-primary and integration-primary readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fede_tr_t7, federation_membership_treaty__sovereignty_primary, theater_ratio, 7, 0.3).
narrative_ontology:measurement(fede_tr_t14, federation_membership_treaty__sovereignty_primary, theater_ratio, 14, 0.35).
narrative_ontology:measurement(fede_tr_t21, federation_membership_treaty__sovereignty_primary, theater_ratio, 21, 0.4).
narrative_ontology:measurement(fede_tr_t28, federation_membership_treaty__sovereignty_primary, theater_ratio, 28, 0.42).
narrative_ontology:measurement(fede_tr_t35, federation_membership_treaty__sovereignty_primary, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(fede_be_t7, federation_membership_treaty__sovereignty_primary, base_extractiveness, 7, 0.59).
narrative_ontology:measurement(fede_be_t14, federation_membership_treaty__sovereignty_primary, base_extractiveness, 14, 0.64).
narrative_ontology:measurement(fede_be_t21, federation_membership_treaty__sovereignty_primary, base_extractiveness, 21, 0.67).
narrative_ontology:measurement(fede_be_t28, federation_membership_treaty__sovereignty_primary, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(fede_be_t35, federation_membership_treaty__sovereignty_primary, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fede_su_t7, federation_membership_treaty__sovereignty_primary, suppression_requirement, 7, 0.63).
narrative_ontology:measurement(fede_su_t14, federation_membership_treaty__sovereignty_primary, suppression_requirement, 14, 0.67).
narrative_ontology:measurement(fede_su_t21, federation_membership_treaty__sovereignty_primary, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(fede_su_t28, federation_membership_treaty__sovereignty_primary, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(fede_su_t35, federation_membership_treaty__sovereignty_primary, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the federation_membership_treaty kernel. The sovereignty_primary reading (this story) privileges member state authority and protects national labor markets; it produces high measured extraction on mobile workers (restricted access) and high preservation of state regulatory autonomy. The integration_primary reading (sibling) presumes free movement as foundational and treats restrictions as presumptively illegitimate; it produces lower measured extraction on mobile workers but higher constraint on state autonomy. The subsidiarity_balance reading (sibling) seeks to preserve both mobility and legitimate national interests through proportionality review; it produces intermediate extracted on workers and intermediate state constraint. All three stories share the same kernel but with different ε values, beneficiary/victim structures, and stakeholder directionalities. Links are declared in network.affects_constraints: sovereignty_primary → integration_primary + subsidiarity_balance (upstream/downstream causal structure, though all three coexist institutively in real federation practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
