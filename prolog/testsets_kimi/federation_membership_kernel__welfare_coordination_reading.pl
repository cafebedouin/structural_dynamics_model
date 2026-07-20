% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: EU Welfare Coordination Free Movement Regime
 *   domain: political/federalism/migration
 *
 * SUMMARY:
 *   The EU free movement regime operates not through supranational welfare
 *   harmonization but through a coordination architecture that preserves
 *   member state autonomy over social security design. The Posted Workers
 *   Directive and anti-social-dumping enforcement create a framework where
 *   workers can be posted across borders under home-country conditions for
 *   limited periods. This welfare_coordination_reading of the
 *   federation_membership_kernel sees the constraint as a genuine
 *   coordination mechanism that nonetheless produces structural victims:
 *   posted workers face coverage gaps and wage undercutting, receiving state
 *   workers face cost-competition displacement, and sending states lose
 *   fiscal capacity without compensation. The EU Commission enforces the
 *   framework as necessary single-market infrastructure, while cross-border
 *   posting firms capture the regulatory arbitrage gains.
 *
 * KEY AGENTS:
 *   - eu_commission: Primary agenda-setter (institutional/arbitrage) â designs and enforces posted workers framework and anti-social-dumping rules
 *   - cross_border_posting_firms: Primary beneficiary (powerful/arbitrage) â captures cost advantages from home-state social contribution exemptions and wage differentials
 *   - posted_workers: Primary target (powerless/trapped) â works under home-country contracts in host states with reduced social protection and effective wage compression
 *   - receiving_state_workers: Secondary target (moderate/constrained) â faces competitive displacement from posting-driven wage undercutting and labor market dualization
 *   - sending_state_governments: Fiscal target (institutional/constrained) â loses skilled labor and social contributions without receiving compensatory fiscal transfers from host states
 *   - labor_unions: Excluded voice (organized/constrained) â advocates for equal treatment but is structurally marginalized in EU service-liberalization frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.62).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.58).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination Free Movement Regime").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '6a799253-d1e2-4846-b5de-f4b38dd03eae').
narrative_ontology:cs_kernel_codification('6a799253-d1e2-4846-b5de-f4b38dd03eae', formalized).
narrative_ontology:cs_authority_grounding('6a799253-d1e2-4846-b5de-f4b38dd03eae', lineage).
narrative_ontology:cs_interpretation_layer_present('6a799253-d1e2-4846-b5de-f4b38dd03eae').
narrative_ontology:cs_reading_relation('6a799253-d1e2-4846-b5de-f4b38dd03eae', federation_membership_kernel__integration_reading, influences).
narrative_ontology:cs_reading_relation('6a799253-d1e2-4846-b5de-f4b38dd03eae', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6a799253-d1e2-4846-b5de-f4b38dd03eae', foundational, welfare_design_autonomy_non_negotiable).
narrative_ontology:cs_axiom_status(welfare_design_autonomy_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('6a799253-d1e2-4846-b5de-f4b38dd03eae', welfare_design_autonomy_non_negotiable, conventional).
narrative_ontology:cs_axiom('6a799253-d1e2-4846-b5de-f4b38dd03eae', foundational, coordinated_mobility_without_harmonization).
narrative_ontology:cs_axiom_status(coordinated_mobility_without_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('6a799253-d1e2-4846-b5de-f4b38dd03eae', coordinated_mobility_without_harmonization, instrumental).
narrative_ontology:cs_reference_frame('6a799253-d1e2-4846-b5de-f4b38dd03eae', national_welfare_coordination_equilibrium).
narrative_ontology:cs_drift_state('6a799253-d1e2-4846-b5de-f4b38dd03eae', post_2018_directive_revision, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a799253-d1e2-4846-b5de-f4b38dd03eae', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, cross_border_posting_firms).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, domestic_service_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the Posted Workers Directive and anti-social-dumping framework, policing member state barriers to cross-border service provision while preserving national welfare design autonomy under EU Treaty freedoms.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Retain formal authority over social security design, contribution levels, and labor law regimes; accept posted workers into their territories but preserve divergent national welfare architectures.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, member_state_governments, beneficiary,
    institutional, generational, constrained, national).

% Post workers across borders under home-country contracts and social contribution regimes, exploiting temporary exemptions from host-state levies to achieve cost advantages over domestic competitors.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, cross_border_posting_firms, beneficiary,
    powerful, biographical, arbitrage, continental).

% Work in host states under home-country contracts with reduced social protection coverage and effective wage compression during the posting period; dependent on employer to maintain posting status and legal residence.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, trapped, national).

% Face wage competition and labor market dualization from posted workers whose employers do not bear full host-state social charges; compete for employment and public services under asymmetric contribution obligations.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_workers, payer,
    moderate, biographical, constrained, national).

% Lose skilled workers to cross-border posting and permanent emigration without receiving corresponding fiscal compensation or social contributions from host states; face erosion of domestic social insurance bases.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, payer,
    institutional, generational, constrained, national).

% Compete against cross-border posting firms that operate under lighter social contribution burdens and wage structures; face market share loss in sectors open to cross-border service provision.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, domestic_service_firms, payer,
    moderate, biographical, constrained, national).

% Advocate for equal treatment and host-state application of labor law but are structurally sidelined in EU-level service-liberalization negotiations; their preferred alternatives are institutionally blocked.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, labor_unions, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, cross_border_posting_firms).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-border labor mobility and service provision across heterogeneous national welfare regimes without requiring full supranational harmonization, by enforcing minimum anti-social-dumping standards while preserving member state autonomy over social security design and contribution levels.
% TRANSFER_FUNCTION: Moves labor cost advantages from host-state social contribution regimes to cross-border posting firms; moves fiscal burdens from sending states to receiving states in the form of brain drain and uncompensated public service provision; moves wage and competitive pressure from posted labor to domestic service firms and native workers in receiving states.
% ABSENT_VOICES: Labor unions and worker advocates arguing for full host-state regime application or equal-pay-equal-place principles; sending state taxpayers seeking fiscal compensation for emigrated workers; receiving state low-wage workers undercut by posted labor but excluded from EU-level service liberalization negotiations.
% DISAPPEARANCE_RATIONALE: If the coordination framework vanished overnight, cross-border posting would lose its legal architecture and cost advantages, posting firms would face host-state social charges and competitive equalization, member states would face immediate pressure to either harmonize welfare contributions upward or reimpose labor market controls at borders, and mobility patterns would shift toward permanent migration regimes rather than temporary service provision.
% FOUNDING_PROBLEM: How to construct a single market for labor and services across deeply heterogeneous welfare states with divergent social contribution levels and labor costs, without either destroying national social solidarity through regulatory competition or freezing mobility through protectionism.
% FOUNDING_PROBLEM_CORROBORATION: EU Commission and Court of Justice attest the problem as live and solved only by Treaty-based mobility rights. Sending state finance ministries and receiving state labor ministries attest the problem has mutated into a structural externality generator. Independent comparative political economists and federalism scholars outside the benefiting parties document the tension between autonomy preservation and cross-border fiscal spillovers.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) captures the substantial but not total extraction inherent in a coordination regime that enables systematic regulatory arbitrage: the anti-social-dumping floor is real but porous, and the temporary exemption window creates persistent cost advantages. Suppression (0.58) reflects the active enforcement required to prevent member states from fully applying host-state regimes or closing borders, combined with the institutional suppression of full-harmonization alternatives. Theater ratio (0.25) is moderate: anti-social-dumping investigations and revised directive language perform coordination concern, while the structural extraction (cabotage undercutting, posting-to-permanent migration pathways) continues. Accessibility collapse (0.45) is moderate: alternatives such as full host-state regime application or Treaty revision to allow closed welfare borders exist in political discourse but are institutionally blocked by ECJ jurisprudence and Commission enforcement. Resistance (0.55) is significant and rising: receiving state labor ministries, sending state governments, and unions resist, but are fragmented by divergent national interests.
 *
 * PERSPECTIVAL GAP:
 *   The EU Commission seat computes the constraint as coordination preserving market integrity and federal balance; the posting-firm seat computes it as a direct subsidy to cross-border competitiveness; the posted-worker and native-worker seats compute it as extraction; the sending-state seat computes it as fiscal drain. The structural divergence arises from identical spatial scopes (EU/national) but radically different exit options: the Commission can reform the directive (arbitrage), posting firms can relocate operations (arbitrage), while posted workers are trapped by employer-dependent status and workers/governments are constrained by Treaty lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU Commission sits at low d (agenda-setter with reform capacity). Cross-border posting firms sit at very low d (direct beneficiaries of the levy exemption and wage differential). Posted workers sit near full-target d (their wages and coverage are structurally depressed by the posting mechanism). Receiving state workers sit at high d (competitive displacement). Sending state governments sit at moderate-high d (fiscal externality). Domestic service firms sit at high d (unfair competition). Labor unions are excluded; their effective d would be high if seated. The directionality derivation follows beneficiary/victim declarations: beneficiaries map to low d, victims to high d, with exit modulation pushing trapped agents toward the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the regime as pure extraction (it genuinely coordinates heterogeneous welfare states without harmonization) or as pure coordination (it systematically generates victims through regulatory arbitrage). The active enforcement requirement separates it from rope: the Commission must continually police member state barriers and social dumping. The presence of both concentrated beneficiaries (posting firms) and diffuse victims (workers, states) with a real coordination function (autonomy preservation, anti-dumping floor) is the canonical tangled rope signature. Mandatrophy would occur if the coordination function atrophied but the framework persisted purely as extraction â the temporal measurements show rising extractiveness and theater, suggesting drift toward that attractor, but the coordination function has not yet fully decayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    posting_vs_permanent_migration_boundary,
    'Does the posted worker regime genuinely regulate temporary service provision, or has it become a structural pathway for permanent labor migration under lighter social obligations?',
    'Longitudinal analysis of posting duration, repeat-posting chains, and transition rates from posted status to permanent residence in host states.',
    'If posting is largely temporary and circular, the coordination reading strengthens; if it functions as permanent migration with reduced contributions, the extraction reading strengthens and the welfare_coordination frame becomes cover for fiscal arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posting_vs_permanent_migration_boundary, empirical, 'Whether posting is temporary mobility or permanent migration under reduced obligations.').

omega_variable(
    harmonization_feasibility,
    'Is welfare system harmonization across EU member states empirically infeasible, making this coordination regime the only viable alternative to closed borders?',
    'Comparative analysis of existing partial harmonization mechanisms and fiscal capacity convergence data across member states.',
    'If harmonization is viable but politically blocked, the coordination function is weaker and extraction more contingent on political choice; if genuinely infeasible due to heterogeneity, the extraction is more like a necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harmonization_feasibility, conceptual, 'Whether extraction stems from infeasible alternatives or blocked political choice.').

omega_variable(
    equal_treatment_implementation_gap,
    'Does the revised Posted Workers Directive (2018) actually close the implementation gap between nominal equal-treatment rights and effective host-state regime application?',
    'Empirical audit of member state transposition quality and labor inspectorate enforcement data across receiving states.',
    'If the gap persists despite revision, the coordination function is weaker than claimed and extraction is structural; if closed, the welfare_coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equal_treatment_implementation_gap, empirical, 'Whether post-2018 reforms narrowed the gap between nominal rights and effective protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 25, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
