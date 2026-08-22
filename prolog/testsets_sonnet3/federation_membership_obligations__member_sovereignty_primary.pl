% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member-State Welfare Closure Authority Over Free Movement (Sovereignty-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   In federations with formally guaranteed free movement of labor
 *   (paradigmatically the EU), member states retain the authority to gate
 *   welfare access through residency tests, waiting periods, and
 *   contribution-history requirements, justified as protecting fiscal
 *   sustainability and domestic labor markets. This reading holds that this
 *   closure authority is the primary and legitimate arrangement: free
 *   movement is a conditional right, subordinate to national welfare
 *   sovereignty. The right to move and the right to draw full benefits are
 *   treated as structurally separable, with the latter gated by the receiving
 *   state. Under this reading, mobile workers, long-resident noncitizens, and
 *   posted workers absorb the coordination costs of protecting national
 *   systems that were financed on the assumption of a largely non-mobile
 *   contributor base.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.62).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member-State Welfare Closure Authority Over Free Movement (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '98f088d8-30e2-4743-a284-d7270147d8df').
narrative_ontology:cs_kernel_codification('98f088d8-30e2-4743-a284-d7270147d8df', formalized).
narrative_ontology:cs_authority_grounding('98f088d8-30e2-4743-a284-d7270147d8df', distributed).
narrative_ontology:cs_reading_relation('98f088d8-30e2-4743-a284-d7270147d8df', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('98f088d8-30e2-4743-a284-d7270147d8df', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('98f088d8-30e2-4743-a284-d7270147d8df', foundational, welfare_solvency_grounds_closure_authority).
narrative_ontology:cs_axiom_status(welfare_solvency_grounds_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('98f088d8-30e2-4743-a284-d7270147d8df', welfare_solvency_grounds_closure_authority, instrumental).
narrative_ontology:cs_axiom('98f088d8-30e2-4743-a284-d7270147d8df', foundational, national_legislative_sovereignty_over_benefit_eligibility).
narrative_ontology:cs_axiom_status(national_legislative_sovereignty_over_benefit_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('98f088d8-30e2-4743-a284-d7270147d8df', national_legislative_sovereignty_over_benefit_eligibility, conventional).
narrative_ontology:cs_reference_frame('98f088d8-30e2-4743-a284-d7270147d8df', national_contributory_welfare_sovereignty).
narrative_ontology:cs_drift_state('98f088d8-30e2-4743-a284-d7270147d8df', post_eastern_enlargement_mobility_surge, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('98f088d8-30e2-4743-a284-d7270147d8df', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_domestic_workforce).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, long_term_resident_noncitizens).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, posted_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set eligibility thresholds, residency tests, and habitual-residence requirements that determine whether a mobile worker can draw welfare benefits. They can tighten or loosen these tests unilaterally within treaty limits and use the sustainability argument to justify closure. They do not personally bear the cost of exclusion; they administer it.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).

% Administer means tests, habitual-residence checks, and contribution-history verification. Preserve fiscal headroom by excluding recent arrivals from full benefit access, which they present as protecting system solvency for existing contributors.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries, agenda_setter).

% Benefits from labor-market protections that slow wage competition from newly arrived mobile workers and from a welfare pool not diluted by non-contributing claimants. Can lobby domestically and vote nationally; has no exit from the arrangement because it is the arrangement's intended beneficiary class.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_domestic_workforce, beneficiary,
    organized, biographical, constrained, national).

% Move to seek work under treaty-guaranteed free movement but face waiting periods, habitual-residence tests, and benefit exclusions in the receiving state. Their formal right to move is real; their right to draw on the welfare system they now live under is deferred or denied. Returning home forfeits the job; staying means bearing the gap uninsured.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers, payer,
    moderate, biographical, constrained, continental).

% Have lived and worked in the receiving state for years, paid taxes and contributions, but remain subject to residual eligibility gaps (e.g., certain non-contributory benefits, family reunification welfare access) that citizens do not face. Naturalization is slow, costly, or structurally blocked; exit means abandoning an established life.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, long_term_resident_noncitizens, payer,
    powerless, biographical, trapped, national).

% Sent by employers in their home state to work temporarily in the receiving state; remain in the home state's social security system by design, but in practice face de facto exclusion from receiving-state protections while doing receiving-state work, with limited capacity to contest terms set by the posting employer.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, posted_workers, payer,
    powerless, immediate, trapped, continental).

% Would argue for equal treatment on the basis of contribution and residence rather than nationality, but bargaining structures are organized nationally; cross-border coordination on welfare eligibility rarely reaches the legislative table where residency tests are set.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_labor_unions, excluded,
    organized, generational, constrained, continental).

% Advocates for the integration-primary reading and periodically litigates residency-test cases before the Court of Justice, but has no unilateral power to override member-state welfare closure absent treaty change or adverse judgment; its voice is present in the discourse but structurally subordinate to legislative sovereignty in this reading.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_commission_free_movement_directorate, excluded,
    institutional, generational, analytical, continental).

% Adjudicate specific cases testing whether a given residency or contribution requirement is proportionate and non-discriminatory. Their rulings can narrow closure authority case by case without eliminating it, producing gradual, contested drift rather than settlement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, constitutional_courts_and_ecj, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries).
narrative_ontology:fixing_cost_class(federation_membership_obligations__member_sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the fiscal sustainability of nationally-financed welfare systems against an open external labor market: without some eligibility gate, a welfare system funded by resident contributions would be exposed to benefit-shopping and to sudden claims volume it did not budget contributions to cover.
% TRANSFER_FUNCTION: Moves welfare-system solvency and domestic-labor-market stability from mobile and newly-arrived workers (who bear waiting periods, exclusions, and uninsured gaps) to the existing national contributor base and domestic workforce (who retain undiluted benefit pools and reduced wage competition).
% ABSENT_VOICES: Cross-border labor unions and the mobile workers themselves rarely sit at the legislature where residency tests are drafted; the EU Commission's integration-primary advocacy is present in Brussels and in court but structurally subordinate to national legislative sovereignty under this reading, so its objections register as external pressure rather than a seat at the table.
% DISAPPEARANCE_RATIONALE: If member-state closure authority disappeared overnight, mobile workers would gain immediate parity welfare access, national welfare ministries would lose their principal sustainability lever, domestic workforces would face faster wage-pool integration, and several national governments would face acute fiscal exposure and likely political backlash — the labor-mobility and welfare-financing landscape across the federation would reorganize substantially.
% FOUNDING_PROBLEM: Free movement of workers was extended across states with radically different welfare financing models and contribution histories; without some closure mechanism, a worker could move purely to access a more generous welfare system without having contributed to it, threatening the solvency and political legitimacy of contribution-financed systems.
% FOUNDING_PROBLEM_CORROBORATION: National welfare ministries and domestic labor organizations attest the problem is still live, citing benefit-tourism concerns and fiscal projections. The EU Commission, cross-border labor unions, and independent migration economists attest that empirical benefit-shopping rates are low relative to the scale of exclusion imposed, and that the closure mechanism now functions more to protect domestic labor-market share than to protect fiscal solvency — this is an outside-the-beneficiary-set corroboration that the founding problem's magnitude has been overstated relative to the arrangement's current function.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 to 0.58) reflecting the tightening of residency and habitual-residence tests across member states over the past three decades as mobility volumes increased — the coordination rationale (protecting contributory solvency) is real but the tests have become more restrictive than the empirical benefit-shopping risk would justify on its own, consistent with a coordination function increasingly layered with protective extraction. Suppression tracks upward similarly (0.40 to 0.62) as enforcement machinery — verification bureaucracies, appeals processes, deportation-adjacent consequences for benefit-dependent noncitizens — has hardened. Theater ratio stays comparatively low and rises modestly (0.15 to 0.28): most of the machinery does real gatekeeping work rather than performing it, though a growing share of procedural requirements (repeated re-verification, redundant documentation) function more as friction than substantive means-testing.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature/ministry seat, this looks like a rope: a genuine, necessary coordination mechanism protecting a contribution-financed system from external claims it did not budget for. From the mobile-worker or long-term-resident seat, the same structure computes as tangled rope or worse: a real coordination function (fiscal sustainability) has been extended well past its necessary scope into de facto protection of domestic labor-market share, producing asymmetric extraction on people who cannot easily exit. The engine should register this divergence directly from the stakeholder power/exit data rather than from either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   National welfare ministries and legislatures are structural beneficiaries and agenda-setters: they design and administer the tests and retain full arbitrage-grade exit (they can tighten or loosen unilaterally). The receiving-state domestic workforce benefits diffusely — undiluted benefit pools, slower wage competition — but has no direct control over the mechanism, hence organized power without agenda-setting. Mobile workers, long-term resident noncitizens, and posted workers are targets: their formal legal right to move is real, but exit from the welfare gap itself is constrained or trapped, because leaving means forfeiting the job or residence that motivated the move in the first place. Posted workers are especially powerless because their formal social-security status (home-state coverage) is used to justify de facto exclusion from receiving-state protections while performing receiving-state work — a directionality worth flagging explicitly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting contribution-financed welfare systems from benefit-shopping by non-contributing mobile claimants) is contested as still live: benefit-tourism rates are empirically low in most well-studied cases, yet residency and habitual-residence tests have tightened rather than relaxed as this became clearer, and administrative machinery has grown more restrictive over the interval even as the underlying fiscal threat has been repeatedly shown to be smaller than initially assumed. This is close to a mandatrophy pattern — the mandate (protect solvency from an outsized benefit-shopping risk) has partially outlived its empirical justification while the enforcement apparatus built to serve it has hardened rather than sunset. It is not fully resolved as mandatrophy because domestic labor-market protection (a distinct, ongoing coordination interest) provides continuing justification independent of the original fiscal-sustainability claim — the classification as tangled_rope rather than snare reflects that a real, live coordination function (domestic labor-market and system stability) persists alongside the extraction, rather than the coordination story being pure cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_capture_boundary,
    'Is member-state welfare closure authority a legitimate expression of national fiscal self-determination, or has it become a captured mechanism protecting domestic labor-market incumbents beyond what fiscal sustainability requires?',
    'Comparative empirical study of actual benefit-shopping rates and net fiscal contribution of mobile workers across member states versus the stringency of residency tests each state applies; a persistent mismatch (low empirical risk, high and rising restriction) would support the capture reading.',
    'If capture is established, the coordination-function claim weakens substantially and the classification would drift from tangled_rope toward snare for the payer stakeholders; if fiscal sustainability risk is genuinely proportionate to the restrictions, the tangled_rope classification with real coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_capture_boundary, empirical, 'Whether closure authority tracks genuine fiscal risk or has drifted into incumbent protection.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is ''free movement conditional on welfare sustainability'' a stable, coherent legal-political settlement, or is it an unstable compromise perpetually re-litigated between the integration_primary and selective_solidarity readings, such that no single reading is actually dominant at any given moment?',
    'Track ECJ and constitutional court case law over time: a stable trend toward one reading''s tests being upheld or struck down would indicate settlement; oscillation would indicate genuine multi-reading contest without resolution.',
    'If unstable, this reading''s claimed_type and metrics describe only a temporary equilibrium point in an ongoing three-way contest, and the sibling readings should be weighted as co-active pressures rather than merely alternative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether member_sovereignty_primary is a settled reading or one pole in continuous contest.').

omega_variable(
    posted_worker_directionality_ambiguity,
    'Do posted workers experience this constraint primarily as home-state beneficiaries (retaining home-state social security while working abroad) or as receiving-state targets (performing receiving-state labor without receiving-state protection)?',
    'Survey posted-worker outcomes on actual benefit utilization and cost-of-living-adjusted welfare access relative to comparable receiving-state workers; if home-state benefits are systematically inadequate for the receiving-state cost environment, the target reading dominates.',
    'Determines whether posted_workers'' directionality should sit nearer full-target (as currently authored) or nearer symmetric, which would materially change effective extraction computed for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(posted_worker_directionality_ambiguity, empirical, 'Ambiguity in posted workers'' true structural position between two welfare systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 5, 0.17).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 15, 0.21).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 25, 0.26).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__member_sovereignty_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the federation_membership_obligations kernel, each with its own ε, beneficiary/victim structure, and classification. member_sovereignty_primary (this story) treats national welfare closure as primary and produces a tangled_rope reading with mobile/resident/posted workers as victims. integration_primary treats free movement as constitutive and would produce a substantially lower ε with mobility rights as the vindicated function and welfare boundaries as the extractive residue. selective_solidarity grounds eligibility in contribution history rather than national closure, producing a different beneficiary set (contributing mobile workers included, non-contributing excluded regardless of nationality) and likely a rope-leaning classification for contributors paired with continued exclusion for non-contributors. The three are linked via affects_constraints because litigation and legislative outcomes in one reading's domain directly pressure the operative equilibrium of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
