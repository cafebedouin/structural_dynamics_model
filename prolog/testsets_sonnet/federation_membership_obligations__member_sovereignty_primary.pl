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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: National Welfare Closure Authority Over Free Movement
 *   domain: political economy / federalism / migration policy / welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the 'member sovereignty primary' reading of the
 *   federation membership obligations kernel: national welfare states retain
 *   closure authority over their own benefit systems, and free movement of
 *   labor is treated as conditional on protecting domestic labor markets and
 *   welfare sustainability, not as an unconditional citizenship right. This
 *   is a deliberately narrow claim — it does not describe the sibling
 *   readings (integration_primary, which treats mobility as constitutive of
 *   citizenship and subordinates welfare boundaries to it; or
 *   selective_solidarity, which ties access to contribution history rather
 *   than either citizenship or national closure). Each reading is a
 *   structurally distinct constraint with its own beneficiary/victim set and
 *   its own epsilon; they are linked here only through network edges and the
 *   omega variables below, not folded together.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.52).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "National Welfare Closure Authority Over Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political economy / federalism / migration policy / welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '358e4ddb-d4ee-481b-af36-300950c51f33').
narrative_ontology:cs_kernel_codification('358e4ddb-d4ee-481b-af36-300950c51f33', distributed).
narrative_ontology:cs_authority_grounding('358e4ddb-d4ee-481b-af36-300950c51f33', distributed).
narrative_ontology:cs_reading_relation('358e4ddb-d4ee-481b-af36-300950c51f33', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('358e4ddb-d4ee-481b-af36-300950c51f33', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('358e4ddb-d4ee-481b-af36-300950c51f33', foundational, national_democratic_control_of_redistribution_is_primary).
narrative_ontology:cs_axiom_status(national_democratic_control_of_redistribution_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('358e4ddb-d4ee-481b-af36-300950c51f33', national_democratic_control_of_redistribution_is_primary, deontological).
narrative_ontology:cs_axiom('358e4ddb-d4ee-481b-af36-300950c51f33', secondary, welfare_system_fiscal_sustainability_justifies_conditional_access).
narrative_ontology:cs_axiom_status(welfare_system_fiscal_sustainability_justifies_conditional_access, holdable).
narrative_ontology:cs_axiom_grounding('358e4ddb-d4ee-481b-af36-300950c51f33', welfare_system_fiscal_sustainability_justifies_conditional_access, instrumental).
narrative_ontology:cs_reference_frame('358e4ddb-d4ee-481b-af36-300950c51f33', westphalian_welfare_closure_baseline).
narrative_ontology:cs_drift_state('358e4ddb-d4ee-481b-af36-300950c51f33', post_2004_enlargement_mobility_surge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('358e4ddb-d4ee-481b-af36-300950c51f33', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_national_workforce).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, posted_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, long_term_resident_noncitizens).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, national_democratic_control_of_redistribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain statutory authority to define welfare eligibility criteria, residency thresholds, and habitual-residence tests. They can and do amend qualification rules in response to domestic political pressure, effectively setting the terms under which mobile workers access unemployment benefit, child benefit, or social housing. They answer to a national electorate, not to mobile workers.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Administer means tests, habitual residence tests, and contribution-record checks that determine which mobile workers clear the bar for full benefit access. They preserve fiscal headroom and political cover by keeping the welfare pool closed to newly arrived workers, and they can tighten administrative thresholds without new legislation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, national_welfare_ministries, beneficiary).

% Benefit from a welfare and labor market pool that is not immediately diluted by newly arrived workers, and from wage floors that closure authority is invoked to protect. They experience the constraint as legitimate self-defense of a contributory system they and their predecessors funded over decades.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_national_workforce, beneficiary,
    organized, biographical, constrained, national).

% Move under free movement rights but face waiting periods, habitual residence tests, and benefit restrictions in the receiving state despite paying taxes and social contributions there. They can return home or move to a third state, but each move resets residency clocks and severs accumulating entitlement, so the practical exit is costly and repeated.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers, payer,
    moderate, biographical, constrained, continental).

% Are sent by employers registered in a lower-cost member state to work temporarily in a higher-cost state, remaining formally covered by their home welfare system while working under host-state labor conditions. They have essentially no individual leverage over which system's protections apply to them and cannot access the receiving state's welfare floor even while physically present and working there.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, posted_workers, payer,
    powerless, immediate, trapped, continental).

% Have lived and contributed in the receiving state for years but remain subject to residual eligibility gaps (family benefit exclusions, delayed access to certain means-tested transfers) that closure-authority rules never fully phase out. Their exit would mean abandoning an established life, job, and social network built over years.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, long_term_resident_noncitizens, payer,
    powerless, biographical, trapped, national).

% Lose working-age contributors and tax base to outward mobility while receiving states retain closure authority over the welfare systems those same workers pay into abroad. They have no formal seat in the receiving state's eligibility-setting process and can only negotiate at the treaty level, which moves far slower than domestic welfare administration.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, sending_state_governments, excluded,
    institutional, generational, analytical, national).

% Adjudicates disputes between free-movement treaty provisions and national welfare closure rules on a case-by-case basis, periodically narrowing or widening the scope member states have to restrict access. Its rulings shift the boundary but do not eliminate the underlying sovereignty claim; member states retool eligibility rules to remain compliant while preserving closure in substance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_court_of_justice, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__member_sovereignty_primary, european_court_of_justice, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows each member state's welfare system to remain fiscally and politically sustainable by controlling the rate at which newly arrived workers become full claimants, preventing sudden, unplanned strain on contributory insurance pools and means-tested transfer programs that were designed and funded around a more closed national population.
% TRANSFER_FUNCTION: Moves the fiscal risk of open welfare access away from national treasuries and onto mobile workers and posted workers, who contribute (via taxation and social charges) to systems from which they are partially or temporarily excluded; correspondingly protects incumbent national workforce access to a welfare pool and labor market that would otherwise be shared immediately and fully with new arrivals.
% ABSENT_VOICES: Mobile workers themselves have no direct vote in the legislatures setting eligibility rules in the state where they work; sending-state governments, who lose contributors and gain remittance dependency, are not parties to the receiving state's domestic welfare-eligibility debates at all.
% DISAPPEARANCE_RATIONALE: If member-state closure authority disappeared overnight, mobile and posted workers would gain immediate full access to host-state welfare systems, materially altering redistribution flows, national welfare budgets, and electoral politics in receiving states; national legislatures would lose a lever they currently use to manage both fiscal exposure and anti-immigration sentiment.
% FOUNDING_PROBLEM: Free movement was designed to build a single labor market, but welfare states were built and funded as closed national contributory systems; the founding problem was preventing 'welfare tourism' and unplanned fiscal exposure from undermining domestic political support for both the welfare state and free movement itself.
% FOUNDING_PROBLEM_CORROBORATION: National governments and their electorates attest the founding problem (unplanned fiscal exposure, benefit-shopping) remains live, citing periodic spikes in cross-border claims. Independent EU Commission and academic labor-mobility studies attest that measured welfare tourism is empirically small relative to net fiscal contribution mobile workers make, suggesting the closure apparatus now serves a domestic political function beyond its original fiscal-protection rationale — corroboration is genuinely split between insider and outside-of-benefiting-party sources.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate-high (0.58) because mobile and posted workers pay into systems whose benefits they access only partially or after delay, while receiving-state incumbents and administrations capture the fiscal and political benefit of that delay. Suppression is moderate (0.52): the mechanism operates mostly through administrative eligibility tests and residency clocks rather than overt coercion, but resetting those clocks on every cross-border move functions as a real structural barrier. Theater ratio is low-moderate and rising (0.12 to 0.28) as habitual-residence tests and means-testing bureaucracy expand faster than any genuine fiscal-sustainability need, suggesting some accumulating administrative overhead beyond the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the member-state legislature's seat this looks like a genuine, democratically legitimate coordination mechanism protecting a contributory system built by and for a national population. From a posted worker's seat the same rule structure looks like extraction: labor and tax contribution flow into a system that returns diminished or delayed benefit. The engine should compute these as different seat-level classifications from the same structural facts rather than requiring either seat's framing to be declared 'correct' at the story level.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state legislatures and welfare ministries are the agenda-setters: they write and administer the eligibility rules and bear no direct extraction themselves (d near the beneficiary end). Receiving-state national workforce benefits from a protected labor pool and undiluted welfare fund (d near beneficiary). Mobile workers and especially posted workers and long-term resident noncitizens sit at the target end: they contribute fiscally but face conditional, delayed, or partial access, and their exit options (returning home, relocating again) impose real costs that reset accumulated entitlement rather than eliminating the underlying asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting fiscal sustainability against sudden unplanned claims) is empirically weaker today than when the closure regime was built, per outside academic and Commission analysis, yet the eligibility apparatus has not been correspondingly relaxed and its administrative complexity (theater_ratio rising from 0.12 to 0.28) has grown. This is not treated as full mandatrophy resolution here because the founding_problem_status is genuinely contested rather than settled dead — national governments still credibly invoke live fiscal exposure risk, particularly amid periodic migration surges, so a Tangled Rope classification (real coordination function plus asymmetric extraction) is more accurate than either a pure Rope (would ignore the victim set) or pure Snare (would ignore the genuine fiscal coordination problem the mechanism still partially addresses) reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_sovereignty_vs_integration,
    'Is the federation membership obligations kernel correctly read as member-sovereignty-primary (this story), integration-primary, or selective-solidarity? The treaty texts and ECJ case law support elements of all three readings depending on which provisions and which era of jurisprudence are emphasized.',
    'No single resolution mechanism exists because this is a genuine committer-level contest, not an empirical question resolvable by new data. It is adjudicated politically and judicially in an ongoing, unsettled way: further ECJ rulings, treaty amendments, or a sustained political shift (e.g. toward stronger union-level welfare coordination or toward renationalization) would shift which reading is dominant in practice.',
    'If integration_primary became structurally dominant, the beneficiary/victim structure here would reverse or dissolve: mobile workers would move from victim to full beneficiary status and member-state closure authority would itself become the constrained/extractive party from the union''s perspective. If selective_solidarity became dominant, the beneficiary set would be redrawn around contribution history rather than national citizenship, cutting across both this reading''s and the integration reading''s boundary lines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_sovereignty_vs_integration, conceptual, 'Which of three live kernel readings correctly characterizes the operative EU free-movement/welfare-closure regime is structurally undetermined and contested by design.').

omega_variable(
    fiscal_exposure_empirical_magnitude,
    'Is the fiscal exposure that closure authority protects against empirically large enough to justify the current administrative eligibility apparatus, or has the apparatus outgrown the risk it was built to manage?',
    'Comparative fiscal studies tracking net contribution-to-claim ratios of mobile workers across member states over multi-year periods, cross-checked against Commission mobility reports and independent academic labor economics research (outside both national ministries and mobile-worker advocacy groups).',
    'If exposure is empirically small and stable, the closure apparatus functions increasingly as protectionist rent extraction dressed as fiscal prudence, pushing the classification toward snare; if exposure is real and rising (e.g. under demographic or crisis-driven migration surges), the coordination function is substantive and the tangled_rope reading with genuine dual function is more strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_exposure_empirical_magnitude, empirical, 'Whether the underlying fiscal-sustainability risk the closure regime targets is empirically substantial or diminished relative to when the regime was designed.').

omega_variable(
    eu_citizenship_beneficiary_construction,
    'Is EU citizenship best modeled as a natural/constitutive status (which would favor treating welfare-closure boundaries as constructed impositions on a pre-existing entitlement) or as a treaty-derived, contingent status whose scope member states legitimately continue to negotiate?',
    'Doctrinal and constitutional analysis of whether EU citizenship rights are self-executing and supranational in character versus derivative of and revisable through member-state treaty consent; tracked through evolving ECJ jurisprudence on the direct effect of citizenship provisions.',
    'The framing chosen here treats welfare closure as a legitimate exercise of retained sovereignty, not as suppression of a prior natural entitlement — but if EU citizenship is better modeled as constitutive and pre-political, the entire beneficiary/victim structure of this reading would need re-examination, since ''closure authority'' would then look more like an exception carved out of a baseline right rather than the baseline itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_citizenship_beneficiary_construction, conceptual, 'Whether EU citizenship is the baseline (making closure the exception) or member-state consent is the baseline (making closure the default) is a framing choice this reading resolves in favor of the latter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t4, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 4, 0.15).
narrative_ontology:measurement(fede_tr_t8, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 12, 0.21).
narrative_ontology:measurement(fede_tr_t16, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 16, 0.24).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t4, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(fede_be_t8, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(fede_be_t16, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(fede_su_t4, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(fede_su_t8, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(fede_su_t16, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This file, federation_membership_obligations__integration_primary, and federation_membership_obligations__selective_solidarity are three siblings decomposing the single natural-language label 'EU free movement vs. welfare sovereignty' into three structurally distinct constraints, each with its own epsilon, beneficiary/victim set, and claimed type, per the epsilon-invariance principle. They are not the same constraint measured three ways; they are three different institutional configurations that could each be the operative reality depending on which legal, political, and doctrinal currents dominate at a given time. Network edges here are declared bidirectionally in intent (each sibling should list the others) to reflect that the kernel contest is genuinely unresolved and any reading's dominance structurally pressures the others' legitimacy and resource base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
