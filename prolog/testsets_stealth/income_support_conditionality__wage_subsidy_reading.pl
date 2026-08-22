% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Payment as Employer Wage Subsidy (Wage-Subsidy Reading)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   A national scheme pays every resident an unconditional monthly sum
 *   financed from general taxation, replacing a patchwork of conditional
 *   benefits. This story authors the arrangement as the wage-subsidy reading
 *   sees it: in low-wage labor markets the payment operates as a subsidy to
 *   employers, because a subsistence floor delivered outside the wage bargain
 *   lets firms hold cash wages flat while total household income stays above
 *   destitution. Part of every payment is thereby recaptured as suppressed
 *   wage growth; the remainder reaches workers as real net income, and
 *   recipients outside the labor market keep all of it. The result is a
 *   genuine coordination good — a poverty floor, simplified administration,
 *   stabilized consumption — with an asymmetric benefit distribution layered
 *   through the wage bargain. Per the epsilon-referent rule, extractiveness
 *   is authored for the standing arrangement as this reading assesses it,
 *   never for the emancipatory arrangement its designers advertised. The
 *   claimed type and the metrics are independent authored facts: the claim
 *   states this reading's structural verdict; the metrics state what the
 *   arrangement's operation looks like in the wage data. KEY AGENTS (by
 *   structural relationship): - low_wage_employers: primary beneficiary
 *   (organized/arbitrage) — recapture a share of every payment through
 *   flattened wage growth - low_wage_workers: primary target
 *   (powerless/constrained) — nominally receive the payment, effectively fund
 *   employer gains through suppressed cash wages - general_taxpayers: fiscal
 *   payer (organized/constrained) — fund the transfer with no seat in the
 *   wage bargains that dispose of it - administering_ministry: agenda_setter
 *   (institutional/constrained) — sets payment level, runs disbursement,
 *   publishes the official record - nonemployed_recipients: residual genuine
 *   beneficiary (powerless/trapped) — receive the floor with nothing
 *   recaptured - labor_unions: excluded voice (organized/constrained) — would
 *   tie the floor to bargaining protections; outside the design coalition -
 *   labor_economists: analytical observer (analytical/analytical) — measure
 *   recapture incidence across rollout boundaries
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.66).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.58).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Payment as Employer Wage Subsidy (Wage-Subsidy Reading)").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "economic/political/social").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '247aa1d0-c242-4585-a914-866ad729433a').
narrative_ontology:cs_kernel_codification('247aa1d0-c242-4585-a914-866ad729433a', formalized).
narrative_ontology:cs_authority_grounding('247aa1d0-c242-4585-a914-866ad729433a', distributed).
narrative_ontology:cs_reading_relation('247aa1d0-c242-4585-a914-866ad729433a', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('247aa1d0-c242-4585-a914-866ad729433a', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('247aa1d0-c242-4585-a914-866ad729433a', foundational, transfer_capture_by_employers_is_structural).
narrative_ontology:cs_axiom_status(transfer_capture_by_employers_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('247aa1d0-c242-4585-a914-866ad729433a', transfer_capture_by_employers_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('247aa1d0-c242-4585-a914-866ad729433a', secondary, transfers_alone_insufficient_for_noncoercive_subsistence).
narrative_ontology:cs_axiom_status(transfers_alone_insufficient_for_noncoercive_subsistence, holdable).
narrative_ontology:cs_axiom_grounding('247aa1d0-c242-4585-a914-866ad729433a', transfers_alone_insufficient_for_noncoercive_subsistence, instrumental).
narrative_ontology:cs_reference_frame('247aa1d0-c242-4585-a914-866ad729433a', income_floor_within_wage_relation).
narrative_ontology:cs_drift_state('247aa1d0-c242-4585-a914-866ad729433a', post_rollout_wage_stagnation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('247aa1d0-c242-4585-a914-866ad729433a', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, nonemployed_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate retail, hospitality, care, and logistics businesses employing large numbers of low-wage staff. Since the payment began they have held starting wages nearly flat while turnover eased, because applicants can cover rent and food from the payment plus a lower wage. Payroll costs grow more slowly than before the scheme, and industry associations lobby to keep the payment at current levels and to resist wage-indexation proposals. Exit is easy: locations can be relocated, roles automated, or work restructured into contracts.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Work hourly jobs in retail, care, hospitality, and warehousing. Each month they receive the payment, and each month their cash wage buys less than it used to: raises have lagged prices since the scheme started, and employers cite the payment when declining requests for more. Switching employers rarely helps because the flat-wage pattern spans the sector; leaving wage work entirely is not viable because the payment alone does not cover rent in most cities. They are simultaneously the named recipients of the transfer and the channel through which part of it returns to employers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, low_wage_workers, beneficiary).

% Fund the payment through income and consumption taxes. Most earn above the range where the payment is a net gain, so they finance transfers whose labor-market disposition they neither control nor directly observe. They can vote on the scheme's size but hold no seat in the wage bargains that determine where the money ends up.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, general_taxpayers, payer,
    organized, biographical, constrained, national).

% Sets the payment level annually within budget ceilings, runs enrollment and disbursement, and publishes the poverty and employment statistics credited to the scheme. Its evaluations track recipient incomes but not the wage trajectories of covered sectors, so the official record shows the floor working while remaining silent on wage adjustment. Statutory mandates and coalition agreements bind it to the scheme's current design.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, administering_ministry, agenda_setter,
    institutional, generational, constrained, national).

% People outside the labor market — disabled residents, unpaid caregivers, those between long spells of work — receive the payment in full with nothing recaptured, because no employer stands opposite them. For them the payment is the difference between managed poverty and destitution, and household budgets depend on its continuation.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, nonemployed_recipients, beneficiary,
    powerless, immediate, trapped, national).

% Organize the same workplaces the payment covers and campaigned for wage-indexation clauses and sectoral bargaining alongside any income floor. Their proposals were left out of the design legislation, and organizers report that members credit the payment for security while accepting frozen wages — a trade the union regards as made over its head. Bargaining coverage in the affected sectors continues to shrink.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_unions, excluded,
    organized, biographical, constrained, national).

% Run the evaluation studies: difference-in-differences wage analyses across rollout boundaries, reservation-wage surveys, and incidence modeling. Their working papers estimate how much of each payment unit surfaces as employer payroll savings versus recipient net income, and they advise ministries that cite their poverty findings more often than their wage findings.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides every resident an unconditional monthly income floor: consumption stabilizes, destitution falls, and a patchwork of conditional benefits collapses into one payment. Workers can survive gaps between jobs without losing housing or food security.
% TRANSFER_FUNCTION: Moves tax revenue to all residents as equal unconditional payments. In low-wage labor markets, part of each payment is recaptured by employers through flatter wage growth, so the effective flow runs from taxpayers through worker households to employers of low-wage labor, with workers retaining the remainder as net income.
% ABSENT_VOICES: Labor unions and collective-bargaining advocates who would tie any income floor to wage-setting protections are outside the policy coalition, as are future cohorts who will carry the fiscal cost. Their objection — that a floor without bargaining power converts public money into wage-bill relief — is raised in commentary and committee testimony but holds no seat in the design process.
% DISAPPEARANCE_RATIONALE: Millions of households budget around the payment; removing it overnight would push working-poor households below subsistence before any wage response could materialize, trigger emergency reinstatement politics, and force employers to raise cash wages or lose staff. Employer pay structures and public finances are both organized around the payment's existence.
% FOUNDING_PROBLEM: Destitution and coercive dependence on any single employer: before the scheme, losing a job meant immediate poverty, and conditional benefits penalized part-time or intermittent work, trapping people in bureaucratic eligibility regimes.
% FOUNDING_PROBLEM_CORROBORATION: National statistics offices and food-bank networks continue to record destitution among people the payment misses or only partly reaches, corroborating that the underlying problem persists; labor-market research documenting flat real wages at the bottom corroborates the recapture dynamic from outside the benefiting parties. No source inside the employer community disputes that destitution predates the scheme.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: substantial but bounded, because the floor function is real and recapture is partial — workers keep part of every payment as net income while the rest returns to employers through flattened wage growth. Suppression is authored at 0.58 as a raw structural property (the engine scales only extractiveness, by directionality and scope): the payment is calibrated below the level at which a worker could refuse employment outright, wage-indexation proposals are repeatedly deferred, and sectoral bargaining is unavailable in most covered industries — yet job-switching and geographic mobility remain partially open, so alternatives are narrowed rather than closed. Theater_ratio at 0.36 reflects an emancipation framing that persists in official communication while bottom-decile cash wages stagnate; the delivery machinery itself functions. Accessibility_collapse at 0.45: alternative designs (a payment sized above the recapture threshold, wage-indexed transfers, bargaining-rights riders) remain legible and periodically proposed. Resistance at 0.38: union campaigns and living-wage movements contest the wage pattern, but the recapture mechanism is diffuse and hard to target, blunting mobilization. The temporal series run on one shared grid (t=0..24 in years since national rollout) so every metric is authored at every examined point: rising base_extractiveness models progressive recapture as employers complete wage adjustment; rising theater_ratio models the widening gap between the liberation framing and wage outcomes; rising suppression_requirement models the growing effort needed to hold the payment below the refusal threshold and to defer indexation. The claimed type (tangled_rope) and these metric values were authored independently — the claim records this reading's structural verdict, the metrics record the arrangement's observed operation, and any divergence between them is measurement, not error.
 *
 * PERSPECTIVAL GAP:
 *   From the employer seat the arrangement reads as stable, flexible staffing: predictable applicants, low turnover, contained payroll growth — a well-functioning piece of coordination it did not have to build. From the worker seat the same arrangement reads as a pay settlement reached over workers' heads: the headline benefit arrives while the raise never does. The ministry seat sees falling poverty statistics and concludes the design works; the excluded union seat sees the bargain that produced those statistics. Same-level dynamics sharpen the divergence: two identical warehouse workers differ only by region — where a rival employer competes for staff, wages move; where a single dominant employer sets the local rate, the payment is absorbed almost entirely. Coalition capacity is the pivot: workers who organize can convert the floor into bargaining leverage, which is precisely why the excluded seat's absence is load-bearing in this arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map cleanly onto the seats, so no directionality overrides are used. Employers are declared beneficiaries with arbitrage-grade exit: the derivation places them near the full-beneficiary end, and the engine's arithmetic renders their effective extraction negative — the arrangement subsidizes them, which is this reading's central claim. Workers are declared victims with constrained exit: they sit near the full-target end, and the fact that they also cash the payment does not move them far, because the recapture runs through that same payment. Taxpayers are payers with constrained exit — high directionality, diffuse per-head cost. Nonemployed recipients are beneficiaries with no exit at all, yet they retain the full payment: low directionality, genuine subsidy. An override keyed to a power atom would misfire here, since workers and nonemployed recipients share the powerless atom while sitting at opposite ends; the declaration-plus-exit derivation separates them correctly without intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — destitution and dependence on any single employer — is still live, so the arrangement is not mandate-expired and no sunset applies. The classification discipline cuts both ways: reading the arrangement as pure extraction would erase the floor that nonemployed recipients genuinely live on; reading it as pure coordination would erase the recapture channel that flattens bottom-decile wages. The hybrid category holds both facts in one structure: a real coordination function carrying an asymmetric beneficiary set enforced through the wage bargain. The status-by-verdict pair (live, world_rearranges) raises no obsolescence flag; the open question is not whether the mandate expired but whether the benefit distribution has quietly inverted while the mandate language stayed fixed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Is employer wage-recapture the dominant characterization of the unconditional payment''s labor-market operation, as this reading holds, or do one of the sibling readings'' mechanisms dominate?',
    'Longitudinal wage-distribution studies comparing rollout regions against matched controls, partitioned by wage decile and by sectoral labor-market concentration.',
    'Resolution toward the freedom_floor reading would remove employers from the beneficiary set and cut measured extraction sharply; resolution toward the dependency_trap reading would add skill-atrophied labor-market leavers as victims and raise suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, empirical, 'Which sibling reading''s mechanism dominates the arrangement''s labor-market operation.').

omega_variable(
    capture_share_magnitude,
    'What fraction of the marginal payment is recaptured by employers through wage adjustment rather than retained by recipients as net income?',
    'Difference-in-differences on wage growth across staggered rollout boundaries, combined with reservation-wage panel surveys.',
    'Recapture above roughly 60% pushes the arrangement toward pure extraction with a residual floor; below 20% the arrangement behaves as a worker subsidy with incidental employer benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_share_magnitude, empirical, 'Magnitude of the employer recapture share of each payment unit.').

omega_variable(
    counterfactual_wage_attribution,
    'Are flat bottom-decile wages caused by the payment, or would monopsony power and weak productivity growth have held wages down regardless?',
    'Natural experiments where payment pilots terminated abruptly; comparison of wage trajectories in covered versus uncovered low-wage occupations.',
    'If wages were flat on the pre-existing trend, the payment is not the operative mechanism and this reading''s extractiveness is overstated; if wages fell relative to trend after rollout, recapture is confirmed and possibly understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_wage_attribution, empirical, 'Causal attribution of wage stagnation to the payment versus background labor-market structure.').

omega_variable(
    design_portability_ambiguity,
    'Do recapture findings from conditional wage-top-up credits generalize to a fully unconditional payment design?',
    'Head-to-head pilots varying conditionality while holding payment size constant, with wage outcomes as the primary endpoint.',
    'If conditionality drives recapture, an unconditional design weakens this reading''s beneficiary claims; if recapture persists unconditionally, the reading strengthens and the beneficiary set is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(design_portability_ambiguity, conceptual, 'Whether capture evidence ports across conditional and unconditional designs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wage_subsidy_reading_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(wage_subsidy_reading_tr_t4, income_support_conditionality__wage_subsidy_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(wage_subsidy_reading_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(wage_subsidy_reading_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(wage_subsidy_reading_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(wage_subsidy_reading_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(wage_subsidy_reading_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.36).

% Extraction over time
narrative_ontology:measurement(wage_subsidy_reading_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(wage_subsidy_reading_be_t4, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(wage_subsidy_reading_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(wage_subsidy_reading_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(wage_subsidy_reading_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(wage_subsidy_reading_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(wage_subsidy_reading_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(wage_subsidy_reading_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(wage_subsidy_reading_su_t4, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(wage_subsidy_reading_su_t8, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(wage_subsidy_reading_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(wage_subsidy_reading_su_t16, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(wage_subsidy_reading_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(wage_subsidy_reading_su_t24, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% The colloquial label for unconditional income support conflates three structurally distinct claims about one arrangement (kernel income_support_conditionality): that it decommodifies labor (freedom_floor_reading), that it erodes work incentives (dependency_trap_reading), and that it subsidizes employers through wage recapture (this file). Each reading carries its own epsilon, beneficiary/victim structure, and classification over the same referent; they form a constraint family linked here. This reading sits between its siblings in extraction: higher than a freedom_floor account (which finds negligible extraction), lower than a dependency_trap account that additionally counts forgone output as a cost.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
