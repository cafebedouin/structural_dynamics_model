% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Universal Dividend Swap — Targeting-Efficiency Reading
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the targeting-efficiency reading of the
 *   income_support_commitment kernel: the claim that income support should be
 *   concentrated on demonstrated need rather than universally distributed.
 *   The arrangement under contest is the universal-dividend swap — replacing
 *   stacked means-tested programs (SNAP, housing vouchers, childcare
 *   subsidies, the EITC, SSI wraparounds) with a single uniform adult payment
 *   financed by eliminating them. Read through this reading's own lights, the
 *   exemplar arithmetic is a Queens parent assembling $31,100 a year from
 *   layered benefits who is converted into a $12,000 check recipient: a
 *   $19,100 annual loss imposed precisely on households with verified need,
 *   while the freed fiscal space spreads as identical payments to every
 *   adult, including those with no need at all. Universality is the
 *   arrangement's public face; the offset list is its operating mechanism.
 *   Per the kernel-reading epsilon-referent rule, epsilon is authored for the
 *   swap arrangement as this reading assesses it — not for the targeted
 *   system this reading endorses, which is the endorsed alternative and
 *   belongs in its own near-zero-epsilon story. Family links to the two
 *   sibling readings are declared in network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ubi_swap_legislators: agenda-setting sponsor of the replacement statute (institutional/arbitrage) — chooses the offset list, dividend level, and financing
 *   - benefit_stacked_poor_households: primary target (powerless/trapped) — bears the $19,100 annual conversion loss
 *   - disabled_benefit_recipients: target (powerless/trapped) — differential-need benefits folded into a flat payment
 *   - large_families_in_deep_poverty: target (powerless/trapped) — steepest per-household cut via per-child benefit loss
 *   - middle_income_dividend_recipients: primary beneficiary (organized/mobile) — net collector of the redistributed flow
 *   - childless_prime_age_adults: beneficiary (moderate/mobile) — largest proportional gain, nothing of their own to lose
 *   - fiscal_simplification_advocates: beneficiary (institutional/arbitrage) — collects program consolidation, indifferent to incidence
 *   - disability_advocacy_organizations: excluded voice (organized/constrained) — objects from outside the drafting room
 *   - distributional_policy_analysts: analytical observer (institutional/analytical) — publishes the incidence tables
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.78).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.62).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Universal Dividend Swap — Targeting-Efficiency Reading").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'b7a33917-d2b1-41b1-986d-0d248454d7e4').
narrative_ontology:cs_kernel_codification('b7a33917-d2b1-41b1-986d-0d248454d7e4', distributed).
narrative_ontology:cs_authority_grounding('b7a33917-d2b1-41b1-986d-0d248454d7e4', distributed).
narrative_ontology:cs_reading_relation('b7a33917-d2b1-41b1-986d-0d248454d7e4', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7a33917-d2b1-41b1-986d-0d248454d7e4', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('b7a33917-d2b1-41b1-986d-0d248454d7e4', foundational, marginal_dollar_goes_to_demonstrated_need).
narrative_ontology:cs_axiom_status(marginal_dollar_goes_to_demonstrated_need, holdable).
narrative_ontology:cs_axiom_grounding('b7a33917-d2b1-41b1-986d-0d248454d7e4', marginal_dollar_goes_to_demonstrated_need, instrumental).
narrative_ontology:cs_axiom('b7a33917-d2b1-41b1-986d-0d248454d7e4', foundational, means_testing_cost_below_depth_gain).
narrative_ontology:cs_axiom_status(means_testing_cost_below_depth_gain, holdable).
narrative_ontology:cs_axiom_grounding('b7a33917-d2b1-41b1-986d-0d248454d7e4', means_testing_cost_below_depth_gain, empirically_contingent).
narrative_ontology:cs_reference_frame('b7a33917-d2b1-41b1-986d-0d248454d7e4', demonstrated_need_distribution).
narrative_ontology:cs_drift_state('b7a33917-d2b1-41b1-986d-0d248454d7e4', contemporary_ubi_mainstreaming_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b7a33917-d2b1-41b1-986d-0d248454d7e4', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, middle_income_dividend_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, childless_prime_age_adults).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, fiscal_simplification_advocates).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, benefit_stacked_poor_households).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, disabled_benefit_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, large_families_in_deep_poverty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, benefit_stacked_poor_households).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, middle_income_dividend_recipients).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, uniform_payment_delivery_feasibility).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, consolidation_self_financing_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sponsor and negotiate the replacement statute: they select which means-tested programs appear on the consolidation list, set the dividend level, and choose the financing. Their districts contain both net losers and net winners, but their own households sit in the net-winner column; if the coalition fails they resume ordinary careers having risked nothing material.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_swap_legislators, agenda_setter,
    institutional, biographical, arbitrage, national).

% Work multiple part-time jobs in a high-rent borough and assemble roughly $31,100 a year from SNAP, a housing voucher, a childcare subsidy, the EITC, and Medicaid. Each benefit was applied for separately and each has its own renewal gauntlet. Under the swap the stack folds into a single $12,000 adult payment; the rent and the childcare invoice do not shrink to match. There is no way to decline the trade or keep the stack.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, benefit_stacked_poor_households, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, benefit_stacked_poor_households, beneficiary).

% Receive SSI and Medicaid sized to verified extra costs of disability — transport to specialists, home care, dietary needs. A flat adult payment ignores those cost differences, and the medical categoricals are the slowest benefits to rebuild once dissolved. Their eligibility rests on assessments they cannot perform on themselves.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, disabled_benefit_recipients, payer,
    powerless, biographical, trapped, national).

% Household benefit value scales with each child — SNAP allotments, the child tax credit, WIC, school meals. A per-adult flat payment cuts their total more steeply than any other household type, and the children absorbing the cut have no vote and no seat in the negotiation.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, large_families_in_deep_poverty, payer,
    powerless, immediate, trapped, national).

% Earn above every relevant eligibility line today, so the swap converts them from outside the system to full claimants overnight. They contribute part of the financing and still net ahead. Their support is the coalition's center of gravity, and they can withdraw it within a single election cycle if the deal turns sour.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, middle_income_dividend_recipients, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, middle_income_dividend_recipients, payer).

% Currently qualify for almost nothing — minimal EITC, SNAP access barred or trivial in many states, no child credit. The swap hands them the full payment they were previously ineligible for, the largest proportional gain of any group, with no benefit of their own to lose.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, childless_prime_age_adults, beneficiary,
    moderate, biographical, mobile, national).

% Think-tank and fiscal-commission staff who have spent a decade cataloguing overlap and error across dozens of means-tested programs. What they collect from the swap is architectural: fewer programs, cleaner budget lines, a single payment rail. Their commitment survives any distributional outcome; the dividend level is negotiable, the consolidation is not.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_simplification_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Have testified in every hearing on benefit consolidation for thirty years, arguing that flat payments erase differential need. They are consulted late, their amendments die in committee, and their opposition gets filed as special-interest pleading against simplicity.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, disability_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Run the microsimulation tables showing net winners and losers by income decile and household type, including the $31,100-to-$12,000 comparison for stacked-benefit households. They publish, they testify, and they have no vote on the offset list.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, distributional_policy_analysts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, middle_income_dividend_recipients).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single universal payment rail replaces dozens of means-tested programs: one application, one deposit, no eligibility interviews, no renewal cycles, no stigma interface — solving fragmentation, non-take-up, and administrative overhead in benefit delivery, stated without evaluation of who nets what.
% TRANSFER_FUNCTION: Moves approximately $19,100 per year from each deeply benefit-dependent household (by folding its $31,100 stack into a $12,000 payment) into the pool that funds uniform payments to all adults — a net flow from the highest-need households to the broad adult population, supplemented by consolidated revenue.
% ABSENT_VOICES: Disability advocates and the contingently future needy (households one job loss away from requiring deep benefits) are outside the drafting room; the negotiation's arithmetic is built around the modal adult recipient, and the seats whose losses scale with household size or medical need enter only as testimony. Frontline caseworkers facing program dissolution also speak nowhere in the design process.
% DISAPPEARANCE_RATIONALE: If the swap vanished overnight, the targeted programs continue, the exemplar household keeps its $31,100 stack, the middle-income and childless cohorts never become claimants, and the fiscal-simplification agenda loses its vehicle — the universalist coalition would have to rebuild its case from scratch, and the anti-consolidation coalition demobilizes.
% FOUNDING_PROBLEM: Means-tested income support had grown into a fragmented maze: overlapping programs with conflicting eligibility lines, take-up rates far below eligibility, punitive renewal burdens, and cliff effects that penalize earnings — the swap was built to deliver support simply, universally, and without surveillance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: administrative-burden scholarship (take-up and compliance-cost studies) and GAO/CBO program-overlap reporting attest that the fragmentation problem is real and live; poverty researchers outside the universalist coalition attest that the swap's remedy fails the problem it cites, since it dissolves the benefit depth that the fragmentation critique presupposes. No party disputes that fragmentation exists; the parties dispute whether the swap answers it.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the swap's defining operation converts the deepest, verified-need benefit streams into a flat payment worth $19,100 less to the exemplar household, and the freed funds flow to adults with no demonstrated need; the more verified need a household has, the larger its loss — the inverse of any need-tracking formula. Suppression (0.62) is structural: once enacted, the benefit categories cease to exist, so the affected households' alternative — keeping what they had — is abolished by the same instrument, and no household-level exit exists. Theater (0.42): delivery simplification and de-stigmatization are real functions, but a growing share of the arrangement's public justification ('everyone is in, no one is singled out') performs solidarity while the incidence table runs the other way. Accessibility_collapse (0.40) is low-moderate: repeal, supplementation, and hybrid redesign remain politically available, so alternatives narrow without vanishing. Resistance (0.68) is high and organized: disability-rights and anti-poverty coalitions treat benefit consolidation as existential, which is why the offset list, not the dividend level, is where the legislative battles occur. Both temporal series share one six-point grid (2015-2025); suppression_requirement is deliberately not tracked because the arrangement's enforcement machinery is prospective (pre-enactment), and the static scalar already carries that picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same fiscal facts. From the benefit-stacked household's position the swap is the confiscation of an assembled survival package it cannot decline; from the middle-income recipient's position it is a dividend it finally qualifies for; from the sponsor's position it is administrative modernization; from the fiscal-simplifier's position it is program consolidation whose incidence is someone else's department. The engine computes these divergences from the declared positions and exits; the authored snare claim adjudicates nothing. Coalition note: the three payer seats are individually powerless, but the organized resistance seat (disability advocacy) supplies the coalition capacity the powerless lack — the reason resistance scores high despite powerless targets. Same-power differentiation: the three powerless seats diverge by exit structure — disabled recipients face the least reversible loss (medical categoricals are the slowest benefits to rebuild), large families face the steepest arithmetic (per-child scaling), and the stacked single parent faces the widest gap between the flat payment and fixed urban costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the middle-income recipients and childless prime-age adults (mobile, net-collecting, nothing to lose); victim declarations drive high d for the three payer seats, amplified by trapped exit — a trapped target sits nearer the full-target end than a mobile one. The fiscal simplifiers derive as beneficiaries but their collection is architectural rather than monetary; the derivation is left uncorrected because directionally they still sit at the beneficiary end. National spatial scope scales effective extraction modestly upward for the targets: verifying and servicing need at national-administrative scale is exactly the difficulty the targeting reading exists to manage, and the swap retires the instruments that did the verifying. No directionality overrides were needed: beneficiary/victim declarations plus exit options reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Reading the swap as a rope (simple, dignified, universal coordination) would bury the extraction inside the delivery story — the coordination function is real, but the arrangement's persistence depends on abolishing the weakest seat's exit, which is the snare signature rather than coordination overhead. Reading all income support as extractive (the dependency-trap reflex) would misclassify the targeted system this reading defends, which carries its own story. The R5 interview shows no zombie profile: the founding problem (fragmentation, take-up failure, cliff effects in means-testing) is contested-live, corroborated from outside the benefiting parties by administrative-burden scholarship and take-up research, and the disappearance verdict is world_rearranges — the fiscal coalition, the advocacy landscape, and the exemplar household's budget all reorganize if the swap vanishes. A contested founding problem plus a world-rearranging verdict is a live fight, not a mandate outliving its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the targeting-efficiency reading of the income_support_commitment kernel; which reading governs the arrangement, and what changes structurally if a sibling reading displaces it?',
    'Track adoption history: if the freedom-floor reading prevails legislatively, the victim set empties (no stacked household loses anything) and the same fiscal flow recomputes as a floor provision; if the dependency-trap reading prevails, targeting persists but re-grounded in work requirements, changing the enforcement shape rather than the victim set.',
    'The identical fiscal arithmetic classifies as a snare under this reading and as a rope/scaffold under the freedom-floor reading; displacement of the reading, not new data, drives the flip.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a three-reading kernel; sibling displacement changes the victim set and the computed type.').

omega_variable(
    epsilon_referent_discipline,
    'Is epsilon authored for the arrangement under contest (the universal-dividend swap) rather than for the targeted system this reading endorses?',
    'Referent audit: the scored arrangement is the swap (stacked benefits replaced by a uniform payment); the targeted system this reading defends is the endorsed alternative and is not scored here — it warrants its own near-zero-epsilon story.',
    'Scoring the endorsed system instead would collapse every advocacy reading''s epsilon toward zero and destroy cross-reading comparability; holding the referent fixed keeps all three readings comparable over one arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_discipline, conceptual, 'Fixes the epsilon referent to the contested swap arrangement per the kernel-reading referent rule.').

omega_variable(
    net_incidence_by_decile,
    'What is the actual net incidence of a specific swap design across income deciles and household types?',
    'CBO/JCT-style microsimulation of enacted bill text: offset list, dividend level, financing mix, and household-size interaction terms.',
    'If the bottom deciles net-gain under a particular design, the extraction reading weakens toward a tangled-rope profile; if losses concentrate on stacked households as the exemplar arithmetic suggests, the snare reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_incidence_by_decile, empirical, 'Whether the swap''s realized incidence matches the exemplar household''s loss.').

omega_variable(
    hybrid_floor_plus_targeting_viability,
    'Can a universal floor and need-indexed depth coexist in one architecture, dissolving the binary the kernel contest presumes?',
    'Comparative welfare-state analysis of universal-base-plus-means-tested-supplement designs, and pilot data on layered delivery systems.',
    'If hybrids are stable, this reading''s exclusive claim (''concentrated, not universal'') softens into an allocation-weighting claim, and the sibling readings become complements rather than rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_floor_plus_targeting_viability, preference, 'Whether the kernel contest is genuinely binary or a weighting dispute.').

omega_variable(
    political_durability_of_targeting,
    'Is the targeted system this reading defends politically durable, or does it decay toward thin, stigmatized programs regardless of the swap?',
    'Longitudinal take-up rates, real benefit levels, and budget shares for major means-tested programs across decades.',
    'If targeting decays on its own, the swap''s extraction story competes with a decay story about the incumbent system and this reading''s defensive posture loses its referent; if targeting is durable, the swap is the sole threat and the snare reading stands alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_durability_of_targeting, empirical, 'Durability of the incumbent targeted system this reading defends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t2015, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(inco_tr_t2015, observed).
narrative_ontology:measurement(inco_tr_t2017, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement_basis(inco_tr_t2017, observed).
narrative_ontology:measurement(inco_tr_t2019, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2019, 0.36).
narrative_ontology:measurement_basis(inco_tr_t2019, observed).
narrative_ontology:measurement(inco_tr_t2021, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement_basis(inco_tr_t2021, observed).
narrative_ontology:measurement(inco_tr_t2023, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2023, 0.44).
narrative_ontology:measurement_basis(inco_tr_t2023, observed).
narrative_ontology:measurement(inco_tr_t2025, income_support_commitment__targeting_efficiency_reading, theater_ratio, 2025, 0.47).
narrative_ontology:measurement_basis(inco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t2015, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(inco_be_t2015, observed).
narrative_ontology:measurement(inco_be_t2017, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement_basis(inco_be_t2017, observed).
narrative_ontology:measurement(inco_be_t2019, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement_basis(inco_be_t2019, observed).
narrative_ontology:measurement(inco_be_t2021, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2021, 0.71).
narrative_ontology:measurement_basis(inco_be_t2021, observed).
narrative_ontology:measurement(inco_be_t2023, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2023, 0.75).
narrative_ontology:measurement_basis(inco_be_t2023, observed).
narrative_ontology:measurement(inco_be_t2025, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(inco_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__targeting_efficiency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel, three readings, three files. The colloquial label 'the income support debate' conflates three structurally distinct commitments with different victim sets, different epsilon referents, and different classifications: the freedom-floor reading scores the absence-of-a-floor arrangement; the dependency-trap reading scores unconditional generosity itself; this reading scores the universal-dividend swap that finances universality by cannibalizing targeted depth. Each file links the others via network.affects_constraints. Upstream/downstream ordering follows argumentative reliance: the freedom-floor reading's feasibility claims are cited by swap sponsors when defending the consolidation financing, making it the upstream sibling whose erosion propagates here; the dependency-trap reading is an allied downstream critic sharing this reading's anti-universalism on different grounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
