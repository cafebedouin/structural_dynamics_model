% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__substantive_employment_reading, []).

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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Reading of the Platform Labor Boundary
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the regime in which platform
 *   labor is engaged as independent contracting: per-task pay,
 *   worker-supplied tools, no social-insurance contributions, no paid leave,
 *   and work allocation governed by platform algorithms. This file
 *   instantiates the substantive_employment_reading of the
 *   employment_boundary kernel, which assesses that arrangement through
 *   substance-over-form lights: where workers are economically dependent on a
 *   platform and managed by its algorithms, the employment relationship
 *   exists regardless of what the contract says. Per the fixed epsilon
 *   referent rule for kernel readings, extractiveness is authored for the
 *   STANDING arrangement as this reading sees it — not for the reading's
 *   endorsed alternative of mandatory reclassification with full
 *   social-insurance and job-security obligations. The claimed type and the
 *   metrics are independent authored facts: the type states what this seat
 *   believes is structurally true of the arrangement; the metrics state what
 *   is descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - platform_operators: Agenda-setter and primary beneficiary seat (institutional/arbitrage) — designs the control and classification architecture, collects the margin it protects, defends it across jurisdictions
 *   - platform_workers: Primary target seat (moderate/constrained) — bears the shifted cost of the employment package under algorithmic management
 *   - public_social_insurance_systems: Secondary payer seat (institutional/trapped) — absorbs fallback costs while receiving reduced contributions
 *   - consumers_of_platform_services: Incidental beneficiary seat (moderate/mobile) — buys convenience at prices subsidized by the risk-shift
 *   - labor_regulators_and_courts: Analytical observer seat (institutional/analytical) — adjudicates among the readings
 *   - global_south_platform_workers: Excluded payer seat (powerless/trapped) — same algorithms, thinner fallbacks, no seat in the rulemaking conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.65).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.6).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Reading of the Platform Labor Boundary").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '49d98a3a-666a-4604-988d-16d970f6e8e9').
narrative_ontology:cs_kernel_codification('49d98a3a-666a-4604-988d-16d970f6e8e9', formalized).
narrative_ontology:cs_authority_grounding('49d98a3a-666a-4604-988d-16d970f6e8e9', distributed).
narrative_ontology:cs_reading_relation('49d98a3a-666a-4604-988d-16d970f6e8e9', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('49d98a3a-666a-4604-988d-16d970f6e8e9', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('49d98a3a-666a-4604-988d-16d970f6e8e9', foundational, employment_status_follows_economic_substance).
narrative_ontology:cs_axiom_status(employment_status_follows_economic_substance, holdable).
narrative_ontology:cs_axiom_grounding('49d98a3a-666a-4604-988d-16d970f6e8e9', employment_status_follows_economic_substance, conventional).
narrative_ontology:cs_axiom('49d98a3a-666a-4604-988d-16d970f6e8e9', foundational, contract_form_cannot_defeat_protection_entitlement).
narrative_ontology:cs_axiom_status(contract_form_cannot_defeat_protection_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('49d98a3a-666a-4604-988d-16d970f6e8e9', contract_form_cannot_defeat_protection_entitlement, deontological).
narrative_ontology:cs_reference_frame('49d98a3a-666a-4604-988d-16d970f6e8e9', substantive_dependence_control_standard).
narrative_ontology:cs_drift_state('49d98a3a-666a-4604-988d-16d970f6e8e9', contemporary_platform_directive_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('49d98a3a-666a-4604-988d-16d970f6e8e9', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, consumers_of_platform_services).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, public_social_insurance_systems).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, global_south_platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, genuine_independence_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the dispatch, pricing, and rating systems that structure platform work and set the contractual terms under which workers are engaged. Classify workers as independent contractors across jurisdictions and defend that classification through litigation, ballot measures, and lobbying. Collect the margin between what customers pay and what workers receive per task, and avoid the fixed costs of an employment relationship — payroll taxes, paid leave, injury coverage, severance. Exit looks like restructuring the work model, automating dispatch, or shifting operations between jurisdictions.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_operators, beneficiary).

% Perform delivery, driving, care, and microtask work dispatched through platform apps. Are paid per task, supply their own tools and vehicles, and carry the costs of illness, injury, and unpaid time between tasks themselves. Accept deactivation, rate changes, and work-allocation rules they did not negotiate. Some value the schedule freedom and low barrier to entry; most depend on the work for a substantial share of income and face alternatives that pay worse, are scarcer, or offer less flexibility. Exit looks like moving to another app with the same structure, or leaving gig work for traditional employment.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, platform_workers, beneficiary).

% Administer unemployment, pension, injury, and health coverage funded substantially by employer and employee contributions. Receive reduced or no contributions from work relationships classified as contracting, while absorbing the costs when platform workers fall back on public benefits during illness, injury, or income gaps. Cannot decline to cover residents who lack private coverage; their exposure is structural and grows with the platform sector's share of the labor market.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, public_social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Order rides, deliveries, and services through platform apps at prices and response times that depend on the current labor structure. Benefit from convenience and price levels that would likely shift if platforms carried full employment costs. Can switch between competing apps easily and individually bear almost none of the arrangement's risk.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers_of_platform_services, beneficiary,
    moderate, immediate, mobile, global).

% Adjudicate classification disputes, legislate tests such as ABC tests and presumptions of employment, and inspect platform labor practices. Have produced conflicting specifications across jurisdictions: some rulings find platform control incompatible with contractor status, while ballot initiatives and statutes have carved platforms out of general tests. Their seat is adjudicative — they decide among the parties' framings rather than collect from or pay into the arrangement.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Work the same apps under the same algorithms in cities outside the jurisdictions where classification rules are being made. Face weaker labor inspectorates, no litigation funding, and no seat in the EU- and US-centered regulatory conversation that will define the boundary their work falls under. Bear the same per-task costs with thinner fallback options and no prospect of collective redress through the courts that are setting the standard.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, global_south_platform_workers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, global_south_platform_workers, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the matching problem of connecting spiky, geographically distributed demand for rides, deliveries, and tasks with a large flexible labor pool: real-time dispatch, standardized payment, rating-based trust, and near-zero-friction entry for workers who want schedule control. The same infrastructure lets platforms scale service networks rapidly without negotiating an employment relationship with each worker.
% TRANSFER_FUNCTION: Moves per-task labor and availability from workers to platforms and customers, and per-task payment from customers and platforms to workers. Simultaneously moves the costs of the employment package — social insurance contributions, paid sick leave, injury liability, income continuity between tasks — away from platforms and onto workers themselves and, when they fall back, onto public systems. Also moves behavioral data and work-allocation control from workers to platform algorithms.
% ABSENT_VOICES: Platform workers outside the rulemaking jurisdictions have no seat in the EU- and US-centered conversation that will set the boundary governing them. Workers who genuinely prefer contractor flexibility appear mainly as platform litigation exhibits rather than as independently organized voices, and informal-sector workers whose claims would ride the same doctrinal boundary are absent entirely.
% DISAPPEARANCE_RATIONALE: If the contractor-classification arrangement vanished overnight — platforms had to treat workers as employees everywhere — platform prices, coverage density, and business models would restructure; social-insurance contribution bases would widen; some marginal services would consolidate or withdraw from thin markets; and the litigation and ballot apparatus built to defend the classification would dissolve. The matching infrastructure would survive, but the cost structure of platform services would be rebuilt around internalized employment obligations.
% FOUNDING_PROBLEM: Scaling on-demand service networks quickly: the early platforms needed large, flexible labor pools without the fixed costs, liabilities, and regulatory overhead of hiring employees — payroll taxes, benefits administration, minimum-wage floors, vehicle licensing regimes. Treating workers as independent contractors let the networks grow at venture speed while both sides were said to benefit from mutual flexibility.
% FOUNDING_PROBLEM_CORROBORATION: The platforms attest the founding problem is live — flexibility is the product and the industry still scales on it. Outside the benefiting parties, the UK Supreme Court's findings in Uber BV v Aslam (2021) documented control incompatible with genuine independence, corroborating that the independence framing was always partly fictional; ILO reports and academic labor-economic studies corroborate both halves of the dispute — real matching-efficiency gains and a real, growing protection gap. No party outside the platforms attests that the original scaling problem required the specific risk-shift the arrangement now embodies.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__substantive_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.65: through this reading's lights the full employment-cost package — contributions, sick pay, injury liability, income continuity — is shifted onto workers while the platform retains pricing, allocation, and deactivation control; genuine per-task payment and real matching and flexibility value keep it below pure-extraction levels, matching the moderate-epsilon structural delta. Suppression is 0.60: persistence depends on active legal and political enforcement (mandatory arbitration, deactivation regimes, ballot-measure preemption of legislative tests, multi-jurisdiction lobbying), though workers retain partial exits into other apps and conventional jobs. Theater is 0.32: the 'independent entrepreneur' framing is heavily performed against a documented control reality, while the dispatch, payment, and trust functions are real. Accessibility collapse is 0.52: alternatives exist but either share the same structural form or are scarcer and less flexible. Resistance is 0.68: strikes, funded litigation, statutory campaigns, and the EU Platform Work Directive. The measurement series share one time grid (T0 is approximately 2009, when the platform model begins scaling; T16 approximately 2025; one unit is roughly one year). Extractiveness rises as algorithmic management deepened and real per-task compensation eroded; the suppression series is the enforcement ratchet — classification was barely contested at T0, then AB5 (T10), Prop 22 (T11, between grid points), the Aslam ruling (T12), and the EU directive fight (T14-T16) forced escalating enforcement investment, which is why suppression_requirement is tracked alongside the other metrics.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute the arrangement very differently from the agenda-setter seat. From platform_workers and public_social_insurance_systems, the arrangement is an employment relationship with the obligations stripped out — full dependence and control, none of the protections. From platform_operators, it is a flexibility product they designed, priced, and defend, with the classification as its legal foundation. consumers_of_platform_services see only price and convenience and would experience reclassification as modest price increases. labor_regulators_and_courts occupy the adjudicating seat where the framings compete. The engine computes per-seat classifications from the structural data; this divergence — the same arrangement computing as different types from different seats — is the expected output, not an inconsistency.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_operators sit near the full-beneficiary end: they authored the classification, collect the margin it protects, and hold arbitrage-grade exit (restructuring, jurisdiction-shifting, automation), which damps their measured cost of the arrangement toward zero. platform_workers sit near the full-target end: they bear the shifted cost package under constrained exit — dependence on per-task income limits walking away, and the alternatives share the structure. public_social_insurance_systems are trapped targets: obligated to absorb fallback costs, unable to decline coverage, receiving reduced contributions. consumers_of_platform_services sit low-moderate: they benefit from prices subsidized by the risk-shift but pay the fares and carry little risk. global_south_platform_workers sit nearest the full-target end of any seat: same algorithms, thinner fallbacks, no seat in the rulemaking conversation. labor_regulators_and_courts are analytical and directionality-neutral. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and the derivation chain handles the dual-positioned workers seat through its primary payer role.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope structure is what keeps the classification from collapsing into either mislabel. Reading the arrangement as pure extraction would erase the real coordination function — matching, payment, trust, low-friction entry — and point reform at abolition rather than obligation-internalization, which this reading's own remedy (reclassify and obligate) contradicts. Reading it as pure coordination would erase the asymmetric risk-shift that this reading documents and that courts have corroborated. The R5 mismatch check finds no zombie signature: the founding problem is contested, not dead — the matching problem the arrangement was built to solve is still real, so status=contested combined with verdict=world_rearranges flags a live dispute over the risk-shift, not a mandate outliving its function. The live question the classification preserves is exactly the right one: whether the risk-shift remains necessary to the coordination. The EU directive's presumption-of-employment route and the carve-out counter-movement are the empirical test of that question now underway.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the employment_boundary kernel; would the formalist or hybrid sibling readings change this constraint''s structure, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling stories: the formalist reading removes platform workers from any employment-protection claim entirely; the hybrid reading moves them to a partial-protection zone with tailored obligations. The disagreement is located at the criterion of status — economic substance (dependence plus algorithmic control) versus contract form plus direct supervision versus a purpose-built third category.',
    'If the formalist reading prevails, this story''s victim set loses standing and the arrangement''s cost-shift accounting collapses toward the formalist epsilon; if the hybrid prevails, the victim set is preserved but the remedy set changes from full employment obligations to tailored protections, altering what this reading obligates platforms to provide.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the employment boundary governs determines the victim set, the platform obligations, and the extraction accounting.').

omega_variable(
    worker_dependence_heterogeneity,
    'How uniformly do platform workers actually exhibit the economic dependence this reading treats as definitional — and does a genuinely independent minority exist that a uniform regardless-of-contract-form rule would misclassify?',
    'Platform-level data on income concentration (share of workers for whom a single platform exceeds half of income), multi-apping rates, and hours distribution, audited by labor statistics agencies rather than self-reported by platforms.',
    'If a substantial minority is genuinely supplementary and independent, the uniform rule over-extends the victim set; the operative classification would drift toward the hybrid reading''s tailored-category solution and this story''s epsilon would fall for that segment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_dependence_heterogeneity, empirical, 'Heterogeneity of economic dependence across the platform workforce.').

omega_variable(
    reclassification_trajectory,
    'Will the substantive reading become operative — via EU Platform Work Directive transposition, litigation diffusion, and statutory presumptions — or will platform-specific carve-outs contain it sector by sector?',
    'Track directive transposition across member states, appellate outcomes in pending classification cases, and whether new statutes generalize the presumption of employment or carve platform work out of general tests.',
    'If carve-outs contain the reading, the standing arrangement persists with its enforcement ratchet and this story''s suppression trajectory keeps rising; if the reading generalizes, the payer seats gain protections and the arrangement''s structure converts toward obligation-internalized coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reclassification_trajectory, empirical, 'Direction and containment of the substantive reading''s legal diffusion.').

omega_variable(
    flexibility_protection_tradeoff,
    'Does mandatory employee reclassification destroy schedule flexibility and low-friction entry that many workers demonstrably value — and if so, does that cost outweigh the protection gains for the workers themselves?',
    'Natural experiments from reclassification and carve-out jurisdictions (Spain''s rider law, UK worker-status outcomes, the operation of California''s Prop 22): service prices, coverage density, active-worker numbers, and worker survey data on schedule satisfaction and income adequacy.',
    'If flexibility losses are large and worker-valued, this reading''s net-benefit claim for its victim set weakens and the hybrid reading gains legitimacy; if protections arrive without large flexibility losses, the reading''s case strengthens and the formalist flexibility argument collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flexibility_protection_tradeoff, preference, 'How workers themselves value flexibility against protection under reclassification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__substantive_employment_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(empl_tr_t3, employment_boundary__substantive_employment_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(empl_tr_t6, employment_boundary__substantive_employment_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__substantive_employment_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(empl_tr_t10, employment_boundary__substantive_employment_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__substantive_employment_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(empl_tr_t14, employment_boundary__substantive_employment_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__substantive_employment_reading, theater_ratio, 16, 0.32).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__substantive_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(empl_be_t3, employment_boundary__substantive_employment_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(empl_be_t6, employment_boundary__substantive_employment_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(empl_be_t8, employment_boundary__substantive_employment_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(empl_be_t10, employment_boundary__substantive_employment_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(empl_be_t12, employment_boundary__substantive_employment_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(empl_be_t14, employment_boundary__substantive_employment_reading, base_extractiveness, 14, 0.63).
narrative_ontology:measurement(empl_be_t16, employment_boundary__substantive_employment_reading, base_extractiveness, 16, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__substantive_employment_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(empl_su_t3, employment_boundary__substantive_employment_reading, suppression_requirement, 3, 0.33).
narrative_ontology:measurement(empl_su_t6, employment_boundary__substantive_employment_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(empl_su_t8, employment_boundary__substantive_employment_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(empl_su_t10, employment_boundary__substantive_employment_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(empl_su_t12, employment_boundary__substantive_employment_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(empl_su_t14, employment_boundary__substantive_employment_reading, suppression_requirement, 14, 0.59).
narrative_ontology:measurement(empl_su_t16, employment_boundary__substantive_employment_reading, suppression_requirement, 16, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three constraint stories under the epsilon-invariance principle: formalist_employment_reading (form controls; no employment-protection claim arises for platform workers, so its extraction accounting over the standing arrangement is low), hybrid_security_reading (third category; partial victim set with tailored obligations), and this file, substantive_employment_reading (substance controls; the standing arrangement's risk-shift is fully visible, moderate-high epsilon). The readings share the standing platform-labor arrangement as referent but differ in victim set and extraction accounting, so each carries its own epsilon, stakeholders, and classification rather than one story with a measurement parameter. Upstream/downstream structure: the formalist reading is the historically default specification and is cited by platforms as the settled baseline; the substantive reading's litigation wins (Aslam) and the EU directive's presumption of employment supply the doctrinal pressure from which the hybrid category would be built. This file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
