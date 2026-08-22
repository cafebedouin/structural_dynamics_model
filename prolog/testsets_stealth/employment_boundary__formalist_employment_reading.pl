% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary: Platform Workers as Independent Contractors
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The formalist reading of the employment boundary holds that employment
 *   exists where there is a formal contract of service and direct
 *   supervision, and that platform workers — engaged through click-through
 *   service agreements and managed by algorithm rather than supervisor — fall
 *   outside it. This story instantiates that reading alone as one clean,
 *   epsilon-invariant constraint. The epsilon referent is the standing
 *   arrangement under contest — the contractor classification of platform
 *   work — assessed by this reading's own lights: the formalist frame
 *   concedes the cost arithmetic (benefit, insurance, and floor costs land on
 *   workers and state backstops) and disputes only the legal criterion that
 *   assigns its consequence. The claim/metric gap is deliberate: the reading
 *   CLAIMS the arrangement as legitimate coordination — a bright-line
 *   classification rule whose participants are net beneficiaries
 *   (claimed_type rope, the reading's own framing) — while the authored
 *   metrics describe the cost structure every party to the dispute concedes.
 *   Sibling readings (substantive_employment_reading,
 *   hybrid_security_reading) instantiate different constraints with different
 *   victim sets and are linked through the network block. KEY AGENTS (by
 *   structural relationship): platform_companies — agenda-setter and primary
 *   beneficiary (institutional/arbitrage), writes the contractor architecture
 *   and collects the avoided obligations; platform_workers — primary
 *   cost-bearers (powerless/constrained), self-fund what employment status
 *   would provide, denied victimhood-of-employment-precarity by this reading
 *   on the ground that they chose flexibility while the costs land on them
 *   regardless; platform_consumers — secondary beneficiary (moderate/mobile);
 *   state_social_insurance_programs — payer (institutional/constrained);
 *   compliant_traditional_employers — payer (organized/constrained);
 *   platform_worker_unions — excluded collective voice (organized/trapped);
 *   labor_standards_enforcement_agencies — analytical observer
 *   (institutional).
 *
 * KEY AGENTS:
 *   - platform_companies: agenda-setter and primary beneficiary (institutional/arbitrage) — write the contractor architecture, collect the avoided employer obligations, and fund the classification's defense
 *   - platform_workers: primary cost-bearers (powerless/constrained) — the reading denies they are victims of employment precarity because they chose flexibility, while conceding the externalized costs land on them
 *   - platform_consumers: secondary beneficiary (moderate/mobile) — receive below-cost services; carry a diffuse taxpayer share of the backstop
 *   - state_social_insurance_programs: payer (institutional/constrained) — absorb the safety-net backstop; can change the rule only through contested statute and referendum
 *   - compliant_traditional_employers: payer (organized/constrained) — bear employment-law costs their platform competitors avoid
 *   - platform_worker_unions: excluded (organized/trapped) — no statutory collective-bargaining channel exists for contractors
 *   - labor_standards_enforcement_agencies: analytical observer (institutional) — investigate and litigate misclassification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.78).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.62).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary: Platform Workers as Independent Contractors").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'df7e35a2-ea9c-416b-9556-dd5330355d11').
narrative_ontology:cs_kernel_codification('df7e35a2-ea9c-416b-9556-dd5330355d11', fixed_text).
narrative_ontology:cs_authority_grounding('df7e35a2-ea9c-416b-9556-dd5330355d11', lineage).
narrative_ontology:cs_interpretation_layer_present('df7e35a2-ea9c-416b-9556-dd5330355d11').
narrative_ontology:cs_reading_relation('df7e35a2-ea9c-416b-9556-dd5330355d11', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('df7e35a2-ea9c-416b-9556-dd5330355d11', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('df7e35a2-ea9c-416b-9556-dd5330355d11', foundational, contract_form_determines_employment_status).
narrative_ontology:cs_axiom_status(contract_form_determines_employment_status, holdable).
narrative_ontology:cs_axiom_grounding('df7e35a2-ea9c-416b-9556-dd5330355d11', contract_form_determines_employment_status, conventional).
narrative_ontology:cs_axiom('df7e35a2-ea9c-416b-9556-dd5330355d11', secondary, flexibility_preserves_worker_welfare).
narrative_ontology:cs_axiom_status(flexibility_preserves_worker_welfare, holdable).
narrative_ontology:cs_axiom_grounding('df7e35a2-ea9c-416b-9556-dd5330355d11', flexibility_preserves_worker_welfare, empirically_contingent).
narrative_ontology:cs_reference_frame('df7e35a2-ea9c-416b-9556-dd5330355d11', contract_form_determines_status).
narrative_ontology:cs_drift_state('df7e35a2-ea9c-416b-9556-dd5330355d11', post_algorithmic_management_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('df7e35a2-ea9c-416b-9556-dd5330355d11', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_consumers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_social_insurance_programs).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, compliant_traditional_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, formalist_control_test_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and sell the contractor architecture: standard click-through service agreements label every worker an independent contractor, app-based algorithmic management supplies the direction that a supervisor once did, and legal teams litigate, legislate, and run ballot-measure campaigns to keep the classification intact. Every dollar not spent on payroll taxes, unemployment insurance, workers compensation, health coverage, wage floors, or overtime flows to the bottom line or into price competition. Exit is easy in the relevant sense: they operate across jurisdictions, restructure corporate entities, and can outspend challengers in referendum fights.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, platform_companies, beneficiary).

% Accept rides, deliveries, or tasks through apps whose terms they cannot negotiate; pay for vehicles, fuel, insurance, and self-employment taxes out of gross earnings; and carry no unemployment coverage, no workers compensation, no paid sick leave, and no floor under effective hourly pay. They can stop working at any time and many run several apps at once, but the contractor label follows them across platforms, and the jobs available outside the gig sector largely offer the same benefit-free terms or less schedule control. Some genuinely prize the scheduling freedom; others platform because benefit-eligible employment is not available to them.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, platform_workers, beneficiary).

% Receive rides, deliveries, and services at prices held down by the absence of employer-borne labor costs, with one-tap convenience. They carry a diffuse share of the arrangement's public costs as taxpayers funding the safety-net programs that absorb platform-work income shocks, and they can switch apps or abstain at will.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Fund unemployment insurance, workers compensation systems, Medicaid, and earned-income tax credits that absorb what employer-paid programs would otherwise cover for platform workers; several states have run fiscal audits quantifying this backstop. They can change the classification rule by statute, and several have tried, but face platform-funded referendum campaigns, preemption fights, and constitutional challenges when they do.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_social_insurance_programs, payer,
    institutional, generational, constrained, national).

% Pay payroll taxes, unemployment insurance, workers compensation premiums, and benefits for comparable work, and compete against platforms structurally relieved of those costs. They lobby for classification enforcement, sometimes in direct opposition to the platforms, and cannot shed employment-law obligations without rebuilding themselves as app-mediated intermediaries.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, compliant_traditional_employers, payer,
    organized, biographical, constrained, national).

% Attempt to organize drivers and couriers for collective bargaining and strike action, but the contractor classification places gig workers outside the statutory collective-bargaining framework, so there is no recognized channel: recognition fights run through litigation and city-level ordinances instead. They would bargain over pay floors, deactivation protections, and portable benefit funds if a channel existed.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_worker_unions, excluded,
    organized, biographical, trapped, national).

% Investigate misclassification claims, audit platform pay practices, and bring enforcement actions; they take testimony from workers, platforms, and state fiscal offices, and their rulings in some jurisdictions have forced reclassification or settlement. Their reach varies by jurisdiction and is contested in court.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_standards_enforcement_agencies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single administrable criterion for the labor market's central legal category: firms, workers, agencies, and courts can determine who is an employee by asking whether there is a contract of service and direct supervision, without open-ended factual investigation into economic dependence. This predictability lowers the cost of classification disputes, lets firms structure work without unbounded liability, and preserves a recognized channel for genuinely independent contracting.
% TRANSFER_FUNCTION: Moves the cost of income security and workplace protection — payroll taxes, unemployment and workers compensation coverage, health benefits, wage floors, overtime premiums — from platforms (with part of the price benefit passing to consumers) onto platform workers' own earnings and onto state social-insurance backstops; it also moves competitive cost advantage from employment-law-compliant employers to platforms.
% ABSENT_VOICES: Platform workers' collective voice: as contractors they stand outside the statutory collective-bargaining framework, so the people bearing the largest cost share had no seat in the contract-architecture decisions or in referendum campaigns framed as flexibility-versus-benefits. State fiscal offices bearing the backstop costs were likewise largely absent from the ballot framing. Both would have argued for cost internalization and a bargaining channel.
% DISAPPEARANCE_RATIONALE: If the formalist classification of platform work vanished overnight, platform business models would restructure immediately: prices rise or service coverage contracts as labor costs internalize, platforms convert to employment relationships with scheduling and floors or shrink, state backstop spending falls, compliant employers regain cost parity, and classification litigation reorganizes around whatever criterion replaced the test.
% FOUNDING_PROBLEM: Nineteenth- and early-twentieth-century courts needed an administrable rule for deciding when an injured party could hold an employer liable for a worker's conduct (vicarious liability), and later, which workers statutory protections covered; the formal control test answered with a rule a court could apply from the contract and the supervision relationship without open-ended economic inquiry.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and employment-law scholars attest the test's vicarious-liability origins; courts applying rival readings (the UK Supreme Court in the Uber driver-status litigation, Spanish courts under the Riders Law) attest that contract form no longer captures the managed-work reality; state fiscal offices attest the backstop costs the test now externalizes. No attestation comes from platform beneficiaries alone — the platforms' own attestation that the test remains fit is the position under contest.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the arrangement moves the full cost of income security — payroll taxes, unemployment and workers compensation coverage, health benefits, wage floors, overtime — off the platform's books and onto workers' gross earnings and public programs; the formalist frame concedes this arithmetic and disputes only the legal consequence. Suppression (0.62) is structural rather than workplace-coercive: the classification forecloses the employee-claim channel itself through arbitration clauses, class-action waivers, deactivation leverage, and referendum-entrenched statutes; suppression is authored as a raw structural property, unscaled by power or scope. Theater (0.31) reflects real classification work done by the bright-line test alongside performative maintenance — entrepreneurship rhetoric and token benefit stipends few workers qualify for. Alternatives do not collapse (accessibility_collapse 0.35) because rival readings have won in whole jurisdictions — the UK Supreme Court, Spain's Riders Law, the EU Platform Work Directive — and resistance is high (0.72) accordingly: a decade of litigation, legislation, strikes, and ballot wars. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity build-up: the arbitration architecture, class waivers, and referendum entrenchment matured and hardened across the interval rather than staying static. The boltzmann coordination type is identity_coordination: the boundary's primary function is membership adjudication — deciding who is inside the employment relationship — against evolving criteria. The powerless worker seat carries a coalition caveat: individually powerless, platform workers have repeatedly exercised collective leverage through strike waves and referendum campaigning on both sides, so the seat's power atom understates potential coordinated power. All three metric series run on one shared time grid (t = 0,3,6,9,12,15,18; roughly 2008 to the present) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the platform seat the arrangement is the coordination it built: a predictable classification that lets it price and scale, with near-beneficiary directionality and low effective extraction. From the worker and state seats the same structure is a cost-transfer machine whose legal channel for relief is foreclosed by the classification itself — high effective extraction, near-target directionality. Compliant traditional employers experience it as competitive injury with no exit from their own compliance obligations; consumers sit near-symmetric, taking the price benefit while carrying a diffuse tax share. The engine computes this divergence from the structural data; the authored rope claim is the reading's claim and does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_companies are declared beneficiaries with arbitrage-grade exit — they sit near the beneficiary end of d. platform_workers are declared victims with constrained exit; the structural derivation would place them near full-target, and the authored override (powerless atom to 0.78 — the only powerless seat in this story) damps it because the reading itself concedes that a genuine scheduling-autonomy good flows to workers through the same structure; the flexibility_premium_size omega calibrates that damping. state_social_insurance_programs and compliant_traditional_employers are victims with no exit from their backstop and compliance roles — near-target. platform_consumers are beneficiaries with mobile exit — near the beneficiary end, damped slightly by their diffuse taxpayer share. The excluded union seat and the observer seat take no directionality from the beneficiary/victim derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an administrable legal test for who is an employee — is still partially live: courts and agencies genuinely need a workable boundary, and the bright-line test does real classification work. But the arrangement's current center of gravity is contested: the same test now allocates an entire sector's labor costs, a use its architects never contemplated, and the mismatch between the live administrability problem and the arrangement's cost-transfer function is exactly what the founding_problem_status x disappearance_verdict comparison surfaces for the engine. The rope claim also prevents the opposite mislabeling: this is not pure cover — the classification function is real, and a snare-only reading would erase the genuine coordination that makes this a hybrid structure rather than pure extraction. Per-seat classification keeps both truths alive simultaneously: the coordination the reading built and the extraction it now performs are visible in the same structural record without being reconciled by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the formalist reading of the employment_boundary kernel; what exactly would the sibling readings change structurally, and where in the structure is the disagreement located?',
    'Comparative adjudication record: track which criterion (contract form plus supervision, economic dependence plus algorithmic control, or a tiered third category) each jurisdiction adopts and what obligation and victim sets follow from each.',
    'Under the substantive reading the victim set expands to the full employment-precarity set and employer obligations attach to platforms (the epsilon referent is unchanged; the victim set and directionality values change). Under the hybrid reading a third category partially attaches obligations while preserving contractor status for some purposes. The disagreement is located in the status-determining criterion, not in the cost arithmetic — every reading concedes that costs land on workers and the state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is one reading of the employment_boundary kernel; sibling readings change the victim set and obligation attachment, not the conceded cost location.').

omega_variable(
    choice_or_residual_option,
    'Is platform work genuinely chosen among adequate alternatives (the formalist frame''s premise that workers chose flexibility and are therefore not owed employment protections), or is contractor status the residual option for workers excluded from benefit-eligible employment?',
    'Longitudinal survey and administrative data on platform workers'' outside options: prior employment history, benefit eligibility, and stated reasons for entering and exiting platform work.',
    'If the choice set is adequate, part of the cost transfer is the price of a real option and the worker seat''s directionality sits below full-target as the authored override holds; if contractor status is a residual option, the reading''s victim-exclusion premise fails, worker directionality moves to full-target, and effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(choice_or_residual_option, empirical, 'Whether the formalist reading''s worker-exclusion premise rests on genuine choice or on constrained option sets.').

omega_variable(
    state_backstop_magnitude,
    'What is the true fiscal magnitude of the public backstop — how much do unemployment insurance, workers compensation, Medicaid, and tax-credit programs spend annually absorbing platform-work income insecurity?',
    'State fiscal audits cross-referenced with platform-work participation data; several states have begun these audits and the methodology can generalize across jurisdictions.',
    'Quantifies the state victim seat''s share of the extraction; a large magnitude supports the high-epsilon profile and establishes that the arrangement''s costs are externalized rather than internalized by the parties to the contracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_backstop_magnitude, empirical, 'Fiscal size of the safety-net backstop substituting for employer-provided coverage.').

omega_variable(
    flexibility_premium_size,
    'How large is the scheduling-autonomy value workers actually realize relative to the benefits and floors they forgo — is the flexibility premium big enough to justify damping the worker seat below full-target directionality?',
    'Willingness-to-accept studies comparing platform work against benefit-eligible alternatives with fixed schedules, plus revealed preference from worker flows into and out of the sector.',
    'Calibrates the authored directionality override for powerless agents (0.78): a large realized premium holds workers below full-target; a small one restores near-full-target extraction and the override should be removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flexibility_premium_size, empirical, 'Size of the genuine flexibility benefit that offsets part of the worker cost burden.').

omega_variable(
    classification_durability,
    'Does the formalist classification consolidate globally (rising suppression as more jurisdictions contest it), or does it retreat jurisdiction by jurisdiction toward the substantive or hybrid readings?',
    'Track classification outcomes and directive implementation across jurisdictions over the coming decade: EU Platform Work Directive transposition, UK-style judicial rulings, and US state referenda and statutes.',
    'Determines whether the rising suppression trajectory continues (entrenchment) or reverses (enforcement decay), changing the temporal classification path even if the current cross-section is unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_durability, empirical, 'Whether the formalist boundary consolidates or erodes across jurisdictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(formalist_employment_reading_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(formalist_employment_reading_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.17).
narrative_ontology:measurement(formalist_employment_reading_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(formalist_employment_reading_tr_t9, employment_boundary__formalist_employment_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement(formalist_employment_reading_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(formalist_employment_reading_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(formalist_employment_reading_tr_t18, employment_boundary__formalist_employment_reading, theater_ratio, 18, 0.31).

% Extraction over time
narrative_ontology:measurement(formalist_employment_reading_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(formalist_employment_reading_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(formalist_employment_reading_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(formalist_employment_reading_be_t9, employment_boundary__formalist_employment_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement(formalist_employment_reading_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(formalist_employment_reading_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(formalist_employment_reading_be_t18, employment_boundary__formalist_employment_reading, base_extractiveness, 18, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(formalist_employment_reading_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(formalist_employment_reading_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.41).
narrative_ontology:measurement(formalist_employment_reading_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement(formalist_employment_reading_su_t9, employment_boundary__formalist_employment_reading, suppression_requirement, 9, 0.53).
narrative_ontology:measurement(formalist_employment_reading_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(formalist_employment_reading_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(formalist_employment_reading_su_t18, employment_boundary__formalist_employment_reading, suppression_requirement, 18, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, identity_coordination).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three constraint stories per the epsilon-invariance principle: this formalist reading (status by contract form and supervision; victims are the cost-bearers of externalization), the substantive reading (status by economic dependence and algorithmic control; the victim set expands to the full employment-precarity set and platform obligations attach), and the hybrid security reading (a third category with partial tailored obligations). Each carries its own epsilon, beneficiary/victim structure, and classification; the upstream formalist reading influences the downstream siblings because its doctrinal wins are cited as evidence that reclassification is unnecessary. All three readings share one conceded fact — the cost arithmetic — and differ on the criterion that assigns its legal consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
