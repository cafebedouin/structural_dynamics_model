% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Hybrid Third Category for Platform Work (Tailored-Protections Reading)
 *   domain: economic/labor/social policy
 *
 * SUMMARY:
 *   A third legal category for platform work sits between employment and
 *   independent contracting: platforms owe injury-insurance premiums and
 *   work-linked medical coverage (coverage reaches roughly 91.5% of platform
 *   workers for medical care and 86.2% for injury), but owe no retirement
 *   contributions, severance, scheduling guarantees, or collective-bargaining
 *   duties. The category was built to close a real coverage gap — platform
 *   workers had no work-linked protection at all under pure contractor status
 *   — and it does deliver real protection. It also institutionalizes a
 *   residual precarity: the obligations it omits are the ones that compound
 *   over a career, and its enactment is now cited in courts and legislatures
 *   against worker claims to full employment status. Per the ε-invariance
 *   decomposition rule, the 'employment boundary' label covers three
 *   structurally distinct constraints with different victim sets and
 *   different ε values; this story instantiates only the hybrid reading, with
 *   ε assessed over the standing arrangement — the third category as enacted
 *   and administered — by the hybrid reading's own lights. The sibling
 *   readings are separate constraint files linked through the network edges.
 *   KEY AGENTS (by structural relationship): - platform_operators: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) — sets contract
 *   terms, collects the obligation gap as retained revenue -
 *   gig_platform_workers: Net target with partial beneficiary position
 *   (powerless/constrained) — receives injury and medical coverage, bears
 *   foregone retirement and career development - labor_regulators:
 *   Co-administrator (institutional/constrained) — drafts and administers the
 *   category's rules - platform_worker_unions: Excluded claimant
 *   (organized/constrained) — presses employee-status claims the category's
 *   frame forecloses - public_assistance_programs: Residual payer
 *   (institutional/constrained) — absorbs old-age and income-support costs of
 *   uncovered retirement risk - ilo_employment_experts: Analytical observer
 *   (analytical/analytical) — tracks cross-jurisdiction coverage statistics
 *
 * KEY AGENTS:
 *   - platform_operators: primary beneficiary and agenda-setter — collects the gap between full-employment cost and partial-obligation cost as retained revenue, with arbitrage-grade exit across jurisdictions
 *   - gig_platform_workers: net target with partial beneficiary position — dual-listed in beneficiaries and victims; receives partial coverage, bears the compounding career-long gap
 *   - labor_regulators: co-administrator — the category exists as their distinct administrative mandate
 *   - platform_worker_unions: excluded claimant — organized outside the frame their members' disputes are channeled into
 *   - public_assistance_programs: residual payer — absorbs the deferred costs of uncovered retirement risk
 *   - ilo_employment_experts: analytical observer — publishes the coverage statistics both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.58).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.6).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Third Category for Platform Work (Tailored-Protections Reading)").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "economic/labor/social policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, '27512b61-a915-4232-94c0-175c5622c486').
narrative_ontology:cs_kernel_codification('27512b61-a915-4232-94c0-175c5622c486', fixed_text).
narrative_ontology:cs_authority_grounding('27512b61-a915-4232-94c0-175c5622c486', lineage).
narrative_ontology:cs_interpretation_layer_present('27512b61-a915-4232-94c0-175c5622c486').
narrative_ontology:cs_reading_relation('27512b61-a915-4232-94c0-175c5622c486', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('27512b61-a915-4232-94c0-175c5622c486', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_axiom('27512b61-a915-4232-94c0-175c5622c486', foundational, third_category_necessary_for_platform_protection).
narrative_ontology:cs_axiom_status(third_category_necessary_for_platform_protection, holdable).
narrative_ontology:cs_axiom_grounding('27512b61-a915-4232-94c0-175c5622c486', third_category_necessary_for_platform_protection, instrumental).
narrative_ontology:cs_axiom('27512b61-a915-4232-94c0-175c5622c486', secondary, obligations_calibrated_below_full_employment).
narrative_ontology:cs_axiom_status(obligations_calibrated_below_full_employment, holdable).
narrative_ontology:cs_axiom_grounding('27512b61-a915-4232-94c0-175c5622c486', obligations_calibrated_below_full_employment, empirically_contingent).
narrative_ontology:cs_reference_frame('27512b61-a915-4232-94c0-175c5622c486', tripartite_tailored_protection_framework).
narrative_ontology:cs_drift_state('27512b61-a915-4232-94c0-175c5622c486', contemporary_platform_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27512b61-a915-4232-94c0-175c5622c486', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, gig_platform_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, gig_platform_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, public_assistance_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the contract terms under which workers access their platforms and lobbied for the legal category that governs their workforces. Under the third category they pay injury-insurance premiums and work-linked medical coverage while owing no retirement contributions, severance, or scheduling guarantees. They can restructure work allocation, adjust piece rates, and shift operations between jurisdictions. The difference between what full employment status would cost them and what the third category requires accrues to them as retained revenue, and they fund the research and consultation processes that defend the category's design.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_operators, beneficiary).

% Perform allocated work across one or more platforms, paid per task with no guaranteed hours. The category gives them injury insurance and work-linked medical coverage they would lack as pure independent contractors. It gives them no retirement credits, no seniority, no career development, and no accumulation of protections over time, so the gap between their coverage and an employee's widens over a working life. Leaving means abandoning platform income entirely; most are bound by arbitration clauses and must pursue disputes inside the category's frame rather than as employment claims.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, gig_platform_workers, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, gig_platform_workers, beneficiary).

% Draft and administer the third category's rules: which platforms qualify, what premiums are owed, what coverage attaches, and how disputes are channeled. They run tripartite consultation processes in which platform representatives arrive heavily resourced while worker representatives are thinner on the ground. Their administrative machinery exists because the category exists as a distinct legal object; folding platform work into general employment law would dissolve much of their dedicated mandate.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Organize rideshare, delivery, and care-platform workers and press for full employment recognition. They were marginal participants in the consultation processes that designed the third category, and the category's enactment is now cited in courts and legislatures against the employee-status claims they advance. Their organizing continues outside the category's frame, which is precisely the frame their members' disputes are channeled into.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_worker_unions, excluded,
    organized, biographical, constrained, regional).

% Absorb the residual costs of workers without retirement security: old-age income top-ups, housing assistance, and healthcare subsidies for workers whose platform income ends with no pension behind it. Their caseloads scale with the size of the workforce the category leaves without retirement coverage, and they have no seat in the consultations that set the category's obligation levels.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, public_assistance_programs, payer,
    institutional, generational, constrained, national).

% Track coverage statistics across jurisdictions that adopted third categories, compare protection levels against both the employment and contractor baselines, and publish the medical and injury coverage figures that anchor the category's performance claims. They see the cross-jurisdiction variation in category design and the divergence between announced protection and career-long coverage.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, ilo_employment_experts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends work-linked social protection to a work form that fits neither the bilateral employment contract nor pure independent contracting: injury insurance and medical coverage are solved once, at the category level, for workers whose attachment is algorithmic, multi-platform, and task-based.
% TRANSFER_FUNCTION: Moves partial obligation from platforms to workers' protection: platforms pay injury premiums and medical contributions while the retirement, severance, career-development, and collective-bargaining package they would owe under employment stays with them as retained revenue; workers receive partial coverage and carry the residual risk, part of which lands later on public assistance.
% ABSENT_VOICES: Platform worker unions and worker advocates who would press for full employment recognition were marginal in the tripartite processes that designed the category; public assistance programs, which absorb the residual costs, had no seat at all. The consultation tables were weighted toward platform operators and administering agencies.
% DISAPPEARANCE_RATIONALE: If the third category vanished overnight, millions of platform workers would lose injury and medical coverage they currently hold, platforms would face immediate reclassification litigation across every jurisdiction, and the employment/contractor binary would have to absorb the entire platform workforce — courts, insurance systems, and platform business models would all reorganize around whichever reading the resulting litigation produced.
% FOUNDING_PROBLEM: Platform work grew up outside both legal categories: workers performed real, controlled, economically dependent work with no injury coverage, no work-linked medical access, and no social protection at all, while platforms owed nothing under either the employment or the contractor frame.
% FOUNDING_PROBLEM_CORROBORATION: National labor statistics and pre-category injury records corroborate the coverage gap the category was built to close; cross-jurisdiction coverage reports from outside the benefiting parties corroborate that the gap persists for the retirement and career dimensions. Worker unions corroborate the original gap but dispute that the category's remedy matches its scale. The claim that the category as designed fully solves the founding problem is asserted only by platform operators and the administering agencies — no source outside the benefiting parties attests it.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58 at interval end): the arrangement transfers a real package to platforms — the difference between full-employment obligation and the partial category — while transferring real partial protection to workers, so ε is neither the near-zero of a pure coordination device nor the high value of outright status denial. Suppression is moderate-high (0.60): the category forecloses the employee-status route once enacted — courts defer to the category, arbitration clauses channel disputes inside it — and holding workers within the frame requires active enforcement that has intensified as organized challenges grow. Theater is moderate (0.40): the protective apparatus is genuine, but a growing share of the category's operation is legitimation — coverage statistics deployed to defeat reclassification claims rather than to extend protection. Accessibility_collapse is moderate (0.55): the employee alternative collapses once the category exists, but pure contracting, multi-platform exit, and conventional employment remain available. Resistance is moderate-high (0.60): union litigation and worker campaigns contest the category continuously. The claim and the metrics are authored independently: claimed_type tangled_rope because both a genuine coordination function (coverage extension to a work form outside the binary) and asymmetric extraction (the obligation gap accruing to platforms through the same structure) are structurally present and actively enforced. The measurement series run on one shared time grid (t = years since first statutory third-category adoption, t0 ≈ 2013) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the platform seat the category is a workable innovation that extended protection without destroying flexible, multi-platform work; from the worker seat the same structure delivers partial coverage while locking in the absence of retirement security and career development; from the regulator seat it is an administrable compromise that gives the agency a distinct mandate; from the union seat it is a litigation shield erected in the name of protection. The payer seat and the beneficiary/agenda-setter seat should compute different types from identical structural data — the platform seat sits near the beneficiary end with arbitrage exit, the worker seat near the target end with constrained exit. The engine computes this per-seat divergence from the power, exit, and role data; the divergence is the measurement, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_operators derive near the beneficiary end: they receive the obligation gap as retained revenue, appear in the beneficiaries array, and hold arbitrage-grade exit (restructure work allocation, shift jurisdictions). gig_platform_workers are dual-positioned — appearing in both the beneficiaries and victims arrays — so the structural derivation cannot resolve their net position from the declarations alone: the protections received pull d toward the beneficiary end while the foregone employment package and constrained exit push toward the target end. The override sets the powerless atom to d=0.68 to capture the net-target position the structural delta describes: foregone retirement and career value exceeds the partial coverage received, and trapped exit amplifies the extraction the workers bear. public_assistance_programs bear the diffuse residual costs (moderate-high d, institutional payer). platform_worker_unions are excluded rather than coordinated — the category's frame is the thing their claims are excluded by, so they feed suppression and resistance rather than the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — work-linked protection for a work form outside both categories — is live: the retirement and career-development gap persists inside the category, and coverage statistics confirm the delivered protections are real. No mandatrophy is declared, and the founding_problem_status × disappearance_verdict pair (live × world_rearranges) raises no capture flag. The lifecycle risk runs the other direction: the category could harden from a gap-filling compromise into a permanent floor that blocks convergence toward employment-level protection — visible in the rising theater_ratio and suppression_requirement series as legitimation and enforcement shares grow while the coordination share stagnates. The tangled_rope classification keeps both components on the books: reading the arrangement as pure extraction would erase the real coverage 86–91% of workers receive; reading it as pure coordination would erase the obligation gap that accrues to platforms through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the employment_boundary kernel — would instantiating a sibling reading change the structural classification of the same platform-labor arrangement?',
    'Comparative classification across jurisdictions that adopted different readings: classify the same arrangement under the formalist, hybrid, and substantive frames and compare the computed types.',
    'Under the formalist reading the category''s protections become voluntary benefits over a contractor baseline and ε collapses toward near zero; under the substantive reading the same arrangement computes as denial of employment status with the full package foregone, driving ε into the high range. The moderate ε is specific to the hybrid reading''s referent — the third category as enacted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Classification is reading-indexed: sibling readings of the employment boundary kernel would classify the same arrangement differently.').

omega_variable(
    scaffold_floor_trajectory,
    'Is the third category a transitional arrangement whose protections will converge toward employment-level coverage, or a steady-state floor that blocks convergence?',
    'Longitudinal tracking of the category''s coverage scope: whether retirement contributions, career-development provisions, and collective-bargaining rights are extended over time, or remain fixed while enforcement against employee-status claims intensifies.',
    'If floor, the category''s suppression component is structural lock-in and its classification drifts toward pure extraction as the coordination share atrophies; if transitional, the arrangement carries an implicit sunset toward full employment and the measured extraction is transition cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_floor_trajectory, empirical, 'Whether the hybrid category converges toward employment-level protection or ossifies as a permanent floor.').

omega_variable(
    counterfactual_baseline_ambiguity,
    'What protection level would platform workers hold absent the third category — near zero under pure contractor status, or full employment via litigation?',
    'Natural experiments in jurisdictions where courts rejected third categories and applied employment tests to platform work; compare worker coverage, claim volumes, and platform behavior before and after.',
    'If the counterfactual is employee status, the category''s extraction includes the entire foregone employment package and ε rises sharply; if the counterfactual is bare contracting, the category is a net protection transfer to workers and ε falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_baseline_ambiguity, empirical, 'The no-category counterfactual determines how much of the category''s value is protection versus foreclosure of the employment route.').

omega_variable(
    flexibility_preservation_claim,
    'Does sub-employment obligation calibration actually preserve the scheduling and multi-platform flexibility that justifies the obligation gap, or would full employment obligations leave platform work substantially intact?',
    'Compare work-supply behavior, platform entry and exit, and worker earnings in jurisdictions that extended employment status to platform work against those that maintained third categories.',
    'If flexibility survives full employment, the obligation gap is rent and ε rises; if full employment destroys the work form, part of the gap is the price of the work form itself and ε falls.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flexibility_preservation_claim, empirical, 'Testability of the flexibility-preservation justification for sub-employment obligations.').

omega_variable(
    confinement_mechanism_composition,
    'Is worker confinement within the third category maintained by structural devices (arbitration clauses, standing rules, judicial deference to the enacted category) or by internalized acceptance of the category as the natural home for platform work?',
    'Claim-rate tracking after structural devices are relaxed: when courts admit employee-status challenges, do workers pursue them at rates consistent with revealed preference, or does claim volume stay low regardless?',
    'If a large share is internalized, suppression persists after the structural devices are removed and effective suppression exceeds the structural measure; if structural, relaxing the devices releases the pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confinement_mechanism_composition, empirical, 'Structural versus internalized share of the category''s confinement of worker claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_security_reading_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hybrid_security_reading_tr_t2, employment_boundary__hybrid_security_reading, theater_ratio, 2, 0.23).
narrative_ontology:measurement(hybrid_security_reading_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(hybrid_security_reading_tr_t6, employment_boundary__hybrid_security_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(hybrid_security_reading_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(hybrid_security_reading_tr_t10, employment_boundary__hybrid_security_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(hybrid_security_reading_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.4).

% Extraction over time
narrative_ontology:measurement(hybrid_security_reading_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hybrid_security_reading_be_t2, employment_boundary__hybrid_security_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(hybrid_security_reading_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(hybrid_security_reading_be_t6, employment_boundary__hybrid_security_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement(hybrid_security_reading_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(hybrid_security_reading_be_t10, employment_boundary__hybrid_security_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(hybrid_security_reading_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_security_reading_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hybrid_security_reading_su_t2, employment_boundary__hybrid_security_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement(hybrid_security_reading_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(hybrid_security_reading_su_t6, employment_boundary__hybrid_security_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(hybrid_security_reading_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(hybrid_security_reading_su_t10, employment_boundary__hybrid_security_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(hybrid_security_reading_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, substantive_employment_reading).

% DUAL FORMULATION NOTE:
% The 'employment boundary' label decomposes into three structurally distinct constraints (ε-invariance family): formalist_employment_reading (no third category; ε assessed over the contractor baseline), this hybrid_security_reading (third category with partial obligations; moderate ε over the standing arrangement), and substantive_employment_reading (full employment status; high ε where that status is denied). This file instantiates only the hybrid reading. Direction of influence: the formalist reading's contractor baseline is the coverage gap the hybrid category was built to correct (upstream); the hybrid category's enactment is now cited in courts and legislatures against the substantive reading's employee-status claims (downstream pressure on the substantive reading's operating environment). Each family member links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
