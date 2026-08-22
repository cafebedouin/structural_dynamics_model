% ============================================================================
% CONSTRAINT STORY: employment_boundary__substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: employment_boundary__substantive_employment_reading
 *   human_readable: Substantive Employment Boundary: Dependence and Algorithmic Control Test
 *   domain: economic/labor/social policy
 *
 * SUMMARY:
 *   This story instantiates the substantive_employment_reading of the
 *   employment_boundary kernel as a single clean constraint: employment
 *   status is determined by economic dependence on the platform and
 *   algorithmic control over the work, regardless of what the contract says,
 *   and platforms whose workforces meet that test owe the full employer
 *   package — social contributions, wage floors, paid leave, severance, and
 *   access to collective labor institutions. The ε referent is this
 *   substantive boundary rule itself, the standing arrangement under contest,
 *   assessed by this reading's own lights: the rule compels large, real
 *   transfers from platform operators while performing a genuine pooling
 *   function (uniform protection financing, one administrable line against
 *   contract-shopping), so ε is authored moderate rather than near-zero — the
 *   reading endorses the transfer as corrective, but endorsement does not
 *   zero the structural burden the rule places on those it governs. The claim
 *   (tangled_rope) is authored independently of the metrics: the rule
 *   coordinates (one boundary, pooled insurance, no reward for relabeling
 *   dependent work as contracting) and transfers asymmetrically (operators
 *   pay; workers and funds receive) through the same structure, held in place
 *   by active enforcement. Relative to the pre-reading arrangement, this
 *   reading moves platform workers from the exposed side of the precarity
 *   regime into the protected class and moves operators into the obligated,
 *   cost-bearing seat. Sibling readings — formalist_employment_reading and
 *   hybrid_security_reading — are separate constraints with their own ε,
 *   beneficiary sets, and classifications, linked through
 *   network.affects_constraints; the kernel contest is documented in the
 *   omegas, never folded into this constraint's classification.
 *
 * KEY AGENTS:
 *   - platform_workers: primary beneficiary (powerless/trapped) — the protected class the boundary is drawn around; gains contributions, wage floors, leave, and severance rights
 *   - platform_operators: primary payer (institutional/arbitrage) — bears the employer package and contests the boundary through courts, ballots, and contract redesign
 *   - labor_courts_and_inspectorates: agenda_setter (institutional/constrained) — applies the dependence and control tests, awards back-contributions, sizes enforcement
 *   - social_insurance_funds: beneficiary (institutional/trapped) — receives the employer and worker contribution flow on reclassified earnings
 *   - compliant_platforms: secondary beneficiary (institutional/constrained) — gains a level field as misclassifying rivals' cost advantage closes
 *   - consumers_of_platform_services: payer with a beneficiary side (organized/mobile) — absorbs pass-through pricing against continued service from an insured workforce
 *   - flexibility_preferring_workers: beneficiary with a payer side (moderate/mobile) — trades schedule autonomy for protections
 *   - micro_platform_operators: excluded voice (moderate/constrained) — bears fixed compliance costs with no seat where thresholds and phase-ins are set
 *   - independent_labor_economists: analytical observer — measures misclassification prevalence and the incidence of reclassification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__substantive_employment_reading, 0.52).
domain_priors:suppression_score(employment_boundary__substantive_employment_reading, 0.55).
domain_priors:theater_ratio(employment_boundary__substantive_employment_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(employment_boundary__substantive_employment_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__substantive_employment_reading, "Substantive Employment Boundary: Dependence and Algorithmic Control Test").
narrative_ontology:topic_domain(employment_boundary__substantive_employment_reading, "economic/labor/social policy").

domain_priors:requires_active_enforcement(employment_boundary__substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__substantive_employment_reading, '6049f48c-686b-42a7-9012-e0b31e8ebd11').
narrative_ontology:cs_kernel_codification('6049f48c-686b-42a7-9012-e0b31e8ebd11', formalized).
narrative_ontology:cs_authority_grounding('6049f48c-686b-42a7-9012-e0b31e8ebd11', lineage).
narrative_ontology:cs_interpretation_layer_present('6049f48c-686b-42a7-9012-e0b31e8ebd11').
narrative_ontology:cs_reading_relation('6049f48c-686b-42a7-9012-e0b31e8ebd11', employment_boundary__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('6049f48c-686b-42a7-9012-e0b31e8ebd11', employment_boundary__hybrid_security_reading, forecloses).
narrative_ontology:cs_axiom('6049f48c-686b-42a7-9012-e0b31e8ebd11', foundational, employment_status_follows_economic_substance).
narrative_ontology:cs_axiom_status(employment_status_follows_economic_substance, holdable).
narrative_ontology:cs_axiom_grounding('6049f48c-686b-42a7-9012-e0b31e8ebd11', employment_status_follows_economic_substance, instrumental).
narrative_ontology:cs_axiom('6049f48c-686b-42a7-9012-e0b31e8ebd11', foundational, algorithmic_control_constitutes_employer_authority).
narrative_ontology:cs_axiom_status(algorithmic_control_constitutes_employer_authority, holdable).
narrative_ontology:cs_axiom_grounding('6049f48c-686b-42a7-9012-e0b31e8ebd11', algorithmic_control_constitutes_employer_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('6049f48c-686b-42a7-9012-e0b31e8ebd11', dependence_based_protective_boundary).
narrative_ontology:cs_drift_state('6049f48c-686b-42a7-9012-e0b31e8ebd11', contemporary_platform_economy, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6049f48c-686b-42a7-9012-e0b31e8ebd11', '').
narrative_ontology:cs_kernel_id(employment_boundary__substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, social_insurance_funds).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, compliant_platforms).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, platform_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, consumers_of_platform_services).
narrative_ontology:constraint_beneficiary(employment_boundary__substantive_employment_reading, flexibility_preferring_workers).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, consumers_of_platform_services).
narrative_ontology:constraint_victim(employment_boundary__substantive_employment_reading, flexibility_preferring_workers).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, economic_dependence_test).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, algorithmic_control_test).
narrative_ontology:constraint_vindicates(employment_boundary__substantive_employment_reading, primacy_of_substance_over_contract_form).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drive, deliver, courier, or complete tasks dispatched through platform apps. Pay, route and order allocation, and deactivation are set unilaterally by algorithms and rating systems. Most lack sick pay, paid leave, pension accrual, and a wage floor, and absorb vehicle, fuel, and insurance costs themselves. Income from one or two platforms covers essentials, so leaving means giving up rent money, not switching to an equivalent protected job.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_workers, beneficiary,
    powerless, biographical, trapped, global).

% Run the apps and write the contract terms, pay algorithms, and deactivation rules. Where the dependence-and-control test applies, they carry employer-level costs: social contributions, wage floors, paid leave, severance funds, and joint liability for subcontracted fleets. They contest the classification in courts, fund ballot campaigns for carve-outs, and redesign contracts and work flows to reduce the dependence indicators courts look for.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, platform_operators, payer,
    institutional, generational, arbitrage, global).

% Employment tribunals, labor courts, and inspectorates apply the dependence and control tests case by case, issue reclassification rulings with back-contributions, and run inspection programs over platform fleets. They can reshape the boundary's practical reach through doctrine but are bound by statutory text and precedent, and their capacity is sized by legislative budget cycles.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, labor_courts_and_inspectorates, agenda_setter,
    institutional, generational, constrained, national).

% Public pension, health, and unemployment insurance agencies receive employer and worker contributions on reclassified earnings. Each reclassified cohort widens their contribution base and reduces outlays for unprotected workers who fall back on public assistance. They have no existence apart from the contribution system they administer.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, social_insurance_funds, beneficiary,
    institutional, generational, trapped, national).

% Operators that already provide contributions, wage floors, and leave to their workforces. When misclassifying competitors are reclassified, the cost gap that undercut them closes; they support enforcement that raises rivals' costs to their own level.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, compliant_platforms, beneficiary,
    institutional, biographical, constrained, global).

% Ride-hail and delivery customers. Fares and fees rise where labor costs are passed through; they also get continued service from a workforce with sick pay and accident coverage instead of uninsured deactivations. Switching to taxis or cooking at home is easy, which limits how much of the cost they will absorb.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, consumers_of_platform_services, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, consumers_of_platform_services, beneficiary).

% Workers who took platform work for schedule control — students, carers, people between jobs. Reclassification brings them contributions and a wage floor but also scheduling obligations, exclusivity pressures, and earnings structures that can reduce the hours they used to cherry-pick. They gain protections on paper and can lose the autonomy they were effectively paid in.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, flexibility_preferring_workers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(employment_boundary__substantive_employment_reading, flexibility_preferring_workers, payer).

% Small local delivery and ride-hail startups. Fixed compliance costs — payroll systems, contributions administration, legal review — weigh on them far more than on incumbents, and they have no seat in the consultations where thresholds and phase-ins are set. Several have exited or sold to larger rivals after reclassification rulings.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, micro_platform_operators, excluded,
    moderate, biographical, constrained, regional).

% Academic and statistical researchers measuring misclassification prevalence and the effects of reclassification on earnings, prices, and insurance coverage. Their studies feed courts, ministries, and international labor bodies; they collect nothing from the boundary's operation and publish findings that cut both ways.
narrative_ontology:constraint_stakeholder(employment_boundary__substantive_employment_reading, independent_labor_economists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__substantive_employment_reading, platform_workers).
narrative_ontology:fixing_cost_class(employment_boundary__substantive_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one uniform, administrable criterion for who bears employer obligations — social contributions, wage floors, paid leave, severance — so that protection financing is pooled across the labor market instead of depending on each platform's contract drafting, and so that no firm can undercut protected competitors by relabeling dependent work as contracting.
% TRANSFER_FUNCTION: Moves employer-paid social contributions, wage-top-ups, paid-leave and severance costs from platform operators to platform workers and social insurance funds; moves scheduling and dismissal authority from unilaterally controlled algorithms to statutorily bounded management; moves adjudication of work relationships from platform terms of service to labor courts and inspectorates.
% ABSENT_VOICES: Micro-platform operators and would-be entrants are under-represented in hearings and consultations dominated by large platforms, unions, and ministries; they would argue fixed compliance costs entrench incumbents and would ask for phase-ins. Workers who chose gig work for schedule autonomy speak through referenda and surveys but hold no formal seat in legislative drafting. Workers in adjacent informal trades just below the dependence threshold are absent entirely and would ask why protection stops at the algorithmic-control line.
% DISAPPEARANCE_RATIONALE: If the substantive boundary vanished overnight, platforms would revert to universal contractor classification, social insurance funds would lose a fast-growing contributor cohort while unemployment and health outlays for unprotected gig workers rose, compliant platforms would lose the level field they currently price against, and the protections won since the mid-2010s — the UK Uber ruling, Spain's Riders Law, the EU platform-work directive — would unwind into renewed misclassification litigation.
% FOUNDING_PROBLEM: Labor-law protections were built on a binary — employee (protected) or independent contractor (unprotected) — assessed by contract form and direct supervision. Platform firms wrote contracts placing their workforces in the unprotected category while managing them through algorithms that set pay, allocation, and deactivation, leaving a fast-growing class of dependent workers outside social insurance exactly as stable employment contracted.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by ILO and OECD platform-economy reports, national labor inspectorate findings on misclassification, and the factual findings of the UK Supreme Court (Uber v Aslam) and the Court of Justice of the EU on platform control and dependence. Platform operators attest the opposite — that their workers are genuinely independent and prefer flexibility — so the problem's persistence is corroborated by courts and statistical agencies against the paying party's denial, not by the constraint's own beneficiaries.
narrative_ontology:disappearance_verdict(employment_boundary__substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__substantive_employment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__substantive_employment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__substantive_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__substantive_employment_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.52 at interval end) because the rule's transfers are large and real — employer social contributions commonly add a fifth to a third of payroll in European systems, plus wage floors, leave accrual, and severance exposure — but they run through a structure with an unmistakable coordination core: one administrable boundary, pooled protection financing, and no payoff for relabeling dependent work. Suppression (0.55) is the rule's coercive maintenance load, not participant preference: compliance does not emerge voluntarily, and persistence depends on inspectorates, reclassification litigation with back-contributions, and statutory overrides of platform contract design. Suppression is authored as a raw structural property — the engine, not the author, scales extractiveness by directionality and scope. Theater is low (0.18): the protections delivered are real; the performative share is compliance formalism (scheduling paperwork, documentation built to defeat the tests) that grows slowly as platforms learn to paper around the criteria. Accessibility_collapse (0.50) is partial by construction of the contest: the contractor-classification alternative collapses for dependent, algorithmically managed work, but work-restructuring exits — genuine multi-homing, higher per-task rates, automation, jurisdictional arbitrage — remain open, and that is exactly where platform resistance concentrates. Resistance (0.65) is high and organized: constitutional challenges, ballot-measure carve-outs, and continuous contract redesign. All three tracked series run on one shared eight-point grid (1995–2025) so every metric is authored at every examined time point; the trajectories are monotonic ratchets — covered scale and enforcement machinery grew together — not cycles, so no intermittent-reinforcement reading is warranted. Receipt surface: the transfer demonstrably accrues to platform_workers (wage floors, credited contributions, leave), so gain_flow names that seat rather than diffuse; fixing_cost is prohibitive because unwinding the boundary now requires legislatures to dismantle codified social-insurance financing that funds already budget against and to revoke directive-level commitments — a cost exceeding any benefit to the paying seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute different types from the same rule. From platform_operators the boundary operates as a compelled transfer they did not price into their unit economics, enforced by inspectors and courts against continuous redesign — the payer seat computes heavy extraction. From platform_workers the same rule is the arrival of sick pay, credited contributions, and a floor — the beneficiary seat computes subsidy or near-symmetry with gains. labor_courts_and_inspectorates experience it as boundary administration: doctrine to apply and dockets to size. compliant_platforms experience it as competitive correction that closes the cost gap rivals exploited. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations drive the derivation: platform_workers, social_insurance_funds, and compliant_platforms sit near the beneficiary end (low d) — the rule subsidizes them. platform_operators sit near the target end (high d) as the declared victim seat, with arbitrage-grade exit (work restructuring, automation, jurisdictional shopping, carve-out campaigns) damping effective extraction somewhat but not to symmetry, because every exit route degrades the platform model itself. consumers_of_platform_services sit near symmetric: pass-through prices against continued service from an insured workforce. flexibility_preferring_workers are pulled partway toward the payer side by autonomy losses the protections do not refund. No directionality_overrides are authored: the beneficiary/victim declarations plus the exit atoms produce the right structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabels. Reading the rule as pure extraction would erase its coordination core — pooled insurance financing and one uniform boundary that stops a race to the bottom via relabeling; the operators' payments buy protections that verifiably reach workers. Reading it as pure coordination would erase the asymmetry — operators pay without a symmetric return and must be compelled. Tangled_rope holds both halves. On mandatrophy: the founding problem (dependent, algorithmically managed work outside protection) is live wherever the reading does not govern, so no sunset applies and the constraint is steady-state rather than transitional. The atrophy risk runs the other way: if platforms restructure work toward genuine independence (real multi-homing, negotiated rather than algorithmic rates), the dependence test's covered class thins and enforcement could persist theatrically around a shrinking object — a drift toward inertial maintenance that the theater_ratio series would register. The founding_problem_status x disappearance_verdict pair (live x world_rearranges) is coherent: the problem persists and existing arrangements depend on the rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the substantive_employment_reading of the employment_boundary kernel. Would the formalist or hybrid sibling readings carve the same working relationships into different protected and unprotected sets, and is this reading''s victim/obligation structure (workers protected, operators obligated) the boundary''s true structure?',
    'Comparative institutional analysis across jurisdictions that have adopted different readings (contract-form tests, dependence-and-control tests, tailored third categories): track which working relationships each reading protects and at what fiscal and earnings incidence.',
    'Under the hybrid reading this constraint''s covered class shrinks to whatever the third category does not absorb and its obligations are tailored rather than full, lowering the transfer; under the formalist reading platform workers exit the protected set entirely and this constraint''s beneficiary and victim structures invert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the employment boundary the dependence-and-control rule instantiates, and what siblings would change structurally.').

omega_variable(
    dependence_threshold_indeterminacy,
    'Where does economic dependence begin — what income share, exclusivity, and control intensity make a platform worker an employee rather than a genuinely independent contractor?',
    'Distributional analysis of platform workers'' income shares, multi-homing rates, and algorithmic control intensity, validated against reclassification outcomes in jurisdictions with codified thresholds.',
    'A high threshold narrows the covered class and shrinks the transfer; a low threshold sweeps in genuinely independent contractors, raising operators'' burdens for relationships this reading''s own rationale does not reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependence_threshold_indeterminacy, empirical, 'The dependence threshold that fixes the boundary''s reach.').

omega_variable(
    algorithmic_control_equivalence,
    'Is algorithmic management structurally equivalent to the hierarchical employer control the employment category was built for, or a novel control form the employee category fits only by analogy?',
    'Comparative analysis of algorithmic management against classical supervision on scheduling discipline, unilateral pay changes, rating-based deactivation, and dismissal power.',
    'If algorithmic control is not equivalent in kind, this reading''s second foundational axiom weakens and the hybrid third-category reading gains force; if equivalent, the employee category applies without strain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_equivalence, conceptual, 'Whether algorithmic control constitutes employer authority in kind — the located point of the kernel disagreement.').

omega_variable(
    flexibility_tradeoff_magnitude,
    'How large are the autonomy losses reclassification imposes on workers who chose platform work for schedule control, relative to the protection gains?',
    'Longitudinal worker surveys and quasi-experimental comparisons of hours, earnings, and schedule autonomy before and after reclassification mandates (Spain''s Riders Law, the UK Uber ruling''s aftermath).',
    'If autonomy losses are large, the worker seat moves partway from protected beneficiary toward bearing real costs and the net transfer to workers shrinks; if small, the beneficiary structure holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_tradeoff_magnitude, empirical, 'Size of the protection-for-autonomy trade among covered workers.').

omega_variable(
    compliance_cost_entrenchment,
    'Does the fixed-cost structure of compliance entrench large platforms against entrants, converting part of the protection transfer into an incumbent advantage?',
    'Entry-rate and market-concentration analysis in platform markets before and after reclassification mandates; compare jurisdictions that phase in obligations for small operators.',
    'If entrenchment is real, compliant_platforms'' benefit is partly anticompetitive and the boundary carries a secondary burden on entrants that the current stakeholder set only partially registers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_entrenchment, empirical, 'Whether compliance costs double as an entry barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__substantive_employment_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t1995, employment_boundary__substantive_employment_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement_basis(empl_tr_t1995, observed).
narrative_ontology:measurement(empl_tr_t2000, employment_boundary__substantive_employment_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement_basis(empl_tr_t2000, observed).
narrative_ontology:measurement(empl_tr_t2005, employment_boundary__substantive_employment_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement_basis(empl_tr_t2005, observed).
narrative_ontology:measurement(empl_tr_t2010, employment_boundary__substantive_employment_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement_basis(empl_tr_t2010, observed).
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__substantive_employment_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement_basis(empl_tr_t2015, observed).
narrative_ontology:measurement(empl_tr_t2018, employment_boundary__substantive_employment_reading, theater_ratio, 2018, 0.16).
narrative_ontology:measurement_basis(empl_tr_t2018, observed).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__substantive_employment_reading, theater_ratio, 2021, 0.17).
narrative_ontology:measurement_basis(empl_tr_t2021, observed).
narrative_ontology:measurement(empl_tr_t2025, employment_boundary__substantive_employment_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(empl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(empl_be_t1995, employment_boundary__substantive_employment_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(empl_be_t1995, observed).
narrative_ontology:measurement(empl_be_t2000, employment_boundary__substantive_employment_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement_basis(empl_be_t2000, observed).
narrative_ontology:measurement(empl_be_t2005, employment_boundary__substantive_employment_reading, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement_basis(empl_be_t2005, observed).
narrative_ontology:measurement(empl_be_t2010, employment_boundary__substantive_employment_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement_basis(empl_be_t2010, observed).
narrative_ontology:measurement(empl_be_t2015, employment_boundary__substantive_employment_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(empl_be_t2015, observed).
narrative_ontology:measurement(empl_be_t2018, employment_boundary__substantive_employment_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement_basis(empl_be_t2018, observed).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__substantive_employment_reading, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement_basis(empl_be_t2021, observed).
narrative_ontology:measurement(empl_be_t2025, employment_boundary__substantive_employment_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(empl_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t1995, employment_boundary__substantive_employment_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement_basis(empl_su_t1995, observed).
narrative_ontology:measurement(empl_su_t2000, employment_boundary__substantive_employment_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement_basis(empl_su_t2000, observed).
narrative_ontology:measurement(empl_su_t2005, employment_boundary__substantive_employment_reading, suppression_requirement, 2005, 0.36).
narrative_ontology:measurement_basis(empl_su_t2005, observed).
narrative_ontology:measurement(empl_su_t2010, employment_boundary__substantive_employment_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement_basis(empl_su_t2010, observed).
narrative_ontology:measurement(empl_su_t2015, employment_boundary__substantive_employment_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement_basis(empl_su_t2015, observed).
narrative_ontology:measurement(empl_su_t2018, employment_boundary__substantive_employment_reading, suppression_requirement, 2018, 0.49).
narrative_ontology:measurement_basis(empl_su_t2018, observed).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__substantive_employment_reading, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement_basis(empl_su_t2021, observed).
narrative_ontology:measurement(empl_su_t2025, employment_boundary__substantive_employment_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(empl_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__substantive_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three structurally distinct constraints, not one constraint with a measurement parameter. This story authors ε for the substantive rule (dependence + algorithmic control => employee status): moderate, because the rule compels large transfers from operators through a structure with genuine pooling. The formalist_employment_reading authors ε for the contract-form boundary it endorses — near-zero from its own lights, though a critical author would measure the standing contractor regime's precarity costs. The hybrid_security_reading authors ε for a tailored third category — lower transfer, narrower covered class, different victim set. The readings are linked here per the ε-invariance principle: separate stories, separate ε, separate stakeholders, with this reading exerting upstream doctrinal pressure on the hybrid (courts that adopt full employment status shrink the space a third category can occupy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
