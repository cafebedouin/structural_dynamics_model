% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__public_safety_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__public_safety_coordination, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: licensing_statute_mandate__public_safety_coordination
 *   human_readable: Occupational Credential Statutes as Consumer-Protection Competence Floor (Public-Safety Reading)
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   Statutory credential requirements condition lawful practice in dozens of
 *   occupations on approved training, examinations, and fees, with boards
 *   empowered to discipline licensees and criminal statutes backing the ban
 *   on unlicensed practice. This file instantiates the
 *   public_safety_coordination reading of the licensing_statute_mandate
 *   kernel: the arrangement exists to prevent consumer harm by guaranteeing a
 *   minimum competence floor in markets where buyers cannot cheaply verify
 *   skill. The epsilon referent is the standing arrangement — the credential
 *   statutes as they actually operate — assessed by this reading's own
 *   lights, which yields a moderate value: most measured cost reads as
 *   quality-assurance overhead, with a recognized excess band in low-risk
 *   occupations. Sibling readings (rent_seeking_suppression,
 *   graduated_access_filter) are separate constraint files over the same
 *   referent with their own epsilon values and victim sets, linked through
 *   network.affects_constraints. The claim/metric split is deliberate: the
 *   reading claims rope (shared quality threshold, net-benefiting
 *   participants) while the metrics record the friction the arrangement
 *   observably generates. KEY AGENTS (by structural relationship): -
 *   service_consumers: Primary intended beneficiary (moderate/constrained) —
 *   receives competence-floor assurance, pays elevated prices -
 *   incumbent_license_holders: Dual-positioned beneficiary/agenda-setter
 *   (organized/mobile) — collects wage premia, occupies board seats -
 *   uncredentialed_practitioners: Excluded payer (powerless/trapped) — barred
 *   from lawful practice, absent from rulemaking -
 *   priced_out_aspiring_entrants: Payer (powerless/constrained) — bears
 *   training, examination, and fee costs before market entry -
 *   state_licensing_authorities: Agenda-setter (institutional/constrained) —
 *   writes and enforces standards, fee-funded - labor_policy_analysts:
 *   Analytical observer (analytical/analytical) — measures harm, wage, and
 *   access effects
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__public_safety_coordination, 0.38).
domain_priors:suppression_score(licensing_statute_mandate__public_safety_coordination, 0.72).
domain_priors:theater_ratio(licensing_statute_mandate__public_safety_coordination, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(licensing_statute_mandate__public_safety_coordination, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__public_safety_coordination, rope).
narrative_ontology:human_readable(licensing_statute_mandate__public_safety_coordination, "Occupational Credential Statutes as Consumer-Protection Competence Floor (Public-Safety Reading)").
narrative_ontology:topic_domain(licensing_statute_mandate__public_safety_coordination, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__public_safety_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, service_consumers).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__public_safety_coordination, incumbent_license_holders).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, uncredentialed_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, priced_out_aspiring_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(licensing_statute_mandate__public_safety_coordination, service_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buy services — healthcare, childcare, electrical work, haircuts, legal help — in markets where they cannot personally verify a provider's training. The license is the main signal available to them. They pay prices that run above what an uncapped market would charge because the pool of legal providers is smaller. Hiring an unlicensed provider offering a lower price is not an option the law leaves open to them.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, service_consumers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, service_consumers, payer).

% Hold the credential the statute requires. Many serve on the boards that write entry rules, grade examinations, and discipline violations, alongside running their practices. The license signals their competence to customers and thins the field of competitors; professional associations they fund lobby legislatures on scope-of-practice bills. Moving to another state means re-qualifying, but the credential travels reasonably well.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, incumbent_license_holders, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, incumbent_license_holders, agenda_setter).

% Have working skills in a licensed trade but no credential — priced out of training, failed by the examination, or trained informally. Practicing anyway brings cease-and-desist orders, fines, confiscation of tools, and occasionally criminal charges. They are not invited to board consultations on the rules that bar them; their main channels of voice are direct lobbying of legislators and litigation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, uncredentialed_practitioners, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__public_safety_coordination, uncredentialed_practitioners, excluded).

% Want to enter a licensed occupation and face the full bill up front: tuition, hundreds to thousands of supervised unpaid hours, examination fees, and license fees — often a year or more of living expenses before the first legal paycheck. Those without family money or credit borrow or delay. Some finish training and still fail the licensing examination, absorbing the cost with no license to show.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, priced_out_aspiring_entrants, payer,
    powerless, biographical, constrained, national).

% Legislatures write the credential statutes; boards under them set detailed standards, issue and revoke licenses, and prosecute unlicensed practice. Most boards finance themselves from license fees rather than appropriations, so their operating budgets scale with how many people must be licensed and how much enforcement runs. Restructuring or abolishing a board requires new legislation.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, state_licensing_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Researchers and policy analysts who measure what credential rules do to wages, employment, prices, service quality, and who manages to get in. Their studies feed court briefs, sunset reviews, and reform bills. They hold no licenses at stake and enforce nothing.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__public_safety_coordination, labor_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__public_safety_coordination, incumbent_license_holders).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__public_safety_coordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the information-asymmetry problem in service markets where buyers cannot cheaply verify practitioner competence before purchase: a shared, verifiable quality threshold lets consumers trust minimum competence without conducting their own investigations, and gives practitioners a common standard to train against.
% TRANSFER_FUNCTION: Moves compliance costs — tuition, supervised hours, examination and renewal fees, continuing-education time — from current and aspiring practitioners to training institutions, testing bodies, and fee-funded boards, and moves pricing power to incumbents through restricted supply, so consumers pay above-market prices for licensed services.
% ABSENT_VOICES: Uncredentialed practitioners and priced-out aspiring entrants have no seat in board rulemaking, which is dominated by incumbent licensees; consumer representation on boards is typically thin. Their objection — that thresholds exceed what harm prevention requires and that the price of entry sorts by wealth — reaches legislatures only indirectly, through reform campaigns and litigation.
% DISAPPEARANCE_RATIONALE: Overnight repeal would flood licensed markets with unverified providers mid-stream, void board enforcement dockets, strand accredited training pipelines and their enrolled students, and reset wage structures across every licensed occupation — a large rearrangement even if the long-run equilibrium proved better.
% FOUNDING_PROBLEM: Progressive-era reformers confronted repeated consumer catastrophe from quackery and dangerous incompetence in medicine and the skilled trades — harm no buyer could detect in advance and tort law reached only after injury. Credential statutes were built to guarantee minimum competence where buyer vigilance fails.
% FOUNDING_PROBLEM_CORROBORATION: Independent health-services researchers and malpractice/liability datasets corroborate a live harm-prevention function in high-stakes occupations such as surgery and electrical work. For low-risk occupations, labor economists outside the benefiting parties find wage premia without measurable quality gains, and no external source attests an ongoing harm problem — corroboration is occupation-dependent, and the parties dispute which occupations the finding covers.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__public_safety_coordination, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__public_safety_coordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__public_safety_coordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__public_safety_coordination, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__public_safety_coordination, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__public_safety_coordination_tests).
:- end_tests(licensing_statute_mandate__public_safety_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.38: compliance costs (tuition, supervised hours, examination and renewal fees) are real and front-loaded on entrants, and restricted supply lifts consumer prices; this reading treats most of that as the price of a trustworthy competence signal while conceding a persistent excess band where thresholds outrun documented harm. Suppression 0.72: unauthorized-practice bans, board subpoena and fining power, and occasional criminal prosecution are the regime's load-bearing wall — descriptively high whatever the justification, and unscaled by scope or directionality in the engine's arithmetic. Theater_ratio 0.27: continuing-education mandates and board proceedings retain real disciplinary function while accumulating ritual components. Accessibility_collapse 0.48: alternatives (voluntary certification, cross-state reciprocity, informal provision) survive only partially. Resistance 0.52: active — economic-liberty litigation, sunset reviews, universal-recognition acts, reform coalitions. All three tracked series share one time grid (points 0-30, step 6); the rising trajectories record scope creep into low-risk occupations, fee-funded enforcement hardening, and a growing performative share. The claimed type (rope) is authored from this reading's structural understanding; the metrics are authored independently from the arrangement's observable operation, and any computed divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same statutes. The incumbent seat pays compliance costs but collects the wage premium and helps write the rules — from inside, the arrangement is a functioning quality institution it staffs. The uncredentialed-practitioner seat meets only the enforcement face — fines, cease-and-desist orders, confiscated tools — and experiences the same statutes as a closed door with no hearing attached. The consumer seat sits nearest symmetric: genuine assurance gained, elevated prices paid, no capacity to audit the trade-off. The analyst seat sees the whole structure and neither pays nor collects. These divergences fall out of the structural data (role, power, exit); nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: service_consumers (declared beneficiary, secondary payer, constrained exit) derive mid-low d — subsidized on the assurance margin, taxed on the price margin. incumbent_license_holders (beneficiary, mobile exit) derive near the beneficiary end. Victim declarations drive high directionality: uncredentialed_practitioners (victim, trapped) sit near the full-target end — no exit, full exposure to enforcement; priced_out_aspiring_entrants (victim, constrained) sit slightly below them. state_licensing_authorities appears in neither array, so the canonical fallback would misplace them; the override (institutional, d=0.25) encodes fee-funded self-financing — board budgets scale with the regime's breadth, making the authorities indirect gainers the structural derivation cannot see. labor_policy_analysts carry the analytical atom with no extraction exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — quackery-era consumer harm in medicine and the trades — is authored as contested, not dead: live where harm evidence is strong (surgery, electrical work), thin where it is not (braiding, floristry). Because status is contested and the disappearance verdict is world_rearranges, the mismatch consumer finds no dead-mandate-plus-dependency flag, and the piton path stays cold — consistent with the low theater ratio. The classification's job here is bidirectional: it blocks the rent-seeking sibling's temptation to read the entire arrangement as pure extraction (which would erase real harm prevention in high-stakes occupations), and it blocks this reading's temptation to certify pure coordination (which would erase the excess band and the rising extraction series). The rope claim plus nonzero, rising extractiveness keeps both errors visible as drift pressure toward tangled_rope rather than settling either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_omega,
    'Which reading of the licensing_statute_mandate kernel does the standing arrangement actually instantiate — public-safety coordination (this file), rent-seeking suppression, or graduated access filtering?',
    'Cross-state, cross-occupation comparison correlating threshold stringency with (a) measured consumer-harm rates, (b) incumbent wage premia, and (c) entrant demographics; whichever correlation is load-bearing selects the reading.',
    'Harm-rate correlation confirms this file''s rope structure; wage-premium dominance shifts the classification toward the rent-seeking sibling''s profile; demographic-sorting dominance shifts it toward the graduated-access sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_omega, conceptual, 'Which kernel reading the actual credential arrangement instantiates.').

omega_variable(
    harm_calibration_decoupling_omega,
    'Are entry thresholds (training hours, examinations, fees) calibrated to documented consumer-harm rates, or do they decouple from harm in low-risk occupations?',
    'Dose-response analysis of injury and complaint rates against training-hour requirements across occupations, controlling for intrinsic task risk.',
    'Calibration supports the rope claim; systematic decoupling adds an extraction component the engine will read as tangled_rope pressure on this reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_calibration_decoupling_omega, empirical, 'Whether threshold stringency tracks harm or outruns it.').

omega_variable(
    voluntary_certification_equivalence_omega,
    'Would voluntary certification combined with liability law deliver equivalent consumer protection at lower cost than mandatory licensure?',
    'Natural experiments in jurisdictions that repealed licensure for specific low-risk occupations, tracking complaint and injury rates before and after repeal.',
    'Equivalence implies the coercive layer adds suppression without added protection, degrading this reading''s rope toward tangled_rope or worse; a persistent protection gap implies the coercion purchases real coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_certification_equivalence_omega, empirical, 'Whether the mandatory layer is separable from the protective function.').

omega_variable(
    board_composition_capture_omega,
    'Do incumbent-dominated licensing boards set entry standards above harm-justified levels?',
    'Compare board-voted standard increases against staff harm analyses and external expert recommendations; examine sunset-review voting records for alignment with association positions.',
    'Confirmed capture converts the coordination machinery into extraction machinery and materially strengthens the rent-seeking sibling''s account of the same statutes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_composition_capture_omega, empirical, 'Whether the standard-setting process is captured by incumbents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__public_safety_coordination, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__public_safety_coordination, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t6, licensing_statute_mandate__public_safety_coordination, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(lice_tr_t6, observed).
narrative_ontology:measurement(lice_tr_t12, licensing_statute_mandate__public_safety_coordination, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(lice_tr_t12, observed).
narrative_ontology:measurement(lice_tr_t18, licensing_statute_mandate__public_safety_coordination, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(lice_tr_t18, observed).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__public_safety_coordination, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(lice_tr_t24, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__public_safety_coordination, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(lice_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t6, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 6, 0.29).
narrative_ontology:measurement_basis(lice_be_t6, observed).
narrative_ontology:measurement(lice_be_t12, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 12, 0.31).
narrative_ontology:measurement_basis(lice_be_t12, observed).
narrative_ontology:measurement(lice_be_t18, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 18, 0.34).
narrative_ontology:measurement_basis(lice_be_t18, observed).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 24, 0.36).
narrative_ontology:measurement_basis(lice_be_t24, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__public_safety_coordination, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(lice_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t6, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(lice_su_t6, observed).
narrative_ontology:measurement(lice_su_t12, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(lice_su_t12, observed).
narrative_ontology:measurement(lice_su_t18, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 18, 0.65).
narrative_ontology:measurement_basis(lice_su_t18, observed).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(lice_su_t24, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__public_safety_coordination, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(lice_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__public_safety_coordination, identity_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__rent_seeking_suppression).
narrative_ontology:affects_constraint(licensing_statute_mandate__public_safety_coordination, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'credential requirements protect consumers' decomposes, per the epsilon-invariance principle, into three structurally distinct claims — this file (public_safety_coordination, moderate epsilon, rope claim), licensing_statute_mandate__rent_seeking_suppression (high epsilon, extraction-centered victim set), and licensing_statute_mandate__graduated_access_filter (high epsilon, class-incidence victim set). This reading is upstream in legitimacy terms: its public-safety warrant is the premise the other two readings say is exploited or exceeded. Each family member links the others through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__public_safety_coordination, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
