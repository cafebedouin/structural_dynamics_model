% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Resource Claim Regime (Institutional Extraction Reading)
 *   domain: higher education governance / labor economics / institutional theory
 *
 * SUMMARY:
 *   A research university sector in which tenure converts an insurance
 *   arrangement written for an earlier fiscal era into a permanent property
 *   claim held by the cohorts who entered before the regime shifted. Senior
 *   faculty hold salary lines that survive enrollment declines, program
 *   closures, and leadership turnover; the institution manages the resulting
 *   rigidity not by reallocating protected lines but by expanding a
 *   contingent periphery — adjuncts, graduate teachers of record, and
 *   term-contract instructors who now constitute the instructional majority.
 *   The permanent core is defended through shared governance, contractual
 *   grievance machinery, and accreditor-legitimated staffing norms, while the
 *   periphery bears the flexibility costs the core sheds. Students pay
 *   through tuition growth and degraded instructional investment; staff
 *   absorb crowded-out operating budgets. This story authors epsilon for the
 *   standing tenure arrangement as the institutional-extraction reading
 *   assesses it — the arrangement under contest, not any replacement this
 *   reading would endorse. KEY AGENTS (by structural relationship):
 *   tenured_senior_faculty hold the permanent claims and defend them through
 *   governance votes; university_administrations administer the two-tier
 *   structure and collect its budget flexibility; tenure_track_junior_faculty
 *   pay probationary costs for a lottery ticket into the beneficiary class;
 *   contingent_adjunct_faculty bear the loaded flexibility costs with no
 *   governance voice; students_and_families absorb tuition and instructional
 *   degradation; departmental_support_staff are squeezed without a seat in
 *   the conversation; accreditation_and_policy_bodies observe the full
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.74).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.64).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Resource Claim Regime (Institutional Extraction Reading)").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher education governance / labor economics / institutional theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13').
narrative_ontology:cs_kernel_codification('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', formalized).
narrative_ontology:cs_authority_grounding('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', distributed).
narrative_ontology:cs_reading_relation('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', foundational, tenure_permits_capture_of_scarcity_rents_by_early_cohorts).
narrative_ontology:cs_axiom_status(tenure_permits_capture_of_scarcity_rents_by_early_cohorts, holdable).
narrative_ontology:cs_axiom_grounding('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', tenure_permits_capture_of_scarcity_rents_by_early_cohorts, empirically_contingent).
narrative_ontology:cs_axiom('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', foundational, flexibility_costs_externalized_to_contingent_labor).
narrative_ontology:cs_axiom_status(flexibility_costs_externalized_to_contingent_labor, holdable).
narrative_ontology:cs_axiom_grounding('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', flexibility_costs_externalized_to_contingent_labor, empirically_contingent).
narrative_ontology:cs_reference_frame('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', universal_permanent_claim_professoriate).
narrative_ontology:cs_drift_state('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', contemporary_adjunctification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ad3fd9c-6f9f-4e5d-b233-4fd5b94eae13', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_senior_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_adjunct_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, tenure_track_junior_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students_and_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, university_administrations).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenure_track_junior_faculty).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, internal_labor_market_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a permanent appointment acquired through a probationary period under earlier fiscal conditions. Salary and benefits continue through enrollment declines and program closures; the line is retired rather than reallocated. Votes in the senate and personnel committees that set hiring priorities and staffing ratios. Rank transfers on lateral moves but vested claims largely do not; leaving the profession forfeits the accumulated value of the claim, so exit exists but at high personal cost.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_senior_faculty, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_senior_faculty, agenda_setter).

% Provosts, deans, and HR administer appointment policy and cannot revoke vested lines without buyouts, litigation, and governance conflict. They manage fiscal shortfall by shifting teaching to the contingent pool, collecting the budget flexibility the two-tier structure generates while answering to trustees and legislatures for payroll growth they did not choose. They perpetuate the structure they cannot unwind.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administrations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administrations, beneficiary).

% Works a six-year probation at below-market wages carrying heavy teaching, service, and publication quotas, paying now for a probabilistic entry into the permanent-claim class. Years of specialist training create sunk costs; professional identity is fused with academic vocation, making exit feel like self-annulment. Whether the ticket pays depends on cohort timing and field fortunes decided elsewhere.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenure_track_junior_faculty, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenure_track_junior_faculty, beneficiary).

% Teaches on per-course contracts with no benefits, no governance voice, and semester-to-semester reappointment discretion held by others. Pieces together loads across multiple campuses within commuting range. Credential-specific skills, saturated academic labor markets, and in many cases visa dependence close exits; unionization campaigns meet organized employer resistance.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_adjunct_faculty, payer,
    powerless, immediate, trapped, regional).

% Carries tuition growth and larger contingent-taught sections as the protected salary share consumes the instructional budget. Can choose among institutions, but degree value is positional and switching is costly; has no vote anywhere on how salary lines are allocated.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students_and_families, payer,
    moderate, biographical, mobile, national).

% Absorbs the squeeze when rigid payroll crowds out operating budgets: hiring freezes, position eliminations, and stagnant wages land on staff first. Has no seat in the shared-governance bodies where the allocations are decided.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, departmental_support_staff, excluded,
    powerless, immediate, constrained, local).

% Accreditors, government statistical agencies, and policy institutes track faculty composition ratios and instructional spending shares, commission analyses, and can condition recognition on staffing standards. Sees the whole structure from outside any campus and profits from none of its flows.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, accreditation_and_policy_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_senior_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts uncertain decades-long investments in deep specialization into a bankable mutual commitment: the institution guarantees lifetime income security, scholars commit irreplaceably specific human capital to one institution, and senior faculty acquire durable stakes that stabilize institutional memory and internal governance.
% TRANSFER_FUNCTION: Moves permanent income claims, schedule autonomy, and priority over budget lines to the cohorts that secured tenure under earlier fiscal conditions; moves variable teaching loads, benefit-free per-course pay, and employment insecurity to the contingent tier; moves the resulting cost of rigid payroll to tuition payers and to crowded-out operating budgets.
% ABSENT_VOICES: Contingent instructors have no vote in the senates and committees where staffing ratios and salary lines are set; students have no seat in budget allocation; departmental staff absorb the squeeze without representation; the future students who will service the resulting debt are absent entirely.
% DISAPPEARANCE_RATIONALE: Overnight removal forces immediate renegotiation of every continuing appointment, unwinds retirement and sabbatical structures built on vested claims, redirects salary-line planning toward explicit term contracts, and reprices tuition as payroll rigidity dissolves. The academic labor market reorganizes around contract terms within a few budget cycles; nothing resembling the current two-tier equilibrium survives intact.
% FOUNDING_PROBLEM: Securing scholarly careers against arbitrary dismissal, political retaliation, and fiscal opportunism so that both scholars and institutions could rationally invest in specialization whose payoff matures over decades.
% FOUNDING_PROBLEM_CORROBORATION: Historical attestation is solid: the AAUP's 1940 Statement, state statute texts, and regental policy archives document the founding problem from sources predating the current fiscal regime. Corroboration that it remains the operative problem comes almost exclusively from beneficiary-side actors — faculty senates and disciplinary associations. From outside the beneficiary set, labor economists' dismissal-rate studies and legislative audit testimony indicate for-cause termination is rare and politically driven terminations are episodic, attesting that the founding problem has largely receded — which is precisely this reading's contention.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.74 at interval end) because the permanent claim is decoupled from current performance and financed increasingly by people who will never hold one; suppression is substantial (0.64) but structural rather than violent — contractual entrenchment, grievance procedures tilted toward incumbents, accreditor staffing norms, anti-union infrastructure in right-to-work jurisdictions, and visa dependence for international contingent faculty. Theater ratio crosses 0.5 late in the interval: annual review, dossier ritual, and post-tenure evaluation increasingly legitimate the claim rather than assess it, a Goodhart signal that proxy maintenance is displacing the evaluative function. Accessibility collapse is moderate (0.5): the alternative that flourished was peripheral expansion (contingency), while the alternative that collapsed is reallocation of vested lines, which contracts, buyout costs, and litigation render nearly inaccessible. Resistance is moderate-to-substantial (0.55): adjunct unionization campaigns, student debt politics, and legislative post-tenure-review pushes, meeting organized defense. The three measurement series run on one shared time grid (1970-2025) with every tracked metric authored at every examined point, so no series borrows another's end-state. Rising suppression_requirement models the deliberate maturation of enforcement machinery (union-suppression consulting, compliance management of the contingent workforce), not incidental drift. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structure. From the tenured senior seat the arrangement is deferred compensation honestly earned through a probationary gauntlet, administered through governance duties — a subsidized asset. From the contingent seat the same structure is a caste barrier: identical instructional labor, fraction of the pay, no claim on the institution, enforced by the senior seat's votes. From the administration seat it is a manageable liability whose fixed costs are offset by the flexible periphery the structure generates — which is why the administrator perpetuates what it cannot revoke. From the student seat it is an opaque cost driver embedded in tuition. Same-nominal-level dynamics matter: junior and senior faculty are nominally peers in one profession, yet cohort entry timing differentiates everything — the junior seat sits identity_locked behind sunk specialist training and a sunk probationary investment, while the senior seat retains constrained portability (rank travels, vested claims mostly do not). Inter-institutionally, administrations and accreditors face the same arrangement with different exit profiles: the administrator is bound by vested contracts, the accreditor holds analytical distance and can condition recognition on staffing standards.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured senior faculty sit at the beneficiary pole (d near zero): the constraint subsidizes them, and their constrained exit preserves the subsidy. Contingent adjunct faculty sit near the full-target pole (d near one): they bear the loaded flexibility costs with trapped exit. Junior tenure-track faculty are victims in the arrays but their payment purchases an option on future rents, so their derived directionality lands high but short of the adjuncts'. Students and families are declared victims with mobile exit, damping effective extraction toward the middle. University administrations are deliberately absent from the beneficiary and victim arrays — their position (net mild gainer: flexibility collected, fixed payroll borne) is invisible to a derivation keyed on those arrays and would fall to the canonical fallback, so a directionality override sets the institutional atom to d=0.38 with the reasoning recorded here. The accreditation observer holds the analytical atom and feeds no chi. Departmental staff carry role excluded: commentary-grade absence only, never a correction input.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is refusing both easy mislabels. A rope-only reading treats permanent claims as neutral coordination for long-horizon scholarship and renders the asymmetric transfer invisible; a snare-only reading denies the residual genuine coordination (deep-specialization finance and institutional memory) that still operates through the same structure. Tangled rope holds both: coordination function and asymmetric extraction ride one arrangement, actively enforced — hence requires_active_enforcement with named beneficiaries and victims. The mandate is NOT declared resolved: the founding problem (career security for long-horizon investment) is contested, not dead, so no mandatrophy_resolved flag is authored. The piton signature fails on its own test: a concentrated beneficiary exists (the senior cohort captures the gains), which excludes inertial-degradation classification regardless of the elevated theater ratio. The theater_ratio crossing 0.5 is documented as symptom, not verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_tenure_kernel,
    'This story instantiates only the institutional_extraction_reading of the tenure_contract kernel — would classifying the same kernel through the academic_freedom or demographic_reproduction readings yield structurally different constraints?',
    'Generate sibling stories for the same kernel and compare computed classifications; divergence in beneficiary/victim sets and epsilon locates the disagreement structurally.',
    'The academic_freedom_reading would author low epsilon with beneficiaries spanning scholars broadly and lean rope; the demographic_reproduction_reading would define victims demographically and relocate extraction into hiring criteria. This story''s verdict holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_tenure_kernel, conceptual, 'Committer-frame routing: this constraint is one reading of the tenure_contract kernel; sibling readings are separate files.').

omega_variable(
    claim_security_vs_extraction_separability,
    'Is permanent-claim security structurally necessary to finance long-horizon scholarship, or can equivalent insurance be delivered through long renewable-term contracts without perpetual claims?',
    'Compare research output and risk-taking across systems that replaced lifetime tenure with extended-term arrangements (UK post-1988, Dutch and German tenure tracks) against comparable US institutions holding field and funding constant.',
    'If separable, the permanent-claim component is excess extraction beyond coordination cost; if inseparable, part of the authored epsilon is the irreducible price of the coordination itself and the classification drifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claim_security_vs_extraction_separability, empirical, 'Whether the extraction component is structurally separable from the coordination function.').

omega_variable(
    fiscal_disinvestment_confound,
    'How much of the measured extraction reflects the tenure arrangement itself versus the parallel collapse of state appropriations that made contingent hiring fiscally attractive?',
    'Difference-in-differences across states with divergent appropriation trajectories holding governance structure constant; within-state before-and-after analysis of appropriation shocks.',
    'If disinvestment dominates, epsilon attributable to the tenure arrangement falls and the story approaches a rope under external fiscal pressure; if extraction persists controlling for funding, the structural reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_disinvestment_confound, empirical, 'Disentangling the constraint''s intrinsic extraction from its fiscal environment.').

omega_variable(
    reallocation_counterfactual_capture,
    'If vested claims were unwound, who would capture the freed resources — instructional spending, reserves, administration, or executive compensation?',
    'Trace expenditures at institutions that shrank tenured lines through retirements and buyouts; follow the marginal dollar from salary-line savings to its destination.',
    'If administrative capture dominates, the student cost-bearer claim weakens because reform would not relieve them, narrowing the extraction target to the contingent tier; if instructional spending rises, the reading''s student-cost claim is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_counterfactual_capture, empirical, 'Where released resources would actually flow under reform.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the contingent tier''s tolerance of precarity maintained by structural barriers (market saturation, visa dependence, geographic immobility) or by internalized acceptance (vocational identity normalizing the gauntlet as a meritocratic trial)?',
    'Post-exit tracking of former contingent instructors: if suppression perception and self-blame persist after leaving academia, the internalized component is substantial.',
    'Internalized suppression raises effective suppression above the structural measure and explains low exit despite negative expected returns; purely structural suppression predicts mass exit once alternatives appear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the suppression holding the two-tier order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1970, tenure_contract__institutional_extraction_reading, theater_ratio, 1970, 0.24).
narrative_ontology:measurement_basis(tenu_tr_t1970, observed).
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__institutional_extraction_reading, theater_ratio, 1980, 0.29).
narrative_ontology:measurement_basis(tenu_tr_t1980, observed).
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__institutional_extraction_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(tenu_tr_t1990, observed).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__institutional_extraction_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(tenu_tr_t2000, observed).
narrative_ontology:measurement(tenu_tr_t2008, tenure_contract__institutional_extraction_reading, theater_ratio, 2008, 0.45).
narrative_ontology:measurement_basis(tenu_tr_t2008, observed).
narrative_ontology:measurement(tenu_tr_t2017, tenure_contract__institutional_extraction_reading, theater_ratio, 2017, 0.49).
narrative_ontology:measurement_basis(tenu_tr_t2017, observed).
narrative_ontology:measurement(tenu_tr_t2025, tenure_contract__institutional_extraction_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(tenu_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1970, tenure_contract__institutional_extraction_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement_basis(tenu_be_t1970, observed).
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__institutional_extraction_reading, base_extractiveness, 1980, 0.54).
narrative_ontology:measurement_basis(tenu_be_t1980, observed).
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__institutional_extraction_reading, base_extractiveness, 1990, 0.59).
narrative_ontology:measurement_basis(tenu_be_t1990, observed).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__institutional_extraction_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement_basis(tenu_be_t2000, observed).
narrative_ontology:measurement(tenu_be_t2008, tenure_contract__institutional_extraction_reading, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement_basis(tenu_be_t2008, observed).
narrative_ontology:measurement(tenu_be_t2017, tenure_contract__institutional_extraction_reading, base_extractiveness, 2017, 0.72).
narrative_ontology:measurement_basis(tenu_be_t2017, observed).
narrative_ontology:measurement(tenu_be_t2025, tenure_contract__institutional_extraction_reading, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(tenu_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1970, tenure_contract__institutional_extraction_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement_basis(tenu_su_t1970, observed).
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__institutional_extraction_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement_basis(tenu_su_t1980, observed).
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__institutional_extraction_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement_basis(tenu_su_t1990, observed).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__institutional_extraction_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement_basis(tenu_su_t2000, observed).
narrative_ontology:measurement(tenu_su_t2008, tenure_contract__institutional_extraction_reading, suppression_requirement, 2008, 0.56).
narrative_ontology:measurement_basis(tenu_su_t2008, observed).
narrative_ontology:measurement(tenu_su_t2017, tenure_contract__institutional_extraction_reading, suppression_requirement, 2017, 0.61).
narrative_ontology:measurement_basis(tenu_su_t2017, observed).
narrative_ontology:measurement(tenu_su_t2025, tenure_contract__institutional_extraction_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(tenu_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'tenure' per the epsilon-invariance principle: one kernel (tenure_contract), three structurally distinct claims. academic_freedom_reading (upstream, historically supply-side legitimated) grounds tenure in truth-seeking protection with negligible extraction; demographic_reproduction_reading locates extraction in hiring criteria and defines victims demographically; this story, institutional_extraction_reading, locates it in permanent resource claims and defines victims economically. The upstream reading historically supplied the legitimacy that the downstream extraction critique contests — hence the influence edge from this story to the academic_freedom sibling, recorded in cs_structure.reading_relations. Each member carries independent epsilon, beneficiaries, and victims; the edges express family coupling only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
