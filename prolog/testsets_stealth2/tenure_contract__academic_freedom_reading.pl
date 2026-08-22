% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Credible Protection for Unpopular Inquiry (Academic-Freedom Reading)
 *   domain: higher_education_governance/labor_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the tenure_contract kernel: the
 *   academic-freedom reading, on which tenure's core function is a credible
 *   institutional commitment that researcher survival does not depend on
 *   institutional displeasure or political backlash, enabling high-risk and
 *   long-horizon inquiry. Per the epsilon-referent rule, extractiveness is
 *   authored for the standing tenure arrangement as this reading assesses it
 *   - not for any arrangement the reading would prefer, and not averaged
 *   across sibling readings. The sibling readings
 *   (institutional_extraction_reading: permanence as rent secured by early
 *   winners, loading costs onto contingent labor;
 *   demographic_reproduction_reading: peer review as demographic gatekeeping
 *   through fit and collegiality criteria) are separate constraint files
 *   linked through network.affects_constraints; their content is deliberately
 *   NOT folded into this file's metrics. The claim/metrics gap is deliberate:
 *   this reading CLAIMS rope (protective coordination whose participants are
 *   net beneficiaries) while the authored metrics record modest but real and
 *   slowly accumulating costs - probationary intensification, rigidity,
 *   performative post-tenure review - plus a rising enforcement burden. The
 *   engine computes per-seat classifications from the structural data;
 *   divergence between claim and computed type is the measurement the corpus
 *   exists to take.
 *
 * KEY AGENTS:
 *   - - tenured_faculty: Primary beneficiary (organized/constrained) - holds peer-adjudicated continuing appointment; runs the evaluation machinery
 *   - - pre_tenure_faculty: Beneficiary-in-waiting carrying the probationary burden (moderate/constrained)
 *   - - doctoral_students: Apprentice beneficiary bearing instructional and laboratory labor (powerless/constrained)
 *   - - university_students: Indirect beneficiary via instruction and research quality (powerless/mobile)
 *   - - long_horizon_science_sponsors: Institutional beneficiary that cannot protect researchers itself (institutional/arbitrage)
 *   - - university_governing_boards: Agenda setter bearing the rigidity costs (institutional/arbitrage)
 *   - - elected_officials_and_major_donors: Primary payer - steering lever removed (powerful/arbitrage)
 *   - - aaup_and_scholarly_societies: Analytical observer maintaining the doctrinal lineage (organized/analytical)
 *   - - adjunct_instructors: Excluded voice - teaches the curriculum, holds no governance seat (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.34).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.42).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Credible Protection for Unpopular Inquiry (Academic-Freedom Reading)").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '9ce18502-5dee-4ffd-a38c-c99fcf54909e').
narrative_ontology:cs_kernel_codification('9ce18502-5dee-4ffd-a38c-c99fcf54909e', formalized).
narrative_ontology:cs_authority_grounding('9ce18502-5dee-4ffd-a38c-c99fcf54909e', lineage).
narrative_ontology:cs_interpretation_layer_present('9ce18502-5dee-4ffd-a38c-c99fcf54909e').
narrative_ontology:cs_reading_relation('9ce18502-5dee-4ffd-a38c-c99fcf54909e', tenure_contract__institutional_extraction_reading, influences).
narrative_ontology:cs_reading_relation('9ce18502-5dee-4ffd-a38c-c99fcf54909e', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('9ce18502-5dee-4ffd-a38c-c99fcf54909e', foundational, survival_independence_required_for_truth_seeking).
narrative_ontology:cs_axiom_status(survival_independence_required_for_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('9ce18502-5dee-4ffd-a38c-c99fcf54909e', survival_independence_required_for_truth_seeking, instrumental).
narrative_ontology:cs_axiom('9ce18502-5dee-4ffd-a38c-c99fcf54909e', foundational, termination_requires_peer_adjudicated_cause).
narrative_ontology:cs_axiom_status(termination_requires_peer_adjudicated_cause, holdable).
narrative_ontology:cs_axiom_grounding('9ce18502-5dee-4ffd-a38c-c99fcf54909e', termination_requires_peer_adjudicated_cause, conventional).
narrative_ontology:cs_reference_frame('9ce18502-5dee-4ffd-a38c-c99fcf54909e', protected_inquiry_baseline).
narrative_ontology:cs_drift_state('9ce18502-5dee-4ffd-a38c-c99fcf54909e', contemporary_politicization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9ce18502-5dee-4ffd-a38c-c99fcf54909e', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, pre_tenure_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, doctoral_students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, university_students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, long_horizon_science_sponsors).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, elected_officials_and_major_donors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, pre_tenure_faculty).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, doctoral_students).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, academic_freedom_doctrine).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, peer_review_legitimacy_norm).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, shared_governance_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold continuing appointments that can be ended only for cause adjudicated by faculty peers under published procedures. Choose research and teaching agendas without needing trustee, donor, or legislative approval. Staff the promotion-and-tenure committees and senates that administer the arrangement. Leaving means relocating institutions or leaving academic life, at real cost to salary, colleagues, and place.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, tenured_faculty, agenda_setter).

% Serve multi-year probationary appointments evaluated intensively for research, teaching, and service, with departure at the end if the case fails. Carry heavy course and committee loads while building the dossier the senior protection requires. The promised endpoint is the same continuing appointment seniors hold. Exiting mid-stream to industry or another institution forfeits years of accumulated progress.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, pre_tenure_faculty, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, pre_tenure_faculty, payer).

% Apprentice under mentors whose positions are secure, completing dissertations while staffing courses and laboratories at stipend-level pay. Absorb the working norm that inquiry is insulated from reprisal. Hold no vote in the governance bodies that set personnel policy; switching programs or advisors restarts progress and costs years.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, doctoral_students, beneficiary,
    powerless, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, doctoral_students, payer).

% Enroll in courses taught by instructors free to present contested material without employer reprisal, and benefit from research produced under long-horizon conditions. Pay tuition that funds committed salary lines. Have little standing in personnel decisions; their practical recourse is transferring or choosing among institutions.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_students, beneficiary,
    powerless, immediate, mobile, national).

% Agencies and foundations funding multi-decade research programs depend on scholars willing to pursue unfashionable or risky questions. They cannot shield an individual researcher from employer or political retaliation, so they rely on the employment arrangement itself to supply that insulation. Their grant portfolios continue across changes of government.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, long_horizon_science_sponsors, beneficiary,
    institutional, civilizational, arbitrage, global).

% Set tenure policy, approve appointments, and control budgets; once an appointment is granted they are bound by handbook commitments and shared-governance norms. They can initiate post-tenure review, invoke financial-exigency retrenchment, or attempt policy change, but each path runs through faculty bodies and legal procedure. Bear the cost of salary lines that persist regardless of enrollment or political weather.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_governing_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Seek to steer public research and teaching through appropriations, board appointments, and gift conditions. The continuing-appointment structure removes their direct lever over individual scholars, so pressure lands on administrators, probationary faculty, and non-tenured staff instead. They retain ways to redirect money, build parallel institutes, or time campaigns to budget crises, but cannot reach the protected core by ordinary persuasion.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, elected_officials_and_major_donors, payer,
    powerful, immediate, arbitrage, national).

% Maintain the doctrinal lineage of academic-freedom and shared-governance statements, investigate and censure institutions that dismiss faculty without cause, and publish interpretations that guide local practice. Observe the arrangement from an analytical seat: they neither grant appointments nor bear payroll.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, aaup_and_scholarly_societies, observer,
    organized, generational, analytical, national).

% Teach a large share of undergraduate courses on term-to-term contracts with no continuing-appointment prospect. Have no seat in the senates and committees that set personnel policy, though the policy shapes their workplace directly. Would press for just-cause standards extending beyond the senior core; their participation is limited to occasional comment periods.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, adjunct_instructors, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem of knowledge production: research careers require decades-long bets on questions whose value is uncertain and whose answers may embarrass funders, trustees, or electoral majorities. By converting 'we will not dismiss you for your findings' into a published, procedurally enforceable commitment, the arrangement lets scholars undertake high-risk, long-horizon, and unpopular lines of work that dismissal-at-will employment would deter.
% TRANSFER_FUNCTION: Moves employment security and agenda-setting authority from institutional and political discretion to individual scholars, and moves evaluation authority to disciplinary peers; correspondingly moves inflexibility costs - committed salary lines, slowed staffing reallocation - to governing boards and budgets, and removes a steering lever from elected officials and donors.
% ABSENT_VOICES: Adjunct instructors and graduate employees teach and staff the enterprise but hold no vote in the senates and committees that set personnel policy; students are affected but rarely consulted; state legislators fund public systems yet sit outside shared governance. The consensus that the arrangement protects inquiry is formed substantially among those the protection covers.
% DISAPPEARANCE_RATIONALE: Overnight repeal would expose every continuing appointee to dismissal-at-will: politically sensitive programs would shed faculty within budget cycles, risk-averse topic selection would spread, and the academic labor market would reorganize around short-cycle renewable contracts - the knowledge-producing arrangements built atop the commitment would not survive intact.
% FOUNDING_PROBLEM: Between the 1890s and 1915, donors and trustees dismissed professors for heterodox economics, criticism of benefactors, and unpopular public positions (the Ely and Ross cases are canonical); the 1915 Declaration and the 1940 Statement were written to make scholarly employment survivable for people whose findings displease the powerful.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the faculty beneficiary set: state legislatures repeatedly introduce post-tenure-review and abolition bills (attesting that political steering remains blocked and contested), dismissal disputes regularly reach courts and arbitration, and historians of higher education independently document the founding retaliation cases. The arrangement's own beneficiaries also attest the problem is live, but the legislative and judicial record stands without them.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.34: this reading locates real but bounded costs in the arrangement - the probationary gauntlet, committed salary lines, slowed reallocation - without concentrated rent collection at any seat; the value sits well above a pure coordination floor yet far below extraction-dominated profiles. Suppression 0.42: holding the protection requires active machinery (published dismissal-for-cause procedures, contract law, censure-backed norms), and alternatives exist (fixed-term contracts with just-cause clauses, civil-service-style systems abroad) but are structurally disadvantaged; suppression is authored as a raw structural property and is NOT scaled by power or scope - only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio 0.26: post-tenure review and annual evaluation rituals are substantially performative, but the core function - blocking retaliatory dismissal - demonstrably operates. Accessibility_collapse 0.48: understanding the arrangement does not collapse alternatives; institutions have adopted and abandoned non-tenure models. Resistance 0.58: sustained legislative, board, and donor resistance meets organized faculty defense. All three temporal series share one grid (points 0-60 at decade steps) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute very different types from identical structural data. From elected_officials_and_major_donors, the arrangement is experienced as the removal of a steering instrument they regard as legitimately theirs - high effective extraction at that seat, achieved through a structure whose suppression of their suppression is difficult to undo. From tenured_faculty, the same structure is insulation - low extraction, high coordination benefit. Governing boards straddle: they administer the commitment and bear its rigidity. Pre-tenure faculty and doctoral students sit between, paying probationary and apprenticeship costs against a promised protected position. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: tenured and pre-tenure faculty, students, doctoral apprentices, and long-horizon sponsors all sit toward the subsidized end, with sponsors lowest-cost since they pay nothing into the arrangement directly. The victim declaration drives high d for elected officials and major donors. One override is authored: the derivation would damp the donors' d because their exit option is arbitrage-grade, but their arbitrage reallocates pressure (parallel institutes, budget timing) rather than escaping the cost - the lost-leverage cost follows them across strategies - so d is overridden upward to 0.85. Governing boards derive near-symmetric: they set the arrangement and bear its flexibility costs in roughly balanced measure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - donor and trustee retaliation against heterodox scholarship, canonized in the 1915 Declaration and the 1940 Statement - remains live: legislatures still pass post-tenure-review and abolition bills, and dismissal litigation still reaches courts. Because founding_problem_status is live and disappearance_verdict is world_rearranges, the mismatch consumer finds no zombie signature, and mandatrophy_resolved is not declared. The classification discipline also guards the reverse error: keeping the epsilon referent fixed on the standing arrangement prevents this reading from laundering the sibling readings' extraction findings into a clean-rope verdict, while the omega variables hold the kernel contest open rather than resolving it by metric tuning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_kernel_reading_underdetermination,
    'This constraint is one reading of the tenure_contract kernel; would instantiating a sibling reading change the structural classification?',
    'Compare the compiled sibling stories (tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading) on epsilon, victim sets, and computed types; convergence on a single type across readings would indicate the kernel, not the reading, carries the structure.',
    'If the extraction reading dominates, the arrangement computes as extraction-heavy with contingent labor as the victim seat; if the demographic reading dominates, the peer-review gate is the operative structure; this file''s rope claim holds only under the academic-freedom framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_kernel_reading_underdetermination, conceptual, 'Committer-frame under-determination: which reading of the tenure kernel the structural data supports.').

omega_variable(
    protection_gatekeeping_separability,
    'Is the survival-decoupling function this reading values structurally separable from the entry-gate and permanence features that ground the sibling readings'' complaints?',
    'Natural experiments from systems that protect incumbents without up-or-out gates (civil-service-style research careers, long fixed-term contracts with just-cause renewal): if inquiry-insulation persists while gate demographics and rigidity costs fall, the functions are separable.',
    'If separable, this reading''s coordination claim is stable and reform can target the gate alone; if inseparable, part of the measured coordination benefit is entangled with the structures the siblings identify as harmful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_gatekeeping_separability, empirical, 'Whether protection, gatekeeping, and permanence are separable components of one arrangement.').

omega_variable(
    political_repudiation_trajectory,
    'Will external repudiation pressure - legislative abolition, post-tenure-review mandates, donor conditionality - erode the protection faster than faculty defense adapts?',
    'Track state-level tenure statutes, board policy adoptions, and censure caseloads over successive legislative sessions; a falling share of appointments carrying continuing status marks erosion.',
    'Sustained successful repudiation would shift the arrangement toward a transitional remainder and eventually invalidate this reading''s reference frame; stable defense would confirm the protection as durable coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_repudiation_trajectory, empirical, 'Trajectory of the repudiation pressure recorded in drift_state.').

omega_variable(
    public_private_enforcement_asymmetry,
    'Does the protection bind equally across public institutions (statutory and constitutional due-process backing) and private institutions (handbook contract only)?',
    'Compare dismissal-for-cause outcomes and litigation results across sectors; divergent survival rates for targeted scholars indicate asymmetric enforcement.',
    'If private-sector protection is markedly weaker, the arrangement''s coordination benefit is sector-dependent and the measured suppression understates fragility for a large share of the academic workforce.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_private_enforcement_asymmetry, empirical, 'Sectoral asymmetry in how firmly the continuing-appointment commitment binds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_af_reading_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t0, observed).
narrative_ontology:measurement(tenure_af_reading_tr_t10, tenure_contract__academic_freedom_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t10, observed).
narrative_ontology:measurement(tenure_af_reading_tr_t20, tenure_contract__academic_freedom_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t20, observed).
narrative_ontology:measurement(tenure_af_reading_tr_t30, tenure_contract__academic_freedom_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t30, observed).
narrative_ontology:measurement(tenure_af_reading_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t40, observed).
narrative_ontology:measurement(tenure_af_reading_tr_t50, tenure_contract__academic_freedom_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t50, observed).
narrative_ontology:measurement(tenure_af_reading_tr_t60, tenure_contract__academic_freedom_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(tenure_af_reading_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(tenure_af_reading_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(tenure_af_reading_be_t0, observed).
narrative_ontology:measurement(tenure_af_reading_be_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement_basis(tenure_af_reading_be_t10, observed).
narrative_ontology:measurement(tenure_af_reading_be_t20, tenure_contract__academic_freedom_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement_basis(tenure_af_reading_be_t20, observed).
narrative_ontology:measurement(tenure_af_reading_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(tenure_af_reading_be_t30, observed).
narrative_ontology:measurement(tenure_af_reading_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement_basis(tenure_af_reading_be_t40, observed).
narrative_ontology:measurement(tenure_af_reading_be_t50, tenure_contract__academic_freedom_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement_basis(tenure_af_reading_be_t50, observed).
narrative_ontology:measurement(tenure_af_reading_be_t60, tenure_contract__academic_freedom_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement_basis(tenure_af_reading_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenure_af_reading_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(tenure_af_reading_su_t0, observed).
narrative_ontology:measurement(tenure_af_reading_su_t10, tenure_contract__academic_freedom_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement_basis(tenure_af_reading_su_t10, observed).
narrative_ontology:measurement(tenure_af_reading_su_t20, tenure_contract__academic_freedom_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(tenure_af_reading_su_t20, observed).
narrative_ontology:measurement(tenure_af_reading_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement_basis(tenure_af_reading_su_t30, observed).
narrative_ontology:measurement(tenure_af_reading_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.34).
narrative_ontology:measurement_basis(tenure_af_reading_su_t40, observed).
narrative_ontology:measurement(tenure_af_reading_su_t50, tenure_contract__academic_freedom_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(tenure_af_reading_su_t50, observed).
narrative_ontology:measurement(tenure_af_reading_su_t60, tenure_contract__academic_freedom_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(tenure_af_reading_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'tenure' conflates three structurally distinct claims about one arrangement; per the epsilon-invariance principle they are authored as a constraint family sharing the tenure_contract kernel. This file (academic_freedom_reading) authors epsilon for the standing arrangement assessed by the protection reading; tenure_contract__institutional_extraction_reading authors epsilon for the same arrangement assessed as rent-and-rigidity extraction centered on contingent labor; tenure_contract__demographic_reproduction_reading authors it as demographic gatekeeping at the entry gate. Each member links the others via network.affects_constraints. Ordering is by empirical contestation: this reading is historically prior and is cited BY the siblings, who argue either that the protection's side effects are the real structure or that the protection is cover.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
