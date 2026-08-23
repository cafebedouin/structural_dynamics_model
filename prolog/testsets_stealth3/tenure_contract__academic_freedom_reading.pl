% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Tenure as Protected-Inquiry Insurance (Academic Freedom Reading)
 *   domain: education/labor/institutional
 *
 * SUMMARY:
 *   Continuing appointment (tenure) insures scholars against dismissal for
 *   unwelcome findings or political nonconformity: after a multi-year
 *   peer-screened probation, removal for cause becomes procedurally expensive
 *   enough that institutional and external actors lose direct personnel
 *   leverage. This story instantiates the ACADEMIC FREEDOM READING of the
 *   tenure kernel, in which that decoupling is the constraint's operative
 *   function and its justification. The same arrangement carries real costs
 *   flowing through the identical structure — a decade-scale exposed-labor
 *   toll on probationary scholars, stripped control capacity for political
 *   and donor actors — which is why the structure is authored as a hybrid
 *   rather than a pure coordination device. Contingent-labor cost-shifting
 *   and demographic gatekeeping are instantiated by the SIBLING stories in
 *   this kernel family and are deliberately not folded into this file's
 *   epsilon or victim set. KEY AGENTS (by structural relationship): -
 *   tenured_faculty: Primary beneficiary (organized / identity_locked) —
 *   collects durable security and expressive discretion -
 *   probationary_junior_faculty: Primary payer (moderate / constrained) —
 *   bears the exposed-labor toll that finances the protection -
 *   research_universities: Secondary beneficiary (institutional / trapped) —
 *   accumulates prestige and recruiting power from protected inquiry -
 *   state_legislatures_seeking_control: Target payer (institutional /
 *   constrained) — loses direct dismissal leverage -
 *   aligned_donors_seeking_viewpoint_influence: Target payer with arbitrage
 *   (powerful / arbitrage) — loses personnel leverage but redirects giving
 *   elsewhere - students_at_research_universities: Mild beneficiary (moderate
 *   / mobile) — consumes protected instruction at low personal cost -
 *   contingent_adjunct_faculty: Excluded voice (powerless / trapped) —
 *   occupies the flexible margin that subsidizes protected lines; absent from
 *   deliberation - higher_education_policy_analysts: Analytical observer
 *   (analytical / analytical)
 *
 * KEY AGENTS:
 *   - tenured_faculty — primary beneficiary; organized power, identity-locked exit; collects security and discretion
 *   - probationary_junior_faculty — primary payer; moderate power, constrained exit; bears probationary exposed-labor toll
 *   - research_universities — secondary beneficiary; institutional power, trapped; converts protected inquiry into standing
 *   - state_legislatures_seeking_control — target payer; institutional power, constrained exit; blocked from direct personnel leverage
 *   - aligned_donors_seeking_viewpoint_influence — target payer with arbitrage exit; leverage denied but resources reroutable
 *   - students_at_research_universities — mild beneficiary; mobile exit; net-positive via protected instruction
 *   - contingent_adjunct_faculty — excluded; powerless, trapped; subsidizes the protected margin without representation
 *   - higher_education_policy_analysts — analytical observer; produces the evidence both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.42).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.5).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Protected-Inquiry Insurance (Academic Freedom Reading)").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "education/labor/institutional").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '97cbf9ea-936e-4d92-bcd8-41693fb97e28').
narrative_ontology:cs_kernel_codification('97cbf9ea-936e-4d92-bcd8-41693fb97e28', formalized).
narrative_ontology:cs_authority_grounding('97cbf9ea-936e-4d92-bcd8-41693fb97e28', expertise).
narrative_ontology:cs_interpretation_layer_present('97cbf9ea-936e-4d92-bcd8-41693fb97e28').
narrative_ontology:cs_reading_relation('97cbf9ea-936e-4d92-bcd8-41693fb97e28', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('97cbf9ea-936e-4d92-bcd8-41693fb97e28', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('97cbf9ea-936e-4d92-bcd8-41693fb97e28', foundational, survival_decoupling_necessary_for_truth_seeking).
narrative_ontology:cs_axiom_status(survival_decoupling_necessary_for_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('97cbf9ea-936e-4d92-bcd8-41693fb97e28', survival_decoupling_necessary_for_truth_seeking, instrumental).
narrative_ontology:cs_axiom('97cbf9ea-936e-4d92-bcd8-41693fb97e28', foundational, external_personnel_control_corrupts_inquiry).
narrative_ontology:cs_axiom_status(external_personnel_control_corrupts_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('97cbf9ea-936e-4d92-bcd8-41693fb97e28', external_personnel_control_corrupts_inquiry, empirically_contingent).
narrative_ontology:cs_reference_frame('97cbf9ea-936e-4d92-bcd8-41693fb97e28', postwar_protected_inquiry_compact).
narrative_ontology:cs_drift_state('97cbf9ea-936e-4d92-bcd8-41693fb97e28', contemporary_accountability_movement, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('97cbf9ea-936e-4d92-bcd8-41693fb97e28', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, research_universities).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students_at_research_universities).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, probationary_junior_faculty).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, state_legislatures_seeking_control).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, aligned_donors_seeking_viewpoint_influence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold continuing appointments granted after a multi-year evaluation. They may pursue unfashionable problems, publish unwelcome findings, and publicly contest institutional or political positions without facing dismissal, since removal requires prolonged for-cause proceedings. They carry governance service and mentorship loads in return, and their pensions, salaries, and professional self-concept are anchored inside the academy, so exiting would mean abandoning the vocation itself rather than changing employers.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, biographical, identity_locked, national).

% Spend six to nine years under continuous evaluation, carrying heavy teaching and service loads, courting senior sponsors, and deferring controversial projects until protection arrives. Leaving mid-track forfeits the accumulated years and typically restarts the clock elsewhere; staying requires accepting below-market total compensation during the evaluation window. Their exposed labor during this period is what finances the security the system later grants.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, probationary_junior_faculty, payer,
    moderate, biographical, constrained, national).

% Accumulate prestige and grant revenue from scholars willing to attempt long-horizon or contrarian work, and recruit talent with the promise of eventual security. They absorb the governance frictions the protection creates and cannot abandon the tenure expectation without losing research standing in international rankings and accreditation terms; the commitment is structural to their identity.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, research_universities, beneficiary,
    institutional, generational, trapped, global).

% Seek to steer public-university curricula, research priorities, and personnel through budgets, statutes, and board appointments. Continuing appointment removes their direct dismissal lever over individual scholars, forcing them into slower indirect routes such as funding conditionality, program consolidation, or post-tenure review mandates. They cannot exit the obligation to govern the institutions inside their jurisdiction.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, state_legislatures_seeking_control, payer,
    institutional, immediate, constrained, regional).

% Condition major gifts on ideological comfort or withdraw support after publicized controversies. Tenure denies them personnel leverage regardless of gift size, so their practical response is to redirect philanthropy toward compliant venues, parallel institutes, or non-academic outlets. Because they can route resources elsewhere at low cost, the binding force of the arrangement on them personally is weaker than on actors confined to one jurisdiction.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, aligned_donors_seeking_viewpoint_influence, payer,
    powerful, biographical, arbitrage, national).

% Take courses and receive research training from instructors who are not afraid to teach contested material or report inconvenient results. They help finance the security through tuition and state-supported operations. They experience few direct costs and can transfer institutions or change fields with modest friction, so their net position is mildly favorable rather than deeply invested.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students_at_research_universities, beneficiary,
    moderate, immediate, mobile, national).

% Teach a large and growing share of courses off the tenure track, on short contracts at low per-course pay, without protection or any realistic path onto the ladder. The flexible margin they occupy is what makes the protected lines affordable. They appear nowhere in tenure deliberations: the committees, senates, and legislative hearings that set the terms of academic employment do not include them.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_adjunct_faculty, excluded,
    powerless, immediate, trapped, national).

% Study tenure's effects on research productivity, innovation rates, workforce rigidity, demographic composition, and expressive freedom. Produce the comparative and longitudinal evidence that both defenders and attackers of the arrangement cite, and hold no material stake beyond analytic standing.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, higher_education_policy_analysts, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the risk-allocation problem of who bears the uncertainty of slow, expensive, potentially unpopular inquiry: it converts case-by-case vulnerability to institutional and political displeasure into a one-time, rule-governed screening followed by durable protection, allowing scholars to commit decades to problems whose value is illegible to present-day patrons.
% TRANSFER_FUNCTION: Moves durable job security and expressive discretion from governing authorities to individual scholars upon completion of a probationary screening; moves six-plus years of exposed, below-market, deference-heavy labor from junior scholars toward institutions during the screening; and strips direct personnel-control capacity from political and donor actors for the duration of each protected appointment.
% ABSENT_VOICES: Contingent adjunct faculty, whose flexible labor subsidizes the protected lines, are absent from every forum where tenure terms are set. Tuition-paying students are consulted only through satisfaction surveys. Taxpaying citizens outside university towns bear opportunity costs of subsidized employment security and are represented only indirectly through legislatures that already appear as antagonists. They are outside the faculty senate, outside the tenure committee, and outside the collective-bargaining unit.
% DISAPPEARANCE_RATIONALE: If continuing appointment vanished overnight, hiring would shift to short-term contracts priced against dismissal risk, research agendas would migrate toward fundable, fashionable, and politically safe topics, controversial findings would chill as scholars priced career survival into topic selection, faculty would organize unions or exit to industry, and governing boards and legislatures would regain direct personnel leverage they currently lack. The production and dissemination of uncomfortable knowledge would visibly reorganize.
% FOUNDING_PROBLEM: Arbitrary dismissal of scholars whose teaching or findings offended trustees, donors, legislators, or administrators, culminating in the WWI-era loyalty purges and the early-20th-century firings that prompted the organized professoriate to demand dismissal only for cause through peer procedure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: legislative hearing records and enacted statutes in multiple states document sitting officials stating intent to direct university personnel and curricula, confirming the dismissal-threat environment is active rather than historical; historians unaffiliated with faculty interests have documented the pre-tenure purge episodes; and litigation disclosures of donor conditionality confirm external pressure channels remain in use. Faculty self-attestation alone would not establish this.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are independent authored facts. I claim tangled_rope because I believe the structure genuinely solves a real risk-allocation problem (coordination half) while simultaneously extracting through the same machinery (asymmetric-extraction half) under active enforcement — not because any predicted engine output suggests it. Metrics describe operation: epsilon 0.42 is moderate because the dominant flow is protective subsidy to the tenured seat, offset by a real probationary toll and stripped external control capacity. Suppression 0.50 reflects enforcement dependence (for-cause procedures, peer-committee gatekeeping, board ratification, and in public systems statutory codification) plus constrained alternatives for juniors mid-stream; it is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, in the engine's computation. Accessibility_collapse 0.52: alternative careers and contract-based research arrangements exist but collapse substantially once a scholar commits to the academic research track. Resistance 0.62: sustained legislative campaigns, board initiatives, donor pressure, and internal junior-faculty grievance meet the arrangement continuously. Theater 0.30: dossier ritualism and metric-driven review ceremony are real but the protective function remains substantively operative. All three tracked series run on ONE shared time grid ({0,6,12,18,24,30}) so no metric row borrows another's endpoint; suppression_requirement is tracked because enforcement capacity is the dynamic this story traces — the protective machinery has hardened under escalating political attack (post-tenure review mandates, statutory codification fights), a rising trajectory, not static enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply and the engine computes that divergence from the structural data. From the tenured seat the arrangement reads as near-pure protection: costs were paid once, benefits compound for life. From the probationary seat the same structure operates as an enforced gauntlet — maximum exposure precisely when leverage is minimal. From the legislature's seat the constraint is experienced as the suppression OF its suppression capacity: a barrier placed between officials and personnel control. From the donor's seat it is a denial of purchased influence, softened by arbitrage. From the student's seat it is a mild free ride on protected instruction. One arrangement, four different constraint-types depending on where one stands — which is why no single seat's verdict is treated as the constraint's type.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation. tenured_faculty and research_universities are declared beneficiaries with locked/trapped exit — low d, damped or inverted effective extraction. probationary_junior_faculty are declared victims with constrained exit (mid-track departure forfeits sunk years, restarting the clock) — high d, amplified extraction; this is the seat where the reading's own honesty about the probationary toll surfaces. state_legislatures_seeking_control are victims with CONstrained exit (jurisdictional obligation) — kept near the full-target end, matching the expected delta of high extraction at the external political seat. aligned_donors_seeking_viewpoint_influence are also victims but carry arbitrage exit — the derivation correctly pulls their d back toward the middle, capturing that money reroutes even where leverage fails. students_at_research_universities are beneficiaries with mobile exit — lowest d among humans, matching 'neutral beneficiaries.' No directionality_overrides are authored: the override surface keys on power_atom alone, and an override at the institutional atom would conflate governing boards (agenda-setter, mid-range position) with legislatures (target, near-full-target), doing more distortion than the structural declarations leave. The agenda_setter seat rides the canonical fallback for institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary dismissal for displeasing findings — is LIVE, attested by sources outside the beneficiary set, so no mandatrophy is declared and the R5 mismatch consumer finds status=live x verdict=world_rearranges, the coherent cell: the arrangement persists because its problem persists, not because anyone forgot to bury it. The classification guards against mislabeling in both directions. Read only from the defender's seat, tenure presents as pure rope and the probationary toll and control-stripping disappear; read only from the attacker's seat, it presents as pure snare and the demonstrated protection of dissident scholarship disappears. The tangled_rope structure with declared beneficiaries, declared victims, and active-enforcement requirement forces both halves into the record. Theater is authored honestly at 0.30 — present in review ritual, insufficient to make the arrangement a piton, because the administrator could not change it cheaply (fixing_cost prohibitive) and the protection still bites where tested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This classification describes exactly one reading of the tenure_contract kernel — the academic_freedom_reading. What structurally changes if a sibling reading is adopted instead?',
    'Cross-file comparison of per-seat classifications across the three linked stories in this kernel family: divergence in victim sets, epsilon, and computed types localizes the disagreement between readings.',
    'Under tenure_contract__institutional_extraction_reading, contingent labor joins the victim set, epsilon rises sharply, and the tenure line itself becomes the extraction instrument. Under tenure_contract__demographic_reproduction_reading, the victims become scholars screened out by fit and collegiality criteria, and the enforcement object shifts from political actors to evaluation discretion itself. Every number in this file is valid only for the academic-freedom reading''s referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer positioning: one-of-three kernel readings; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    probationary_toll_calibration,
    'Is the probationary period''s extractive intensity an inherent price of reliable screening, or an inflated toll sustained by doctoral-labor oversupply that would shrink under tighter market clearing?',
    'Cross-system comparison of protection delivered per probationary year: jurisdictions and sectors with shorter or differently structured evaluation windows (abolished-tenure systems, junior professor tracks, teaching-first ladders) reveal whether screening reliability tracks probation length or applicant-pool slack.',
    'If the toll is inherent, part of the measured extraction is coordination cost and the faculty-side seats classify rope-leaning; if it is inflated, excess extraction concentrates on the payer seat and the structure leans snare-ward there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probationary_toll_calibration, empirical, 'Whether the junior-faculty toll is screening cost or rent extracted from labor oversupply.').

omega_variable(
    protection_under_concentrated_attack,
    'Does tenure''s protection actually hold where political backlash concentrates — board capture, legislative supermajorities, statutory rewriting — or does protection fail precisely where the founding problem is most acute?',
    'Outcome tracking in jurisdictions that moved against the arrangement (protection-stripping acts, mandated post-tenure review, reorganization of governing boards): did retention of controversial scholars, dismissal-for-cause difficulty, and expression incidence measurably change afterward?',
    'If protection collapses under concentrated attack, this reading''s core promise degrades toward performed protection in exactly those seats and the classification there drifts toward inertial maintenance; if it holds, the resistance and suppression figures are validated as functional rather than theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_under_concentrated_attack, empirical, 'Whether the protection survives the environments that justify it.').

omega_variable(
    bundled_gatekeeping_insurance,
    'Is scholar protection structurally separable from the peer-evaluation discretion that delivers it, or does durable protection necessarily empower evaluators whose judgments extend beyond merit into taste and collegiality?',
    'Institutional-design comparison: employment-insurance variants that protect speech and employment without discretionary gatekeeping (cause-only dismissal contracts with fixed renewal terms, ombuds-reviewed evaluation) versus committee-discretionary models.',
    'If separable, a pure-protection variant exists and the tangled character of this constraint is a bundling choice rather than a necessity — decomposing it would isolate the extractive residue. If inseparable, part of the measured extraction is the unavoidable price of the protection itself and belongs inside the coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundled_gatekeeping_insurance, conceptual, 'Whether insurance and gatekeeping can be unbundled, determining how much extraction is design-contingent.').

omega_variable(
    self_censorship_internalization,
    'Among tenured scholars, is residual topic-caution the structural residue of having survived the probationary gauntlet, or internalized caution that persists after the structural vulnerability has been removed?',
    'Within-person longitudinal comparison of topic daring and finding-framing before versus after protection arrives, controlling for field, cohort, and career stage.',
    'If the caution is largely internalized, the scalar suppression understates experienced chilling and the constraint carries its costs beyond its structural reach; if largely structural, earlier delivery of protection would close the gap and the measured suppression is accurately located in the probationary design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_censorship_internalization, empirical, 'Structural versus internalized component of residual scholarly self-censorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tc_af_reading_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tc_af_reading_tr_t6, tenure_contract__academic_freedom_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(tc_af_reading_tr_t12, tenure_contract__academic_freedom_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(tc_af_reading_tr_t18, tenure_contract__academic_freedom_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement(tc_af_reading_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(tc_af_reading_tr_t30, tenure_contract__academic_freedom_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(tc_af_reading_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(tc_af_reading_be_t6, tenure_contract__academic_freedom_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement(tc_af_reading_be_t12, tenure_contract__academic_freedom_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(tc_af_reading_be_t18, tenure_contract__academic_freedom_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(tc_af_reading_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(tc_af_reading_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tc_af_reading_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(tc_af_reading_su_t6, tenure_contract__academic_freedom_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(tc_af_reading_su_t12, tenure_contract__academic_freedom_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(tc_af_reading_su_t18, tenure_contract__academic_freedom_reading, suppression_requirement, 18, 0.46).
narrative_ontology:measurement(tc_af_reading_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(tc_af_reading_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'tenure' conflates three structurally distinct claims about one arrangement. This story (academic_freedom_reading) authors epsilon for the standing arrangement AS THE PROTECTION READING SEES IT: moderate epsilon, beneficiaries led by the tenured seat, victims led by probationary scholars and blocked external controllers. tenure_contract__institutional_extraction_reading authors epsilon for the SAME standing arrangement as a rigidity-and-rent structure, with contingent labor in the victim set and sharply higher epsilon. tenure_contract__demographic_reproduction_reading authors the evaluation machinery as demographic gatekeeping, shifting victims to screened-out scholars. Per the epsilon-invariance principle these are three constraints, not one constraint under three observables: each file carries its own epsilon, beneficiary/victim structure, and classification, linked here via affects_constraints. The upstream/downstream texture runs both directions: the protection reading legitimizes the peer discretion the demographic reading critiques, while extraction critiques supply the fiscal arguments protection defenders must answer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
