% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Reproduction Gate
 *   domain: economic/educational/institutional
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the demographic reproduction reading
 *   — of the contested tenure_contract kernel. Its siblings (an
 *   academic-freedom reading and an institutional-extraction reading) are
 *   separate constraint files with their own epsilon values, beneficiary
 *   sets, and classifications; nothing about them is averaged into this one.
 *   The epsilon referent throughout is the standing arrangement under
 *   contest: promotion-and-tenure peer review as it actually operates,
 *   assessed by this reading's own lights — not the inquiry-protecting
 *   arrangement its defenders describe, and not any reform alternative. On
 *   that referent, this reading finds that 'fit' and 'collegiality' carry
 *   decisive weight unanchored to publication, funding, or teaching records,
 *   and that these judgments track demographic similarity to incumbent
 *   evaluators, so the apparatus reproduces the profession's existing
 *   composition while performing merit assessment. The claim/metric split is
 *   deliberate: the constraint is CLAIMED as tangled_rope (a genuine
 *   quality-allocation core with a demographic-filter overlay) while the
 *   metrics describe heavily extractive, actively enforced operation — the
 *   engine computes each seat's verdict from the structural data, and the
 *   claim is not reconciled to them. Interval mapping: time 0 corresponds to
 *   the AAUP 1940 Statement's consolidation of the up-or-out structure; time
 *   80 approximates the present.
 *
 * KEY AGENTS:
 *   - - tenured_majority_professoriate: agenda-setting seat (institutional / identity_locked) — authors criteria, staffs committees, casts decisive votes, collects the positional rents
 *   - - dominant_group_junior_candidates: primary beneficiary (moderate / constrained) — records read against a template resembling their own trajectory
 *   - - underrepresented_faculty_candidates: primary target (powerless / trapped) — bears the fit/collegiality penalty with the fewest outside options
 *   - - norm_departing_scholars: secondary target (moderate / constrained) — penalized for portfolio deviation independent of productivity
 *   - - university_administration: administering seat (institutional / mobile) — convenes and certifies, inherits criteria it did not write
 *   - - deterred_prospective_applicants: excluded voice (powerless / mobile) — objects by never entering
 *   - - faculty_diversity_researchers: analytical observer — sees the aggregate pattern no committee participant can
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.74).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.68).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Reproduction Gate").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "economic/educational/institutional").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '4e13c145-43a3-4c44-beaf-c3f1c2499e51').
narrative_ontology:cs_kernel_codification('4e13c145-43a3-4c44-beaf-c3f1c2499e51', formalized).
narrative_ontology:cs_authority_grounding('4e13c145-43a3-4c44-beaf-c3f1c2499e51', practice).
narrative_ontology:cs_interpretation_layer_present('4e13c145-43a3-4c44-beaf-c3f1c2499e51').
narrative_ontology:cs_reading_relation('4e13c145-43a3-4c44-beaf-c3f1c2499e51', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('4e13c145-43a3-4c44-beaf-c3f1c2499e51', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('4e13c145-43a3-4c44-beaf-c3f1c2499e51', foundational, fit_collegiality_are_demographic_proxies).
narrative_ontology:cs_axiom_status(fit_collegiality_are_demographic_proxies, holdable).
narrative_ontology:cs_axiom_grounding('4e13c145-43a3-4c44-beaf-c3f1c2499e51', fit_collegiality_are_demographic_proxies, empirically_contingent).
narrative_ontology:cs_axiom('4e13c145-43a3-4c44-beaf-c3f1c2499e51', foundational, review_output_is_composition_reproduction).
narrative_ontology:cs_axiom_status(review_output_is_composition_reproduction, holdable).
narrative_ontology:cs_axiom_grounding('4e13c145-43a3-4c44-beaf-c3f1c2499e51', review_output_is_composition_reproduction, empirically_contingent).
narrative_ontology:cs_reference_frame('4e13c145-43a3-4c44-beaf-c3f1c2499e51', ingroup_composition_baseline).
narrative_ontology:cs_drift_state('4e13c145-43a3-4c44-beaf-c3f1c2499e51', contemporary_accountability_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4e13c145-43a3-4c44-beaf-c3f1c2499e51', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, tenured_majority_professoriate).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_group_junior_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, norm_departing_scholars).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, meritocratic_status_quo_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments and staff every promotion-and-tenure committee. They draft and revise the evaluation criteria, solicit and weigh the external letters, deliberate behind closed doors, and cast the decisive votes. Most come from the demographic groups that have predominated in the profession since the mid-century expansion, and the criteria they administer ask candidates to resemble, in manner and orientation, the people writing the assessments. Departure at this career stage would forfeit pensions, laboratories, and a professional identity assembled over decades; almost none leave.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenured_majority_professoriate, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, tenured_majority_professoriate, beneficiary).

% Pass through the same review with records read against a template that resembles their own trajectory. Ambiguous signals — a thin publication year, an unpolished job talk, light service — tend to be read charitably, and mentors transmit the tacit expectations about demeanor, ambition-framing, and departmental manners that the committees reward. Completing probation where they started is the assumed path; changing fields or institutions mid-stream would reset the clock.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, dominant_group_junior_candidates, beneficiary,
    moderate, biographical, constrained, national).

% Enter review as visible minorities in their departments. Collegiality and fit judgments turn on cultural cues, mentoring style, and which established colleagues vouch for them; service burdens arrive earlier and heavier, and conduct that reads as leadership when displayed by majority peers reads as abrasion when they display it. Denial rates exceed those of otherwise comparable majority candidates. Outside options are thin: narrow specialist training, geographically concentrated openings, and partner-employment constraints make departure costly, and restarting probation at another institution repeats the gauntlet under new judges.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, trapped, national).

% Work on questions or with methods that depart from the department's inherited portfolio, publish across disciplinary borders, or maintain visible public engagement. Committees weigh such programs as risky bets or poor departmental citizenship regardless of citation counts or grant totals. They can seek institutions friendlier to their program, but every move restarts probation and shrinks the pool of departments that would host the work.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, norm_departing_scholars, payer,
    moderate, biographical, constrained, national).

% Convene the committees, certify the outcomes, and answer to trustees, accreditors, and courts for what the process produces. They inherit faculty-authored criteria and rarely rewrite them; their discretion runs to process compliance and liability management rather than to the substance of any individual judgment. Administrators circulate between institutions readily, carrying reputations rather than laboratories.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, agenda_setter,
    institutional, biographical, mobile, national).

% Look at the demographic profile of senior faculty and at the published accounts of those denied, and choose graduate school in another country, industry, government, or another sector altogether. They never enter review, so their objections register only as thinner applicant pools and quiet attrition. Their labor is welcomed elsewhere.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, deterred_prospective_applicants, excluded,
    powerless, biographical, mobile, national).

% Compile hiring, promotion, and attrition data across institutions; run interview and audit studies of evaluation language; publish the demographic accounting that the review process itself never produces. They stand outside the votes and can see the aggregate pattern that no individual participant observes from inside a single committee.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, faculty_diversity_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, tenured_majority_professoriate).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools the dispersed judgments of senior researchers into a single decision about which early-career scholars merit conversion to permanent appointment, allocating scarce permanent lines and laboratory investment and supplying departments with a shared, defensible standard for a choice no lone member could carry.
% TRANSFER_FUNCTION: Moves permanent employment, salary security, laboratory space, doctoral-student access, and disciplinary authority from an open candidate pool to whichever candidates incumbent reviewers select; on this reading the selection weights favor candidates demographically similar to the reviewers, so the positional goods land disproportionately on majority-group scholars.
% ABSENT_VOICES: Denied candidates learn only the outcome: deliberations are confidential and the operative criteria stay tacit, so the people holding the sharpest information about how fit judgments actually work are structurally outside the room afterward. Scholars who left the profession after denial carry the evidence but not the standing. Applicants deterred at the hiring stage never enter at all. Doctoral students, staff, and the publics that fund the enterprise hold no seat.
% DISAPPEARANCE_RATIONALE: Appointment ladders, promotion timelines, salary scales, departmental budgeting, and the entire shape of the academic labor market presuppose the probationary-review structure. Overnight removal would force institutions onto fixed-term contracts or metric-based promotion within a hiring cycle, redistribute permanent lines across fields, and strand every mid-probation career.
% FOUNDING_PROBLEM: Early twentieth-century boards and donors dismissed professors over research conclusions and political speech; tenure was constructed (AAUP declarations of 1915 and 1940) to make inquiry economically survivable by requiring demonstrated institutional cause for dismissal after a defined probationary period.
% FOUNDING_PROBLEM_CORROBORATION: Labor historians and the AAUP archival record attest the anti-dismissal origin from outside today's beneficiary roster; NSF workforce studies and professional-society climate reports document a current demographic operation that diverges from that founding purpose; university counsel and accreditors treat the protective rationale as live wherever political-interference cases arise. External corroboration is abundant — the dispute is over which problem the machinery now solves, not over whether the founding problem existed.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74 at interval end) is high because the decisive criteria are unmoored from the productivity record the process advertises: candidates cannot satisfy fit and collegiality by producing more or better research, so the marginal return to scholarship flattens while the return to resemblance steepens. Suppression (0.68) is authored as a raw structural property and is NOT scaled by power or scope anywhere in this file — the engine owns that arithmetic; the value reflects enforced probationary dependence, confidential deliberation, thin specialist labor markets, and restart-the-clock mobility costs, with a further internalized component carried by targets themselves (see omega internalized_fit_norms). Theater ratio (0.42) rises across the interval because the merit vocabulary — rubrics, teaching portfolios, external-letter protocols — expands considerably faster than the compositional outcomes change; a growing fraction of evaluation activity documents conformity rather than measures scholarship. Accessibility collapse is moderate (0.45): industry, teaching-intensive institutions, government, and overseas systems remain reachable at real cost, so understanding the gate does not close every exit. Resistance (0.55) is substantial and persistent — denial litigation, cluster hires, union organizing, funder and accreditor pressure, professional-society audits — and the constraint visibly bends under it without dissolving, which is why the profile is not that of an unchallenged natural order. Temporal notes: the mild extractiveness dip around t=20 reflects the civil-rights-era opening, when affirmative-pressure briefly compressed the gate; the subsequent monotonic climb tracks the shift of screening from criteria that were becoming legally indefensible to facially neutral proxies administered through expanding machinery — hence the rising suppression_requirement series, which models enforcement build-out rather than any change in the scalar suppression picture. The t=20 wobble is externally driven (statutory pressure), not intermittent reinforcement; the long-run trend is the signal. All three series share one nine-point grid so the engine samples a complete row at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the procedure is the guardian of standards its holders built and embody: they experience review as the thing standing between the department and decline, and the demographic outcome as the residue of merit. From the trapped payer seat the identical procedure operates as a filter calibrated against them — same forms, same letters, same vote, opposite lived constraint. The sharpest divergence sits between two candidate classes of formally identical rank and credentials: what separates them is not power or standing but the implicit reference person embedded in the criteria, which is why same-level actors at equal nominal position compute different verdicts. Administration occupies a third position — it touches every case yet captures little, experiencing the arrangement chiefly as a liability-management workflow. The engine computes these per-seat classifications from the authored structural data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation, and no overrides were needed. The professoriate declares both agenda and benefit with identity-locked exit, pinning it near the full-beneficiary pole. Majority junior candidates are beneficiaries with constrained (not arbitrage-grade) exit — subsidized, but still paying probationary-years opportunity cost. Underrepresented candidates are victims with trapped exit, placing them nearest the full-target pole and receiving the largest amplification; the coalition caveat matters here: individually powerless against a committee, they acquire partial leverage only through caucuses, unions, and litigation, which the resistance metric partially registers. Norm-departing scholars are victims whose somewhat better exit options temper but do not remove the targeting. Administration straddles: it enforces without capturing, sitting near symmetric. Deterred applicants sit wholly outside the transfer loop. The vindicated proposition — that existing composition tracks merit — collects no rents and is listed separately from beneficiaries for exactly that reason.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — insulating inquiry from donor and trustee dismissal — is contested rather than dead: political-interference cases keep the protective rationale nominally alive, while this reading's evidence indicates the operative output has drifted toward compositional maintenance. Reading the arrangement through the six-type lattice blocks two symmetrical errors. Calling it a natural summit of merit selection fails because the gate requires continuous active enforcement, produces identifiable losers, and meets sustained resistance — none of which describes a law of nature. Calling it pure predation fails because the allocation core is real: departments genuinely must decide which research programs merit permanent investment, and productivity assessment demonstrably occurs inside the process. The tangled_rope claim holds both truths in one structure. Against the R5 mismatch consumer: founding_problem_status=contested combined with disappearance_verdict=world_rearranges raises no dead-mandate-plus-dependence flag, and that absence is itself the finding — the mandate has thinned without dying, which is precisely the condition under which the coordination cover does its quietest work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the demographic_reproduction_reading of the tenure_contract kernel — how would compiling the academic_freedom_reading and institutional_extraction_reading restructure the beneficiary set, the victim set, and epsilon?',
    'Cross-reading comparison once the sibling files are compiled: align the three stories on the same underlying cases and observe which structural declarations survive translation and which invert.',
    'Under the freedom reading the beneficiaries become all scholars and future inquirers, the victims shrink to dismissed dissenters, and epsilon falls sharply toward coordination cost; under the extraction reading the victim set is replaced wholesale by contingent labor and early-career cohorts. Per-seat classifications flip accordingly — this story''s verdict is conditional on its reading, not on the tenure kernel simpliciter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel, with sibling readings that would reassign every structural role.').

omega_variable(
    proxy_variance_attribution,
    'Does the variance in fit and collegiality assessments that survives controls for publications, grants, and teaching actually covary with demographic similarity between candidate and evaluators?',
    'Multi-institution linked audit and panel designs scoring evaluation language against evaluator-candidate demographic distance with productivity covariates; natural experiments from mandated structured rubrics.',
    'A null result collapses this reading toward the academic_freedom_reading — epsilon drops and the victim set dissolves into ordinary evaluation noise. Confirmation hardens the extraction component and pushes the computed type toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_variance_attribution, empirical, 'Whether the demographic-proxy mechanism this reading posits is statistically real after productivity controls.').

omega_variable(
    internalized_fit_norms,
    'How much of the measured suppression is structural (probationary dependence, closed deliberation, thin markets) versus internalized (targets pre-conforming their research agendas, demeanor, and self-presentation before review ever convenes)?',
    'Post-denial career trajectory studies and pre-review survey instruments comparing agenda self-censorship across demographic groups; longitudinal designs tracking whether conforming behavior persists after targets exit the review population.',
    'If the internalized share is large, effective suppression exceeds the structural measure and persists after institutional reform — procedural fixes would leave the gate standing inside the candidates. If small, structural remedies suffice and the scalar suppression is close to the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_fit_norms, empirical, 'Structural versus internalized split of the constraint''s suppressive force.').

omega_variable(
    productivity_referent_regression,
    'The reading''s defining contrast — fit criteria ''unmoored from research productivity'' — presupposes a productivity measure independent of peer judgment, yet publication venues, citation counts, grant awards, and even teaching evaluations are themselves products of peer-mediated systems.',
    'Instrumental and bibliometric constructions of productivity that minimize reviewer overlap with the evaluating committee; sensitivity analysis across referent choices.',
    'If no criterion-independent referent exists, part of what this reading books as demographic extraction is indistinguishable from irreducible coordination cost, and epsilon attribution between the allocation core and the gatekeeping overlay shifts upward — softening the extraction claim at its foundation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(productivity_referent_regression, conceptual, 'Whether the fit-versus-productivity boundary is measurable or partly conventional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(tenu_tr_t50, tenure_contract__demographic_reproduction_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement(tenu_tr_t60, tenure_contract__demographic_reproduction_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(tenu_tr_t70, tenure_contract__demographic_reproduction_reading, theater_ratio, 70, 0.39).
narrative_ontology:measurement(tenu_tr_t80, tenure_contract__demographic_reproduction_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(tenu_be_t50, tenure_contract__demographic_reproduction_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(tenu_be_t60, tenure_contract__demographic_reproduction_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(tenu_be_t70, tenure_contract__demographic_reproduction_reading, base_extractiveness, 70, 0.7).
narrative_ontology:measurement(tenu_be_t80, tenure_contract__demographic_reproduction_reading, base_extractiveness, 80, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(tenu_su_t50, tenure_contract__demographic_reproduction_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(tenu_su_t60, tenure_contract__demographic_reproduction_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(tenu_su_t70, tenure_contract__demographic_reproduction_reading, suppression_requirement, 70, 0.64).
narrative_ontology:measurement(tenu_su_t80, tenure_contract__demographic_reproduction_reading, suppression_requirement, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'tenure' decomposes into at least three structurally distinct claims sharing one kernel. This file (demographic_reproduction_reading) carries high epsilon with victims defined by demographic position; the academic_freedom_reading carries low epsilon with beneficiaries spanning all future inquiry; the institutional_extraction_reading carries high epsilon with victims defined by labor-market timing rather than demographic position. Edges run from this file to both siblings because documented demographic gatekeeping erodes the evidentiary conditions of the protective claim (influence) while competing with the rent-rigidity account for the true-critique slot (coexistence). No averaging occurs across the family; each member keeps its own epsilon, stakeholders, and claimed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
