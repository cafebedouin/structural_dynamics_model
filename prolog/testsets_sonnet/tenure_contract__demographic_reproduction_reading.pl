% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story instantiates the demographic_reproduction_reading of the
 *   tenure_contract kernel: tenure peer review, evaluated specifically
 *   through the lens of who is denied on collegiality/fit grounds relative to
 *   documented research output, operates as a mechanism that reproduces the
 *   demographic composition of incumbent faculty. This is a distinct
 *   constraint from the academic_freedom_reading (tenure as protection for
 *   risky inquiry) and the institutional_extraction_reading (tenure as rent
 *   extraction against contingent labor) — same underlying institution, three
 *   structurally distinct claims with three different beneficiary/victim
 *   structures and three different epsilon values. Rising theater_ratio and
 *   suppression_requirement over the measured interval track the increasing
 *   use of confidential, unappealable collegiality language as formal
 *   diversity-in-hiring pressure has grown — the criterion has become more
 *   theatrical (harder to audit, more insulated from productivity comparison)
 *   precisely as external scrutiny of demographic outcomes has intensified.
 *
 * KEY AGENTS:
 *   - incumbent_majority_faculty: agenda_setter/beneficiary (institutional/arbitrage) — administers fit judgments, bears no risk under the standard
 *   - underrepresented_minority_candidates: primary payer (moderate/trapped) — bears extraction through denied tenure despite comparable productivity
 *   - women_in_male_dominated_fields: payer (moderate/constrained) — bears extraction through gendered collegiality double standards
 *   - first_generation_academics: payer (powerless/trapped) — lacks inherited social fluency the fit criterion silently rewards
 *   - civil_rights_and_faculty_advocacy_groups: excluded observer (organized/analytical) — documents the pattern but has no standing in individual decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.71).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.68).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '8c8cb934-f7c4-4d8f-8b7f-a4255e295ade').
narrative_ontology:cs_kernel_codification('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', formalized).
narrative_ontology:cs_authority_grounding('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', practice).
narrative_ontology:cs_interpretation_layer_present('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade').
narrative_ontology:cs_reading_relation('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', foundational, fit_criteria_track_demographic_similarity_not_merit).
narrative_ontology:cs_axiom_status(fit_criteria_track_demographic_similarity_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', fit_criteria_track_demographic_similarity_not_merit, empirically_contingent).
narrative_ontology:cs_axiom('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', secondary, unreviewable_discretionary_judgment_cannot_certify_neutral_evaluation).
narrative_ontology:cs_axiom_status(unreviewable_discretionary_judgment_cannot_certify_neutral_evaluation, holdable).
narrative_ontology:cs_axiom_grounding('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', unreviewable_discretionary_judgment_cannot_certify_neutral_evaluation, conventional).
narrative_ontology:cs_reference_frame('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', peer_judgment_of_scholarly_and_collegial_fitness).
narrative_ontology:cs_drift_state('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', post_diversity_audit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c8cb934-f7c4-4d8f-8b7f-a4255e295ade', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, departments_maintaining_demographic_status_quo).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_minority_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, women_in_male_dominated_fields).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, first_generation_academics).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, candidates_with_nonconforming_research_styles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administrations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on tenure committees and write the letters that decide who is judged to 'fit' the department. Apply collegiality and fit criteria that track cultural similarity to themselves — shared communication style, research taste, social ease at department functions. Already tenured, so they bear none of the risk of the standard they administer, and their own past evaluation under looser or more homogeneous cohorts is never revisited.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty, beneficiary).

% As institutional entities, benefit from low internal friction, predictable colleague behavior, and continuity of departmental culture across hiring cycles. The 'fit' criterion externalizes the labor of demographic integration onto candidates rather than requiring the department to change; the arrangement reproduces itself with minimal institutional cost.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, departments_maintaining_demographic_status_quo, beneficiary,
    institutional, civilizational, arbitrage, national).

% Produce comparable or superior research records but are evaluated on vague collegiality and fit standards applied inconsistently and post hoc. A tenure denial forecloses re-entry to the same institutional tier of the profession; the multi-year probationary period means the cost of a negative fit judgment is an entire career trajectory, not a single job. Lateral moves are possible but land in departments with the same structural evaluation logic.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_minority_candidates, payer,
    moderate, biographical, trapped, national).

% Face collegiality assessments that penalize assertiveness rewarded in male colleagues, and service/mentorship burdens that consume research time but are not credited as fit-enhancing in the way informal collaboration among same-gender peers is. Exit to industry exists in some fields but forfeits the specific research trajectory built over the probationary period.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, women_in_male_dominated_fields, payer,
    moderate, biographical, constrained, national).

% Lack the inherited social fluency in departmental norms, conference codes, and informal mentorship networks that produce a 'good fit' read. Their research is judged on the same rubric but their social legibility to the committee is lower by construction, and no formal mechanism translates unfamiliarity with the norms into a documented deficiency they could contest.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, first_generation_academics, payer,
    powerless, biographical, trapped, national).

% Pursue interdisciplinary, activist, or methodologically unconventional research that correlates with demographic minority status in many fields; face fit judgments that recast disciplinary disagreement as personal incompatibility. Their productivity metrics may be strong but are discounted as 'not really what we do here.'
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, candidates_with_nonconforming_research_styles, payer,
    moderate, biographical, constrained, national).

% Ratify department-level tenure recommendations with limited independent review, benefiting from the deniability of a peer-driven process while diversity metrics used in institutional marketing lag behind. Occasionally intervene when legal exposure (discrimination suits) becomes visible, but structurally defer to departmental fit judgments as the default.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administrations, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, university_administrations, beneficiary).

% Document patterns of disparate tenure denial and would argue for standardized, productivity-anchored evaluation criteria replacing 'fit' and 'collegiality' language, but have no seat on individual tenure committees and can only intervene after the fact through grievance or litigation channels.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, civil_rights_and_faculty_advocacy_groups, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Peer evaluation of scholarly quality that formal metrics (citation counts, grant totals) cannot fully capture — assessing whether work is rigorous, original, and likely to sustain a research trajectory requires situated judgment by disciplinary peers.
% TRANSFER_FUNCTION: Moves career security and institutional voice from candidates judged demographically or culturally atypical to the incumbent majority, via a discretionary 'fit'/'collegiality' criterion that runs parallel to, and can override, the productivity criterion nominally governing the decision.
% ABSENT_VOICES: Denied candidates rarely see the substance of collegiality objections (protected as confidential peer deliberation) and have no venue to contest characterizations of their personality or lab culture before the decision is final. Civil rights and faculty advocacy groups that document the pattern across institutions have no standing inside any single department's process.
% DISAPPEARANCE_RATIONALE: If fit and collegiality criteria were stripped from tenure standards and evaluation ran solely on documented research productivity and teaching record, departmental demographic composition would shift measurably within a decade, incumbent committees would lose discretionary control over succession, and a body of grievance and litigation practice built around contesting these criteria would become largely moot.
% FOUNDING_PROBLEM: Departments needed a mechanism to assess whether a probationary scholar would sustain independent, high-quality research and function as a durable colleague in a small, long-lived collegial body — something CVs and citation counts alone could not certify.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent faculty and departments attest the collegiality function is still necessary for functioning small research units. Independent audits by faculty senates at several universities, EEOC investigations, and peer-reviewed sociology-of-science literature (attesting from outside the tenured beneficiary population) document that 'fit' and 'collegiality' language correlates with demographic outcome disparities uncorrelated with independently measured research output, supporting the reading that the criterion has drifted from its stated function toward reproduction of existing composition.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.71) reflects that a substantial share of tenure denials under this reading are attributable to fit/collegiality judgment rather than documented productivity shortfall, and that this share falls disproportionately on demographically atypical candidates. Suppression (0.68) is high because the confidentiality of peer deliberation and the vagueness of the criteria make individual denials nearly impossible to contest — accessibility_collapse (0.62) is elevated because, once a candidate understands the criterion is discretionary and unreviewable, there is no internal appeal path that reliably reopens the substantive judgment. Resistance (0.55) is moderate: contestation exists (grievance filings, EEOC complaints, faculty senate reform pushes) but rarely reverses individual decisions and mostly operates at the aggregate policy level, years after the fact.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent committee's seat, collegiality assessment is a genuine, necessary judgment about whether someone will function well in a small permanent body — indistinguishable, from inside, from ordinary professional evaluation. From the denied candidate's seat, the same judgment is unreviewable, unappealable, and correlated with demographic traits unrelated to scholarly merit. The engine computing different types from these two structural positions is exactly the phenomenon this reading is written to surface — under the academic_freedom_reading of the same kernel, tenure's protective function is real; under this reading, the demographic-sorting function riding alongside it is what gets measured.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent majority faculty and demographically homogeneous departments sit at the beneficiary end: they administer the criterion, are shielded from its discretion by already holding tenure, and benefit from continuity of departmental culture. Underrepresented minority candidates, women in male-dominated fields, first-generation academics, and nonconforming researchers sit at the target end: trapped or constrained exit options because a tenure denial forecloses the specific institutional tier they invested a probationary period building toward, with no lateral escape from the same evaluation logic elsewhere in the profession.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assessing sustained scholarly quality and collegial function that metrics alone can't certify — is not dead; peer judgment of research trajectory remains genuinely necessary. But the founding_problem_status is contested because the specific instrument (fit/collegiality as applied) has drifted from assessing scholarly trajectory to assessing cultural similarity, while retaining the institutional legitimacy of the original, narrower coordination function. Declaring this tangled_rope rather than snare preserves that the underlying peer-review function is real coordination (distinguishing rigor from padding) even as an asymmetric extraction mechanism (demographic sorting) rides on the same procedural surface and requires active enforcement (confidentiality of deliberation, absence of appeal) to persist undetected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collegiality_criterion_independent_validity,
    'Does ''collegiality'' as applied by tenure committees measure a real, function-relevant trait (ability to sustain long-term departmental cooperation) independent of demographic similarity to the evaluators, or is it structurally inseparable from cultural-similarity judgment given how it is currently operationalized?',
    'Blind or structured-rubric evaluation trials comparing collegiality scores assigned under anonymized versus identified review, cross-referenced against post-tenure collegial functioning outcomes (committee service, co-authorship, departmental conflict rates) to test whether the criterion predicts anything beyond demographic similarity.',
    'If collegiality scores predict real post-tenure outcomes independent of demographic similarity, the coordination function is more substantial than this reading credits and the constraint moves toward genuine (if imperfect) rope; if scores predict nothing but similarity to evaluators, the coordination story is closer to pure cover and the constraint moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collegiality_criterion_independent_validity, empirical, 'Whether collegiality assessment has independent predictive validity or is a proxy for demographic similarity.').

omega_variable(
    reading_decomposition_boundary,
    'Is the demographic-reproduction function of tenure review structurally separable from the academic-freedom function and the extraction-against-contingent-labor function, or do all three ride on the same discretionary review apparatus such that reforming one necessarily reforms the others?',
    'Compare institutions that have implemented structured, productivity-anchored tenure rubrics (removing discretionary fit/collegiality language) against peer institutions retaining discretionary criteria, tracking demographic outcomes, contingent-labor cost-shifting, and researcher risk-taking (proxied by citation-lag or controversial-topic publication) across both.',
    'If the three functions are separable, each sibling reading names a genuinely distinct constraint and reform of one need not disturb the others; if inseparable, the kernel readings are more entangled than the ε-invariance framing assumes and a fourth, unified story may be warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_decomposition_boundary, conceptual, 'Whether the three kernel readings identify structurally independent mechanisms or one entangled apparatus.').

omega_variable(
    committee_composition_endogeneity,
    'Is demographically homogeneous committee composition a cause of biased fit judgments, or is it itself downstream of decades of prior tenure decisions under this same criterion — i.e., is the constraint self-reinforcing across generations in a way that makes ''reform'' and ''reproduction'' the same historical process viewed at different points?',
    'Longitudinal tracking of committee composition and denial-rate disparities across multiple tenure cycles at the same institutions, testing whether disparities shrink, persist, or grow as composition slowly diversifies.',
    'If self-reinforcing, the constraint''s persistence does not require any single committee to act in bad faith — the extraction is structural and generational, which affects what remedy (individual accountability vs. structural rule change) would actually work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committee_composition_endogeneity, empirical, 'Whether demographic reproduction is a self-reinforcing multigenerational process independent of any single committee''s intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__demographic_reproduction_reading, theater_ratio, 8, 0.41).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__demographic_reproduction_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__demographic_reproduction_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__demographic_reproduction_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__demographic_reproduction_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__demographic_reproduction_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__demographic_reproduction_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__demographic_reproduction_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__demographic_reproduction_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__demographic_reproduction_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__demographic_reproduction_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__demographic_reproduction_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the colloquial concept 'tenure peer review' under the ε-invariance principle: academic_freedom_reading (Rope/Tangled-Rope candidate — genuine coordination protecting inquiry), demographic_reproduction_reading (this story — Tangled Rope, fit/collegiality criteria reproducing incumbent composition), and institutional_extraction_reading (rent extraction against contingent labor). Each has its own beneficiary/victim structure and its own ε; none is a measurement-basis variant of the others. Linked here via affects_constraints; the demographic-reproduction function plausibly amplifies the extraction reading's contingent-labor cost-shifting by narrowing the tenured pool's composition, and it structurally undercuts the academic-freedom reading's legitimacy claim insofar as the freedom protected turns out to correlate with prior demographic sorting rather than with inquiry risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
