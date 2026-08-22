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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping (Fit/Collegiality Criteria)
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story instantiates the demographic_reproduction_reading of the
 *   tenure_contract kernel: tenure peer review, evaluated through its
 *   informal 'fit' and 'collegiality' criteria, functions as a mechanism that
 *   reproduces the demographic composition of the incumbent faculty by
 *   discounting candidates whose scholarly style, communication norms, or
 *   research agendas diverge from committee-familiar patterns, using language
 *   that carries no citation trail and admits no formal appeal. This is a
 *   distinct constraint from the academic_freedom_reading (which evaluates
 *   tenure's decoupling of survival from institutional or political
 *   displeasure) and from the institutional_extraction_reading (which
 *   evaluates tenure as rent extraction by early winners against contingent
 *   labor). All three share the same underlying kernel — the tenure contract
 *   as a stabilized commitment — but read radically different structural
 *   facts off it, and each authors its own ε rather than averaging across
 *   readings, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - incumbent_majority_faculty: beneficiary/agenda_setter (institutional/arbitrage) — sets fit criteria that reproduce their own norms
 *   - department_chairs_administering_review: agenda_setter (institutional/constrained) — administers the mechanism, could reform it, bears political cost of doing so
 *   - underrepresented_minority_candidates: payer (moderate/trapped) — bears extraction via unappealable informal criteria
 *   - first_generation_academics: payer (moderate/trapped) — lacks informal networks that teach the unwritten norms being assessed
 *   - candidates_with_nontraditional_research_agendas: payer (moderate/constrained) — penalized for evaluator unfamiliarity mislabeled as fit deficiency
 *   - faculty_equity_offices: excluded (moderate/constrained) — sees aggregate disparity but not the committee-room mechanism producing it
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
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping (Fit/Collegiality Criteria)").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'd117b6b7-411b-4c64-9d05-f0b0f3980fbe').
narrative_ontology:cs_kernel_codification('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', formalized).
narrative_ontology:cs_authority_grounding('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', practice).
narrative_ontology:cs_interpretation_layer_present('d117b6b7-411b-4c64-9d05-f0b0f3980fbe').
narrative_ontology:cs_reading_relation('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', foundational, collegiality_criteria_function_as_demographic_proxy).
narrative_ontology:cs_axiom_status(collegiality_criteria_function_as_demographic_proxy, holdable).
narrative_ontology:cs_axiom_grounding('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', collegiality_criteria_function_as_demographic_proxy, empirically_contingent).
narrative_ontology:cs_axiom('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', secondary, unappealable_informal_criteria_are_illegitimate_regardless_of_intent).
narrative_ontology:cs_axiom_status(unappealable_informal_criteria_are_illegitimate_regardless_of_intent, holdable).
narrative_ontology:cs_axiom_grounding('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', unappealable_informal_criteria_are_illegitimate_regardless_of_intent, deontological).
narrative_ontology:cs_reference_frame('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', collegiality_as_genuine_durability_assessment).
narrative_ontology:cs_drift_state('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', post_civil_rights_era_formal_exclusion_ban, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d117b6b7-411b-4c64-9d05-f0b0f3980fbe', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, department_chairs_administering_review).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_minority_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, first_generation_academics).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, candidates_with_nontraditional_research_agendas).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on tenure committees and write letters that invoke 'fit' and 'collegiality' as legitimate evaluative criteria. Their own scholarly style, communication norms, and social affect were never tested against these criteria because they set them; their tenure cases were adjudicated by people who already looked and argued like them. They benefit from a review process that renders their own patterns invisible as norms rather than as one style among several.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty, agenda_setter).

% Convene the committee, frame the case narrative sent upward, and decide which qualitative concerns get elevated to the dossier. They can shape outcomes heavily through informal signaling ('some concerns about fit') that never appears as a formal criterion and therefore cannot be appealed on its own terms. They administer the mechanism and could reform it, but doing so risks conflict with senior colleagues whose comfort constitutes their own political capital.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, department_chairs_administering_review, agenda_setter,
    institutional, generational, constrained, national).

% Produce research output comparable to or exceeding peers, but face repeated informal characterizations as 'not quite fitting the department culture' or 'difficult in meetings' — assessments with no citation trail and no appeal mechanism because they are not codified criteria. A negative tenure decision ends the academic career track entirely in most fields; there is no lateral market for a denied case, making exit effectively trapped rather than merely costly.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_minority_candidates, payer,
    moderate, biographical, trapped, national).

% Lack the informal mentorship networks that teach unwritten collegiality norms — how to perform ease at receptions, which disagreements are legible as 'rigor' versus 'abrasiveness.' Their research record is evaluated formally, but the informal layer of the process, which they were never taught to navigate, silently discounts them. Like the minority-candidate seat, a denial is close to career-terminal.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, first_generation_academics, payer,
    moderate, biographical, trapped, national).

% Work on topics or methods (e.g. public scholarship, interdisciplinary or community-engaged work) that fall outside the incumbent committee's evaluative fluency; this unfamiliarity is frequently voiced as a fit concern rather than owned as an evaluator limitation. Some retain mobility to different institution types; others are locked into a single subfield's narrow tenure track.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, candidates_with_nontraditional_research_agendas, payer,
    moderate, biographical, constrained, national).

% Benefits from a review process that produces plausible-sounding, difficult-to-litigate denials, insulating the institution from having to defend explicit criteria in court while still achieving demographic continuity in the faculty body it presents to accreditors and donors.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, beneficiary,
    institutional, generational, arbitrage, national).

% Track aggregate tenure outcome disparities and would object to the fit/collegiality criteria's role in producing them, but typically hold advisory rather than decision authority and are not seated on the committees that generate the language driving actual denials.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, faculty_equity_offices, excluded,
    moderate, biographical, constrained, national).

% Provide external letters assessing research contribution; largely insulated from the local fit/collegiality layer, but their formal assessments are weighed against, and sometimes overridden by, the local committee's informal characterization of the candidate.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, external_reviewers_and_discipline, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, incumbent_majority_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally solves a genuine coordination problem: departments need some mechanism to assess whether a colleague will be a durable, functional member of a small, long-lived collegial body, since tenure is effectively a lifetime appointment and interpersonal friction has real costs to a small unit.
% TRANSFER_FUNCTION: Moves career security, salary progression, and institutional standing from candidates whose research is judged adequate but whose informal 'fit' is judged deficient, to a stable incumbent faculty composition — effectively transferring positions that would otherwise open to a more demographically representative pool into continued occupancy by patterns resembling the existing majority.
% ABSENT_VOICES: Denied candidates rarely appear in the institutional record after the fact — files are sealed, litigation is discouraged by confidentiality agreements, and the candidate has typically left academia by the time any pattern would be visible. Faculty equity offices see aggregate numbers but not the committee-room language that produced them.
% DISAPPEARANCE_RATIONALE: If 'fit' and 'collegiality' were stripped from tenure criteria and only documented research, teaching, and service records were weighed, committee deliberations would need to rest on legible, appealable evidence; several currently-denied candidates would likely have been granted tenure, and faculty demographic composition would shift measurably over a decade-scale horizon.
% FOUNDING_PROBLEM: Small academic departments needed a way to assess whether a permanent colleague could function within a collegial governing body over decades, given that formal scholarly metrics alone don't capture interpersonal durability or shared governance capacity.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent faculty and administrators attest the collegiality concern remains live — citing real cases of dysfunctional permanent hires. Faculty equity offices, disparity-focused institutional researchers, and denied candidates who have gone public attest that in practice the criterion functions predominantly as an unaccountable proxy for demographic and stylistic conformity, citing outcome disparities that formal research-productivity metrics do not explain.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.71) and rising because informal fit/collegiality criteria have, on this reading, no natural ceiling — they can absorb any research-record improvement by shifting the qualitative complaint, which is why the theater_ratio also rises in tandem (0.58): more of the formal review apparatus (letters, meetings, rubrics) increasingly serves to launder an outcome already reached informally. Suppression (0.68) reflects that the criteria are structurally unappealable — there is no formal standard against which 'lack of fit' can be contested, which is a stronger suppressive mechanism than an explicit, contestable rule would be. Accessibility_collapse (0.62) and resistance (0.55) are set at rope/tangled_rope-typical mid-levels rather than mountain levels: alternative evaluative frameworks (structured rubrics, blinded review of scholarly output) are demonstrably workable and are resisted, not physically foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-faculty seat, the criteria read as ordinary, functional collegiality assessment — a genuine coordination need for a small permanent body. From the payer seats, the identical committee process reads as unaccountable demographic filtering. The engine should compute these as structurally different experiences of the same mechanism from the declared power/exit/scope data; this divergence is the analytical point of the reading, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent majority faculty and administration are declared beneficiaries: the informal criteria that would burden an atypical candidate simply do not apply to evaluators whose own style set the informal norm, so their derived directionality sits near the full-beneficiary end. Underrepresented minority candidates, first-generation academics, and nontraditional-agenda candidates are declared victims with trapped or constrained exit — a tenure denial is close to career-terminal in most disciplines, which is exactly the condition that pushes derived d toward the full-target end rather than a milder symmetric reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (assessing durable collegial fitness for a lifetime appointment) may remain partially live even as its current instantiation functions predominantly as demographic gatekeeping — the founding_problem_status is authored 'contested' rather than 'dead' because incumbent faculty can point to real cases of interpersonal dysfunction the criterion was built to catch. Classifying this as tangled_rope rather than pure snare preserves that genuine residual coordination function while still naming the asymmetric extraction that rides on it — collapsing to snare would erase the coordination claim incumbents can honestly point to; collapsing to rope would erase the documented outcome disparity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fit_criterion_severability,
    'Is the ''fit and collegiality'' criterion severable from a genuine collegial-durability assessment, or is any operationalization of it necessarily proxy-laden for demographic and stylistic conformity?',
    'Comparative outcome analysis across departments that have replaced informal fit language with structured, documented collegiality rubrics (e.g. specific behavioral incidents, peer 360 processes) versus departments retaining undocumented informal criteria; if disparities persist under structured rubrics, the criterion itself (not merely its informality) is doing the exclusionary work.',
    'If severable, the fix is proceduralization (formal rubrics) and the constraint moves toward rope/scaffold; if not severable, the criterion is intrinsically extractive and no amount of proceduralization resolves it, supporting continued tangled_rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fit_criterion_severability, empirical, 'Whether fit/collegiality criteria can be operationalized without demographic proxy effects.').

omega_variable(
    founding_problem_genealogy_contested,
    'Was the informal fit/collegiality layer of tenure review ever primarily designed to assess interpersonal durability, or did it emerge (or get retained) specifically because it provided plausible deniability for demographic exclusion once explicit exclusionary criteria became legally unavailable?',
    'Historical institutional record analysis: compare the era in which formal demographic exclusion criteria were legally struck down against the timing of increased reliance on informal ''fit'' language in tenure dossiers.',
    'If the informal criterion''s prominence rose in temporal proximity to the removal of explicit exclusionary mechanisms, this strongly supports reading the criterion as substitute gatekeeping rather than genuine, independently-motivated collegiality assessment — sharpening the case for snare over tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_genealogy_contested, conceptual, 'Whether the informal criterion is a genealogically independent function or a substitute for struck-down exclusionary mechanisms.').

omega_variable(
    reading_selection_under_determination,
    'Given that the same tenure contract supports three structurally distinct readings (academic freedom, demographic reproduction, institutional extraction), is the demographic-reproduction framing the dominant lens for THIS specific case, or does the choice of lens depend on which stakeholder group is narrating the same committee decision?',
    'Cross-reading comparison of the same tenure denial cases: does the academic_freedom_reading''s stakeholder set (protecting inquiry) and this reading''s stakeholder set (demographic gatekeeping) converge on the same case files, or diverge systematically by who is narrating?',
    'If the readings converge on the same case files with the same evidentiary weight, that strengthens the claim that this is the operative structural fact rather than a contested interpretive overlay; if they diverge by narrator identity alone, the reading''s claim to be describing structure rather than perspective weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the demographic-reproduction framing is a structural fact about the mechanism or a narrator-dependent overlay on a shared kernel.').


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
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__demographic_reproduction_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__demographic_reproduction_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__demographic_reproduction_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__demographic_reproduction_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__demographic_reproduction_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__demographic_reproduction_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__demographic_reproduction_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__demographic_reproduction_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__demographic_reproduction_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__demographic_reproduction_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__demographic_reproduction_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__demographic_reproduction_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language label 'tenure contract' per the ε-invariance principle. academic_freedom_reading authors low extraction (genuine coordination protecting inquiry from retaliation); institutional_extraction_reading authors extraction along a labor-rigidity axis (early winners vs. contingent labor); this story (demographic_reproduction_reading) authors extraction along a demographic-composition axis (incumbent majority vs. underrepresented candidates). Each carries a distinct ε, distinct beneficiary/victim sets, and a distinct claimed_type; they are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
