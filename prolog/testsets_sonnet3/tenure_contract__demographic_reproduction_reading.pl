% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story reads the tenure kernel through the lens of demographic
 *   reproduction: peer review's 'fit' and 'collegiality' criteria function as
 *   an unmeasured, discretionary filter that systematically disadvantages
 *   candidates who deviate from the department's incumbent demographic and
 *   intellectual profile, while research productivity — the criterion that
 *   would be race/gender/paradigm-neutral in principle — is available to be
 *   overridden by the qualitative filter whenever the outcome would otherwise
 *   favor a non-incumbent-profile candidate. Rising theater_ratio over the
 *   interval reflects institutions layering diversity-statement requirements,
 *   bias training, and procedural safeguards atop the same discretionary core
 *   without displacing it — proceduralization that performs reform while the
 *   underlying gatekeeping function persists and, on the record, intensifies
 *   its share of denial rationales.
 *
 * KEY AGENTS:
 *   - incumbent_dominant_group_faculty: structural beneficiary and agenda-setter — administers the fit criterion, is never measured against it
 *   - underrepresented_minority_candidates: primary target — strong research records overridden by unmeasured fit findings
 *   - women_faculty_in_male_dominated_fields: primary target — double-bind between required service labor and collegiality scrutiny of assertiveness
 *   - faculty_with_nonconforming_research_agendas: secondary target — paradigm nonconformity read as fit failure independent of demographic category
 *   - university_administration: secondary beneficiary — outsources disparate-impact liability to peer judgment framed as academic autonomy
 *   - prospective_diverse_applicant_pool: excluded — screened out before entering the pipeline, invisible to the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.71).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.62).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '172f8ca9-fc98-4d72-bb9d-f0783b3e31ee').
narrative_ontology:cs_kernel_codification('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', formalized).
narrative_ontology:cs_authority_grounding('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', practice).
narrative_ontology:cs_interpretation_layer_present('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee').
narrative_ontology:cs_reading_relation('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', foundational, collegiality_fit_is_discretionary_proxy_not_neutral_measure).
narrative_ontology:cs_axiom_status(collegiality_fit_is_discretionary_proxy_not_neutral_measure, holdable).
narrative_ontology:cs_axiom_grounding('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', collegiality_fit_is_discretionary_proxy_not_neutral_measure, empirically_contingent).
narrative_ontology:cs_axiom('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', secondary, peer_governance_legitimacy_requires_even_handed_application).
narrative_ontology:cs_axiom_status(peer_governance_legitimacy_requires_even_handed_application, holdable).
narrative_ontology:cs_axiom_grounding('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', peer_governance_legitimacy_requires_even_handed_application, conventional).
narrative_ontology:cs_reference_frame('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', peer_governed_meritocratic_evaluation).
narrative_ontology:cs_drift_state('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', post_disparate_impact_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('172f8ca9-fc98-4d72-bb9d-f0783b3e31ee', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, incumbent_dominant_group_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, department_chairs_administering_fit_criteria).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_minority_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, women_faculty_in_male_dominated_fields).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, faculty_with_nonconforming_research_agendas).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, collegiality_is_a_legitimate_tenure_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on tenure and promotion committees where 'fit' and 'collegiality' are invoked as evaluative criteria. Their own scholarly style, temperament, and social affect are the implicit baseline against which candidates are judged, so they rarely trigger the criteria against themselves. They control committee composition, letter-solicitation, and the weighting of subjective factors relative to publication counts.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, incumbent_dominant_group_faculty, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, incumbent_dominant_group_faculty, agenda_setter).

% Convene tenure committees, frame case narratives, and decide which qualitative concerns get elevated to the dean or provost. They administer the vague criteria and could tighten them to research-output-only standards, but doing so would require overriding senior faculty who benefit from discretion; the chair's own promotion depended on the same latitude.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, department_chairs_administering_fit_criteria, agenda_setter,
    institutional, generational, arbitrage, national).

% Undergo the same publication review as everyone else but are additionally evaluated on collegiality and cultural fit — criteria that penalize accented speech, unfamiliar mentorship styles, community-engaged scholarship framed as 'not rigorous,' or advocacy work read as 'political.' A negative fit finding can override a strong research record; appeal mechanisms exist on paper but are adjudicated by the same body that produced the finding. Leaving means restarting a tenure clock elsewhere with no guarantee of a different outcome.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_minority_candidates, payer,
    moderate, biographical, constrained, national).

% Face collegiality scrutiny keyed to deference and likability norms; assertiveness that reads as normal ambition in male peers reads as 'difficult' in fit evaluations. Service and mentoring labor disproportionately assigned to them is simultaneously required and undervalued in the research-productivity half of the file, creating a double bind the fit criterion resolves against them.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, women_faculty_in_male_dominated_fields, payer,
    moderate, biographical, constrained, national).

% Pursue interdisciplinary, applied, or politically salient scholarship that departs from the department's dominant paradigm. Fit language ('not a good match for the department's intellectual culture') is used to disqualify strong research records that would otherwise clear a productivity-only bar.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, faculty_with_nonconforming_research_agendas, payer,
    moderate, biographical, constrained, national).

% Benefits from a self-policing faculty body that absorbs the legal and reputational cost of exclusionary decisions under a peer-review label, insulating the institution from direct liability for demographic outcomes while retaining plausible deniability that the process is objective.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, beneficiary,
    institutional, generational, mobile, national).

% Never enters the tenure pipeline at all because hiring committees anticipating tenure-stage fit problems screen candidates out earlier, or because the visible pattern of fit-based denials discourages application. They have no seat in any committee and no visibility into why the pool looks as it does.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, prospective_diverse_applicant_pool, excluded,
    powerless, biographical, trapped, national).

% Review aggregate tenure outcome data for disparate impact and can flag patterns, but typically lack authority to overturn a specific tenure committee's substantive judgment about collegiality, which is treated as academic judgment beyond their remit.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, affirmative_action_and_equity_offices, observer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, affirmative_action_and_equity_offices, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, incumbent_dominant_group_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Peer evaluation of scholarly quality by disciplinary experts who can judge work no outside administrator could competently assess, plus assessment of whether a colleague can function within a small, permanent, self-governing unit.
% TRANSFER_FUNCTION: Moves career security, salary trajectory, and institutional voice from candidates whose research records are strong but who fail an unmeasured 'fit' test, to incumbents whose own unexamined fit is never tested — while insulating the institution from having to defend demographic outcomes as policy choices.
% ABSENT_VOICES: Prospective applicants who self-select out of the pipeline after observing denial patterns are never present to testify to their exclusion; denied candidates who leave academia rarely appear in any subsequent institutional record; equity offices are present but structurally boxed out of substantive judgment.
% DISAPPEARANCE_RATIONALE: If 'fit' and 'collegiality' were stripped from tenure criteria overnight and evaluation ran on research productivity and teaching record alone, tenure rates for the affected groups would shift, senior faculty would lose a discretionary lever currently used to shape department composition, and administrations would lose a mechanism that currently absorbs disparate-impact liability under the cover of academic judgment.
% FOUNDING_PROBLEM: Faculty governance needed a way to assess qualities relevant to functioning in a small permanent self-governing body — collaborative viability, professional conduct, willingness to do committee and mentoring work — that publication counts alone don't capture.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent faculty and administrations attest the fit criterion still serves a live governance need. Equity-office data, EEOC disparate-impact filings, and peer-reviewed sociology-of-the-academy research from outside any single department's beneficiary group attest that in practice the criterion functions primarily to reproduce existing demographic composition rather than to assess collaborative capacity — an attestation from outside the benefiting parties that the stated founding problem has been substantially displaced by its own instrument.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high and rising (0.52 to 0.71) because the coordination story (assessing collaborative viability) is real but thin relative to the volume of career-determining discretion it licenses, and because the qualitative override capacity is structurally asymmetric — it can veto a strong research file but cannot, by design, be vetoed by a weak one for an incumbent-profile candidate. Suppression (0.62) reflects that dissenting candidates face a closed appeals loop adjudicated by the same body, not open external review. Accessibility_collapse (0.66) reflects that once tenure is denied on fit grounds, the same discretionary standard reasserts itself at most peer institutions, closing lateral escape. Theater_ratio (0.58) captures the growing proceduralization — DEI statements, bias-awareness training, structured rubrics — layered on top of an unreformed discretionary core.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent dominant-group faculty and the administrators who benefit from cost-free liability transfer sit near the beneficiary end: they set the criteria, are structurally exempt from being measured by them, and collect career security and institutional shaping power. Underrepresented candidates, women in male-dominated fields, and nonconforming-research faculty sit near the target end: they face the same productivity bar plus an additional, unmeasured filter with no reciprocal application. The excluded applicant pool never enters the visible dataset at all, which is itself part of the extraction — it launders the appearance of a race/gender-neutral applicant pipeline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assessing collaborative capacity in a small permanent self-governing unit — was real when the criterion was created and remains partially live in the abstract. But under this reading its declared function has been substantially captured by a demographic-reproduction function: equity-office data and external sociology-of-the-academy research (corroboration from outside the beneficiary set) attest that the criterion's practical operation tracks incumbent demographic profile far more reliably than any measurable collaborative-viability outcome. Classifying this as tangled_rope rather than snare preserves the genuine (if diminished) coordination residue — some fit assessment is not pure fabrication — while still naming the asymmetric extraction that the coordination story now primarily launders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fit_criterion_measurability_ambiguity,
    'Is ''collegiality''/''fit'' a coherent, measurable construct tracking genuine collaborative-viability risk, or is it structurally unfalsifiable — applied post hoc to justify whatever outcome the committee''s demographic composition already favors?',
    'Blind audit comparing fit-language incidence and valence across matched research-productivity-equivalent candidate pairs differing in demographic category or paradigm conformity; if fit language tracks demographic category independent of any documented collegiality incident, the construct is functioning as a proxy rather than a genuine measure.',
    'If fit is shown to be a demographic proxy, this reading strengthens toward snare (coordination function collapses to pure cover); if genuine collegiality incidents predict fit findings independent of demographic category, the tangled_rope coordination residue is larger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fit_criterion_measurability_ambiguity, empirical, 'Whether the fit/collegiality criterion has independent content or is a demographic proxy.').

omega_variable(
    academic_freedom_claim_dependency,
    'Does the academic-freedom justification for tenure depend on the fit criterion being applied evenly, such that demographically skewed application undermines the academic-freedom reading''s own legitimacy claim, or are the two readings fully independent (tenure protects inquiry for whoever survives the gate, regardless of how the gate is constructed)?',
    'Compare research risk-taking and viewpoint diversity outcomes for tenured cohorts admitted under high-fit-discretion versus low-fit-discretion (productivity-only) tenure regimes at peer institutions; convergent risk-taking outcomes would support independence, divergent outcomes would support dependency.',
    'If dependent, demographic gatekeeping under this reading structurally undermines the sibling academic_freedom_reading''s normative force rather than merely coexisting with it — the constraints would not merely coexist but the demographic_reproduction_reading would corrode the legitimacy conditions the academic_freedom_reading relies on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(academic_freedom_claim_dependency, conceptual, 'Whether demographic capture of the fit criterion undermines the academic-freedom justification for tenure generally.').

omega_variable(
    coalition_remedy_feasibility,
    'Could underrepresented and nonconforming-research faculty form an effective coalition (cross-department, cross-institution) to press for productivity-only tenure standards, given their dispersed, individually moderate power?',
    'Track outcomes of existing faculty-union or AAUP-affiliated campaigns for standardized, rubric-based tenure review; measure whether such campaigns achieved binding rubric adoption or were absorbed into procedural theater (additional forms without reduced discretion).',
    'If coalition remedies have historically been absorbed into theater rather than binding rubric change, the prohibitive fixing_cost rating is corroborated; if binding rubric reforms have succeeded elsewhere, fixing_cost may be overstated as prohibitive for all institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_remedy_feasibility, empirical, 'Whether affected faculty have a feasible collective remedy path despite individually moderate power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__demographic_reproduction_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__demographic_reproduction_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__demographic_reproduction_reading, theater_ratio, 24, 0.49).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__demographic_reproduction_reading, theater_ratio, 32, 0.54).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__demographic_reproduction_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__demographic_reproduction_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__demographic_reproduction_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__demographic_reproduction_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__demographic_reproduction_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__demographic_reproduction_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__demographic_reproduction_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__demographic_reproduction_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__demographic_reproduction_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the tenure_contract kernel. academic_freedom_reading authors low-to-moderate ε keyed to inquiry protection for the tenured; institutional_extraction_reading authors high ε keyed to rent-lock against contingent labor; this story authors high ε keyed to demographic/paradigm gatekeeping via the 'fit' criterion specifically, with a distinct victim set (demographic and paradigm outsiders rather than contingent faculty broadly). The three share the same underlying tenure_contract kernel and institutional machinery but are not the same constraint — each has its own stable ε and victim structure per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
