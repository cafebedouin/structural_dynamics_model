% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Tenure as Academic Freedom Guarantee
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This story instantiates the academic_freedom_reading of the
 *   tenure_contract kernel: tenure as a pre-commitment device that decouples
 *   a researcher's survival from institutional or political displeasure,
 *   enabling inquiry that would otherwise be individually irrational to
 *   pursue. Under this reading, faculty are the coordination beneficiaries
 *   (low effective extraction, gains from independence), students and the
 *   public are diffuse downstream beneficiaries via research quality, and
 *   external political actors attempting to control institutional outputs are
 *   the parties who experience the arrangement as costly friction (high
 *   effective extraction from their seat, because the due-process requirement
 *   is precisely what makes suppression of unpopular findings difficult).
 *   This is a distinct constraint from the institutional_extraction_reading
 *   (which treats the same contractual form as rent extraction by incumbents
 *   against contingent labor) and the demographic_reproduction_reading (which
 *   treats peer review under tenure as demographic gatekeeping). All three
 *   share a kernel — the tenure contract as a stabilized commitment — but
 *   each reading identifies different beneficiaries, different victims, and a
 *   different ε. This file speaks only to the academic-freedom function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.28).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.35).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Guarantee").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '33ded96d-3463-4747-bef2-29c0d83090c1').
narrative_ontology:cs_kernel_codification('33ded96d-3463-4747-bef2-29c0d83090c1', formalized).
narrative_ontology:cs_authority_grounding('33ded96d-3463-4747-bef2-29c0d83090c1', practice).
narrative_ontology:cs_interpretation_layer_present('33ded96d-3463-4747-bef2-29c0d83090c1').
narrative_ontology:cs_reading_relation('33ded96d-3463-4747-bef2-29c0d83090c1', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('33ded96d-3463-4747-bef2-29c0d83090c1', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('33ded96d-3463-4747-bef2-29c0d83090c1', foundational, inquiry_requires_survival_independence).
narrative_ontology:cs_axiom_status(inquiry_requires_survival_independence, holdable).
narrative_ontology:cs_axiom_grounding('33ded96d-3463-4747-bef2-29c0d83090c1', inquiry_requires_survival_independence, instrumental).
narrative_ontology:cs_axiom('33ded96d-3463-4747-bef2-29c0d83090c1', secondary, due_process_dismissal_standard_serves_scholarship_not_incumbency).
narrative_ontology:cs_axiom_status(due_process_dismissal_standard_serves_scholarship_not_incumbency, holdable).
narrative_ontology:cs_axiom_grounding('33ded96d-3463-4747-bef2-29c0d83090c1', due_process_dismissal_standard_serves_scholarship_not_incumbency, conventional).
narrative_ontology:cs_reference_frame('33ded96d-3463-4747-bef2-29c0d83090c1', post_ross_case_academic_freedom_settlement).
narrative_ontology:cs_drift_state('33ded96d-3463-4747-bef2-29c0d83090c1', contemporary_legislative_pressure_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('33ded96d-3463-4747-bef2-29c0d83090c1', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students_and_public_via_research_quality).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, political_actors_seeking_institutional_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold indefinite appointments removable only for cause through a due-process hearing. Can pursue controversial, long-horizon, or politically unpopular research questions without risking dismissal for the conclusions reached. Exit from any single institution is costly but the protection itself is portable in kind across the sector.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, civilizational, arbitrage, national).

% Administers the tenure review process, grants or withholds tenure, and defends faculty against external pressure to dismiss or discipline them for unpopular findings. Also absorbs political and funding backlash on the faculty's behalf, which it cannot fully deflect.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Receive the downstream benefit of research and teaching produced under conditions where faculty are not incentivized to self-censor findings that displease funders, administrators, or political actors. Do not participate in the tenure process itself; benefit is indirect and diffuse.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students_and_public_via_research_quality, beneficiary,
    moderate, generational, analytical, national).

% Legislators, boards, and donors who wish to defund, discipline, or remove faculty whose research or public statements are politically inconvenient. Tenure's due-process requirement raises the cost of doing so to the point of near-impossibility absent a formal for-cause proceeding, which is exactly the friction the arrangement is built to generate against this seat.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, political_actors_seeking_institutional_control, payer,
    powerful, biographical, constrained, national).

% Perform substantial teaching and research labor without the protection, job security, or voice in governance that tenure confers. Would have grounds to object that the protection is unevenly distributed, but their situation is analytically bracketed out of THIS reading, which concerns tenure's function for the protected class, not its distributive effects on the unprotected class (see sibling reading: institutional_extraction_reading).
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_and_adjunct_faculty, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without a credible pre-commitment against retaliatory dismissal, individual researchers rationally under-invest in research that is high-risk, long-horizon, or likely to produce unpopular conclusions, because any single researcher bears the full career cost of institutional or political displeasure. Tenure decouples survival from displeasure, making such inquiry individually rational again.
% TRANSFER_FUNCTION: Moves the cost of institutional and political friction from the individual researcher to the institution as a whole (which must defend a tenured position through formal process) and, in the limit, to political actors who must now clear a higher procedural bar to remove a faculty member than they would for an at-will employee.
% ABSENT_VOICES: Contingent and adjunct faculty, who perform much of the teaching and research labor without the protection, are not parties to this reading's coordination story; they would object that the protection is a rationed good rather than a general labor-market feature — that objection belongs to the sibling institutional_extraction_reading, not this one.
% DISAPPEARANCE_RATIONALE: If tenure protections vanished overnight, faculty research agendas would measurably shift away from politically contested or institutionally inconvenient topics within a few review cycles, administrators would face direct political pressure to discipline individual researchers without a due-process buffer, and the academic labor market would reorganize around at-will risk premiums.
% FOUNDING_PROBLEM: Early-20th-century cases of professors dismissed for unpopular economic, political, or scientific positions (e.g. the Ross case at Stanford, Scopes-era biology faculty) demonstrated that at-will academic employment let institutional trustees and outside political actors directly control the content of scholarship by threatening livelihood.
% FOUNDING_PROBLEM_CORROBORATION: Faculty governance bodies and the AAUP (a body institutionally downstream of tenure's beneficiaries but not identical to any single faculty member) attest the founding problem remains live, citing contemporary legislative efforts to defund or dismiss faculty over politically contested research. External critics — including some university boards and state legislators outside the beneficiary class — contest this, arguing the protection has drifted from shielding controversial inquiry to shielding unproductive incumbents; that drift claim is the subject of the sibling institutional_extraction_reading, not adjudicated here.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low-to-moderate (0.28 at interval end) because, under this reading, the primary transfer is a procedural cost imposed on would-be suppressors rather than a rent extracted from a captive population — the people who pay are political actors whose exit options are themselves constrained by the arrangement's design, not powerless dependents. Suppression is authored moderate (0.35) reflecting the genuine friction tenure imposes on dismissal, which is the mechanism's intended function, not an incidental defect. Theater is low (0.15): the for-cause review process is functionally real, not primarily ceremonial, under this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the faculty seat, tenure is genuine coordination protecting a public good (unconstrained inquiry). From the political-actor seat, the identical due-process requirement is experienced as extraction — a structural block on their ability to act on discovered institutional or political preferences. The engine should compute these divergently from the same structural facts; this reading does not resolve that divergence, it names one side of it as the constraint's coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty sit near the beneficiary end: the constraint subsidizes their capacity to pursue inquiry without survival risk. Political actors seeking institutional control sit near the target end: the same due-process machinery that protects faculty is experienced by this seat as an expensive, hard-to-route-around obstacle to removing a researcher whose findings are unwelcome — exactly the intended effect of the constraint under this reading. Students and the public are near-symmetric diffuse beneficiaries: real but indirect gains from higher-quality, less self-censored scholarship, with no direct cost borne.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (protecting inquiry from retaliatory dismissal) as contested-but-plausibly-still-live: contemporary legislative pressure on faculty over research findings suggests the mechanism still has active work to do, which cuts against a pure mandatrophy verdict for THIS reading specifically. Whether the same contractual form has ALSO drifted into pure incumbent-protection for a different population is the separate question the institutional_extraction_reading addresses — conflating the two readings would misclassify either a live coordination function as pure extraction, or an atrophied protection as live coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_kernel_reading_disagreement,
    'Does the tenure contract''s operative function today match the academic-freedom coordination story, the institutional-extraction story, or the demographic-reproduction story — or some empirically-varying mixture across institutions and fields?',
    'Comparative case study of dismissal-for-cause proceedings and their triggers across institutions and time periods: proceedings triggered predominantly by politically or institutionally unpopular findings support this reading; proceedings rare and tenure denial patterns tracking demographic ''fit'' support the demographic_reproduction_reading; contingent-labor cost-shifting patterns support the institutional_extraction_reading.',
    'If the academic-freedom function has substantially atrophied at most institutions relative to the other functions, this reading''s low ε may not reflect the contract''s dominant contemporary operation — though it would remain a valid measurement of the specific coordination function this story isolates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_kernel_reading_disagreement, conceptual, 'Which reading of the tenure kernel best describes contemporary dominant function is unresolved and likely institution-dependent.').

omega_variable(
    faculty_self_censorship_baseline,
    'In the absence of tenure, how much would faculty research agendas actually shift away from controversial topics — is the counterfactual self-censorship large or modest?',
    'Natural experiment: compare research topic selection and public statement patterns between tenured and non-tenured/contingent faculty in the same departments and fields, controlling for seniority and field.',
    'A large observed gap would strongly corroborate the coordination-function claim central to this reading; a small gap would suggest the academic-freedom justification is largely rhetorical even where the protection is genuinely used for other purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faculty_self_censorship_baseline, empirical, 'Whether tenure''s academic-freedom effect is empirically large or largely nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__academic_freedom_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__academic_freedom_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__academic_freedom_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__academic_freedom_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__academic_freedom_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__academic_freedom_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__academic_freedom_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__academic_freedom_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__academic_freedom_reading, suppression_requirement, 32, 0.33).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.1).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint, tenure_contract__institutional_extraction_reading, and tenure_contract__demographic_reproduction_reading are three readings of a single contested kernel (tenure_contract). Each reading identifies a distinct coordination/extraction structure, distinct beneficiaries and victims, and a distinct ε from the same underlying contractual form. They are linked here rather than merged because merging would violate the ε-invariance principle: measuring the tenure contract via the academic-freedom observable yields a low, coordination-dominant ε, while measuring it via the contingent-labor-cost observable yields a substantially higher, extraction-dominant ε. These are structurally different claims wearing one colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
