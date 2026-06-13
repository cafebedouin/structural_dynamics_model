% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Institutional Rent Extraction and Employment Rigidity
 *   domain: higher_education/labor_economics/institutional_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the INSTITUTIONAL_EXTRACTION_READING
 *   of the tenure_contract kernel. Tenure operates as a permanent employment
 *   guarantee and compensation floor for a protected ring of faculty,
 *   financed by extracting flexibility and cost-bearing from contingent
 *   workers (adjuncts, postdocs) and students (via tuition and reduced
 *   instructional investment). The reading claims this is not primarily
 *   coordination for academic freedom but rather rent collection: the
 *   founding problem (institutional suppression of research) is substantially
 *   solved in wealthy research universities, yet the permanent employment
 *   claim persists, maintained by gatekeeping peer review and rhetorical
 *   defense. Three readings of this kernel coexist: (1)
 *   academic_freedom_reading holds tenure is essential to truth-seeking
 *   despite its costs; (2) demographic_reproduction_reading holds tenure
 *   gatekeeping reproduces dominant group composition; (3) this reading holds
 *   tenure is primarily extraction from early winners, loading costs onto
 *   later entrants and students. This story generates ONLY the extraction
 *   reading; the other readings are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - tenured_faculty: permanent claim-holders, collective beneficiary, agenda-setters for hiring and promotion
 *   - contingent_faculty: victims bearing employment volatility and low compensation, structurally excluded from governance
 *   - students: cost-bearers via tuition hikes and reduced instructional availability
 *   - department chairs/provosts: agenda-setters administering the constraint while shifting costs onto contingent pools
 *   - university boards: agenda-setters with legal authority but practical constraints limiting reform
 *   - prospective academics: structurally absent from governance, bear the entry-barrier gatekeeping
 *   - academic freedom advocates: analytical observers defending tenure's necessity
 *   - labor economists: analytical observers documenting employment rigidity and inequality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction and Employment Rigidity").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_governance").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, 'ff51d4c5-36fc-47e2-a35e-d240d9440177').
narrative_ontology:cs_kernel_codification('ff51d4c5-36fc-47e2-a35e-d240d9440177', formalized).
narrative_ontology:cs_authority_grounding('ff51d4c5-36fc-47e2-a35e-d240d9440177', extraction).
narrative_ontology:cs_interpretation_layer_present('ff51d4c5-36fc-47e2-a35e-d240d9440177').
narrative_ontology:cs_reading_relation('ff51d4c5-36fc-47e2-a35e-d240d9440177', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff51d4c5-36fc-47e2-a35e-d240d9440177', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('ff51d4c5-36fc-47e2-a35e-d240d9440177', foundational, tenure_as_permanent_rent_claim).
narrative_ontology:cs_axiom_status(tenure_as_permanent_rent_claim, holdable).
narrative_ontology:cs_axiom_grounding('ff51d4c5-36fc-47e2-a35e-d240d9440177', tenure_as_permanent_rent_claim, empirically_contingent).
narrative_ontology:cs_axiom('ff51d4c5-36fc-47e2-a35e-d240d9440177', foundational, extraction_persists_through_rhetorical_naturalization).
narrative_ontology:cs_axiom_status(extraction_persists_through_rhetorical_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('ff51d4c5-36fc-47e2-a35e-d240d9440177', extraction_persists_through_rhetorical_naturalization, deontological).
narrative_ontology:cs_reference_frame('ff51d4c5-36fc-47e2-a35e-d240d9440177', tenure_as_institutional_safeguard).
narrative_ontology:cs_drift_state('ff51d4c5-36fc-47e2-a35e-d240d9440177', contemporary_research_university_context, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ff51d4c5-36fc-47e2-a35e-d240d9440177', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, adjuncts_and_postdocs).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).

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
 *   Extractiveness rises from 0.58 to 0.78 across the 40-year interval, driven by two mechanisms: (1) contingent faculty proportion grows (enrollment rises but tenure-track hiring stagnates), so the gap between permanent and precarious compensation widens; (2) tuition rises faster than general inflation, financing permanent salary commitments while reducing instructional investment. The rise plateaus after year 30 because institutions reach saturation on contingency (80%+ non-tenure-track hires in many departments) and tuition faces political resistance. Theater ratio rises from 0.22 to 0.48, indicating growing decoupling between the coordination narrative ('we need tenure to protect academic freedom') and actual enforcement object ('we need tenure gatekeeping to restrict supply and maintain compensation'). As alternative employment arrangements emerge (fixed-term research contracts, industry hiring), the constraint's necessity claim weakens, and enforcement becomes more explicitly rhetorical. Suppression requirement is high (0.55–0.72) because the constraint persists against empirical challenge and labor-market alternatives only through active governance machinery: peer review screening, gatekeeping narrative maintenance, institutional resistance to detenuring proposals, and exclusion of contingent voices from reform conversations. The measurements are authored on a single shared time grid (every metric at every time point) to avoid OQ-105-style misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   From the tenured faculty seat, tenure is essential coordination protecting intellectual independence and long-term mentoring relationships. From the contingent faculty seat, it is a rigidity mechanism that rationalizes their precarity as a market necessity ('institutions must maintain flexibility') while permanent claims are defended as untouchable. From the student seat, it is an opaque cost loading (tuition covers permanent salaries, not expanded offerings). From the board seat, it is a constraint they administer but cannot change without faculty rebellion. These divergences are structural, not perceptual error — each seat has genuine claims about what the constraint does. The engine computes per-seat classifications from the structural data; the authored claim (tangled_rope) reflects this reading's analysis that genuine coordination occurs but is subordinated to extraction benefits for tenured faculty.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty derive d near the beneficiary end (0.1–0.2): they collect permanent employment security and seniority-indexed compensation, face zero dismissal risk, and have arbitrage-level exit (voluntary departure, which almost never occurs due to golden-handcuff lock-in). Contingent faculty derive d near the target end (0.8–0.95): they bear all flexibility costs, receive 40–60% lower compensation for equivalent work, face dismissal at will, and are trapped in academic labor-market dependence (identity-locked: their PhD training created path dependence that makes exit into non-academic work costly). Students derive mid-range d (0.55–0.65): they benefit marginally from faculty stability for mentoring continuity but bear costs through tuition hikes and reduced instructional availability. The directionality overrides below capture the institutional agents (provosts, boards) whose power level suggests moderate-range d but whose structural relationship to the constraint's beneficiary ring places them partly as enforcers, partly as constrained administrators — they maintain the constraint despite having legal authority to reform it, which suggests their actual d is higher (toward target) than power alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional suppression of inquiry) was real and pressing in the early 20th century and remains live in authoritarian and religiously conservative institutional contexts. In wealthy research universities in democratic societies, the founding problem is substantially solved: firing for research findings is nearly unknown, institutional suppression is limited, and employment is de facto stable. Yet the permanent tenure claim persists unchanged — it is no longer necessary to the function it was designed to protect. This is the mandatrophy signal. The constraint continues as rent collection by tenured faculty and administrative gatekeeping by department chairs, defended through rhetorical conflation ('tenure = academic freedom protection') that obscures the distinction between the founding problem's resolution and the constraint's ongoing extraction. Tangled_rope classification captures this: there is a genuine coordination function (stable employment does enable long-term mentoring and research planning) alongside asymmetric extraction (tenured faculty collect permanent security while contingent faculty and students bear costs). The theater_ratio rising to 0.48 indicates growing performative maintenance: institutional leadership publicly defends tenure's necessity while privately adapting through contingent hiring — the speech act maintains the legitimacy of the constraint while the actual employment system diverges from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_or_dead,
    'Is institutional suppression of academic inquiry still a significant threat in wealthy research universities, or has it been substantially solved?',
    'Empirical history of dismissals and suppressions in US research universities (1980–present); comparative data from jurisdictions with different employment protections; interviews with faculty about institutional pressure they actually face.',
    'If suppression is dead in research universities, tenure persists as rent collection without its justifying function, reclassifying as pure snare. If suppression remains live, the constraint''s coordination function is essential and validates extraction costs as necessary overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, empirical, 'Whether the founding problem (institutional suppression) that justified tenure is still live or has been solved.').

omega_variable(
    alternative_employment_arrangements_viability,
    'Could fixed-term research contracts, annual renewal, or merit-based employment provide equivalent career stability and intellectual autonomy without permanent tenure?',
    'Natural experiments from jurisdictions using non-tenure employment (Australian universities, UK contracts, industry research labs); comparative career outcomes and publication/mentoring quality across employment types.',
    'If alternatives provide equivalent stability and autonomy, tenure is pure extraction; if tenure is uniquely enabling, it is necessary coordination cost. If alternatives exist but are suppressed by tenured faculty gatekeeping, that suppression is part of the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_employment_arrangements_viability, empirical, 'Whether tenure is structurally necessary or whether alternatives could provide equivalent stability without permanent contracts.').

omega_variable(
    extraction_suppression_mechanism_internalized,
    'Is contingent faculty''s acceptance of precarity structurally imposed (no exit alternatives exist due to PhD path-dependence) or internalized (they believe precarity is legitimate apprenticeship)?',
    'Career-trajectory data tracking postdocs and adjuncts who exit academia; interviews with those who leave vs. those who stay; psychological assessment of identity-fusion (''am I a real academic?'' binds career to academia even without job security).',
    'If suppression is purely structural, the constraint''s extraction declines if exit alternatives appear (different career paths). If internalized, the constraint carries the suppression forward even after exit from academia — contingent faculty bear the extraction perpetually, not just while employed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_suppression_mechanism_internalized, empirical, 'Whether contingent faculty''s acceptance of precarity is structurally imposed or psychologically internalized through identity-fusion with academic identity.').

omega_variable(
    reading_foreclosure_test,
    'Does this institutional_extraction_reading logically foreclose the academic_freedom_reading, or do they coexist in different institutional contexts?',
    'Institutional analysis: can an institution simultaneously hold that (a) tenure is essential to protect academic freedom AND (b) tenure persists primarily as rent collection? If yes, they coexist. If institutions must choose, one reading forecloses the other.',
    'If readings coexist, they are different constraints. If this reading forecloses the freedom reading, many institutions claiming to support academic freedom are self-deceiving.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the extraction reading and the academic-freedom reading are logically compatible or logically exclusive.').

omega_variable(
    demographic_gatekeeping_decoupling,
    'Is tenure gatekeeping''s demographic reproduction effect an incidental outcome of peer review, or a structurally essential mechanism for the extraction reading?',
    'If demographic reproduction could be addressed (diverse peer review, structured hiring criteria) without changing tenure permanence, then gatekeeping and extraction are separable mechanisms. If attempts to diversify peer review face organized resistance from tenured faculty, that resistance is part of the extraction mechanism.',
    'If separable, the extraction reading and demographic_reproduction_reading are independent constraints. If inseparable, gatekeeping is part of how the extraction mechanism sustains itself — demographic reproduction is a side-effect that reinforces the beneficiary in-group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_gatekeeping_decoupling, empirical, 'Whether demographic gatekeeping is incidental to peer review or structurally essential to the extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__institutional_extraction_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__institutional_extraction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__institutional_extraction_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__institutional_extraction_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__institutional_extraction_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(tenu_tr_t35, tenure_contract__institutional_extraction_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__institutional_extraction_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__institutional_extraction_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__institutional_extraction_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__institutional_extraction_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__institutional_extraction_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(tenu_be_t35, tenure_contract__institutional_extraction_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__institutional_extraction_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__institutional_extraction_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__institutional_extraction_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__institutional_extraction_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__institutional_extraction_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(tenu_su_t35, tenure_contract__institutional_extraction_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.18).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, contingent_faculty_precarity_system).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, higher_education_cost_externalization).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel admits three distinct readings: (1) academic_freedom_reading frames tenure as necessary protection against institutional suppression of inquiry; (2) demographic_reproduction_reading frames tenure peer-review gatekeeping as a mechanism reproducing dominant demographics; (3) institutional_extraction_reading (this constraint) frames tenure as permanent rent collection by early winners, financed by extraction from contingent labor and students. Each reading instantiates a different constraint with different ε profiles, beneficiary/victim structures, and type classifications. The readings coexist in real governance disputes — different institutional actors hold different readings. This story (extraction_reading) is linked to its siblings via network edges; together they form a constraint family covering the multiple structural interpretations of the same kernel text ('tenure protects academic faculty employment').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
