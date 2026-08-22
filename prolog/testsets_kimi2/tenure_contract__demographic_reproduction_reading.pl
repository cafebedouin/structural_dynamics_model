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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the demographic_reproduction_reading of the
 *   tenure_contract kernel. The natural-language institution of 'tenure'
 *   conflates multiple structurally distinct arrangements. In this reading,
 *   tenure peer review does not primarily protect inquiry or allocate
 *   positions by merit; rather, it operates as a demographic gatekeeping
 *   mechanism in which 'collegiality' and 'fit' criteriaâunmoored from
 *   research productivityâreproduce the composition of historically
 *   dominant groups. The claim/metric independence is maintained: the
 *   constraint is claimed as snare (pure extraction under this reading) while
 *   the metrics describe a high-extraction, high-suppression, high-theater
 *   arrangement in which meritocratic justification is performed but not
 *   performed.
 *
 * KEY AGENTS:
 *   - Dominant demographic faculty (beneficiary/powerful/mobile): collect tenure lines and cultural validation through criteria that naturalize their own social capital.
 *   - Underrepresented faculty (payer/moderate/identity_locked): bear extraction through higher denial rates and costly cultural labor; exit is blocked by professional identity fusion.
 *   - Tenure review committees (agenda_setter/institutional/constrained): administer the gatekeeping function through subjective peer judgment, bound by tradition and homophily.
 *   - Equity advocates (excluded/organized/mobile): excluded from closed deliberations; push for transparent alternatives.
 *   - Critical higher-ed researchers (observer/analytical/analytical): provide external empirical corroboration of demographic disparity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.82).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'f73eb451-d940-4b37-8464-e4390ab1a84b').
narrative_ontology:cs_kernel_codification('f73eb451-d940-4b37-8464-e4390ab1a84b', formalized).
narrative_ontology:cs_authority_grounding('f73eb451-d940-4b37-8464-e4390ab1a84b', practice).
narrative_ontology:cs_interpretation_layer_present('f73eb451-d940-4b37-8464-e4390ab1a84b').
narrative_ontology:cs_reading_relation('f73eb451-d940-4b37-8464-e4390ab1a84b', tenure_contract__academic_freedom_reading, forecloses).
narrative_ontology:cs_reading_relation('f73eb451-d940-4b37-8464-e4390ab1a84b', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('f73eb451-d940-4b37-8464-e4390ab1a84b', foundational, peer_review_reproduces_dominant_demographics).
narrative_ontology:cs_axiom_status(peer_review_reproduces_dominant_demographics, holdable).
narrative_ontology:cs_axiom_grounding('f73eb451-d940-4b37-8464-e4390ab1a84b', peer_review_reproduces_dominant_demographics, empirically_contingent).
narrative_ontology:cs_reference_frame('f73eb451-d940-4b37-8464-e4390ab1a84b', neutral_productivity_based_advancement).
narrative_ontology:cs_drift_state('f73eb451-d940-4b37-8464-e4390ab1a84b', contemporary_diversity_critique_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f73eb451-d940-4b37-8464-e4390ab1a84b', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_demographic_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tenured faculty whose cultural backgrounds, interpersonal styles, and social networks align with historical departmental majorities. They benefit from collegiality and fit criteria that naturalize their own cultural capital as professionalism, and they disproportionately occupy review committee seats.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, dominant_demographic_faculty, beneficiary,
    powerful, generational, mobile, national).

% Junior faculty from racial, gender, or class minorities who face higher tenure denial rates despite comparable productivity. They must perform additional cultural labor to demonstrate collegiality, bear the risk of exclusionary judgments, and face high exit costs due to professional identity fusion and sunk training investments.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty, payer,
    moderate, biographical, identity_locked, national).

% Senior faculty panels that translate ambiguous tenure criteria into binding decisions. They exercise discretion through subjective assessments of fit and collegiality, grounded in disciplinary tradition, and are structurally incentivized to preserve departmental culture as they experienced it.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenure_review_committees, agenda_setter,
    institutional, biographical, constrained, local).

% Administrators, scholars, and activists who promote transparent metrics, rubrics, and bias audits in tenure. They are structurally excluded from closed peer-review deliberations and lack formal authority to override committee judgments.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, equity_advocates, excluded,
    organized, generational, mobile, national).

% Researchers who empirically document disparities in tenure outcomes and the divergence between stated meritocratic criteria and actual demographic results. They provide external analytical corroboration without institutional power to change review practices.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, critical_higher_ed_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, dominant_demographic_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates the allocation of scarce permanent faculty positions by evaluating research productivity, teaching, and service; structurally, under this reading, the arrangement solves the problem of maintaining departmental demographic composition and cultural homogeneity through opaque interpersonal criteria.
% TRANSFER_FUNCTION: Moves tenure lines, institutional resources, and long-term job security from underrepresented scholars to demographically dominant faculty by converting cultural similarity into collegiality and redefining merit to include subjective fit.
% ABSENT_VOICES: Underrepresented faculty filtered out before tenure review; quantitative-evaluation advocates who would replace collegiality with transparent productivity metrics; contingent labor organizers who would challenge the bottleneck that makes tenure lines so scarce.
% DISAPPEARANCE_RATIONALE: If tenure peer review and its collegiality criteria vanished overnight, the demographic composition of tenured ranks would shift as differently socialized scholars advanced; departmental culture would reorganize around transparent metrics rather than homophily, and the current distribution of permanent positions would destabilize.
% FOUNDING_PROBLEM: To protect scholarly inquiry from political and commercial interference by insulating researchers from short-term performance pressures, and to ensure that long-term intellectual investments could be assessed by peers rather than administrators.
% FOUNDING_PROBLEM_CORROBORATION: Critical higher education researchers and quantitative sociologists attest that tenure outcomes correlate more strongly with demographic characteristics than with research productivity under current collegiality regimes; tenured faculty beneficiaries and university administrations typically claim the founding problem remains live. Independent corroboration comes from outside the benefiting parties, including critical race theorists in education and labor economists studying academic stratification.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint transfers scarce permanent positions and departmental power to dominant demographic groups through criteria decoupled from productivity. Suppression is high (0.78) because the arrangement actively excludes alternative evaluation methods (blind review, quantitative rubrics, external audits) that would reveal the demographic correlation. Theater ratio is high (0.70) because the peer-review process performs meritocratic diligenceâfiles, letters, meetingsâwhile the actual sorting operates through opaque interpersonal affinity. Accessibility collapse is substantial (0.75): once a scholar understands that collegiality is a demographic proxy, alternatives (applying elsewhere, changing behavior) remain structurally unavailable due to market scarcity and identity lock. Resistance is moderate (0.55): growing critique from excluded seats and quantitative researchers, but met with institutional defense of academic autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (dominant faculty) experiences the constraint as neutral professional judgment and necessary cultural stewardship; the payer seat (underrepresented faculty) experiences it as arbitrary exclusion dressed in meritocratic language. The engine computes this divergence from identical structural data: low directionality for beneficiaries, high directionality for identity-locked targets. The agenda-setter seat (committees) experiences constrained enforcement of tradition; the observer seat sees a pattern of statistical disparity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality: dominant_demographic_faculty is named in beneficiaries, yielding a low d (subsidized by the constraint). Underrepresented_faculty is named in victims, yielding a high d (targeted for extraction). The committee is an agenda_setter with constrained exit, placing its d near symmetric but slightly toward enforcement. The identity_locked exit of underrepresented faculty amplifies their effective extraction relative to mobile beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the coordination story (academic freedom, quality assurance) from the extraction mechanism (demographic closure). If the founding problem (protecting inquiry) is dead while the arrangement persists and rearranges the world upon disappearance, the mandatrophy signature is satisfied: the constraint has outlived its justification. The high theater ratio indicates that performative maintenance of meritocratic ritual now exceeds genuine functional alignment with research productivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collegiality_productivity_correlation,
    'Do subjective collegiality and fit scores in tenure review correlate with subsequent research productivity, or with demographic characteristics of candidates?',
    'Large-N statistical analysis of tenure cases controlling for publication metrics, citation counts, demographics, and discipline, combined with qualitative audit of committee deliberations.',
    'If collegiality is uncorrelated with productivity but correlated with demographics, the gatekeeping reading is strongly validated; if correlated with productivity, the academic freedom reading retains legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collegiality_productivity_correlation, empirical, 'Whether collegiality criteria track merit or demographics.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by underrepresented faculty structural (scarce alternative academic jobs, high mobility costs) or internalized (belief that lack of fit reflects personal deficiency rather than systemic gatekeeping)?',
    'Post-exit trajectory studies: if suppressed career choices and self-limiting behavior persist after leaving academia, the suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because targets carry the suppression with them after exit, deepening the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_separability,
    'Is the demographic gatekeeping function structurally separable from the tenure kernel, or inherent to peer-review practice?',
    'Comparative analysis across national systems or fields with varying tenure densities and evaluation formalization.',
    'If separable, reforms like blind review or standardized rubrics could recover a coordination function; if inseparable, the tenure kernel itself is compromised and the demographic reproduction reading implies abolition or radical restructuring is necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_separability, conceptual, 'Whether extraction is inherent to tenure peer review or a property of its current interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_demographic_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tenure_demographic_tr_t9, tenure_contract__demographic_reproduction_reading, theater_ratio, 9, 0.42).
narrative_ontology:measurement(tenure_demographic_tr_t18, tenure_contract__demographic_reproduction_reading, theater_ratio, 18, 0.5).
narrative_ontology:measurement(tenure_demographic_tr_t27, tenure_contract__demographic_reproduction_reading, theater_ratio, 27, 0.58).
narrative_ontology:measurement(tenure_demographic_tr_t36, tenure_contract__demographic_reproduction_reading, theater_ratio, 36, 0.65).
narrative_ontology:measurement(tenure_demographic_tr_t45, tenure_contract__demographic_reproduction_reading, theater_ratio, 45, 0.7).

% Extraction over time
narrative_ontology:measurement(tenure_demographic_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(tenure_demographic_be_t9, tenure_contract__demographic_reproduction_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(tenure_demographic_be_t18, tenure_contract__demographic_reproduction_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(tenure_demographic_be_t27, tenure_contract__demographic_reproduction_reading, base_extractiveness, 27, 0.75).
narrative_ontology:measurement(tenure_demographic_be_t36, tenure_contract__demographic_reproduction_reading, base_extractiveness, 36, 0.79).
narrative_ontology:measurement(tenure_demographic_be_t45, tenure_contract__demographic_reproduction_reading, base_extractiveness, 45, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tenure_demographic_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tenure_demographic_su_t9, tenure_contract__demographic_reproduction_reading, suppression_requirement, 9, 0.52).
narrative_ontology:measurement(tenure_demographic_su_t18, tenure_contract__demographic_reproduction_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(tenure_demographic_su_t27, tenure_contract__demographic_reproduction_reading, suppression_requirement, 27, 0.68).
narrative_ontology:measurement(tenure_demographic_su_t36, tenure_contract__demographic_reproduction_reading, suppression_requirement, 36, 0.73).
narrative_ontology:measurement(tenure_demographic_su_t45, tenure_contract__demographic_reproduction_reading, suppression_requirement, 45, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into multiple constraints because the colloquial label 'tenure' conflates a protection-of-inquiry mechanism, a demographic-reproduction mechanism, and a rent-extraction mechanism. Each reading carries a different epsilon, beneficiary/victim structure, and classification. Natural-law immunity does not apply: tenure is a constructed institutional arrangement, not a physical law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
