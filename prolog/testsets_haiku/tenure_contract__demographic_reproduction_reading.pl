% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: higher_education/labor_economics/institutional_theory
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.81).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.76).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '4d34c8a5-a16d-4c6e-a151-af6b9775823b').
narrative_ontology:cs_kernel_codification('4d34c8a5-a16d-4c6e-a151-af6b9775823b', formalized).
narrative_ontology:cs_authority_grounding('4d34c8a5-a16d-4c6e-a151-af6b9775823b', expertise).
narrative_ontology:cs_interpretation_layer_present('4d34c8a5-a16d-4c6e-a151-af6b9775823b').
narrative_ontology:cs_reading_relation('4d34c8a5-a16d-4c6e-a151-af6b9775823b', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d34c8a5-a16d-4c6e-a151-af6b9775823b', tenure_contract__institutional_extraction_reading, influences).
narrative_ontology:cs_axiom('4d34c8a5-a16d-4c6e-a151-af6b9775823b', foundational, peer_review_operates_meritocratically).
narrative_ontology:cs_axiom_status(peer_review_operates_meritocratically, holdable).
narrative_ontology:cs_axiom_grounding('4d34c8a5-a16d-4c6e-a151-af6b9775823b', peer_review_operates_meritocratically, empirically_contingent).
narrative_ontology:cs_axiom('4d34c8a5-a16d-4c6e-a151-af6b9775823b', foundational, fit_and_collegiality_proxy_for_research_quality).
narrative_ontology:cs_axiom_status(fit_and_collegiality_proxy_for_research_quality, overridden).
narrative_ontology:cs_axiom_grounding('4d34c8a5-a16d-4c6e-a151-af6b9775823b', fit_and_collegiality_proxy_for_research_quality, empirically_contingent).
narrative_ontology:cs_reference_frame('4d34c8a5-a16d-4c6e-a151-af6b9775823b', meritocratic_peer_evaluation).
narrative_ontology:cs_drift_state('4d34c8a5-a16d-4c6e-a151-af6b9775823b', contemporary_equity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d34c8a5-a16d-4c6e-a151-af6b9775823b', '2026-06-13T14:32:00Z').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_scholars).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, contingent_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, institutional_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tenured scholars from majority demographics (historically white, male, and from privileged class backgrounds in many disciplines) control hiring committees and tenure review panels. They set evaluation criteria, interpret 'fit' and 'collegiality' in their own image, and sustain the composition resembling their own cohort. They use peer review mechanisms ostensibly designed for quality control to enforce conformity to departmental culture, methodological orthodoxy, and social reproduction.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, agenda_setter,
    institutional, generational, arbitrage, national).

% Early-career scholars from underrepresented demographic groups (women, scholars of color, scholars from lower-income backgrounds, LGBTQ+ researchers, disabled scholars, international scholars) face heightened scrutiny in tenure review on dimensions coded as 'fit' and 'collegiality' — criteria that carry implicit demographic preferences and are evaluated with higher subjective variance and lower transparency than research productivity measures. Their scholarly voice is read through a demographic filter: the same methodological risk-taking is 'innovative' for dominant-group scholars and 'not rigorous enough' for others. Exit means abandoning the profession they trained for, the fieldwork relationships they built, the research program they invested years developing. Identity fusion with the disciplinary role makes exit costly even where nominally possible.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_scholars, payer,
    moderate, biographical, identity_locked, national).

% Non-tenure-track faculty, adjuncts, teaching-focused instructors, and postdoctoral researchers bear the downstream cost of tenure scarcity: as tenured seats persist and are rarely vacated, institutions substitute contingent precarious labor. They have no gatekeeping power, no long-term job security, no voice in departmental governance, and their labor directly subsidizes the tenure system by enabling departments to maintain tenure ratios while growing teaching loads. Trapped: the only option is leaving academia entirely, and many carry sunk costs in training specific to this field.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_labor, payer,
    powerless, immediate, trapped, national).

% University administration benefits from tenure gatekeeping by gaining stable control over departmental composition without the political cost of explicit demographic engineering. They defer to peer review and 'meritocracy' framing while the gatekeeping machinery reproduces predictable tenure cohorts. They also benefit from contingent labor substitution: contingent workers enable flexibility in enrollment downturns and funding changes while tenured seats remain constant.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, institutional_administration, beneficiary,
    powerful, generational, mobile, national).

% Publication counts, citation impact, grant funding, and other productivity measures are formally present in tenure files but are systematically subordinated in deliberation to subjective 'fit' and 'collegiality' assessments. Productivity metrics are not excluded entirely but are reinterpreted through demographic lenses when they conflict with gatekeeping intuitions.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, research_productivity_metrics, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tenure_contract__demographic_reproduction_reading, research_productivity_metrics).

% Advocates for academic freedom and protection of dissenting voices would argue that if tenure review is primarily demographic gatekeeping unmoored from research merit, then it fails its own justificatory promise and should be reformed or abolished. They are structurally excluded from tenure deliberation because tenure committees are self-perpetuating and structured to protect existing gatekeeping authority.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, academic_freedom_advocates, excluded,
    organized, generational, constrained, national).

% Campus equity offices document the demographic composition of tenure lines over time, conduct implicit bias training, issue recommendations on hiring and retention procedures. Their authority to alter tenure gatekeeping is structurally limited: they can recommend but not decide, can audit but not reform the criteria themselves. They observe the mechanism without possessing structural power to change it.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_and_equity_offices, observer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure review is presented as maintaining research quality and scholarly rigor by assembling committees of subject experts to assess candidates' contributions, fit, and capacity to sustain intellectual inquiry within a collegial community. Ostensibly, peer review allocates permanent positions to scholars whose work merits long-term institutional commitment.
% TRANSFER_FUNCTION: Moves permanent employment, prestige, departmental voice, governance rights, and access to institutional resources from a large candidate pool to a small cohort. Simultaneously channels contingent precarious labor to those filtered out. The constraint transfers demographic reproduction from one generation of faculty to the next: gatekeeping authority is self-perpetuating because the same demographic majority that controls review panels is the same demographic majority that benefits from the review criteria they set.
% ABSENT_VOICES: Early-career scholars from underrepresented demographics who were rejected at earlier career stages; scholars who left academia after non-renewal of contingent positions; international scholars whose credentials were devalued; scholars whose research challenged disciplinary orthodoxy and were read as 'not a good fit'. These populations would attest that tenure review is a demographic filter, not a meritocratic one. Their absence from tenure committees ensures their exclusion from the deliberation itself.
% DISAPPEARANCE_RATIONALE: If tenure gatekeeping and its demographic filtering disappeared overnight, departmental composition would become noticeably more diverse within 10–15 years; contingent labor markets would compress as permanent positions opened; research fields would diversify as methodological gatekeeping relaxed; the cost structure of academic institutions would shift as they could not rely on contingent labor substitution to maintain tenure ratios. The world rearranges because the constraint actively reproduces power.
% FOUNDING_PROBLEM: Academic inquiry requires researchers to pursue questions that may displease powerful interests (governments, corporations, ideological establishments) without fear of retaliation. Tenure was designed to decouple career survival from institutional or political displeasure, enabling high-risk truth-seeking.
% FOUNDING_PROBLEM_CORROBORATION: Academic freedom advocates and the AAUP attest the founding problem is still live and tenure remains the primary defense. Simultaneously, scholars who have been denied tenure and equity researchers attest that tenure review has become primarily a demographic and ideological gatekeeping mechanism, not a truth-seeking protection. Empirical studies of tenure rejection rates and demographic composition show that the founding problem's solution has accumulated substantial gatekeeping function decoupled from the founding problem itself. No corroboration from outside the tenure-benefiting parties supports the unambiguous 'academic freedom protection' reading.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    research_productivity_vs_fit_criteria,
    'Are ''fit'' and ''collegiality'' criteria genuinely proxies for research quality and sustainability, or are they mechanisms for enforcing cultural homogeneity decoupled from research productivity measures?',
    'Controlled analysis of tenure rejection rates correlated with (1) research productivity metrics (publications, citations, grants, field impact) vs. (2) demographic characteristics and departmental diversity footprint. If rejection rates are higher for high-productivity researchers from underrepresented demographics than for lower-productivity researchers from dominant demographics, then ''fit'' criteria are gatekeeping mechanisms, not quality proxies.',
    'If criteria are gatekeeping mechanisms, the constraint is Snare (extraction of demographic authority). If criteria are genuine quality proxies, the constraint might be Rope or Tangled Rope (coordination with asymmetric costs justified by selection criteria). This omega determines whether the demographic reproduction is incidental to meritocratic selection or structural to the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(research_productivity_vs_fit_criteria, empirical, 'Whether ''fit'' and ''collegiality'' measure research quality or enforce demographic conformity.').

omega_variable(
    founding_problem_persistence,
    'Is academic freedom under genuine threat at the institutional level in contemporary academia, or has it been substantially secured through multiple alternative mechanisms (legal precedent, professional association backing, public discourse norms) while tenure persists as gatekeeping?',
    'Comparative study of academic persecution in tenured and non-tenured institutions and jurisdictions; tracking of academic freedom cases before and after tenure reform or abolition; documentation of whether researchers face retaliation for scholarly positions independent of tenure status.',
    'If academic freedom is substantially secured independent of tenure, the founding problem is dead and the constraint has undergone mandatrophy: its justification has evaporated but it persists due to inertia and gatekeeping benefits. Reform or abolition becomes rationally warranted. If academic freedom remains under threat specifically from tenure loss, the founding problem is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem (''tenure protects against retaliation'') is still generatively driving tenure persistence.').

omega_variable(
    identity_lock_vs_constrained_exit,
    'Is the high cost of exit for underrepresented scholars primarily structural (economic: no alternative career paths with equivalent income; geographic: concentrated in elite institutions that also gatekeep) or internalized (psychological: fused identity with the discipline; social: leaving means losing research community relationships and mentorship)?',
    'Qualitative study of exit narratives from scholars denied tenure or who left academia: what barriers do they identify as primary? Follow-up study of scholars who successfully exited: what enabled their transition? Comparison with dominant-group scholars'' exit costs.',
    'If primarily structural, the constraint''s suppression is structural and persists even after explicit gatekeeping disappears. If primarily internalized, the constraint could be disrupted by identity-reframing or community rebuilding outside academia. The suppression mechanism (structural vs. internalized) affects the classification stability and the design of remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether identity-lock in academic careers is structural or internalized, determining suppression stability post-exit.').

omega_variable(
    alternative_gatekeeping_mechanisms,
    'If tenure peer review were reformed or abolished, would demographic gatekeeping disappear, or would it migrate to alternative mechanisms (e.g., postdoctoral appointment gatekeeping, grant funding gatekeeping, journal editorial boards)?',
    'Comparative study of demographic representation in tenured institutions with and without tenure; tracking of demographic gatekeeping in alternative career structures (grant funding, postdoctoral markets, contingent instructor hiring).',
    'If gatekeeping migrates rather than disappears, tenure abolition would dismantle one gatekeeping site while leaving systemic demographic reproduction intact. This affects the design of reform: is tenure the target, or is tenure one manifestation of a broader gatekeeping system? If gatekeeping is systemic, reform of tenure alone is insufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_gatekeeping_mechanisms, conceptual, 'Whether demographic gatekeeping is tenure-specific or a property of the broader academic career structure.').

omega_variable(
    contingent_labor_as_intended_or_consequence,
    'Is the growth of contingent labor a deliberate mechanism to subsidize tenure gatekeeping, or an unintended consequence of tenure rigidity and enrollment volatility?',
    'Historical analysis of administrative decision-making around contingent hiring: what were the stated rationales? Interview study with institution leaders: did they anticipate that maintaining tenure ratios would require contingent substitution? Compare institutions with different tenure/contingent ratios and ask whether institutions actively chose the trade-off.',
    'If deliberate, the constraint includes institutional administration as a conscious beneficiary and the extraction is collective. If unintended consequence, the extraction is diffuse and the constraint might be reformed by architectural change (e.g., mandatory conversion of contingent positions to tenure lines) without confronting deliberate gatekeeping. This affects strategy for change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingent_labor_as_intended_or_consequence, empirical, 'Whether contingent labor growth is structural consequence or deliberate mechanism of tenure gatekeeping.').

omega_variable(
    sibling_reading_contention,
    'Are the three readings of the tenure_contract kernel (academic_freedom, demographic_reproduction, institutional_extraction) held by different parties in genuine dispute, or do different parties hold overlapping readings sequentially (e.g., someone holds academic_freedom early in their career and shifts to institutional_extraction after becoming tenured)?',
    'Qualitative study of tenure discourse: who advocates for each reading? Are advocacy coalitions stable across time or do individuals shift readings? Do the readings appear in the same texts (same author holding multiple readings) or different institutional sites (different constituencies advocating different readings)?',
    'If parties genuinely coexist with different readings (coexists_with relation), the kernel is fundamentally contested and reform requires managing multiple legitimate framings. If individuals shift readings with tenure status, the readings are developmental rather than fundamentally contested and reform might focus on disrupting the individual trajectory (e.g., postdoctoral restructuring, alternative career paths).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_contention, conceptual, 'Whether the three tenure readings are genuinely coexisting readings or developmental stages individuals pass through.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(tenu_tr_t0, observed).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__demographic_reproduction_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement_basis(tenu_tr_t5, observed).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(tenu_tr_t10, observed).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__demographic_reproduction_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement_basis(tenu_tr_t15, observed).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(tenu_tr_t20, observed).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__demographic_reproduction_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(tenu_tr_t25, observed).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(tenu_tr_t30, observed).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(tenu_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(tenu_be_t0, observed).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__demographic_reproduction_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(tenu_be_t5, observed).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(tenu_be_t10, observed).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__demographic_reproduction_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(tenu_be_t15, observed).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(tenu_be_t20, observed).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__demographic_reproduction_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement_basis(tenu_be_t25, observed).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(tenu_be_t30, observed).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(tenu_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(tenu_su_t0, observed).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__demographic_reproduction_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(tenu_su_t5, observed).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(tenu_su_t10, observed).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__demographic_reproduction_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(tenu_su_t15, observed).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(tenu_su_t20, observed).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__demographic_reproduction_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(tenu_su_t25, observed).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(tenu_su_t30, observed).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(tenu_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(tenu_grid_01, tenure_contract__demographic_reproduction_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(tenu_grid_02, tenure_contract__demographic_reproduction_reading, accessibility_collapse(class), 40, 0.62).
narrative_ontology:measurement(tenu_grid_03, tenure_contract__demographic_reproduction_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(tenu_grid_04, tenure_contract__demographic_reproduction_reading, accessibility_collapse(individual), 40, 0.74).
narrative_ontology:measurement(tenu_grid_05, tenure_contract__demographic_reproduction_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(tenu_grid_06, tenure_contract__demographic_reproduction_reading, accessibility_collapse(organizational), 40, 0.67).
narrative_ontology:measurement(tenu_grid_07, tenure_contract__demographic_reproduction_reading, accessibility_collapse(structural), 0, 0.71).
narrative_ontology:measurement(tenu_grid_08, tenure_contract__demographic_reproduction_reading, accessibility_collapse(structural), 40, 0.73).
narrative_ontology:measurement(tenu_grid_09, tenure_contract__demographic_reproduction_reading, resistance(class), 0, 0.62).
narrative_ontology:measurement(tenu_grid_10, tenure_contract__demographic_reproduction_reading, resistance(class), 40, 0.66).
narrative_ontology:measurement(tenu_grid_11, tenure_contract__demographic_reproduction_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(tenu_grid_12, tenure_contract__demographic_reproduction_reading, resistance(individual), 40, 0.64).
narrative_ontology:measurement(tenu_grid_13, tenure_contract__demographic_reproduction_reading, resistance(organizational), 0, 0.48).
narrative_ontology:measurement(tenu_grid_14, tenure_contract__demographic_reproduction_reading, resistance(organizational), 40, 0.52).
narrative_ontology:measurement(tenu_grid_15, tenure_contract__demographic_reproduction_reading, resistance(structural), 0, 0.44).
narrative_ontology:measurement(tenu_grid_16, tenure_contract__demographic_reproduction_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(tenu_grid_17, tenure_contract__demographic_reproduction_reading, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement(tenu_grid_18, tenure_contract__demographic_reproduction_reading, stakes_inflation(class), 40, 0.76).
narrative_ontology:measurement(tenu_grid_19, tenure_contract__demographic_reproduction_reading, stakes_inflation(individual), 0, 0.78).
narrative_ontology:measurement(tenu_grid_20, tenure_contract__demographic_reproduction_reading, stakes_inflation(individual), 40, 0.81).
narrative_ontology:measurement(tenu_grid_21, tenure_contract__demographic_reproduction_reading, stakes_inflation(organizational), 0, 0.64).
narrative_ontology:measurement(tenu_grid_22, tenure_contract__demographic_reproduction_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(tenu_grid_23, tenure_contract__demographic_reproduction_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(tenu_grid_24, tenure_contract__demographic_reproduction_reading, stakes_inflation(structural), 40, 0.71).
narrative_ontology:measurement(tenu_grid_25, tenure_contract__demographic_reproduction_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(tenu_grid_26, tenure_contract__demographic_reproduction_reading, suppression(class), 40, 0.71).
narrative_ontology:measurement(tenu_grid_27, tenure_contract__demographic_reproduction_reading, suppression(individual), 0, 0.74).
narrative_ontology:measurement(tenu_grid_28, tenure_contract__demographic_reproduction_reading, suppression(individual), 40, 0.78).
narrative_ontology:measurement(tenu_grid_29, tenure_contract__demographic_reproduction_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(tenu_grid_30, tenure_contract__demographic_reproduction_reading, suppression(organizational), 40, 0.64).
narrative_ontology:measurement(tenu_grid_31, tenure_contract__demographic_reproduction_reading, suppression(structural), 0, 0.64).
narrative_ontology:measurement(tenu_grid_32, tenure_contract__demographic_reproduction_reading, suppression(structural), 40, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, contingent_labor_precarity).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_research_methodology_orthodoxy).

% DUAL FORMULATION NOTE:
% tenure_contract is a contested kernel with three active readings: academic_freedom_reading (tenure protects inquiry from retaliation), demographic_reproduction_reading (tenure peer review gates entry by demographic conformity), and institutional_extraction_reading (tenure creates rent-extraction and employment rigidity). Each reading is instantiated as a separate constraint with its own ε, beneficiary/victim structure, and classification. This file is the demographic_reproduction_reading. All three readings share the formal tenure contract and peer review practice but differ structurally in the identified primary extractive/coordinative mechanism. Links via network.affects_constraints enable the analysis to track how the three readings create downstream constraints (contingent labor precarity is affected by whichever reading dominates institutional practice; methodological gatekeeping is affected by how 'fit' is defined in each reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
