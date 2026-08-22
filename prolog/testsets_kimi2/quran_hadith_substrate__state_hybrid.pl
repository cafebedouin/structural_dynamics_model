% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Selective Sharia Hybrid (Political Sovereignty Reading)
 *   domain: legal_theory/religious_authority
 *
 * SUMMARY:
 *   This constraint story models the state_hybrid reading of the
 *   quran_hadith_substrate kernel: the claim that modern Muslim states
 *   possess sovereignty to selectively adopt classical fiqh rulings in family
 *   and criminal law while applying reformist or secular frameworks in
 *   commercial and administrative law, grounding legitimacy in political
 *   sovereignty rather than doctrinal fidelity. The state captures the
 *   coordination benefit of religious legitimation and modern governability,
 *   while asymmetrically extracting from traditionalists (whose comprehensive
 *   authority is truncated) and reformists (whose critical readings are
 *   suppressed where they threaten the selective arrangement).
 *
 * KEY AGENTS:
 *   - state_elites: Primary agenda-setter and beneficiary (institutional/arbitrage) â administer selective adoption and capture legitimacy.
 *   - traditionalist_scholars: Primary payer (organized/constrained) â bear the cost of truncated classical jurisdiction.
 *   - reformist_critics: Secondary payer (moderate/constrained) â bear the cost of suppression in family and criminal domains.
 *   - comparative_legal_scholars: Analytical observer â track the divergence between codified law and classical sources.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.55).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Selective Sharia Hybrid (Political Sovereignty Reading)").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '4b3f7a99-1ef0-46d9-b964-000630f59808').
narrative_ontology:cs_kernel_codification('4b3f7a99-1ef0-46d9-b964-000630f59808', fixed_text).
narrative_ontology:cs_authority_grounding('4b3f7a99-1ef0-46d9-b964-000630f59808', extraction).
narrative_ontology:cs_interpretation_layer_present('4b3f7a99-1ef0-46d9-b964-000630f59808').
narrative_ontology:cs_reading_relation('4b3f7a99-1ef0-46d9-b964-000630f59808', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('4b3f7a99-1ef0-46d9-b964-000630f59808', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('4b3f7a99-1ef0-46d9-b964-000630f59808', foundational, state_sovereignty_selects_rulings).
narrative_ontology:cs_axiom_status(state_sovereignty_selects_rulings, holdable).
narrative_ontology:cs_axiom_grounding('4b3f7a99-1ef0-46d9-b964-000630f59808', state_sovereignty_selects_rulings, conventional).
narrative_ontology:cs_axiom('4b3f7a99-1ef0-46d9-b964-000630f59808', foundational, domain_differentiated_legal_validity).
narrative_ontology:cs_axiom_status(domain_differentiated_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('4b3f7a99-1ef0-46d9-b964-000630f59808', domain_differentiated_legal_validity, conventional).
narrative_ontology:cs_reference_frame('4b3f7a99-1ef0-46d9-b964-000630f59808', state_sovereignty_over_sharia_substrate).
narrative_ontology:cs_drift_state('4b3f7a99-1ef0-46d9-b964-000630f59808', contemporary_globalized_legal_order, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4b3f7a99-1ef0-46d9-b964-000630f59808', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_critics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise political sovereignty to select which classical fiqh rulings are codified into state family and criminal law, while delegating commercial and administrative law to secular or reformist frameworks. Instrumentalize sharia as a source of legitimacy without submitting state power to comprehensive classical authority. Control judicial appointments, religious institutional budgets, and the text of codified family law.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Uphold comprehensive classical madhhab authority but find their jurisdiction truncated to family law and selected criminal provisions. State codification overrides their interpretive autonomy; they are consulted for legitimation but not for binding legal determination. Exit means abandoning institutional religious roles or accepting subordination to state-selected classical rulings.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    organized, generational, constrained, national).

% Advocate contextual ijtihad and human-rights-aligned readings across all legal domains, including family and criminal law. Face institutional exclusion, censorship, or legal charges when their critiques threaten regime stability or the state's selective classical legitimization. Their reformist frameworks are partially co-opted in commercial law but barred from domains where classical rulings serve state legitimacy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_critics, payer,
    moderate, biographical, constrained, national).

% Study the divergence between state codification and classical fiqh, documenting how political sovereignty filters religious legal sources. Neither collect from nor pay into the constraint; their analysis tracks the structural gap between claimed doctrinal fidelity and actual selective adoption.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates governance in Muslim-majority states by preserving religious legitimacy through classical family and criminal law while enabling modern administrative and commercial legal frameworks necessary for state development and global economic integration.
% TRANSFER_FUNCTION: Moves legitimacy capital from classical Islamic legal texts and scholarly authority to the state apparatus; moves policy autonomy from the religious legal domain into state administrative discretion. Transfers the cost of truncated religious vision onto traditionalist scholars, and transfers the cost of suppressed critical reform onto reformist critics and populations governed by classical family law provisions.
% ABSENT_VOICES: Comprehensive traditionalist jurists demanding state submission to full madhhab authority across all domains, and systemic reformists demanding ijtihad in family and criminal law, are both structurally marginalized: the former because the state claims sovereignty over selection, the latter because the state bars reform where classical rulings serve legitimacy needs.
% DISAPPEARANCE_RATIONALE: Without the selective hybrid, the state would face a legitimation crisis: either traditionalist movements would demand comprehensive classical restoration, reformists would push for uniform secular or progressive codification, or the state would have to construct an entirely new legitimating narrative independent of sharia. The current legal dualism would collapse into one of these poles.
% FOUNDING_PROBLEM: Post-colonial state-building in Muslim societies required a legal system that could claim Islamic authenticity for popular legitimacy while providing the flexible modern governance (commercial contracts, administrative regulation, international treaty compliance) that comprehensive classical fiqh did not accommodate.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional historians and post-colonial legal theorists outside both the state beneficiary class and the traditionalist establishment attest the founding crisis of legal pluralism and legitimacy. Traditionalist scholars dispute that the hybrid solves it, arguing instead that it perpetuates colonial distortion; reformist critics argue the problem has mutated into authoritarian legitimation.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects moderate but structurally embedded extraction: the state gains legitimacy and policy flexibility that it does not reciprocate to the classical sources it selectively quotes. Suppression (0.55) is moderate-to-high because the arrangement depends on actively marginalizing both traditionalist claims to comprehensive authority and reformist claims to universal ijtihad. Theater_ratio (0.42) captures the performative dimension: states often present family law codification as faithful sharia implementation while the selection process is thoroughly instrumental. Accessibility_collapse (0.45) indicates that alternatives (full secularism, full classical restoration, comprehensive reform) are visible but politically blocked. Resistance (0.50) reflects persistent opposition from both flanks.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state elites) experiences the constraint as a necessary and legitimate legal architecture; the payer seats (traditionalists and reformists) experience it as an imposed truncation of their respective visions. The engine will compute this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites are structural beneficiaries (d near 0.0): they collect legitimacy and discretion. Traditionalist scholars and reformist critics are structural targets (d near 1.0): they bear the costs of exclusion and suppression. The comparative legal scholar seat is analytical (d neutral). Effective extraction is amplified for the scholars and critics by their constrained exit and national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as Tangled Rope rather than Rope because there are identifiable victims (traditionalists, reformists) and active enforcement is required to maintain the selectivity against both groups. It is not a Snare because a genuine coordination function exists: the hybrid does solve the post-colonial legitimation-development bind for a multi-religious and multi-ethnic state. The Mandatrophy analysis prevents mislabeling by requiring both coordination function AND asymmetric extraction with active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_variability_extraction,
    'Does the constraint''s extractiveness vary primarily with regime type (authoritarian vs semi-democratic) or with economic development pressure?',
    'Comparative case studies across multiple Muslim-majority states tracking legal codification reforms against regime typology indices and GDP-per-capita/legal-complexity metrics.',
    'If extraction tracks regime type, the constraint is an authoritarian instrument; if it tracks development pressure, it is a functional adaptation mechanism with different classification implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_variability_extraction, empirical, 'Variability of extraction across state contexts').

omega_variable(
    kernel_reading_underdetermination,
    'Would the same state legal arrangement be classified as a Snare if viewed through a reformist_ijtihad lens, or as illegitimate innovation through a traditionalist_taqlid lens?',
    'Cross-reading classification comparison: compile the same structural facts into the sibling reading frames and observe computed seat types.',
    'If the classification changes radically across readings, the constraint''s identity is reading-dependent and the kernel must be decomposed into separate constraint families.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Reading-dependent classification of the state hybrid arrangement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of reformist critics structural (legal penalties, institutional exclusion) or internalized (self-censorship within religious discourse)?',
    'Survey of reformist jurists on perceived constraints, matched against documented prosecutions and professional sanctions.',
    'If internalized, effective suppression exceeds the structural measure; if purely structural, removal of legal barriers might rapidly shift the constraint type toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of reformist voices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.25).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.3).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__state_hybrid, theater_ratio, 30, 0.35).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.4).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__state_hybrid, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__state_hybrid, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__state_hybrid, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(qura_su_t30, quran_hadith_substrate__state_hybrid, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__state_hybrid, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
