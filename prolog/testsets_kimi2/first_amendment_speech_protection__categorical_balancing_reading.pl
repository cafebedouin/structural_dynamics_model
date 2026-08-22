% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing Doctrine
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The First Amendment's text ('Congress shall make no law... abridging the
 *   freedom of speech') is read through a doctrinal framework in which the
 *   Supreme Court creates protected and unprotected categories (obscenity,
 *   incitement, true threats, fighting words) and applies ad hoc balancing
 *   within the protected sphere. This reading treats the judiciary as the
 *   legitimate engine of speech jurisprudence. The beneficiary is the
 *   institutional judiciary, which accumulates interpretive control and
 *   docket authority. The victims are minority speakersâwho find themselves
 *   nominally protected but practically vulnerable to shifting category
 *   boundariesâand low-resource litigants, who cannot afford the
 *   case-by-case litigation the framework demands. The constraint is claimed
 *   as tangled_rope because it carries a genuine coordination function
 *   (preventing arbitrary censorship, structuring a vast legal domain)
 *   alongside asymmetric extraction (judicial power consolidation,
 *   unpredictability costs imposed on speakers).
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter and beneficiary (institutional/generational/constrained) â accumulates interpretive control
 *   - minority_speakers: primary payer (powerless/biographical/constrained) â bears unpredictability and chilling costs
 *   - low_resource_litigants: secondary payer (powerless/biographical/trapped) â blocked from effective exit by litigation costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.55).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing Doctrine").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'c9ed06f1-2460-4073-96b9-d209b0766afd').
narrative_ontology:cs_kernel_codification('c9ed06f1-2460-4073-96b9-d209b0766afd', fixed_text).
narrative_ontology:cs_authority_grounding('c9ed06f1-2460-4073-96b9-d209b0766afd', lineage).
narrative_ontology:cs_interpretation_layer_present('c9ed06f1-2460-4073-96b9-d209b0766afd').
narrative_ontology:cs_reading_relation('c9ed06f1-2460-4073-96b9-d209b0766afd', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('c9ed06f1-2460-4073-96b9-d209b0766afd', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('c9ed06f1-2460-4073-96b9-d209b0766afd', foundational, judicial_category_creation_authority).
narrative_ontology:cs_axiom_status(judicial_category_creation_authority, holdable).
narrative_ontology:cs_axiom_grounding('c9ed06f1-2460-4073-96b9-d209b0766afd', judicial_category_creation_authority, conventional).
narrative_ontology:cs_axiom('c9ed06f1-2460-4073-96b9-d209b0766afd', foundational, commensurable_speech_harm_balancing).
narrative_ontology:cs_axiom_status(commensurable_speech_harm_balancing, holdable).
narrative_ontology:cs_axiom_grounding('c9ed06f1-2460-4073-96b9-d209b0766afd', commensurable_speech_harm_balancing, instrumental).
narrative_ontology:cs_reference_frame('c9ed06f1-2460-4073-96b9-d209b0766afd', judicially_moderated_speech_order).
narrative_ontology:cs_drift_state('c9ed06f1-2460-4073-96b9-d209b0766afd', contemporary_polarized_speech_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c9ed06f1-2460-4073-96b9-d209b0766afd', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, low_resource_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and refines speech-protective categories through case-by-case adjudication, claiming authority to define constitutional limits on content regulation. Retains docket control, institutional prestige, and interpretive supremacy over democratic branches through the balancing framework.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary, beneficiary).

% Occupy doctrinal categories labeled 'protected' yet face uncertain application of time-place-manner rules, hostile-audience doctrines, and discretionary injunctions. Bear the cost of litigation to prove protection and suffer chilling effects when category boundaries shift unexpectedly.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers, payer,
    powerless, biographical, constrained, national).

% Cannot afford the repeated litigation required to test speech categories or to appeal adverse balancing outcomes. Their exit from the constraint is blocked by poverty and the absence of effective legal representation in constitutional speech cases.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, low_resource_litigants, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves conflicts between expressive liberty and public harm by sorting speech into constitutionally protected and unprotected categories, providing a decision procedure for courts and guidance for legislatures.
% TRANSFER_FUNCTION: Moves interpretive authority over the boundaries of permissible speech from elected legislatures to the federal judiciary; moves litigation costs, chilling-effect burdens, and category-uncertainty risks to speakers, especially those in politically marginal or culturally specific communities.
% ABSENT_VOICES: State legislators who would prefer bright-line regulatory rules are sidelined by judicial supremacy; speakers whose expression does not map cleanly onto existing categories (emergent digital formats, subcultural codes) are absent until they become defendants; absolutist constitutional scholars are heard in dissent but rarely in majority opinions.
% DISAPPEARANCE_RATIONALE: Without the categorical balancing framework, legislatures would regain direct authority to define speech limits, prior judicial precedents would lose their organizing force, and the federal judiciary would surrender its central role in American cultural and political conflict. Speech regulation would fragment across jurisdictions and administrative regimes.
% FOUNDING_PROBLEM: How to maintain a meaningful guarantee of free expression against legislative majorities without disabling all government capacity to regulate speech that produces tangible harms such as incitement, obscenity, and true threats.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the judiciary attest to the historical problem of majoritarian censorship. However, critical race theorists and democratic experimentalists outside the judiciary contest that the current categorical framework solves this problem, arguing it substitutes judicial majoritarianism for legislative majoritarianism; no neutral empirical corroboration establishes that the current doctrine outperforms alternative frameworks at protecting dissent.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial cost imposed on speakers by category uncertainty and litigation burden, offset slightly by the genuine protective function. Suppression (0.55) captures the degree to which alternative regulatory frameworks (legislative bright-line rules, local administrative discretion) are foreclosed by judicial supremacy. Theater_ratio (0.40) registers the ritualized nature of modern balancing tests, which often track ideological priors rather than neutral weighing. Accessibility_collapse (0.45) is moderate: alternatives like absolutism or harm-based administrative regimes are theoretically available but practically excluded by precedent. Resistance (0.60) is elevated because the framework faces sustained critique from textualists, democratic experimentalists, and critical scholars. Temporal measurements show extraction and theater rising through the twentieth century as the doctrine expanded and ritualized, with a slight recent plateau as the framework encounters platform-era speech that resists categorical sorting.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences this constraint as a necessary coordination mechanism that preserves constitutional order and protects minorities from legislative overreach. Minority speakers and low-resource litigants experience it as an unpredictable terrain in which protection is announced in the abstract but withheld in specific applications, requiring costly litigation to vindicate. The engine computes this divergence from the structural data: the judiciary sits at institutional power with constrained exit and beneficiary status, while speakers sit at powerless status with constrained or trapped exit and victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is declared a beneficiary and agenda_setter, producing a low directionality value (subsidy side). Minority speakers and low-resource litigants are declared victims (payers), producing high directionality values (target side). The effective extraction is thus amplified for speakers and damped for the judiciary. Scope is national, so verification is moderately difficult but not intractable; the scope modifier amplifies extraction modestly for the most trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the coordination function (providing a decision procedure for speech disputes, preventing arbitrary censorship) from the extraction function (consolidating judicial control and imposing litigation costs). A pure snare reading would ignore the genuine protective history of the doctrine (e.g., New York Times v. Sullivan, Brandenburg). A pure rope reading would ignore the asymmetric distribution of costs and the concentration of interpretive power. By declaring both beneficiaries and victims and requiring active enforcement, the tangled_rope classification captures the hybrid nature of the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_absolutist_foreclosure,
    'Does the categorical balancing reading''s core premise (judicial case-by-case weighing) logically foreclose the absolutist reading (''no law'' as categorical barrier), or can both coexist as live judicial philosophies within a single framework?',
    'Historical analysis of whether any single doctrinal regime has simultaneously maintained a categorical balancing test and an absolutist ''no law'' prohibition without subordinating one to the other.',
    'If foreclosed, the categorical reading''s legitimacy depends on rejecting absolutist textualism; if coexisting, the constraint''s classification may vary by judicial seat rather than by doctrine alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_absolutist_foreclosure, conceptual, 'Logical relationship between categorical balancing and absolutist readings').

omega_variable(
    harm_limited_boundary_ambiguity,
    'Where exactly does the categorical balancing reading diverge structurally from the harm_limited readingâis the divergence in the locus of harm assessment (ex ante category definition versus post hoc harm demonstration) or in the distribution of interpretive authority?',
    'Comparative doctrinal analysis of hypothetical and actual outcomes under both readings in identical speech scenarios to identify divergent results.',
    'If the divergence is only rhetorical, the two readings may collapse into a single constraint with high theater_ratio; if the divergence is real, they constitute distinct extraction patterns with different victim profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_limited_boundary_ambiguity, conceptual, 'Structural boundary between categorical balancing and harm-limited readings').

omega_variable(
    minority_protection_empirical_status,
    'Does the categorical balancing framework actually protect minority and dissident speakers more effectively than a bright-line rule or an absolutist regime would?',
    'Quantitative analysis of speech-prosecution rates, chilling-effect surveys, and litigation success rates across doctrinal regimes and jurisdictional comparisons.',
    'If minority speakers fare worse under balancing, the coordination function is weaker and the extraction (judicial control) stronger, pushing effective classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_empirical_status, empirical, 'Empirical test of coordination benefit for minority speakers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(firs_tr_t12, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(firs_tr_t24, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(firs_tr_t36, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement(firs_tr_t48, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 48, 0.34).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(firs_tr_t72, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 72, 0.44).
narrative_ontology:measurement(firs_tr_t80, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(firs_be_t12, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(firs_be_t24, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(firs_be_t36, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 36, 0.54).
narrative_ontology:measurement(firs_be_t48, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 48, 0.58).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement(firs_be_t72, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 72, 0.6).
narrative_ontology:measurement(firs_be_t80, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(firs_su_t12, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(firs_su_t24, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(firs_su_t36, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 36, 0.47).
narrative_ontology:measurement(firs_su_t48, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 48, 0.53).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(firs_su_t72, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 72, 0.6).
narrative_ontology:measurement(firs_su_t80, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 80, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three kernel readings of the First Amendment speech protection kernel. The categorical balancing reading treats the kernel as authorizing judicial category-creation; the absolutist reading treats the same text as a near-absolute barrier; the harm-limited reading treats protection as yielding to demonstrated harm. They share the same constitutional text but instantiate different beneficiary/victim structures and extraction profiles. Each reading has its own epsilon and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
