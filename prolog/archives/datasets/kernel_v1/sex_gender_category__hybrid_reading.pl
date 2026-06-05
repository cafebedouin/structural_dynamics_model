% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership via Medical Gatekeeping (Hybrid Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   The hybrid reading of sex/gender category membership creates a
 *   gatekeeping constraint centered on medical institutions. Under this
 *   reading, trans women can access category membership if and only if they
 *   undergo medical transition (hormone therapy, documented medical
 *   supervision) and obtain clinical authorization. This creates a three-tier
 *   victim structure: (1) non-transitioning trans individuals are
 *   categorically excluded regardless of identity; (2) trans individuals with
 *   medical access experience the constraint as Tangled Rope (coordination +
 *   extraction); (3) trans individuals without medical access experience it
 *   as Snare (pure extraction, with exit gates locked by cost/geography).
 *   Medical gatekeeping institutions are beneficiaries — they gain legitimate
 *   authority to determine category boundaries and control the transition
 *   process. The cisgender majority benefits from category stability and
 *   boundary clarity maintained via the medical filter. The extractiveness
 *   value (0.58) reflects that the hybrid mechanism coordinates access to
 *   category membership (genuine benefit for resourced trans individuals)
 *   while simultaneously extracting through the gatekeeping requirement (high
 *   financial cost, years of clinical dependency, documentation obligations,
 *   authority concentration). The theater ratio (0.62) captures that some
 *   medical requirements serve genuine health monitoring functions while
 *   others serve primarily administrative/boundary-maintenance purposes
 *   (repeated identity documentation, clinical sign-off on social role,
 *   psychological assessment criteria that vary by clinician ideology). The
 *   rising trajectory in measurements reflects institutional hardening: as
 *   the hybrid model has become established in legal and medical systems,
 *   both extraction and suppression have increased — waiting lists
 *   lengthened, documentation requirements expanded, clinician gatekeeping
 *   norms ossified.
 *
 * KEY AGENTS:
 *   - Non-Transitioning Trans Individuals: Structural exclusion (powerless/trapped) — no pathway to category membership under hybrid reading; maximum extraction via identity suppression
 *   - Trans Individuals with Limited Medical Access: Gatekeeping victims (moderate/constrained) — face severe resource barriers to accessing the medical transition requirement; constrained exit options
 *   - Trans Individuals with Medical Resources: Resourced transition seekers (moderate/constrained) — can access medical gatekeeping pathway; experience mixed coordination and extraction
 *   - Medical Gatekeeping Institutions: Institutional beneficiaries (institutional/arbitrage) — derive authority and organizational function from determining transition criteria and authorizing category changes
 *   - Cisgender Majority: Structural beneficiaries (organized/constrained) — benefit from stable category boundaries maintained via medical filter; face minimal direct cost
 *   - Category Integrity Epistemic Commons: Victim (powerless/trapped) — abstract collective good that bears the cost of category instability claims or contested boundaries; cannot organize; abstract notion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership via Medical Gatekeeping (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '34fb5646-f752-49d0-a283-4558d2c61cf9').
narrative_ontology:cs_kernel_codification('34fb5646-f752-49d0-a283-4558d2c61cf9', distributed).
narrative_ontology:cs_authority_grounding('34fb5646-f752-49d0-a283-4558d2c61cf9', extraction).
narrative_ontology:cs_reading_relation('34fb5646-f752-49d0-a283-4558d2c61cf9', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('34fb5646-f752-49d0-a283-4558d2c61cf9', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('34fb5646-f752-49d0-a283-4558d2c61cf9', foundational, category_change_requires_medical_authorization).
narrative_ontology:cs_axiom_status(category_change_requires_medical_authorization, holdable).
narrative_ontology:cs_axiom_grounding('34fb5646-f752-49d0-a283-4558d2c61cf9', category_change_requires_medical_authorization, conventional).
narrative_ontology:cs_axiom('34fb5646-f752-49d0-a283-4558d2c61cf9', secondary, medical_gatekeeping_legitimizes_category_boundaries).
narrative_ontology:cs_axiom_status(medical_gatekeeping_legitimizes_category_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('34fb5646-f752-49d0-a283-4558d2c61cf9', medical_gatekeeping_legitimizes_category_boundaries, conventional).
narrative_ontology:cs_reference_frame('34fb5646-f752-49d0-a283-4558d2c61cf9', medical_transition_mediated_category_access).
narrative_ontology:cs_drift_state('34fb5646-f752-49d0-a283-4558d2c61cf9', contemporary_medical_institutional_consolidation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('34fb5646-f752-49d0-a283-4558d2c61cf9', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_majority).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, economically_precarious_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, category_integrity_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-TRANSITIONING TRANS INDIVIDUAL (SNARE) — Structurally excluded from category membership regardless of identity claim. No exit pathway available: either internalize exclusion or challenge the entire classification system. Maximum extraction (suppression of identity claim) with no coordination benefit. The hybrid reading explicitly forecloses this agent's self-identification.
constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANS INDIVIDUAL WITH LIMITED MEDICAL ACCESS (SNARE) — Face severe resource barriers (cost, geographic access, waiting lists, clinician gatekeeping) to access the medical transition pathway. High exit costs but theoretically available. Extraction operates through the medical requirement itself — the constraint uses access barriers to filter who qualifies for category membership. Experienced extraction is near-maximal due to the prohibitive cost structure of medical gatekeeping.
constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TRANS INDIVIDUAL WITH MEDICAL RESOURCES (TANGLED ROPE) — Can afford and access medical transition pathway. Experiences genuine coordination (medical transition enables social recognition, legal category change, material access to gendered spaces) AND asymmetric extraction (high financial cost, years of clinical dependency, medical professionals retain authority over transition decisions, monitoring and documentation obligations). Both functions are present and significant.
constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL GATEKEEPING INSTITUTIONS (ROPE) — Experience the hybrid classification as a coordination mechanism: defining transition criteria enables them to authorize category changes and manage clinical processes. Beneficiary position via institutional authority — they control the boundary of who qualifies. Low experienced extraction because they frame the mechanism as their legitimate expert domain. The constraint coordinates the delegation of category authority to the medical profession.
constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CISGENDER MAJORITY (ROPE) — Benefits from category stability and boundary clarity maintained via medical gatekeeping. The medical requirement creates a filter (cost, accessibility, documentation) that keeps category boundaries legible and relatively stable. Experiences the constraint as coordination (predictable category definitions) without experiencing significant extraction (no cost to them). Their stable category membership is preserved.
constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HYBRID READING COMMITTER (TANGLED ROPE) — Sees both genuine coordination (medical gatekeeping provides a processual path for trans individuals to access category membership and institutional recognition) AND genuine extraction (the medical requirement filters out economically precarious and non-medically-transitioning individuals; it concentrates authority in medical institutions; it imposes monitoring and documentation obligations). The constraint is neither pure coordination nor pure extraction — it is a hybrid mechanism where access to coordination services is itself extracted.
constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sex_gender_category__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The hybrid reading enables genuine coordination (medical transition provides a recognized pathway to category membership and legal status change) while imposing significant extraction (the medical requirement itself is a filtering mechanism that redistributes resources toward gatekeeping institutions and excludes economically precarious individuals). The value reflects both functions operating simultaneously. The rising trajectory (0.42→0.58) captures institutional hardening: as the hybrid model became established in multiple jurisdictions, gatekeeping criteria expanded and waiting periods lengthened, increasing the extraction cost. Suppression (0.72): High. The constraint suppresses alternative pathways to category membership (direct legal recognition without medical transition is unavailable; self-identification alone carries no official recognition). For non-transitioning trans individuals, suppression is total (no exit pathway). For constrained agents, suppression is severe (medical access barriers). Theater ratio (0.62): Moderate-high. Medical transition involves genuine health considerations (hormone monitoring, health screening) but also performative requirements (repeated identity documentation, clinician sign-off rituals, psychological assessment criteria that vary by ideology rather than clinical necessity). The theater has increased as the hybrid model standardized — what began as individualized clinical assessment has become a ritualized gatekeeping process with performative elements (e.g., 'real-life test' requirements that serve boundary maintenance rather than clinical monitoring). Tangled Rope classification: The hybrid reading exhibits both genuine coordination (it does coordinate the legal transition process; it provides a recognized pathway) and genuine extraction (the gatekeeping mechanism itself filters access by economic capacity; it imposes high resource and time costs; it concentrates authority in medical institutions). Both functions must be present and significant for Tangled Rope. The beneficiaries declaration (medical institutions, cisgender majority) captures who gains from the constraint. The victims declaration (non-transitioning trans, economically precarious trans, epistemic commons) captures who bears the costs.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is structural: the same mechanism appears as pure coordination (Rope) from the medical institution's perspective, as mixed coordination-extraction (Tangled Rope) from resourced trans individuals' perspective, and as pure extraction (Snare) from non-transitioning or economically precarious individuals' perspective. The cisgender majority sees Rope (category stability, no direct cost). The medical institutions see Rope (they control the boundary, they coordinate the process, they benefit from authority). The resourced trans individual sees Tangled Rope (genuine access to category membership AND high costs to obtain it). The constrained trans individual sees Snare (barriers to the gatekeeping pathway are insurmountable). The non-transitioning trans individual sees Snare (the hybrid reading categorically excludes them). The analytical observer sees Tangled Rope (both coordination and extraction are structural features). This gap reveals that what medical institutions frame as neutral expertise (coordinating transition processes) operates as extraction for agents without medical access and as categorical exclusion for non-transitioning individuals.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position: beneficiaries (medical institutions, cisgender majority) have d-values near 0.0 (full beneficiary), meaning they experience low or negative effective extraction via the sigmoid function. Victims (trans individuals constrained by medical gatekeeping, non-transitioning trans individuals) have d-values near 0.8-1.0 (full targets), meaning they experience high effective extraction. The medical institutions' institutional power + arbitrage exit options produce low d; the medical gate is their domain of expertise and they can arbitrage to other institutional roles if medical gatekeeping loses legitimacy. The non-transitioning trans individual's powerless power + trapped exit options produce very high d (they are maximally targeted and have no exit within the hybrid framework). The constrained trans individual's moderate power + constrained exit options produce d in the 0.75-0.85 range (high targeting, but some theoretical exit possible if they acquire resources or if institutional barriers decrease). The medical institutions' institutional power + arbitrage options + beneficiary status produce d near 0.05-0.15 (they are full beneficiaries and retain exit optionality). The perspective tuple then applies the sigmoid f(d) to compute effective extraction chi = epsilon * f(d) * scope_modifier. For high-d agents (victims), f(d) > 1.0, amplifying the base extractiveness. For low-d agents (beneficiaries), f(d) < 0.0, producing negative chi (they experience the constraint as beneficial). The analytical observer with identity_locked orientation would note that for trans individuals who have internalized the medical gatekeeping framework as legitimate, their perceived exit option may be identity_locked rather than constrained — they have structural mobility (they could theoretically exit the transition pathway) but cannot exercise it because their identity is constituted through the medical framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves via the tangled_rope classification. The hybrid reading avoids the false choice between pure coordination (Rope) and pure extraction (Snare) by recognizing that the mechanism serves both functions: it DOES coordinate a recognized transition pathway (genuine coordination benefit), AND it DOES extract via gatekeeping requirements (genuine extraction mechanism). The classification holds both simultaneously. The perspectival gap confirms this: different agents experience the same mechanism as having different dominant characteristics because their structural positions differ. The cisgender majority and medical institutions perceive coordination (because they are not the targets of extraction). Non-transitioning and economically precarious trans individuals perceive extraction (because they are the targets of gatekeeping filters). The resourced trans individual perceives both (because they experience both the coordination benefit and the extraction cost). The false summit detector would flag the biological-reading perspective if it were declared as a Mountain (natural law about sex category) — the presence of medical gatekeeping beneficiaries would trigger FSM evaluation. The hybrid reading itself avoids this by explicitly building extraction into the mechanism: the hybrid reading is not claiming biological immutability; it is claiming that a combination of biology + medical process determines membership. This is clearly not a natural law; it is an institutional arrangement with identifiable beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_gatekeeping_function,
    'Does medical gatekeeping serve a legitimate medical/clinical necessity (monitoring health during transition) or is the gatekeeping function primary (filtering category membership)?',
    'Comparative analysis of gatekeeping criteria across jurisdictions: do strict identity/dysphoria documentation requirements map to clinical monitoring outcomes or to category filtering logic? Cost-benefit analysis of medical oversight vs. harm from delayed access.',
    'If genuine medical necessity: classification shifts toward Rope (coordination dominant). If gatekeeping function primary: classification shifts further toward Snare (extraction dominant, especially for constrained agents).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_gatekeeping_function, empirical, 'Whether medical gatekeeping function is driven by clinical necessity or category boundary maintenance').

omega_variable(
    kernel_reading_identity_locked,
    'Is the hybrid reading instantiating a genuinely tenable normative position or is it a rationalizing framework that the biology_reading and identity_reading both reject as incoherent?',
    'Discourse analysis of judicial and legislative arguments in hybrid-gatekeeping jurisdictions: do they defend the reading as internally coherent or do they oscillate between biology_reading and identity_reading when pressed? Evidence of explicit rejection by biology_reading and identity_reading advocates.',
    'If tenable: the kernel has three coexisting readings with distinct epistemic bases. If rationalizing: the hybrid_reading coexists with but is not equally legitimate as the other two — it functions as a compromise that neither pole endorses as foundational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_locked, conceptual, 'Whether hybrid reading is genuinely tenable normative position or rationalizing compromise').

omega_variable(
    medical_access_as_extraction_vector,
    'What proportion of trans individuals who desire medical transition are unable to access it due to cost, geography, or clinician refusal? Does this proportion constitute a genuine snare (structural economic extraction) or a temporary access problem?',
    'Population surveys on medical transition access: prevalence of financial barriers, geographic barriers, and clinician gatekeeping refusals as obstacles to transition. Longitudinal data on access changes in response to policy or institutional interventions.',
    'If >40% face prohibitive barriers: the snare classification for constrained/powerless perspectives is robust; extraction is a primary mechanism. If <20%: constraints may be decreasing over time; trajectory is toward Rope or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_access_as_extraction_vector, empirical, 'Prevalence and severity of barriers to medical access for transition-seeking trans individuals').

omega_variable(
    identity_lock_in_hybrid_framework,
    'For trans individuals who accept the medical gatekeeping framework as legitimate, is their acceptance structural (external barriers, internalized cost-benefit reasoning) or identity-locked (the framework has become constitutive of their understanding of their own transition)?',
    'Qualitative interviews with trans individuals who have completed medical gatekeeping: distinguish between ''I accept this because alternatives are worse'' (constrained) and ''I see medical authority as legitimate to my own identity'' (identity_locked). Language analysis of framing: self-advocacy language vs. authority-deference language.',
    'If primarily identity_locked: the hybrid reading is more stable and harder to dislodge (normative commitment, not just pragmatic acceptance). If primarily constrained: the reading is vulnerable to disruption if access barriers decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_hybrid_framework, empirical, 'Whether acceptance of medical gatekeeping by trans individuals reflects identity fusion or pragmatic constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_hybrid_tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sgc_hybrid_tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sgc_hybrid_tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(sgc_hybrid_be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sgc_hybrid_be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(sgc_hybrid_be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sgc_hybrid_su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sgc_hybrid_su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(sgc_hybrid_su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel sex_gender_category. The kernel has three coexisting readings instantiated in separate constraint stories: biology_reading (category by immutable reproductive biology), hybrid_reading (category by biology + medical transition), identity_reading (category by self-identification). All three readings share the same kernel but have different epsilon values, different victim sets, different authority groundings, and different institutional implications. The network links show the interdependencies: the hybrid reading occupies the middle ground and influences both siblings by establishing the category boundary as negotiable and mediated by institutions. See kernel_context for reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
