% ============================================================================
% CONSTRAINT STORY: legal_terminology_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_terminology_regime, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_terminology_regime
 *   human_readable: Legal Terminology Regime
 *   domain: law/institutional/epistemology
 *
 * SUMMARY:
 *   The legal terminology regime is a system of standardized linguistic
 *   conventions governing how law is written, interpreted, and enforced. This
 *   constraint exhibits the core tension between coordination (shared
 *   terminology enables efficient legal commerce and predictable case
 *   development) and extraction (the same terminology creates barriers to
 *   entry, suppresses lay understanding, and privileges those with access to
 *   legal credentials). The regime operates at multiple levels: substantive
 *   terminology ('consideration,' 'mens rea,' 'laches'), procedural
 *   terminology ('demurrer,' 'res judicata,' 'habeas corpus'), and structural
 *   terminology that embeds power relationships ('party,' 'judge,'
 *   'standing'). From the legal profession's perspective, this is pure
 *   coordination—shared language enables interstate commerce and efficient
 *   common law development. From the lay person's perspective, it is
 *   extraction disguised as inevitability. From the institutional perspective
 *   of courts, it is both: courts genuinely coordinate legal predictability
 *   while simultaneously enforcing the regime's opacity as a disciplinary
 *   mechanism. The regime's extractiveness has increased over the measurement
 *   interval (0.35 → 0.52) as specialized terminology has proliferated and as
 *   plain-language reform movements have been systematically contained by bar
 *   association gatekeeping. Theater ratio has similarly increased (0.42 →
 *   0.58) as the regime's formal updating mechanisms (statute codification,
 *   restatements) have become increasingly performative—annual
 *   'clarifications' that change nothing substantive.
 *
 * KEY AGENTS:
 *   - Lay Persons and Unrepresented Litigants: Primary victims (powerless/trapped) — cannot exit the legal system, cannot master its terminology, cannot challenge the regime without credentials
 *   - Linguistic Innovators and Law Reformers: Secondary victims (moderate/constrained) — face gatekeeping, reputational cost, and the coordination paradox (clarity benefits all but innovation cost falls on proposer)
 *   - Legal Profession (Lawyers, Law Schools, Bar Associations): Primary beneficiary (institutional/arbitrage) — controls terminology standards, derives income and status from gatekeeping, designs the regime
 *   - Courts and Judges: Secondary beneficiary (institutional/constrained) — benefit from predictability, enforces regime compliance, but constrained by precedent and procedural norms
 *   - Justice System Accessibility: Victim (powerless/trapped) — abstract collective good that cannot organize; bears full cost of linguistic barriers to justice
 *   - Analytical Observer: Meta-position (analytical/analytical) — risks naturalizing contingent linguistic choices as inherent to law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_terminology_regime, 0.52).
domain_priors:suppression_score(legal_terminology_regime, 0.65).
domain_priors:theater_ratio(legal_terminology_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_terminology_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(legal_terminology_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legal_terminology_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_terminology_regime, tangled_rope).
narrative_ontology:human_readable(legal_terminology_regime, "Legal Terminology Regime").
narrative_ontology:topic_domain(legal_terminology_regime, "law/institutional/epistemology").

domain_priors:requires_active_enforcement(legal_terminology_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_terminology_regime, legal_profession).
narrative_ontology:constraint_beneficiary(legal_terminology_regime, institutional_incumbents).
narrative_ontology:constraint_victim(legal_terminology_regime, lay_persons).
narrative_ontology:constraint_victim(legal_terminology_regime, linguistic_innovators).
narrative_ontology:constraint_victim(legal_terminology_regime, justice_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY PERSON (SNARE) — Trapped within the legal system they cannot exit. Must navigate terminology designed to be opaque. Suppression is total: cannot challenge the regime without mastering its language; cannot master language without years of credentialing. No self-exit mechanism.
constraint_indexing:constraint_classification(legal_terminology_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LINGUISTIC INNOVATOR (TANGLED ROPE) — Seeks to clarify legal language or propose clearer terminology. Constrained by professional gatekeeping (bar associations), reputational risk, and the coordination benefit of unified terminology (everyone must use the same terms or communication breaks). The regime both enables communication and suppresses innovation. Asymmetric extraction: clarity benefits all, but innovation cost falls on the proposer.
constraint_indexing:constraint_classification(legal_terminology_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL PROFESSION (ROPE) — Benefits from terminology regime as a coordination mechanism: shared vocabulary enables efficient case law development, precedent citation, and interstate legal commerce. The profession perceives the regime as pure coordination (Rope) because they designed and maintain it. Arbitrage: they can exit by adopting alternative terminology in particular jurisdictions if needed.
constraint_indexing:constraint_classification(legal_terminology_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COURTS (TANGLED ROPE) — Both coordinate (enable predictability via stable terminology) and extract (enforce the regime as binding, punish non-compliance with contempt of court). Courts are constrained by procedural norms and precedent even as they enforce terminology requirements. Asymmetric: litigants bear the cost of linguistic compliance; courts benefit from predictability and reduced interpretation burden.
constraint_indexing:constraint_classification(legal_terminology_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATUTE CODIFICATION (PITON) — The formal updating of legal terminology through legislative process is substantially performative. Statutes are periodically revised to 'clarify' language, but the underlying terminology regime persists unchanged. Codification theater masks the inertia of the system. Theater ratio high; functional change low.
constraint_indexing:constraint_classification(legal_terminology_regime, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some legal terminology regime is inevitable: any system of binding rules requires precise language to avoid ambiguity. Natural language is inherently polysemic; legal language must constrain meaning. This perspective risks naturalizing what is actually a contingent institutional choice (Latin-derived formalism, adversarial terminology, judge-centric grammar). The engine will identify this as a false summit.
constraint_indexing:constraint_classification(legal_terminology_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_terminology_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_terminology_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_terminology_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_terminology_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_terminology_regime, TR),
    TR >= 0.70.

:- end_tests(legal_terminology_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The regime creates measurable extraction: legal fees scale with terminology complexity, lay persons cannot self-represent effectively, litigation costs are driven partly by linguistic barriers rather than substantive disputes. However, the extraction is not maximal (snare-level) because genuine coordination value exists—shared terminology does enable legal predictability and efficient case law. The increase over time (0.35 → 0.52) reflects intensification through specialization: as law has become more complex, terminology has become more baroque, and the gatekeeping advantage has increased. Suppression (0.65): High. Multiple suppression vectors: professional licensing requirements, bar association enforcement against non-standard terminology, contempt sanctions for non-compliance in court, educational barriers (law school as mandatory credentialing step). But suppression is not total because plain-language movements exist and some courts have begun permitting simplified filings. The regime is maintained through active enforcement and cultural capture, not through mathematical or physical necessity. Theater ratio (0.58): Moderate-high and increasing. Statute codification and legal restatement projects perform the function of 'clarifying' terminology, but substantive change is minimal—the theatrical performance masks inertial resistance to plain language. The increase over time reflects the proliferation of performative legal 'reforms' that preserve the regime while appearing to modernize it.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The legal profession sees pure coordination (Rope): shared terminology is necessary for case law development, interstate legal commerce, and professional communication. They perceive their gatekeeping as legitimate quality control. Lay persons see extraction with no escape (Snare): they cannot exit the system, cannot master the language, and face penalties for non-compliance. Courts see mixed coordination-extraction (Tangled Rope from the institutional perspective): they genuinely enable predictability while simultaneously enforcing opacity as a disciplinary mechanism. Linguistic innovators see the coordination benefit but face asymmetric costs (Tangled Rope from the moderate perspective): they bear the cost of proposing clarity while all benefit. The piton perspective observes that formal codification and statute updates are increasingly performative—they create the appearance of modernization while preserving the regime's core opacity. The analytical observer risks seeing this as a natural law (Mountain)—'precise legal language requires specialized terminology'—but this naturalizes a contingent institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are determined by beneficiary/victim status and exit options. Lay persons are victims with no exit (trapped) → d ≈ 0.95 → high f(d) → experience high χ. The legal profession are beneficiaries with exit capacity (arbitrage: can reform terminology regionally if needed) → d ≈ 0.05 → low f(d) → experience negative χ (subsidy). Courts are institutional beneficiaries constrained by precedent and procedure → d ≈ 0.25 → moderate f(d). Linguistic innovators are moderate-power victims constrained by gatekeeping → d ≈ 0.70 → high f(d). The derivation chain shows that the same structural phenomenon (terminology regime) produces radically different experienced extractiveness depending on power level and exit capacity. This is the core diagnostic: if all perspectives classified identically, the regime would be a natural law (mountain). Instead, perspectives diverge sharply, revealing that the regime's nature depends entirely on the observer's structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT IDENTITY RESOLUTION: The legal terminology regime illustrates the Mandatrophy Puzzle—the same structural phenomenon (standardized linguistic conventions) can be read as pure coordination (Rope) or extractive gatekeeping (Snare) depending on agent position. The resolution is perspectival: both readings are correct. The regime genuinely coordinates (shared terminology enables case law efficiency). The regime genuinely extracts (terminology barriers exclude lay participation and privilege credentialed professionals). The Tangled Rope classification resolves the apparent paradox by declaring that BOTH functions are real and BOTH are asymmetric. The legal profession benefits from coordination without bearing its costs (lay persons do); lay persons bear suppression costs without receiving coordination benefits (the system is opaque to them). The mandate to classify as either Rope (pure coordination) or Snare (pure extraction) is itself misdirected—the constraint's real nature is hybrid. The theater component (statue codifications that change nothing) adds a piton dimension: some aspects of the regime persist through inertia and theatrical renewal rather than active function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    terminology_vs_substance,
    'Is the extraction mechanism the terminology regime itself, or is terminology merely the vector through which substantive legal doctrines extract?',
    'Counterfactual test: if terminology were simplified to plain language while keeping substantive law unchanged, would extraction cease? If substantive law were reformed while keeping current terminology, would extraction cease? Which reform would be resisted more fiercely?',
    'If terminology is primary: the constraint is primarily linguistic/coordination (Rope with markup). If substantive law is primary: terminology is merely disguise, and the real extraction mechanism lies elsewhere. If both are coupled: the regime is genuinely tangled (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminology_vs_substance, conceptual, 'Whether extraction is driven by terminology regime or by substantive legal doctrine').

omega_variable(
    accessibility_vs_legitimacy,
    'Is legal terminology opacity necessary for perceived legitimacy of the legal system? Would simplified terminology undermine public confidence in the law''s impartiality?',
    'Comparative jurisdictional analysis: study legal systems with simplified terminology (plain-language court documents, simplified court procedures) and measure public trust levels, compliance rates, and appeal volumes vs. traditional terminology regimes',
    'If legitimacy depends on opacity: the regime serves a real psychological function (high coordination value). If legitimacy is independent of terminology: opacity is pure extraction. If mixed: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_vs_legitimacy, empirical, 'Whether perceived legitimacy depends on terminology complexity').

omega_variable(
    international_harmonization,
    'Is the regime enforced locally by bar associations, or is it globally entrenched through international legal harmonization (international treaties, model laws, soft law standards)?',
    'Trace legal terminology standards through treaty bodies, model law adoption, and soft law instruments. Identify which terminology choices are locally contestable vs globally locked by treaty obligations or institutional dependencies.',
    'If locally enforced: suppression is institutional/contestable (lower suppression, possibility of reform). If globally locked: suppression is structural and harder to escape (higher suppression, snare classification more likely). Affects exit_options for institutional reformers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_harmonization, empirical, 'Whether regime is locally or globally entrenched').

omega_variable(
    plainlanguage_sufficiency,
    'Can legal concepts actually be expressed accurately in plain language without substantial loss of precision, or does precision genuinely require specialized terminology?',
    'Pilot studies: take complex legal concepts and attempt faithful translation to plain language. Test comprehension and error rates in lay audiences. Compare to traditional terminology understanding rates among lay persons.',
    'If plain language is sufficient: the regime''s suppression is artificial (extraction-driven). If precision genuinely requires specialized language: the suppression is necessary (coordination-driven). This determines whether the Snare or Rope classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plainlanguage_sufficiency, empirical, 'Whether plain language can express legal concepts accurately').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_terminology_regime, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legterm_tr_t0, legal_terminology_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(legterm_tr_t50, legal_terminology_regime, theater_ratio, 50, 0.51).
narrative_ontology:measurement(legterm_tr_t100, legal_terminology_regime, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(legterm_be_t0, legal_terminology_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legterm_be_t50, legal_terminology_regime, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(legterm_be_t100, legal_terminology_regime, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_terminology_regime, information_standard).
narrative_ontology:boltzmann_floor_override(legal_terminology_regime, 0.08).
narrative_ontology:affects_constraint(legal_terminology_regime, legal_professionalization_barrier).
narrative_ontology:affects_constraint(legal_terminology_regime, justice_access_inequality).
narrative_ontology:affects_constraint(legal_terminology_regime, contract_interpretation_asymmetry).

% DUAL FORMULATION NOTE:
% The legal terminology regime can be decomposed into three structurally distinct constraints: (1) terminology_as_coordination (ε ≈ 0.15, Rope) — the genuine coordination value of shared language for case development; (2) terminology_as_gatekeeping (ε ≈ 0.68, Snare) — the extractive barrier to entry created by professionalization requirements; (3) terminology_as_theater (ε ≈ 0.40, Piton) — the performative updating of language without substantive reform. This story models the integrated system; decomposition is available if finer structural analysis is needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_terminology_regime, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
