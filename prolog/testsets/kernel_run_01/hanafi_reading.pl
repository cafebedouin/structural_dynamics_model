% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanafi_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Reason and Analogy as Primary Interpretive Tools
 *   domain: islamic_jurisprudence/legal_philosophy/commitment_systems
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method, emphasizing reason (aql) and analogy
 *   (qiyas) as primary interpretive tools with juristic preference (istihsan)
 *   as valid secondary source, represents one contested reading of how
 *   Islamic law should be interpreted and administered. This reading
 *   prioritizes flexibility and reasoned judgment over strict textualism,
 *   enabling adaptive jurisprudence suited to administrative governance and
 *   changing circumstances. The constraint operates as a hybrid
 *   coordination-extraction mechanism: it genuinely solves the problem of
 *   adapting sacred textual law to administrative complexity and merchant
 *   commercial practice (coordination function), while simultaneously
 *   centralizing interpretive authority in institutional elites and enabling
 *   strategic rulings favorable to state and merchant interests (extraction
 *   function). The beneficiaries are state administrators seeking flexible
 *   rulings for governance needs, judicial elites whose interpretive
 *   authority is elevated by the requirement for reasoned judgment, and urban
 *   merchant classes who can structure transactions for legitimacy through
 *   creative analogies. The victims are textualist purists whose epistemic
 *   commitment (direct text as sole authority) is systematically
 *   deprioritized, bedouin customary practitioners whose local judgment is
 *   overridden by centralized judicial authority, and strict literalist
 *   constituencies whose preferred interpretive methodology is subordinated.
 *   The theater ratio (0.35) reflects that while the Hanafi method performs
 *   greater fidelity to methodological transparency than pure discretion
 *   would, it still maintains a legitimacy theater: the formal commitment to
 *   'Quran and Sunnah alone' persists as doctrine while aql and istihsan
 *   operationally override textual literalism. Measurements show extraction
 *   accumulation over the interval (0.22 → 0.38), with theater rising as
 *   contemporary criticism of discretionary authority increases pressure to
 *   justify rulings through reasoned principle rather than explicit juristic
 *   preference.
 *
 * KEY AGENTS:
 *   - State Administrators: Primary beneficiary (institutional/arbitrage) — require flexible rulings for administrative necessity; Hanafi method provides legitimacy cover for responsive governance
 *   - Judicial Elites: Primary beneficiary (institutional/arbitrage) — interpretive authority centralized through requirement for reasoned judgment; istihsan requires expert juristic discretion
 *   - Urban Merchant Coalition: Secondary beneficiary (organized/constrained) — can structure transactions for legitimacy through qiyas and istihsan; benefits from efficient dispute resolution
 *   - Textualist Purists: Primary victim (powerless/trapped) — core epistemic commitment (text as sole authority) systematically deprioritized; no exit path within legal system
 *   - Bedouin Customary Practitioners: Secondary victim (moderate/constrained) — local judgment overridden by centralized authority; customary practices recognized only through istihsan discretion
 *   - Strict Literalist Constituencies: Tertiary victim (powerless/trapped) — preferred methodology subordinated; textual interpretation constrained by reasoned necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as genuine coordination hybrid (adapting law to complexity) that simultaneously enables institutional extraction through discretionary authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.38).
domain_priors:suppression_score(hanafi_reading, 0.42).
domain_priors:theater_ratio(hanafi_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Jurisprudential Method: Reason and Analogy as Primary Interpretive Tools").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_philosophy/commitment_systems").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, '607e3a6c-c3a0-491c-9eb8-725788e84b23').
narrative_ontology:cs_created_at('607e3a6c-c3a0-491c-9eb8-725788e84b23', '').
narrative_ontology:cs_kernel_codification('607e3a6c-c3a0-491c-9eb8-725788e84b23', fixed_text).
narrative_ontology:cs_authority_grounding('607e3a6c-c3a0-491c-9eb8-725788e84b23', lineage).
narrative_ontology:cs_interpretation_layer_present('607e3a6c-c3a0-491c-9eb8-725788e84b23').
narrative_ontology:cs_kernel_id(hanafi_reading, jurisprudential_method_kernel).
narrative_ontology:cs_reading_relation('607e3a6c-c3a0-491c-9eb8-725788e84b23', maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('607e3a6c-c3a0-491c-9eb8-725788e84b23', shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('607e3a6c-c3a0-491c-9eb8-725788e84b23', hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('607e3a6c-c3a0-491c-9eb8-725788e84b23', foundational, aql_primacy_in_interpretation).
narrative_ontology:cs_axiom_status(aql_primacy_in_interpretation, holdable).
narrative_ontology:cs_axiom('607e3a6c-c3a0-491c-9eb8-725788e84b23', foundational, istihsan_as_valid_juristic_method).
narrative_ontology:cs_axiom_status(istihsan_as_valid_juristic_method, holdable).
narrative_ontology:cs_reference_frame('607e3a6c-c3a0-491c-9eb8-725788e84b23', institutional_flexibility_framework).
narrative_ontology:cs_drift_state('607e3a6c-c3a0-491c-9eb8-725788e84b23', contemporary_islamic_jurisprudence, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, state_administrators).
narrative_ontology:constraint_beneficiary(hanafi_reading, urban_merchant_classes).
narrative_ontology:constraint_beneficiary(hanafi_reading, judicial_elites).
narrative_ontology:constraint_victim(hanafi_reading, textualist_purists).
narrative_ontology:constraint_victim(hanafi_reading, bedouin_customary_practitioners).
narrative_ontology:constraint_victim(hanafi_reading, strict_literalist_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUALIST PURIST (SNARE) — Trapped in a legal framework where their core epistemic commitment (direct text as sole authority) is systematically deprioritized. Reason (aql) and juristic preference (istihsan) override textual literalism, foreclosing their interpretive strategy. Maximum suppression of alternative hermeneutics; no exit path within the legal system.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BEDOUIN CUSTOMARY PRACTITIONER (TANGLED ROPE) — Benefits from local customary practices being recognized as valid through istihsan and aql-based reasoning, yet constrained by the centralization of jurisprudential authority into urban Hanafi circles. Mixed coordination (local customs matter) and extraction (centralized courts override local judgment).
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATOR (ROPE) — Primary beneficiary. The Hanafi method's emphasis on juristic discretion (istihsan) and reasoned analogy (qiyas) enables responsive governance: administrators can justify flexible rulings for administrative necessity without appearing to violate the law. The constraint solves a genuine coordination problem (adapting sacred law to administrative complexity) while providing cover for state interests.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JUDICIAL ELITE (ROPE) — Institutional beneficiary. The primacy of reason and analogy elevates the interpretive authority of trained jurists. Istihsan requires juristic judgment; qiyas requires methodological expertise. This constrains legal interpretation to credentialed scholars, centralizing authority and enabling strategic juristic discretion — all while maintaining a coordination function (reducing legal chaos through systematic methodology).
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: URBAN MERCHANT COALITION (TANGLED ROPE) — Organized beneficiary with constrained exit. The Hanafi method's flexibility enables merchants to structure transactions for legitimacy through creative analogies (qiyas) and juristic preference (istihsan). This provides genuine coordination (resolving contract disputes efficiently) alongside strategic extraction (merchants can engineer rulings favoring their interests through juristic discretion). High agency but embedded in a framework they do not control.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: TEXTUAL LITERALIST TRADITION (PITON) — The commitment to 'Quran and Sunnah alone' persists as formal doctrine in Hanafi jurisprudence but functions theatrically: aql and istihsan operationally override textual literalism while nominally deriving from textual authority. The literalist position is maintained for legitimacy, not function. Theater ratio high because the tradition performs fidelity to text while systematically subordinating text to reason.
constraint_indexing:constraint_classification(hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the Hanafi method coordinates legitimate legal reasoning (aql, analogy, juristic preference are tools for principled interpretation) while enabling institutional extraction (state and judicial elites consolidate authority through discretionary rulings justified by reason). The framework is neither pure coordination nor pure extraction, but a structured hybrid where flexibility serves coordination at the same time it empowers gatekeepers.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Hanafi method's flexibility is not pure extraction like strict predatory mechanisms — it solves genuine coordination problems (administrative adaptation, commercial complexity). However, the discretionary authority it enables (istihsan, aql-based judgment) creates structural opportunities for beneficiary-aligned rulings. The extraction is embedded within coordination, not separate from it. The value reflects that roughly 38% of the observed beneficial outcomes for state/judicial/merchant actors come from extractive positioning rather than genuine coordination benefit. Suppression (0.42): Moderate. Significant barriers exist to alternative interpretive methodologies (textualism, literalism) within Hanafi jurisprudential frameworks. The hierarchical positioning of aql over text creates strong suppression of literalist approaches. However, suppression is not total — Hanbali and other schools maintain textual priority, and contemporary critiques of istihsan are increasing. The value reflects structural deprioritization rather than absolute foreclosure. Theater ratio (0.35): Moderate-low, indicating relatively transparent methodological commitment compared to pure discretion, but increasing over time as contemporary pressures require greater justification through reasoned principle rather than explicit juristic preference.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival gaps between beneficiaries and victims. State administrators and judicial elites see the Hanafi method as enabling principled flexibility (Rope) — a coordination mechanism for adapting law to complexity. Textualist purists see the same mechanism as suppression of their epistemic methodology (Snare) — they are trapped in a system that deprioritizes their core interpretive commitment. Urban merchants see mixed coordination and discretionary advantage (Tangled Rope). Bedouin practitioners see constrained recognition of local custom through gatekeepers (Tangled Rope). The analytical observer sees the legitimate coordination function alongside the institutional extraction function (Tangled Rope). The literalist tradition itself maintains a piton classification — the formal commitment to textual primacy persists through doctrine and invocation while aql and istihsan operationally override it. This is the exemplary perspectival divergence: the beneficiary's 'reasoned flexibility' is the victim's 'suppression of methodology,' and both are structurally accurate from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the interpretive hierarchy. State administrators and judicial elites benefit from the prioritization of aql and istihsan — their position in the system is elevated when reason and juristic discretion are primary (d ≈ 0.15, low target status, beneficiary position). Textualist purists bear maximum extraction from the subordination of their methodology — they are targets of the constraint (d ≈ 0.95, near-maximum target status). Bedouin practitioners have intermediate position: they benefit from istihsan's recognition of custom but are constrained by centralized judicial authority overriding their judgment (d ≈ 0.65, mixed). Urban merchants benefit from flexibility enabling transaction structuring while being constrained by judicial gatekeeping (d ≈ 0.50, balanced). The analytical observer occupies the neutral position (d ≈ 0.72, per canonical derivation for analytical power atom).
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi reading resolves the mandatrophy between coordination and extraction by documenting that the method is genuinely both. The primary coordination function (adapting sacred law to administrative complexity and commerce) is real — the Hanafi framework solves problems that pure textualism cannot solve efficiently. The extraction function (centralizing authority in institutional elites, enabling state-favorable rulings through discretionary istihsan) is also real — the method's flexibility is systematically applied to benefit specific agents. The mandatrophy dissolves when we recognize that coordination and extraction are not mutually exclusive but structurally layered: the same mechanism that solves the coordination problem simultaneously enables extractive positioning. This is the exemplary structure of Tangled Rope — the constraint cannot be understood by analyzing either function in isolation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    juristic_discretion_vs_reasoned_principle,
    'Is juristic preference (istihsan) a principled method for reasoned judgment or a post-hoc legitimation of discretionary authority?',
    'Analysis of istihsan rulings: are they consistently principle-driven or do patterns show correlation with beneficiary interests (state needs, merchant class interests, judicial power consolidation)? Historical case studies comparing istihsan justifications to outcomes.',
    'If principled: Hanafi method is genuine coordination mechanism (Rope/Tangled Rope classification sustained). If post-hoc: method is extractive cover story (Snare/Tangled Rope extraction component dominates). Classification shifts toward higher effective extraction and lower legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(juristic_discretion_vs_reasoned_principle, empirical, 'Whether istihsan functions as principled reasoning or discretionary authority cover').

omega_variable(
    analogy_scope_constraint,
    'Does the Hanafi emphasis on qiyas as primary interpretive tool systematically privilege certain types of analogies (urban commercial, administrative) over others (pastoral, customary)?',
    'Corpus analysis of Hanafi qiyas rulings: distribution of analogical structures by social domain. Comparative analysis with Hanbali literalism and Maliki customary preference to identify whether Hanafi method is neutral reasoning tool or domain-selective framework.',
    'If neutral: aql-based reasoning is genuinely open to all observables (legitimate coordination). If domain-selective: Hanafi method structurally privileges urban/administrative/merchant interests (extraction revealed). Shifts suppression measure and victim identification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(analogy_scope_constraint, empirical, 'Whether Hanafi analogy privileges certain social domains over others').

omega_variable(
    textual_authority_subordination_timing,
    'At what point in Hanafi jurisprudential development did aql and istihsan move from ''subordinate to text'' to ''equal or primary''? Was this a deliberate doctrinal shift or gradual practice drift?',
    'Diachronic analysis of jurisprudential texts (early Abu Hanifa vs later Hanafi schools vs Ottoman codification). Track the formal status of aql, istihsan, and qiyas relative to explicit text across periods. Identify whether texts document the shift or obscure it.',
    'If deliberate shift documented: enables recognition as institutional evolution (Scaffold with sunset clause for textual primacy). If obscured/denied: theatricality of literalist commitment increases, piton classification strengthened. If gradual drift: supports mandatrophy analysis (coordinate system gradually replaced, but transition remains unacknowledged).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_subordination_timing, empirical, 'Whether Hanafi emphasis on reason was deliberate doctrinal shift or unacknowledged practice drift').

omega_variable(
    reading_vs_sibling_differentiation,
    'What is the core structural difference between the Hanafi reading (reason and analogy primary) and its sibling readings (Hanbali literalism, Maliki customary, Shafii systematization)?',
    'Comparative jurisprudential analysis: mapping the interpretive hierarchy (text > reason > analogy > custom > necessity) for each school. Identify whether differences reflect epistemological principles (how knowledge is grounded) or strategic institutional positioning.',
    'If epistemological: schools represent genuine methodological diversity (coexist_with). If strategic positioning: Hanafi emphasis on reason reflects historical position as state-centered school (influences or forecloses sibling readings). Shapes reading_relations declarations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_differentiation, conceptual, 'Whether Hanafi method reflects epistemological principle or institutional positioning').

omega_variable(
    istihsan_legitimacy_erosion,
    'In contemporary Islamic jurisprudence, has istihsan lost legitimacy relative to explicit textual grounding and Shafii-style systematic reasoning?',
    'Survey of contemporary Islamic legal scholarship and fatwa trends: frequency of istihsan invocation, acceptability across schools, treatises defending vs criticizing istihsan. Comparison with 19th-century Ottoman jurisprudence (peak Hanafi influence) vs 21st-century jurisprudence.',
    'If legitimacy eroded: Hanafi reading is experiencing ''authority_erosion'' drift (cs_structure.drift_state direction). Theater ratio may increase (performance of textual fidelity becomes more necessary as discretionary authority becomes less defensible). Piton classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_legitimacy_erosion, empirical, 'Contemporary legitimacy trajectory of istihsan-based reasoning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_theater_t0, hanafi_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanafi_theater_t5, hanafi_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(hanafi_theater_t10, hanafi_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(hanafi_extractiveness_t0, hanafi_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hanafi_extractiveness_t5, hanafi_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(hanafi_extractiveness_t10, hanafi_reading, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(hanafi_reading, 0.12).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).
narrative_ontology:affects_constraint(hanafi_reading, ottoman_codification_constraint).
narrative_ontology:affects_constraint(hanafi_reading, contemporary_istihsan_legitimacy).

% DUAL FORMULATION NOTE:
% The Hanafi jurisprudential reading is one constraint in a family of four: maliki_reading, shafii_reading, hanbali_reading instantiate the rival interpretive methodologies for the same kernel (jurisprudential_method_kernel). Each reading has distinct epsilon values reflecting different empirical statuses, institutional influence, and observed extraction mechanisms. They are not the same constraint viewed from different angles but structurally distinct constraints grounded in the same contested kernel. This decomposition follows the ε-invariance principle: if changing which jurisprudential school is the reading changes the epsilon value (because that school's actual institutional influence, discretionary scope, and empirical extraction differ), then separate constraints are required. The sibling readings are linked via network.affects_constraints and share the kernel_id in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanafi_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
