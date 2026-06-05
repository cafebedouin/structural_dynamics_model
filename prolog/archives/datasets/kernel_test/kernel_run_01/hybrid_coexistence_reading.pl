% ============================================================================
% CONSTRAINT STORY: hybrid_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_coexistence_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hybrid_coexistence_reading
 *   human_readable: Hybrid Coexistence of Liturgical and Native Hebrew Language Forms
 *   domain: sociolinguistics/religious_studies/nation_building
 *
 * SUMMARY:
 *   The hybrid coexistence reading of Hebrew language revival claims that
 *   both liturgical and native forms constitute valid, complementary
 *   expressions of a unified living language — neither is subordinate or
 *   derivative of the other. This reading emerged post-revisionism, after the
 *   initial Zionist language planners had already made foundational choices
 *   about separation of domains. The reading coordinates religious authority
 *   (which depends on liturgical texts retaining sacred status) with
 *   linguistic modernization (which requires native-speaker generativity and
 *   natural semantic drift). The constraint exhibits moderate extractiveness
 *   because coordination costs are real — educators must manage
 *   code-switching, curricula must include classical texts, language planning
 *   must accommodate competing legitimacy claims — but there is no
 *   categorical victim. Religious authorities benefit from integration of
 *   their texts; language communities benefit from access to classical
 *   resources; diaspora communities benefit from dual-track entry points. The
 *   reading represents a post-hoc synthesis rather than the historical
 *   revivalist position, which makes it vulnerable to the committer-axis
 *   ambiguity: is this a genuine coordination mechanism, or a rationalization
 *   of institutional power distribution among religious and secular elites?
 *
 * KEY AGENTS:
 *   - Hebrew Language Planners (organized/constrained): Institutional actors solving coordination problem of building living language while preserving authority continuity
 *   - Religious Authority Institutions (institutional/arbitrage): Synagogues, rabbinic bodies, liturgical authorities benefiting from integration into national language project
 *   - Native Speakers and Modern Hebrew Users (organized/constrained): Language community bearing modest coordination costs of dual-register system
 *   - Liturgical Conservators and Ultra-Orthodox Communities (moderate/constrained): Experience mixed coordination (preserved liturgical status) and modest extraction (language change imposed through curricula)
 *   - Jewish Diaspora Communities (powerful/mobile): Access Hebrew through either liturgical or modern pathways; experience coexistence as enabling optionality
 *   - Post-Revisionist Synthesists (organized/constrained): See hybrid coexistence as temporary scaffold enabling transition to unified living language
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional choices as immutable features of language revival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_coexistence_reading, 0.28).
domain_priors:suppression_score(hybrid_coexistence_reading, 0.35).
domain_priors:theater_ratio(hybrid_coexistence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_coexistence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(hybrid_coexistence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(hybrid_coexistence_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_coexistence_reading, rope).
narrative_ontology:human_readable(hybrid_coexistence_reading, "Hybrid Coexistence of Liturgical and Native Hebrew Language Forms").
narrative_ontology:topic_domain(hybrid_coexistence_reading, "sociolinguistics/religious_studies/nation_building").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_coexistence_reading, '8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91').
narrative_ontology:cs_created_at('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', '').
narrative_ontology:cs_kernel_codification('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', formalized).
narrative_ontology:cs_authority_grounding('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', distributed).
narrative_ontology:cs_kernel_id(hybrid_coexistence_reading, hebrew_living_language).
narrative_ontology:cs_reading_relation('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', foundational, liturgical_and_native_both_valid).
narrative_ontology:cs_axiom_status(liturgical_and_native_both_valid, holdable).
narrative_ontology:cs_axiom('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', foundational, dual_register_coordination_enables_transmission).
narrative_ontology:cs_axiom_status(dual_register_coordination_enables_transmission, holdable).
narrative_ontology:cs_reference_frame('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', unified_language_with_complementary_registers).
narrative_ontology:cs_drift_state('8a09dbcd-6c34-42a9-afeb-87bbe0c2cc91', contemporary_post_revisionist, gap(stable, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_coexistence_reading, hebrew_linguistic_community).
narrative_ontology:constraint_beneficiary(hybrid_coexistence_reading, jewish_religious_practice).
narrative_ontology:constraint_beneficiary(hybrid_coexistence_reading, israeli_national_identity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEBREW LANGUAGE PLANNERS (ROPE) — Organized agents (linguists, educators, Academy of the Hebrew Language) see hybrid coexistence as a coordination mechanism solving the real problem: how to build a living language while preserving liturgical/classical authority. Native speakers benefit from access to classical resources; language planners benefit from institutional legitimacy derived from continuity with liturgical tradition. Pure coordination with modest resource costs.
constraint_indexing:constraint_classification(hybrid_coexistence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS AUTHORITY (ROPE) — Synagogues, rabbinic institutions, and liturgical authorities benefit from the hybrid reading: their texts retain sacred status AND are integrated into living language instruction. No extraction — they gain legitimacy and transmission continuity. The constraint coordinates preservation with functionality.
constraint_indexing:constraint_classification(hybrid_coexistence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LITURGICAL CONSERVATORS (TANGLED ROPE) — Ultra-orthodox communities and classical language purists experience mixed coordination and asymmetric extraction. They benefit from institutional recognition of liturgical Hebrew's centrality, but bear the cost of language change they did not choose. Native colloquializations are imposed on their sacred texts through incorporation into public curricula. Some extraction but not pure snare — the conservators retain institutional power and can resist specific changes.
constraint_indexing:constraint_classification(hybrid_coexistence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DIASPORA COMMUNITIES (ROPE) — Global Jewish communities experience the hybrid reading as enabling: they can access Hebrew through either liturgical tradition (prayer, religious study) OR modern native-speaker norms (secular education, Israeli culture). The coexistence creates optionality rather than constraint. Multiple entry points into language transmission.
constraint_indexing:constraint_classification(hybrid_coexistence_reading, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-REVISIONIST SYNTHESIS (SCAFFOLD) — A subset of language planners and educational reformers see hybrid coexistence as a temporary scaffold: the dual-track system enables transmission during the transition from inherited religious literacy to native-speaker competence, but should ideally mature into a unified living language where classical resources are historical/textual rather than prescriptive. Theater ratio reflects the current performance of distinguishing domains (liturgical vs native) that may eventually merge.
constraint_indexing:constraint_classification(hybrid_coexistence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, any revived language must undergo this phase: classical/liturgical forms are the only available continuity anchor, but living languages require native-speaker generativity and drift. The coexistence is an immutable feature of language revival itself. However, this reading risks naturalizing the specific institutional choice (maintaining liturgical-native distinction in public curricula) as inevitable, when other revived languages have managed integration differently.
constraint_indexing:constraint_classification(hybrid_coexistence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_coexistence_reading_tests).
:- end_tests(hybrid_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The hybrid coexistence reading coordinates genuine competing claims: religious authorities require textual/liturgical continuity; language communities require native generativity and drift. These are not inherently zero-sum. The coordination mechanism distributes institutional recognition and curricular resources such that both claims are accommodated. The value is not zero because there ARE real costs: educators manage code-switching, language planners navigate dual legitimacy standards, liturgical texts are modified in educational contexts. But these costs reflect coordination overhead, not asymmetric extraction. If resource allocation between liturgical and native domains is actually asymmetric (favoring religious authority), extractiveness should be higher. Suppression (0.35): Moderate. Some barriers to exit exist: native speakers cannot fully discard classical forms (they appear in religious contexts, historical literature, formal registers); religious conservatives cannot fully resist language change (native speakers' usage shapes meaning). However, suppression is not coercive — speakers can choose register switching, communities can emphasize one domain over another. The theater ratio declining from 0.50 to 0.40 over 50 years reflects the maturation of the hybrid system: as younger generations grow up in dual-register environment, code-switching becomes naturalized rather than performative. The system is becoming more functional, less theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid coexistence reading produces maximal perspectival variance because it represents a post-hoc synthesis of opposing positions. Language planners and religious authorities see pure Rope (coordination). Liturgical conservators see Tangled Rope (their sacred texts are being modified). Diaspora communities see Rope (optionality). Post-revisionist advocates see Scaffold (temporary dual-track during transition). The analytical observer risks seeing Mountain (structural inevitability) but this may be a false summit — the dual-track system is an institutional choice made by Israeli nation-builders, not an immutable feature of language revival. This perspectival gap is diagnostic of the reading's nature: it claims to resolve opposing positions but may actually be preserving the power relations that generated the original conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for this reading differs from standard constraints because the beneficiaries and victims are not clearly asymmetric. Religious authorities benefit from institutional integration (derived d ~0.15, low extraction). Language communities benefit from access to resources (derived d ~0.35, moderate but shared extraction). Liturgical conservators bear modest costs but also gain institutional recognition (derived d ~0.45, moderate shared). The absence of clear victims is the distinguishing feature: all agents experience some mix of benefit and coordination cost. This is why the classification is Rope from most perspectives — pure coordination with no identifiable victim. The tangled_rope classification from the conservator perspective reflects their subordinate power position in the dual system: they must accommodate native-speaker language change while religious authority institutions maintain symbolic veto power. The directionality computation should yield lower chi for beneficiary perspectives (language planners, diaspora) and higher chi for constrained perspectives (conservators, native speakers), but all values remain below snare thresholds because the constraint contains genuine coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by refusing to choose between liturgical and native generation — it claims both are valid. However, this creates a new analytical danger: the reading may be rationalizing power distribution (religious authorities maintain institutional legitimacy; language planners maintain professional authority) as coordination. The measurement data supports the coordinate interpretation: extractiveness and theater_ratio both decline over time, suggesting the system becomes more functional and less performative. But this could also reflect ossification — as the system matures, the power distribution becomes naturalized and harder to question. The true mandatrophy test: are there any agents who would prefer a different institutional arrangement (pure native generation, pure liturgical preservation, or true integration where classical forms are historical context rather than prescriptive authority) but cannot advocate for change because the hybrid framing forecloses alternatives?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_native_integration_boundary,
    'Is the liturgical-native distinction a necessary structural feature of language revival, or a contingent institutional choice made during Israeli nation-building?',
    'Comparative analysis of other revived languages (Irish, Welsh, Icelandic) and their relationship to liturgical/classical forms. Do they maintain dual-track systems or integrate classical resources into living language without formal distinction?',
    'If necessary: hybrid coexistence is a genuine coordination mechanism (Rope from all perspectives). If contingent: the distinction may be extractive (religious authorities maintaining institutional power over language legitimacy) and the constraint should classify as Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_native_integration_boundary, empirical, 'Whether liturgical-native distinction is structural or contingent').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the hybrid coexistence reading logically foreclose either the pure liturgical preservation reading or the pure native generation reading, or do all three remain structurally viable positions?',
    'Doctrinal analysis: can a party committed to hybrid coexistence still hold the core premises of liturgical preservation (sacred texts must remain authoritative) and native generation (living language must develop organically)? Or does hybrid coexistence require rejecting one of those premises?',
    'If coexistence is logically foreclosed by either sibling: clarify which reading''s core premise the hybrid reading contradicts. If all three remain structurally viable: the reading_relations should be coexists_with, not forecloses. This affects how the sibling constraints should be authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether hybrid reading forecloses or coexists with sibling readings').

omega_variable(
    resource_allocation_asym,
    'Does the hybrid coexistence framework allocate institutional resources (curricula, funding, scholarly prestige) symmetrically between liturgical preservation and native generation, or does one receive disproportionate investment?',
    'Empirical audit of educational ministry budgets, Academy of the Hebrew Language institutional prioritization, university linguistics department research funding, and publishing emphasis. Quantify allocation ratio between liturgical textual scholarship and modern native-speaker linguistics.',
    'If symmetric: the constraint is pure coordination (Rope). If asymmetric: the apparent coexistence masks extraction (lithurgical authority benefits disproportionately; native-generation research is underfunded), suggesting Tangled Rope or Snare classification. This directly challenges the 0.28 extractiveness value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_asym, empirical, 'Resource allocation symmetry between liturgical and native domains').

omega_variable(
    committer_axis_natural_law_risk,
    'This reading was authored from the analytical perspective as a Mountain (structural inevitability). Is this perspective capturing the actual constraints of language revival, or is it naturalizing the specific institutional choices made by Israeli nation-builders (which could have been made differently)?',
    'Historical counterfactual: could Israeli Hebrew have been revived as a unified living language WITHOUT maintaining formal distinction between liturgical and native registers? What did early Zionist language planners reject, and why? Were those rejections based on unavoidable linguistic constraints or on political/religious commitments?',
    'If constraint is truly natural: Mountain classification is correct, and the sibling readings are perspectival variations on inevitable structure. If choice-based: the analytical mountain is a false summit — it naturalizes a contingent institutional arrangement. The actual constraint is Tangled Rope or Snare (religious authority benefits from maintaining the distinction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axis_natural_law_risk, conceptual, 'Whether analytical mountain perspective naturalizes or correctly identifies structural inevitability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_coexistence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybr_tr_t0, hybrid_coexistence_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(hybr_tr_t25, hybrid_coexistence_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(hybr_tr_t50, hybrid_coexistence_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(hybr_be_t0, hybrid_coexistence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hybr_be_t25, hybrid_coexistence_reading, base_extractiveness, 25, 0.29).
narrative_ontology:measurement(hybr_be_t50, hybrid_coexistence_reading, base_extractiveness, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_coexistence_reading, identity_coordination).
narrative_ontology:affects_constraint(hybrid_coexistence_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(hybrid_coexistence_reading, native_generation_reading).

% DUAL FORMULATION NOTE:
% The hybrid coexistence reading is one of three structurally distinct constraints derived from the hebrew_living_language kernel. The preservation reading (liturgical_preservation_reading) focuses on classical/sacred textual authority and has lower extractiveness (ε≈0.15, Mountain or Rope). The generation reading (native_generation_reading) focuses on organic modernization and native-speaker autonomy, with higher extractiveness reflecting resistance to religious authority constraints (ε≈0.55, Tangled Rope or Snare from conservator perspectives). The coexistence reading bridges these by claiming both are valid — with intermediate extractiveness reflecting coordination costs but absence of categorical victim. Each reading has its own ε value and its own perspectives reflecting how different institutional actors perceive the constraint from that reading's frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_coexistence_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
