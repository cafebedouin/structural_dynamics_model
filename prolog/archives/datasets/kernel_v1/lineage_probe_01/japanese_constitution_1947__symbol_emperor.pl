% ============================================================================
% CONSTRAINT STORY: japanese_constitution_1947__symbol_emperor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_japanese_constitution_1947__symbol_emperor, []).

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
 *   constraint_id: japanese_constitution_1947__symbol_emperor
 *   human_readable: Emperor as Constitutional Symbol (1947 Reading)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The 1947 Japanese Constitution represents one of history's most explicit
 *   acts of constitutional surgery: the relocation of sovereignty from the
 *   emperor to the people in a single text, accomplished without physical
 *   removal of the emperor himself. Article 1 states: 'The Emperor shall be
 *   the symbol of the State and of the unity of the people, deriving his
 *   position from the will of the people with whom resides sovereign power.'
 *   This reading instantiates the specific structural claim about what
 *   changed: imperial sovereignty doctrine (kokutai theology) was suppressed
 *   and replaced with popular sovereignty, while the institutional figure of
 *   the emperor was retained. The constraint exhibits all six DR types
 *   depending on the observer's relationship to the sovereignty shift.
 *   Kokutai theology experiences the arrangement as a snare—suppressed
 *   without refutation, trapped in the margins. Traditionalists are
 *   identity-locked, unable to exit a framework that now denies the imperial
 *   premise they depend upon. The occupational administration sees pure
 *   coordination—a lever for managing the transition. The democratic
 *   coalition sees tangled rope—they won sovereignty but at the cost of
 *   symbolic compromise. Constitutional formalism sees theater—the text's
 *   repeated invocation of imperial dignity masking the transfer of actual
 *   power. The analytical observer risks naturalizing the arrangement as
 *   inevitable, when it was a contingent political choice to preserve a
 *   symbol while suppressing a theology.
 *
 * KEY AGENTS:
 *   - Kokutai Theologians: Victims (powerless/trapped) — imperial sovereignty doctrine is suppressed without textual refutation; trapped at generational and civilizational horizons; cannot exit or voice the doctrine
 *   - Imperial Household and Traditionalist Factions: Mixed victims/participants (moderate/identity_locked) — benefit from the emperor's institutional survival but are victims of sovereignty suppression; identity-locked to the imperial institution; constrained by the constitutional text but able to participate in its ceremonial enforcement
 *   - Occupational Administration (GHQ and Collaborating Japanese Government): Primary beneficiary (institutional/arbitrage) — experiences the symbol-emperor arrangement as pure coordination enabling transition; arbitrage options available (could have dissolved the throne entirely, but calculated retention as lower-friction path)
 *   - Popular Sovereignty Movement and Democratic Coalition: Secondary beneficiary (organized/constrained) — wins the constitutional relocation of sovereignty (their goal) but constrained by the compromise: retaining the emperor limits ideological clarity and creates ambiguity about ultimate authority
 *   - Constitutional Formalists and Judges: Institutional participants (institutional/arbitrage) — experience the symbol role as primarily performative; arbitrage via interpretive evolution (they can expand or contract the emperor's role through case law without amending the text)
 *   - Analytical Observer: Detached analyst (analytical/analytical) — risks naturalizing the arrangement as inevitable structural necessity rather than contingent political choice; vulnerable to false summit reasoning (treating the constraint as a mountain when beneficiaries demonstrably exist)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(japanese_constitution_1947__symbol_emperor, 0.38).
domain_priors:suppression_score(japanese_constitution_1947__symbol_emperor, 0.62).
domain_priors:theater_ratio(japanese_constitution_1947__symbol_emperor, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(japanese_constitution_1947__symbol_emperor, extractiveness, 0.38).
narrative_ontology:constraint_metric(japanese_constitution_1947__symbol_emperor, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(japanese_constitution_1947__symbol_emperor, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(japanese_constitution_1947__symbol_emperor, tangled_rope).
narrative_ontology:human_readable(japanese_constitution_1947__symbol_emperor, "Emperor as Constitutional Symbol (1947 Reading)").
narrative_ontology:topic_domain(japanese_constitution_1947__symbol_emperor, "political/legal/constitutional").

domain_priors:requires_active_enforcement(japanese_constitution_1947__symbol_emperor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(japanese_constitution_1947__symbol_emperor, '76547fb5-af28-47ef-bd64-f9a600ebfafe').
narrative_ontology:cs_kernel_codification('76547fb5-af28-47ef-bd64-f9a600ebfafe', formalized).
narrative_ontology:cs_authority_grounding('76547fb5-af28-47ef-bd64-f9a600ebfafe', extraction).
narrative_ontology:cs_interpretation_layer_present('76547fb5-af28-47ef-bd64-f9a600ebfafe').
narrative_ontology:cs_reading_relation('76547fb5-af28-47ef-bd64-f9a600ebfafe', japanese_constitution_1947__article_9_renunciation, influences).
narrative_ontology:cs_reading_relation('76547fb5-af28-47ef-bd64-f9a600ebfafe', japanese_constitution_1947__ghq_drafting_imposition, coexists_with).
narrative_ontology:cs_reading_relation('76547fb5-af28-47ef-bd64-f9a600ebfafe', japanese_constitution_1947__rights_catalog_1947, influences).
narrative_ontology:cs_axiom('76547fb5-af28-47ef-bd64-f9a600ebfafe', foundational, popular_sovereignty_supreme).
narrative_ontology:cs_axiom_status(popular_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('76547fb5-af28-47ef-bd64-f9a600ebfafe', popular_sovereignty_supreme, deontological).
narrative_ontology:cs_axiom('76547fb5-af28-47ef-bd64-f9a600ebfafe', foundational, imperial_institution_structurally_compatible_with_democracy).
narrative_ontology:cs_axiom_status(imperial_institution_structurally_compatible_with_democracy, holdable).
narrative_ontology:cs_axiom_grounding('76547fb5-af28-47ef-bd64-f9a600ebfafe', imperial_institution_structurally_compatible_with_democracy, instrumental).
narrative_ontology:cs_reference_frame('76547fb5-af28-47ef-bd64-f9a600ebfafe', imperial_sovereignty_doctrine_displaced_popular_sovereignty_established).
narrative_ontology:cs_drift_state('76547fb5-af28-47ef-bd64-f9a600ebfafe', contemporary_post_cold_war_japan, gap(stable, minor, false)).
narrative_ontology:cs_created_at('76547fb5-af28-47ef-bd64-f9a600ebfafe', '').
narrative_ontology:cs_kernel_id(japanese_constitution_1947__symbol_emperor, japanese_constitution_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__symbol_emperor, popular_sovereignty_doctrine).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__symbol_emperor, occupational_administration).
narrative_ontology:constraint_victim(japanese_constitution_1947__symbol_emperor, imperial_sovereignty_theology).
narrative_ontology:constraint_victim(japanese_constitution_1947__symbol_emperor, kokutai_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KOKUTAI THEOLOGY (SNARE) — The imperial sovereignty doctrine (kokutai) is structurally displaced by Article 1 without refutation. The theology is trapped: forbidden from legitimate expression in institutional channels, yet cannot be defeated through argument (since the Constitution closes debate via fiat). Suppression is high and inescapable at generational time — the doctrine bears the cost of delegitimacy while lacking exit or voice.
constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMPERIAL HOUSEHOLD / TRADITIONALISTS (TANGLED ROPE) — Identity-locked to the emperor as a constitutive element of their political and cultural identity. They benefit from the symbol's retention (the emperor survives, albeit shorn of sovereignty) but are victims of the sovereignty suppression. Their exit options are cognitively constrained: abandoning the emperor would require a complete identity reconstruction. They participate in the constitutional system (enforcement of the symbol role) while being trapped within it.
constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: OCCUPATIONAL ADMINISTRATION (ROPE) — GHQ and the collaborating Japanese government experience the symbol-emperor arrangement as a pure coordination mechanism: retaining the emperor reduces resistance to occupation and enables governance without imperial theo-politics. The arrangement solves a collective action problem (how to legitimize a new order while preserving social continuity). No extraction is experienced from this perspective — the constraint coordinates a transition.
constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POPULAR SOVEREIGNTY MOVEMENT (TANGLED ROPE) — The democratic forces that advocated for the 1947 Constitution benefit from Article 1 (sovereignty relocated to the people), but are victims of the compromise: the emperor's symbolic retention prevents full ideological clarity and enables nationalist reversion. They are constrained by the postwar political settlement — pushing for full removal of the imperial symbol invites backlash and alliance fracture. The constraint coordinates their victory (popular sovereignty established) while extracting a cost (symbolic continuity that muddies the doctrine).
constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL FORMALISM (PITON) — From the vantage of constitutional law as a discipline, Article 1 is a performative text: it declares sovereignty relocated but leaves the emperor's actual role, dignity, and social power substantially intact. The text's authority derives from repeated invocation and ritual affirmation, not from any enforcement that changes the emperor's structural position. The emperor as symbol is largely theater — the constitutional machinery persists through legitimacy (doctrinal invocation) rather than through functional redesign of imperial authority.
constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the relationship between a symbolic head of state and actual sovereignty is presented as a structural necessity: you cannot eliminate the emperor without destabilizing the entire social fabric, so the 'natural' solution is to keep the symbol while relocating power. However, this naturalizes what is actually a contingent political choice to preserve the imperial institution while suppressing the doctrine that once grounded it. The engine will flag this as a false summit: beneficiaries exist (occupational administration, popular sovereignty advocates), indicating a constructed rather than necessary constraint.
constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(japanese_constitution_1947__symbol_emperor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(japanese_constitution_1947__symbol_emperor, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(japanese_constitution_1947__symbol_emperor, TR),
    TR >= 0.70.

:- end_tests(japanese_constitution_1947__symbol_emperor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The suppression of imperial sovereignty doctrine is real (extracting from kokutai theology), but the extraction is not maximal because the occupational administration's goal was coordination (enabling a transition acceptable to traditionalists), not pure domination. The democratic coalition benefits from sovereignty relocation, so they do not experience extraction from the constraint overall—though they do experience the cost of symbolic compromise. The metric reflects the mixed nature: genuine suppression of a doctrine (extraction from its adherents) but not extraction-driven governance. Theater ratio (0.68): Moderate-high. The emperor's 'symbol' role is substantially performative. The constitutional text, imperial ceremonies, and state protocols repeatedly invoke the emperor's dignity and role while the actual substance—the authority to make binding decisions—has been relocated to democratic institutions. The theater has risen slightly over the 75-year interval (0.55 → 0.72) because the symbolic role has become MORE performative as democratic practice has solidified and the emperor's real influence (never high) has diminished further. Suppression (0.62): Moderate-high. The kokutai theology is structurally suppressed—forbidden from institutional expression, delegitimized in educational and legal discourse, treated as a dangerous residue from the imperial period. However, suppression is not total: traditionalist voices persist, imperial rituals continue, and the emperor retains significant social dignity. The suppression requirement has actually declined slightly over the interval (0.70 → 0.62) as Japan's democratic settlement has stabilized and the threat from revival of imperial sovereignty doctrine has diminished.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects the fundamental ambiguity in the symbol-emperor arrangement: did Article 1 truly suppress imperial sovereignty doctrine, or merely hide it? Kokutai theology sees a snare (suppressed without refutation). Traditionalists see a tangled rope (compromise that preserves the emperor while constraining the doctrine). The occupational administration sees rope (successful coordination). The democratic coalition sees tangled rope (victory with symbolic cost). Constitutional formalism sees piton (performative text without real enforcement). The analytical observer risks seeing mountain (naturalizing the arrangement as inevitable). No single type is correct—the presheaf over the observation site reveals the structure. The constraint succeeds for occupational administrators and mostly succeeds for popular sovereignty advocates, but this success is predicated on suppressing a doctrine (kokutai theology) that remains logically coherent and epistemically available to traditionalist interpreters. The constraint is stable at the populational level—democratic practice has solidified—but fragile at the doctrinal level: if political conditions changed (military revival, national crisis, generational change in imperial household), the suppressed theology could re-emerge.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) values for each perspective are derived from the agent's structural relationship to the sovereignty shift. Kokutai theology is positioned as a pure victim (d ≈ 0.95): the doctrine bears full cost of delegitimization with no benefit and no exit. Traditionalists are identity-locked beneficiaries-and-victims (d ≈ 0.65): they benefit from the emperor's symbolic retention but are victims of sovereignty suppression; identity-lock constrains their ability to move away from the framework. The occupational administration is a pure beneficiary (d ≈ 0.10): they achieve their coordination goal with no cost and possess full arbitrage (could abandon the arrangement if it became inefficient). The democratic coalition is a constrained beneficiary-victim (d ≈ 0.50): they achieve sovereignty relocation (primary benefit) but constrained by the symbolic compromise (cost). Constitutional formalism is institutional-arbitrage beneficiary (d ≈ 0.15): formalists benefit from the text's stability and can interpret creatively within it. The analytical observer (d ≈ 0.72) is positioned to see the full structure but risks misclassifying it as a natural law. These d-values are not overridden in this story—they derive cleanly from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense (governance mechanism collapsing into pure extraction or pure theater). Rather, it demonstrates how mandatrophy can be *prevented* through committer choice: by retaining the emperor as a symbol (appeasing traditionalists) while relocating sovereignty to the people (satisfying democrats), the occupation succeeded in avoiding the mandatrophy that would have resulted from either full abolition of the throne (triggering nationalist resistance and guerrilla legitimacy for kokutai theology) or retention of imperial sovereignty (blocking the constitutional transition). The arrangement's stability over 75 years (theater rising but extractiveness and suppression declining) suggests the compression has held. However, the latent mandatrophy risk is real: if the suppression of kokutai theology ever breaks down—if the doctrine resurfaces as politically legitimate—the constraint would shift from tangled rope toward snare (suppression reasserts) or piton (the symbol becomes purely performative and the constitution requires rewriting to clarify what 'sovereignty of the people' means when the imperial institution persists).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbol_versus_sovereign_ambiguity,
    'Is the emperor a genuinely symbolic figure stripped of sovereign authority, or a sovereign power wearing symbolic garb?',
    'Historical analysis of imperial prerogative exercise post-1947: when conflicts emerge between the emperor''s preferences and democratic decisions, who prevails and with what institutional friction? Longitudinal tracking of imperial influence over constitutional interpretation.',
    'If genuinely symbolic: Article 1 is a successful constitutional relocation of sovereignty (Rope from most perspectives). If symbology masks residual sovereign authority: the constraint is still extractive (the emperor retains influence while the text denies it, enabling obscured power). Classification could shift to Tangled Rope or Snare depending on the magnitude of hidden prerogative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_versus_sovereign_ambiguity, empirical, 'Whether the emperor''s symbol role truly suppresses sovereign authority or merely conceals it').

omega_variable(
    kokutai_theology_foreclosure,
    'Does the 1947 Constitution logically foreclose the kokutai reading of imperial sovereignty, or merely suppress its institutional expression while leaving the doctrine nominally alive?',
    'Textual analysis: does Article 1 refute kokutai theology, or does it sidestep the theological claim by relocating sovereignty to the people while leaving the metaphysical question of the emperor''s essence unresolved? Can a committed kokutai theorist accept Article 1 while maintaining that the emperor''s true nature (as head of an eternal lineage) remains constitutionally intact?',
    'If logically foreclosed: the sibling reading (kokutai theology as doctrine) is structurally impossible within the 1947 framework. If merely suppressed: the doctrine remains a live alternative that could resurface if political conditions permit. This determines whether the reading_relations edge to kokutai theology should be forecloses or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kokutai_theology_foreclosure, conceptual, 'Whether Article 1 logically forecloses or merely suppresses imperial sovereignty theology').

omega_variable(
    occupational_imposition_versus_consensual_adoption,
    'To what extent does the symbol-emperor arrangement represent genuine Japanese political consensus versus imposed occupational framework?',
    'Historiography of 1947 drafting: degree of GHQ coercion vs. Japanese negotiating latitude; post-occupation political evolution (did the arrangement persist because of consensus or because of path dependence and institutional lock-in?); modern polling on imperial retention.',
    'If genuinely consensual: the beneficiary status of popular sovereignty advocates is accurate (they adopted what they believed was wise). If primarily imposed: the beneficiary designation masks occupational enforcement, shifting the reading toward a Snare or forced coordination. This affects the reading''s legitimacy grounding in cs_structure.axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupational_imposition_versus_consensual_adoption, empirical, 'Degree of consensual adoption vs. occupational imposition of the 1947 emperor-as-symbol reading').

omega_variable(
    identity_lock_mechanisms_in_traditionalists,
    'For traditionalist factions that retain commitment to imperial sovereignty doctrine despite the 1947 Constitution, is their continued adherence due to structural exit barriers (career penalties for dissent, legal prohibition) or to identity fusion (the emperor doctrine is constitutive of their political self)?',
    'Ethnographic and psychological analysis: can traditionalists articulate a version of themselves that abandons imperial sovereignty? Do they perceive the Constitution as a temporary constraint awaiting removal, or as having permanently altered what it means to be Japanese? Comparison with exit costs across different traditionalist subgroups (some may face higher material barriers, others may be purely identity-locked).',
    'If primarily identity-locked: the classification of this perspective should remain identity_locked (Tangled Rope). If primarily constrained by structural barriers: might shift to constrained (changing the classification and the d-value derived). This affects the directionality computation for traditionalist factions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanisms_in_traditionalists, empirical, 'Whether traditionalist commitment to imperial sovereignty is identity-based or structurally constrained').

omega_variable(
    sibling_reading_logical_relationships,
    'How do the logical structures of the four sibling readings relate? Does the symbol-emperor reading foreclose any of them, or do they coexist as different framings of the same constitutional text?',
    'Analytical reconstruction of each reading''s core premises: Article 9 reading focuses on war renunciation; GHQ imposition reading focuses on drafting authority and legitimacy; rights catalog reading focuses on substantive protections; symbol-emperor reading focuses on sovereignty relocation. Do these premises logically exclude each other within any single interpretive framework?',
    'This omega directly informs the cs_structure.reading_relations declarations. If symbol-emperor reading''s axiom about popular sovereignty logically excludes kokutai theology axioms, that edge is forecloses; if multiple factions hold both simultaneously in public discourse, edges are coexists_with; if symbol-emperor creates institutional conditions that alter Article 9 interpretation downstream, that edge is influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_relationships, conceptual, 'Logical relationships between symbol-emperor reading and its sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(japanese_constitution_1947__symbol_emperor, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jc1947_sym_theater_t0, japanese_constitution_1947__symbol_emperor, theater_ratio, 0, 0.55).
narrative_ontology:measurement(jc1947_sym_theater_t25, japanese_constitution_1947__symbol_emperor, theater_ratio, 25, 0.68).
narrative_ontology:measurement(jc1947_sym_theater_t75, japanese_constitution_1947__symbol_emperor, theater_ratio, 75, 0.72).

% Extraction over time
narrative_ontology:measurement(jc1947_sym_extract_t0, japanese_constitution_1947__symbol_emperor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jc1947_sym_extract_t25, japanese_constitution_1947__symbol_emperor, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(jc1947_sym_extract_t75, japanese_constitution_1947__symbol_emperor, base_extractiveness, 75, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(jc1947_sym_suppress_t0, japanese_constitution_1947__symbol_emperor, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jc1947_sym_suppress_t25, japanese_constitution_1947__symbol_emperor, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(jc1947_sym_suppress_t75, japanese_constitution_1947__symbol_emperor, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(japanese_constitution_1947__symbol_emperor, identity_coordination).
narrative_ontology:affects_constraint(japanese_constitution_1947__symbol_emperor, japanese_constitution_1947__article_9_renunciation).
narrative_ontology:affects_constraint(japanese_constitution_1947__symbol_emperor, japanese_constitution_1947__ghq_drafting_imposition).
narrative_ontology:affects_constraint(japanese_constitution_1947__symbol_emperor, japanese_constitution_1947__rights_catalog_1947).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the japanese_constitution_1947 kernel. The kernel is the 1947 Constitution text itself, which multiple readings interpret differently. The symbol_emperor reading focuses on the relocation of sovereignty from the emperor to the people via Article 1 and the suppression of kokutai theology. Sibling readings (article_9 on war renunciation, ghq_imposition on drafting legitimacy, rights_catalog on substantive protections) are separate constraints with their own ε values and perspectival structures. All four readings share the same interval (0-75 years post-1947) but decompose the Constitution's structural achievement differently. The network links show that this reading influences the others: the symbol-emperor reading establishes the framework (popular sovereignty) within which the other readings operate. Article 9 renunciation makes sense as the exercise of sovereign power by the people; rights catalog protections flow from relocated sovereignty; GHQ imposition is the process by which the reading was authoritatively established. No other constraint in the corpus is logically upstream of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
