% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: The Textualist Paradox: Universal Language Irreconcilable with Restricted Application
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The textualist paradox reading of 'all men are created equal'
 *   instantiates a specific critique of originalist constitutional authority:
 *   the Declaration and Constitution employ universally quantified language
 *   ('all men,' 'people,' 'citizen') while the political instantiation
 *   restricted these categories to a narrow subset (property holders, males,
 *   non-enslaved persons). This reading treats the gap between universal
 *   language and restricted application as exposing a performative
 *   contradiction in originalist methodology — if originalism claims to
 *   discover the fixed meaning of the text, it must confront that the text's
 *   universal language is irreconcilable with the legal apparatus's
 *   restricted application. The reading does not resolve the contradiction
 *   (that would be the universalist reading's task); it reveals the
 *   contradiction as structural. The constraint exhibits tangled_rope
 *   structure: the universal language coordinates political actors around
 *   abstract principle (coordination function) while the restricted
 *   application extracts legitimacy by allowing exclusion to appear
 *   constitutional (extraction function). Originalist jurisprudence requires
 *   the suppression of the contradiction — interpretive labor devoted to
 *   reconciling universal text with restricted practice — which is why the
 *   theater ratio is high and extractiveness increases over time as the
 *   contradiction becomes harder to conceal.
 *
 * KEY AGENTS:
 *   - Enslaved and Disenfranchised Populations: Primary victims (powerless/trapped) — subject to exclusion from the universal claim while bearing the constraint of legal subordination
 *   - Abolitionist and Reconstructionist Movements: Secondary agents (organized/constrained) — constrained by legal barriers but leverage the universal language as rhetorical and constitutional ground
 *   - Slaveholding Elite and Originalist Jurists: Primary beneficiaries (institutional/arbitrage) — benefit from the gap between universal language and restricted application; use originalist interpretation to justify exclusion
 *   - Constitutional Order / Federal State: Structural beneficiary (institutional/constrained) — the Constitution coordinates union and legitimacy through universal language while extracting power from those excluded
 *   - Originalist Interpretive Framework: Institutional actor (institutional/arbitrage) — the interpretive method persists through institutional inertia despite the paradox, requiring increasing theater to maintain authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the gap as inherent to language rather than recognizing it as a political choice to restrict universal principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.58).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.62).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "The Textualist Paradox: Universal Language Irreconcilable with Restricted Application").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '085e6e42-428a-469b-ac28-9d1d98157dec').
narrative_ontology:cs_kernel_codification('085e6e42-428a-469b-ac28-9d1d98157dec', formalized).
narrative_ontology:cs_authority_grounding('085e6e42-428a-469b-ac28-9d1d98157dec', extraction).
narrative_ontology:cs_interpretation_layer_present('085e6e42-428a-469b-ac28-9d1d98157dec').
narrative_ontology:cs_reading_relation('085e6e42-428a-469b-ac28-9d1d98157dec', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('085e6e42-428a-469b-ac28-9d1d98157dec', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('085e6e42-428a-469b-ac28-9d1d98157dec', foundational, universal_language_semantic_binding).
narrative_ontology:cs_axiom_status(universal_language_semantic_binding, holdable).
narrative_ontology:cs_axiom_grounding('085e6e42-428a-469b-ac28-9d1d98157dec', universal_language_semantic_binding, empirically_contingent).
narrative_ontology:cs_axiom('085e6e42-428a-469b-ac28-9d1d98157dec', foundational, originalist_coherence_failure).
narrative_ontology:cs_axiom_status(originalist_coherence_failure, holdable).
narrative_ontology:cs_axiom_grounding('085e6e42-428a-469b-ac28-9d1d98157dec', originalist_coherence_failure, deontological).
narrative_ontology:cs_reference_frame('085e6e42-428a-469b-ac28-9d1d98157dec', universal_semantic_principle).
narrative_ontology:cs_drift_state('085e6e42-428a-469b-ac28-9d1d98157dec', contemporary_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('085e6e42-428a-469b-ac28-9d1d98157dec', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, universal_language_coherence).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, constitutional_integrity_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED/DISENFRANCHISED (SNARE) — Trapped by the text's universal claim that is legally applied only to a defined subset. The constraint enforces maximum suppression: the constitutional language promises equality but the legal apparatus enforces exclusion. No exit options. Trapped in the gap between the text's reach and its restricted application.
constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ABOLITIONISTS/RECONSTRUCTIONISTS (TANGLED ROPE) — Constrained by legal barriers to reinterpreting the constitution; face suppression (arrests, violence) but also benefit from the universal language as rhetorical leverage. The constraint has a genuine coordination function (federal union, shared constitutional authority) alongside extraction (the universal language is selectively enforced). Constrained exit — can argue for universalist reading but cannot unilaterally impose it.
constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SLAVEHOLDING ELITE / ORIGINALIST JURISTS (ROPE) — Benefits from the constraint as pure coordination: the universal text combined with originalist interpretation (reading the intent of drafters) allows them to exclude enslaved persons while maintaining the appearance of constitutional fidelity. High arbitrage — they can shift between claiming the text is universally binding and claiming originalist intent was exclusionary. Net beneficiary.
constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL ORDER (TANGLED ROPE) — The federal system itself benefits from the universal language (legitimacy, unity, appeal to abstract principle) while being structurally constrained by the restriction to exclude non-property holders. The Constitution coordinates the union while extracting political power from those it explicitly excludes. Civilizational scope — the constraint operates at the structural level of the state itself.
constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALISM AS DOCTRINE (PITON) — The originalist interpretive method persists as the primary legitimation mechanism for constitutional authority, but the textualist paradox has exposed the framework as degraded: the 'original public meaning' or 'intent of framers' cannot coherently apply universal language to exclude most of the population without self-contradiction. The theater is high because originalism continues to be invoked despite the paradox; the constraint's function (lending false objectivity to restricted application) persists through institutional inertia.
constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, the gap between universal language and restricted application is an immutable feature of written constitutions: all founding documents use universal terms (rights, people, citizens) but must be instantiated in a specific political context. The tension is inherent to language itself — universal statements must be applied by a community of particular interpreters. However, the structural data reveals this as a false summit: the gap is not inherent to language but to the political choice to exclude certain groups from the community while claiming universal principles.
constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(all_men_created_equal__textualist_paradox_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, TR),
    TR >= 0.70.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint operates through the gap between universal language and restricted application. The original drafters and slaveholding elite extract political power and legitimacy from this gap — they can claim fidelity to universal constitutional principles while enforcing exclusion. The extraction is not maximal (not 0.70+) because the universal language also provides rhetorical leverage to abolitionists and later movements to challenge the restriction. Over the 100-year interval, extractiveness increases slightly (0.48 → 0.58) as the gap becomes harder to defend and requires more interpretive labor (theater) to maintain. Suppression (0.62): Moderate-high. Significant barriers to recognizing the contradiction include: (1) institutional investment in originalist jurisprudence as a legitimation mechanism, (2) political resistance to admitting the Constitution's founding incoherence, (3) epistemic closure within originalist communities that treat the question as already settled. Suppression decreases over time (0.75 → 0.62) because the contradiction becomes more visible and harder to suppress as historical consciousness improves and amendment history accumulates. Theater ratio (0.68): High. Originalist jurisprudence produces substantial performative content: statutory interpretation aimed at reconciling universal text with restricted practice, originalist scholarship devoted to demonstrating coherence, judicial opinions that deploy historical analysis to justify exclusion while using universal language. The theater ratio increases over time (0.55 → 0.68) as the contradiction becomes more obvious and requires more interpretive work to conceal.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival gap between the beneficiaries and victims. The slaveholding elite and originalist judges see the constraint as pure coordination (rope): the universal language, combined with originalist interpretation, solves the problem of maintaining constitutional legitimacy across a heterogeneous polity. The enslaved see pure extraction (snare): the universal language promises equality, the legal apparatus enforces exclusion, with no exit options. The abolitionists see mixed coordination and extraction (tangled_rope): the constraint both enables and constrains their movement. The Constitutional order sees itself coordinating the union (rope) while the paradox reading shows it extracting legitimacy through unequal application (tangled_rope). The originalist interpretive framework sees itself as objective meaning-discovery (rope) but the paradox reveals its function as performance maintenance (piton). The analytical observer risks seeing a natural law (mountain: universal language always exceeds restricted application) but structural analysis reveals a political choice (tangled_rope: the restriction is enforced, not inherent).
 *
 * DIRECTIONALITY LOGIC:
 *   The textualist paradox reading's directionality is determined by the agent's structural relationship to the contradiction itself. Agents who benefit from maintaining the gap (slaveholding elite, originalist jurists) experience low or negative d — they are beneficiaries of the performative contradiction. Agents trapped by the gap (enslaved persons, excluded populations) experience high d — they bear the full cost of the restriction. Abolitionist movements experience medium-to-high d — they are targets of suppression but also leverage the universal language, so they are partly beneficiaries of the contradiction (they can use the text against its restricters) and partly victims (they are suppressed for challenging it). The constraint's extractiveness is scaled by the agent's exit options: trapped agents with no exit experience maximum extraction; constrained agents with costly exit experience high extraction; beneficiaries with arbitrage options experience negative extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that the textualist paradox is a structural feature of how the originalist framework handles universal language applied to restricted contexts. The originalist method claims to discover fixed meaning from historical intent and text, but the textualist paradox shows that the text's universality is irreconcilable with the intent's restriction. The constraint is tangled_rope, not snare, because the universal language has a genuine coordination function (federal unity, constitutional legitimacy) alongside its extraction function (justifying exclusion). The paradox is not that originalism is incoherent (it can accommodate the restriction as the original intent) but that the accommodation requires suppressing the semantic universal reading of the text. The mandatrophy resolves when we recognize that the constraint serves both coordination (provides legitimacy) and extraction (provides cover for exclusion), and that the textualist reading reveals the extraction by foregrounding the irreconcilability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_coherence,
    'Did the founding generation intend ''all men'' to mean all humans, or was ''all men'' (of the propertied class) the coherent original intent?',
    'Textual analysis of founding-era writings, declarations, correspondence; comparison of universalist rhetoric vs. legal instantiation (property requirements, slavery clauses, women''s exclusion)',
    'If ''all humans'' intended: originalism is incoherent — the method reveals contradiction in its own authority source. If ''all propertied men'' intended: the paradox is not a paradox but a misreading of the text — but this requires admitting the universal language was rhetorical cover for restricted scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_coherence, empirical, 'Whether the founding generation''s intent was truly universal or restricted').

omega_variable(
    originalism_self_refutation,
    'Does the textualist paradox constitute a self-refuting critique of originalist methodology, or merely expose an incoherence that originalism can accommodate through stricter attention to original meaning?',
    'Theoretical test: originalism combined with the textualist paradox entails that the universal language must be applied universally OR the ''original meaning'' was restricted. Either horn requires abandoning the claim that originalism achieves objective meaning-discovery.',
    'If self-refuting: originalism lacks a coherent foundation and must be replaced by competing interpretive methods. If accommodable: the paradox is a spur to more careful originalist analysis but does not delegitimize the method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_self_refutation, conceptual, 'Whether the paradox is self-refuting or merely constraining for originalism').

omega_variable(
    performative_contradiction_grounding,
    'Is the performative contradiction (claiming universal principles while enforcing restricted application) a feature of the text itself, or a feature of how the text was deployed by political actors?',
    'Distinction between textual semantics (what the words say) and political pragmatics (what speakers did with the words). The text is universal; the deployment was restricted. Does the paradox belong to the text or to the history of its use?',
    'If textual feature: the Constitution contains an irresolvable contradiction that no interpretive method can escape. If pragmatic feature: different interpretive methods (originalism, living constitutionalism, etc.) can acknowledge the deployment without replicating its restriction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_contradiction_grounding, conceptual, 'Whether the paradox is textual or pragmatic in origin').

omega_variable(
    amendment_as_paradox_resolution,
    'Do the Fourteenth and Fifteenth Amendments resolve the textualist paradox by explicitly extending the universal principle, or do they confirm the original paradox by requiring textual amendment to achieve what the original text already claimed universally?',
    'Interpretive question: are amendments clarifications of the original universal meaning, or corrections of the original restricted application? If clarifications, the paradox was never real (the original text was always understood to allow later expansion). If corrections, the paradox was real and required constitutional amendment to resolve.',
    'If clarifications: the paradox is a hermeneutical artifact of modern reading, not a structural feature of the original text. If corrections: the amendments confirm that the original text left the paradox unresolved and required political intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_as_paradox_resolution, conceptual, 'Whether amendments clarify or correct the original paradox').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(textp_tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(textp_tr_t50, all_men_created_equal__textualist_paradox_reading, theater_ratio, 50, 0.65).
narrative_ontology:measurement(textp_tr_t100, all_men_created_equal__textualist_paradox_reading, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(textp_be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(textp_be_t50, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(textp_be_t100, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(textp_su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(textp_su_t50, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(textp_su_t100, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, constitutional_authority_legitimation).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, federalist_union_coherence).

% DUAL FORMULATION NOTE:
% The all_men_created_equal kernel decomposes into three structurally distinct constraint stories: (1) the originalist reading, which claims the restriction was the original intent and sees the text as internally coherent; (2) the universalist reading, which claims the text is inherently universal and amendments clarified the original meaning; (3) this textualist paradox reading, which treats the gap between universal text and restricted practice as exposing the incoherence of originalist methodology. Each reading has a different beneficiary/victim structure, different extractiveness, and different omega variables. They are linked by the shared kernel but represent incommensurable interpretive frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
