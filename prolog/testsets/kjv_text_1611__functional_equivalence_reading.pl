% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: KJV Functional Equivalence: Multiple Valid Translations Coordinate Meaning While Extracting Authority
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   The King James Version of 1611 and its subsequent translations constitute
 *   a contested kernel: a stabilized commitment (the biblical text) that
 *   different traditions read differently. This constraint instantiates the
 *   FUNCTIONAL EQUIVALENCE reading—the claim that multiple translations serve
 *   complementary purposes and convey equivalent theological meaning—as
 *   opposed to the EXCLUSIVE INSPIRATION reading (which holds one text as
 *   uniquely authoritative) and the REVISABLE TRANSLATION reading (which
 *   treats translations as provisional approximations awaiting improvement).
 *   The functional equivalence reading solves a coordination problem: how to
 *   acknowledge the legitimacy of multiple translations while maintaining
 *   that there exists such a thing as 'the biblical message.' It does this by
 *   decoupling message from text—multiple texts can carry the same
 *   message—and by shifting authority from textual fidelity to functional
 *   adequacy. This is genuinely useful: it legitimizes ecumenical dialog,
 *   enables comparative theology, and allows communities to choose
 *   translations for different purposes (literary, devotional, academic). But
 *   it also extracts a cost: it displaces the question 'what does the text
 *   say?' with 'what does the text do?'—a question answerable only by
 *   interpretive authorities (scholars, clergy, theologians) who manage what
 *   counts as functionally equivalent. Lay readers without training cannot
 *   assess equivalence; they become dependent on expertise gatekeeping.
 *
 * KEY AGENTS:
 *   - Translation Scholars & Textual Critics: Primary beneficiaries (institutional/arbitrage) — extract professional authority by managing translation diversity and defining equivalence standards
 *   - Ecumenical Movement: Secondary beneficiary (institutional/arbitrage) — extracts legitimacy from functional equivalence framework that enables dialog across traditions
 *   - Lay Readers / Single Translation Adherents: Primary victims (powerless/trapped) — lack expertise to assess functional equivalence; depend on authority structures to validate their chosen translation
 *   - Textual Purity Tradition: Secondary victim (institutional/constrained) — doctrine of textual hierarchy undermined by functional equivalence logic; maintains performance of authority without logical foundation (piton perspective)
 *   - Open-Source Translation Movement: Organized agents (organized/constrained) — building alternative authority structures and distributed translation platforms that bypass scholarly gatekeeping; represent sunset mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent interpretive choice (what counts as 'equivalent') as an immutable feature of translation itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.38).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.42).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "KJV Functional Equivalence: Multiple Valid Translations Coordinate Meaning While Extracting Authority").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism/theology").

domain_priors:requires_active_enforcement(kjv_text_1611__functional_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, '2668a914-5f63-45bd-ac41-7c6767ef8e47').
narrative_ontology:cs_kernel_codification('2668a914-5f63-45bd-ac41-7c6767ef8e47', fixed_text).
narrative_ontology:cs_authority_grounding('2668a914-5f63-45bd-ac41-7c6767ef8e47', lineage).
narrative_ontology:cs_interpretation_layer_present('2668a914-5f63-45bd-ac41-7c6767ef8e47').
narrative_ontology:cs_reading_relation('2668a914-5f63-45bd-ac41-7c6767ef8e47', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('2668a914-5f63-45bd-ac41-7c6767ef8e47', kjv_text_1611__revisable_translation_reading, influences).
narrative_ontology:cs_axiom('2668a914-5f63-45bd-ac41-7c6767ef8e47', foundational, message_separable_from_text).
narrative_ontology:cs_axiom_status(message_separable_from_text, holdable).
narrative_ontology:cs_axiom_grounding('2668a914-5f63-45bd-ac41-7c6767ef8e47', message_separable_from_text, instrumental).
narrative_ontology:cs_axiom('2668a914-5f63-45bd-ac41-7c6767ef8e47', foundational, adequacy_measured_by_function_not_fidelity).
narrative_ontology:cs_axiom_status(adequacy_measured_by_function_not_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('2668a914-5f63-45bd-ac41-7c6767ef8e47', adequacy_measured_by_function_not_fidelity, conventional).
narrative_ontology:cs_reference_frame('2668a914-5f63-45bd-ac41-7c6767ef8e47', unified_theological_meaning_across_languages).
narrative_ontology:cs_drift_state('2668a914-5f63-45bd-ac41-7c6767ef8e47', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2668a914-5f63-45bd-ac41-7c6767ef8e47', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, translation_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, ecumenical_communities).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, academic_comparative_theology).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, textual_purity_traditions).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, lay_interpreters_without_expertise).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, single_translation_adherents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY READER / SINGLE TRANSLATION ADHERENT (SNARE) — Trapped by educational barriers and religious authority structures that present one translation as normative. Cannot access original languages or comparative translations without substantial training cost. Bears the extraction cost of interpretive monopoly: textual gatekeepers (scholars, clergy, translation committees) extract authority by controlling which text counts as 'the' word. No meaningful exit.
constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DENOMINATIONAL COMMUNITY (TANGLED ROPE) — Benefits from shared translation as identity marker and coordination mechanism (unified worship language, shared interpretive tradition), but constrained by leadership prerogatives around which translations are permitted in official settings. Experiences extraction through controlled access to translation diversity; also experiences genuine coordination benefit from standardized liturgical language. Can shift translations at community cost, not free but possible.
constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSLATION SCHOLARS / ECUMENICAL MOVEMENT (ROPE) — Net beneficiaries of the functional equivalence frame. Scholars extract professional authority by managing translation plurality; ecumenical bodies extract legitimacy by coordinating across traditions. The constraint solves their genuine coordination problem: how to acknowledge multiple valid readings while maintaining scholarly authority and institutional coherence. High agency; can navigate the translation ecosystem.
constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE BIBLE TRANSLATION MOVEMENT (SCAFFOLD) — Organized agents (Open English Bible, Wycliffe Bible Translators, YouVersion) are building parallel translation pathways with distributed authority and low gatekeeping. This represents a genuine sunset mechanism: as digital access to comparative translations increases and community translation projects mature (estimated 15-25 years), the extraction mechanism (scholarly monopoly on textual authority) loses force. Temporary coordination problem being solved by decentralized platforms.
constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TEXTUAL CRITICISM ESTABLISHMENT / PURITY TRADITION (PITON) — The doctrine of textual purity (one authoritative original, translations ranked by fidelity to it) persists through institutional inertia and performance rather than function. The functional equivalence reading has undermined the logical foundation of purity doctrine—if multiple translations validly convey meaning, the ranking collapses—but the authority structure maintains performance of textual hierarchy. Theater_ratio high because the work of comparative criticism is real, but the truth-claim about fidelity hierarchy has degraded.
constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HERMENEUTIC NATURAL LAW (MOUNTAIN) — From a universal/civilizational lens, the impossibility of perfect translation is a structural feature of language itself: no two languages have identical semantic fields, cultural referents, or phonetic/rhythmic properties. From this view, translation always involves choice and loss—functional equivalence is the only coherent linguistic framework. This appears as immutable law. HOWEVER: the structural data reveals this as a false summit. The 'constraints of translation' frame naturalizes what is actually a choice about which losses to emphasize—a choice controlled by translators and gatekeeping institutions, not by language itself.
constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_text_1611__functional_equivalence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, TR),
    TR >= 0.70.

:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The functional equivalence reading reduces extraction compared to the exclusive inspiration reading (which would have ε ≈ 0.65, with a single text monopoly on authority). However, it does not eliminate extraction—it displaces it from textual gatekeeping to hermeneutic/methodological gatekeeping. Scholars extract authority by defining equivalence standards and managing translation plurality. The reduction from 0.58 (at the emergence of ecumenical movement ~1920s) to 0.38 (contemporary period) reflects the maturation of digital access and open-source translation platforms—extraction mechanisms are weakening as institutional monopolies erode. Suppression (0.42): Moderate. Significant barriers include linguistic expertise requirements, training costs to access original languages, educational gatekeeping, and institutional authority claims. But suppression is declining (from 0.68 to 0.42 over the interval) as digital resources democratize access to comparative translations and linguistic tools. Theater ratio (0.55): Moderate-high. The scholarly work of establishing translation equivalence is genuine and intellectually rigorous, but contains performative elements: scholars maintain elaborate hierarchies of translation types (word-for-word, dynamic equivalence, paraphrase) that perform precision and control, and these hierarchies may exceed their actual descriptive power. Theater is rising (0.42 → 0.55) as the theory becomes more elaborate to defend against pressures from open-source and lay participation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental divide between beneficiaries and victims. Translation scholars and ecumenical bodies experience the functional equivalence frame as genuine coordination (rope perspective)—it enables comparative work and dialog. Lay readers experience it as extraction (snare perspective)—they are told their chosen translation is valid but must defer to expertise to understand why, and they cannot meaningfully assess competing claims. The textual purity tradition experiences degradation: the logical foundation of its rankings has collapsed, but it maintains performance of authority through inertia (piton perspective). The open-source movement experiences a temporary coordination problem being solved (scaffold perspective)—distributed translation platforms are rendering the scholarly monopoly obsolete. The analytical observer risks a false summit: treating the 'constraints of translation' as immutable law rather than recognizing them as the product of choices embedded in institutional authority structures.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation here follows from the structural relationship of each agent to the extraction flow. Beneficiaries (translation scholars, ecumenical bodies) with arbitrage options derive low d values; they can navigate the translation ecosystem, maintain authority, and extract professional legitimacy. Victims (lay readers) with trapped exit options derive high d values; they face insurmountable barriers to expertise and cannot exit single-translation dependence. The textual purity tradition, though institutionally powerful, is a victim because functional equivalence doctrine has eliminated the logical ground it stood on—it experiences a specific kind of extraction: the extraction of its justification. As digital access increases and open-source projects mature, the d values shift: lay readers gain constrained (not mobile, but less trapped) options; scholars' arbitrage shrinks as gatekeeping loses force; suppression_requirement declines.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    purity_doctrine_foundation,
    'Does the doctrine of textual purity (one authoritative original text, translations ranked by fidelity) have epistemologically sound grounding, or does it rest on a category mistake?',
    'Philosophical analysis of whether ''fidelity'' is a coherent concept across languages with incommensurable semantic fields; examination of whether purity doctrine is descriptive (how translation works) or prescriptive (how we want it to work)',
    'If unsound: the purity tradition is performative maintenance of an indefensible doctrine, and the constraint is pure extraction (snare) disguised as natural law. If sound: textual hierarchy has logical ground, and functional equivalence represents actual loss of information.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(purity_doctrine_foundation, conceptual, 'Whether textual purity doctrine has coherent epistemological grounding').

omega_variable(
    semantic_equivalence_definition,
    'What counts as ''functional equivalence'' in translation? How is adequacy defined without reference to a privileged original?',
    'Comparative study of translation evaluation criteria across languages; examination of whether functional equivalence collapses into reader-response relativism or maintains normative standards',
    'If no stable definition: functional equivalence reading dissolves into relativism, and extraction mechanism shifts from textual gatekeeping to hermeneutic authority (readers decide meaning). If definition holds: functional equivalence is coherent alternative framework, and extraction is genuinely reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_equivalence_definition, conceptual, 'Coherence and stability of ''functional equivalence'' as evaluative standard').

omega_variable(
    scholarly_authority_persistence,
    'Does the functional equivalence frame actually decentralize translation authority, or does it concentrate authority in translation scholars as arbiters of ''what counts as equivalent''?',
    'Institutional analysis of who controls translation commissioning, evaluation, and distribution; comparison of lay translator participation rates in open-source projects vs traditional scholarly translation workflows',
    'If authority remains concentrated: functional equivalence is a new form of extraction (scholars extract authority through methodological gatekeeping). If genuinely decentralized: extraction is substantially reduced and open-source pathway becomes real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarly_authority_persistence, empirical, 'Whether functional equivalence genuinely decentralizes translation authority or concentrates it differently').

omega_variable(
    exclusive_inspiration_incompatibility,
    'Are the functional equivalence reading and the exclusive inspiration reading (which holds one text as uniquely Spirit-guided) logically compatible within the same theological framework, or does accepting functional equivalence require abandoning exclusive inspiration?',
    'Systematic theological analysis of whether ''multiple translations convey equivalent meaning'' logically entails ''no single translation is uniquely authoritative''; examination of theological traditions that hold both views',
    'If incompatible: reading_relations should be ''forecloses''. If compatible: reading_relations should be ''coexists_with''. This determines whether adherents must choose between readings or can hold both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusive_inspiration_incompatibility, conceptual, 'Logical compatibility of functional equivalence with exclusive inspiration doctrine').

omega_variable(
    historical_emergence_contingency,
    'Did the functional equivalence frame emerge from genuine linguistic/hermeneutic insight, or from institutional pressures (ecumenical movement, Protestant diversification, translation proliferation) that required legitimizing multiple texts?',
    'Historical analysis of when and why the functional equivalence frame was adopted; examination of whether it would have emerged without institutional pressures; comparison with cultures where translation monopoly remains uncontested',
    'If contingent on institutional pressure: the frame is a constructed justification for extraction (scholarly authority over interpretation), and the omega reveals that functional equivalence is false summit discourse. If grounded in genuine insight: the frame reflects real hermeneutic truth independent of institutional interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_emergence_contingency, empirical, 'Whether functional equivalence frame emerged from insight or institutional necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__functional_equivalence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__functional_equivalence_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__functional_equivalence_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__functional_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, biblical_authority_structure_institutional).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, textual_purity_doctrine).

% DUAL FORMULATION NOTE:
% The KJV 1611 kernel decomposes into three distinct constraints corresponding to three distinct readings: exclusive_inspiration_reading (ε ≈ 0.65, snare from lay perspective, rope from institutional), functional_equivalence_reading (ε ≈ 0.38, this story), and revisable_translation_reading (ε ≈ 0.42, tangled rope across all perspectives). Each reading instantiates a different extraction mechanism and beneficiary/victim structure. The ε values differ by ~0.25 because they measure structurally distinct claims about textual authority. Network links show upstream/downstream dependencies: the exclusive_inspiration reading is upstream (it is what functional_equivalence reading responds to and partly forecloses); the functional_equivalence reading influences both alternative readings by constraining what moves are available in the interpretive space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__functional_equivalence_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
