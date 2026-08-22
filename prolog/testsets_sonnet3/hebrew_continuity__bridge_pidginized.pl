% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact/Bridge Language (Bridge-Pidginized Reading)
 *   domain: sociolinguistics/religious_institutional
 *
 * SUMMARY:
 *   This story instantiates ONE of three contested readings of what it means
 *   for Hebrew to 'live' in the contemporary Jewish diaspora. This reading
 *   holds that Hebrew persists chiefly as a bridge/contact language: a
 *   partial, functional register used for event participation,
 *   cross-community brokering, and instrumental transactions (siddur
 *   navigation, Israel-trip logistics, ritual call-and-response) — neither
 *   the deep liturgical-textual competence of the preservationist reading nor
 *   the native generative fluency of the naturalist reading. This register is
 *   institutionally profitable: it never resolves into full competence, so
 *   pedagogy vendors, day schools, and communal brokers have a durable market
 *   in perpetually intermediate learners. The two sibling readings
 *   (liturgical_preservation, native_generative) each dismiss this register
 *   as 'not really Hebrew' — the former sees it as thin ritual mimicry
 *   without textual depth, the latter as pidgin degradation without native
 *   intuition — but neither sibling reading is authored here; they are
 *   separate constraints with their own ε and stakeholder structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.44).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.38).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.44).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact/Bridge Language (Bridge-Pidginized Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/religious_institutional").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '21218fbb-4668-4c1b-a322-772ab8d4378f').
narrative_ontology:cs_kernel_codification('21218fbb-4668-4c1b-a322-772ab8d4378f', distributed).
narrative_ontology:cs_authority_grounding('21218fbb-4668-4c1b-a322-772ab8d4378f', distributed).
narrative_ontology:cs_reading_relation('21218fbb-4668-4c1b-a322-772ab8d4378f', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('21218fbb-4668-4c1b-a322-772ab8d4378f', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('21218fbb-4668-4c1b-a322-772ab8d4378f', foundational, instrumental_functional_use_constitutes_language_life).
narrative_ontology:cs_axiom_status(instrumental_functional_use_constitutes_language_life, holdable).
narrative_ontology:cs_axiom_grounding('21218fbb-4668-4c1b-a322-772ab8d4378f', instrumental_functional_use_constitutes_language_life, conventional).
narrative_ontology:cs_axiom('21218fbb-4668-4c1b-a322-772ab8d4378f', secondary, partial_competence_is_not_deficient_relative_to_a_purer_standard).
narrative_ontology:cs_axiom_status(partial_competence_is_not_deficient_relative_to_a_purer_standard, holdable).
narrative_ontology:cs_axiom_grounding('21218fbb-4668-4c1b-a322-772ab8d4378f', partial_competence_is_not_deficient_relative_to_a_purer_standard, instrumental).
narrative_ontology:cs_reference_frame('21218fbb-4668-4c1b-a322-772ab8d4378f', post_dispersal_functional_contact_medium).
narrative_ontology:cs_drift_state('21218fbb-4668-4c1b-a322-772ab8d4378f', contemporary_diaspora_institutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21218fbb-4668-4c1b-a322-772ab8d4378f', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_pedagogy_industry).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, cross_community_brokers).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, heritage_learners).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, non_liturgical_diaspora_youth).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, israeli_hebrew_purists_excluded_from_defining_it).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, hebrew_as_living_instrumental_medium).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, diaspora_unity_through_shared_partial_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federations, day schools, and umbrella bodies set curricula and event norms that treat a working, partial, bridge-register Hebrew as sufficient communal glue. They benefit organizationally from a low bar that keeps dispersed communities transacting with each other without requiring fluency, and they administer the programs (birthright trips, camp Hebrew, siddur transliteration systems) that institutionalize this register as 'enough.'
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, diaspora_communal_organizations, beneficiary).

% Ulpan franchises, textbook publishers, and app developers monetize the perpetual incompleteness of the bridge register — learners never quite arrive at fluency, so there is always another course, another app tier, another workbook. Their business model depends on the gap between liturgical competence and native competence remaining permanently occupied by paying intermediate learners.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_pedagogy_industry, beneficiary,
    organized, biographical, mobile, global).

% Rabbis, cantors, and communal professionals who circulate between diaspora communities use bridge Hebrew as a professional credential and lingua franca; their employability depends on this register existing as the accepted standard rather than requiring either full liturgical mastery or native fluency.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, cross_community_brokers, beneficiary,
    moderate, biographical, mobile, continental).

% Diaspora Jews investing years of Hebrew school, bar/bat mitzvah prep, and adult ulpan classes typically plateau at the bridge register and are quietly told this is a complete, legitimate relationship to the language — while feeling, and sometimes being told by purists, that they never really learned Hebrew. They bear the cost of sustained partial competence: money, time, and a persistent sense of linguistic inadequacy with no clear path to resolution.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, heritage_learners, payer,
    powerless, biographical, constrained, national).

% Younger diaspora Jews with no strong liturgical formation and no access to native immersion inherit a fragmented Hebrew register through pop culture, social media, and occasional travel. They are structurally locked into the bridge register by geography and institutional design, unable to access either the liturgical depth of the older reading or the generative fluency of the native reading, yet are still expected to perform 'connection to Hebrew' at communal events.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, non_liturgical_diaspora_youth, payer,
    powerless, biographical, trapped, national).

% Native Israeli speakers and language-academy figures who consider bridge-register Hebrew a degraded pidgin have little institutional standing over diaspora Hebrew education; their objections are voiced but the diaspora institutions that administer this reading do not answer to them, since the diaspora's Hebrew economy is self-governing and does not require Israeli certification.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, israeli_hebrew_purists_excluded_from_defining_it, excluded,
    moderate, biographical, analytical, national).

% Document the bridge register as a genuine, stable contact-language phenomenon comparable to other diaspora lingua francas, neither romanticizing it as authentic continuity nor dismissing it as failure, providing the closest thing to outside corroboration of what the register actually does and does not accomplish.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, linguistic_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides diaspora Jews scattered across many countries, denominations, and levels of religious observance a shared, low-barrier medium — event Hebrew, prayer-adjacent vocabulary, marketplace and Israel-trip functional phrases — that lets communities coordinate ritual participation and cross-community contact without requiring either deep liturgical scholarship or native fluency neither of which is realistically attainable at scale in diaspora conditions.
% TRANSFER_FUNCTION: Moves tuition, program fees, and years of instructional time from heritage learners and diaspora families toward pedagogy vendors, day schools, and communal organizations, in exchange for a bridge competence that is institutionally declared sufficient but that the register's own occupants often experience as permanently incomplete.
% ABSENT_VOICES: Israeli Hebrew purists and native-speaker linguists who would object that this register is not really Hebrew have no institutional authority over diaspora Hebrew education and are not consulted in curriculum design; their absence lets the bridge reading's proponents declare adequacy without external linguistic accountability.
% DISAPPEARANCE_RATIONALE: If the bridge-pidginized register vanished overnight, diaspora communities would lose their primary shared linguistic medium for cross-community ritual and social coordination; day schools, ulpan programs, siddur design, and Israel-trip curricula would need complete restructuring around either pure liturgical Hebrew (inaccessible to most) or genuine native immersion (structurally unavailable outside Israel) — the current pedagogy and communal-brokering economy would collapse or radically retool.
% FOUNDING_PROBLEM: Post-emigration and post-Holocaust diaspora communities lost dense concentrations of fluent Hebrew speakers and needed SOME shared linguistic thread connecting geographically dispersed, religiously heterogeneous Jews to each other and to Israel, without requiring either seminary-level liturgical training or full linguistic assimilation into Israeli Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic anthropologists studying diaspora contact languages corroborate that the coordination need was real and remains partially live, but note the register has since become self-perpetuating as an institutional and commercial product independent of whether deeper Hebrew connection is actually being built; Israeli purists (an outside, non-benefiting party) attest the founding problem persists but argue the bridge register manages rather than solves it, while pedagogy vendors and communal organizations — the primary beneficiaries — are the ones asserting the problem is fully and adequately addressed.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.44) is moderate: real coordination value exists (diaspora communities do need a shared functional medium), but a substantial share of the tuition/time investment by heritage learners produces a permanently incomplete competence that primarily benefits the pedagogy industry's recurring revenue model and communal organizations' low administrative bar. Suppression (0.38) is present but not severe — heritage learners are not coerced into staying at this register, but institutional design (curricula that stop short of fluency, absence of native-immersion pathways in diaspora) channels most learners here regardless of preference. Theater ratio (0.41) reflects rising performative Hebrew usage at communal events (recitation without comprehension, transliterated call-and-response) that increasingly substitutes for genuine linguistic engagement. Accessibility collapse (0.32) is moderate-low because alternatives (intensive immersion, aliyah, dedicated liturgical study) remain visible and occasionally taken, they are just structurally harder to access from the diaspora. Resistance (0.55) is comparatively high because both sibling reading-communities actively contest this register's legitimacy, and it is a genealogy note.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal organizations and the pedagogy industry sit near the beneficiary end: they administer or profit from the register's perpetual-intermediate structure and face negligible cost from its incompleteness. Cross-community brokers benefit professionally from the register's institutional standing. Heritage learners and non-liturgical youth sit near the target end: they invest years and money and inherit a register that both sibling communities tell them is inadequate, with no institutionally offered path to resolve the inadequacy. Israeli purists are excluded rather than coordinated or extracted from — their objection carries no institutional weight over diaspora curricula, which is the structural feature that keeps this reading's authority self-contained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (diaspora communities needing SOME shared Hebrew medium post-dispersal) was real and partially remains live, which is why this is authored as tangled_rope rather than a pure snare: there is a genuine coordination function underneath the extraction. But the corroboration record shows an outside party (linguistic anthropologists, and the excluded Israeli purists) attesting that the register has become self-perpetuating as a commercial/institutional product independent of whether it still solves the original problem for most participants — the classic mandatrophy signature of an arrangement whose justification has drifted from its founding function toward its own maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_disagreement,
    'Is ''Hebrew lives'' a claim about textual/liturgical transmission fidelity, about native generative competence, or about functional contact-language sufficiency — and can these three readings even be adjudicated against a single shared standard of what counts as ''the language surviving''?',
    'No single empirical test resolves this; it is a conceptual dispute about what continuity criterion is being applied. Sociolinguistic surveys can describe usage patterns, but the disagreement is over which pattern counts as ''living'' rather than ''declined'' or ''pidginized.'' This is the exact structural location where the three sibling readings diverge.',
    'Under the liturgical_preservation reading, this bridge register is a symptom of decline (Hebrew reduced to incomprehensible recitation). Under the native_generative reading, this register is not Hebrew at all but a contact pidgin borrowing Hebrew vocabulary. Under this reading (bridge_pidginized), the same observed usage pattern is the primary and legitimate site of contemporary diaspora Hebrew life. The classification of the SAME empirical facts differs entirely depending on which reading is applied — this is the committer-axis disagreement, not a factual dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_disagreement, conceptual, 'The kernel-level disagreement: what counts as Hebrew ''living'' is a framing choice, not a fact about usage.').

omega_variable(
    instrumental_utility_vs_authenticity_erasure,
    'Does grounding Hebrew continuity in instrumental/functional utility (this reading''s core premise) legitimately capture a real form of language life, or does it launder a decline narrative by redefining ''living language'' downward to match what diaspora institutions can actually deliver?',
    'Compare against other documented contact/bridge languages (e.g., diaspora Yiddish revival efforts, heritage-language maintenance literature) to see whether bridge registers elsewhere are treated by linguists as genuine language life or as terminal-stage attrition markers.',
    'If bridge registers are typically classified by linguists as attrition markers rather than genuine continuity, this reading''s beneficiary institutions have an incentive to insist otherwise — supporting the tangled_rope classification''s asymmetric-extraction component. If bridge registers are typically treated as legitimate contact-language phenomena, the reading is more defensibly rope-like with lower true extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumental_utility_vs_authenticity_erasure, empirical, 'Whether defining continuity by instrumental utility is itself a beneficiary-serving redefinition.').

omega_variable(
    beneficiary_administered_adequacy_declaration,
    'Who gets to declare that bridge-register competence is ''enough'' Hebrew connection — and does the fact that this declaration is made almost exclusively by the institutions that profit from perpetual intermediate learners undermine its credibility?',
    'Track whether adequacy declarations (curricular endpoints, communal program design) are ever made by parties without a financial or administrative stake in maintaining the bridge register as terminal, versus always by pedagogy vendors and communal organizations.',
    'If adequacy is only ever declared by beneficiary institutions, this strengthens the tangled_rope reading (asymmetric extraction under a coordination cover). If independent or learner-driven bodies also affirm adequacy, the coordination function is more clearly primary and extraction more incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_administered_adequacy_declaration, empirical, 'Self-interested adequacy declarations as a marker of extraction versus genuine coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.22).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__bridge_pidginized, theater_ratio, 10, 0.26).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__bridge_pidginized, theater_ratio, 20, 0.3).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__bridge_pidginized, theater_ratio, 30, 0.33).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.36).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__bridge_pidginized, theater_ratio, 50, 0.39).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__bridge_pidginized, theater_ratio, 60, 0.41).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__bridge_pidginized, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__bridge_pidginized, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__bridge_pidginized, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__bridge_pidginized, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__bridge_pidginized, base_extractiveness, 60, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__bridge_pidginized, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__bridge_pidginized, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__bridge_pidginized, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__bridge_pidginized, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__bridge_pidginized, suppression_requirement, 50, 0.37).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__bridge_pidginized, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the hebrew_continuity kernel (bridge_pidginized, liturgical_preservation, native_generative). Each reading authors its own ε, beneficiary/victim structure, and classification per the ε-invariance principle: the underlying label 'is Hebrew alive in the diaspora' conflates three structurally distinct claims about what continuity consists in. This reading (bridge_pidginized) shows moderate extractiveness (0.44) driven by a perpetual-intermediate pedagogy economy; the sibling readings are expected to show different ε profiles reflecting their different beneficiary structures (liturgical institutions administering textual gatekeeping vs. native-fluency advocates contesting diaspora legitimacy claims). All three should be read as a family, not as competing measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
