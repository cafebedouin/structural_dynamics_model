% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Native-Generative Vitality Criterion for Hebrew Continuity
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The colloquial question 'is Hebrew alive?' decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   constraints — three readings of the hebrew_continuity kernel. This file
 *   instantiates the native_generative reading: the criterion that Hebrew
 *   lives only through native speaker intuition and daily generative use.
 *   Historically this reading was engineered into existence by the revival
 *   movement: native children were set as the production target of the whole
 *   enterprise, the lexicon was expanded by committee (later the Academy of
 *   the Hebrew Language) so the language could carry modern life, and a
 *   Sephardic-based pronunciation was standardized over the Ashkenazi
 *   liturgical inheritance. The standing arrangement under contest — and the
 *   fixed epsilon referent for this story — is that revivalist arrangement
 *   itself, assessed by this reading's own lights: a real coordination
 *   achievement (one fully functional vernacular for a polyglot immigrant
 *   society) that simultaneously transfers linguistic legitimacy and custody
 *   away from liturgical-textual seats, classifies liturgical-only
 *   communities as speakers of a dead language, and discounts diaspora
 *   competence. The sibling readings (liturgical_preservation,
 *   bridge_pidginized) are separate constraint files, not positions inside
 *   this one.
 *
 * KEY AGENTS:
 *   - - hebrew_revival_leadership: Agenda-setter (institutional/identity_locked) — defined the native-transmission target and frames rival modes as relics
 *   - - hebrew_language_academy: Agenda-setter and institutional beneficiary (institutional/identity_locked) — administers lexicon, pronunciation, and the vitality criterion; concentrated capturer of the transferred custody
 *   - - israeli_school_system: Agenda-setter (institutional/constrained) — produces native speakers cohort by cohort
 *   - - native_speaker_first_generation: Beneficiary with payer burden (powerless/trapped) — received the mother tongue; bore the discipline of Hebraization
 *   - - liturgical_only_communities: Primary payer (organized/identity_locked) — their recitation-based Hebrew is officially classified as dead
 *   - - rabbinic_textual_authorities: Payer (organized/identity_locked) — lost arbitration over the language to native intuition and the Academy
 *   - - diaspora_hebrew_learners: Payer (moderate/mobile) — competence discounted under the native criterion
 *   - - diaspora_hebrew_educators: Excluded voice (moderate/mobile) — produce real second-language competence; outside the organs that define vitality
 *   - - comparative_linguists: Analytical observer (analytical/analytical) — documents the structure and contests the revival's self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.52).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.42).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native-Generative Vitality Criterion for Hebrew Continuity").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '9af72fd2-8957-42e5-893b-65abf20b70e6').
narrative_ontology:cs_kernel_codification('9af72fd2-8957-42e5-893b-65abf20b70e6', distributed).
narrative_ontology:cs_authority_grounding('9af72fd2-8957-42e5-893b-65abf20b70e6', practice).
narrative_ontology:cs_interpretation_layer_present('9af72fd2-8957-42e5-893b-65abf20b70e6').
narrative_ontology:cs_reading_relation('9af72fd2-8957-42e5-893b-65abf20b70e6', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('9af72fd2-8957-42e5-893b-65abf20b70e6', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('9af72fd2-8957-42e5-893b-65abf20b70e6', foundational, native_acquisition_constitutes_life).
narrative_ontology:cs_axiom_status(native_acquisition_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('9af72fd2-8957-42e5-893b-65abf20b70e6', native_acquisition_constitutes_life, empirically_contingent).
narrative_ontology:cs_axiom('9af72fd2-8957-42e5-893b-65abf20b70e6', secondary, liturgical_recitation_insufficient_for_life).
narrative_ontology:cs_axiom_status(liturgical_recitation_insufficient_for_life, holdable).
narrative_ontology:cs_axiom_grounding('9af72fd2-8957-42e5-893b-65abf20b70e6', liturgical_recitation_insufficient_for_life, empirically_contingent).
narrative_ontology:cs_reference_frame('9af72fd2-8957-42e5-893b-65abf20b70e6', exclusive_native_generative_vitality).
narrative_ontology:cs_drift_state('9af72fd2-8957-42e5-893b-65abf20b70e6', post_consolidation_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9af72fd2-8957-42e5-893b-65abf20b70e6', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_revival_leadership).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_speaker_first_generation).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_school_system).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, rabbinic_textual_authorities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, native_speaker_first_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ideologues and institution-builders of the revival (Ben-Yehuda's circle and its successors) who set native child acquisition as the goal of the entire enterprise, campaigned for Hebrew-medium schooling, and framed rival modes of Hebrew use as relics. Their careers, reputations, and life meaning are bound to the project; leaving it would mean disavowing their life's work.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_revival_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% The statutory body that decides new words, standard pronunciation, and orthography, applying the principle that the spoken usage of native speakers is the measure of the language. It holds formal custody of the language and the authority to arbitrate correctness; its mandate and budget exist because the criterion assigns it that role.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, hebrew_language_academy, beneficiary).

% State and pre-state schools that produce native speakers by teaching every subject in Hebrew and socializing children into the standard. They convert the criterion into daily practice and certify its output — fluent young speakers — cohort after cohort; their curricula and staffing are organized around that conversion.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_school_system, agenda_setter,
    institutional, generational, constrained, national).

% Children raised in Hebrew-speaking homes and schools from the 1890s onward, who received a complete mother tongue — precisely what the criterion says constitutes the language's life — while bearing the discipline of Hebraization: punishment for childhood languages, distance from parents' tongues, and the expectation of carrying a national project in their speech. Exit from the language environment was not available to a child.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_speaker_first_generation, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, native_speaker_first_generation, payer).

% Traditional communities — the Old Yishuv in Jerusalem and observant diaspora communities — whose Hebrew exists as prayer, liturgy, and sacred-text study. Under the criterion their usage is classified as a dead language rather than a living one; the standardized pronunciation adopted by schools and broadcast media displaced their inherited pronunciations in public standing. Abandoning the recitational practice would mean abandoning the covenantal practice that constitutes their identity, so they continue their mode regardless of its official classification.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_communities, payer,
    organized, civilizational, identity_locked, global).

% Scholars and decisors whose mastery of the classical texts made them the language's custodians for centuries. The criterion transfers arbitration over correct Hebrew to native-speaker intuition and the Academy's decisions; their rulings on language now carry weight only inside their own communities, and the philological authority accumulated over generations no longer governs the public language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, rabbinic_textual_authorities, payer,
    organized, civilizational, identity_locked, global).

% Jews outside Israel who learn Hebrew for prayer, study, or connection, achieving reading fluency and liturgical competence without native acquisition. Under the criterion their competence is discounted — they are told they know a dead or deficient language — so their labor of learning yields less recognition than an Israeli child's unreflective fluency. Exit is available: many simply disengage from Hebrew altogether.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_learners, payer,
    moderate, biographical, mobile, continental).

% Teachers and professors of Hebrew in diaspora schools and universities who produce genuinely competent second-language speakers. They hold that communicative command attained by adults is a real form of the language's life, but they sit outside the organs that define vitality — the Academy, the national curriculum, the broadcast standard — and their objection registers nowhere official.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_educators, excluded,
    moderate, biographical, mobile, continental).

% Sociolinguists and historians of the language who document the revival's mechanics and contest parts of its self-description — some argue the spoken language is a hybrid shaped by European substrate languages rather than a pure continuation of the classical tongue. They analyze the structure without collecting from it or paying into it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of a multilingual immigrant population needing a single fully functional vernacular: fixes native child acquisition as the production target, commissions lexical expansion so the language covers modern domains, and standardizes one pronunciation for cross-community intelligibility.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and custodial authority over Hebrew from liturgical-textual transmission chains to the native speech community and its standard-setting organs; moves prestige and institutional resourcing toward the nativizing institutions; imposes a dead-language classification, and the prestige losses that follow it, on liturgical-only usage and diaspora-acquired competence.
% ABSENT_VOICES: Liturgical communities and rabbinic authorities objected loudly but held no seat in the organs that defined vitality — Academy composition, curriculum design, and broadcast standards were built without them. Diaspora Hebrew educators, who produce real second-language competence, likewise had no vote. Their objection — that recitation and study sustain a form of linguistic life the criterion refuses to count — registered only as resistance, never as input.
% DISAPPEARANCE_RATIONALE: Native transmission is now self-sustaining: millions speak Hebrew as a mother tongue and would continue regardless of the ideology. What rearranges is the legitimacy architecture — the Academy's mandate framing, the dead-language verdict on liturgical usage, the discount on diaspora competence, and the master-exemplar status this criterion supplies to revival movements for other threatened languages.
% FOUNDING_PROBLEM: In nineteenth-century Europe Hebrew functioned as a written and liturgical medium while Jewish everyday speech shifted to vernaculars; revivalists judged that without native child acquisition the language would die with emancipation, and set out to manufacture a native speech community.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical-community spokesmen and rabbinic authorities attest the founding fear was real — their own communities' shift to vernaculars is their evidence — while disputing the remedy's exclusivity. Historians of the language outside the Academy corroborate both the mortality crisis and the completion of the rescue. No corroborating source outside the beneficiary set attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.52 (end-state): the arrangement solves a genuine collective-action problem — a polyglot immigrant population needed one fully functional vernacular, and the native-child target plus lexical expansion delivered it — while transferring legitimacy, custody, and prestige away from identifiable seats (liturgical communities, rabbinic authorities, diaspora learners). Both halves are load-bearing, hence tangled_rope as the authored claim. Suppression 0.42 is the current, post-consolidation level: enforcement machinery (school discipline, Academy rulings, public ridicule of exilic Hebrew) matured to a mid-century peak and relaxed once native transmission became self-sustaining — the series below traces that arc. Theater_ratio 0.45: with the rescue accomplished, a growing share of activity around the criterion is commemorative and boundary-performative (revival anniversaries, purity disputes, authenticity policing) rather than rescue work. Accessibility_collapse 0.45: the criterion forecloses the legitimacy of rival modes, not their practice — liturgical recitation continues daily regardless, so alternatives remain partially accessible. Resistance 0.6: liturgical communities maintained counterpractice for a century, diaspora learners kept learning, and scholars contest the revival-from-death narrative itself. All three series run on one shared eight-point grid (0-140); the final values equal the base_properties scalars. Coalition note: the payer seats are organized communities with real coalition capacity — which is precisely why their counterpractice survived.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (Academy, school system, revival leadership) the arrangement presents as the successful completion of a rescue: a coordination achievement they administer and narrate. From the payer seats the same structure operates as a standing verdict — 'your Hebrew is dead, your competence is deficient' — enforced by curricula and broadcast standards, experienced as extraction. The first native generation straddles the divide: recipients of the gift the criterion celebrates and bearers of the discipline that produced it. The engine derives these per-seat classifications from the structural data; the divergence between the administrator's coordination-experience and the payer's extraction-experience is the perspectival content of this story.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: liturgical_only_communities and rabbinic_textual_authorities are identity_locked payers — their linguistic practice constitutes their religious identity, so exit is unthinkable and they sit near the full-target end. Diaspora_hebrew_learners are payers with mobile exit (many disengage), placing them elevated but short of full target. The Academy, revival leadership, and school system receive custody, mandate, and output respectively — beneficiary-end directionality, with the Academy the concentrated capturer (see gain_flow). Native_speaker_first_generation declares beneficiary with a payer secondary role and trapped exit: the derivation places them near the beneficiary pole, and the omega first_generation_dual_position records that the true value may sit higher. No directionality_overrides are authored: beneficiary/victim declarations plus exit modulation already produce the structural relationships, and the dual-position case is routed through the omega rather than a silent override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Hebrew was ceasing to be spoken and would die with modernization unless native transmission was engineered — is solved: millions of native speakers reproduce the language without ideological assistance. Founding_problem_status is therefore dead, while disappearance_verdict is world_rearranges (the legitimacy architecture — Academy mandate, the dead-language verdict, diaspora devaluation, the export exemplar for other revival movements — depends on the criterion even though the language no longer does). That dead-status/world-rearranges mismatch is the capture signal the battery is built to surface, and mandatrophy_resolved is declared accordingly. The classification prevents two symmetrical errors: reading the whole arrangement as pure extraction erases the real coordination achievement (a functioning vernacular for millions); reading it as pure coordination erases the standing verdict imposed on liturgical communities. Tangled_rope holds both halves; the rising theater_ratio traces the drift toward ceremonial maintenance without claiming the underlying function is gone — people still speak, and the Academy still coins words.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates only the native_generative reading of the hebrew_continuity kernel; what structural differences would the sibling readings (liturgical_preservation, bridge_pidginized) introduce if instantiated instead?',
    'Compare the three reading files'' victim sets, epsilon values, and enforcement structures; the disagreement is located in the criterion of linguistic life — native generative use versus ritual recitation versus contact-medium function.',
    'Under liturgical_preservation the victim set dissolves (no community is deemed dead) and epsilon falls toward coordination-floor levels; under bridge_pidginized the native-child requirement relaxes, diaspora competence is rehabilitated, and the Academy''s arbitration role shrinks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this constraint is one of three readings of the Hebrew-continuity kernel.').

omega_variable(
    death_classification_status,
    'Is the classification of liturgical-only Hebrew as a dead language an empirical finding about transmission capacity, or a rhetorical instrument of the exclusivity claim?',
    'Comparative vitality studies of recitation-based transmission: fluency ranges, productive versus receptive competence, and intergenerational transfer inside liturgical communities.',
    'If liturgical transmission sustains genuine productive competence, the victim-defining verdict weakens and epsilon falls; if it cannot regenerate a full vernacular, the criterion''s core claim is strengthened and the costs look more like accurate diagnosis than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(death_classification_status, empirical, 'Empirical basis of the dead-language verdict on liturgical Hebrew.').

omega_variable(
    first_generation_dual_position,
    'Were first-generation native children net beneficiaries of the revival or conscripted instruments of it — and does the derived directionality for that seat misdescribe their position?',
    'Educational-history archives and first-generation testimony on Hebraist schooling discipline, language punishment, and adult retrospection.',
    'If instrumentalization dominated, the beneficiary seat''s effective extraction rises above the derived value and the arrangement''s asymmetry deepens; if the gift dominated, the seat sits nearer the beneficiary pole and total extraction is lower than the payer seats report.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_generation_dual_position, empirical, 'Dual position of the first native-speaking generation.').

omega_variable(
    standardization_cost_attribution,
    'How much of the cost borne by liturgical communities reflects irreducible coordination cost of standardizing one pronunciation for mutual intelligibility, versus status politics that preferred the Sephardic-based standard and demoted the Ashkenazi liturgical inheritance?',
    'Phonological and historical analysis: whether functional intelligibility required a single standard at all, and whether the choice among existing pronunciations tracked prestige rather than function.',
    'Attribution to coordination cost lowers excess extraction toward the Boltzmann floor; attribution to status politics raises it and sharpens the payer-seat classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standardization_cost_attribution, conceptual, 'Decomposing standardization costs into coordination overhead versus prestige-driven displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_native_gen_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.1).
narrative_ontology:measurement(heb_native_gen_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.12).
narrative_ontology:measurement(heb_native_gen_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.18).
narrative_ontology:measurement(heb_native_gen_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.22).
narrative_ontology:measurement(heb_native_gen_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.28).
narrative_ontology:measurement(heb_native_gen_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.34).
narrative_ontology:measurement(heb_native_gen_tr_t120, hebrew_continuity__native_generative, theater_ratio, 120, 0.4).
narrative_ontology:measurement(heb_native_gen_tr_t140, hebrew_continuity__native_generative, theater_ratio, 140, 0.45).

% Extraction over time
narrative_ontology:measurement(heb_native_gen_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(heb_native_gen_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(heb_native_gen_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(heb_native_gen_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(heb_native_gen_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(heb_native_gen_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.56).
narrative_ontology:measurement(heb_native_gen_be_t120, hebrew_continuity__native_generative, base_extractiveness, 120, 0.54).
narrative_ontology:measurement(heb_native_gen_be_t140, hebrew_continuity__native_generative, base_extractiveness, 140, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(heb_native_gen_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(heb_native_gen_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(heb_native_gen_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(heb_native_gen_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(heb_native_gen_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(heb_native_gen_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(heb_native_gen_su_t120, hebrew_continuity__native_generative, suppression_requirement, 120, 0.46).
narrative_ontology:measurement(heb_native_gen_su_t140, hebrew_continuity__native_generative, suppression_requirement, 140, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Hebrew is a living language' decomposes into three structurally distinct claims with different epsilon values, per the epsilon-invariance principle. This story (native_generative) authors epsilon for the revivalist arrangement: moderate-high, because native-child engineering coordinates a real vernacular while stripping custody from liturgical seats. The sibling liturgical_preservation story authors epsilon for the recitation-transmission arrangement its proponents defend (low extraction, high identity value); the sibling bridge_pidginized story authors epsilon for the contact-medium arrangement of diaspora interaction. The upstream reading (this one) influences the siblings' operating environment: its institutional victory redefined liturgical usage as 'dead' and diaspora usage as 'deficient,' changing the legitimacy conditions under which the sibling readings are held — without eliminating them in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
