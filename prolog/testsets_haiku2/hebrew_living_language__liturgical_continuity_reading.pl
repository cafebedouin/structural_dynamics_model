% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_liturgical_continuity, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Living Language via Unbroken Liturgical Continuity
 *   domain: historical_linguistics/commitment_systems
 *
 * SUMMARY:
 *   Hebrew is a language without a native daily-speech community for most of
 *   its diaspora history (70 CE–1948 CE). Yet it remained semantically and
 *   phonologically living through unbroken recitation of liturgical texts and
 *   continuous scholarly exegesis. This constraint story instantiates ONE
 *   READING of the contested kernel 'What makes Hebrew a living language?':
 *   the liturgical continuity reading holds that recitation + textual study
 *   across diaspora communities constitute a sufficient condition for
 *   linguistic aliveness, even without native daily speech. The reading is
 *   contestable because two sibling readings — the native_generation_reading
 *   (only living when native speakers exist) and the literary_revival_reading
 *   (living via written literary production) — deploy different criteria.
 *   This story authors the liturgical reading's own structural data: its
 *   claimed type (rope: coordination of communal practice), its low
 *   extractiveness (voluntary participation, no victim set), and its own
 *   uncertainty omega (what counts as a language's living status). The
 *   claim/metric independence rule applies: the reading claims rope because
 *   it frames the constraint as authentic coordination; the metrics describe
 *   low extractiveness and low suppression because participation is genuinely
 *   voluntary and no party bears asymmetric cost.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Primary coordinating seat — sustain liturgical recitation and textual transmission across centuries and continents as voluntary religious practice. Benefit from cultural continuity and linguistic coherence with canonical tradition.
 *   - liturgical_specialist_class: Agenda-setter and transmission authority (cantors, rabbis, scholars) — administer standards of correct liturgical performance and textual accuracy. Benefit from social authority and specialized knowledge valuation.
 *   - textual_religious_tradition: Non-agent entity (doctrine/corpus) vindicated by the constraint's operation — the tradition's claim to unbroken continuity depends on Hebrew remaining living through diaspora.
 *   - native_hebrew_speakers: Excluded from diaspora-liturgical narrative but central to the native_generation_reading — would argue liturgical recitation alone does not constitute linguistic aliveness without native generative speech.
 *   - modern_hebrew_revival_movement: Excluded but historically consequential — contests the sufficiency of liturgical continuity, advocates for native-speaker criterion.
 *   - secular_linguistic_community: Observer seat — measures whether liturgical competence satisfies structural definitions of language-aliveness (productive grammar, native-speaker baseline, novel-utterance generation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.12).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Living Language via Unbroken Liturgical Continuity").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '4cd68ebb-c357-4715-a668-3e11c1bf7d62').
narrative_ontology:cs_kernel_codification('4cd68ebb-c357-4715-a668-3e11c1bf7d62', distributed).
narrative_ontology:cs_authority_grounding('4cd68ebb-c357-4715-a668-3e11c1bf7d62', lineage).
narrative_ontology:cs_interpretation_layer_present('4cd68ebb-c357-4715-a668-3e11c1bf7d62').
narrative_ontology:cs_reading_relation('4cd68ebb-c357-4715-a668-3e11c1bf7d62', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cd68ebb-c357-4715-a668-3e11c1bf7d62', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('4cd68ebb-c357-4715-a668-3e11c1bf7d62', foundational, liturgical_recitation_preserves_linguistic_aliveness).
narrative_ontology:cs_axiom_status(liturgical_recitation_preserves_linguistic_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('4cd68ebb-c357-4715-a668-3e11c1bf7d62', liturgical_recitation_preserves_linguistic_aliveness, conventional).
narrative_ontology:cs_axiom('4cd68ebb-c357-4715-a668-3e11c1bf7d62', foundational, unbroken_transmission_constitutes_continuity).
narrative_ontology:cs_axiom_status(unbroken_transmission_constitutes_continuity, holdable).
narrative_ontology:cs_axiom_grounding('4cd68ebb-c357-4715-a668-3e11c1bf7d62', unbroken_transmission_constitutes_continuity, deontological).
narrative_ontology:cs_reference_frame('4cd68ebb-c357-4715-a668-3e11c1bf7d62', diaspora_continuity_framework).
narrative_ontology:cs_drift_state('4cd68ebb-c357-4715-a668-3e11c1bf7d62', post_israeli_statehood_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4cd68ebb-c357-4715-a668-3e11c1bf7d62', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, textual_religious_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_specialist_class).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_hebrew_preserves_generative_grammar).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, memorized_recitation_maintains_phonological_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain Hebrew as a living language through liturgical participation and textual study across centuries and continents without territorial sovereignty or daily-speech immersion. They sustain the constraint by continuing communal prayer, study circles, and transmission of liturgical competence to new generations. Participation is voluntary; the coordination benefit is cultural continuity and religious identity coherence.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% The legal and interpretive tradition whose authoritative texts are in Biblical and Mishnaic Hebrew. The constraint preserves the tradition's canonical language in living use; it validates the claim that the tradition has never been interrupted, only transformed.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, textual_religious_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__liturgical_continuity_reading, textual_religious_tradition).

% Cantors, scholars, rabbis, and transmission-guardians who specialize in Hebrew liturgical performance and textual exegesis. They administer the standards of correct pronunciation, cantillation, and textual accuracy across communities. Their authority derives from demonstrated mastery of the liturgical corpus and transmission lineage. They benefit from the constraint's persistence because their specialized knowledge commands social and religious authority.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, liturgical_specialist_class, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, liturgical_specialist_class, beneficiary).

% Modern native speakers of Hebrew (primarily in Israel post-1948) who use the language as daily vernacular speech. They would argue that a language is truly living only when native speakers produce generative, unrehearsed daily speech — that liturgical recitation, no matter how continuous, preserves a language in amber but does not constitute living use. They are absent from diaspora-centered liturgical narrative.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, native_hebrew_speakers, excluded,
    moderate, biographical, arbitrage, national).

% Linguists, historians of language, and academic observers who study language death and revival. They analyze whether the liturgical continuity reading satisfies structural definitions of a living language (generative grammar, productive morphosyntax, native-speaker competence) or preserves a language in a specialized register that cannot generate novel utterances outside scripted contexts.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_linguistic_community, observer,
    institutional, biographical, analytical, universal).

% 19th–20th century linguistic revivalists (Eliezer Ben-Yehuda and successors) who argue Hebrew's living status derives from deliberate standardization and native-speaker community formation, not passive liturgical preservation. They would contest that liturgical continuity is insufficient; they advocate for the native_generation_reading as the true criterion. Their movement succeeded in creating a native-speaker base but remains in structural tension with the liturgical continuity reading's sufficiency claim.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, modern_hebrew_revival_movement, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the canonical language of a textual religious tradition across diaspora contexts without territorial state or daily vernacular use. Solves the coordination problem of maintaining linguistic identity coherence across dispersed communities separated by geography, time, and vernacular languages — diaspora members can participate in the same liturgy, access the same texts, and maintain claim to the same linguistic tradition despite never meeting and speaking different languages at home.
% TRANSFER_FUNCTION: Moves the maintenance burden of Hebrew linguistic competence from the broader community to specialized liturgical practitioners (cantors, scholars, rabbis) and from daily conversational use to formalized ritual and textual study contexts. The constraint channels language-preservation effort into high-commitment sites (prayer, study) rather than distributing it across vernacular daily speech.
% ABSENT_VOICES: Native Hebrew speakers and linguistic revivalists argue that liturgical preservation without native generative competence does not constitute a living language — the definition used in the liturgical continuity reading excludes their criterion. Secular linguists observing from outside the tradition would ask whether memorized-recitation competence satisfies structural definitions of language-aliveness. The native_generation_reading (native speakers creating speech ex tempore) and literary_revival_reading (written generative competence without recitation) represent excluded alternative readings of what keeps Hebrew alive.
% DISAPPEARANCE_RATIONALE: If unbroken liturgical recitation ceased, diaspora communities would face immediate crisis in ritual practice, canonical-text access, and identity coherence across generations. The linguistic competence embedded in liturgical contexts (pronunciation, morphology, exegetical terminology) would attenuate within a generation or two of non-transmission. The textual tradition's claim to unbroken continuity would be empirically broken. However, the language itself would not disappear — it would revert to documented-text status (as it was pre-diaspora) and might be re-acquired later (as it was via the native-generation reading in modern Israel). The world would rearrange around that disruption: either renewed transmission or gradual loss of diaspora Hebrew competence.
% FOUNDING_PROBLEM: After destruction of the Temple and dispersal into diaspora (70 CE onwards), the Jewish people faced the challenge of maintaining linguistic and religious coherence without territorial sovereignty, shared vernacular, or centralized authority. Hebrew was the language of the tradition's canonical texts and liturgy, but no territory used it as daily speech. Communities needed a mechanism to preserve the language's living role in their religious and collective identity despite diaspora dispersion and adoption of local vernaculars.
% FOUNDING_PROBLEM_CORROBORATION: The liturgical continuity reading itself attests the founding problem — the diaspora's linguistic challenge is the reading's origin claim. However, secular historians of language and the native_generation_reading both contest whether the problem was solved by liturgical preservation alone: they argue the problem persisted (and remained unsolved) until the 19th–20th century native-speaker movement created actual living speech. The literary_revival_reading contests whether the problem required recitation-based solution — written literary production might have been sufficient. No external voice from before the Enlightenment independent of the religious tradition itself corroborates the reading's specific framing of the problem's adequacy.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 at interval end) because the constraint is grounded in voluntary religious practice, not enforced obligation. No party is coerced into participation; diaspora communities sustain liturgical Hebrew because the practice coheres with their religious identity and cultural continuity. Suppression is correspondingly minimal (0.08): the constraint persists through preference alignment and transmission, not through active coercion of dissenters. Theater is near-zero (0.05): the liturgical function is genuine — communities actually need the coordination mechanism to maintain linguistic and religious identity across diaspora. The metrics are flat across the interval because the liturgical continuity constraint has remained structurally stable for nearly two millennia — participation levels fluctuate with political and economic conditions, but the core coordination function does not degrade. Extraction rises slightly (0.08 to 0.12) only to reflect modest intensification of specialist-class gatekeeping in medieval Jewish communities, but never reaches levels that would trigger snare or tangled-rope classification. The measurement series captures the historical record: liturgical Hebrew has remained low-extractiveness coordination precisely because it was never imposed as extractive overhead — its persistence depends on communities' voluntary desire for religious and linguistic continuity.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora community perspective: the constraint is a successful coordination mechanism that solved the founding problem of maintaining linguistic identity across diaspora. From the native_generation_reading perspective: the constraint preserves a language in ritual and textual amber but does not constitute true linguistic aliveness — a language is living only when native speakers generate utterances ex tempore. From the secular linguistic observer perspective: the constraint satisfies some structural criteria for language persistence (phonological fidelity, morphosyntactic stability, continuous transmission) but may not satisfy others (generative grammar in unrehearsed contexts, novel-utterance formation outside scripted domains). These are not measurement gaps to reconcile; they are different readings of the same kernel, each valid within its own framing. The engine should compute different type classifications from different seats: a participant in liturgical practice might compute rope (genuine coordination), while a native speaker might compute piton (theatrical preservation with atrophied generative function), while a revivalist might compute scaffold (transitional preservation pending native-speaker revival). These divergences are not errors — they are the structural signature of a contested kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure is symmetric or near-symmetric from the community perspective: diaspora members are both participants in (and thus partly responsible for) the coordination practice and beneficiaries of its linguistic and religious coherence output. The liturgical specialist class sits asymmetrically higher on the extractiveness axis (d nearer 1.0) because their specialized knowledge commands authority and gatekeeping power, but the extraction remains modest because the gatekeeping is consensually valued (communities want accurate transmission and correct liturgical performance). No victim set exists because participation is voluntary and no excluded party bears systematic cost from the constraint's operation — excluded voices (native speakers, revivalists) contest the reading's framing of what constitutes linguistic aliveness, but they are not victimized by it. The constraint does not trap them; it simply operates under a different definition than they would advocate.
 *
 * MANDATROPHY ANALYSIS:
 *   The liturgical continuity reading faces a potential mandatrophy trap: if the founding problem (maintaining linguistic coherence across diaspora) is no longer live post-1948 (because Hebrew now has a native-speaker base in Israel), does the constraint persist by mandate or by inertia? The reading's own internal logic suggests the mandate may be dead: if languages truly live through native speech, then the advent of native Israeli Hebrew speakers means the founding problem is solved by the native_generation_reading, not by the liturgical reading. However, the liturgical constraint persists because diaspora communities choose to continue recitation and study independent of whether native speakers exist elsewhere. This is not mandatrophy in the classical sense (constraint outlives its function but persists through coercion): it is voluntary continuity of a practice whose instrumental justification has shifted. The omega variable on reading-relatedness addresses this: as native-speaker Hebrew grows, does the liturgical reading's claim to sufficiency weaken, or does the reading simply become one of multiple simultaneous readings of the kernel (a language is living because native speakers exist AND because diaspora communities maintain it liturgically)? The resolution depends on whether readings can coexist or must foreclose one another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_aliveness_criterion,
    'What structural criteria define a language as linguistically alive? Does the liturgical continuity reading satisfy them?',
    'Comparative linguistic analysis of language-aliveness metrics across three domains: (1) phonological fidelity and stability (tested via historical phonology); (2) productive generative grammar applied to novel contexts (tested via unseen sentences in liturgical and non-liturgical contexts); (3) native-speaker competence baseline (tested via speaker population and speech-community continuity). If liturgical practitioners can generate grammatically novel sentences within the liturgical register, the reading satisfies criterion 2 partially. If they cannot generate outside that register, criterion 2 is partially violated.',
    'If liturgical Hebrew satisfies productive generative criteria within its register, the constraint should compute as rope (genuine coordination with real linguistic function). If it satisfies only memorization and recitation criteria, the constraint should compute as piton (theatrical preservation of a language whose generative function has atrophied). If the native_generation_reading alone satisfies all criteria, the liturgical reading becomes a subordinate or obsolete reading of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_aliveness_criterion, empirical, 'Whether liturgical recitation without native generative speech constitutes linguistic aliveness.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the three readings of the hebrew_living_language kernel coexist as simultaneous valid perspectives, or does one reading foreclose the others?',
    'Historical and normative analysis of how different Jewish communities and linguistic scholars have held these readings simultaneously or exclusively. Can a scholar hold that both native-generation speech (Israeli Hebrew) AND liturgical continuity (diaspora Hebrew) constitute the language being alive, or does claiming one entail denying the other? Examine whether any authority structure has attempted to rank or prioritize the readings.',
    'If readings coexist: the constraint classification should account for multiple simultaneous valid readings (the engine may compute different types per reading). If readings foreclose: the rise of native-speaker Hebrew may functionally supersede the liturgical reading, and the constraint should compute as piton (inertial theatrical preservation of a reading no longer endorsed as sufficient). If readings influence but do not foreclose: the native-speaker success changes the legitimacy conditions of the liturgical reading without eliminating it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether readings of the kernel are mutually exclusive or coexistent.').

omega_variable(
    specialist_gatekeeping_extraction,
    'Does the authority of liturgical specialists (cantors, rabbis, scholars) in maintaining transmission standards constitute a form of low-level extraction or exploitative gatekeeping?',
    'Examine historical records of specialist compensation, community deference, and barriers to entry into the specialist class. If specialist status confers disproportionate authority or material benefit relative to the service rendered, extraction is higher. If the specialist role is honored but materially modest and accessible to motivated community members, extraction remains low.',
    'If specialist gatekeeping is exploitative, the constraint should be reclassified upward toward tangled_rope (coordination function + asymmetric extraction through authority). If gatekeeping is genuinely consensual and proportionate, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialist_gatekeeping_extraction, empirical, 'Whether liturgical specialist authority constitutes extractive gatekeeping.').

omega_variable(
    diaspora_vs_israel_mandate_drift,
    'Has the founding problem (maintaining linguistic coherence across diaspora) remained live, or has it shifted post-1948 with the establishment of Hebrew as a native language in Israel?',
    'Examine diaspora Jewish discourse post-1948: do communities continue to frame liturgical Hebrew preservation as necessary for their linguistic identity, or do they increasingly frame it as optional once Hebrew has native speakers elsewhere? Measure the stated justifications for continued liturgical practice: identity preservation vs. instrumental language maintenance.',
    'If the founding problem remains live (diaspora communities still need linguistic coherence), the constraint should remain rope. If the founding problem is dead (linguistic coherence is now provided by the existence of native speakers and Israeli Hebrew media), the constraint should compute as piton — it persists through inertia and identity preference, but the original mandate has been displaced. If contested (diaspora communities split on whether the mandate is still necessary), the verdict should be ''contested mandatrophy.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_vs_israel_mandate_drift, empirical, 'Whether the foundational diaspora-coherence mandate remains live post-native-speaker revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_liturgical_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(hebrew_liturgical_tr_t0, observed).
narrative_ontology:measurement(hebrew_liturgical_tr_t7, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 7, 0.03).
narrative_ontology:measurement_basis(hebrew_liturgical_tr_t7, observed).
narrative_ontology:measurement(hebrew_liturgical_tr_t14, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 14, 0.04).
narrative_ontology:measurement_basis(hebrew_liturgical_tr_t14, observed).
narrative_ontology:measurement(hebrew_liturgical_tr_t21, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 21, 0.05).
narrative_ontology:measurement_basis(hebrew_liturgical_tr_t21, observed).
narrative_ontology:measurement(hebrew_liturgical_tr_t28, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 28, 0.05).
narrative_ontology:measurement_basis(hebrew_liturgical_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(hebrew_liturgical_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(hebrew_liturgical_be_t0, observed).
narrative_ontology:measurement(hebrew_liturgical_be_t7, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 7, 0.09).
narrative_ontology:measurement_basis(hebrew_liturgical_be_t7, observed).
narrative_ontology:measurement(hebrew_liturgical_be_t14, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 14, 0.11).
narrative_ontology:measurement_basis(hebrew_liturgical_be_t14, observed).
narrative_ontology:measurement(hebrew_liturgical_be_t21, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 21, 0.12).
narrative_ontology:measurement_basis(hebrew_liturgical_be_t21, observed).
narrative_ontology:measurement(hebrew_liturgical_be_t28, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 28, 0.12).
narrative_ontology:measurement_basis(hebrew_liturgical_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_liturgical_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(hebrew_liturgical_su_t0, observed).
narrative_ontology:measurement(hebrew_liturgical_su_t7, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 7, 0.07).
narrative_ontology:measurement_basis(hebrew_liturgical_su_t7, observed).
narrative_ontology:measurement(hebrew_liturgical_su_t14, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 14, 0.08).
narrative_ontology:measurement_basis(hebrew_liturgical_su_t14, observed).
narrative_ontology:measurement(hebrew_liturgical_su_t21, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 21, 0.08).
narrative_ontology:measurement_basis(hebrew_liturgical_su_t21, observed).
narrative_ontology:measurement(hebrew_liturgical_su_t28, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 28, 0.08).
narrative_ontology:measurement_basis(hebrew_liturgical_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__liturgical_continuity_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct readings with different ε values and beneficiary/victim structures. The liturgical_continuity_reading (this story) maintains low ε and voluntary participation because it grounds linguistic aliveness in unbroken ritual transmission. The native_generation_reading grounds aliveness in native-speaker daily speech (likely higher ε for Israel-centric readings, lower for diaspora-inclusive readings). The literary_revival_reading grounds aliveness in written generative literary production (intermediate ε, different gatekeeping). Each reading has its own constraint story; they are linked via network.affects_constraints to enable kernel-contest analysis. The readings influence each other: the success of the native_generation_reading (creation of native Israeli Hebrew) changed the legitimacy conditions of the liturgical_continuity_reading (weakened the instrumental argument for diaspora maintenance), but did not foreclose it (diaspora communities continue to hold the liturgical reading as sufficient for their context). See cs_structure.reading_relations for formal relationship declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
