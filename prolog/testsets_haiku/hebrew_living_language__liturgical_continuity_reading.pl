% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Living Through Liturgical Continuity Across Diaspora
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical-continuity reading of the
 *   contested kernel 'Hebrew lives / is living.' The core claim: Hebrew
 *   remains a living language because unbroken communities across diaspora
 *   maintain it through daily prayer recitation, Sabbath services, and
 *   textual study (Torah, Talmud, rabbinic interpretation). The constraint
 *   solves a language-identity coordination problem: how diaspora communities
 *   keep a language continuous without native speakers. Authority grounds
 *   itself in lineage — unbroken transmission from rabbinic Judaism — and the
 *   tradition vindicates itself through successful transmission across
 *   millennia and geographies. Extractiveness is very low (0.12 at interval
 *   end) because participants are voluntary, receive identity and community
 *   membership in return, and the constraint does not concentrate resource
 *   flows. Suppression is minimal (0.08) because there is no victim
 *   population and no systemic coercion — communities maintain the practice
 *   because they choose to, not because they are forced. This reading
 *   coexists with and influences two sibling readings: the literary-revival
 *   reading (Hebrew lives through Haskalah and later written production) and
 *   the native-generation reading (Hebrew lives only when spoken natively and
 *   generatively). The three readings compete over what 'living' means but
 *   are not logically incompatible within the historical record — different
 *   eras and communities adopt different readings, and elements of all three
 *   persist in contemporary Hebrew ecology.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_communities: voluntary participants in recitation, study, textual interpretation; identity-locked to the practice; collectively set standards for pronunciation and meaning
 *   - hebrew_linguistic_tradition: the transmitted corpus (texts, norms, patterns); non-agent entity whose continuity is the constraint's object
 *   - rabbinic_authority_structure: institutional agenda-setter; authenticates texts, sets pronunciation standards, transmits lineage
 *   - linguistic_descriptivists: excluded from authority structure; would contest the 'living' claim by strict definitional criteria
 *   - secular_hebrew_writers & native_hebrew_speakers: observers instantiating alternative readings of the same kernel
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
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Living Through Liturgical Continuity Across Diaspora").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '56f06e5f-45c5-4da8-89e9-337187a57538').
narrative_ontology:cs_kernel_codification('56f06e5f-45c5-4da8-89e9-337187a57538', fixed_text).
narrative_ontology:cs_authority_grounding('56f06e5f-45c5-4da8-89e9-337187a57538', lineage).
narrative_ontology:cs_interpretation_layer_present('56f06e5f-45c5-4da8-89e9-337187a57538').
narrative_ontology:cs_reading_relation('56f06e5f-45c5-4da8-89e9-337187a57538', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_reading_relation('56f06e5f-45c5-4da8-89e9-337187a57538', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('56f06e5f-45c5-4da8-89e9-337187a57538', foundational, liturgical_recitation_constitutes_liveness).
narrative_ontology:cs_axiom_status(liturgical_recitation_constitutes_liveness, holdable).
narrative_ontology:cs_axiom_grounding('56f06e5f-45c5-4da8-89e9-337187a57538', liturgical_recitation_constitutes_liveness, conventional).
narrative_ontology:cs_axiom('56f06e5f-45c5-4da8-89e9-337187a57538', foundational, unbroken_textual_transmission_establishes_continuity).
narrative_ontology:cs_axiom_status(unbroken_textual_transmission_establishes_continuity, holdable).
narrative_ontology:cs_axiom_grounding('56f06e5f-45c5-4da8-89e9-337187a57538', unbroken_textual_transmission_establishes_continuity, deontological).
narrative_ontology:cs_reference_frame('56f06e5f-45c5-4da8-89e9-337187a57538', unbroken_diaspora_transmission).
narrative_ontology:cs_drift_state('56f06e5f-45c5-4da8-89e9-337187a57538', contemporary_pluralist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('56f06e5f-45c5-4da8-89e9-337187a57538', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, hebrew_linguistic_tradition).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, languages_persist_through_textual_practice).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_recitation_preserves_phonological_integrity).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, diaspora_identity_constitutes_language_maintenance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains Hebrew through daily prayer, Sabbath services, and textual study (Torah, Talmud, later rabbinic commentary). Practitioners view linguistic continuity as indivisible from religious identity and covenant obligation. The practice is voluntary but identity-fused: to abandon Hebrew recitation is to sever participation in the tradition itself. Communities deliberate on pronunciation standards, liturgical texts, and interpretation methods; this collective determination shapes what counts as 'living' Hebrew at any moment.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_diaspora_communities, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, jewish_diaspora_communities, agenda_setter).

% The transmitted corpus of liturgical texts, phonological norms, grammatical patterns, and interpretive methods. Not itself an actor, but the entity whose continuity is the constraint's object. The tradition persists because communities treat it as authoritative and worth maintaining across generations.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_linguistic_tradition, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__liturgical_continuity_reading, hebrew_linguistic_tradition).

% Modern linguists who dispute whether 'living language' can apply to speech not produced generatively by native-speaker communities without written reference. They would argue that liturgical recitation is learned, memorized performance, not spontaneous generative speech — and thus does not qualify as a 'living' language by strict definitional criteria. They are excluded from the constraint's authority structure, which rests on religious and historical claims, not linguistic science.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, linguistic_descriptivists, excluded,
    institutional, biographical, analytical, continental).

% Authors and intellectuals who produced Hebrew literature in the Haskalah and later movements, and in modern Israel. They operate as a separate reading of the same kernel: they ask whether Hebrew 'lives' through literary production and native daily speech rather than liturgical recitation. They are not excluded — they participate in the broader contest over what makes a language 'living' — but they represent a distinct structural claim about language continuity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_hebrew_writers, observer,
    institutional, biographical, mobile, regional).

% Contemporary daily speakers in Israel and diaspora who use Hebrew generatively without written reference. They instantiate a third reading: that Hebrew is 'living' because it is used generatively for daily communication. From their position, the liturgical reading is necessary but not sufficient for 'living' status — the reading treats liturgical continuity as the marker of living-ness, while the native-speaker reading treats generative daily speech as the marker.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, native_hebrew_speakers, observer,
    powerful, biographical, mobile, regional).

% Formal and informal bodies of rabbinic consensus (historical and contemporary) that authenticate and transmit liturgical texts, pronunciation standards, and hermeneutical methods. Sets the standard for what counts as 'correct' Hebrew recitation, adjudicates textual variants, and enforces continuity of transmission. Authority grounds itself in lineage — unbroken transmission from Sinai through Talmud through accumulated rabbinic commentary to the present.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a language community dispersed geographically and culturally maintains linguistic and textual identity across generations without native-speaker consensus on daily speech. The solution: anchor language practice in liturgical recitation and textual study, where the text IS the standard and communities learn to reproduce it faithfully. This coordinates pronunciation, grammar, and meaning across diaspora without requiring consensus on spontaneous usage.
% TRANSFER_FUNCTION: Transfers labor (study, memorization, teaching, recitation performance) and commitment (time, identity-fusion with the tradition) from practitioners to the maintenance of linguistic continuity. Reciprocally, the constraint transfers membership status, ritual standing, and cultural identity back to practitioners who maintain it. No wealth or resource extraction — the transfers are symbolic and relational.
% ABSENT_VOICES: Linguistic descriptivists who would define 'living' language strictly by generative native-speaker production are structurally excluded from the authority structure that grounds the constraint (rabbinic authentication and lineage). They would object that memorized liturgical recitation, however faithful, does not constitute a 'living' language by scientific definition. Their exclusion is both structural (they do not participate in rabbinic deliberation) and ideological (the constraint's authority structure rests on religious and historical premises, not linguistic criteria). Secular Hebrew speakers and literary revivalists are less fully excluded — they engage the contest over language livingness — but their reading (language lives through literature and/or daily speech) competes with the liturgical reading.
% DISAPPEARANCE_RATIONALE: If the liturgical-continuity constraint disappeared — if communities stopped treating recitation and study as the anchor of Hebrew livingness — the diaspora would lose a unified standard for transmission. Pronunciation would diverge, textual interpretation would fragment, and the claim that Hebrew is 'living' across diaspora would collapse into regional or ideological variants with no shared basis. The Hebrew language (as a unified, continuous tradition) would be reorganized around either native-speaker generation (if such communities existed and chose to claim it) or literary revival (if written production became the standard). The constraint's disappearance would not kill Hebrew entirely, but it would end the claim of unbroken liturgical continuity.
% FOUNDING_PROBLEM: The Jewish diaspora faced a language-identity crisis: scattered across empires and cultures, communities spoke the local vernacular but needed to maintain collective identity through shared text and ritual. Hebrew was the language of scripture and prayer, but no longer spoken natively in any diaspora population. The founding problem: how can a language remain 'living' — continuous, recoverable, identifiable — without native speakers or daily use? The solution: make the text itself the standard; organize communities to recite and study it faithfully, treating correct pronunciation and interpretation as marks of tradition continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the diaspora (Salo Baron, Yosef Hayim Yerushalmi) and historians of Hebrew (Benjamin Harshav, William Popper) attest the founding problem from outside the religious community: diaspora language-shift was a real crisis, and liturgical anchoring was a real solution that communities adopted. Linguistic descriptivists contest whether the solution actually solves it — they argue that a language without native-speaker continuity cannot be 'living' in any meaningful sense, liturgical maintenance notwithstanding. Secular Hebrew revivalists (Eliezer Ben-Yehuda, early Haskalah writers) attested from within the tradition but offered an alternative solution: Hebrew becomes living only through literary and later daily production. The founding problem is live in some sense (diaspora identity still requires language anchoring) and dead in another (the rise of native-speaker communities in Israel arguably supersedes the diaspora solution), making the status contested.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).

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
 *   Extractiveness is very low (0.12) because the constraint is fundamentally a coordination mechanism with no victim set. Practitioners voluntarily participate, receive cultural identity and community membership as benefit, and face no direct coercion. The constraint persists because communities find value in maintaining it, not because they are trapped or suppressed. The gradual drift from 0.08 to 0.13 and back to 0.12 over 2000 years models periods of intensified rabbinical standardization and literary expansion (mid-interval) balanced against the core continuity of the practice. Suppression is minimal (0.08) because there is no coercive enforcement machinery. Resistance is near-zero (0.04) because the constraint is not imposed from outside but internally generated and maintained by communities. Theater is very low (0.05) because the practice is not theatrical — it is a sincere, functional mechanism for transmission. The constraint achieves high accessibility-collapse (0.92) not through coercion but through the sheer difficulty of exiting identity-locked participation: to leave the practice is to sever one's claim to membership in the tradition. The one-shot example (platform commission) shows a Rope with higher extraction and higher theater; this constraint differs structurally: extraction is much lower, theater is much lower, and the mechanism is relational rather than economic.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora community's seat, the constraint is a solution to a real linguistic problem and a source of identity and continuity. From the rabbinic authority seat, the constraint is a success: transmission has held across centuries despite geographic dispersal. From the linguistic-descriptivist seat, the constraint fails on its own terms — recitation without native generation is not 'living' language. From the literary-revivalist seat, the constraint is insufficient — true livingness requires generative written production (Haskalah) or native speech (Ben-Yehuda). These perspectival gaps are structural, not observational. The engine computes each seat's experienced type from the authored metrics and power atoms. The diaspora community should compute as Rope (genuine coordination, low extraction, voluntary). The rabbinic authority should compute as Rope with possible tangled-rope coloring if enforcement tensions emerge (some communities resist standardization). The linguistic-descriptivists and literary revivalists should compute as observing an alternative reading, not disagreeing with the present one — their disagreement is about what 'living' means, not about whether this constraint exists and functions as described.
 *
 * DIRECTIONALITY LOGIC:
 *   The jewish_diaspora_communities are simultaneously beneficiary and agenda-setter, clustered near d=0.1 (full beneficiary end). They receive cultural continuity, identity membership, and the coordination benefit of shared linguistic standards. They are not targets of extraction — they are the agents who set and maintain the standards. The hebrew_linguistic_tradition is a non-agent beneficiary (the constraint preserves it). Linguistic_descriptivists are excluded but not targeted — they are kept outside the authority structure because they operate from different epistemic premises (scientific definition vs. historical/religious grounding). The constraint has no payer seat in the economic sense — there is no resource extraction. The cost is labor (study, memorization, teaching), which practitioners bear voluntarily as part of identity maintenance. This asymmetry (all beneficiaries, no victims, low extraction) is structurally coherent for a Rope: genuine coordination with minimal asymmetric extraction. The directionality for each seat is derived from the beneficiary/victim declaration (all beneficiaries, zero victims) plus the exit-options and power atoms: identity_locked exit on organized communities reduces directionality-at-target (they cannot easily leave), organized power reduces directionality-at-target (collective action can resist if they choose), and voluntary participation further drives d toward beneficiary. The rabbinic authority structure sits near moderate d (~0.3–0.4) despite the agenda-setter role because its power is legitimated by lineage, not institutional force, and it faces genuine internal contestation (different communities interpret standards differently; reform movements challenge it; secular alternatives emerge). No overrides are needed — the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora language identity without native speakers) was live at the constraint's origin and remains contested. The constraint was built to solve it, and it has functioned for two millennia. Mandatrophy would arise if the founding problem died but the constraint persisted as pure theater. The rise of native Hebrew-speaking communities in Israel (20th century onward) superficially resolves the founding problem — Hebrew is now spoken natively again. But the liturgical-continuity reading does not disappear; it is reframed. Contemporary diaspora communities still anchor identity in the same constraint, and Israeli communities value the liturgical reading as a link to diaspora and tradition even while they also claim the native-generation reading. The mandate has not died, but it has split: the constraint now functions both as a solution to the original diaspora problem AND as a marker of continuity and identity across the native-speaker threshold. This is not classic mandatrophy (function death + persistence) but rather mandate-splitting (one function bifurcates into two as the context changes). No declaration of mandatrophy_resolved is warranted — the constraint is not in zombie state. It is actively reinterpreted as context changes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_vs_liveness_boundary,
    'Does ''living language'' necessarily require generative daily speech by native speakers, or can a language remain ''living'' through continuous textual transmission and recitation by non-native communities?',
    'Linguistics discipline consensus on definitional standards. Contemporary examples: how linguists classify languages with no native speakers but active descriptive/revitalization study (e.g., Classical Sanskrit in Hindu contexts, Ecclesiastical Latin in Catholic liturgy). Do they get classified as ''living''?',
    'If ''living'' is defined strictly by generative native speech, this constraint''s claim fails and the native_generation_reading becomes mandatory. If ''living'' can include textual continuity without native production, this reading stands. If multiple readings are accepted as legitimate under different frameworks, all three readings coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_vs_liveness_boundary, conceptual, 'Boundary between linguistic liveness and historical-cultural continuity.').

omega_variable(
    phonological_preservation_vs_innovation,
    'When diaspora communities with different regional accents maintain Hebrew recitation, are they preserving a single ''living'' language or fragmenting it into regionally differentiated variants that can no longer claim unbroken continuity?',
    'Phonological analysis comparing Ashkenazi, Sephardic, Mizrahi, and other regional Hebrew pronunciation traditions. Do they remain mutually intelligible for liturgical purposes? Do deliberate standardization efforts (e.g., academic Hebrew, Israeli standard) successfully re-unify them, or do they acknowledge legitimate plurality?',
    'If regional variants diverge to mutual unintelligibility, the claim of ''unbroken continuity'' becomes questionable — the constraint would fragment into regional constraints, not one global one. If variants remain coordinated by shared textual standard and mutual comprehension, the unbroken-continuity claim holds. Contemporary pluralistic acceptance of multiple pronunciations might reframe the constraint from ''one living language'' to ''one living tradition with plural realizations.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonological_preservation_vs_innovation, empirical, 'Whether regional phonological variation preserves or fragments the linguistic continuity.').

omega_variable(
    identity_fusion_reversibility,
    'The constraint''s persistence depends partly on identity-locking: practitioners view Hebrew recitation as inseparable from Jewish identity and covenant obligation. What happens if this identity fusion breaks — if practitioners can maintain Jewish identity without Hebrew or Hebrew practice without religious identity?',
    'Historical trajectory of identity-fusion in secular contexts (Israeli native speakers who do not practice liturgy; diaspora Jews who maintain cultural identity through literature/language revival but not religious recitation). Measurement: what happens to liturgical participation rates when identity and religious obligation uncouple?',
    'If identity fusion is essential and breaks, the constraint loses its primary motivation and may degrade to theater or extinction. If the constraint can survive partial decoupling (some practitioners maintain it for cultural rather than religious reasons), it becomes more resilient but also less unified. Contemporary data suggests identity-fusion is weakening in some communities while remaining strong in others — this may be fragmenting the constraint into sub-constraints with different motivations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_fusion_reversibility, empirical, 'Whether identity-fusion with religious obligation is necessary for the constraint''s persistence.').

omega_variable(
    committer_structure__liturgical_vs_native_reading_contest,
    'Is the liturgical-continuity reading''s core claim (Hebrew lives through unbroken textual tradition across diaspora without native-speaker generation) logically foreclosed by the native-generation reading''s premise (Hebrew is living only when produced generatively by native speakers), or do the readings coexist under different definitional frames?',
    'Clarification of what ''living'' means in each reading''s framework. If ''living'' is defined as ''continuous and recoverable by community practice,'' the liturgical reading holds and is not foreclosed. If ''living'' is defined as ''generatively produced by native speakers,'' the native reading would foreclose the liturgical reading. The two readings can coexist if they are understood as applying to different phenomena (the language''s historical status vs. its contemporary state) or different reference frames (diaspora vs. Israel).',
    'If the readings foreclose each other, the kernel is genuinely contested and only one can be true. If they coexist, the kernel has legitimate plural readings, and this constraint story and the native_generation_reading story are both valid accounts of different aspects of the same historical process. The committer structure suggests coexistence: different communities and eras adopt different readings, and contemporary Hebrew ecology contains elements of all three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure__liturgical_vs_native_reading_contest, conceptual, 'Kernel-level contestation: whether liturgical continuity and native generation are competing or complementary readings of ''living Hebrew.''').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(hebr_tr_t0, projected).
narrative_ontology:measurement(hebr_tr_t250, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 250, 0.04).
narrative_ontology:measurement_basis(hebr_tr_t250, projected).
narrative_ontology:measurement(hebr_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.04).
narrative_ontology:measurement_basis(hebr_tr_t500, observed).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t1000, observed).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1500, 0.06).
narrative_ontology:measurement_basis(hebr_tr_t1500, observed).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(hebr_be_t0, projected).
narrative_ontology:measurement(hebr_be_t250, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 250, 0.1).
narrative_ontology:measurement_basis(hebr_be_t250, projected).
narrative_ontology:measurement(hebr_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.11).
narrative_ontology:measurement_basis(hebr_be_t500, observed).
narrative_ontology:measurement(hebr_be_t1000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement_basis(hebr_be_t1000, observed).
narrative_ontology:measurement(hebr_be_t1500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1500, 0.13).
narrative_ontology:measurement_basis(hebr_be_t1500, observed).
narrative_ontology:measurement(hebr_be_t2000, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement_basis(hebr_be_t2000, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__liturgical_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__liturgical_continuity_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The kernel 'hebrew_living_language' decomposes into three constraint readings with distinct ε values and structural properties. The liturgical_continuity_reading (this story) claims low extractiveness (~0.12) grounded in voluntary identity-locked participation and no victim set. It coexists with the literary_revival_reading (ε ~0.25, extractive bias in the selection of canonical authors and texts) and the native_generation_reading (ε ~0.35, competitive tension between native and liturgical communities). All three readings are instantiated separately; they link via network.affects_constraints because they share a kernel, dispute its meaning, and influence each other's legitimacy and resource allocation. The liturgical reading is the oldest and most established; the literary and native readings emerged as challengers and alternatives. See omegas for the structurally unavoidable reading-contestation logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
