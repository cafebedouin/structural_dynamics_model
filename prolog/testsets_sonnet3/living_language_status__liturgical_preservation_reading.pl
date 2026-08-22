% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Liturgical Preservation Standard for Hebrew Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested 'living
 *   language status' kernel as applied to Hebrew: the claim that continuous
 *   liturgical recitation, study, and ritual use of the sacred corpus is
 *   SUFFICIENT to establish that a language is living, independent of
 *   vernacular transmission or literary productivity. This reading has
 *   structural stakes because it locates the authority to adjudicate Hebrew's
 *   vitality with rabbinical and yeshiva institutions, whose interpretive
 *   monopoly over the sacred texts is what the standard's proof condition
 *   actually tests. Two sibling readings — the native_generation_reading
 *   (vitality requires generational mother-tongue transmission) and the
 *   literary_continuity_reading (vitality requires productive new
 *   literary/intellectual work) — are NOT part of this story; they are
 *   separate constraints with their own ε, beneficiaries, and stakeholders,
 *   linked here only through network.affects_constraints and
 *   cs_structure.reading_relations. ε is authored low here because the
 *   coordination function — sustaining continuity around a fixed liturgical
 *   corpus across dispersed, non-vernacular communities — is real and
 *   requires modest overhead: no innovation, no demographic risk, and a
 *   well-understood transmission chain (yeshiva, synagogue, printed text).
 *   The extraction is not primarily material; it is definitional — the
 *   standard extracts recognition and interpretive authority away from
 *   vernacular speakers and literary revivalists who might otherwise contest
 *   what counts as linguistic life.
 *
 * KEY AGENTS:
 *   - rabbinical_authority: agenda_setter, sets and enforces the liturgical-sufficiency standard
 *   - yeshiva_institutions: beneficiary, institutional infrastructure whose activity the standard certifies
 *   - secular_speech_community: payer, vernacular Hebrew speakers delegitimized by the standard's proof condition
 *   - haskalah_literary_movement: payer, literary revivalists whose claim to vitality the standard renders irrelevant
 *   - orthodox_diaspora_communities: beneficiary/payer, validated by the standard but identity-bound to its exclusions
 *   - historical_linguists: analytical observer of the classification contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.28).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.52).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Preservation Standard for Hebrew Vitality").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '96846bcf-57f2-4a7a-90a5-a8307055f348').
narrative_ontology:cs_kernel_codification('96846bcf-57f2-4a7a-90a5-a8307055f348', fixed_text).
narrative_ontology:cs_authority_grounding('96846bcf-57f2-4a7a-90a5-a8307055f348', lineage).
narrative_ontology:cs_interpretation_layer_present('96846bcf-57f2-4a7a-90a5-a8307055f348').
narrative_ontology:cs_reading_relation('96846bcf-57f2-4a7a-90a5-a8307055f348', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('96846bcf-57f2-4a7a-90a5-a8307055f348', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('96846bcf-57f2-4a7a-90a5-a8307055f348', foundational, ritual_recitation_constitutes_sufficient_vitality).
narrative_ontology:cs_axiom_status(ritual_recitation_constitutes_sufficient_vitality, holdable).
narrative_ontology:cs_axiom_grounding('96846bcf-57f2-4a7a-90a5-a8307055f348', ritual_recitation_constitutes_sufficient_vitality, conventional).
narrative_ontology:cs_axiom('96846bcf-57f2-4a7a-90a5-a8307055f348', secondary, vernacular_use_is_orthogonal_to_sacred_status).
narrative_ontology:cs_axiom_status(vernacular_use_is_orthogonal_to_sacred_status, holdable).
narrative_ontology:cs_axiom_grounding('96846bcf-57f2-4a7a-90a5-a8307055f348', vernacular_use_is_orthogonal_to_sacred_status, conventional).
narrative_ontology:cs_reference_frame('96846bcf-57f2-4a7a-90a5-a8307055f348', diaspora_liturgical_continuity_sufficiency).
narrative_ontology:cs_drift_state('96846bcf-57f2-4a7a-90a5-a8307055f348', post_zionist_vernacular_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('96846bcf-57f2-4a7a-90a5-a8307055f348', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, haskalah_literary_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, orthodox_diaspora_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, orthodox_diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the standard by which Hebrew's status as 'living' is adjudicated within religious communities: continuous liturgical recitation, Talmudic study, and ritual use are treated as sufficient proof of vitality. This authority sets curricula, certifies textual transmission, and controls interpretive access to the sacred corpus. Because the standard locates linguistic life in ritual performance rather than daily speech, secular or literary claims to Hebrew's vitality can be waved off as irrelevant to the question the authority has defined. The authority's interpretive monopoly over the texts is preserved regardless of whether anyone speaks the language at a market or a dinner table.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Operate the study infrastructure — schools, seminaries, printed and now digital text networks — that constitutes the 'continuous recitation and study' the standard treats as decisive. Their institutional funding, prestige, and pedagogical authority depend on liturgical transmission being recognized as sufficient proof of a living language; a rival standard that requires native daily speech would not automatically devalue their function, but it would strip away the exclusive claim that their activity alone settles the vitality question.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    institutional, generational, constrained, global).

% Revived Hebrew as a spoken, generational mother tongue in Ottoman and later Mandate Palestine, then in Israel — children raised speaking Hebrew at home, in markets, in the army. Under the liturgical-preservation standard, this activity is not what makes Hebrew 'living'; the standard's proof condition is satisfied independently by ritual recitation, so vernacular speakers gain no additional recognition from the standard and can be framed by its proponents as having desacralized or 'profaned' a holy tongue by using it for commerce and daily life rather than prayer. They bear the cost of being read, within this reading's framework, as a threat to sanctity rather than as the language's most vivid evidence of life.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% Nineteenth-century Hebrew periodicals, novels, and intellectual writing (the Haskalah) sought to establish Hebrew as a living medium for modern literature and thought, independent of ritual use. Under this reading, that literary productivity is not the operative criterion — the sacred-text standard does not require new writing, only continuity of the old corpus — so the movement's central claim to have revived the language finds no purchase inside this reading's proof condition and can be dismissed by liturgical-standard proponents as a secular distraction from the real (ritual) basis of vitality.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, haskalah_literary_movement, payer,
    moderate, biographical, constrained, regional).

% Communities for whom Hebrew has never functioned as a vernacular but who recite, study, and transmit it liturgically across generations. The standard validates their relationship to the language as sufficient and complete, which is a genuine coordination benefit — it does not require them to adopt vernacular Hebrew to count as maintaining a living tradition. But their identity is now bound to a reading that treats vernacular revival as a lesser or even corrosive claim, foreclosing an easy accommodation with secular Hebrew speakers who share the same texts.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, orthodox_diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, orthodox_diaspora_communities, payer).

% Study the criteria by which languages are classified as living, dead, or revived, and note that the liturgical-preservation criterion has historically been used to argue Hebrew was never 'dead' at all, a claim with direct bearing on debates about whether the modern Hebrew revival was a resurrection or a continuous thread. They observe the standard's interpretive convenience for religious authority without adjudicating which reading is correct.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides religious communities a stable, low-overhead criterion for what counts as maintaining Hebrew: continuous liturgical recitation and study of a fixed sacred corpus, requiring no innovation, no vernacular fluency, and no institutional risk beyond transmission of existing texts.
% TRANSFER_FUNCTION: Moves interpretive and definitional authority over 'what counts as a living language' toward rabbinical and yeshiva institutions, and away from vernacular speakers and secular literary movements, who are recast as either irrelevant to or corrosive of the language's true (sacred) vitality.
% ABSENT_VOICES: Vernacular Hebrew speakers and Haskalah writers are not consulted in the liturgical standard's own terms — the standard's proof condition does not require or invite their testimony, so their claim to have revived the language is structurally outside the frame rather than argued against directly.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation criterion vanished, rabbinical and yeshiva institutions would lose their exclusive claim to defining Hebrew's vitality, but their study and recitation practices would almost certainly continue unchanged — the practice does not depend on the classificatory claim. Secular speakers and literary-continuity advocates would experience no material change either way, since the standard never granted them recognition to begin with. The contest is over the definitional adjudication itself, not over any practice's survival.
% FOUNDING_PROBLEM: In diaspora settings without territorial concentration or a shared vernacular, communities needed a way to sustain a claim that Hebrew remained a living, sacred tongue rather than a dead language like Latin — liturgical continuity offered proof that did not depend on demography, geography, or state support.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists and scholars of language revival (writing from outside rabbinical institutions) corroborate that liturgical continuity is a real and historically significant fact about Hebrew's transmission, distinguishing it from purely dead languages with no living use. However, these same outside observers do not corroborate the further claim that liturgical continuity alone settles the vitality question against vernacular or literary criteria — that adjudicative claim is attested primarily by the benefiting religious authorities themselves.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because the coordination function this reading serves — maintaining Hebrew's status as sacred/living without requiring vernacular risk — is genuinely low-overhead: the corpus is fixed, transmission is institutionally routinized, and no party need innovate or compete for resources to sustain it. Suppression is moderate (0.52) because the standard does real interpretive work suppressing rival claims: vernacular speech and literary innovation are not merely un-rewarded by this reading, they are actively recast as desecration or irrelevance, which requires sustained rhetorical and institutional effort (sermons, rulings, communal boundary-maintenance) to hold against the historically much louder claim of the Hebrew revival. Theater ratio rises modestly over the interval (0.20 to 0.35) as the standard's persistence increasingly depends on reasserting its sufficiency against an ascendant vernacular Hebrew (in Israel) that the wider world overwhelmingly treats as the language's living form — more of the standard's operation over time is defensive assertion rather than unchallenged coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority sits at the beneficiary end: it collects interpretive control and communal legitimacy from a standard it also administers, with global arbitrage-grade mobility across diaspora contexts. Yeshiva institutions and orthodox diaspora communities are beneficiaries whose recognition and continuity depend on the standard's sufficiency claim holding. Secular speech community and Haskalah literary movement are targets: the standard's proof condition does not merely fail to recognize their claim, it actively repositions their linguistic practice as profane or irrelevant, which is a real cost even though no material extraction changes hands — the cost is definitional exclusion and, historically, communal stigma.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proving Hebrew was not a dead language like Latin, using liturgical continuity as evidence, in a diaspora with no vernacular base — remains live for the communities the standard serves: continuous ritual transmission genuinely happens and genuinely matters to those communities' self-understanding. This is not mandatrophy in the classic sense (the coordination function has not obviously atrophied). What has shifted is that a second, independently successful vitality-claim (vernacular revival) now exists and is not accommodated by this reading's proof condition, producing contest rather than obsolescence. Classifying this as tangled_rope rather than snare or mountain avoids two errors: treating the liturgical community's genuine coordination need as pure extraction (it is not — the community benefit is real), and treating the definitional exclusion of vernacular and literary claims as natural or costless (it is not — it is an authored, enforced boundary with real victims of recognition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_versus_necessity_of_liturgical_use,
    'Is liturgical recitation genuinely SUFFICIENT for linguistic life (this reading''s claim), or is it evidence of preservation-as-artifact that a rival vernacular-based reading would classify as a dead-but-honored language, analogous to how a mummified body is not a living body regardless of ceremonial handling?',
    'No purely empirical resolution exists; the dispute is over the definition of ''living language'' itself. Partial resolution could come from comparative linguistics establishing whether other languages preserved solely through liturgy (Sanskrit, Ge''ez, Church Slavonic) are conventionally classified as living or dead, and whether that convention tracks liturgical continuity or vernacular use.',
    'If the comparative convention in linguistics tracks vernacular use rather than liturgical continuity (as it largely does for Sanskrit and Ge''ez, both usually classified as liturgical/classical rather than living), this reading''s proof condition would be revealed as an outlier definition serving institutional interests rather than tracking the field''s own standard usage — strengthening a false-summit reading of the beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_versus_necessity_of_liturgical_use, conceptual, 'Whether liturgical sufficiency is a defensible linguistic criterion or an institutionally convenient redefinition.').

omega_variable(
    kernel_framing_choice,
    'Given three declared readings of the living_language_status kernel (liturgical_preservation, native_generation, literary_continuity), is the liturgical reading''s low ε an artifact of choosing a narrow, easily-satisfied proof condition — i.e., does the reading''s sufficiency claim look coordination-light specifically BECAUSE it excludes the harder-to-satisfy vernacular and literary criteria from consideration?',
    'Compare the three readings'' ε values directly (each authored in its own sibling story): if native_generation_reading and literary_continuity_reading show substantially higher ε or different beneficiary/victim structures for the same underlying language, the divergence itself is the data — per the ε-invariance principle, this confirms three distinct constraints rather than one measured three ways.',
    'Confirms the decomposition decision: this story''s low ε is not evidence the liturgical standard is innocuous overall, only that ITS OWN narrow proof condition is low-overhead to satisfy. The kernel''s contested status is precisely that the three readings are not commensurable on a single ε scale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the low authored ε reflects genuine coordination-lightness or reflects a narrow proof condition chosen to avoid the vernacular/literary contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(livi_tr_t80, living_language_status__liturgical_preservation_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(livi_tr_t120, living_language_status__liturgical_preservation_reading, theater_ratio, 120, 0.31).
narrative_ontology:measurement(livi_tr_t160, living_language_status__liturgical_preservation_reading, theater_ratio, 160, 0.33).
narrative_ontology:measurement(livi_tr_t200, living_language_status__liturgical_preservation_reading, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(livi_be_t80, living_language_status__liturgical_preservation_reading, base_extractiveness, 80, 0.22).
narrative_ontology:measurement(livi_be_t120, living_language_status__liturgical_preservation_reading, base_extractiveness, 120, 0.26).
narrative_ontology:measurement(livi_be_t160, living_language_status__liturgical_preservation_reading, base_extractiveness, 160, 0.27).
narrative_ontology:measurement(livi_be_t200, living_language_status__liturgical_preservation_reading, base_extractiveness, 200, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(livi_su_t80, living_language_status__liturgical_preservation_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(livi_su_t120, living_language_status__liturgical_preservation_reading, suppression_requirement, 120, 0.48).
narrative_ontology:measurement(livi_su_t160, living_language_status__liturgical_preservation_reading, suppression_requirement, 160, 0.5).
narrative_ontology:measurement(livi_su_t200, living_language_status__liturgical_preservation_reading, suppression_requirement, 200, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the living_language_status kernel applied to Hebrew. liturgical_preservation_reading (this story) authors low ε and a tangled_rope classification: genuine coordination benefit for liturgical communities, with definitional extraction against vernacular speakers and literary revivalists. native_generation_reading (sibling) would author a structure where the liturgical standard is itself the extractive object — treating vernacular revival as the only legitimate vitality claim and liturgical-only preservation as institutional theater or 'linguistic embalming.' literary_continuity_reading (sibling) would center Haskalah-era literary productivity as the vitality criterion, with its own distinct beneficiary/victim structure (likely benefiting secular literary elites, at cost to both purely liturgical and purely vernacular claimants). The three readings are not the same constraint measured three ways — per the ε-invariance principle, each gets its own file, its own metrics, and its own stakeholders, linked here structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
