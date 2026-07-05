% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story addresses one specific structural claim within the broader
 *   colloquial concept of 'Hebrew as a living language': that Hebrew remains
 *   'living' by virtue of unbroken liturgical recitation and textual study
 *   maintained continuously across diaspora communities from antiquity to the
 *   present, absent a shared territory or continuous native daily-speech
 *   population. This is distinct from the claim that Hebrew's revival
 *   required native generative speech production (native_generation_reading)
 *   or from the claim that literary production during the Haskalah
 *   constituted the relevant continuity (literary_revival_reading). Each of
 *   these is a structurally distinct assertion with a different ε profile,
 *   different beneficiary/participant structure, and different empirical
 *   status, and each is authored as its own constraint story per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: primary beneficiary and practice-carrier (organized/mobile) — sustains the recitation and study tradition
 *   - rabbinic_scholars: agenda_setter and transmission authority (institutional/identity_locked) — sets standards for correct reading and interpretation
 *   - individual_worshippers: payer of time/effort investment, secondary beneficiary (moderate/mobile) — voluntary participants
 *   - native_hebrew_speakers_modern_israel: excluded from this specific constraint, subject of a sibling story (organized/analytical)
 *   - historical_linguists: analytical observer assessing continuity claims (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.12).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew as Living Language via Unbroken Liturgical Recitation and Textual Study").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, 'e8b0f8b4-3002-4dbf-b9f8-a95978c14768').
narrative_ontology:cs_kernel_codification('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', fixed_text).
narrative_ontology:cs_authority_grounding('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', lineage).
narrative_ontology:cs_interpretation_layer_present('e8b0f8b4-3002-4dbf-b9f8-a95978c14768').
narrative_ontology:cs_reading_relation('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', foundational, recitation_and_study_sufficient_for_linguistic_continuity).
narrative_ontology:cs_axiom_status(recitation_and_study_sufficient_for_linguistic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', recitation_and_study_sufficient_for_linguistic_continuity, conventional).
narrative_ontology:cs_axiom('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', secondary, generative_daily_speech_not_required_for_living_status).
narrative_ontology:cs_axiom_status(generative_daily_speech_not_required_for_living_status, holdable).
narrative_ontology:cs_axiom_grounding('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', generative_daily_speech_not_required_for_living_status, empirically_contingent).
narrative_ontology:cs_reference_frame('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', unbroken_diaspora_transmission_chain).
narrative_ontology:cs_drift_state('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', contemporary_post_haskalah_and_revival_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e8b0f8b4-3002-4dbf-b9f8-a95978c14768', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_reading_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, individual_worshippers).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, individual_worshippers).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, continuity_of_sacred_language_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain synagogue liturgy, Torah reading cycles, and textual study (Talmud, Mishnah commentary) in Hebrew across dispersed communities for roughly two millennia without a contiguous territory or native daily-speech base. Participation is voluntary communal practice; exit is available (assimilation, non-observance, switching primary liturgical language) without formal penalty, though social and identity costs of exit vary by community.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, civilizational, mobile, global).

% Transmit and interpret the textual corpus, train successive generations of readers and commentators, and set the standards for correct recitation and exegesis. Their professional and often personal identity is substantially constituted by fluency in this textual tradition; exit from the role is possible but costly to self-concept and community standing. They do not extract material rent from participants — teaching and interpretation are typically communally supported rather than coercively priced.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Invest time learning to read (not necessarily converse in) Hebrew liturgical text, attend services, and participate in study. The 'cost' is time and effort in acquiring a specialized reading competence; the benefit is continuity of communal identity, access to textual tradition, and participation in a millennia-spanning practice. No one compels attendance or study; those who decline face social rather than structural sanction.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, individual_worshippers, payer,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, individual_worshippers, beneficiary).

% Speak Hebrew generatively as a first, daily-use language following the Zionist revival — a structurally distinct achievement from liturgical recitation. They are not parties to this particular constraint (the liturgical-continuity claim), though their existence is sometimes invoked by advocates of the rival native_generation_reading to argue liturgical recitation alone does not constitute a 'living' language. They are named here for completeness; their claim is the subject of a sibling story, not this one.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, native_hebrew_speakers_modern_israel, excluded,
    organized, generational, analytical, national).

% Study whether unbroken recitation and study constitute genuine linguistic continuity ('living') or a different phenomenon (liturgical/ritual preservation, akin to Sanskrit or Church Latin). They assess phonological drift, reading-tradition divergence (Ashkenazi/Sephardi/Yemenite), and the textual corpus's uninterrupted transmission chain.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, historical_linguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed communities across centuries and continents around a shared textual and liturgical standard, enabling any literate member of any diaspora community to read the same core texts, participate in the same prayer cycle, and access the same interpretive tradition without a shared territory or state apparatus.
% TRANSFER_FUNCTION: Moves interpretive authority and correct-reading competence from teacher to student across generations; moves no material resource of consequence from any participant to any other — the principal transfer is time and attention invested by learners, received by no single extracting party but distributed across the community's own continuity.
% ABSENT_VOICES: Proponents of the native_generation_reading would object that liturgical recitation without generative daily speech is not 'living' in the linguistic sense — they are not excluded from society but are simply arguing a different constraint (a sibling story), not absent from this one. Within this reading, no group is structurally silenced; participation and departure are both unforced.
% DISAPPEARANCE_RATIONALE: If unbroken liturgical recitation and study vanished, diaspora communities would lose their primary continuous link to a common textual and interpretive tradition predating the modern Hebrew revival; communal identity practices, prayer, and inherited exegesis would need to be reconstructed from archival record rather than living transmission chain — a materially different situation than the practice's continued operation.
% FOUNDING_PROBLEM: Following exile and dispersion, communities needed a way to preserve access to sacred text, communal prayer, and legal-interpretive tradition without a shared territory, common vernacular, or centralized institution capable of enforcing continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists and comparative religion scholars outside the participating communities corroborate that the liturgical-recitation and textual-study chain is documented as continuous across the diaspora period (via manuscript tradition, cross-community textual comparison, and reading-tradition analysis), independent of any claim advanced by the practicing communities themselves about the practice's value.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.12, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.12) because the practice's principal transfer is time and attention among voluntary participants, with no concentrated party capturing value from others' investment — the rabbinic transmission function is communally supported, not rent-extracting. Suppression is low (0.08) because exit (non-observance, assimilation, adoption of vernacular liturgy) has historically been available, if socially costly, rather than structurally barred. Accessibility collapse is moderate-low (0.25): alternatives to liturgical Hebrew (vernacular prayer, translation) have existed and been adopted by various movements (e.g., Reform liturgy) without the practice collapsing entirely — this is not a mountain-grade collapse. Resistance is low (0.15), consistent with a genuine, mostly voluntary coordination function rather than a defended extraction. Theater ratio rises modestly (0.15 to 0.22) reflecting increasing performative/ceremonial elements in some communities as generative Hebrew competence outside Israel has declined relative to historical peaks, but this remains a minor drift, not a dominant pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and individual worshippers are declared beneficiaries because the constraint subsidizes their continuity of identity and access to tradition at low forced cost; the low derived directionality reflects genuine voluntary participation with real exit options (mobile). Rabbinic scholars sit closer to the agenda-setting seat but are identity-locked rather than extracting — their entrenchment is professional/relational, not rent-collecting, which is why no victim group is declared. This reading deliberately authors NO victims: unlike constraints where continuity is coercively maintained, this reading's structural claim is that the practice persisted through voluntary, distributed participation without institutional coercion — a different structural profile than a state-enforced language mandate would have.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving textual/liturgical access absent territorial continuity) remains live rather than dead — diaspora dispersion continues to be a real condition many communities live under, and the coordination function (shared textual standard across dispersed communities) continues to solve a real present problem, not merely a historical one. This blocks a mandatrophy read: this is not an arrangement whose function evaporated while the shell persisted for its own sake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_recitation_constitutes_living_language,
    'Does unbroken liturgical recitation and textual study, absent native generative daily speech across most of the diaspora period, constitute a ''living language'' in the sense linguists apply to the term, or is it better classified as a preserved ritual/liturgical register (comparable to Church Latin or Vedic Sanskrit)?',
    'Comparative linguistic analysis against the accepted definitional criteria for language ''life'' (generative productivity, native acquisition, community-wide vernacular use) versus liturgical preservation cases; examination of the extent and continuity of Hebrew''s use beyond ritual recitation in medieval/early-modern responsa literature, correspondence, and legal writing.',
    'If the stricter linguistic definition is adopted, this reading''s claim of ''living language'' status would be contested or downgraded to ''preserved liturgical register,'' shifting the classification question to whether the constraint is better framed as cultural/religious continuity rather than linguistic continuity — this would not change ε (which already reflects low extraction) but would affect how the claimed_type (rope vs. a non-linguistic coordination category) is interpreted downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_recitation_constitutes_living_language, conceptual, 'Whether liturgical-recitation continuity satisfies the definitional bar for ''living language'' status.').

omega_variable(
    reading_reachability_boundary,
    'Where exactly does the liturgical_continuity_reading''s claim end and the native_generation_reading''s claim begin — is there a strict-reachability break between memorized/recited Hebrew and generatively produced Hebrew (e.g., original halakhic responsa, private correspondence, poetry) throughout the diaspora period, or did generative production persist continuously alongside recitation such that the readings are not as cleanly separable as declared?',
    'Philological survey of medieval and early-modern Hebrew textual production outside of fixed liturgy (responsa literature, Hebrew poetry such as the Golden Age of Spain, private correspondence) to establish whether generative competence was continuously present alongside recitation, which would blur the boundary between this reading and the literary_revival_reading.',
    'If generative production is found to have been continuous rather than confined to elite literary bursts, the sharp separation between this reading and its siblings weakens, suggesting the three readings may share more structural overlap than the kernel''s decomposition currently assumes — though each remains independently authored per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_reachability_boundary, empirical, 'Whether generative Hebrew production was genuinely absent (isolating this reading) or continuously present (blurring reading boundaries) throughout the diaspora.').

omega_variable(
    voluntary_participation_vs_communal_pressure,
    'Is participation in liturgical Hebrew study and recitation genuinely voluntary as authored, or does communal/familial social pressure constitute a soft form of suppression not captured by the low suppression score (0.08)?',
    'Comparative sociological study of exit rates and exit costs across diaspora communities of varying insularity (e.g., Haredi enclaves versus liberal diaspora communities) to assess whether social sanction for non-participation rises to a suppression-relevant level in some subpopulations.',
    'If strong communal enforcement is found in specific subcommunities, the suppression score for THOSE communities'' instantiation of this constraint would need to be authored higher than the story-level average presented here, potentially shifting a subset of cases toward a tangled_rope classification even while the aggregate reading remains rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_vs_communal_pressure, empirical, 'Whether the low suppression score understates communal social pressure in more insular diaspora subcommunities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(hebr_tr_t80, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(hebr_tr_t100, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(hebr_be_t80, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(hebr_be_t100, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__liturgical_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings decomposing the colloquial claim 'Hebrew is a living language.' The liturgical_continuity_reading (this file) locates continuity in unbroken recitation and textual study across diaspora, authored as low-extraction voluntary coordination with no victim set. The native_generation_reading locates 'living' status specifically in generative daily native speech production, applying a stricter reachability test that this reading does not require. The literary_revival_reading locates continuity in Haskalah-era generative literary production, a narrower elite-textual claim distinct from both liturgical practice and native vernacular speech. All three share the kernel_id hebrew_living_language but are authored as independent ε-invariant constraints per the decomposition principle, linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
