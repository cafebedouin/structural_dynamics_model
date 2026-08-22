% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status: Literary Continuity Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The literary-continuity reading of 'living language' status emerged in
 *   18th–19th-century Jewish intellectual circles facing diaspora
 *   fragmentation. Maskilim reformers redefined language vitality away from
 *   native-speaker demographics (which were collapsing) toward continuous
 *   production of new literary and intellectual work. Hebrew periodicals,
 *   secular philosophy, scientific translation, and modern poetry became the
 *   evidentiary standard for claiming the language 'lives.' This reading
 *   enabled cultural authority for an intellectual elite without requiring
 *   mass adoption or generational transmission. It is one of three contested
 *   readings of the same kernel—the definition of what makes a language
 *   'living'—each anchored to different evidence (literary productivity,
 *   liturgical continuity, native-speaker transmission) and benefiting
 *   different stakeholders (secular intellectuals, religious authorities,
 *   demographic communities).
 *
 * KEY AGENTS:
 *   - maskilim_secular_intellectuals: Elite cultural producers who gatekeep literary canon and define vitality through their own productivity
 *   - illiterate_non_literary_speakers: Erased from vitality definition despite daily language use; structurally invisible in assessment
 *   - traditional_religious_authorities: Language-keepers through centuries of liturgical continuity; excluded from recognition under secular literary criterion
 *   - native_speaker_communities: Vernacular speakers whose generational transmission patterns are irrelevant to the literary-continuity reading
 *   - reading_publics_international: Testifiers to the reading through their consumption of new literature
 *   - linguistic_science_observers: Analytical seat holding competing vitality definitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.38).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.42).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status: Literary Continuity Reading").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '82724a04-1272-4371-9eae-294ba39f1f1f').
narrative_ontology:cs_kernel_codification('82724a04-1272-4371-9eae-294ba39f1f1f', distributed).
narrative_ontology:cs_authority_grounding('82724a04-1272-4371-9eae-294ba39f1f1f', distributed).
narrative_ontology:cs_reading_relation('82724a04-1272-4371-9eae-294ba39f1f1f', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('82724a04-1272-4371-9eae-294ba39f1f1f', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('82724a04-1272-4371-9eae-294ba39f1f1f', foundational, literary_productivity_constitutes_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('82724a04-1272-4371-9eae-294ba39f1f1f', literary_productivity_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('82724a04-1272-4371-9eae-294ba39f1f1f', foundational, elite_intellectual_authority_sufficient_without_mass_adoption).
narrative_ontology:cs_axiom_status(elite_intellectual_authority_sufficient_without_mass_adoption, holdable).
narrative_ontology:cs_axiom_grounding('82724a04-1272-4371-9eae-294ba39f1f1f', elite_intellectual_authority_sufficient_without_mass_adoption, deontological).
narrative_ontology:cs_reference_frame('82724a04-1272-4371-9eae-294ba39f1f1f', diaspora_fragmented_vernacular_transmission).
narrative_ontology:cs_drift_state('82724a04-1272-4371-9eae-294ba39f1f1f', contemporary_native_speaker_emergence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('82724a04-1272-4371-9eae-294ba39f1f1f', '2026-06-11T14:32:18Z').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_non_literary_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, reading_publics_international).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, traditional_religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces and curates new literary, philosophical, and scientific works in Hebrew; establishes what counts as evidence of 'productivity' and 'vitality'; gains cultural authority and institutional legitimacy through their gatekeeping role over literary canon formation. They define the reading's criteria—literary output, intellectual continuity, secular intellectual work—and occupy the primary seats from which 'living language' status is assessed and conferred.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals, agenda_setter).

% Excluded from the vitality definition: their actual use of the language in speech, trade, household, and communal life is rendered invisible by the literary-continuity criterion. Trapped in the language (cannot easily switch for daily life) but structurally invisible in the definition that determines whether their language 'counts' as living. Bears the cost of being erased from the vitality verdict while having no voice in how vitality is assessed.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_non_literary_speakers, payer,
    powerless, biographical, trapped, national).

% Maintain the language through liturgical and halakhic transmission; their centuries of textual continuity and ritual preservation are superseded in status by the secular literary reading. Identity-locked to the language through religious obligation and theological authority. Structurally excluded from the 'literary work' criterion—liturgical recitation and commentary do not count as evidence of vitality under this reading. Their actual language maintenance work is invisible to the assessment.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, traditional_religious_authorities, payer,
    powerful, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, traditional_religious_authorities, excluded).

% Speakers for whom the language is (or was) a daily mother tongue; their generational transmission and vernacular use is structurally irrelevant to the literary-continuity reading. They are excluded not because they are absent but because their evidence—spoken daily language, acquisition patterns, vernacular creativity—does not register as proof of vitality under the literary criterion. They could object that intellectual work in a dead language is theater without generational carriers.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, native_speaker_communities, excluded,
    moderate, biographical, constrained, national).

% Consumers and supporters of the new literary and intellectual works; gain access to a continuous stream of literature, philosophy, journalism, and scientific work. They testify to the constraint's framing: if books are published and read, the language is living. Their testimony is mobile—they can read in any language and their choice to read Hebrew is volitional—which makes them a weaker anchor for the constraint than the native speaker populations they partially displace as the primary evidence of vitality.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, reading_publics_international, beneficiary,
    organized, generational, mobile, global).

% Classify languages by vitality criteria: native speaker demographics, intergenerational transmission rates, language endangerment indices, corpus analysis. From this seat, the literary-continuity reading is one claim among competing definitions of 'living language'; other definitions (native speaker status, vernacular transmission) produce different classifications and cannot be adjudicated within the literary-production frame.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguistic_science_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals).
narrative_ontology:fixing_cost_class(living_language_status__literary_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared criterion for assessing language vitality that permits sustained literary and intellectual production without requiring mass native-speaker adoption or intergenerational family transmission. Coordinates around the production and circulation of new written works as the metric of cultural continuity.
% TRANSFER_FUNCTION: Transfers cultural authority and legitimacy from those who speak the language daily (illiterate and non-literary speakers, vernacular communities) to those who produce and curate literature and intellectual work. Also transfers the authority to define 'language vitality' from sociolinguistic demographic measures to literary-production measures.
% ABSENT_VOICES: Native speaker communities (especially vernacular speakers without formal education) are structurally excluded from the conversation; their daily use and transmission patterns do not constitute acceptable evidence of vitality. Religious authorities whose centuries of textual transmission maintained the language are excluded from recognition as language-keepers under the secular literary criterion. Sociolinguists using speaker demographics would argue for different vitality criteria entirely.
% DISAPPEARANCE_RATIONALE: If the literary-continuity reading vanished—if intellectual and literary work ceased being the measure of language vitality—authority over the 'living language' verdict would revert to sociolinguistic demographic criteria or to the religious/liturgical transmission communities. The Haskalah and modern Hebrew literary canon would persist as historical artifacts, but they would no longer be cited as PROOF of the language's vitality. The constraint's disappearance would make the language's future dependent on generational native-speaker transmission and demographic reproduction, not on continued elite intellectual production.
% FOUNDING_PROBLEM: In the 18th–19th centuries, Hebrew faced a succession crisis: the traditional liturgical and scholarly elite could not sustain the language as a living speech community for growing diaspora populations. Maskilim reformers reclaimed Hebrew not for daily speech but for intellectual modernization, arguing that a language remains 'alive' if it can be productively used for new philosophy, science, and literature. This reading solved the problem of how to justify language continuity when intergenerational native-speaker transmission was fragmenting.
% FOUNDING_PROBLEM_CORROBORATION: The maskilim and their intellectual heirs attest that the problem was real and the solution worked: Hebrew literature flourished, intellectual work continued, and the language remained culturally productive despite diaspora fragmentation. Demographers and sociolinguists attest that the problem of LANGUAGE VITALITY was never solved this way—generational transmission rates continued to plummet until 20th-century national movement created new native-speaker demographics in Palestine/Israel. Religious traditionalists attest they solved the vitality problem through liturgical continuity and would not have needed the elite literary reading. The founding-problem status is contested because the 'problem' itself is reading-indexed: a vitality crisis in one reading's frame is irrelevant in another's.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the reading coordinates around a genuine function—maintaining literary production—but does so by transferring authority away from those who actually speak and transmit the language daily. The suppression requirement rises from 0.18 to 0.42 across the interval as the reading must increasingly work to displace competing definitions (liturgical, demographic) and silence native-speaker evidence. Theater ratio rises from 0.08 to 0.28 because over time the claim that 'literary work proves vitality' requires more performative assertion as actual generational transmission continues to decline—the reading must work harder to maintain its authority as the evidence it relies on (actual living speech communities) diverges from the criterion it defines (literary productivity). The measurement series documents the growing tension between the reading's claim and the vernacular reality of language decline. All metrics share one time grid (1770, 1820, 1860, 1890, 1920) so temporal analysis has complete data.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and beneficiary seats should compute entirely different type classifications. From the maskilim's seat, this is a rope: elite coordination around literary production with shared benefits (cultural continuity, intellectual authority). From the illiterate speaker's seat, this is a snare: a definition that erases their language use as irrelevant while claiming the language 'lives' because intellectuals write about it. From the religious authority's seat, this is a snare: centuries of textual maintenance are superseded and declared insufficient. From the sociolinguist's analytical seat, this is a false mountain—the reading asserts an objective fact ('living language') when it is actually a reading-indexed claim about what evidence counts. The engine computes these seat-divergences from the structural data; the claim remains 'rope' while the metrics describe moderate extraction and rising suppression requirement, capturing the gap between the reading's self-description and its actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim and secular intellectuals benefit structurally: they gain cultural authority to define language vitality without needing mass adoption, they claim gatekeeping power over what counts as 'living language,' and their institutional positions are strengthened by intellectual authority. For them, d approaches 0.0 (strong beneficiary). Illiterate and non-literary speakers are the targets: their actual language use is rendered invisible and irrelevant to the vitality verdict; they are trapped in a language that is simultaneously declared 'living' (through elite literary production) while their own speech patterns are declared irrelevant to that assessment. For them, d approaches 1.0 (full target). Religious authorities sit between: they have maintained the language (beneficiary position) but are excluded from recognition under the secular criterion (payer position) and are identity-locked to the language (cannot exit). Traditional religious authorities carry d ≈ 0.6. Native speakers carry d ≈ 0.7 (their generational transmission is made irrelevant to vitality status, erasing their labor as language-keepers).
 *
 * MANDATROPHY ANALYSIS:
 *   The literary-continuity reading faces a fundamental mandatrophy: it was authored to solve a problem (how to maintain language continuity without generational transmission) by redefining 'vitality' to mean 'literary productivity.' But by the early 20th century, the original problem was solved differently—through the Zionist national movement's creation of a new native-speaker community in Palestine with generational transmission. The reading's founding mandate (maintain cultural continuity under diaspora fragmentation) became obsolete even as the reading persisted as a cultural authority structure. The theater ratio's rise reflects this: the reading must increasingly perform as 'proof' of vitality even as actual vernacular communities develop and make the literary-only criterion less necessary. A mandatrophy verdict: the founding problem (diaspora fragmentation preventing native-speaker transmission) is dead (solved by Palestinian/Israeli resettlement and new native communities), but the reading persists as a gatekeeping authority structure over what counts as 'living language' long after its justifying problem was resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_productivity_vs_actual_vitality,
    'Does the literary-continuity reading''s claim that ''literary productivity proves language vitality'' map to actual language health as measured by sociolinguistic criteria (speaker demographics, intergenerational transmission, vernacular expansion)?',
    'Comparative analysis of three readings'' predictions against historical outcomes: (1) literary-continuity reading predicts vitality wherever new literary work continues; (2) liturgical-preservation reading predicts vitality wherever ritual use continues; (3) native-generation reading predicts vitality wherever native-speaker transmission occurs. Measure outcomes (actual language persistence, community sustainability, intergenerational continuity) against each prediction.',
    'If literary productivity did not predict actual language persistence (if communities with vibrant literature but no native speakers eventually lost the language, while communities with strong generational transmission but little literature preserved it), the reading''s foundational claim is empirically false. This would reclassify the reading from rope to snare—the literary criterion was never evidence of vitality, only a cover story for intellectual gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literary_productivity_vs_actual_vitality, empirical, 'Whether literary productivity actually correlates with language persistence or is orthogonal to it.').

omega_variable(
    reading_foreclosure_conditions,
    'Does the literary-continuity reading''s definition logically foreclose the native-generation reading (does it rule out that generational transmission is necessary), or do the two readings coexist as incompatible claims held by different parties?',
    'Examine whether a party could consistently hold BOTH readings simultaneously: that language vitality requires literary productivity AND generational transmission, without contradiction. If yes, the readings coexist. If no (if insisting on literary criterion requires denying the generational criterion), then foreclosure holds.',
    'If the readings coexist (a party could say ''vitality requires both''), then the kernel is structured as persistent competing claims without logical resolution—a contested kernel where no reading rules out another. If foreclosure holds (literary criterion logically negates generational necessity), then the three readings form a foreclosure triplet rather than a coexistence triplet, and the engine should compute foreclosure relations rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_conditions, conceptual, 'Whether the literary and generational readings are logically incompatible or can coexist in a single framework.').

omega_variable(
    suppression_mechanism_identity_locked,
    'For the illiterate and non-literary speakers and for traditional religious authorities, is the suppression structural (they cannot produce literature, cannot access the literary criterion, economic barriers to literacy) or internalized (they accept the reading''s premise that literary work is the real proof of vitality, leading them to devalue their own daily use and transmission)?',
    'Post-suppression trajectory analysis: if suppression is purely structural, speakers who gain access to literacy and literary circles should abandon the daily vernacular use without cognitive dissonance. If suppression is internalized, speakers who gain literacy often retain the belief that their own vernacular use ''doesn''t count,'' carrying the suppression even after structural barriers are removed.',
    'If internalized, the measured suppression (0.42) understates the constraint''s effective suppression on the payer seats—the targets carry the erasing definition with them after exit and may not recognize their own language practice as valid proof of vitality even after they become literate. This would reclassify some aspects of the constraint from snare (externally enforced definition) to identity-fusion (the definition becomes part of the target''s self-concept, making exit psychologically costly even after structural barriers fall).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_locked, empirical, 'Whether suppression of non-literary evidence is structural or internalized in language-preserving communities.').

omega_variable(
    committer_frame_reading_choice,
    'Why is the literary-continuity reading—rather than the liturgical-preservation reading or the native-generation reading—the one authenticated as ''the'' reading of the living-language-status kernel in this constraint story?',
    'The choice reflects the author''s analytical seat: a reading through which modern Hebrew''s documented history (Haskalah periodicals, secular intellectual work) becomes the primary evidence of vitality. This reading is historically instantiated and documented. The other readings are also historically instantiated (liturgical communities maintained Hebrew through medieval and early modern periods; native-speaker communities re-emerged in 20th-century Palestine/Israel). The choice to generate THIS reading and defer the others is a reading-selection decision, not a discovery.',
    'The three readings should be authored as three separate constraint stories, each clean and ε-invariant within its own reading frame, linked via network.affects_constraints. The choice to author literary_continuity_reading first creates a framing priority that may shape how the sibling readings are subsequently authored. Acknowledging this omega makes explicit that reading-selection is part of the authoring act and not invisible or inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_choice, conceptual, 'The reading-selection is an authored choice that structures what constraint story gets generated first, framing how siblings are subsequently understood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1770, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1770, living_language_status__literary_continuity_reading, theater_ratio, 1770, 0.08).
narrative_ontology:measurement(livi_tr_t1820, living_language_status__literary_continuity_reading, theater_ratio, 1820, 0.12).
narrative_ontology:measurement(livi_tr_t1860, living_language_status__literary_continuity_reading, theater_ratio, 1860, 0.18).
narrative_ontology:measurement(livi_tr_t1890, living_language_status__literary_continuity_reading, theater_ratio, 1890, 0.24).
narrative_ontology:measurement(livi_tr_t1920, living_language_status__literary_continuity_reading, theater_ratio, 1920, 0.28).

% Extraction over time
narrative_ontology:measurement(livi_be_t1770, living_language_status__literary_continuity_reading, base_extractiveness, 1770, 0.22).
narrative_ontology:measurement(livi_be_t1820, living_language_status__literary_continuity_reading, base_extractiveness, 1820, 0.28).
narrative_ontology:measurement(livi_be_t1860, living_language_status__literary_continuity_reading, base_extractiveness, 1860, 0.35).
narrative_ontology:measurement(livi_be_t1890, living_language_status__literary_continuity_reading, base_extractiveness, 1890, 0.37).
narrative_ontology:measurement(livi_be_t1920, living_language_status__literary_continuity_reading, base_extractiveness, 1920, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1770, living_language_status__literary_continuity_reading, suppression_requirement, 1770, 0.18).
narrative_ontology:measurement(livi_su_t1820, living_language_status__literary_continuity_reading, suppression_requirement, 1820, 0.25).
narrative_ontology:measurement(livi_su_t1860, living_language_status__literary_continuity_reading, suppression_requirement, 1860, 0.32).
narrative_ontology:measurement(livi_su_t1890, living_language_status__literary_continuity_reading, suppression_requirement, 1890, 0.38).
narrative_ontology:measurement(livi_su_t1920, living_language_status__literary_continuity_reading, suppression_requirement, 1920, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three structurally distinct constraints, one per reading. The literary_continuity_reading (this story) defines vitality through new literary and intellectual work, benefiting the maskilim intellectual elite. The liturgical_preservation_reading defines vitality through continuous ritual use and textual study, benefiting traditional religious authorities. The native_generation_reading defines vitality through intergenerational native-speaker transmission, benefiting vernacular speaker communities. Each reading has different ε, different beneficiaries/victims, and different structural relationships to the language and its speakers. The three readings are live simultaneously in contemporary discourse—different parties, institutions, and readings of history adopt different vitality criteria. This constraint (literary_continuity_reading) is downstream of no parent and upstream of both siblings in that it was historically authored first (18th-century Haskalah) and the other readings emerged as responses to or rejections of it. All three should be authored as separate constraint stories linked via network.affects_constraints, each with its own cs_structure.reading_relations and cs_structure.axioms documenting the triplet.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
