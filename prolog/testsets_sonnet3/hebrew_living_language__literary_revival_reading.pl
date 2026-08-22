% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew as Living Language via Haskalah Literary Production
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the literary_revival_reading of the
 *   hebrew_living_language kernel: the claim that Hebrew remained a 'living'
 *   language through the Haskalah period (roughly 1781, the founding of
 *   Hameassef, through 1917) because it sustained generative literary
 *   production — new prose, poetry, periodicals, coined vocabulary for modern
 *   concepts — even though almost none of its writers or readers spoke it as
 *   a native daily vernacular. This is a genuinely low-extraction constraint:
 *   a voluntary literary coordination among intellectuals producing text
 *   nobody was coerced to write or read, with no identifiable victim class.
 *   The reading is deliberately narrow — it does not claim vernacular
 *   continuity (that is the liturgical_continuity_reading's and
 *   native_generation_reading's territory) and does not require anyone's
 *   daily speech to have been Hebrew. Its central ambiguity, which this story
 *   does not resolve, is whether written generative competence in the absence
 *   of a native speech community meets any defensible bar for 'a living
 *   language' at all, or whether it names a real and interesting but distinct
 *   phenomenon (literary vitality) that the kernel's colloquial label
 *   conflates with linguistic life.
 *
 * KEY AGENTS:
 *   - haskalah_literary_circles: primary agent and beneficiary (moderate/mobile) — produces the literary corpus that grounds the claim
 *   - modern_hebrew_literature_scholars: downstream beneficiary (moderate/arbitrage) — builds interpretive authority on the corpus
 *   - zionist_cultural_revival_movement: downstream institutional beneficiary (organized/mobile) — uses the reading as continuity evidence for the later vernacular project
 *   - yiddish_and_vernacular_speaking_jewish_communities: observer, outside the claim's scope, not harmed or served by it
 *   - comparative_linguists: analytical observer assessing the reading against standard linguistic-vitality criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as Living Language via Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '06acc1da-764a-4461-a6df-50b287cf83c6').
narrative_ontology:cs_kernel_codification('06acc1da-764a-4461-a6df-50b287cf83c6', distributed).
narrative_ontology:cs_authority_grounding('06acc1da-764a-4461-a6df-50b287cf83c6', practice).
narrative_ontology:cs_interpretation_layer_present('06acc1da-764a-4461-a6df-50b287cf83c6').
narrative_ontology:cs_reading_relation('06acc1da-764a-4461-a6df-50b287cf83c6', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('06acc1da-764a-4461-a6df-50b287cf83c6', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('06acc1da-764a-4461-a6df-50b287cf83c6', foundational, generative_literary_production_constitutes_life).
narrative_ontology:cs_axiom_status(generative_literary_production_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('06acc1da-764a-4461-a6df-50b287cf83c6', generative_literary_production_constitutes_life, conventional).
narrative_ontology:cs_axiom('06acc1da-764a-4461-a6df-50b287cf83c6', secondary, native_daily_speech_not_required_for_vitality).
narrative_ontology:cs_axiom_status(native_daily_speech_not_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('06acc1da-764a-4461-a6df-50b287cf83c6', native_daily_speech_not_required_for_vitality, conventional).
narrative_ontology:cs_reference_frame('06acc1da-764a-4461-a6df-50b287cf83c6', premodern_liturgical_register_ceiling).
narrative_ontology:cs_drift_state('06acc1da-764a-4461-a6df-50b287cf83c6', post_vernacularization_retrospective, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('06acc1da-764a-4461-a6df-50b287cf83c6', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_literary_circles).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, modern_hebrew_literature_scholars).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, zionist_cultural_revival_movement).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, written_generative_competence_constitutes_linguistic_life).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, unbroken_literary_chain_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 18th-19th century maskilim writers producing novels, poetry, periodicals, and scholarly prose in Hebrew across Central and Eastern Europe. They generate new vocabulary, syntax adapted to modern genres, and demonstrate that Hebrew can express contemporary ideas — all while speaking Yiddish, German, Russian, or other vernaculars at home and in the street. Their exit option is simply writing in another language, which many contemporaries do; nothing coerces the choice to write in Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_literary_circles, agenda_setter,
    moderate, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, haskalah_literary_circles, beneficiary).

% Later academics and literary historians who build careers, canons, and courses around the Haskalah corpus as evidence of Hebrew's unbroken vitality. They gain professional and institutional standing from a reading that treats textual generativity as sufficient for a language's life. Their exit is trivial — they could study other literatures — but this reading gives their object of study definitional weight.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, modern_hebrew_literature_scholars, beneficiary,
    moderate, generational, arbitrage, global).

% Later nationalist cultural project drawing on Haskalah literary Hebrew as proof that Hebrew possessed sufficient underlying vitality to be revived as a spoken vernacular in Palestine/Israel. They benefit from the literary-revival reading because it supplies a continuous prestige lineage predating the Ben-Yehuda-era vernacular project, even though the two are structurally distinct achievements.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, zionist_cultural_revival_movement, beneficiary,
    organized, civilizational, mobile, global).

% The overwhelming majority of the Jewish population in the same period, for whom Hebrew was a written/liturgical register while daily life ran in Yiddish, Judeo-Spanish, or local vernaculars. They are neither harmed nor extracted from by the literary-revival claim; they simply are not the population the claim is about, and the claim does not purport to describe their daily speech.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, yiddish_and_vernacular_speaking_jewish_communities, observer,
    moderate, biographical, mobile, regional).

% Scholars who assess whether written generative competence without a speech community meets standard criteria for a 'living language.' They note this reading sits deliberately below the bar of vernacular reachability and evaluate it on its own narrower terms — written productivity, not spoken continuity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community of writers and readers around producing and consuming original, non-liturgical Hebrew text that expands the language's expressive range into modern genres (novel, essay, periodical, scientific prose) — solving the real problem that premodern Hebrew's textual corpus (liturgy, halakha, commentary) had a narrower generative register than what Enlightenment-era Jewish intellectuals wanted to express in a Jewish literary language.
% TRANSFER_FUNCTION: Moves prestige and interpretive authority toward writers and scholars who can demonstrate Hebrew's modern literary productivity; moves relatively little in the way of material resources — this is a low-stakes symbolic/cultural transfer, not an extraction of goods or labor from any bounded group.
% ABSENT_VOICES: The vast vernacular-speaking Jewish population (Yiddish, Ladino, local languages) is not represented in this claim and would likely say the claim is true but narrow — Hebrew's literary vitality in this period says nothing about whether Hebrew was anyone's living daily language. They are not suppressed; they are simply outside the claim's scope.
% DISAPPEARANCE_RATIONALE: If Haskalah literary production were erased from the historical record, the modern Hebrew revival narrative would lose an important continuity link, and some scholarly and nationalist claims about Hebrew's unbroken vitality would need to rest more heavily on liturgical continuity alone or concede a sharper discontinuity before Ben-Yehuda-era vernacularization. Whether this constitutes 'the world rearranging' depends on whether one credits the literary-revival reading as doing real independent work, or as a decorative footnote to processes (liturgical continuity, later vernacular revival) that would proceed regardless — hence contested rather than settled either way.
% FOUNDING_PROBLEM: Enlightenment-era Jewish intellectuals wanted a modern secular Jewish literary culture, and needed Hebrew to be capable of expressing Enlightenment ideas, scientific concepts, and novelistic prose — a register the liturgical/halakhic corpus alone did not supply.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish Enlightenment (e.g. scholarship on the Haskalah outside the Zionist cultural-memory tradition) attest that the specific 19th-century problem — proving Hebrew a viable modern literary medium — was resolved by the literature's own existence and is no longer contested; the claim now persists mainly within literary-historical curricula and national-revival narratives rather than as a live linguistic question. No corroboration is offered from within vernacular-speaking Jewish communities of the period, who were not asked and left no organized record addressing this specific claim.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08 at interval end) because this reading describes a voluntary elite literary practice: no one is compelled to write, read, or fund Haskalah Hebrew literature, and no resource transfer of consequence occurs. Suppression is near-zero (0.05) for the same reason — there is no enforcement apparatus keeping anyone inside this arrangement. Theater ratio is modest and slowly rising (0.10 to 0.16) reflecting the gradual layering of retrospective nationalist significance onto what began as a fairly unselfconscious literary practice — some of the later 'vitality' claims about Haskalah literature are more performative (serving 20th-century revival narratives) than the original writers intended. Accessibility collapse is moderate (0.35): once you accept that literary generativity is the relevant criterion, alternative framings (vernacular continuity, liturgical continuity) do not disappear — they remain live and are simply not what this reading is about. Resistance is low (0.2): literary historians occasionally push back on overstating the corpus's cultural reach, but this is scholarly disagreement, not organized resistance to an extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Haskalah writers themselves are near-symmetric beneficiaries — they gain literary and intellectual standing from participating, at modest personal cost (writing in a register with a small readership). Downstream scholars and the Zionist revival movement are clearer beneficiaries with no offsetting cost: they inherit prestige and continuity-narrative capital without having done the original literary labor. No victim group is declared because no one bears an extractive cost through this specific arrangement — the vernacular-speaking majority is unaffected by it, not victimized by it, which is why they are coded as observer rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proving Hebrew capable of secular modern literary expression) is genuinely dead — settled by the literature's own accumulated existence well before 1917. The claim persists today mainly in literary-historical curricula and national-memory narratives, not because the original problem is unresolved. This is not mandatrophy in the extractive sense, however, because no one is coerced to maintain or fund the claim's persistence — it is closer to inertial scholarly convention than to an extraction structure requiring enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_competence_sufficiency,
    'Does written generative literary competence, absent any native speech community, satisfy any defensible definition of a language being ''living,'' or does it name a distinct phenomenon (literary vitality) that the kernel''s colloquial label improperly conflates with linguistic life?',
    'Comparative linguistic analysis against other historical cases of written-only generative traditions (e.g. medieval Latin scholarly production) to establish whether ''living language'' status has ever been granted on written generativity alone without a speech community.',
    'If written competence is insufficient, this reading''s claim collapses into a claim about ''living literary tradition'' rather than ''living language,'' which would not contradict the native_generation_reading''s stronger bar but would clarify that the two readings are not actually in tension — they are answering different questions dressed in the same label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_competence_sufficiency, conceptual, 'Whether written-only generativity meets any real bar for linguistic life, or is a distinct phenomenon mislabeled by the shared kernel term.').

omega_variable(
    haskalah_continuity_relation_to_vernacularization,
    'Did Haskalah literary Hebrew causally contribute to the later successful vernacularization project (native_generation_reading), or is it a separate achievement that the Zionist revival movement retrospectively annexed as continuity evidence?',
    'Historical-linguistic tracing of vocabulary, syntax, and stylistic borrowings from Haskalah literature into early modern spoken Hebrew (Ben-Yehuda''s lexicographic sources, early Yishuv press) versus independent grammatical innovations in the vernacular project.',
    'If strong causal continuity is found, this reading gains structural weight as a necessary precursor to the native_generation_reading (an influences relation is well-grounded); if the connection is largely retrospective narrative-building, the vindicated_propositions here are more purely symbolic and the causal claim embedded in the Zionist revival movement''s beneficiary status is weaker than commonly asserted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(haskalah_continuity_relation_to_vernacularization, empirical, 'Whether literary Haskalah Hebrew causally fed the later vernacular revival or is only retrospectively linked to it.').

omega_variable(
    elite_practice_representativeness,
    'Does the very small size of the Haskalah literary readership/writership (a fraction of a percent of the Jewish population in this period) undermine the claim that Hebrew was ''living'' in any collectively meaningful sense, even on the literary-revival reading''s own narrow terms?',
    'Circulation and readership data for Haskalah periodicals and literary works relative to total contemporary Jewish population, compared against thresholds used elsewhere for judging minority/elite literary traditions as sustaining linguistic vitality.',
    'If the readership was vanishingly small, this reading may itself be better understood as documenting a coterie practice rather than anything approaching language-wide vitality, which bears on how much weight the reading''s beneficiaries can fairly claim for it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_practice_representativeness, empirical, 'Whether the small size of the Haskalah literary public undercuts the vitality claim on its own terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1781, 1917).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1781, hebrew_living_language__literary_revival_reading, theater_ratio, 1781, 0.1).
narrative_ontology:measurement(hebr_tr_t1815, hebrew_living_language__literary_revival_reading, theater_ratio, 1815, 0.11).
narrative_ontology:measurement(hebr_tr_t1850, hebrew_living_language__literary_revival_reading, theater_ratio, 1850, 0.13).
narrative_ontology:measurement(hebr_tr_t1881, hebrew_living_language__literary_revival_reading, theater_ratio, 1881, 0.15).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__literary_revival_reading, theater_ratio, 1900, 0.16).
narrative_ontology:measurement(hebr_tr_t1917, hebrew_living_language__literary_revival_reading, theater_ratio, 1917, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1781, hebrew_living_language__literary_revival_reading, base_extractiveness, 1781, 0.04).
narrative_ontology:measurement(hebr_be_t1815, hebrew_living_language__literary_revival_reading, base_extractiveness, 1815, 0.05).
narrative_ontology:measurement(hebr_be_t1850, hebrew_living_language__literary_revival_reading, base_extractiveness, 1850, 0.06).
narrative_ontology:measurement(hebr_be_t1881, hebrew_living_language__literary_revival_reading, base_extractiveness, 1881, 0.07).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__literary_revival_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement(hebr_be_t1917, hebrew_living_language__literary_revival_reading, base_extractiveness, 1917, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.05).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial claim 'Hebrew is/was a living language' per the eps-invariance principle. Each sibling reading has a structurally distinct ee: liturgical_continuity_reading rests on unbroken recitation/study (near-zero extraction, near-total population coverage, no elite/vernacular gap); literary_revival_reading (this story) rests on elite written generative production in a narrow window (very low extraction, tiny population coverage, explicit non-requirement of native speech); native_generation_reading rests on the much later achievement of native daily speech production (the strongest and most contested bar, structurally distinct victim/beneficiary set tied to the Zionist vernacularization project). The three are linked, not merged, because measuring 'is Hebrew living' by recitation, by literary output, or by native speech generativity yields three different ee values and three different classifications — exactly the decomposition the eps-invariance principle requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
