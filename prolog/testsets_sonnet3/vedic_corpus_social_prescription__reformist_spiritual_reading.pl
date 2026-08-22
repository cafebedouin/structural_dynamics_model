% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of the Vedic Corpus (Non-Prescriptive Cosmology)
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   This story instantiates the reformist spiritual reading of the Vedic
 *   corpus kernel: the claim that Vedic hymns and Upanishadic material
 *   describe metaphysical unity (atman-brahman identity) and cosmological
 *   metaphor, and contain no binding social prescription — the varna
 *   references present in the text are read as either later interpolations,
 *   symbolic descriptions of psychological temperament (guna), or corruptions
 *   layered on by later redactors seeking to naturalize hereditary hierarchy.
 *   This reading emerged prominently in 19th-20th century reform movements
 *   (Brahmo Samaj, Arya Samaj strands, universalist Vedanta) responding to
 *   both indigenous caste critique and colonial codification pressures. It
 *   shares a textual kernel with two sibling readings — the orthodox varna
 *   reading (which takes the same hierarchical passages as literally binding
 *   divine mandate) and the colonial orientalist reading (which treats the
 *   corpus as a unified administrative law code) — but is authored here as
 *   its own constraint with its own ε, because its coordination function,
 *   beneficiary structure, and extraction profile are structurally distinct
 *   from both.
 *
 * KEY AGENTS:
 *   - reformist_practitioners: primary beneficiary/practitioner base (moderate/mobile) — gains coherent scripturally-warranted egalitarian practice
 *   - cross_caste_spiritual_seekers: secondary beneficiary (powerless/mobile) — gains access previously denied under orthodox reading
 *   - modern_hindu_reform_movements: agenda_setter (organized/mobile) — institutionalizes and propagates the reading through persuasion, not enforcement
 *   - orthodox_varna_institutions: excluded party (institutional/constrained) — sidelined by, not coerced by, this reading's success
 *   - textual_philologists: analytical observer (analytical) — assesses but does not adjudicate the reading's truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.05).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of the Vedic Corpus (Non-Prescriptive Cosmology)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '4c4525b8-e417-4ece-931c-2f5c2534fd67').
narrative_ontology:cs_kernel_codification('4c4525b8-e417-4ece-931c-2f5c2534fd67', fixed_text).
narrative_ontology:cs_authority_grounding('4c4525b8-e417-4ece-931c-2f5c2534fd67', practice).
narrative_ontology:cs_interpretation_layer_present('4c4525b8-e417-4ece-931c-2f5c2534fd67').
narrative_ontology:cs_reading_relation('4c4525b8-e417-4ece-931c-2f5c2534fd67', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('4c4525b8-e417-4ece-931c-2f5c2534fd67', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('4c4525b8-e417-4ece-931c-2f5c2534fd67', foundational, varna_content_is_corruption_or_metaphor).
narrative_ontology:cs_axiom_status(varna_content_is_corruption_or_metaphor, holdable).
narrative_ontology:cs_axiom_grounding('4c4525b8-e417-4ece-931c-2f5c2534fd67', varna_content_is_corruption_or_metaphor, conventional).
narrative_ontology:cs_axiom('4c4525b8-e417-4ece-931c-2f5c2534fd67', foundational, atman_brahman_unity_precludes_hierarchical_ontology).
narrative_ontology:cs_axiom_status(atman_brahman_unity_precludes_hierarchical_ontology, holdable).
narrative_ontology:cs_axiom_grounding('4c4525b8-e417-4ece-931c-2f5c2534fd67', atman_brahman_unity_precludes_hierarchical_ontology, deontological).
narrative_ontology:cs_reference_frame('4c4525b8-e417-4ece-931c-2f5c2534fd67', universalist_vedantic_non_dualism).
narrative_ontology:cs_drift_state('4c4525b8-e417-4ece-931c-2f5c2534fd67', post_colonial_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4c4525b8-e417-4ece-931c-2f5c2534fd67', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_spiritual_seekers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, modern_hindu_reform_movements).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, atman_brahman_unity_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, varna_as_later_social_corruption_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practice Vedic-derived meditation, chant, and philosophical study while explicitly rejecting caste-based social prescription. They read the hymns and Upanishadic material as metaphysical poetry about non-dual unity (atman-brahman identity) rather than as a social rulebook, and organize study circles and ashrams open to all castes and none.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Individuals historically excluded from Vedic study under orthodox varna readings (particularly those classed as shudra or outside the varna system) who gain access to the corpus's spiritual content once it is read as universal and non-prescriptive. Their access depends entirely on this reading displacing the exclusionary one in local practice.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_spiritual_seekers, beneficiary,
    powerless, biographical, mobile, regional).

% Organizations (in the lineage of Brahmo Samaj, Arya Samaj strands, and later universalist Vedanta movements) that actively promote this reading through publication, translation, and institution-building. They administer the interpretive framework — teaching that varna references are later interpolations or symbolic descriptions of temperament (guna) rather than birth-ascribed social rank — and have no enforcement mechanism beyond persuasion and institutional example.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, modern_hindu_reform_movements, agenda_setter,
    organized, generational, mobile, national).

% Traditional authorities whose social and ritual privilege depends on the literal hierarchical reading. They are not consulted as authorities within this reading's framework and would object that the reformist reading discards textually explicit passages (e.g. Purusha Sukta) as corruption rather than engaging them as scripture; they are structurally sidelined by this reading's success, not coerced by it.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_institutions, excluded,
    institutional, generational, constrained, national).

% Historical-critical scholars who assess dating, layering, and interpolation claims across the Vedic corpus. Their findings are invoked by this reading (to support a later-corruption thesis) but the reading does not depend on their conclusions being final — it can persist as a devotional/interpretive stance independent of philological consensus.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, textual_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared spiritual vocabulary and practice framework (meditation on non-dual unity, cosmological metaphor) that can be adopted across caste, sect, and even national lines without requiring participants to accept or perform a social hierarchy as a condition of practice.
% TRANSFER_FUNCTION: Moves interpretive authority away from hereditary ritual specialists and toward reform institutions and individual practitioners; moves access to Vedic study and meaning-making toward previously excluded groups. No material extraction is involved — the transfer is one of interpretive legitimacy and access, not rents or resources.
% ABSENT_VOICES: Orthodox ritual authorities and ethnographers of lived caste practice would object that the reading discards textually explicit hierarchical content (e.g. hymns explicitly enumerating social orders) by reclassifying it as corrupt interpolation rather than confronting it as canonical; they are not structurally excluded from speaking, but this reading treats their textual evidence as disqualified by definition, which forecloses rather than answers their objection within its own terms.
% DISAPPEARANCE_RATIONALE: If this reading vanished, reform institutions built on it would lose their scriptural warrant for caste-inclusive practice and might rely instead on purely modern/secular egalitarian arguments; cross-caste practitioners might lose an important pathway of felt continuity with tradition. Orthodox institutions would be relatively unaffected since the reading does not currently displace their practice by force. Whether 'the world rearranges' depends on which population is asked — practitioners inside reform movements say yes, disinterested historians of the institutions say the underlying social patterns are largely independent of which reading wins.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century reformers confronted colonial-era codification of caste hierarchy as 'authentic Hindu law' and needed a scripturally grounded basis to argue that caste oppression was not intrinsic to Vedic religion, in order to pursue social reform without abandoning textual authority entirely.
% FOUNDING_PROBLEM_CORROBORATION: Reform movement historians and some Vedic scholars outside the movements themselves (e.g. scholars tracing Brahmo and Arya Samaj intellectual history) corroborate that the founding motivation was genuine anti-caste reform under colonial pressure. Orthodox commentators and some philologists dispute the underlying textual claim (that hierarchy content is interpolated rather than original), so the founding problem's status as 'solved by correct reading' rather than 'solved by selective reading' remains actively contested from outside the reform movements themselves.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored very low (0.08) because this reading names no victim group, imposes no transfer of resources, and requires no enforcement apparatus — its persistence depends on voluntary adoption in practice communities, not coercion. Suppression is low (0.05) because no alternative reading is blocked by force; orthodox and colonial readings continue to be taught and practiced elsewhere without interference from this one. Theater ratio is low-moderate (0.15) reflecting some institutional performance in reform movement publications and public disputation, but the bulk of activity (meditation practice, textual study, community building) is functional rather than performative. Resistance is moderate (0.35) because orthodox authorities and some philologists actively contest the interpolation thesis — this is a live scholarly and religious dispute, not a settled matter, which is why resistance sits well above a genuine mountain's near-zero floor despite the low extraction score.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist practitioners and reform movements sit near the beneficiary end: they gain interpretive legitimacy, practice access, and social standing from this reading without bearing offsetting costs. Cross-caste seekers benefit even more sharply because their prior exclusion under the orthodox reading is the specific harm this reading removes — their directionality is strongly toward subsidy. No agent is authored as a victim because the reading's operation does not extract resources, labor, or status from any identifiable group; its main structural effect on orthodox institutions is loss of exclusive interpretive authority, which is a competitive rather than extractive relationship (they are excluded from THIS reading's framework, not preyed upon by it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (providing scripturally-grounded warrant for anti-caste reform under colonial-era pressure to codify hierarchical 'Hindu law') is contested as to whether it remains live: reform movement historians say caste-based exclusion from spiritual practice persists in many contexts, keeping the founding problem live; critics note the reading sometimes functions as a rhetorical move that sidesteps engagement with textually explicit hierarchical content rather than resolving the underlying dispute. This mismatch (a founding problem claimed live by beneficiaries, contested by outside observers) is exactly the R5 signal the framework is built to surface, and it is why founding_problem_status is authored as contested rather than live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpolation_thesis_historicity,
    'Are the hierarchical/varna-referencing passages in the Vedic corpus (e.g. Purusha Sukta) genuinely later interpolations, or are they original content that this reading dismisses for reformist convenience?',
    'Philological dating of textual layers via linguistic archaism, meter analysis, and cross-referencing with archaeological and epigraphic evidence of social organization in the relevant periods.',
    'If the hierarchical passages are original rather than interpolated, this reading''s central textual claim is undermined and it becomes a normative reinterpretation dressed as historical-critical recovery — which would not change its low ε but would change how the reading should be described (as revisionist rather than restorative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpolation_thesis_historicity, empirical, 'Whether the corpus''s hierarchical content is original or interpolated is unresolved and central to this reading''s self-understanding.').

omega_variable(
    reading_as_kernel_contest_resolution_mechanism,
    'Is the disagreement between this reading, the orthodox varna reading, and the colonial orientalist reading resolvable by any shared evidentiary standard, or is it a genuine framework-incommensurability (different criteria for what counts as ''what the text says'')?',
    'Comparative hermeneutics: identify whether the three readings share enough interpretive premises (e.g. agreement on textual boundaries, translation choices) that evidence could in principle adjudicate between them, or whether they operate with incommensurable criteria for textual authority (literalist vs. reformist-allegorical vs. administrative-codification).',
    'If genuinely incommensurable, the three sibling constraints should remain permanently coexisting rather than one eventually displacing the others by evidence; if commensurable, the network relationship between siblings should eventually resolve toward foreclosure of at least one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernel_contest_resolution_mechanism, conceptual, 'Whether the kernel contest among the three readings can be resolved by shared evidentiary standards or is a framework-level incommensurability.').

omega_variable(
    reformist_reading_as_cover_for_status_quo,
    'Does the reformist spiritual reading, by relocating caste critique entirely into hermeneutics, reduce pressure for material and institutional reform of caste-based inequality that persists independently of textual interpretation?',
    'Compare social-outcome trajectories (intercaste marriage rates, access to temple entry, economic mobility) in communities where the reformist reading is dominant versus communities pursuing purely secular/legal anti-caste reform, controlling for other factors.',
    'If the reformist reading correlates with reduced material reform pressure, its low ε as a purely spiritual/coordination constraint could mask a diffuse extractive function (preserving caste privilege by converting a material struggle into a textual one) that this story''s current metrics do not capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_reading_as_cover_for_status_quo, empirical, 'Whether textual reinterpretation substitutes for or complements material anti-caste reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 120, 0.14).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 160, 0.15).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 40, 0.06).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 120, 0.07).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 160, 0.08).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 200, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vedic_corpus_social_prescription kernel (constraint family). orthodox_varna_reading claims high ε (identifiable victim groups under literal hierarchical mandate, active enforcement via ritual and social exclusion). colonial_orientalist_reading claims moderate-to-high ε (administrative codification serving colonial governance interests, with victims among those whose customary practice was overridden by codified 'law'). This reformist_spiritual_reading claims low ε (no victim set, no enforcement mechanism, voluntary practice adoption). All three share the same underlying textual corpus as their referent but are authored as separate constraints per the ε-invariance principle, since evaluating the corpus under each reading's own lights yields structurally different extraction profiles, beneficiary sets, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
