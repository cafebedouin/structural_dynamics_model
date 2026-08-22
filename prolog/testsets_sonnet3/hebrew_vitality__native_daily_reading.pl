% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Native Daily Generation as the Sole Criterion of Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'native_daily_reading' of the contested
 *   Hebrew vitality kernel: the claim that only native daily generation of a
 *   language constitutes genuine vitality, and that ritual/liturgical
 *   recitation — however unbroken and continuous — is mere preservation, not
 *   life. This reading underwrote the Zionist vernacular revival project
 *   (Ben-Yehuda and successors): it required active pedagogical and
 *   institutional enforcement (Hebrew-only schooling, army service, civil
 *   administration) to convert a population fluent in diaspora vernaculars
 *   and liturgical Hebrew into native Hebrew speakers, and it reclassified
 *   centuries of liturgical transmission as a lesser, non-living mode of the
 *   language's existence. Sibling readings — liturgical_reading (ritual
 *   preservation constitutes vitality on its own) and
 *   hybrid_continuity_reading (liturgical preservation was a necessary
 *   enabling substrate, insufficient alone) — are NOT part of this
 *   constraint; they are separate files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - zionist_state_building_project: agenda-setter and primary beneficiary, institutional power, sets the vitality criterion and enforces it through schooling and civil administration
 *   - ivrit_pedagogical_establishment: organized beneficiary, builds and certifies the modern vernacular standard
 *   - sabra_native_speaker_generation: moderate-power beneficiary, inherits full linguistic capital under the new standard
 *   - liturgical_hebrew_tradition: powerless, trapped payer, reclassified from living language to mere preservation
 *   - diaspora_yiddish_speaking_communities: powerless payer, pressured to abandon their actual vernacular
 *   - religious_hebrew_reading_communities: moderate-power payer, deep textual fluency recategorized as non-vital
 *   - comparative_sociolinguists: analytical observer of the whole contested kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.52).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.61).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native Daily Generation as the Sole Criterion of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistics/language_revitalization/jewish_studies").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '10762b7e-d469-401b-9641-e202bc7f3965').
narrative_ontology:cs_kernel_codification('10762b7e-d469-401b-9641-e202bc7f3965', distributed).
narrative_ontology:cs_authority_grounding('10762b7e-d469-401b-9641-e202bc7f3965', extraction).
narrative_ontology:cs_interpretation_layer_present('10762b7e-d469-401b-9641-e202bc7f3965').
narrative_ontology:cs_reading_relation('10762b7e-d469-401b-9641-e202bc7f3965', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('10762b7e-d469-401b-9641-e202bc7f3965', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('10762b7e-d469-401b-9641-e202bc7f3965', foundational, native_acquisition_is_necessary_for_vitality).
narrative_ontology:cs_axiom_status(native_acquisition_is_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('10762b7e-d469-401b-9641-e202bc7f3965', native_acquisition_is_necessary_for_vitality, conventional).
narrative_ontology:cs_axiom('10762b7e-d469-401b-9641-e202bc7f3965', foundational, ritual_recitation_without_native_speech_is_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_recitation_without_native_speech_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('10762b7e-d469-401b-9641-e202bc7f3965', ritual_recitation_without_native_speech_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('10762b7e-d469-401b-9641-e202bc7f3965', diaspora_multilingual_liturgical_equilibrium).
narrative_ontology:cs_drift_state('10762b7e-d469-401b-9641-e202bc7f3965', post_1948_state_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('10762b7e-d469-401b-9641-e202bc7f3965', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, ivrit_pedagogical_establishment).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, sabra_native_speaker_generation).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_yiddish_speaking_communities).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, religious_hebrew_reading_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the criterion that a language is 'alive' only if it is natively generated in daily speech, and builds the school system, army, and bureaucracy that enforce Hebrew as the sole vernacular. Collects the nation-building payoff: a unified, modernized, deliberately de-diasporized national identity organized around a reconstructed vernacular.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, zionist_state_building_project, beneficiary).

% Language academies, teacher-training colleges, and the Hebrew Language Committee/Academy build curricula, coin vocabulary, and certify what counts as correct modern Hebrew. Their institutional relevance and funding depend on the native-generation standard remaining the accepted measure of vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, ivrit_pedagogical_establishment, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, ivrit_pedagogical_establishment, agenda_setter).

% Children raised speaking Hebrew as a first language inherit full linguistic capital in the new national vernacular: schooling, employment, and civic belonging all run through native fluency. They benefit directly from the standard that treats their speech as the living language, while older or diaspora relatives' Hebrew literacy is treated as inert or archaic by comparison.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sabra_native_speaker_generation, beneficiary,
    moderate, biographical, mobile, national).

% Centuries of unbroken ritual reading, study, and prayer in Hebrew sustained the language across the diaspora without native daily speech. Under the native-generation standard this entire mode of transmission is reclassified as mere 'preservation' rather than life — its custodians (rabbis, yeshiva scholars, prayer communities) cannot exit the reclassification; their linguistic practice persists but is demoted in status regardless of what they do.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_hebrew_tradition, payer,
    powerless, civilizational, trapped, global).

% Communities whose actual vernacular was Yiddish (with Hebrew reserved for liturgy and study) find their linguistic arrangement declared non-vital by the new standard, accelerating institutional and social pressure to abandon Yiddish in favor of the reconstructed Hebrew vernacular in the new national context. Exit from this pressure requires either emigration away from Zionist institutions or generational language shift.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_yiddish_speaking_communities, payer,
    powerless, generational, constrained, continental).

% Orthodox and traditionalist communities that maintain intensive Hebrew literacy through study and prayer but resist or are ambivalent about vernacularization find their mode of Hebrew competence recategorized as non-native and therefore non-vital, regardless of the depth of their textual fluency. Some resist the secular vernacular project outright (e.g. historical objections to using the holy tongue for mundane speech).
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, religious_hebrew_reading_communities, payer,
    moderate, civilizational, constrained, global).

% Study the Hebrew case as the paradigm instance of language revival and debate whether 'vitality' should be defined by native acquisition alone or by a broader continuity criterion that includes sustained literary and liturgical use.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, comparative_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_building_project).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, teachable, unambiguous criterion — native daily generation — around which schools, the army, and civil administration can coordinate language planning, curriculum design, and national identity formation without relying on contested or graded notions of linguistic health.
% TRANSFER_FUNCTION: Moves symbolic and institutional legitimacy from liturgical and diasporic Hebrew/Yiddish practice toward the reconstructed national vernacular and the generation raised speaking it; moves pedagogical authority and resources toward the bodies that define and certify 'living' Hebrew.
% ABSENT_VOICES: Diaspora religious communities who maintained Hebrew literacy for centuries without native speech were not centrally represented in the committees that set the vitality criterion; their objections (that this desacralizes or trivializes the language of prayer) surface mainly in religious polemic literature, not in the linguistic-planning record.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion vanished as the operative standard, the state's institutional apparatus (schools, army, civil service) built around vernacular Hebrew would not disappear — but the STATUS HIERARCHY that ranks native speech above liturgical/literary competence would collapse, restoring liturgical and diasporic practice to parity as 'vital' rather than merely 'preserved.' Some sociolinguists argue the underlying vernacular already stabilized and no longer needs the criterion to survive; others argue the criterion still actively structures immigrant absorption policy today.
% FOUNDING_PROBLEM: Early Zionist language planners faced a diaspora Jewish population fragmented across Yiddish, Ladino, Arabic, and other vernaculars, with Hebrew functioning only as a liturgical and literary language nobody spoke natively; the project needed a single spoken national language to unify immigrants and constitute a modern national culture.
% FOUNDING_PROBLEM_CORROBORATION: Israeli linguists and state historians attest the founding problem (linguistic fragmentation impeding nation-building) was real and is now resolved — Hebrew is a thriving native vernacular. Scholars of Jewish liturgical history and diaspora sociolinguistics, writing from outside the Zionist institutional project, attest that the 'problem' was partly manufactured by devaluing an already-functioning multilingual diasporic equilibrium in which Hebrew served real communal functions without needing to be a native vernacular.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects moderate but real cost: the standard did not merely describe an already-existing native vernacular, it actively displaced and devalued existing modes of Hebrew competence (liturgical, literary, diasporic) that had sustained the language for centuries, redirecting institutional legitimacy and resources toward the vernacular-reconstruction project. Suppression (0.61) is substantial at the outset — early Zionist institutions actively discouraged Yiddish and other diaspora languages in schools and public life — and eases somewhat as the vernacular stabilizes and needs less active enforcement of the criterion itself, though the status hierarchy the criterion established persists. Theater ratio rises modestly (0.10 to 0.28) as the coordination function (a genuinely difficult vernacular-reconstruction and unification project) becomes increasingly overlaid with performative nationalist rhetoric about linguistic 'rebirth' and 'living language' framing, some of which serves identity-affirmation more than functional coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The Zionist state-building project and the pedagogical establishment sit at the beneficiary end: they set the criterion, administer the schools and institutions that enforce it, and collect the payoff of a unified national vernacular. The sabra native-speaker generation benefits by inheriting the linguistic capital the standard privileges. Liturgical Hebrew tradition, diaspora Yiddish communities, and religious reading communities sit at the target end: their actual, functioning modes of Hebrew transmission and competence are reclassified as non-vital regardless of their intrinsic continuity or richness, and their exit options are trapped or constrained — a rabbi cannot simply choose to have his textual fluency recognized as 'living' under this criterion, and a Yiddish-speaking immigrant community faced structural pressure toward Hebrew vernacular shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this as pure extraction: the coordination function is genuine — a fragmented, multilingual diaspora population did need a shared spoken vernacular to function as a modern nation-state, and the reconstruction of Hebrew as a native language is one of history's few successful deliberate language revivals. But the SAME structure that solved this coordination problem also asymmetrically extracted status and legitimacy from communities whose Hebrew competence was real but non-native, requiring active enforcement (school policy, army service, civil administration) to hold. Treating this purely as extraction would erase the genuine nation-building achievement; treating it purely as coordination would erase the real desacralization and marginalization the criterion imposed on liturgical and diasporic communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_naturalness,
    'Is ''native daily generation'' a linguistically principled, discoverable criterion for language vitality, or a criterion constructed to serve a specific nation-building project''s legitimacy needs?',
    'Comparative sociolinguistic analysis of other revived/maintained languages (e.g. Irish, Cornish, Sanskrit''s continued liturgical/scholarly use) assessing whether native-speaker acquisition is a necessary condition for a language''s functional survival, or whether sustained literary/liturgical/scholarly use independently constitutes a form of vitality.',
    'If native generation is the only linguistically defensible criterion, this reading''s classification is closer to a rope solving a real coordination problem with an accurate diagnostic. If the criterion is substantially a construction serving Zionist institutional legitimacy, the extractiveness and victim-designation (liturgical tradition, diaspora communities) are better understood as the criterion''s actual function rather than a side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_criterion_naturalness, conceptual, 'Whether the native-generation vitality criterion is a linguistic discovery or an institutionally constructed standard.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the three readings of the hebrew_vitality kernel (native_daily_reading, liturgical_reading, hybrid_continuity_reading) be reconciled into a single graded vitality measure, or do they rest on genuinely incommensurable premises about what ''life'' means for a language?',
    'Examine whether any sociolinguistic framework has successfully operationalized a graded/composite vitality index that scholars from all three traditions accept as capturing their respective claims, versus whether the disagreement is irreducibly normative (about what a language is FOR).',
    'If reconcilable, all three readings could in principle collapse into a single constraint with a composite ε; if genuinely incommensurable, the decomposition into three separate constraint stories (per the ε-invariance principle) is the correct and stable representation, and no single ε value could honestly represent ''Hebrew vitality'' as a topic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel''s sibling readings are reconcilable or structurally incommensurable.').

omega_variable(
    desacralization_harm_magnitude,
    'How much real harm did the vernacularization project inflict on liturgical Hebrew''s status and practice, versus how much liturgical use continued unaffected in religious communities regardless of the secular vitality criterion''s rhetoric?',
    'Historical and ethnographic study of Orthodox and traditionalist Hebrew textual practice across the 20th century: did liturgical/study use measurably decline, or did it persist in parallel with the vernacular''s growth, with only the SYMBOLIC status ranking changing?',
    'If liturgical practice persisted materially unaffected and only symbolic status shifted, the victim designation (liturgical_hebrew_tradition) should be understood mainly as a status/legitimacy harm rather than a practice-disruption harm, lowering the effective extraction somewhat. If liturgical practice itself measurably eroded due to the vernacular project''s institutional dominance, the extraction is more substantial than symbolic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desacralization_harm_magnitude, empirical, 'Whether the harm to liturgical tradition is primarily symbolic/status-based or a material practice disruption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__native_daily_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__native_daily_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__native_daily_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__native_daily_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__native_daily_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__native_daily_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__native_daily_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__native_daily_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__native_daily_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__native_daily_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__native_daily_reading, base_extractiveness, 80, 0.51).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__native_daily_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__native_daily_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__native_daily_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__native_daily_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(hebr_su_t60, hebrew_vitality__native_daily_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(hebr_su_t80, hebrew_vitality__native_daily_reading, suppression_requirement, 80, 0.61).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__native_daily_reading, suppression_requirement, 100, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_vitality kernel, each authored as a separate constraint story with its own ε, beneficiary/victim structure, and claimed type per the ε-invariance principle. native_daily_reading (this story) claims moderate ε (0.52) driven by institutional enforcement and desacralization costs; liturgical_reading would claim near-zero ε if ritual preservation is itself accepted as vitality with no imposed hierarchy; hybrid_continuity_reading would claim an intermediate ε reflecting a synthesis premise (both liturgical substrate and vernacular reconstruction were necessary). All three link to each other via affects_constraints to preserve the kernel-family structure for contamination/coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
