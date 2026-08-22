% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Revisable Translation (Scholarly/Publishing Authority)
 *   domain: religious/theological/textual_criticism
 *
 * SUMMARY:
 *   This constraint instantiates the revisable_translation_reading of the
 *   kjv_text_1611 kernel. The standing arrangement under contest is the
 *   modern textual-critical regime in which the King James Version is treated
 *   as a provisional, improvable translation whose authority is subordinate
 *   to scholarly manuscript evaluation and ongoing linguistic revision. The
 *   sibling readings are exclusive_inspiration_reading (KJV as exclusively
 *   inspired and inerrant) and functional_equivalence_reading (KJV and modern
 *   versions as complementary tools). This reading produces a constraint
 *   characterized by low suppressionâtranslation selection becomes consumer
 *   choiceâbut with extraction shifted to publishing-industry control of
 *   copyrighted modern translations and academic scholars as the arbiters of
 *   textual legitimacy.
 *
 * KEY AGENTS:
 *   - textual_scholarship_guild: Primary agenda-setter (institutional/constrained) â controls manuscript arbitration, translation committees, and seminary curricula
 *   - translation_publishers: Primary beneficiary (institutional/arbitrage) â captures revenue from copyrighted modern translations and study-bible editions
 *   - lay_bible_readers: Intended coordination beneficiary (moderate/mobile) â receives updated language and scholarly notes but faces marketplace fragmentation
 *   - kjv_traditionalist_communities: Primary payer (moderate/identity_locked) â bears the cost of deauthorization, marginalization, and institutional exclusion
 *   - local_congregations: Secondary payer (moderate/constrained) â bears material costs of curriculum replacement and liturgical retraining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.48).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.28).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Revisable Translation (Scholarly/Publishing Authority)").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/theological/textual_criticism").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '6837f64d-95c0-4f5c-89ea-1477dc579342').
narrative_ontology:cs_kernel_codification('6837f64d-95c0-4f5c-89ea-1477dc579342', fixed_text).
narrative_ontology:cs_authority_grounding('6837f64d-95c0-4f5c-89ea-1477dc579342', expertise).
narrative_ontology:cs_interpretation_layer_present('6837f64d-95c0-4f5c-89ea-1477dc579342').
narrative_ontology:cs_reading_relation('6837f64d-95c0-4f5c-89ea-1477dc579342', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('6837f64d-95c0-4f5c-89ea-1477dc579342', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('6837f64d-95c0-4f5c-89ea-1477dc579342', foundational, older_manuscripts_superior_to_received_text).
narrative_ontology:cs_axiom_status(older_manuscripts_superior_to_received_text, holdable).
narrative_ontology:cs_axiom_grounding('6837f64d-95c0-4f5c-89ea-1477dc579342', older_manuscripts_superior_to_received_text, empirically_contingent).
narrative_ontology:cs_axiom('6837f64d-95c0-4f5c-89ea-1477dc579342', foundational, scholarly_community_authorizes_revision).
narrative_ontology:cs_axiom_status(scholarly_community_authorizes_revision, holdable).
narrative_ontology:cs_axiom_grounding('6837f64d-95c0-4f5c-89ea-1477dc579342', scholarly_community_authorizes_revision, conventional).
narrative_ontology:cs_reference_frame('6837f64d-95c0-4f5c-89ea-1477dc579342', critical_textual_authority).
narrative_ontology:cs_drift_state('6837f64d-95c0-4f5c-89ea-1477dc579342', contemporary_publishing_market, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6837f64d-95c0-4f5c-89ea-1477dc579342', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, textual_scholarship_guild).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, translation_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, lay_bible_readers).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, kjv_traditionalist_communities).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, local_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edits the critical Greek and Hebrew texts used as bases for modern translations, chairs translation committees, peer-reviews manuscript claims, and trains clergy in seminaries. Career trajectories and institutional funding depend on the ongoing authority of textual criticism over fixed traditional texts.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, textual_scholarship_guild, agenda_setter,
    institutional, generational, constrained, global).

% Holds exclusive copyrights to popular modern translations, finances new editions and study bibles, markets them to churches through denomination channels, and earns recurring revenue as congregations replace older materials.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, translation_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Purchase translations for personal study; benefit from updated language and scholarly notes, but face a marketplace of dozens of versions with inconsistent verse renderings and no stable memorization text.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, lay_bible_readers, beneficiary,
    moderate, biographical, mobile, national).

% Organize around the King James Version as the preserved Word of God; their schools, churches, and publishing houses are built on the KJV text; they experience exclusion from mainstream evangelical institutions and academic biblical studies.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_traditionalist_communities, payer,
    moderate, generational, identity_locked, national).

% Buy pew bibles, Sunday school curricula, and projection licenses from publishers; when denominations adopt new translations, they bear the cost of retraining readers, reprinting materials, and harmonizing liturgy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, local_congregations, payer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, translation_publishers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates access to earlier manuscript witnesses and updated linguistic knowledge so that biblical texts can be rendered into contemporary language with claimed improvements in accuracy and comprehensibility.
% TRANSFER_FUNCTION: Moves textual authority from a fixed ecclesiastical text to an evolving scholarly consensus, and moves revenue from congregations and individual purchasers to publishers of copyrighted modern translations.
% ABSENT_VOICES: KJV-only communities and pre-modern textual traditionalists are structurally marginalized in academic discourse; their manuscript objections are framed as theological rather than textual, and their translation preferences are excluded from seminary curricula.
% DISAPPEARANCE_RATIONALE: If the scholarly revision regime vanished, churches and seminaries would revert to stable traditional texts, the biblical translation market would collapse to a few public-domain versions, and the livelihoods and authority structures built around ongoing textual criticism and new copyrighted editions would dissolve.
% FOUNDING_PROBLEM: The King James Version was translated from a limited set of late Greek manuscripts (the Textus Receptus) and in Early Modern English; as older manuscripts were discovered and English changed, the church needed a translation that reflected better source evidence and contemporary language.
% FOUNDING_PROBLEM_CORROBORATION: Secular manuscript libraries and museums attest to the existence of pre-KJV manuscript witnesses; however, the claim that these discoveries necessitate a publishing-controlled ongoing revision cycle is attested primarily by the benefiting scholarly guild and publishing houses, while KJV-traditionalist communities and some confessional theologians outside the guild dispute that the textual differences are doctrinally significant.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48) is moderate because the constraint shifts significant ongoing value to copyrighted translation publishers and sustains an academic guild, while still delivering genuine textual coordination. Suppression (0.28) is low because consumer choice is legally and socially preserved; the KJV remains available and no single modern translation is state-enforced. Theater ratio (0.30) reflects that a portion of scholarly revision activity and marketing of newer translations serves publishing imperatives more than purely textual necessity. Accessibility collapse (0.35) captures the partial delegitimization of the KJV in mainstream evangelical and academic spaces without its total disappearance. Resistance (0.60) is substantial because KJV-traditionalist communities actively contest the scholarly manuscript hierarchy. The metrics are authored descriptively and are independent of the tangled_rope claim; divergence between claim and computed output is the intended measurement signal.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (textual scholarship guild) experiences the constraint as a necessary scholarly service delivering accuracy and progress; the payer seats (KJV traditionalists, local congregations) experience it as the deauthorization of their stable text and a forced participation in a commodified revision cycle. Lay readers experience mixed costs and benefits. The engine will compute different per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The textual scholarship guild and translation publishers are structural beneficiaries: the guild gains institutional authority and career sustainment from ongoing revision, while publishers capture recurring revenue on copyrighted texts. KJV-traditionalist communities are identity-locked targets bearing the highest directional extraction through marginalization and deauthorization. Local congregations are constrained targets bearing material replacement costs. Lay readers are near-symmetric: they benefit from readability but pay into the publishing ecosystem and lose textual stability. No override is needed because the structural derivation from role, power, and exit options accurately maps these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâlimited manuscript basis and archaic languageâis contested but not dead, which prevents classifying the constraint as a pure piton or pure snare. However, the extraction has clearly migrated from ecclesiastical monopoly to publishing-industry control, which prevents classifying it as a pure rope. Because the coordination function (manuscript-based accuracy, contemporary language) is genuine and the extraction is asymmetric and actively enforced through copyright and academic gatekeeping, tangled_rope is the structurally faithful classification. It preserves both the coordination and the transfer without collapsing them into one another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    publishing_extraction_magnitude,
    'To what extent does the modern translation economy extract rents through copyright-controlled revision cycles rather than recover genuine coordination costs?',
    'Economic analysis of translation copyright terms, publisher profit margins, and frequency of new edition releases compared to rates of textual discovery.',
    'Would distinguish rope-like coordination from tangled_rope extraction; a wide gap would indicate that scholarly justification is partly cover for commercial cycling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publishing_extraction_magnitude, empirical, 'Whether publishing control constitutes rent extraction or cost recovery').

omega_variable(
    reading_sibling_boundary,
    'Does the revisable translation reading logically foreclose the exclusive inspiration reading, or do they coexist as incommensurable ecclesial practices?',
    'Examine whether any single community can simultaneously hold that the KJV is improvable based on better manuscripts and that it is exclusively inspired/inerrant without internal contradiction.',
    'If foreclosed, the kernel readings are irreconcilable frameworks; if coexisting, the constraint is one option in a plural religious marketplace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_boundary, conceptual, 'Logical relationship between revisable and exclusive readings').

omega_variable(
    textual_instability_lay_cost,
    'Does the continual revision of the base text and proliferation of translations create meaningful religious or educational costs for lay communities and memorization traditions?',
    'Survey of congregational material replacement costs, educational obsolescence, and liturgical stability metrics across denominations with high vs. low translation turnover.',
    'If costs are substantial, lay communities and congregations are stronger victim seats; if negligible, extraction is concentrated primarily on identity-locked traditionalists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_instability_lay_cost, empirical, 'Whether textual revision imposes diffuse costs on lay practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__revisable_translation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__revisable_translation_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__revisable_translation_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__revisable_translation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(kjv__tr_t80, kjv_text_1611__revisable_translation_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(kjv__tr_t100, kjv_text_1611__revisable_translation_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(kjv__tr_t120, kjv_text_1611__revisable_translation_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement(kjv__tr_t140, kjv_text_1611__revisable_translation_reading, theater_ratio, 140, 0.3).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__revisable_translation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__revisable_translation_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__revisable_translation_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__revisable_translation_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(kjv__be_t80, kjv_text_1611__revisable_translation_reading, base_extractiveness, 80, 0.39).
narrative_ontology:measurement(kjv__be_t100, kjv_text_1611__revisable_translation_reading, base_extractiveness, 100, 0.43).
narrative_ontology:measurement(kjv__be_t120, kjv_text_1611__revisable_translation_reading, base_extractiveness, 120, 0.46).
narrative_ontology:measurement(kjv__be_t140, kjv_text_1611__revisable_translation_reading, base_extractiveness, 140, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__revisable_translation_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__revisable_translation_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__revisable_translation_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(kjv__su_t60, kjv_text_1611__revisable_translation_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement(kjv__su_t80, kjv_text_1611__revisable_translation_reading, suppression_requirement, 80, 0.24).
narrative_ontology:measurement(kjv__su_t100, kjv_text_1611__revisable_translation_reading, suppression_requirement, 100, 0.26).
narrative_ontology:measurement(kjv__su_t120, kjv_text_1611__revisable_translation_reading, suppression_requirement, 120, 0.27).
narrative_ontology:measurement(kjv__su_t140, kjv_text_1611__revisable_translation_reading, suppression_requirement, 140, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kjv_text_1611 kernel, decomposed from the colloquial label 'the KJV' which conflates fixed-text authority, revisable scholarly text, and functional complementarity. Each reading carries a distinct epsilon, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
