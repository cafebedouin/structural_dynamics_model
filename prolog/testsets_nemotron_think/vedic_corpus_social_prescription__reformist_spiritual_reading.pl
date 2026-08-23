% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Vedic Corpus as Spiritual Unity — Reformist Reading
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint story captures the reformist_spiritual_reading of the
 *   contested kernel 'vedic_corpus_social_prescription'. The reading asserts
 *   that Vedic texts (sruti) describe spiritual unity (advaita/brahman) and
 *   metaphorical cosmology (purusha sukta as cosmic allegory) with zero
 *   prescriptive social content. Varna references are either symbolic
 *   (qualities, not birth) or later smriti corruptions. This reading
 *   functions as a rope: it coordinates spiritual practice across caste
 *   lines, enables universalist Hindu identity, and requires no enforcement —
 *   practitioners adopt it voluntarily. The claimed_type is rope; metrics
 *   confirm low extraction, low suppression, genuine coordination. No victim
 *   set exists in this reading's own structural logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Corpus as Spiritual Unity — Reformist Reading").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/social_stratification/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '153f7971-169a-425e-950a-4ff1122fffb9').
narrative_ontology:cs_kernel_codification('153f7971-169a-425e-950a-4ff1122fffb9', fixed_text).
narrative_ontology:cs_authority_grounding('153f7971-169a-425e-950a-4ff1122fffb9', lineage).
narrative_ontology:cs_interpretation_layer_present('153f7971-169a-425e-950a-4ff1122fffb9').
narrative_ontology:cs_reading_relation('153f7971-169a-425e-950a-4ff1122fffb9', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('153f7971-169a-425e-950a-4ff1122fffb9', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('153f7971-169a-425e-950a-4ff1122fffb9', foundational, vedic_texts_prescribe_no_social_hierarchy).
narrative_ontology:cs_axiom_status(vedic_texts_prescribe_no_social_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('153f7971-169a-425e-950a-4ff1122fffb9', vedic_texts_prescribe_no_social_hierarchy, deontological).
narrative_ontology:cs_axiom('153f7971-169a-425e-950a-4ff1122fffb9', secondary, varna_is_metaphorical_or_later_corruption).
narrative_ontology:cs_axiom_status(varna_is_metaphorical_or_later_corruption, holdable).
narrative_ontology:cs_axiom_grounding('153f7971-169a-425e-950a-4ff1122fffb9', varna_is_metaphorical_or_later_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('153f7971-169a-425e-950a-4ff1122fffb9', original_spiritual_unity).
narrative_ontology:cs_drift_state('153f7971-169a-425e-950a-4ff1122fffb9', contemporary_reformist_resurgence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('153f7971-169a-425e-950a-4ff1122fffb9', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_unity_of_existence).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, metaphorical_nature_of_cosmology).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, non_prescriptive_character_of_sruti).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate and defend the reading that Vedic texts are exclusively spiritual and cosmological, not socially prescriptive. They publish translations, commentaries, and philological analyses. Their authority derives from scholarly credentials and institutional positions in universities and reformist organizations. Exit means shifting to other interpretive projects or academic fields.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_scholars, agenda_setter,
    organized, generational, mobile, global).

% Use this reading as a coordination framework for cross-tradition spiritual practice. It authorizes universalist, non-hierarchical engagement with Vedic material. They gain interpretive freedom and compatibility with modern egalitarian values. Exit means adopting other spiritual frameworks (Buddhist, secular mindfulness, other Hindu traditions).
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Hold the orthodox_varna_reading: Veda literally prescribes varna hierarchy as cosmic order. They control major temples, mathas, and traditional education. This reformist reading threatens their interpretive monopoly and social authority. Their exit is blocked by identity fusion — the reading constitutes their tradition's self-understanding.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_literalists, excluded,
    institutional, civilizational, identity_locked, global).

% Inherit the colonial_orientalist_reading's structural legacy: codified 'Hindu law' for governance. Modern state institutions (personal law boards, temple management acts) still operate on the premise that Vedic/Dharmashastra texts yield unified prescriptive law. This reading undermines that premise. They can pivot to other legal sources but lose the 'indigenous law' legitimacy claim.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_administrator_heirs, excluded,
    institutional, generational, arbitrage, global).

% Study the contest between readings as a case of hermeneutic conflict. They have no stake in which reading prevails but document the philological, historical, and sociological evidence. Their exit is trivial — they study other contests.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a non-hierarchical interpretive framework that allows spiritual practitioners across traditions to engage Vedic texts without endorsing social stratification, enabling cross-tradition dialogue and modern egalitarian practice.
% TRANSFER_FUNCTION: Moves interpretive authority from hereditary priestly institutions and state codifiers to individual practitioners and academic scholars; no material transfer, only epistemic repositioning.
% ABSENT_VOICES: Dalit and anti-caste activists who reject Vedic authority entirely (not reform); traditional householders for whom varna is lived practice not text; they are absent because the reformist frame assumes Vedic authority is worth reclaiming.
% DISAPPEARANCE_RATIONALE: If this reading vanished, spiritual practitioners would use other non-hierarchical frameworks (Bhakti universalism, Buddhist texts, secular ethics). The texts themselves and the social order would not rearrange — the reading is one interpretive option among many.
% FOUNDING_PROBLEM: Late 19th–early 20th century reformers needed to reconcile Vedic authority with colonial modernity, anti-caste critique, and universalist ethics without abandoning the tradition's textual anchor.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary reformist organizations (Arya Samaj offshoots, neo-Vedanta groups) attest the problem persists. Comparative religion scholars outside the beneficiary set (e.g., Wendy Doniger, Brian Smith) corroborate that the 'non-prescriptive Veda' reading solves a genuine hermeneutic problem for modern practitioners but is philologically contested.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_unchanged).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.12: the reading imposes no material costs, collects no rents. The only 'cost' is abandoning literalist identity, which is voluntary. Suppression 0.08: no coercion enforces this reading; it spreads by persuasion. Theater 0.15: some performative citation of texts occurs but the coordination function (spiritual practice compatibility) is genuine. Accessibility_collapse 0.25: alternative readings (orthodox, colonial, secular) remain fully available. Resistance 0.2: orthodox institutions resist but cannot suppress the reading. All metrics are stable across the interval — this reading emerged ~1875 and has maintained its structural profile.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist seat, this is pure coordination (rope). From the orthodox seat, this reading is a threat to their authority — but that threat comes from the *contest between readings*, not from this reading's internal operation. The engine computes per-seat types from structural data; the orthodox seat's experience of the *kernel contest* differs from this reading's internal classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars (agenda_setter) and spiritual practitioners (beneficiary) sit at d ≈ 0.1–0.2 (beneficiary end): they gain interpretive freedom and coordination. Orthodox literalists and colonial heirs (excluded) are not targets of extraction — they are excluded from this reading's coordination circle but not harmed by it. Their high power and identity_locked exit reflect their stake in rival readings, not victimhood under this one. The engine will compute low χ for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling Vedic authority with modernity/egalitarianism) remains live. The reading has not atrophied into piton — it actively coordinates practice. No mandatrophy to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'This constraint is one reading of a contested kernel — how does the kernel structure affect classification?',
    'Track all three readings as separate constraint stories linked by network.affects_constraints. Compare their ε, beneficiary/victim structures, and computed seat types.',
    'If the kernel contest itself creates extraction (e.g., state enforcement of one reading), that extraction belongs to the *enforcement constraint*, not to this reading. This reading''s ε stays low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Committer frame: this is one reading of vedic_corpus_social_prescription kernel; sibling readings are separate constraints.').

omega_variable(
    philological_grounding_of_non_prescriptivity,
    'Do Vedic texts genuinely lack prescriptive social content, or does this reading selectively ignore prescriptive passages (e.g., Purusha Sukta RV 10.90, varna-dharma in Upanishads)?',
    'Philological consensus on whether ''prescriptive social content'' is a category error for sruti texts vs. smriti; comparative analysis of RV 10.90, AV 19.6, Upanishadic varna references.',
    'If texts contain genuine prescriptive elements, this reading''s coordination function includes suppression of those elements — raising theater_ratio and possibly creating a victim set (those for whom the texts'' prescriptive force is real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(philological_grounding_of_non_prescriptivity, empirical, 'Whether the reformist reading''s textual basis is philologically honest or selectively constructed.').

omega_variable(
    spiritual_unity_as_ahistorical_projection,
    'Is ''spiritual unity'' a genuine Vedic concept or a modern universalist projection onto heterogeneous texts?',
    'History of ideas: trace ''spiritual unity'' from Upanishads through Shankara to Vivekananda to modern reformers; compare with textual diversity of Vedic corpus.',
    'If projection, the reading''s coordination function serves modern identity needs more than textual fidelity — theater_ratio rises, but still no extraction unless enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_unity_as_ahistorical_projection, conceptual, 'Whether the reading''s core hermeneutic key is textual or constructive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1875, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_reformist_tr_t1875, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1875, 0.1).
narrative_ontology:measurement(vedic_reformist_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(vedic_reformist_tr_t1925, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1925, 0.14).
narrative_ontology:measurement(vedic_reformist_tr_t1950, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1950, 0.13).
narrative_ontology:measurement(vedic_reformist_tr_t1975, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(vedic_reformist_tr_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(vedic_reformist_tr_t2025, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(vedic_reformist_be_t1875, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1875, 0.05).
narrative_ontology:measurement(vedic_reformist_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement(vedic_reformist_be_t1925, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1925, 0.1).
narrative_ontology:measurement(vedic_reformist_be_t1950, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(vedic_reformist_be_t1975, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1975, 0.11).
narrative_ontology:measurement(vedic_reformist_be_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(vedic_reformist_be_t2025, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(vedic_reformist_su_t1875, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1875, 0.05).
narrative_ontology:measurement(vedic_reformist_su_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1900, 0.07).
narrative_ontology:measurement(vedic_reformist_su_t1925, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1925, 0.08).
narrative_ontology:measurement(vedic_reformist_su_t1950, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1950, 0.08).
narrative_ontology:measurement(vedic_reformist_su_t1975, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 1975, 0.08).
narrative_ontology:measurement(vedic_reformist_su_t2000, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(vedic_reformist_su_t2025, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This reading, orthodox_varna_reading, and colonial_orientalist_reading form the vedic_corpus_social_prescription constraint family. All three share the kernel (Vedic corpus as authority source) but differ radically in ε: reformist ≈ 0.12 (rope), orthodox ≈ 0.7 (tangled_rope/snare — coordinates ritual order but extracts via birth hierarchy), colonial ≈ 0.6 (tangled_rope — coordinates administration but extracts via codified inequality). This reading influences both siblings by undermining their textual premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
