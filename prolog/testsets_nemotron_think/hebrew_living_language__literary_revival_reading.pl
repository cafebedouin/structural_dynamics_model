% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Hebrew Literary Revival (Haskalah)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The Haskalah (Jewish Enlightenment, ~1770s–1880s) produced a massive
 *   secular Hebrew literature — journals, poetry, novels, scientific and
 *   philosophical works — creating a modern Hebrew literary public sphere.
 *   This reading of the kernel 'hebrew_living_language' claims that Hebrew
 *   was a living language during this period because it supported generative
 *   written competence: writers produced new texts, readers understood them,
 *   and the language expanded to cover modern domains. No native speakers
 *   existed; vitality is located in the literary chain. The constraint is the
 *   literary production system itself, coordinating writers and readers
 *   around a shared written standard.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.1).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.2).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Literary Revival (Haskalah)").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0').
narrative_ontology:cs_kernel_codification('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', distributed).
narrative_ontology:cs_authority_grounding('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', practice).
narrative_ontology:cs_interpretation_layer_present('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0').
narrative_ontology:cs_reading_relation('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', foundational, literary_production_suffices_for_vitality).
narrative_ontology:cs_axiom_status(literary_production_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', literary_production_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', secondary, written_chain_constitutes_continuity).
narrative_ontology:cs_axiom_status(written_chain_constitutes_continuity, holdable).
narrative_ontology:cs_axiom_grounding('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', written_chain_constitutes_continuity, conventional).
narrative_ontology:cs_reference_frame('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', haskalah_literary_standard).
narrative_ontology:cs_drift_state('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', modern_hebrew_revival, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3c1d1072-f8c6-4499-b11f-cd0ef62eb6d0', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_writers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, jewish_literate_elite).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, hebrew_literary_vitality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maskilim (enlightened Jewish intellectuals) who produce secular Hebrew literature — poetry, journals, novels, scientific texts — creating a modern Hebrew literary canon. They choose Hebrew over Yiddish or European languages as a cultural-national project. They could write in other languages (mobile exit) but commit to Hebrew for ideological and cultural reasons.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_writers, agenda_setter,
    organized, biographical, mobile, regional).

% Hebrew-reading Jewish public in Central and Eastern Europe who consume the new Hebrew press, literature, and educational materials. They gain access to modern knowledge and a shared cultural discourse through Hebrew. They could read in Yiddish, Russian, German, or Polish (mobile exit) but participate in the Hebrew public sphere by choice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, jewish_literate_elite, beneficiary,
    moderate, biographical, mobile, regional).

% Religious authorities who view secular Hebrew writing as profanation of the holy tongue (lashon hakodesh). They are structurally excluded from the Haskalah literary project and would object to its claim that Hebrew vitality lies in secular literature. Their identity is fused with liturgical Hebrew (identity_locked exit), making engagement with the secular literary project nearly unthinkable.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditional_rabbinate, excluded,
    institutional, generational, identity_locked, regional).

% Late 19th/early 20th century Zionist activists (e.g., Ben-Yehuda, Second Aliyah pioneers) who inherit the Haskalah literary corpus and transform it into a spoken vernacular. They observe the literary revival as a necessary predecessor but redefine vitality around native speech. Their analytical seat examines the literary period as a historical phase.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, modern_hebrew_revivalists, observer,
    organized, generational, analytical, national).

% Historical linguists and sociolinguists who study the Haskalah as a case of language revitalization without native speakers. They evaluate the literary_revival_reading's claim that written generative competence constitutes language vitality against cross-linguistic typology.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, linguistic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Hebrew as a living literary language through coordinated production and consumption of secular Hebrew texts, providing a shared written standard for the Jewish intelligentsia across disparate diaspora communities.
% TRANSFER_FUNCTION: Moves cultural capital and linguistic innovation from writers to readers, establishing a modern Hebrew literary canon that sustains the language's vitality without native speakers.
% ABSENT_VOICES: Traditional religious authorities who view secular Hebrew literature as profanation of the holy tongue; Yiddishists who argue for Yiddish as the living vernacular; future native Hebrew speakers who would redefine vitality exclusively around generative daily speech.
% DISAPPEARANCE_RATIONALE: The Haskalah literary corpus provided the vocabulary, stylistic models, and institutional infrastructure (periodicals, publishing networks, schools) that the later spoken revival built upon. Without it, modern Hebrew would lack its literary foundation and the transition to native speech would have been far more impoverished.
% FOUNDING_PROBLEM: How to maintain Hebrew as a living cultural vehicle when it has no native speakers and faces competition from vernaculars (Yiddish, European languages) and religious restriction to liturgical use.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists and Hebrew literature scholars outside the Haskalah movement attest that the literary period was a response to the problem of Hebrew's vitality in the diaspora; the Haskalah's own manifestos (e.g., Mendelssohn's Bi'ur, Wessely's Divrei Shalom v'Emet) state the problem explicitly.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.1, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is very low (0.1) because the literary practice is voluntary and elite — no group is coerced into participating or paying rents. Suppression is low (0.2) because alternatives (Yiddish, European languages, liturgical Hebrew) remain fully available; the constraint does not actively suppress them. Theater ratio is low (0.1) — the literary output is genuine creative work, not performative compliance. Accessibility collapse is moderate (0.3) because the literary standard does not collapse the alternative of spoken vernaculars; they coexist. Resistance is moderate (0.4) because the traditional rabbinate and Yiddishists actively contest the claim that secular Hebrew literature constitutes language vitality.
 *
 * PERSPECTIVAL GAP:
 *   From the writers' seat (agenda_setter, organized, mobile), the constraint is a voluntary coordination project they lead — a Rope. From the literate elite's seat (beneficiary, moderate, mobile), it is a beneficial cultural good they consume — also a Rope. From the traditional rabbinate's seat (excluded, institutional, identity_locked), the same literary production appears as a profanation that threatens the sacred status of Hebrew — they experience it as a Snare-like intrusion, but they are not structurally bound by it. The engine will compute these divergent per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The writers and readers are beneficiaries (d near 0.0) — they gain cultural capital and linguistic community without extracting from others. The traditional rabbinate is excluded (d not applicable) — they bear no cost from the literary practice itself, only ideological offense. No victim set exists because participation is voluntary and exit to other languages is easy. The constraint's persistence depends on continued voluntary coordination, not enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining Hebrew vitality without native speakers) was eventually solved by the native_generation_reading's realization — spoken Hebrew revival. The literary_revival_reading's mandate did not outlive its function; it successfully bridged the gap and handed off to the next phase. No mandatrophy: the arrangement faded because its transitional function was completed, not because it persisted inertially.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_literary_revival,
    'How does the literary_revival_reading''s claim that written generative competence constitutes language vitality relate to the sibling readings of the same kernel?',
    'Comparative analysis of the three readings'' structural profiles: the literary reading has near-zero extractiveness and no victims; the liturgical reading has near-zero extractiveness but identity_locked participants; the native_generation reading has higher extractiveness (demanding massive behavioral change) and a clear victim set (those forced to acquire Hebrew).',
    'If the kernel is defined by the literary reading, it classifies as Rope; if by the native_generation reading, it classifies as Tangled Rope or Snare. The kernel''s classification depends on which reading''s structural profile is taken as definitive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_literary_revival, conceptual, 'Committer-frame ambiguity: which reading of ''hebrew_living_language'' sets the kernel''s structural profile?').

omega_variable(
    strict_reachability_ambiguity,
    'Does written generative competence without native speech satisfy the reachability criterion for a living language?',
    'Sociolinguistic typology: compare Haskalah Hebrew to other literary-only languages (Classical Arabic, Literary Chinese, Sanskrit) and assess whether linguists classify them as ''living'' during their literary-only phases.',
    'If reachability requires native speech, the literary_revival_reading''s claim is false and the constraint is a false summit (Mountain claim with beneficiaries). If reachability includes literary competence, the claim holds and the constraint is a genuine Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_ambiguity, conceptual, 'Ambiguity on whether ''living language'' requires native speakers or includes literary communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1770, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1770, hebrew_living_language__literary_revival_reading, theater_ratio, 1770, 0.05).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__literary_revival_reading, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_living_language__literary_revival_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1770, hebrew_living_language__literary_revival_reading, base_extractiveness, 1770, 0.08).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__literary_revival_reading, base_extractiveness, 1800, 0.09).
narrative_ontology:measurement(hebr_be_t1840, hebrew_living_language__literary_revival_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.1).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1770, hebrew_living_language__literary_revival_reading, suppression_requirement, 1770, 0.15).
narrative_ontology:measurement(hebr_su_t1800, hebrew_living_language__literary_revival_reading, suppression_requirement, 1800, 0.18).
narrative_ontology:measurement(hebr_su_t1840, hebrew_living_language__literary_revival_reading, suppression_requirement, 1840, 0.2).
narrative_ontology:measurement(hebr_su_t1860, hebrew_living_language__literary_revival_reading, suppression_requirement, 1860, 0.2).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__literary_revival_reading, suppression_requirement, 1880, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'hebrew_living_language'. The literary_revival_reading (this story) claims Hebrew vitality through Haskalah literature. The native_generation_reading claims vitality only through native speech. The liturgical_continuity_reading claims vitality through unbroken liturgical use. All three share the kernel but have different ε, beneficiary/victim structures, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
