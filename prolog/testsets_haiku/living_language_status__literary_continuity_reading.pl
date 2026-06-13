% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language as Literary Continuity (Haskalah Reading)
 *   domain: sociolinguistics/cultural_nationalism
 *
 * SUMMARY:
 *   The Haskalah (Jewish Enlightenment, 18th–19th century) established a
 *   reading of language vitality centered on literary and intellectual
 *   productivity in Hebrew, independent of native-speaker status. This
 *   constraint formalizes that reading: a language is living if it sustains
 *   new literary work, scholarly discourse, and intellectual expression.
 *   Hebrew's vigorous literary output in periodicals, poetry, and fiction
 *   became the proof of vitality. This reading marginalizes other measures —
 *   native-speaker transmission, liturgical continuity, daily oral use — by
 *   treating them as preservation rather than vitality. The constraint
 *   benefits maskilim intellectuals and modern literary establishments by
 *   granting them authority over language health; it extracts from
 *   non-literary speakers and oral communities by making their language use
 *   invisible to the definition. The claim/metric divergence is intentional:
 *   CLAIMED as rope (elite coordination around a shared literary standard)
 *   while the metrics show substantial suppression (active exclusion of
 *   alternatives) and growing theater (the constraint's enforcement
 *   increasingly requires defending the literary criterion against
 *   native-speaker empirics).
 *
 * KEY AGENTS:
 *   - maskilim_intellectuals — elite agents setting the agenda by producing new Hebrew literature and declaring it the proof of vitality
 *   - secular_literary_establishment — institutional beneficiary, reproduces and enforces the literary-criterion through academia and publishing
 *   - non_literary_speakers — structurally excluded from the definition; their use does not count as evidence of language life
 *   - oral_culture_communities — identity-locked to Hebrew via religion/tradition but have no power to redefine vitality; treated as preservation, not vitality
 *   - liturgical_authority — explicitly excluded; ritual continuity is delegitimized as proof of living language
 *   - native_speaker_communities — empirically central (their speech emergence was the constraint's foundation) but not its primary stakeholders
 *   - academic_linguistics — observer seat, can measure divergence between this reading and others
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.38).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.52).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language as Literary Continuity (Haskalah Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/cultural_nationalism").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'fe279e9c-27dd-4938-a196-06e4e3411673').
narrative_ontology:cs_kernel_codification('fe279e9c-27dd-4938-a196-06e4e3411673', distributed).
narrative_ontology:cs_authority_grounding('fe279e9c-27dd-4938-a196-06e4e3411673', lineage).
narrative_ontology:cs_interpretation_layer_present('fe279e9c-27dd-4938-a196-06e4e3411673').
narrative_ontology:cs_reading_relation('fe279e9c-27dd-4938-a196-06e4e3411673', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe279e9c-27dd-4938-a196-06e4e3411673', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('fe279e9c-27dd-4938-a196-06e4e3411673', foundational, literary_productivity_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fe279e9c-27dd-4938-a196-06e4e3411673', literary_productivity_sufficient_for_vitality, empirically_contingent).
narrative_ontology:cs_axiom('fe279e9c-27dd-4938-a196-06e4e3411673', foundational, native_speaker_status_unnecessary_for_language_life).
narrative_ontology:cs_axiom_status(native_speaker_status_unnecessary_for_language_life, overridden).
narrative_ontology:cs_axiom_grounding('fe279e9c-27dd-4938-a196-06e4e3411673', native_speaker_status_unnecessary_for_language_life, empirically_contingent).
narrative_ontology:cs_reference_frame('fe279e9c-27dd-4938-a196-06e4e3411673', literary_productivity_as_vitality_measure).
narrative_ontology:cs_drift_state('fe279e9c-27dd-4938-a196-06e4e3411673', contemporary_native_speaker_dominance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fe279e9c-27dd-4938-a196-06e4e3411673', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_literary_establishment).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, oral_culture_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).

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
 *   Extractiveness is moderate and rising (0.18 → 0.38 over 200 years) because the maskilim's definition initially had to overcome significant resistance from traditional communities. The growth trajectory reflects institutionalization: as the literary criterion became embedded in academic institutions and national-identity narratives, it extracted more authority from alternative measures. Suppression is higher and accelerating (0.15 → 0.52) because maintaining the literary-criterion required active defense against the empirical pressure of native-speaker transmission — by the 20th century, the constraint had to suppress the competing reading (native speakers as the true measure) to sustain its authority. Theater ratio rises in parallel (0.22 → 0.41) because enforcing a literary standard in the face of native-speaker vitality requires performative activity — literary prizes, academic certification, nationalist narratives about Hebrew literature as the vehicle of Jewish modernity. All three metrics stabilize after 1900 (suppression, theater plateau at 0.52 and 0.41; extractiveness plateaus at 0.38) because by then the institutional apparatus had fully absorbed the literary criterion; further enforcement became maintenance rather than growth. The measurement series uses one shared time grid (1750, 1800, 1850, 1900, 1925, 1950), with projections for 1750 (pre-empirical) and observations thereafter.
 *
 * PERSPECTIVAL GAP:
 *   From the maskilim's institutional seat, the constraint is a genuine rope: it solved the problem of whether Hebrew could sustain intellectual work and coordinates around a shared literary standard. From non-literary speakers' seat, the same structure is a snare: they are enclosed in a definition of vitality they did not author and cannot exit, their language use is rendered invisible, and the enforcement machinery (academic gatekeeping, national-identity narratives) is built to suppress alternatives. From the observer seat (academic linguistics), both readings are visible simultaneously — the constraint serves genuine coordination functions while extracting authority from non-elite speakers. The engine computes per-seat types; this gap is the structural fact the computation measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim intellectuals are full beneficiaries (d → 0.0): they gain cultural authority and intellectual legitimacy by setting the criterion; their power is organized, their exit options are arbitrage (they can move between communities; they are not trapped). Non-literary speakers are full targets (d → 1.0): they bear the cost of exclusion from the vitality definition; their power is powerless, their exit is trapped (they cannot stop speaking Hebrew or switch to a different native language). Secular literary establishment sits between (d ~ 0.3): they benefit from the institutional apparatus but also depend on native speakers as their audience and cultural foundation; they have institutional power and mobile exit (they can invest in alternative language definitions if literary production declines). Oral-culture communities are partial targets (d ~ 0.75): they speak the language and transmit it orally, yet are excluded from the vitality definition; they are identity-locked and their power is moderate (they can resist and advocate, but lack institutional machinery to redefine the criterion). The engine will derive these directionalities from the beneficiary/victim declarations and the power/exit atoms; the commentary explains why the same constraint produces different d-values at different seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — can Hebrew sustain new intellectual work? — was genuinely live in 1750 and genuinely solved by 1850 (the Haskalah periodicals and literature proved the answer was yes). By 1900, the problem should have been dead, yet the constraint persists and strengthens. Why? The mechanism is institutional capture: the literary criterion, once useful for reviving Hebrew, became embedded in academia, publishing, and national identity such that defending it became self-perpetuating. The 1925-1950 plateau (extractiveness and suppression both flat) suggests the constraint has become piton-like — performatively maintained but increasingly theatrical. The theater_ratio of 0.41 at 1950 indicates significant performative activity relative to function. The mandate (proving Hebrew's vitality through literary work) is dead — native speakers and native transmission now carry far more of the language's actual vitality — yet the institutional machinery of the literary criterion persists. This is neither rope (the coordination problem is solved) nor snare (it is not consciously hidden or defended as extraction); it is piton. No directionality override needed; the metrics themselves describe the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literary_criteria_vs_speaker_vitality,
    'Is literary productivity genuinely a measure of language vitality, or is it a proxy for cultural authority that conflates elite intellectual activity with the health of the language system?',
    'Empirical: measure language vitality by multiple metrics (native-speaker transmission, community size, institutional use, literary output, educational transmission) and examine whether they track together or diverge. If native speakers and literary production decouple (native speakers thrive while literary output declines, or vice versa), the metrics are measuring different phenomena.',
    'If they diverge, the literary criterion is revealed as a measure of elite cultural authority, not language vitality — the constraint would be reclassified from rope to snare. If they track, the literary production criterion might capture something real about vitality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literary_criteria_vs_speaker_vitality, empirical, 'Whether literary productivity tracks actual language vitality or is a proxy for institutional power.').

omega_variable(
    kernel_reading_committer_choice,
    'Why does the literary_continuity_reading exist as a distinct constraint, rather than being collapsed into native_generation_reading or liturgical_preservation_reading?',
    'Commitment history: the maskilim authoritatively chose to measure vitality through literary work because (a) it allowed them to claim Hebrew was living without waiting for native-speaker mass adoption, which was not yet observable in the 18th century, and (b) it granted secular intellectuals cultural authority over language status, displacing religious and traditional authorities. This is a choice grounded in institutional position and power, not in discovered fact.',
    'Recognizing the reading as a choice, not a discovery, opens the possibility of contesting it — native speakers and religious communities become legitimate alternative authorities rather than failed speakers of the ''real'' (literary) language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_choice, conceptual, 'The literary reading is a reading of the contested kernel, authored by a specific seat (maskilim) with specific interests (cultural authority). The constraint''s persistence depends on this reading remaining unchallenged.').

omega_variable(
    oral_community_suppression_internalization,
    'Is the suppression of non-literary speakers'' language use structural (they are externally barred from literacy or publication) or internalized (they have internalized the literary criterion and now perceive their own speech as degraded)?',
    'Post-exclusion status: if non-literary speakers gain access to literacy and publication but do not use it (internalized suppression), or if they gain access and immediately begin producing literature and redefine themselves as vital (structural suppression), the distinction becomes clear.',
    'If internalized, the constraint''s effective suppression is higher than the authored 0.52 suggests — the targets carry the suppression with them after exit. If structural, the suppression is correctly measured and would drop sharply if barriers were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_community_suppression_internalization, empirical, 'Structural vs. internalized suppression in language-status hierarchy.').

omega_variable(
    native_speaker_emergence_paradox,
    'The constraint claims to measure language vitality without native speakers, yet it was born in a moment when native Hebrew speakers were just beginning to emerge in Palestine. Is the constraint genuinely speaker-independent, or does it parasitize on the empirical fact of native-speaker emergence while denying native speakers authority?',
    'Historical counterfactual: if the maskilim had written their literature in Yiddish or German, would they have declared those languages living by the same criterion? Or is the literary criterion plausible only because native Hebrew speakers were about to become empirically observable?',
    'If the criterion is speaker-parasitic, the constraint derives much of its legitimacy from native-speaker vitality while denying native speakers authority — a form of institutional capture. If the criterion is genuinely speaker-independent, literary production in Hebrew remains vitality proof even if native transmission had never emerged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_emergence_paradox, conceptual, 'Whether the literary criterion is speaker-independent or parasitizes on native-speaker emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1750, living_language_status__literary_continuity_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement(livi_tr_t1800, living_language_status__literary_continuity_reading, theater_ratio, 1800, 0.26).
narrative_ontology:measurement(livi_tr_t1850, living_language_status__literary_continuity_reading, theater_ratio, 1850, 0.32).
narrative_ontology:measurement(livi_tr_t1900, living_language_status__literary_continuity_reading, theater_ratio, 1900, 0.39).
narrative_ontology:measurement(livi_tr_t1925, living_language_status__literary_continuity_reading, theater_ratio, 1925, 0.41).
narrative_ontology:measurement(livi_tr_t1950, living_language_status__literary_continuity_reading, theater_ratio, 1950, 0.41).

% Extraction over time
narrative_ontology:measurement(livi_be_t1750, living_language_status__literary_continuity_reading, base_extractiveness, 1750, 0.18).
narrative_ontology:measurement(livi_be_t1800, living_language_status__literary_continuity_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(livi_be_t1850, living_language_status__literary_continuity_reading, base_extractiveness, 1850, 0.32).
narrative_ontology:measurement(livi_be_t1900, living_language_status__literary_continuity_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(livi_be_t1925, living_language_status__literary_continuity_reading, base_extractiveness, 1925, 0.37).
narrative_ontology:measurement(livi_be_t1950, living_language_status__literary_continuity_reading, base_extractiveness, 1950, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1750, living_language_status__literary_continuity_reading, suppression_requirement, 1750, 0.15).
narrative_ontology:measurement(livi_su_t1800, living_language_status__literary_continuity_reading, suppression_requirement, 1800, 0.28).
narrative_ontology:measurement(livi_su_t1850, living_language_status__literary_continuity_reading, suppression_requirement, 1850, 0.39).
narrative_ontology:measurement(livi_su_t1900, living_language_status__literary_continuity_reading, suppression_requirement, 1900, 0.48).
narrative_ontology:measurement(livi_su_t1925, living_language_status__literary_continuity_reading, suppression_requirement, 1925, 0.52).
narrative_ontology:measurement(livi_su_t1950, living_language_status__literary_continuity_reading, suppression_requirement, 1950, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, hebrew_language_revival__educational_transmission).

% DUAL FORMULATION NOTE:
% Part of the living_language_status constraint family. This reading (literary_continuity) is one of three distinct constraints grounded in the same kernel but producing different beneficiary structures and extraction profiles. The literary reading benefits elite intellectuals and suppresses non-literary speakers; the liturgical reading would benefit religious institutions and marginalize secular novelty; the native-generation reading would benefit native-speaker communities and treat literary/liturgical uses as secondary. Each reading should be authored as a separate constraint with its own ε and stakeholder structure; they are not alternative measurements of the same constraint but structurally distinct constraints grounded in competing readings of what language vitality means.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
