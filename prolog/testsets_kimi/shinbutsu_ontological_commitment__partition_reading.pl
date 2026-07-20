% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhist Functional Domain Partition
 *   domain: religious studies/japanese history
 *
 * SUMMARY:
 *   This constraint story instantiates the partition reading of the shinbutsu
 *   ontological commitment kernel, which holds that pre-modern Japanese
 *   religion was structured by a functional division of labor between Shinto
 *   (life-cycle rituals) and Buddhism (afterlife care), without requiring
 *   ontological integration or doctrinal synthesis. The claim is one of three
 *   contested readings of the same historical kernel, alongside the syncretic
 *   reading (honji-suijaku unity) and the incoherence reading (no stable
 *   commitment). As a rope-class constraint, the arrangement coordinates
 *   ritual jurisdiction with minimal extraction or enforcement.
 *
 * KEY AGENTS:
 *   - Shinto priests (organized/constrained): Primary beneficiary of life-ritual jurisdiction
 *   - Buddhist clergy (organized/constrained): Primary beneficiary of death-ritual jurisdiction
 *   - Rural commoners (powerless/constrained): Lay beneficiaries of clear functional mapping
 *   - Syncretic practitioners (moderate/constrained): Excluded voice whose practice is marginalized by the partition frame
 *   - Religious studies scholars (analytical/analytical): Observer seat constructing the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.18).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhist Functional Domain Partition").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious studies/japanese history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '17ace9f6-118a-410d-b35a-c9aaaf55c3cb').
narrative_ontology:cs_kernel_codification('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', distributed).
narrative_ontology:cs_authority_grounding('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', practice).
narrative_ontology:cs_reading_relation('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', foundational, functional_complementarity_constitutes_coherence).
narrative_ontology:cs_axiom_status(functional_complementarity_constitutes_coherence, holdable).
narrative_ontology:cs_axiom_grounding('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', functional_complementarity_constitutes_coherence, empirically_contingent).
narrative_ontology:cs_axiom('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', foundational, doctrinal_synthesis_unnecessary_for_religious_order).
narrative_ontology:cs_axiom_status(doctrinal_synthesis_unnecessary_for_religious_order, holdable).
narrative_ontology:cs_axiom_grounding('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', doctrinal_synthesis_unnecessary_for_religious_order, empirically_contingent).
narrative_ontology:cs_reference_frame('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', functional_domain_partition).
narrative_ontology:cs_drift_state('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', medieval_syncretic_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17ace9f6-118a-410d-b35a-c9aaaf55c3cb', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, rural_commoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer shrine rituals for births, marriages, and seasonal festivals. Receive patronage and social standing from their jurisdiction over life-affirming ceremonies. Their institutional role depends on maintaining a distinct domain separate from funeral and memorial services.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_priests, beneficiary,
    organized, generational, constrained, national).

% Administer funerals, memorial rites, and afterlife care at temples. Receive patronage, land tenure, and doctrinal authority from their jurisdiction over death-related domains. Their economic and social position depends on maintaining control of the afterlife ritual market.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_clergy, beneficiary,
    organized, generational, constrained, national).

% Move between shrine and temple as life stages require: shrine for birth and marriage, temple for funerals and ancestral memorial. Benefit from a clear social map that assigns specific religious tasks to specific institutions without requiring personal theological synthesis. Have limited ability to opt out of the dual system entirely because social membership is enacted through these rituals.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, rural_commoners, beneficiary,
    powerless, biographical, constrained, local).

% Maintain practices that blend kami and buddha worship, administer joint rites, or teach that local deities are manifestations of buddhas. Their practice is rendered institutionally marginal or analytically invisible by narratives that insist on strict domain separation. They persist regionally but lack recognition in the partition frame.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, syncretic_practitioners, excluded,
    moderate, biographical, constrained, regional).

% Analyze historical texts, archaeological evidence, and ritual records to determine whether Japanese religion was functionally partitioned, metaphysically syncretic, or incoherent. Their classifications construct the competing readings of the same historical kernel and determine which practitioners are centered or excluded in the narrative.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates ritual jurisdiction between shrine and temple institutions so that birth, marriage, and seasonal rites are handled by Shinto specialists while death, memorial, and afterlife care are handled by Buddhist clergy, avoiding doctrinal conflict through functional complementarity.
% TRANSFER_FUNCTION: Moves ritual patronage and social-religious authority across the life cycle: from shrine to temple as individuals and families progress from birth through death, without requiring theological synthesis or institutional merger.
% ABSENT_VOICES: Syncretic priests of joint shrine-temple complexes who taught honji-suijaku metaphysics; Meiji-era nationalist scholars who would reject Buddhist involvement in 'Shinto' life events; commoners whose devotional practice may not have recognized the institutional boundary that the partition reading assumes.
% DISAPPEARANCE_RATIONALE: If the partition vanished, the clear mapping of life stages to ritual institutions would collapse; families would need to negotiate religious jurisdiction without the functional template, shrine and temple economies would compete directly for the same rites, and the historiographical narrative of Japanese religious coherence would require complete rewriting.
% FOUNDING_PROBLEM: How do two major religious traditions with distinct cosmologies, priesthoods, and ritual systems share a single society without persistent doctrinal conflict or destructive institutional competition?
% FOUNDING_PROBLEM_CORROBORATION: Social historians of Japanese religion attest to the functional division as an emergent property of institutional practice. Proponents of the syncretic reading dispute that this division was ever stable or primary, citing widespread shinbutsu-shugo evidence. Proponents of the incoherence reading argue that no single coordinating logic prevailed. Corroboration from outside the benefiting parties comes from modern academic historiography, though that historiography itself is divided.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects a coordination mechanism where benefits are distributed across shrine priests, Buddhist clergy, and laity rather than captured by a single party. Low suppression (0.18) indicates the partition persisted through functional convenience and social convention rather than active exclusion of alternatives. Low theater (0.10) signals the arrangement was primarily operational, not performative. Accessibility collapse is moderate (0.30) because once the functional template was accepted, alternatives (e.g., Buddhist weddings, Shinto funerals) became conceptually marginal but were not forcibly suppressed. Resistance is minimal (0.12) because all major parties benefited from clear jurisdictional boundaries.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (priests of both traditions) and beneficiary seats (laity) should compute similarly as low-d seats because all parties gain from coordination. The excluded seat (syncretic practitioners) computes differentlyâif treated as a victim, they would show higher d, but the partition reading minimizes their structural weight by treating them as exceptions rather than suppressed targets. The analytical observer seat (scholars) sits at analytical exit and sees the divergence between the partition frame and syncretic evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priests and Buddhist clergy are symmetric beneficiaries of the jurisdictional partitionâeach receives ritual patronage and institutional role from clear domain assignment. Rural commoners are also net beneficiaries, gaining a legible social map for life-cycle transitions. No victim group is declared because the constraint's operation does not extract asymmetrically from any identifiable party; syncretic practitioners are structurally excluded by the reading's framing but are not operationally suppressed by the constraint itself. Directionality for all named parties derives toward the beneficiary end (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists mislabeling the partition as a mountain (it is not a natural law but a historical coordination pattern) and resists mislabeling it as a snare (there is no concentrated extraction or coercive suppression). The rope classification captures that genuine coordination occurredâsociety needed a way to handle multiple religious traditionsâwithout masking that the coordination produced a specific distribution of ritual authority. If the founding problem (coexistence of Shinto and Buddhism) is judged dead, the constraint risks piton decay; however, the temporal measurements show stable low extraction, suggesting the arrangement maintained its coordination function throughout the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_syncretism_empirical_basis,
    'Was the functional separation of Shinto and Buddhism an accurate description of pre-modern Japanese practice, or a modern scholarly projection that obscures widespread syncretism?',
    'Archaeological and textual evidence of shrine-temple complexes versus separate ritual calendars; demographic studies of priestly affiliations.',
    'If syncretism was the dominant practice, the partition reading misdescribes the constraint and the true constraint is either syncretic or incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_syncretism_empirical_basis, empirical, 'Whether the partition reading accurately describes historical practice').

omega_variable(
    lay_experience_of_shinbutsu_boundary,
    'Did common practitioners experience Shinto and Buddhism as functionally separate domains, or as an undifferentiated religious landscape?',
    'Ethnohistorical analysis of lay ritual practice, pilgrimage patterns, and devotional texts from the medieval and early modern periods.',
    'If lay experience was undifferentiated, the partition reading describes elite institutional logic rather than lived religion, changing the scope and beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_experience_of_shinbutsu_boundary, empirical, 'Whether commoners recognized the institutional boundary').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the partition reading reflect an actual historical constraint, or is it a post-Meiji scholarly construct responding to modern category imperatives?',
    'Genealogy of the partition narrative in Japanese religious studies; identification of when ''Shinto'' and ''Buddhism'' became treated as distinct religions.',
    'If the partition is a modern construct, the constraint''s interval and agents are anachronistic, and the true kernel is modern disciplinary formation rather than pre-modern practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the partition reading is a modern historiographical construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 250, 0.06).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(shin_tr_t750, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 750, 0.09).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1000, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(shin_be_t250, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 250, 0.11).
narrative_ontology:measurement(shin_be_t500, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(shin_be_t750, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 750, 0.13).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1000, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, incoherence_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu ontological commitment kernel decomposes into three structurally distinct constraints: partition_reading (functional separation), syncretic_reading (metaphysical unity), and incoherence_reading (no stable commitment). Each reading carries a different epsilon and stakeholder structure. This reading claims low extraction via distributed coordination; siblings claim different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
