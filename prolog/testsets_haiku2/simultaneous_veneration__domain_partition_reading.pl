% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Reading: Kami-Buddha Functional Specialization
 *   domain: religious/cultural
 *
 * SUMMARY:
 *   In the domain-partition reading, Kami and Buddhas are functionally
 *   distinct entities governing separate domains: Kami address this-worldly
 *   prosperity, health, social order, and material welfare; Buddhas address
 *   transcendence, enlightenment, and afterlife salvation. Simultaneous
 *   veneration is not metaphysical confusion but domain-appropriate
 *   specialization. This reading was institutionally viable from roughly the
 *   8th to 19th centuries in Japan, operationalized through the coordinated
 *   priesthoods of Shinto and Buddhism, each legitimized within its proper
 *   sphere. The Meiji Restoration (1868) displaced this reading through
 *   state-mandated institutional separation (shinbutsu bunri), which
 *   foreclosed the syncretic option and required a choice between
 *   institutional allegiances. This is ONE READING of the contested kernel
 *   'simultaneous veneration' — the other readings (ontological fusion,
 *   pragmatic incoherence) represent fundamentally different interpretations
 *   of the same historical and theological facts.
 *
 * KEY AGENTS:
 *   - Practitioners (beneficiaries): gain coherent theological framework for dual veneration without forced ontological integration
 *   - Shinto priesthood (agenda-setter): administers kami domain, legitimizes material welfare authority
 *   - Buddhist priesthood (agenda-setter): administers buddha domain, legitimizes soteriological authority
 *   - Meiji state (excluded observer, post-interval): mandates separation, displaces the reading
 *   - Folk practitioners (beneficiaries, powerless): maintain lived practice aligned with the reading's logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Reading: Kami-Buddha Functional Specialization").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '67ba4e50-2a49-4263-b498-78687e7f3e2d').
narrative_ontology:cs_kernel_codification('67ba4e50-2a49-4263-b498-78687e7f3e2d', distributed).
narrative_ontology:cs_authority_grounding('67ba4e50-2a49-4263-b498-78687e7f3e2d', practice).
narrative_ontology:cs_interpretation_layer_present('67ba4e50-2a49-4263-b498-78687e7f3e2d').
narrative_ontology:cs_reading_relation('67ba4e50-2a49-4263-b498-78687e7f3e2d', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('67ba4e50-2a49-4263-b498-78687e7f3e2d', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('67ba4e50-2a49-4263-b498-78687e7f3e2d', foundational, kami_buddha_functional_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_functional_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('67ba4e50-2a49-4263-b498-78687e7f3e2d', kami_buddha_functional_distinctness, conventional).
narrative_ontology:cs_axiom('67ba4e50-2a49-4263-b498-78687e7f3e2d', foundational, domain_partition_coherent_without_fusion).
narrative_ontology:cs_axiom_status(domain_partition_coherent_without_fusion, holdable).
narrative_ontology:cs_axiom_grounding('67ba4e50-2a49-4263-b498-78687e7f3e2d', domain_partition_coherent_without_fusion, deontological).
narrative_ontology:cs_reference_frame('67ba4e50-2a49-4263-b498-78687e7f3e2d', dual_priesthood_functional_autonomy).
narrative_ontology:cs_drift_state('67ba4e50-2a49-4263-b498-78687e7f3e2d', pre_meiji_institutional_maturity, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('67ba4e50-2a49-4263-b498-78687e7f3e2d', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, folk_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practitioners of Shinto and Buddhism (or syncretic religion integrating both) benefit from the functional clarity of domain partition: kami address immediate material and social concerns (harvest, health, prosperity in this life), buddhas address transcendence and soteriological concerns (enlightenment, escape from samsara in afterlife). This partition allows practitioners to maintain coherent practical theology without forced integration of ontologically incommensurable claims.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Administers and legitimizes the kami domain through ritual practice, shrine maintenance, and doctrinal teaching. The domain partition reading upholds their institutional authority over this-worldly welfare concerns and validates their priesthood as essential to material prosperity. Identity-locked through professional training, lineage transmission, and self-conception as kami intermediaries.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shinto_priesthood, agenda_setter,
    organized, generational, identity_locked, national).

% Administers and legitimizes the buddha domain through sutra recitation, temple maintenance, and soteriological teaching. The domain partition reading validates their priesthood as essential to salvation and frames buddhist concerns as orthogonal to kami authority, protecting institutional autonomy over death-domain matters. Identity-locked through ordination, philosophical training, and self-conception as enlightenment mediators.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddhist_priesthood, agenda_setter,
    organized, generational, identity_locked, national).

% Scholars and theologians across centuries have attempted to reconcile or differentiate kami and buddha frameworks. The domain partition reading is one coherent intellectual position; observers monitor which reading gains institutional and popular endorsement, and how the boundary between domains is maintained or contested.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, philosophical_commentators, observer,
    analytical, civilizational, analytical, global).

% From the Meiji period onward, the state mandated institutional separation (shinbutsu bunri) as policy, foreclosing the syncretic option that the domain partition reading had operationalized. The state's enforcement created a boundary the reading had managed through conceptual means alone. Excluded from the collaborative institutional arrangement the reading describes because the state's enforced separation was a rival coordination.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, meiji_state_authority, excluded,
    institutional, generational, trapped, national).

% Village-level practitioners benefit from the domain partition framework as it legitimizes their practice of visiting both shrines and temples without doctrinal confusion. The reading provides narrative cover for simultaneous veneration and resolves the potential cognitive dissonance that ontological fusion or pragmatic incoherence readings would highlight.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, folk_practitioners, beneficiary,
    powerless, biographical, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the pragmatic problem of maintaining two distinct religious institutional systems (Shinto priesthood and Buddhist sangha) serving different soteriological functions within a single population, without requiring believers to hold contradictory metaphysical claims. Domain partition allows practitioners to assign each system to its appropriate sphere — kami to material welfare and social order, buddhas to transcendence and afterlife — and treat simultaneous veneration as domain-appropriate specialization rather than metaphysical confusion.
% TRANSFER_FUNCTION: Practitioners allocate religious resources (prayer, offerings, ritual participation, monetary support) across two institutional priesthoods according to domain relevance: to kami when seeking this-worldly benefits, to buddhas when seeking soteriological advancement. Both priesthoods maintain institutional authority and receive material support, legitimized by the reading's assignment of each to a functionally necessary domain.
% ABSENT_VOICES: Philosophical skeptics who doubted either the kami or buddha domains' reality (whether historical materialists, Hindu-influenced monists, or radical reformers) were never admitted to the institutional consensus this reading describes. The reading assumes both domains are real and functionally necessary; voices questioning that assumption are structurally absent from the coordination framework.
% DISAPPEARANCE_RATIONALE: If the domain partition reading disappeared and no coherent alternative replaced it, the institutional structure sustaining both Shinto and Buddhism would lose its legitimating narrative. Practitioners would face forced choice between systems, priesthoods would lose complementary institutional standing, and the syncretic religious culture that depended on this reading would reorganize. The Meiji separation, when institutional forces displaced this reading, caused exactly this reorganization — temples and shrines became legally and administratively separate, and the coordinating framework collapsed.
% FOUNDING_PROBLEM: How can a single religious population maintain dual priesthood systems (Shinto and Buddhism) without believers experiencing cognitive dissonance or being forced to choose one exclusive allegiance? How can material welfare (kami domain) and transcendence (buddha domain) be pursued by the same person within a coherent worldview?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is attested by Japanese religious historians (Kuroda Toshio, Grapard, Boucher), who document that practitioners prior to Meiji routinely performed kami and buddha rites in the same day without perceived contradiction. Folk religious practice continues to reflect this partition (visit shrine for this-year prosperity, temple for ancestral remembrance and afterlife). Contemporary syncretic practice that reconstructs dual veneration after Meiji suppression confirms the founding problem remains live wherever practitioners seek to escape enforced separation.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the domain partition reading posits no asymmetric transfer or rent collection — both priesthoods provide genuine services (kami priesthood delivers material welfare rituals, buddhist priesthood delivers soteriological instruction and ancestor care). The coordination is non-zero because both systems require institutional maintenance and practitioner support, but neither system is extracting from the other or from practitioners; practitioners benefit from the functional clarity the partition provides. Suppression is minimal (0.08) because the reading succeeds through intellectual coherence, not through enforced belief — practitioners can exit either tradition without losing access to the other. Theater is low-to-moderate (0.15) because the separation of functions is genuine (kami rites do address material concerns, buddhist rites do address transcendence), though some portion of priestly activity defends institutional autonomy rather than directly delivering the coordination benefit. Accessibility collapse is high (0.72) because once the domain partition framework is understood, alternatives (treating kami and buddhas as identical, or holding them as incoherent) become less accessible without explicit intellectual effort — the partition is intuitively coherent to practitioners within the framework. Resistance is low (0.18) because no powerful seat has interest in disputing the partition prior to Meiji; the state-enforced separation emerges AFTER the interval, when political ideology (emperor-worship via kami, foreign religion via buddhism) created institutional conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood seats (both Shinto and Buddhist), the domain partition reading is a victory: it secures institutional autonomy and legitimacy over a defined domain. From the practitioner seats, it is a coordination benefit: cognitive coherence without forced choice. From the state seat (Meiji onward), it is precisely the target for displacement — the reading's non-enforced coherence made it vulnerable to top-down institutional separation. The philosopher-observer seat sees the reading as one coherent intellectual position among three; its displacement by institutional force is empirical fact, not logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners are beneficiaries: they gain theological coherence and institutional choice. Both priesthoods are agenda-setters: they administer their respective domains and defend institutional autonomy. No stakeholder is trapped or identity-locked (except priesthoods via professional identity, which is normal for institutional actors). The directionality is symmetric for practitioners and both priesthoods — this is a genuine coordination rope, not an asymmetric extraction. The low extractiveness and suppression scores reflect this symmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition reading operationalized a coordinating framework (dual priesthoods serving different functions) without mandatrophy. The founding problem remained live throughout the interval because syncretic practice persisted and the partition continued to solve it. Mandatrophy enters when the Meiji state (external to this reading's structure) enforces separation and renders the coordinating framework inoperable. That separation is NOT part of this reading's classification; it is the external event that displaced the reading. The reading itself shows no theater-ratio accumulation or extraction-creep that would signal internal decay — the rising theater_ratio in the measurements reflects increased priestly emphasis on institutional differentiation (defending autonomy against fusion theories) as intellectual pressure rose, not functional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_distinctness_vs_ontological_identity,
    'Does functional distinctness (different domains, different purposes) entail or preclude ontological identity (same being viewed through different lenses)? Can kami and buddhas be functionally distinct but ontologically identical?',
    'Theological analysis of the two readings'' logical structure: whether ''same being, different functions'' is coherent without collapsing into pure functional equivalence or pure fusion.',
    'If functional distinctness is compatible with ontological identity, the domain_partition reading and ontological_fusion reading can coexist within a single theological framework. If they are mutually exclusive, they represent genuinely incompatible readings of the kernel, and the engine''s classification of reading_relations should reflect hard foreclosure rather than coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_distinctness_vs_ontological_identity, conceptual, 'Whether the partition reading''s core claim (functional distinctness) logically rules out the fusion reading''s core claim (ontological identity).').

omega_variable(
    institutional_pressure_vs_intellectual_coherence,
    'Did the domain partition reading persist as institutional consensus because it was intellectually coherent, or because institutional forces had not yet pressured practitioners to choose between systems?',
    'Comparative analysis of syncretic practice in regions with early institutional conflict (e.g., zones where Buddhist-Shinto competition was acute) vs. regions with cooperative priesthoods. If partition clarity correlates with low institutional pressure, the reading owes its viability to absence of enforced choice, not coherence.',
    'If the reading''s viability depended on institutional non-enforcement, it is fragile to state pressure — consistent with the Meiji displacement. If coherence explains persistence despite institutional pressure, it is more robust to external challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_pressure_vs_intellectual_coherence, empirical, 'Whether the partition reading''s institutional success was driven by intellectual force or institutional quiescence.').

omega_variable(
    honji_suijaku_compatibility,
    'Is the domain-partition reading compatible with honji-suijaku (original essence / trace manifestation) theory, or does honji-suijaku necessarily entail ontological fusion?',
    'Textual analysis of medieval Buddhist writings: whether any authoritative commentator endorsed both domain partition AND honji-suijaku without reducing one to the other.',
    'If compatible: the partition reading and fusion reading are not logically foreclosing; they are interpretive choices layered on the same metaphysical substrate. If incompatible: honji-suijaku interpretation directly entails fusion and forecloses partition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honji_suijaku_compatibility, empirical, 'Whether honji-suijaku doctrine is logically entailed by the fusion reading or whether partition can accommodate it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(simu_tr_t800, projected).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.12).
narrative_ontology:measurement_basis(simu_tr_t1000, projected).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.14).
narrative_ontology:measurement_basis(simu_tr_t1200, observed).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__domain_partition_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement_basis(simu_tr_t1500, observed).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__domain_partition_reading, theater_ratio, 1700, 0.16).
narrative_ontology:measurement_basis(simu_tr_t1700, observed).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement_basis(simu_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.08).
narrative_ontology:measurement_basis(simu_be_t800, projected).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.1).
narrative_ontology:measurement_basis(simu_be_t1000, projected).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.11).
narrative_ontology:measurement_basis(simu_be_t1200, observed).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement_basis(simu_be_t1500, observed).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1700, 0.13).
narrative_ontology:measurement_basis(simu_be_t1700, observed).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement_basis(simu_be_t1868, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The kernel 'simultaneous_veneration' decomposes into three structurally distinct constraints, each instantiating a different reading. Domain_partition_reading posits two functionally distinct domains with independent ε values (kami constraint ε~0.05, buddha constraint ε~0.08, partition itself ε~0.12). Ontological_fusion_reading posits a single underlying reality with two manifestation layers (ε~0.08, no partition extraction). Pragmatic_incoherence_reading posits genuine contradiction held together by institutional non-enforcement (ε~0.35, incoherence itself is the measured extractiveness). The three readings share a kernel but have incommensurable ε values and stakeholder structures — they are separate constraints linked by kernel identity, not competing observables of one constraint. Each reading's institutional displacement by Meiji separation supports the pragmatic_incoherence reading's diagnosis; neither partition nor fusion readings predict institutional break-apart as necessary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
