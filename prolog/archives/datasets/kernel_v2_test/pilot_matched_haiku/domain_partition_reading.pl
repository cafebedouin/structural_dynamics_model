% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domain_partition_reading, []).

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
 *   constraint_id: domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: Jurisdictional Separation of This-Worldly and Afterlife Domains
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The kami-buddha domain partition is a foundational principle in Japanese
 *   religious practice that allocates kami to this-worldly concerns
 *   (life-cycle events, seasonal festivals, agricultural blessings,
 *   place-based spirituality) and buddhas to transcendent concerns (death,
 *   afterlife, salvation, memorial services). This constraint is ONE READING
 *   of a contested kernel (kami_buddha_ontology) that admits multiple
 *   structurally distinct interpretations. The domain_partition_reading
 *   claims that kami and buddhas govern separate, non-overlapping
 *   jurisdictional domains with no hierarchy between them — a model of
 *   functional religious pluralism where both traditions coexist
 *   autonomously. This reading is instantiated in shrine and temple
 *   institutional structures, ritual calendars, and doctrinal texts that
 *   formalize the boundary. However, the partition coexists with widespread
 *   syncretic practice (kami invoked at temples, buddhas at shrines,
 *   households invoking both for overlapping concerns) and with alternative
 *   readings that emphasize fusion or pragmatic incoherence. The partition
 *   reading benefits local ritual autonomy and institutional independence for
 *   both shrines and temples, but it also constrains bereaved households into
 *   temple dependency for funerary and memorial services. The constraint
 *   exhibits low extractiveness (0.15) and low suppression (0.08) because the
 *   partition is genuinely functional for coordination and institutional
 *   autonomy, not primarily coercive. However, the theater ratio (0.22)
 *   reflects the gap between the doctrinal partition and syncretic practice —
 *   the partition is maintained as a formal principle even as practitioners
 *   routinely blur the boundary.
 *
 * KEY AGENTS:
 *   - Local Ritual Communities: Primary beneficiary (moderate/mobile) — experience the partition as clear coordination; know which ritual specialist to consult for which life event
 *   - Shrine Authorities: Institutional beneficiary (institutional/arbitrage) — partition legitimizes shrine autonomy over this-worldly kami rituals without subordination to temples
 *   - Temple Authorities: Institutional beneficiary (institutional/arbitrage) — partition legitimizes temple autonomy over death and afterlife concerns without subordination to shrines
 *   - Bereaved Households: Secondary victim (powerless/constrained) — experience the partition as both coordination (clear funerary pathway) and constraint (temple dependency, funerary fees, memorial obligations)
 *   - Syncretic Practitioners: Moderate agent (moderate/mobile) — routinely violate the partition in actual practice; experience the partition as performative rather than binding
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the partition as ontological necessity rather than institutional negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.15).
domain_priors:suppression_score(domain_partition_reading, 0.08).
domain_priors:theater_ratio(domain_partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, rope).
narrative_ontology:human_readable(domain_partition_reading, "Kami-Buddha Domain Partition: Jurisdictional Separation of This-Worldly and Afterlife Domains").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(domain_partition_reading, '1d699ca0-0d2d-4722-9a29-a55757e79282').
narrative_ontology:cs_kernel_codification('1d699ca0-0d2d-4722-9a29-a55757e79282', formalized).
narrative_ontology:cs_authority_grounding('1d699ca0-0d2d-4722-9a29-a55757e79282', lineage).
narrative_ontology:cs_interpretation_layer_present('1d699ca0-0d2d-4722-9a29-a55757e79282').
narrative_ontology:cs_reading_relation('1d699ca0-0d2d-4722-9a29-a55757e79282', domain_partition_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d699ca0-0d2d-4722-9a29-a55757e79282', domain_partition_reading__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('1d699ca0-0d2d-4722-9a29-a55757e79282', foundational, non_overlapping_jurisdictional_domains).
narrative_ontology:cs_axiom_status(non_overlapping_jurisdictional_domains, holdable).
narrative_ontology:cs_axiom_grounding('1d699ca0-0d2d-4722-9a29-a55757e79282', non_overlapping_jurisdictional_domains, conventional).
narrative_ontology:cs_axiom('1d699ca0-0d2d-4722-9a29-a55757e79282', foundational, institutional_autonomy_without_hierarchy).
narrative_ontology:cs_axiom_status(institutional_autonomy_without_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('1d699ca0-0d2d-4722-9a29-a55757e79282', institutional_autonomy_without_hierarchy, conventional).
narrative_ontology:cs_reference_frame('1d699ca0-0d2d-4722-9a29-a55757e79282', non_hierarchical_coexistence).
narrative_ontology:cs_drift_state('1d699ca0-0d2d-4722-9a29-a55757e79282', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d699ca0-0d2d-4722-9a29-a55757e79282', '').
narrative_ontology:cs_kernel_id(domain_partition_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, local_ritual_communities).
narrative_ontology:constraint_beneficiary(domain_partition_reading, shrine_temple_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE RITUAL PRACTITIONER (ROPE) — Experiences the partition as genuine coordination: kami handle life-cycle events (births, coming-of-age, marriages, harvests), buddhas handle death and afterlife. The separation solves a real coordination problem — practitioners know which ritual specialist to consult for which life event. Low extraction, clear functional benefit. Mobile exit: practitioners can shift emphasis between kami and buddha practices based on life circumstances without penalty.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: SHRINE AUTHORITY (ROPE) — Institutional beneficiary of the partition. Shrines maintain jurisdiction over this-worldly kami rituals (seasonal festivals, life-cycle rites, agricultural blessings). The partition legitimizes shrine autonomy without requiring subordination to temples. Arbitrage exit: shrines can emphasize or de-emphasize their role in the partition depending on regional power dynamics and economic conditions. Net beneficiary — the partition protects shrine institutional space.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEMPLE AUTHORITY (ROPE) — Institutional beneficiary of the partition. Temples maintain jurisdiction over death, funerary rites, and afterlife concerns (memorial services, ancestor veneration, Buddhist salvation narratives). The partition legitimizes temple autonomy without requiring subordination to shrines. Arbitrage exit: temples can emphasize or de-emphasize their role in the partition depending on regional power dynamics and economic conditions. Net beneficiary — the partition protects temple institutional space.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: BEREAVED HOUSEHOLD (TANGLED ROPE) — Experiences the partition as both coordination and constraint. The partition clarifies that death and afterlife are temple domain — the household must engage temple services for funerary rites, memorial services, and ancestor veneration. This is coordination (clear ritual pathway) but also extraction: temple fees for funerary services, ongoing memorial obligations, and spiritual dependency on temple mediation for afterlife concerns. Constrained exit: households cannot easily exit temple engagement after death in the family without social stigma and spiritual risk. Moderate extraction — the partition creates a captive market for temple services at a vulnerable moment.
constraint_indexing:constraint_classification(domain_partition_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the partition appears to reflect a natural ontological boundary: kami are immanent spirits of this world (nature, place, life-force), buddhas are transcendent principles of liberation and afterlife. The partition could be read as emerging naturally from the metaphysical structure of the two traditions. However, this reading risks naturalizing what is actually a negotiated institutional arrangement. The engine will likely compute this as a false summit, revealing that the 'natural ontological boundary' framing obscures the historical contingency of the partition's codification.
constraint_indexing:constraint_classification(domain_partition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SYNCRETIC RITUAL APPARATUS (PITON) — The partition is maintained through performative enforcement of a distinction that practitioners routinely violate in actual practice. Kami receive offerings at temples; buddhas receive prayers at shrines; households invoke both for overlapping concerns. The partition persists as a formal doctrine and institutional boundary despite widespread practical syncretism. Theater ratio is moderate (0.22) because the partition is genuinely functional for institutional autonomy, but the performative element is substantial — the partition is maintained as a formal principle even as practitioners blur the boundary in lived practice. Piton classification derives from the gap between the codified partition and the syncretic reality.
constraint_indexing:constraint_classification(domain_partition_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(domain_partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(domain_partition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(domain_partition_reading, TR),
    TR >= 0.70.

:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The partition is primarily a coordination mechanism — it clarifies ritual roles and institutional boundaries without substantial asymmetric extraction. The modest extractiveness reflects the bereaved household's constrained exit from temple services at a vulnerable moment (death in the family), but this is moderate rather than severe. The partition does not require coercive enforcement; it is maintained through institutional practice and doctrinal authority. Suppression (0.08): Low. The partition does not suppress alternatives — practitioners routinely invoke both kami and buddhas for overlapping concerns, and the partition does not prevent this. Suppression is minimal because the partition is not enforced through coercion or elimination of alternatives; it is a formal doctrinal boundary that practitioners navigate flexibly. Theater ratio (0.22): Low-moderate. The partition is functionally real — shrines and temples do maintain distinct ritual roles, and the partition does coordinate ritual practice. However, the theater element is present because the partition is maintained as a formal doctrine despite widespread syncretic practice that violates it. The gap between doctrinal partition and actual practice is the source of the theater ratio. Over the interval (0-400 years), the theater ratio has risen slightly (0.18 to 0.25) as modernization and secularization have made the partition less functionally necessary but more formally maintained as institutional boundary.
 *
 * PERSPECTIVAL GAP:
 *   The partition reading produces a perspectival gap between institutional and powerless agents. Shrine and temple authorities experience the partition as genuine coordination (rope) — it legitimizes their institutional autonomy and clarifies their ritual roles. Local ritual practitioners experience the partition as coordination (rope) — it helps them navigate ritual choices. But bereaved households experience the partition as mixed coordination and constraint (tangled_rope) — the partition clarifies the funerary pathway but also locks them into temple dependency at a vulnerable moment. The analytical observer risks seeing the partition as a natural ontological boundary (mountain) — kami as immanent, buddhas as transcendent — but the structural data reveals this as a false summit: the partition is a negotiated institutional arrangement, not a law of nature. The syncretic ritual apparatus perspective (piton) reveals that the partition is maintained performatively despite widespread violation in actual practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the partition. Shrine and temple authorities are beneficiaries with arbitrage exit (d ≈ 0.2-0.3) — they benefit from the partition's legitimization of their institutional autonomy and can adjust their emphasis depending on regional dynamics. Local ritual practitioners are beneficiaries with mobile exit (d ≈ 0.25-0.35) — they benefit from the partition's coordination function and can shift between kami and buddha practices without penalty. Bereaved households are victims with constrained exit (d ≈ 0.65-0.75) — they bear the cost of temple dependency for funerary services and cannot easily exit without social stigma and spiritual risk. The analytical observer has analytical exit (d ≈ 0.5) — the observer can recognize the partition as contingent institutional arrangement rather than natural law. The engine derives effective extraction (χ) from these d values, power levels, and scope; the partition's low base extractiveness (0.15) is amplified for trapped agents and damped for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading does not exhibit mandatrophy in the classical sense — the partition's founding mandate (to clarify shrine-temple institutional boundaries and enable functional religious pluralism) remains live and functional. However, the rising theater ratio (0.18 to 0.25 over the interval) suggests that the partition's functional necessity has declined as modernization and secularization have reduced shrine-temple competition and ritual demand. The partition persists as a formal doctrinal principle even as its practical necessity has diminished. This is not mandatrophy (mandate outlived) but rather a shift from functional necessity to institutional inertia — the partition is maintained because it is institutionally embedded, not because it solves an urgent contemporary problem. The piton perspective captures this: the partition is maintained as a formal principle through institutional practice and doctrinal authority, but the performative element has increased as the underlying functional problem has become less acute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_institutional_boundary,
    'Is the kami-buddha partition a reflection of genuine ontological difference (kami as immanent, buddhas as transcendent) or a negotiated institutional boundary created to manage shrine-temple competition?',
    'Historical analysis of partition codification: when was the partition formally articulated, by whom, and in response to what institutional pressures? Comparison with pre-partition texts and practices to identify whether the ontological distinction preceded or followed institutional separation.',
    'If ontological: the partition is a natural law (mountain from all perspectives). If institutional: the partition is a contingent arrangement (rope/tangled_rope from most perspectives, false summit from analytical). This is the core reading-level ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_institutional_boundary, conceptual, 'Whether the partition reflects ontological difference or institutional negotiation').

omega_variable(
    syncretic_practice_vs_doctrinal_partition,
    'How much does actual ritual practice violate the partition doctrine, and does this violation constitute a different constraint (syncretic_fusion_reading) or a degradation of the partition constraint itself (piton)?',
    'Ethnographic documentation of shrine and temple practices: frequency of kami invocation at temples, buddha invocation at shrines, household practices that cross the partition boundary. Measurement of theater_ratio through gap between doctrinal partition and actual practice.',
    'If violation is minimal: partition is robust (rope). If violation is substantial: partition is performative (piton) or the syncretic_fusion_reading is the actual operative constraint. This determines whether the partition reading or the fusion reading is the primary constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(syncretic_practice_vs_doctrinal_partition, empirical, 'Extent of syncretic practice violating the partition doctrine').

omega_variable(
    beneficiary_asymmetry_between_shrines_and_temples,
    'Do shrines and temples benefit equally from the partition, or does one institution extract more value from the jurisdictional separation?',
    'Comparative analysis of shrine vs temple economic resources, institutional autonomy, and ritual demand. Historical analysis of how the partition has been invoked to defend institutional interests in shrine-temple disputes.',
    'If equal benefit: partition is genuine coordination (rope from both institutional perspectives). If asymmetric: the partition may mask extraction by the benefiting institution (tangled_rope or snare from the disadvantaged institution''s perspective). This affects whether the partition is a true coordination mechanism or a cover story for institutional hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_asymmetry_between_shrines_and_temples, empirical, 'Whether shrine-temple benefits from the partition are symmetric or asymmetric').

omega_variable(
    reading_identity_partition_vs_fusion,
    'This constraint instantiates the domain_partition_reading of the kami_buddha_ontology kernel. The sibling syncretic_fusion_reading claims that kami and buddhas are fundamentally unified in Japanese religious practice, not separated. Can both readings coexist as live positions held by different parties, or does one logically foreclose the other?',
    'Examination of whether shrine and temple authorities, ritual practitioners, and doctrinal texts can simultaneously hold the partition view and the fusion view without internal contradiction. If both are held by different institutional actors or in different contexts, they coexist. If one is explicitly rejected as incoherent by the other, foreclosure may apply.',
    'If coexistence: both readings are live (coexists_with relation). If foreclosure: one reading''s core premise rules out the other (forecloses relation). This determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_partition_vs_fusion, conceptual, 'Whether partition and fusion readings coexist or foreclose each other').

omega_variable(
    mandate_obsolescence_partition,
    'Was the partition created to solve a specific historical problem (shrine-temple competition, doctrinal clarification, institutional autonomy)? If so, does that problem still exist, or has the partition outlived its mandate?',
    'Historical analysis of the partition''s origins and the problem it was designed to solve. Contemporary assessment of whether that problem remains live or has been superseded by new institutional dynamics (modernization, secularization, state regulation).',
    'If mandate is dead but partition persists: constraint exhibits mandatrophy (piton classification confirmed). If mandate is live: partition is functionally necessary (rope classification confirmed). This affects the terminal state assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_partition, empirical, 'Whether the partition''s founding mandate remains live or has become obsolete').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dompart_tr_t0, domain_partition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dompart_tr_t200, domain_partition_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(dompart_tr_t400, domain_partition_reading, theater_ratio, 400, 0.25).

% Extraction over time
narrative_ontology:measurement(dompart_be_t0, domain_partition_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(dompart_be_t200, domain_partition_reading, base_extractiveness, 200, 0.15).
narrative_ontology:measurement(dompart_be_t400, domain_partition_reading, base_extractiveness, 400, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(dompart_su_t0, domain_partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(dompart_su_t200, domain_partition_reading, suppression_requirement, 200, 0.08).
narrative_ontology:measurement(dompart_su_t400, domain_partition_reading, suppression_requirement, 400, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(domain_partition_reading, pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(domain_partition_reading, shrine_temple_institutional_autonomy).
narrative_ontology:affects_constraint(domain_partition_reading, funerary_service_market_extraction).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel admits three structurally distinct constraint readings: domain_partition_reading (this file), syncretic_fusion_reading, and pragmatic_incoherence_reading. Each reading has its own ε value, beneficiary/victim structure, and classification. The partition reading (ε=0.15, rope) emphasizes functional coordination and institutional autonomy. The fusion reading (ε lower, rope or mountain) emphasizes fundamental unity. The incoherence reading (ε higher, tangled_rope or snare) emphasizes unresolved contradiction. These are not three perspectives on one constraint; they are three distinct constraints instantiated by different readings of the same kernel. They are linked via network.affects_constraints to enable contamination propagation analysis across the reading family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
