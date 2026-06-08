% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
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
 *   human_readable: Domain Partition Reading: Kami and Buddhas in Separate Jurisdictional Domains
 *   domain: religious_studies/japanese_religious_ontology
 *
 * SUMMARY:
 *   The kami-buddha domain partition represents a foundational religious
 *   innovation of the Edo period (1603–1868), formalized through shogunal
 *   edicts and institutional reorganization. Prior to formal partition (Heian
 *   period), kami and buddhas were substantially conflated in folk practice,
 *   esoteric Buddhist doctrine, and shrine-temple complexes (jingūji,
 *   temple-shrine compounds with shared cult). Tokugawa policy enforced
 *   institutional separation: shrines were registered as kami-focused
 *   institutions under state supervision; temples were registered as buddhist
 *   institutions under separate supervision; dual ordination (shrine priest
 *   and monk) was prohibited; ideological justification shifted from
 *   doctrinal syncretism to ontological partition — kami govern mundane
 *   affairs (this-worldly vitality), buddhas govern transcendent affairs
 *   (death, liberation). This institutional framework persisted through the
 *   Meiji period, though state support shifted and shrine privilege waxed
 *   while temple authority waned. Post-war disestablishment (1945) formally
 *   disestablished state support for both, but the institutional partition
 *   remained the legal framework (Shinto vs Buddhism as separate religions in
 *   census categories). However, contemporary practice shows persistent drift
 *   toward syncretism and boundary-blurring: folk funerals incorporate kami
 *   observances; death rites invoke both Buddhist and kami elements; new
 *   religions dissolve the partition entirely. The domain_partition_reading
 *   is structurally what the Tokugawa state and clerical institutions
 *   actively maintained through enforcement; it is what contemporary practice
 *   partially rejects. The reading's authority grounding is lineage
 *   (transmission from Edo-period doctrinal synthesis) combined with
 *   extraction (both shrine and temple orders benefit from the partition by
 *   maintaining institutional monopolies on their respective domains). The
 *   reading is currently experiencing substantial drift — the founding
 *   problem (preventing religious conflict through domain separation) remains
 *   acknowledged, but the axiom (kami and buddhas govern ontologically
 *   distinct domains) is increasingly overridden by practice.
 *
 * KEY AGENTS:
 *   - Local Ritual Participants: Powerless/mobile actors engaging both shrines and temples based on ritual need; experience the partition as helpful coordination, not extraction
 *   - Shrine Priesthood (Shinto Authorities): Organized/constrained institutional actors; benefit from exclusive jurisdiction over kami and worldly vitality; maintain partition through doctrinal authority and gatekeeping
 *   - Buddhist Monastic Order: Organized/constrained institutional actors; benefit from exclusive jurisdiction over death and salvation; maintain partition through funeral monopoly and merit-transfer doctrine
 *   - Tokugawa State Administration: Powerful/arbitrage actors; collect stability from the partition (reduced religious conflict) but extract compliance through licensing, edicts, and enforcement; create subordinate institutional roles for both shrine and temple orders
 *   - Analytical Observer: Civilizational perspective; risks naturalizing the partition as ontological necessity when it is actually institutional contingency requiring sustained enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.15).
domain_priors:suppression_score(domain_partition_reading, 0.2).
domain_priors:theater_ratio(domain_partition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, rope).
narrative_ontology:human_readable(domain_partition_reading, "Domain Partition Reading: Kami and Buddhas in Separate Jurisdictional Domains").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/japanese_religious_ontology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(domain_partition_reading, '1c390628-1e36-442e-89cf-00558e8f3300').
narrative_ontology:cs_kernel_codification('1c390628-1e36-442e-89cf-00558e8f3300', formalized).
narrative_ontology:cs_authority_grounding('1c390628-1e36-442e-89cf-00558e8f3300', extraction).
narrative_ontology:cs_interpretation_layer_present('1c390628-1e36-442e-89cf-00558e8f3300').
narrative_ontology:cs_reading_relation('1c390628-1e36-442e-89cf-00558e8f3300', domain_partition_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c390628-1e36-442e-89cf-00558e8f3300', domain_partition_reading__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('1c390628-1e36-442e-89cf-00558e8f3300', foundational, kami_buddha_ontological_separation).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_separation, holdable).
narrative_ontology:cs_axiom_grounding('1c390628-1e36-442e-89cf-00558e8f3300', kami_buddha_ontological_separation, deontological).
narrative_ontology:cs_axiom('1c390628-1e36-442e-89cf-00558e8f3300', foundational, institutional_non_overlap_stability).
narrative_ontology:cs_axiom_status(institutional_non_overlap_stability, overridden).
narrative_ontology:cs_axiom_grounding('1c390628-1e36-442e-89cf-00558e8f3300', institutional_non_overlap_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('1c390628-1e36-442e-89cf-00558e8f3300', ontological_domain_partition).
narrative_ontology:cs_drift_state('1c390628-1e36-442e-89cf-00558e8f3300', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c390628-1e36-442e-89cf-00558e8f3300', '2026-02-27T00:00:00Z').
narrative_ontology:cs_kernel_id(domain_partition_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, local_ritual_communities).
narrative_ontology:constraint_beneficiary(domain_partition_reading, shrine_temple_authority).
narrative_ontology:constraint_beneficiary(domain_partition_reading, ritual_specialization_ecology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% LOCAL PARTICIPANT (ROPE): Experiences the partition as functional coordination — engaging kami for birth, marriage, seasonal transitions; engaging buddhas for death, memorial, otherworldly welfare. No coercion; participants navigate between shrines and temples freely based on ritual need. Net beneficiary of the clarity: knows which specialist to approach for which life-domain.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% SHRINE AUTHORITIES (ROPE): Benefit from clear domain boundary — kami jurisdiction over worldly vitality, fertility, seasonal cycles preserves shrine economic role and ritual authority. Active maintenance of the partition is low-cost coordination (teaching which observances belong where). Constrained exit because priesthood identity is fused with shrine institution, but no external suppression prevents schismatic rival shrines or defection to temples.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% MONASTIC AUTHORITIES (ROPE): Mirror shrine position — benefit from buddha jurisdiction over death, merit-transfer, salvation. Control over funeral ritual, memorial services, and afterlife doctrine sustains monastic economic base and soteriological authority. Constrained exit identical to shrines: institutional identity fused, but no suppression prevents institutional competition. The partition is actively maintained because both parties benefit from non-overlap.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% STATE AUTHORITY (TANGLED ROPE): Benefits from partition as coordination mechanism — reduces religious conflict by formalizing non-overlapping domains. Enforces the boundary through edicts and licensing: shrine vs temple registration, prohibition of dual ordination, state recognition conditional on domain compliance. State collects from coordination (social stability) but also extracts: both shrine and temple institutions become subordinate administrative units, their authority derives from state license, not autonomous legitimacy. Effective extraction moderate because institutions retain substantial agency within their respective domains.
constraint_indexing:constraint_classification(domain_partition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN): The partition emerges as an inevitable consequence of ontological necessity: kami are immanent powers constitutively tied to cyclic renewal and worldly vitality; buddhas are transcendent paths to liberation and post-mortem existence. The domains are logically separate because the fundamental nature of each power differs categorically. From this perspective, no enforcement is required — the partition is self-stabilizing because attempting to confuse domains produces incoherence. However, the structural data reveals this as a false summit: the partition requires constant active maintenance, institutional boundary-policing, and state enforcement. The 'inevitability' naturalizes what is actually a contingent institutional achievement.
constraint_indexing:constraint_classification(domain_partition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(domain_partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(domain_partition_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.15): The partition is minimally extractive when considered as a coordination mechanism. The base value reflects: (1) modest administrative overhead imposed by the state (licensing, registration, reporting) — approximately 0.08; (2) modest constraints on ritual specialists who cannot legally serve both shrine and temple — approximately 0.07; (3) participant experience is predominantly net-beneficial (clarity about which specialist to approach, low switching cost between shrine and temple). The metric is scaled by directionality: local participants (beneficiaries, mobile exit) experience negative effective extraction (subsidy from clarity); shrine and temple orders (beneficiaries, constrained exit) experience low positive extraction (institutional privilege is offset by identity fusion); the state (powerful, arbitrage) experiences high effective extraction (collects stability without substantial enforcement cost). The overall base value of 0.15 reflects that the constraint functions primarily as coordination with minimal parasitic overhead. SUPPRESSION (0.20): Moderate-low. The partition is enforced through: doctrinal authority (clerical teaching that the domains are separate), institutional gatekeeping (licensing system preventing unlicensed shrine-temple hybrids), state coercion (edicts and penalties for boundary violation during Edo period), and economic incentive (those complying with partition receive state support and social legitimacy). Suppression peaked at 0.35 during early Edo (when partition was novel and needed enforcement) and has declined to 0.02 by contemporary period (when partition is culturally normalized and state enforcement has ceased). The 0.20 value represents the mature Edo state of normalized institutional separation with reduced coercive overhead. THEATER RATIO (0.35): Moderate. The partition requires active teaching and ritual demonstration to maintain because the natural drift (documented across all centuries) is toward syncretism. Theater components: doctrinal instruction distinguishing kami and buddha domains; ritual specialists demonstrating separate procedures; institutional separation itself (shrine vs temple buildings, separate priesthoods) as theatrical display of partition. Theater has risen over the interval (0.25 → 0.65) because the functional justification (preventing conflict) has weakened after disestablishment, while the institutional form persists. By contemporary period (0.65), the partition is substantially performative — maintained through habit and institutional continuity rather than functional need. The contemporary theater rise signals the reading is approaching piton status (function atrophied, performance persists).
 *
 * PERSPECTIVAL GAP:
 *   PERSPECTIVAL DIVERGENCE: The local ritual participant sees rope (coordination with no coercion). The shrine priesthood sees rope (clear domain boundary benefits them). The monastic order sees rope (clear domain boundary benefits them). The state sees tangled_rope (coordination benefits them, but they also extract compliance and subordinate institutional authority). The analytical observer risks seeing mountain (natural law), but this is revealed as false summit by the structural data showing active enforcement, institutional boundary-policing, and state coercion. The gap between local experience (rope) and state position (tangled_rope) is small but significant: locals benefit without perceiving extraction; state collects without imposing severe costs. The gap between the natural law reading and structural reality is substantial and diagnostically important: the partition appears inevitable from a civilizational analytical distance, but close structural analysis reveals constant maintenance, state enforcement, and drift management. This is the classic false summit pattern: what appears as natural law from analytical distance is actually contingent institutional achievement requiring sustained work.
 *
 * DIRECTIONALITY LOGIC:
 *   DIRECTIONALITY COMPUTATION: Each perspective's d value (0.0 = full beneficiary to 1.0 = full target) is determined by structural position. Local participants are net beneficiaries of the clarity (d ≈ 0.2, low target status); shrine and temple orders are beneficiaries of domain exclusivity (d ≈ 0.25, beneficiary with institutional constraints); state is moderate beneficiary collecting stability (d ≈ 0.45, asymmetric beneficiary because it extracts compliance). The derived effective extraction χ = f(d) × scale(scope) shows: beneficiaries experience low or negative chi (receive subsidy from clarity); state experiences moderate positive chi (collects stability); no perspective shows high d or high chi because the base suppression is low (0.20) and theater is moderate (0.35). If suppression spiked (forcible participation in partition enforcement), or theater spiked (partition is pure performance with no coordination function), then d and chi would amplify. The current metrics reflect the stable Edo state where partition is normalized, enforcement is routine rather than coercive, and both institutional orders benefit substantially.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: The partition's founding mandate (prevent religious conflict by formalizing domain separation) remains acknowledged but structurally obsolete. Evidence: (1) post-Meiji disestablishment removed state enforcement, yet the partition persists in institutional form; (2) contemporary folk practice shows continuous drift toward syncretism without producing conflict (contradicting the conflict-prevention mandate); (3) the institutional partition persists despite its founding problem (sectarian conflict) being no longer active. The reading does NOT formally declare mandatrophy_resolved (that would require explicit authority acknowledgment), but the structural drift is visible in theater ratio climbing from 0.25 to 0.65 — the partition is increasingly maintained through performance (institutional continuity, clerical authority, cultural habit) rather than functional necessity. This is the signature of incipient mandatrophy: the mandate (prevent conflict) is being replaced by pure institutional inertia (maintain separation because institutional form exists). The reading is in transition from functional coordination (rope) toward degraded institutional theater (piton). The fact that multiple sibling readings coexist (syncretic fusion still actively held; pragmatic incoherence still operative in folk practice) is evidence that the partition's authority is eroding while institutional form persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_naturalization,
    'Is the kami/buddha domain partition an inevitable consequence of the powers'' intrinsic nature (natural law), or a contingent institutional achievement that required deliberate construction and sustained enforcement?',
    'Historical-comparative analysis: (a) pre-partition texts showing domains as contested or overlapping (Heian literature, esoteric Buddhism incorporating kami); (b) documents of boundary-construction (Edo period edicts, monastic regulations codifying domain separation); (c) contemporary practices where partition breaks down (funeral kami observances, kami-buddha syncretism in folk religion, contemporary new religions dissolving the boundary).',
    'If natural law: mountain classification confirmed; the partition is intrinsically stable. If contingent: false-summit detected; the analytical perspective naturalizes an institutional arrangement that requires sustained enforcement and would collapse without it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_boundary_naturalization, empirical, 'Whether domain partition is ontological necessity or institutional construction').

omega_variable(
    syncretic_pressure_suppression,
    'What mechanisms suppress the continuous drift toward syncretism (documented at all periods: kami names paired with buddha names, dual rites, shared cosmological space in folk belief)? Are these mechanisms coercive or merely persuasive?',
    'Ethnographic analysis of suppression mechanisms: clerical authority and doctrinal instruction, legal prohibition and licensing enforcement, economic incentive (those complying with partition receive state support; those blending suffer withdrawal of recognition), social shame and priestly gatekeeping. Measurement: compare compliance rates in periods of strong state enforcement vs. weak state presence; survey folk religious practice in contexts where state enforcement is minimal.',
    'If suppression is merely doctrinal persuasion: the partition is indeed rope (pure coordination). If suppression includes significant coercion or material consequence: the constraint is tangled rope or snare (asymmetric extraction protecting the partition). The state perspective (currently tangled rope) becomes more extractive if suppression mechanisms are revealed as coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_pressure_suppression, empirical, 'Suppression mechanisms maintaining domain partition').

omega_variable(
    kernel_reading_alternative_framings,
    'Which alternative readings of the kami-buddha kernel (syncretic_fusion_reading, pragmatic_incoherence_reading) would be structurally viable if this reading''s axioms were modified or overridden?',
    'Comparative CS analysis of the three readings: trace which axioms distinguish THIS reading from siblings; identify which axiom modifications would collapse this reading into a sibling; assess whether sibling readings represent live historical positions (doctrine actually held by clergy) or modern revisionist reframings of the tradition.',
    'If all three readings are live in historical clergy doctrine: kernel admits genuine pluralism; no reading forecloses another within the tradition. If only partition_reading is live and siblings are modern interpretations: the reading has functional authority grounding despite contemporary contestation. If any sibling is live among contemporary practitioners: the kernel''s reference frame is drifting and the reading faces repudiation pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Alternative readings of the kami-buddha kernel and their structural viability').

omega_variable(
    false_summit_detection_signal,
    'The analytical perspective classifies the partition as mountain (natural law) despite low suppression (0.20) and moderate theater ratio (0.35) — does this divergence signal a false summit?',
    'Engine computation: if base_properties classify as mountain but perspectives show active enforcement, state licensing, and institutional boundary-maintenance (indicating tangled_rope structure), the engine''s false-summit signature fires. The divergence between claimed mountain and structural data (requires_active_enforcement: false; but state perspective shows enforcement + extraction) indicates naturalization.',
    'False-summit detection would reclassify the analytical perspective from mountain to tangled_rope, revealing the ''natural law'' reading as a cover story for institutional contingency. The partition is sustained because both shrine and temple orders benefit; the state enforces boundaries to prevent conflict; the ''natural'' framing legitimizes institutional arrangements that would not survive without enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_signal, empirical, 'Detection of false-summit naturalization in the mountain perspective').

omega_variable(
    reformation_potential,
    'The reading axiomatizes domain separation as foundational. Under what conditions would this axiom become overridden — i.e., when would the reading itself acknowledge that partition is no longer operative?',
    'Historical observation: the partition has already been substantially overridden in contemporary Japan and in diaspora contexts (new religions, new age syncretism, funeral Buddhism incorporating kami elements, post-war disestablishment reducing state enforcement). Does the reading''s own authority tradition acknowledge this drift, or does it continue to assert partition despite practice contradicting it?',
    'If the reading acknowledges drift to overridden status: the axiom is holdable in historical context but overridden in contemporary practice — the reading has lost authority grounding. If the reading ignores drift and continues asserting partition: the reading exhibits performance (theater ratio higher than 0.35) and faces repudiation pressure from practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_potential, empirical, 'Contemporary override of the partition axiom in lived religious practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dompart_theater_t0_edo_doctrinal, domain_partition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dompart_theater_t150_edo_ritual_formalism, domain_partition_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement(dompart_theater_t200_meiji_disestablishment_crisis, domain_partition_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement(dompart_theater_t250_contemporary_maintenance_without_function, domain_partition_reading, theater_ratio, 250, 0.65).

% Extraction over time
narrative_ontology:measurement(dompart_extractiveness_t0_edo_early, domain_partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(dompart_extractiveness_t150_edo_mature, domain_partition_reading, base_extractiveness, 150, 0.18).
narrative_ontology:measurement(dompart_extractiveness_t200_meiji_disestablishment, domain_partition_reading, base_extractiveness, 200, 0.15).
narrative_ontology:measurement(dompart_extractiveness_t250_postwar_syncretism, domain_partition_reading, base_extractiveness, 250, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(dompart_suppression_t0_edo_early_edicts, domain_partition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dompart_suppression_t150_edo_normalized, domain_partition_reading, suppression_requirement, 150, 0.2).
narrative_ontology:measurement(dompart_suppression_t200_meiji_postwar, domain_partition_reading, suppression_requirement, 200, 0.05).
narrative_ontology:measurement(dompart_suppression_t250_contemporary, domain_partition_reading, suppression_requirement, 250, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(domain_partition_reading, pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(domain_partition_reading, kami_buddha_institutional_differentiation).
narrative_ontology:affects_constraint(domain_partition_reading, death_ritual_authority_monopoly).

% DUAL FORMULATION NOTE:
% The kami-buddha domain partition is the READING layer (this file: domain_partition_reading) analyzing a contested KERNEL (kami_buddha_ontology). The kernel has three readings with different epsilon values and structural data. This reading (partition) asserts clear ontological and institutional separation; epsilon=0.15 reflects coordination with minimal extraction. Sibling readings have different epsilon values: syncretic_fusion_reading has lower epsilon (fusion is more transparently coordination); pragmatic_incoherence_reading has lower epsilon (pragmatic use requires no enforcement). The three readings are linked by the kernel_id, not by simple causality. Downstream constraints (institutional_differentiation, death_ritual_monopoly) are APPLICATIONS of this reading's framework (how it shapes institutional structure), not SIBLINGS of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
