% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
 *   human_readable: Domain Partition Reading: Kami and Buddhas Govern Separate Domains
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   The domain partition reading interprets the long coexistence of kami
 *   worship and Buddhism in Japan (shinbutsu shugo, c. 8th–19th centuries) as
 *   a functional division of labor rather than ontological syncretism. Kami
 *   govern this-world concerns — agriculture, fertility, local protection,
 *   life transitions — while buddhas and bodhisattvas govern afterlife
 *   concerns — death pollution, ancestor veneration, karmic salvation. This
 *   reading holds that the two systems occupied separate jurisdictional
 *   domains and could coexist without deep theological integration. The Meiji
 *   government's forced separation of shrines and temples (shinbutsu bunri,
 *   1868–) is interpreted as structurally straightforward under this reading:
 *   because the domains were never ontologically fused, they could be
 *   administratively separated without incoherence. The base extractiveness
 *   (0.18) reflects modest institutional extraction — temples monopolized
 *   lucrative funeral rites, shrines controlled some land and tax exemptions
 *   — but the extraction is low compared to what a coercive theological
 *   monopoly would produce. The theater ratio rises during the medieval
 *   period as honji suijaku doctrine (kami as manifestations of buddhas) adds
 *   a theological layer that may not match practitioners' functional
 *   experience, then falls slightly post-Meiji as the forced separation
 *   removes some of the theological superstructure. This constraint is one
 *   reading of a contested kernel. Two sibling readings interpret the same
 *   historical coexistence differently: the syncretic_fusion_reading sees
 *   deep ontological blending, and the incoherent_bundle_reading sees
 *   pragmatic coexistence of incompatible systems with no stable resolution.
 *
 * KEY AGENTS:
 *   - Village Ritual Practitioners: Moderate power / mobile exit — call on shrine priests for harvest rites, Buddhist clergy for funerals; experience the partition as useful specialization.
 *   - Shrine Administrators: Institutional power / constrained exit — benefit from controlling this-world ritual monopoly but lost some revenue to Buddhist funeral dominance; net beneficiaries with some constraints.
 *   - Temple Funeral Specialists: Institutional power / mobile exit — primary beneficiaries of Buddhist control over death pollution and afterlife anxiety; stable income from funerals and memorial services.
 *   - Aristocratic Households: Powerful / arbitrage exit — can patronize both systems or import alternatives; experience the partition as convenient simplification without being bound by it.
 *   - Analytical Observer: Sees the partition reading as a framing that minimizes ontological entanglement and predicts easy Meiji separation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.18).
domain_priors:suppression_score(domain_partition_reading, 0.22).
domain_priors:theater_ratio(domain_partition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(domain_partition_reading, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(domain_partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, rope).
narrative_ontology:human_readable(domain_partition_reading, "Domain Partition Reading: Kami and Buddhas Govern Separate Domains").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(domain_partition_reading, 'edf842b3-c3c6-46ab-a613-ed47bcc604b7').
narrative_ontology:cs_kernel_codification('edf842b3-c3c6-46ab-a613-ed47bcc604b7', implicit).
narrative_ontology:cs_authority_grounding('edf842b3-c3c6-46ab-a613-ed47bcc604b7', distributed).
narrative_ontology:cs_reading_relation('edf842b3-c3c6-46ab-a613-ed47bcc604b7', domain_partition_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('edf842b3-c3c6-46ab-a613-ed47bcc604b7', domain_partition_reading__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('edf842b3-c3c6-46ab-a613-ed47bcc604b7', foundational, separate_domain_governance).
narrative_ontology:cs_axiom_status(separate_domain_governance, holdable).
narrative_ontology:cs_axiom_grounding('edf842b3-c3c6-46ab-a613-ed47bcc604b7', separate_domain_governance, conventional).
narrative_ontology:cs_axiom('edf842b3-c3c6-46ab-a613-ed47bcc604b7', foundational, ontological_non_fusion).
narrative_ontology:cs_axiom_status(ontological_non_fusion, holdable).
narrative_ontology:cs_axiom_grounding('edf842b3-c3c6-46ab-a613-ed47bcc604b7', ontological_non_fusion, conventional).
narrative_ontology:cs_reference_frame('edf842b3-c3c6-46ab-a613-ed47bcc604b7', functional_jurisdictional_separation).
narrative_ontology:cs_drift_state('edf842b3-c3c6-46ab-a613-ed47bcc604b7', edo_period_honji_suijaku_elaboration, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('edf842b3-c3c6-46ab-a613-ed47bcc604b7', '').
narrative_ontology:cs_kernel_id(domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, village_ritual_practitioners).
narrative_ontology:constraint_beneficiary(domain_partition_reading, local_shrine_administrators).
narrative_ontology:constraint_beneficiary(domain_partition_reading, temple_funeral_specialists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE RITUAL PRACTITIONER (ROPE) — Experiences the partition as functional coordination: kami rituals for harvest, life transitions, local protection; Buddhist rites for funerals, ancestor veneration, afterlife security. Low extraction — the partition solves a real coordination problem (which specialist to call for which life event) without significant coercion.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: SHRINE ADMINISTRATOR (ROPE) — The domain partition coordinates institutional jurisdiction: shrines handle this-world concerns (agriculture, fertility, local governance rituals), temples handle afterlife concerns (funerals, memorial services, salvation). Constrained exit reflects that while the partition is generally beneficial, some shrines lost revenue when funeral rites became Buddhist monopoly. Still rope — the coordination function dominates.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TEMPLE FUNERAL SPECIALIST (ROPE) — Benefits from the partition: Buddhist institutional control over death pollution and afterlife anxiety creates stable temple income from funeral and memorial services. Mobile exit because temples can shift doctrinal emphasis or ritual offerings without challenging the partition itself. Low effective extraction — primary beneficiary of a genuine coordination.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ARISTOCRATIC HOUSEHOLD (ROPE) — Arbitrage-grade exit: can patronize both shrines and temples, commission private rituals, or import alternative Buddhist schools. Experiences the partition as useful simplification — outsource this-world ritual to shrines, afterlife security to temples — without being bound by it.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — The domain partition reading interprets shinbutsu shugo as pragmatic coexistence rather than ontological fusion. Kami and buddhas are not merged into a single cosmology; they govern separate jurisdictions with minimal theological entanglement. This reading sees the Meiji separation (shinbutsu bunri) as structurally easy because the domains were never ontologically fused — the partition could be administratively enforced without theological incoherence. Low extractiveness reflects that the partition coordinated institutional roles without heavy coercion.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).
:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The domain partition coordinates institutional roles (who handles which life event) with modest rent extraction — temples captured funeral revenue, shrines controlled some agricultural ritual fees — but the extraction is far lower than a coercive theological monopoly or forced conversion would produce. The partition was functional, not coercive. Most of the 'extraction' is legitimate payment for ritual services that solve real coordination problems (death pollution management, seasonal rites). Suppression (0.22): Low. Alternatives were not heavily suppressed — individuals and households could mix practices, patronize both systems, or import new Buddhist schools. The domain partition was weakly enforced; practitioners experienced it as convenience, not compulsion. The modest suppression reflects institutional gatekeeping (temple registry systems, shrine land monopolies) rather than ideological coercion. Theater ratio (0.35): Moderate. Some theological superstructure (honji suijaku doctrine layered over the functional partition) adds performative content that may not match how practitioners experienced the systems. The ratio rises during the Heian-Kamakura period as doctrinal synthesis elaborates, then falls slightly post-Meiji when the forced separation strips away some theological overlay. But the core partition itself is low-theater — the functional division (this-world vs afterlife) mapped to real practitioner needs.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives in this reading converge on Rope: the domain partition is experienced as coordination across power levels. Village practitioners see useful specialization. Shrine and temple institutions see jurisdictional clarity (with some constraints for shrines who lost funeral revenue, but still net positive). Aristocratic households see convenient simplification. The analytical observer sees a framing that predicts easy separation. The lack of perspectival gap is itself diagnostic: if the domain partition reading is correct, the structure was genuinely coordinative and non-coercive. If one of the sibling readings (syncretic fusion or incoherent bundle) is more accurate, we would expect greater extraction or suppression from some perspectives, producing a perspectival gap this reading does not show.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared agents are beneficiaries under this reading — the domain partition solved real coordination problems for practitioners, shrines, and temples. No victims are declared because the reading interprets the coexistence as functional rather than extractive. The analytical perspective aligns with the beneficiaries: low extraction, low suppression, Rope across perspectives. The lack of victim declaration is a structural claim of this reading (coexistence was mutualistic) and distinguishes it from the sibling readings (syncretic fusion might declare victims if ontological blending caused cognitive dissonance; incoherent bundle might declare victims if practitioners were trapped between incompatible systems).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves potential mandatrophy (the risk of mislabeling coordination as extraction or vice versa) by showing that the domain partition had a genuine coordination function: it allocated ritual jurisdiction in a way that matched practitioners' functional needs (kami for this-world, buddhas for afterlife). The low extractiveness and suppression distinguish this from a Snare (if the partition were coercive theological monopoly) or Tangled Rope (if it combined coordination with heavy extraction). The Rope classification holds across perspectives because the partition was experienced as mutualistic. Mandatrophy remains unresolved at the meta-level: we do not yet know whether this reading or one of its siblings (syncretic fusion, incoherent bundle) better captures the historical structure. The omegas document the irreducible uncertainties that would resolve this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_uncertainty,
    'Is this constraint one reading of the contested kernel ''shinbutsu_ontological_substrate'', or an independent constraint that happens to involve both kami and buddhas?',
    'Examine whether the domain partition reading presupposes a shared substrate (the kernel) that different readings interpret differently, or whether the partition is orthogonal to substrate questions. If orthogonal, this is not a kernel reading — it is a separate constraint.',
    'If this is a kernel reading: the cs_structure fields are warranted and the reading_relations to syncretic_fusion_reading and incoherent_bundle_reading are meaningful. If not a kernel reading: the cs_structure block should be removed and this constraint stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_uncertainty, conceptual, 'Whether the domain partition is a reading of the ontological substrate kernel or an independent constraint').

omega_variable(
    meiji_separation_ease,
    'Did the Meiji shinbutsu bunri separate easily because the domains were never deeply fused (validating this reading), or did it cause widespread disruption that this reading minimizes?',
    'Historical analysis of separation implementation: temple-shrine mergers dissolved, honji suijaku iconography removed, dual-function clergy forced to choose. Compare disruption in areas with strong honji suijaku practice vs areas with clearer partition. Quantify: number of forcible separations, destruction of syncretic artifacts, clergy re-assignments.',
    'If separation was structurally easy: this reading is empirically supported — the partition was functional, not ontological, and could be administratively undone. If separation caused major disruption: this reading underestimates the degree of institutional and theological entanglement, and one of the sibling readings (syncretic_fusion or incoherent_bundle) may better capture the structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_separation_ease, empirical, 'Whether Meiji separation ease validates the domain partition reading or contradicts it').

omega_variable(
    honji_suijaku_compatibility,
    'Is honji suijaku (original ground / manifest trace doctrine) compatible with the domain partition reading, or does it require the syncretic fusion reading?',
    'Theological analysis: honji suijaku claims kami are local manifestations of universal buddhas. Does this claim entail ontological fusion (syncretic reading), or can it coexist with domain partition (buddhas and kami remain in separate jurisdictions despite shared ontology)? Test: can a practitioner hold honji suijaku doctrine while maintaining strict domain separation in practice?',
    'If honji suijaku requires fusion: the domain partition reading is incompatible with the dominant medieval theological framework and was not a live reading during the honji suijaku period. If honji suijaku allows partition: the domain partition reading was continuously available as an alternative framing, even during periods of apparent syncretism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_compatibility, conceptual, 'Whether honji suijaku doctrine forecloses the domain partition reading').

omega_variable(
    cs_framing_alternative,
    'Is the kernel the ontological relationship between kami and buddhas, or is the kernel the institutional/legal framework governing shrine-temple relations?',
    'Identify what actors were committed to: an ontological claim (kami and buddhas share a substrate / are separate / are incoherent), or an institutional arrangement (jurisdiction over ritual domains, temple-shrine land rights, clergy licensing). If actors disputed ontology, the kernel is ontological. If they disputed jurisdiction while being indifferent to ontology, the kernel is institutional.',
    'If the kernel is ontological: the current cs_structure (authority_grounding: distributed, kernel_codification: implicit) is correct. If the kernel is institutional: authority_grounding should be extraction or practice (temple and shrine institutions defending jurisdictions), and kernel_codification might be formalized (bakufu or court edicts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Whether the kernel is ontological or institutional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(domain_part_theater_nara, domain_partition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(domain_part_theater_heian, domain_partition_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(domain_part_theater_kamakura, domain_partition_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(domain_part_theater_edo, domain_partition_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(domain_part_theater_meiji, domain_partition_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(domain_part_extract_nara, domain_partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(domain_part_extract_heian, domain_partition_reading, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(domain_part_extract_kamakura, domain_partition_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(domain_part_extract_edo, domain_partition_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(domain_part_extract_meiji, domain_partition_reading, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(domain_partition_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_substrate kernel. The readings differ in their interpretation of what coexistence meant: functional partition (this reading), ontological fusion (syncretic_fusion_reading), or pragmatic incoherence (incoherent_bundle_reading). All three readings model the same historical period but assign different ε values because they interpret the coordination and extraction mechanisms differently. The domain partition reading has the lowest ε (0.18) because it sees the coexistence as mutualistic coordination. The syncretic fusion reading likely has higher ε if ontological blending caused extraction (forced synthesis, suppression of non-syncretic practice). The incoherent bundle reading may have moderate ε if practitioners paid costs to navigate incompatible systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
