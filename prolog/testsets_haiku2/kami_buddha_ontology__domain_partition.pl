% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Domain Partition (Ontological Distinctness Reading)
 *   domain: religious/philosophical/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the domain-partition reading of the
 *   kami-buddha kernel: kami and buddhas are ontologically distinct entities
 *   governing separate functional domains. Shinto manages the living realm
 *   (birth, health, purity, community welfare); Buddhism manages death, the
 *   deceased, and transcendence. The reading presents this partition as
 *   natural complementarity rather than hierarchy or fusion. Shinto
 *   priesthood, Buddhist clergy, and state administration together enforce
 *   and benefit from the boundary. This is ONE reading of a contested kernel;
 *   two sibling readings (honji-suijaku monism and the incoherent-bundle
 *   critique) offer structurally different accounts of the same religious
 *   syncretism.
 *
 * KEY AGENTS:
 *   - Shinto priesthood: maintains kami domain, exclusive authority over life-cycle rituals and purity; identity-locked to the partition framework
 *   - Buddhist clergy: maintains buddha domain, exclusive authority over death rites and memorial services; identity-locked to the partition framework
 *   - Lay practitioners: navigate both systems pragmatically; constrained exit because forgoing either realm is culturally and spiritually costly
 *   - State religious administration: institutionalizes the partition through law and education; mobile enough to reorganize if political conditions change
 *   - Syncretic movements: excluded by the partition, historically advocated fusion
 *   - Scholars (domain-partition, honji-suijaku, incoherent-bundle readings): occupy different analytical seats, offering competing theoretical accounts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.42).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.28).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Domain Partition (Ontological Distinctness Reading)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious/philosophical/cultural").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, 'e7959683-7deb-4d04-ba6c-fdd934a4c64d').
narrative_ontology:cs_kernel_codification('e7959683-7deb-4d04-ba6c-fdd934a4c64d', distributed).
narrative_ontology:cs_authority_grounding('e7959683-7deb-4d04-ba6c-fdd934a4c64d', extraction).
narrative_ontology:cs_interpretation_layer_present('e7959683-7deb-4d04-ba6c-fdd934a4c64d').
narrative_ontology:cs_reading_relation('e7959683-7deb-4d04-ba6c-fdd934a4c64d', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('e7959683-7deb-4d04-ba6c-fdd934a4c64d', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('e7959683-7deb-4d04-ba6c-fdd934a4c64d', foundational, kami_buddha_ontological_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('e7959683-7deb-4d04-ba6c-fdd934a4c64d', kami_buddha_ontological_distinctness, deontological).
narrative_ontology:cs_axiom('e7959683-7deb-4d04-ba6c-fdd934a4c64d', foundational, functional_domain_complementarity).
narrative_ontology:cs_axiom_status(functional_domain_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('e7959683-7deb-4d04-ba6c-fdd934a4c64d', functional_domain_complementarity, conventional).
narrative_ontology:cs_reference_frame('e7959683-7deb-4d04-ba6c-fdd934a4c64d', complementary_dual_ontology).
narrative_ontology:cs_drift_state('e7959683-7deb-4d04-ba6c-fdd934a4c64d', contemporary_academic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7959683-7deb-4d04-ba6c-fdd934a4c64d', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, state_religious_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, practitioners_lay_population).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, practitioners_lay_population).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, dual_ontology_principle).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_complementarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the domain partition by claiming exclusive authority over kami worship, life-cycle rituals, purity practices, and living-world coordination. Teaches and enforces the distinction that kami govern the living realm, purification, seasonal cycles, and community welfare. Collects ritual patronage and maintains institutional independence by defending the boundary against Buddhist encroachment into kami-domain functions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, shinto_priesthood, beneficiary).

% Maintains the domain partition by claiming exclusive authority over death, afterlife, funeral rites, memorial services, and the deceased realm. Teaches that buddhas and bodhisattvas govern the deathless/transcendent realm while kami remain phenomenal guardians of life. Collects funeral patronage, operates temples as community ritual centers, and defends the boundary by resisting absorption into Shinto frameworks.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_clergy, beneficiary).

% Navigate both systems simultaneously: Shinto for birth, marriage, healing, seasonal rites, and living community welfare; Buddhism for death, funerals, ancestors, and transcendence. The domain partition provides clarity about which priesthood to approach for which need. They also pay both institutions through patronage, donations, and ritual fees. Exit would mean forgoing either life-cycle protection or death-rite legitimacy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, practitioners_lay_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, practitioners_lay_population, payer).

% Institutionalizes and enforces the domain partition through law, education policy, and administrative structure. Registers Shinto shrines and Buddhist temples in separate jurisdictions, teaches the distinction in schools, and allocates state ritual roles (kami veneration for state occasions; Buddhist services for state funerals). Benefits from the clarity the partition provides for governance but could reorganize religious administration differently if the partition dissolved.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, state_religious_administration, agenda_setter,
    institutional, generational, mobile, national).

% Historically advocated for or practiced fusion of kami and buddha under a single ontology (e.g., Shinbutsu-Kongoha, certain folk traditions). Are marginalized or suppressed by institutional enforcement of the partition. Would be empowered if the boundary were erased; their exclusion is what the enforcement apparatus maintains.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, syncretic_movements, excluded,
    moderate, biographical, constrained, local).

% Analyze and defend the domain-partition reading: kami and buddhas are structurally distinct, each governing its own ontological and functional domain without hierarchy or fusion. This is the reading instantiated by this constraint story. They provide theoretical justification for the institutional partition and critique the alternative readings (honji-suijaku monism as reductive, incoherent-bundle as dismissive).
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, scholars_theologians_domain_partition_reading, observer,
    analytical, civilizational, analytical, global).

% Defend the honji-suijaku reading: kami are manifestations (suijaku) of buddhas/bodhisattvas as their underlying ground (honji). This reading coexists with the domain-partition reading as a scholarly/theological alternative. Would gain prominence if the partition's conceptual boundary weakened. Their analysis is not suppressed but is marginalized in state-enforced institutional frameworks.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, scholars_honji_suijaku_reading, excluded,
    analytical, civilizational, analytical, global).

% Argue that shinbutsu-shugo (kami-buddha syncretism) is not a coherent kernel but an institutionally sustained bundle of contradictory commitments — simultaneous fusion and separation, hierarchies that flip, systematization imposed on unsystematized practice. They read the domain partition as one stabilized layer within a fundamentally incoherent arrangement. Their scholarly position contests the coherence of all readings including this one.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, scholars_incoherent_bundle_reading, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for religious practice by assigning kami to the life-realm (birth, health, purification, community welfare) and buddhas to the death-realm (funerals, ancestors, transcendence). This partition prevents doctrinal conflict between the traditions by giving each exclusive functional authority in its domain, allowing lay practitioners to use both without conceptual collision.
% TRANSFER_FUNCTION: Moves ritual authority and patronage: practitioners approach Shinto clergy for life-cycle rites and community welfare, approaching Buddhist clergy for death rites and memorial services. Each institution collects fees, donations, and social status from its functional domain. The state transfers religious administrative authority to both priesthoods, each in its assigned domain.
% ABSENT_VOICES: Syncretic movements that historically advocated fusion (Shinbutsu-Kongoha practitioners, folk traditions integrating both) are structurally excluded by the partition itself. Adherents of honji-suijaku monism (kami as manifestations of buddhas) are marginalized in state-enforced frameworks, though they remain live as a scholarly and theological position. Indigenous pre-Buddhist Shinto practitioners (who understood kami in pre-Buddhist terms) have no contemporary voice in this institutional arrangement.
% DISAPPEARANCE_RATIONALE: If the domain partition dissolved overnight, the institutional separation of Shinto and Buddhism would collapse. Lay practitioners would face doctrinal choice (fusion into Buddhism? separate kami worship? honji-suijaku reading?) rather than pragmatic dual use. Priesthoods would reorganize their territorial, economic, and doctrinal boundaries. The state would need to reorganize religious administration entirely. Centuries of ritual practice organized by the partition would need reconceptualization.
% FOUNDING_PROBLEM: Early medieval Japan (8th-13th centuries) faced the challenge of integrating Buddhism (arriving from China/Korea with its own cosmology) with indigenous Shinto (kami-centered, ancestor-linked, place-based). The domain partition solved the political and theological problem: Buddhism would govern the transcendent/deathless realm and the elite intellectual framework; Shinto would govern the immediate living realm, community welfare, and indigenous continuity. This allowed both traditions to coexist institutionally without doctrinal subordination of one to the other.
% FOUNDING_PROBLEM_CORROBORATION: Shinto and Buddhist institutional authorities attest the founding problem remains live: kami protect the living; buddhas govern the dead. However, scholars of Japanese religion (independent of both institutions) argue that the founding problem was substantially solved by the medieval period — institutional separation and functional complementarity are now maintained not because doctrinal integration is impossible, but because both priesthoods benefit from the status quo. Honji-suijaku theorists attest that the 'problem' was never theological but political — a way to avoid declaring Buddhism's philosophical supremacy while maintaining Christian-like institutional hierarchy.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint does extract patronage and institutional authority — practitioners pay both priesthoods, state privilege both religions — but the extraction is bounded and functional. Each priesthood genuinely solves problems in its domain (kami provide living-realm security and purity; buddhas provide death-rite legitimacy and ancestor care). Suppression is low-moderate (0.28) because the partition is maintained partly through institutional enforcement (legal separation, educational teaching, state privilege) but also through genuine lay acceptance — the partition works for most practitioners. Theater ratio rises modestly (0.18→0.31 over the interval) because the theological justification for the partition becomes increasingly theoretical as lay practice grows more pragmatic; the priesthoods invest more in maintaining the doctrinal boundary as its natural necessity erodes. Accessibility collapse is moderate-high (0.62): once practitioners understand the partition, alternatives (fusion, honji-suijaku, pure Shinto or pure Buddhism) exist intellectually but are constrained by institutional enforcement and cultural weight. Resistance is moderate-high (0.58) because syncretic movements, honji-suijaku theorists, and scholars of incoherence all mount sustained intellectual and occasional practical resistance. The measurements trace a plateau: extractiveness, suppression, and theater stabilize after the initial state-enforcement phase (intervals 0-16), suggesting the constraint has reached an institutional equilibrium where its maintenance requires steady but not escalating effort.
 *
 * PERSPECTIVAL GAP:
 *   The two agenda-setter seats (Shinto priesthood, Buddhist clergy) should compute as beneficiaries collecting patronage and authority — low directionality, potential subsidy-level extraction. But they are locked into identity-fusion with the partition itself; their institutional identity IS the domain partition, which means exit (leaving the framework) is identity-death, not mobility. The lay practitioner seat experiences constraint (mobile-constrained), not identity-lock, yet they benefit incidentally from the clarity the partition provides. State administration is the most mobile; it could reorganize religious authority entirely, but does not because the partition provides administrative convenience. This perspectival divergence reflects the difference between being structurally trapped in an arrangement (priesthoods) and being pragmatically served by it (practitioners, state).
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priesthood and Buddhist clergy are the primary beneficiaries: they collect patronage, maintain institutional authority, and defend their domains against encroachment. Their directionality is artificially low-to-symmetric because they are so thoroughly identity-locked to the partition that exit is conceptually impossible for them — they do not experience the constraint as an external force imposing extraction, but as the framework that constitutes their institutional being. Lay practitioners are near-symmetric: they benefit from clarity and access to both realms, but pay in patronage and are excluded from exit (forgoing either is too costly). State administration sits near the beneficiary end (convenience of administration, avoidance of doctrinal choice) but with moderate mobility — they could reorganize if incentives aligned. Excluded syncretic movements would have positive directionality (target position) if they had formal voice, but they are structurally silenced by the partition itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medieval integration of Buddhism into Shinto society) was live when the partition formed but is now contested. Shinto and Buddhist authorities still claim the partition solves an ongoing theological/organizational problem. But scholars of Japanese religion (external to both institutions) argue the founding problem is dead — institutional separation is now maintained by institutional inertia and mutual benefit, not by genuine necessity. The constraint does not show full mandatrophy (it still functions, still carries real extraction) but shows strong mandatrophy-pressure: the theoretical justification for the partition has weakened even as the institutional enforcement has stabilized. This is a classic zombie constraint in the mid-range: functional for the beneficiaries, pragmatically tolerable for practitioners, not yet corroding, but no longer grounded in the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_honji_suijaku_boundary,
    'Is the partition between kami and buddha domains ontologically real, or is it an institutional/pedagogical convenience layered over an underlying honji-suijaku monism?',
    'Textual and liturgical analysis: do Shinto and Buddhist texts and rituals themselves maintain the distinction, or do they presuppose or reference honji-suijaku interpenetration? Ethnographic study of how practitioners actually understand the relationship when not prompted by institutional teaching.',
    'If the partition is ontologically foundational, this reading stands as a Rope/genuine coordination. If it is institutional convenience over honji-suijaku, the constraint is Tangled Rope or Snare (the partition maintains a hierarchy while claiming equality). This is the most consequential ambiguity for this reading''s classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_partition_vs_honji_suijaku_boundary, conceptual, 'Whether domain partition is ontologically fundamental or institutional cover for underlying monism.').

omega_variable(
    suppression_mechanism_institutional_or_internalized,
    'Is the suppression of syncretic and fusion movements structural (legal prohibition, institutional exclusion) or internalized (practitioners believe the partition is natural)?',
    'Historical analysis of legal bans on syncretism; ethnographic study of practitioners who reject the partition; observation of how vigorously the partition is defended when not facing external pressure.',
    'If suppression is primarily structural, relaxing enforcement could allow fusion movements to reemerge — the constraint''s persistence is coercion-dependent. If internalized, the partition would persist even without enforcement — it would be a Rope rather than a Tangled Rope. Current evidence suggests mixed: state enforcement was substantial (especially Meiji period), but lay internalization is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_or_internalized, empirical, 'Whether partition suppression is structural coercion or internalized worldview.').

omega_variable(
    mandatrophy_resolution_status,
    'Has the founding problem (medieval integration of Buddhism) actually been solved, rendering the partition a dead-function zombie, or is it still live?',
    'Contrast testimony from Shinto and Buddhist authorities (attest problem is live) with independent scholars of Japanese religion (attest problem is dead or solved). Track whether institutional maintenance effort increases or decreases over time as enforcement costs rise.',
    'If the founding problem is dead, the constraint is Piton (maintained by institutional inertia despite lost function). If live, it is Tangled Rope or Rope. Current measurements show stabilization of suppression requirement after early-state enforcement, suggesting the problem may be dead but the arrangement persists by momentum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_resolution_status, empirical, 'Whether the founding problem persists or has been solved, rendering the constraint mandatrophic.').

omega_variable(
    kernel_reading_vs_kernel_critique_distinction,
    'Is the ''incoherent-bundle'' reading a rival reading of the same kernel (shinbutsu-shugo), or is it a meta-level critique that the kernel itself is not coherent enough to admit coherent readings?',
    'Philosophical analysis of whether incoherent-bundle is internally coherent (if so, it is a coherent reading claiming the kernel is incoherent); ethnographic/historical evidence of whether the bundle is actually sustained by institutionally managed contradictions or by something more fundamental.',
    'If incoherent-bundle is a coherent reading of an incoherent kernel, this reading (domain-partition) faces a rival that contests the kernel''s very structure. If it is a meta-critique that dissolves the kernel category, all three readings are potentially dissolved — the constraint''s entire theoretical ground shifts. This affects whether CS structure classification is even stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_vs_kernel_critique_distinction, conceptual, 'Whether incoherent-bundle is a reading-level rival or a meta-level kernel-critique.').

omega_variable(
    extraction_through_partition_maintenance,
    'To what degree does the extractiveness (0.42) represent genuine coordination costs, and to what degree does it represent institutional maintenance of boundaries that have become functionally unnecessary?',
    'Cost accounting: compare the real cost of maintaining separate priesthoods, separate ritual infrastructure, and state religious administration to the cost that would be incurred if religious authority were unified (or if the partition were truly pragmatic/unsystematized). If separation costs significantly more than unified alternatives, extraction via partition is happening.',
    'If extractiveness is mostly coordination cost, the constraint is Rope. If substantial extractiveness represents boundary-maintenance rent-seeking, it is Tangled Rope or Snare. Theater ratio''s rise (0.18→0.31) suggests boundary maintenance is becoming more performative over time, which would suggest higher extractiveness than 0.42 if re-measured today.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_through_partition_maintenance, empirical, 'How much of the constraint''s extractiveness is coordination cost versus institutional rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t4, kami_buddha_ontology__domain_partition, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(kami_tr_t4, observed).
narrative_ontology:measurement(kami_tr_t8, kami_buddha_ontology__domain_partition, theater_ratio, 8, 0.25).
narrative_ontology:measurement_basis(kami_tr_t8, observed).
narrative_ontology:measurement(kami_tr_t12, kami_buddha_ontology__domain_partition, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(kami_tr_t12, observed).
narrative_ontology:measurement(kami_tr_t16, kami_buddha_ontology__domain_partition, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(kami_tr_t16, observed).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__domain_partition, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(kami_tr_t20, observed).
narrative_ontology:measurement(kami_tr_t24, kami_buddha_ontology__domain_partition, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(kami_tr_t24, observed).
narrative_ontology:measurement(kami_tr_t28, kami_buddha_ontology__domain_partition, theater_ratio, 28, 0.31).
narrative_ontology:measurement_basis(kami_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t4, kami_buddha_ontology__domain_partition, base_extractiveness, 4, 0.37).
narrative_ontology:measurement_basis(kami_be_t4, observed).
narrative_ontology:measurement(kami_be_t8, kami_buddha_ontology__domain_partition, base_extractiveness, 8, 0.39).
narrative_ontology:measurement_basis(kami_be_t8, observed).
narrative_ontology:measurement(kami_be_t12, kami_buddha_ontology__domain_partition, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(kami_be_t12, observed).
narrative_ontology:measurement(kami_be_t16, kami_buddha_ontology__domain_partition, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(kami_be_t16, observed).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__domain_partition, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(kami_be_t20, observed).
narrative_ontology:measurement(kami_be_t24, kami_buddha_ontology__domain_partition, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(kami_be_t24, observed).
narrative_ontology:measurement(kami_be_t28, kami_buddha_ontology__domain_partition, base_extractiveness, 28, 0.42).
narrative_ontology:measurement_basis(kami_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(kami_su_t0, projected).
narrative_ontology:measurement(kami_su_t4, kami_buddha_ontology__domain_partition, suppression_requirement, 4, 0.18).
narrative_ontology:measurement_basis(kami_su_t4, observed).
narrative_ontology:measurement(kami_su_t8, kami_buddha_ontology__domain_partition, suppression_requirement, 8, 0.21).
narrative_ontology:measurement_basis(kami_su_t8, observed).
narrative_ontology:measurement(kami_su_t12, kami_buddha_ontology__domain_partition, suppression_requirement, 12, 0.24).
narrative_ontology:measurement_basis(kami_su_t12, observed).
narrative_ontology:measurement(kami_su_t16, kami_buddha_ontology__domain_partition, suppression_requirement, 16, 0.27).
narrative_ontology:measurement_basis(kami_su_t16, observed).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__domain_partition, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(kami_su_t20, observed).
narrative_ontology:measurement(kami_su_t24, kami_buddha_ontology__domain_partition, suppression_requirement, 24, 0.28).
narrative_ontology:measurement_basis(kami_su_t24, observed).
narrative_ontology:measurement(kami_su_t28, kami_buddha_ontology__domain_partition, suppression_requirement, 28, 0.28).
narrative_ontology:measurement_basis(kami_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__domain_partition, 0.12).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kami-buddha kernel (shinbutsu-shugo). The kernel admits three structurally distinct readings: domain-partition (this story — kami and buddhas are ontologically distinct, functionally complementary), honji-suijaku-monism (kami are manifestations of buddhas, honji-ground and suijaku-trace), and incoherent-bundle (shinbutsu-shugo is not coherent but an institutionally managed bundle of contradictions). Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different classifications. They are linked via network.affects_constraints because they offer competing interpretations of the same persistent institutional arrangement and compete for authority in Japanese religious discourse. The domain-partition reading claims natural complementarity; honji-suijaku claims underlying unity with phenomenal distinction; incoherent-bundle denies coherence itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
