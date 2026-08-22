% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Kami-Buddha Ontological Partition: Domain Division (Shinto Life/Buddhism Death)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   In medieval and early modern Japan, the relationship between indigenous
 *   Shinto and imported Buddhism crystallized into a functional partition:
 *   kami govern living affairs (birth, purification, prosperity, seasonal
 *   cycles), while Buddha and bodhisattva figures govern death, impurity,
 *   suffering, and liberation. This is the 'domain partition' reading of the
 *   shinbutsu-shugo (kami-buddha amalgamation) kernel. The reading claims
 *   that two distinct ontologies can coexist without requiring reduction to
 *   one or the other, and without requiring a unified theoretical framework.
 *   Kami and buddhas are genuinely different kinds of being, governing
 *   genuinely different domains, with practitioners moving between them
 *   according to life-cycle and seasonal context. This reading opposes the
 *   'honji-suijaku' reading, which claims kami are phenomenal manifestations
 *   of Buddhist truth, and opposes the 'incoherent bundle' reading, which
 *   claims no coherent framework binds them at all. The partition reading is
 *   the working consensus in household practice and the dominant frame in
 *   contemporary Shinto intellectualism; it is challenged by scholarly voices
 *   committed to Buddhist theological primacy and by those who see the
 *   partition as an unstable compromise masking unresolved contradiction.
 *
 * KEY AGENTS:
 *   - Shinto practitioners (organized, beneficiary) — maintain kami domain without requiring explanation of ultimate questions
 *   - Buddhist practitioners (organized, beneficiary) — maintain Buddhist domain without requiring integration with kami cosmology
 *   - Ritual specialists (moderate power, agenda-setter) — execute the partition in lived practice, administering domain-appropriate rites
 *   - Household families (moderate power, beneficiary) — organize life-cycle and seasonal ritual according to the partition
 *   - Honji-suijaku theologians (excluded, powerful-when-speaking) — argue for Buddhist ontological primacy; their voices are suppressed in this reading's framing
 *   - Philosophical interpretive tradition (observer, non-agent) — the learned consensus that produces theological justifications for the partition's coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.38).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.22).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.38).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Ontological Partition: Domain Division (Shinto Life/Buddhism Death)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, 'e783b742-ef85-4df4-9ee7-e58f5dc5de79').
narrative_ontology:cs_kernel_codification('e783b742-ef85-4df4-9ee7-e58f5dc5de79', distributed).
narrative_ontology:cs_authority_grounding('e783b742-ef85-4df4-9ee7-e58f5dc5de79', practice).
narrative_ontology:cs_interpretation_layer_present('e783b742-ef85-4df4-9ee7-e58f5dc5de79').
narrative_ontology:cs_reading_relation('e783b742-ef85-4df4-9ee7-e58f5dc5de79', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('e783b742-ef85-4df4-9ee7-e58f5dc5de79', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('e783b742-ef85-4df4-9ee7-e58f5dc5de79', foundational, kami_buddha_ontological_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('e783b742-ef85-4df4-9ee7-e58f5dc5de79', kami_buddha_ontological_distinctness, conventional).
narrative_ontology:cs_axiom('e783b742-ef85-4df4-9ee7-e58f5dc5de79', foundational, functional_complementarity_without_hierarchy).
narrative_ontology:cs_axiom_status(functional_complementarity_without_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('e783b742-ef85-4df4-9ee7-e58f5dc5de79', functional_complementarity_without_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('e783b742-ef85-4df4-9ee7-e58f5dc5de79', coequal_functional_domains).
narrative_ontology:cs_drift_state('e783b742-ef85-4df4-9ee7-e58f5dc5de79', contemporary_postwar_japan, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e783b742-ef85-4df4-9ee7-e58f5dc5de79', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_practitioners).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, ritual_specialists).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, household_families).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_complementarity_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, ontological_distinction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain a coherent domain of kami-based practice for life, purity, prosperity, and vital force. The domain partition allows Shinto to operate as a complete cosmology for living affairs without requiring explanation of death or the afterlife. They articulate and defend the distinction from Buddhism.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_practitioners, beneficiary,
    organized, generational, mobile, national).

% Maintain a coherent domain of Buddhist practice for death, impurity management, and soteriological concerns. The partition allows Buddhism to operate as a complete answer to the ultimate question (suffering, liberation) without requirement to explain life-force or kami. They articulate and defend the distinction from Shinto.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_practitioners, beneficiary,
    organized, generational, mobile, national).

% Priests, priestesses, and monks who implement and maintain the domain partition in lived ritual practice. They perform Shinto rites for living affairs (birth, marriage, purification, harvest), Buddhist rites for death and memorialization. The partition organizes their professional identity and ritual repertoire.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, ritual_specialists, agenda_setter,
    moderate, biographical, constrained, regional).

% Organize their annual and life-cycle ritual practice around the partition: Shinto for birth, coming-of-age, marriage, house protection, seasonal festivals; Buddhism for death, funerals, ancestor memorialization. The partition provides clear cognitive maps for when which tradition applies.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, household_families, beneficiary,
    moderate, biographical, constrained, local).

% The learned Buddhist-Shinto scholastic tradition, developing theological justifications for the partition's coherence. They produce arguments that two ontologies can coexist functionally without fusion or hierarchy—this is not an actor but the body of thought that interprets and sustains the framework.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, philosophical_interpretive_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kami_buddha_ontology__domain_partition, philosophical_interpretive_tradition).

% Scholarly and institutional voices arguing that kami are phenomenal manifestations (suijaku) of Buddhist buddhas/bodhisattvas as their true ground (honji)—i.e., that ontological unity underlies apparent distinction. Their reading would dissolve the partition into a framework of hierarchical reduction. They are structurally excluded from this reading's consensus but remain a live alternative within the kernel contest.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, honji_suijaku_theologians, excluded,
    organized, generational, mobile, national).

% The state's regulatory and legitimation framework. Historically, various regimes have enforced, permitted, or constrained the partition: the Meiji establishment promoted Shinto over Buddhism; post-WWII separationist policies mandated stronger partition; contemporary Japanese governance permits both. The state's official stance shapes the constraint's operation but is not itself the constraint.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, institutional_state_apparatus, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, diffuse).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions two coherent cosmologies into functionally non-overlapping domains: Shinto handles questions of life, purity, prosperity, and vital force; Buddhism handles questions of death, impurity, suffering, and liberation. This allows practitioners to maintain both traditions without requiring a unified theoretical answer to all religious questions, and allows ritual specialists to serve both functions without contradiction.
% TRANSFER_FUNCTION: Transfers religious authority and legitimacy bidirectionally: practitioners give credibility and patronage to both Shinto and Buddhist institutions by accepting that each has valid jurisdiction over distinct domains; Shinto and Buddhist establishments transfer authority to each other by recognizing domain boundaries rather than competing for universality.
% ABSENT_VOICES: Theologians and practitioners committed to honji-suijaku monism (Buddhist-supremacist readings where kami are manifestations of Buddha-nature) are excluded from the consensus framing; so are Christian and secular rationalist critics who argue the partition is incoherent or that neither system should govern any domain. Their arguments would dissolve or replace the partition if admitted.
% DISAPPEARANCE_RATIONALE: If the partition disappeared, Japanese religious life would rearrange significantly but the outcome is contested: some argue practitioners would consolidate into pure Buddhism (pre-Meiji pattern) or pure Shinto (Meiji assimilationist pattern); others argue the functional complementarity is so embedded in household practice that alternative partitions would re-emerge; a third position holds that practitioners would adopt a fully syncretic, non-partitioned cosmology. The disagreement is not incidental—it lies at the heart of what the partition IS.
% FOUNDING_PROBLEM: Early medieval Japanese synthesis of indigenous kami-worship and imported Buddhism faced the theological problem: are kami and buddhas the same ontological kind or radically different kinds? The partition solved this by declaring them functionally specialized—kami govern living affairs, buddhism governs death and ultimate liberation—without requiring that they reduce to one or the other, or that they coexist in a unified framework.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Buddhist scholars (Sueki, Itoh) and Shinto intellectuals attest that the partition was built precisely to resolve the contradiction between imported Buddhism and indigenous practice. Historians of medieval syncretism document the development of the distinction in theological texts from 8th–12th century. However, contemporary scholars of honji-suijaku monism (Teeuwe, Fabio Rambelli) argue that the founding problem was never truly 'solved'—the monist reading remained dominant in intellectual circles even as the partition functioned in popular practice, leaving an unresolved tension rather than a founding resolution. This disagreement is within the scholarly community itself; no voices outside the tradition provide corroboration (by design—the constraint belongs entirely to the Japanese religio-philosophical world).
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The partition reading claims moderate extractiveness (0.38) because it does not concentrate material benefit or authority in a single seat; both Shinto and Buddhist institutions benefit from their recognized domains, and households benefit from clarity about which tradition applies when. It is not extraction in the sense that one party coerces another into bearing costs—instead, the constraint coordinates two parallel systems by establishing clear boundaries. Suppression is low (0.22) because the partition is broadly accepted as functional in lived practice; enforcement is minimal once the boundary conventions are established. Theater is modest (0.18) because the partition's primary function is genuine coordination, not performance, though some performative affirmation of the boundary occurs in theological discourse. Accessibility collapse is moderate (0.45) because alternatives do exist and are articulated—the honji-suijaku reading, pure Buddhism, pure Shinto, secular rejection—but the partition has become institutionalized and psychologically embedded in household practice. Resistance is substantial (0.71) from excluded voices (Buddhist supremacists, syncretic critics, rationalist skeptics) who contest the partition's legitimacy. The measurement trajectory shows modest rise in extractiveness during the period 0–30 (increased institutional gatekeeping and differentiation during modernization and postwar consolidation), plateauing after 30 as the partition stabilizes as a normalized convention rather than an actively defended position. Theater ratio rises slowly as the partition becomes increasingly justified through academic and ceremonial language rather than lived necessity.
 *
 * PERSPECTIVAL GAP:
 *   Shinto practitioners experience the partition as liberation: it allows Shinto to be complete and coherent without having to answer Buddhist questions about death and liberation. Buddhist practitioners experience the same partition as liberation: it allows Buddhism to address ultimate questions without having to explain or compete with kami-based life-force and prosperity magic. The agenda-setter (ritual specialists) experiences the partition as clarifying their professional identity and repertoire—they know which toolkit applies when. Excluded voices (honji-suijaku theologians) experience the partition as a theoretical incoherence masking Buddhist truth. The engine computes these seats differently from their own declared positions: the partition's actual function for Shinto is NOT purely coordination but also protective—it shields Shinto from Buddhist philosophical pressure; for Buddhism, it is also protective—it avoids the charge of irrelevance to life-concerns. This protective function is an asymmetry the partition's own beneficiaries do not fully articulate but that the structural analysis reveals.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto practitioners: beneficiary, organized power, mobile exit—the partition enables their tradition to flourish independently; d near 0.2 (beneficiary side). Buddhist practitioners: beneficiary, organized power, mobile exit—the partition similarly enables their tradition; d near 0.2. Ritual specialists: agenda-setter, moderate power, constrained exit (professional identity bound to the framework)—they administer the partition and benefit from its clarity, but they are also somewhat bound by it; d near 0.35 (slight target side due to constrained exit). Households: beneficiary, moderate power, constrained exit (culturally and socially embedded)—they benefit from the clarity but cannot easily exit; d near 0.4. Honji-suijaku theologians: excluded, organized power, mobile exit—they are excluded from the consensus but not coerced; they can speak and publish their views; d near 0.5 (symmetric—they are neither benefiting nor bearing structural cost, but they are excluded from the discourse). The constraint does not extract in the coercive sense, but it does create asymmetries in who gets to articulate legitimacy and who is pushed to the margins.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition's founding problem was genuine: medieval Japan needed to integrate two religious traditions without forcing reduction or syncretism. The partition solved it by establishing a functional division. However, the founding problem is now contested: some argue death/impurity/liberation are genuinely separate from life/purity/prosperity and the partition correctly names this distinction; others argue the distinction is artificial and that a unified Buddhist or Shinto cosmology would be more coherent. The constraint shows no signs of mandate obsolescence—it continues to organize household practice effectively and to give institutional coherence to both traditions. The rising theater ratio suggests that the partition is increasingly maintained through explicit theological and ceremonial assertion rather than through automatic practice, but this is not mandatrophy proper; it is normalization. The constraint would be classified as a ROPE (genuine coordination of two functional domains) if measured from the seat of practitioners and households; it would be classified as a TANGLED ROPE or SNARE if measured from the seat of excluded theological voices, because it coordinates the dominant consensus while suppressing alternative readings. The per-seat divergence is the point: the constraint is stable precisely because both Shinto and Buddhist practitioners experience it as beneficial, while dissenting theologians cannot dislodge it because they lack institutional power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_monism_boundary,
    'Are the kami and buddhist ontologies genuinely distinct in principle, or is the perceived distinction an artifact of institutional separation that masks an underlying Buddhist philosophical primacy?',
    'Philological and historical analysis of medieval theological texts and their development trajectories. If honji-suijaku monism is documented as the dominant reading among learned Buddhist scholars even during periods when the partition functioned in practice, the distinction is institutional-pragmatic rather than principled-ontological.',
    'If the partition is genuine, it is a roof-level coordination (two coequal ontologies). If monism is the true learned position even during the partition''s historical operation, the partition is a false-level coordination—practical acceptance of a distinction while theoretical primacy remains unresolved. This would reclassify the constraint from rope toward tangled-rope or piton (institutional theater maintaining a false coherence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_vs_monism_boundary, empirical, 'Whether the domain partition is a genuine ontological framework or an institutional compromise masking theoretical subordination.').

omega_variable(
    functional_complementarity_stability,
    'Is the functional complementarity (Shinto for life, Buddhism for death) structurally stable, or does it depend on continuous suppression of voices that argue for unified cosmology?',
    'Longitudinal analysis of debate and dissent within both Shinto and Buddhist communities. If dissent is consistently marginalized or requires institutional suppression, the partition is maintained by enforcement rather than by genuine functional stability. If dissent is permitted and articulated but practitioners remain committed to the partition anyway, the partition is genuinely stable.',
    'High suppression with weak enforcement = piton (theater maintaining a dead mandate). High suppression with strong enforcement = snare (cosmology maintained by exclusion). Low suppression = rope (genuine coordination). Current measurement estimates low-moderate suppression; resolution would determine whether it rises or remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_complementarity_stability, empirical, 'Whether functional complementarity is intrinsically stable or maintained by institutional suppression of alternatives.').

omega_variable(
    axiom_ontological_distinctness,
    'What grounds the claim that kami and buddhas are ontologically distinct rather than reducible to one another? Is this grounding deontological (an irreducible moral/spiritual difference), empirical (kami operate on different causal principles), or conventional (institutional decision to treat them as distinct)?',
    'Analysis of the theological justifications offered within the tradition for the partition''s legitimacy. Deontological grounding (kami have intrinsic sacred status not reducible to Buddhist categories) would make the partition resistant to theoretical challenge. Empirical grounding (kami have different properties/effects) would make it vulnerable to evidence. Conventional grounding (we treat them as distinct by institutional agreement) would make it vulnerable to renegotiation.',
    'Deontological grounding = robust to theoretical critique; empirical grounding = vulnerable to new cosmological frameworks; conventional grounding = renegotiable if institutions change. This affects the constraint''s long-term stability classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_ontological_distinctness, conceptual, 'What kind of claim grounds the ontological distinctness that the partition asserts.').

omega_variable(
    excluded_voices_suppression_mechanism,
    'Are honji-suijaku theologians and syncretic critics excluded from the domain-partition consensus through structural barriers (institutional gatekeeping, publication control) or through discursive defeat (their arguments are engaged and found wanting)?',
    'Historiography of how Buddhist supremacist and syncretic arguments have been treated in scholarly discourse. If excluded through institutional power (journal gatekeeping, seminary curriculum control), the constraint involves active suppression. If excluded through argumentation (their positions are engaged and critiqued), the constraint involves discursive competition without structural suppression.',
    'Structural exclusion = higher effective suppression, moves the constraint toward snare or tangled-rope. Discursive exclusion = lower effective suppression, supports the rope classification. Current suppression measurement (0.22) suggests primarily discursive competition; verification would confirm or revise this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_voices_suppression_mechanism, empirical, 'Whether exclusion of alternative readings is maintained through institutional power or through discursive persuasion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(kami_tr_t0, observed).
narrative_ontology:measurement(kami_tr_t10, kami_buddha_ontology__domain_partition, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(kami_tr_t10, observed).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__domain_partition, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(kami_tr_t20, observed).
narrative_ontology:measurement(kami_tr_t30, kami_buddha_ontology__domain_partition, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(kami_tr_t30, observed).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__domain_partition, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(kami_tr_t40, observed).
narrative_ontology:measurement(kami_tr_t50, kami_buddha_ontology__domain_partition, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(kami_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(kami_be_t0, observed).
narrative_ontology:measurement(kami_be_t10, kami_buddha_ontology__domain_partition, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(kami_be_t10, observed).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__domain_partition, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(kami_be_t20, observed).
narrative_ontology:measurement(kami_be_t30, kami_buddha_ontology__domain_partition, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(kami_be_t30, observed).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__domain_partition, base_extractiveness, 40, 0.37).
narrative_ontology:measurement_basis(kami_be_t40, observed).
narrative_ontology:measurement(kami_be_t50, kami_buddha_ontology__domain_partition, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(kami_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(kami_su_t0, observed).
narrative_ontology:measurement(kami_su_t10, kami_buddha_ontology__domain_partition, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(kami_su_t10, observed).
narrative_ontology:measurement(kami_su_t20, kami_buddha_ontology__domain_partition, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(kami_su_t20, observed).
narrative_ontology:measurement(kami_su_t30, kami_buddha_ontology__domain_partition, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(kami_su_t30, observed).
narrative_ontology:measurement(kami_su_t40, kami_buddha_ontology__domain_partition, suppression_requirement, 40, 0.21).
narrative_ontology:measurement_basis(kami_su_t40, observed).
narrative_ontology:measurement(kami_su_t50, kami_buddha_ontology__domain_partition, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(kami_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__domain_partition, 0.12).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, japanese_household_ritual_practice).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, meiji_state_shinto_promotion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu-shugo (kami-buddha amalgamation) kernel. The constraint family comprises three stories: 'domain_partition' (this file) — two coequal ontologies governing non-overlapping functional domains; 'honji_suijaku_monism' — kami are phenomenal manifestations of Buddhist truth (ontological reduction); 'incoherent_bundle' — no coherent kernel, only institutional contradiction. Each reading has a distinct epsilon: domain_partition measures low-to-moderate extraction (coordination with asymmetric gatekeeping = rope); honji_suijaku_monism measures higher extraction (Buddhist supremacy + asymmetric suppression = tangled-rope or snare from Shinto seats); incoherent_bundle measures the constraint as a performance theater = piton. The three stories are linked by their shared kernel identity and by influence relations: domain_partition provides the working consensus, honji_suijaku_monism challenges it from within scholarly Buddhism, incoherent_bundle challenges it from outside the tradition entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
