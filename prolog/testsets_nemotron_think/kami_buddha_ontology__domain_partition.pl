% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Domain Partition (Life/Death Ritual Allocation)
 *   domain: religious/cultural/historical
 *
 * SUMMARY:
 *   The domain_partition reading presents kami and buddhas as ontologically
 *   distinct entities governing separate functional domains: Shinto for
 *   life/purity/living, Buddhism for death/impurity/deceased. This reading
 *   claims practical coordination without theoretical unity — a clean
 *   functional division of ritual labor. Historically, this partition was
 *   violently imposed by the Meiji state (1868 shinbutsu bunri) after
 *   centuries of syncretic fusion (shinbutsu-shugo). The constraint today
 *   operates as a tangled rope: genuine coordination (people know which
 *   institution serves which life event) layered with asymmetric extraction
 *   (shrines and temples hold protected ritual markets). The claim/metric
 *   independence is deliberate: the reading claims rope-like coordination;
 *   the metrics reveal substantial extraction and historical enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.42).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.35).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Domain Partition (Life/Death Ritual Allocation)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious/cultural/historical").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '3a1817a2-4548-42ee-9e8e-90338c1b1e33').
narrative_ontology:cs_kernel_codification('3a1817a2-4548-42ee-9e8e-90338c1b1e33', distributed).
narrative_ontology:cs_authority_grounding('3a1817a2-4548-42ee-9e8e-90338c1b1e33', practice).
narrative_ontology:cs_interpretation_layer_present('3a1817a2-4548-42ee-9e8e-90338c1b1e33').
narrative_ontology:cs_reading_relation('3a1817a2-4548-42ee-9e8e-90338c1b1e33', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('3a1817a2-4548-42ee-9e8e-90338c1b1e33', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('3a1817a2-4548-42ee-9e8e-90338c1b1e33', foundational, ontological_distinction_kami_buddha).
narrative_ontology:cs_axiom_status(ontological_distinction_kami_buddha, holdable).
narrative_ontology:cs_axiom_grounding('3a1817a2-4548-42ee-9e8e-90338c1b1e33', ontological_distinction_kami_buddha, deontological).
narrative_ontology:cs_axiom('3a1817a2-4548-42ee-9e8e-90338c1b1e33', foundational, functional_domain_partition).
narrative_ontology:cs_axiom_status(functional_domain_partition, holdable).
narrative_ontology:cs_axiom_grounding('3a1817a2-4548-42ee-9e8e-90338c1b1e33', functional_domain_partition, conventional).
narrative_ontology:cs_reference_frame('3a1817a2-4548-42ee-9e8e-90338c1b1e33', ontological_distinction_kami_buddha).
narrative_ontology:cs_drift_state('3a1817a2-4548-42ee-9e8e-90338c1b1e33', meiji_shinbutsu_bunri, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('3a1817a2-4548-42ee-9e8e-90338c1b1e33', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_shrines).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_temples).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, funeral_industry).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, lay_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, lay_practitioners).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, ontological_distinction_kami_buddha).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_domain_partition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer life-cycle rituals (birth, coming-of-age, marriage, groundbreaking) at shrines; collect offering fees and patronage. The domain partition assigns them the 'pure/living' ritual market. Their institutional continuity depends on maintaining the boundary against Buddhist encroachment on life rituals.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, shinto_priesthood, beneficiary).

% Administer death rituals (funerals, memorial services, ancestral rites) at temples; collect funeral fees, posthumous name fees, and ongoing memorial patronage. The domain partition assigns them the 'impure/death' ritual market. Their economic base is the funeral industry, which the partition secures.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_clergy, beneficiary).

% Pay fees to both shrines (life events) and temples (death events); receive ritual services from both systems. The partition means they must engage two separate institutional economies for a complete ritual life. Exit from either system is socially constrained — skipping shrine visits or temple funerals invites stigma.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, lay_practitioners, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, lay_practitioners, beneficiary).

% The Meiji state (1868-1912) forcibly imposed shinbutsu bunri (separation of kami and buddhas), destroying syncretic institutions and legally mandating the domain partition. This historical actor created the enforcement infrastructure the constraint still rides on, though the state no longer directly administers religion.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, imperial_state_meiji, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_non_agent(kami_buddha_ontology__domain_partition, imperial_state_meiji).

% Modern corporate funeral service providers who operate within the Buddhist temple network. They capture a growing share of death-ritual revenue while relying on the partition's assignment of death to Buddhism. Their business model depends on the partition persisting.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, funeral_industry, beneficiary,
    organized, biographical, mobile, national).

% Analyze the partition as a historical construct, a functional coordination mechanism, or a theological claim. They do not pay ritual fees nor collect them; their stake is interpretive. Their work shapes public understanding but does not directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns each major life transition to a single authoritative ritual system — shrines for purity/life events, temples for impurity/death events — so that every Japanese person knows which institution to approach for each passage without doctrinal negotiation.
% TRANSFER_FUNCTION: Moves ritual fees, offerings, and ongoing patronage from lay practitioners to shrine and temple institutions, segmented by life-domain: shrines capture birth/marriage/groundbreaking revenue; temples capture funeral/memorial/ancestral revenue. The partition prevents either system from poaching the other's ritual market.
% ABSENT_VOICES: Practitioners who maintain syncretic home altars (kamidana and butsudan together) and resist the institutional boundary; modern secular Japanese who find the life/death partition arbitrary and expensive; new religious movements that offer unified ritual alternatives. These voices are structurally excluded from the shrine/temple duopoly.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, the ritual economy would reorganize: shrines and temples would compete for all life-cycle rituals, new hybrid providers would emerge, funeral costs would face competitive pressure, and the institutional boundary that sustains the current fee structure would dissolve. The duopoly's revenue model depends on the partition.
% FOUNDING_PROBLEM: Pre-modern Japanese society needed authoritative ritual management for all life transitions without doctrinal conflict between kami cults (local, this-worldly, purity-focused) and buddha cults (universal, other-worldly, salvation-focused). The partition solved this by allocating non-overlapping domains.
% FOUNDING_PROBLEM_CORROBORATION: The original doctrinal conflict was resolved not by the partition but by centuries of shinbutsu-shugo (syncretic fusion) where kami and buddhas were worshipped together. The domain partition was state-manufactured in 1868 (shinbutsu bunri orders) — documented in Meiji government records and analyzed by scholars including Jason Josephson-Storm (The Invention of Religion in Japan), Mark Teeuwen, and Fabio Rambelli. No non-state corroboration supports the partition as a solution to a live doctrinal problem.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.42) reflects the duopoly's capture of life-cycle ritual fees — not monopoly pricing but protected market segmentation. Suppression (0.35) is moderate: no legal enforcement remains, but social stigma and institutional inertia maintain the boundary. Theater (0.28) is present: shrines/temples perform 'traditional' partition rhetoric while the original theological justification is largely abandoned. Accessibility collapse (0.32) is low — syncretic practice persists at household level. Resistance (0.41) is moderate: historical resistance to forced separation, contemporary indifference. The measurement series tracks: pre-Meiji syncretism (T=0), Meiji enforcement (T=30), wartime State Shinto (T=60), post-war religious freedom (T=90), secularization (T=120), present (T=150).
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood/clergies' seats, the partition is functional coordination they maintain. From lay practitioners' seat, it is a duopoly extracting fees for a complete ritual life. From the historical state's seat, it was a manufactured tool for national unification. The engine computes this divergence from the structural data — the reading's claim of 'clean coordination' does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priesthood and Buddhist clergy are dual agenda_setters — each administers and benefits from their assigned domain (d near beneficiary end). Lay practitioners are payers to both systems with constrained exit (d near target end). Meiji state was historical agenda_setter with arbitrage exit (created the constraint, then withdrew). Funeral industry is a mobile beneficiary capturing death-ritual revenue. Scholars are analytical observers. The domain partition creates symmetric extraction from lay practitioners to BOTH institutional seats — a rare dual-payer structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal conflict between kami/buddha cults) was dead by 1868 — centuries of syncretism had resolved it. The partition was imposed to solve a NEW problem: state need for a unified national religion. That mandate is dead (post-1945 religious freedom), but the partition persists as institutional inertia and revenue protection. This is classic mandatrophy: the constraint's current function (protecting ritual markets) differs from its founding justification (resolving doctrinal conflict). The mismatch (founding_problem_status=dead, disappearance_verdict=world_rearranges) flags this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_natural_vs_manufactured,
    'Does the life/death domain partition reflect a pre-existing functional logic in Japanese religious practice, or was it entirely manufactured by the Meiji state''s shinbutsu bunri?',
    'Comparative analysis of pre-Meiji ritual records: if life/death specialization existed de facto before 1868, the partition revealed a latent structure; if syncretic fusion was universal, the partition was manufactured.',
    'If manufactured, the constraint''s coordination function is a post-hoc rationalization for state-imposed extraction; if natural, the Meiji enforcement merely formalized an existing coordination equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_natural_vs_manufactured, empirical, 'Whether the domain partition is a discovered natural law or a constructed constraint.').

omega_variable(
    ontological_distinction_status,
    'Is the ontological distinction between kami and buddhas a genuine theological commitment of the domain_partition reading, or a pragmatic boundary-maintenance device?',
    'Examine whether shrine and temple institutions ever cooperate in ways that violate the ontological boundary (e.g., joint rituals, shared deities) while maintaining the functional partition.',
    'If pragmatic, the ''ontological distinction'' is a cover story for market segmentation — the constraint is a snare disguised as a mountain. If genuine, the coordination is theologically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_distinction_status, conceptual, 'Whether the claimed ontological distinction is load-bearing or decorative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the current suppression of syncretic alternatives structural (institutional duopoly, regulatory barriers) or internalized (practitioners believe the partition is natural/traditional)?',
    'Post-exit suppression trajectory: survey practitioners who engage syncretic or non-aligned rituals — do they face institutional penalties, or only social friction from internalized norms?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the boundary lives in practitioners'' cognition. If structural, removing institutional barriers would rapidly dissolve the partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the contemporary partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.12).
narrative_ontology:measurement(kami_tr_t30, kami_buddha_ontology__domain_partition, theater_ratio, 30, 0.25).
narrative_ontology:measurement(kami_tr_t60, kami_buddha_ontology__domain_partition, theater_ratio, 60, 0.38).
narrative_ontology:measurement(kami_tr_t90, kami_buddha_ontology__domain_partition, theater_ratio, 90, 0.32).
narrative_ontology:measurement(kami_tr_t120, kami_buddha_ontology__domain_partition, theater_ratio, 120, 0.29).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__domain_partition, theater_ratio, 150, 0.28).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(kami_be_t30, kami_buddha_ontology__domain_partition, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(kami_be_t60, kami_buddha_ontology__domain_partition, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(kami_be_t90, kami_buddha_ontology__domain_partition, base_extractiveness, 90, 0.45).
narrative_ontology:measurement(kami_be_t120, kami_buddha_ontology__domain_partition, base_extractiveness, 120, 0.41).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__domain_partition, base_extractiveness, 150, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(kami_su_t30, kami_buddha_ontology__domain_partition, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(kami_su_t60, kami_buddha_ontology__domain_partition, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(kami_su_t90, kami_buddha_ontology__domain_partition, suppression_requirement, 90, 0.38).
narrative_ontology:measurement(kami_su_t120, kami_buddha_ontology__domain_partition, suppression_requirement, 120, 0.31).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__domain_partition, suppression_requirement, 150, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__domain_partition, 0.08).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, meiji_shinto_state_formation).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, japanese_funeral_industry).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, shinto_shrine_system).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, buddhist_temple_economy).

% DUAL FORMULATION NOTE:
% This is the domain_partition reading of the kami_buddha_ontology kernel. It differs from honji_suijaku_monism (ontological identity, hierarchical) in ε (0.42 vs ~0.15) and from incoherent_bundle (no coherent kernel) in structural coherence. The three readings form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
