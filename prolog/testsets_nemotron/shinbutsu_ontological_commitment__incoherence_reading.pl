% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo Institutionalized Incoherence (Incoherence Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   Shinbutsu-shugo — the historical amalgamation of kami worship and
 *   Buddhism in Japan — is read here as an institutionally tolerated
 *   incoherence: no stable ontological commitment ever unified kami and
 *   buddhas into a single system. Instead, the arrangement persisted through
 *   pragmatic coordination (shared ritual space, economic symbiosis,
 *   political utility) while ontological questions were deferred or answered
 *   differently in different contexts. The constraint is the standing
 *   arrangement of tolerated incoherence itself, which reduced coordination
 *   costs for local institutions but accumulated performative overhead over
 *   12 centuries. The Meiji separation (shinbutsu bunri) demonstrated the
 *   arrangement's low structural binding: when state-building required a
 *   unified Shinto, the incoherence dissolved rapidly because no deep
 *   ontological synthesis had ever been built. Beneficiaries include Meiji
 *   state-builders (who inherited a separable system), institutional Buddhism
 *   (which secured land, patronage, and parishioners through temple-shrine
 *   complexes), and local shrine networks (which accessed Buddhist
 *   organizational infrastructure). No victim class is declared — the
 *   arrangement extracted diffusely through performative maintenance rather
 *   than concentrated transfer.
 *
 * KEY AGENTS:
 *   - meiji_state_builders: Primary beneficiary (institutional/arbitrage) — inherited a separable system for nation-building
 *   - institutional_buddhism: Beneficiary (institutional/biographical) — secured resources through temple-shrine complexes
 *   - local_shrine_networks: Beneficiary (organized/biographical) — accessed Buddhist infrastructure
 *   - practitioners: Payer (moderate/identity_locked) — bore performative costs of maintaining dual affiliation
 *   - syncretic_theologians: Agenda setter (organized/biographical) — produced honji-suijaku frameworks that papered over incoherence
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure of tolerated incoherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.38).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.34).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, piton).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo Institutionalized Incoherence (Incoherence Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '6ec4ee6c-26a8-4f4e-999d-b565b15cb242').
narrative_ontology:cs_kernel_codification('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', distributed).
narrative_ontology:cs_authority_grounding('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', practice).
narrative_ontology:cs_interpretation_layer_present('6ec4ee6c-26a8-4f4e-999d-b565b15cb242').
narrative_ontology:cs_reading_relation('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_axiom('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', foundational, no_stable_ontological_commitment).
narrative_ontology:cs_axiom_status(no_stable_ontological_commitment, holdable).
narrative_ontology:cs_axiom_grounding('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', no_stable_ontological_commitment, empirically_contingent).
narrative_ontology:cs_axiom('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', secondary, incoherence_as_institutional_strategy).
narrative_ontology:cs_axiom_status(incoherence_as_institutional_strategy, holdable).
narrative_ontology:cs_axiom_grounding('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', incoherence_as_institutional_strategy, conventional).
narrative_ontology:cs_reference_frame('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', pragmatic_coordination_without_synthesis).
narrative_ontology:cs_drift_state('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', tokugawa_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ec4ee6c-26a8-4f4e-999d-b565b15cb242', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, institutional_buddhism).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, local_shrine_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, ontological_pragmatism_primary).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, institutional_coordination_without_integration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherited a religious landscape where kami and buddhas were institutionally entangled but ontologically uncommitted. Used the arrangement's latent separability to construct State Shinto as a unifying national ideology — the separation (shinbutsu bunri) was administratively easy because no deep synthesis had to be undone. Gained a ready-made symbolic system for nation-building.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, beneficiary,
    institutional, generational, arbitrage, national).

% Secured landholdings, parishioner bases (danka system), and political protection through temple-shrine complexes (jingū-ji). The incoherence allowed Buddhism to absorb kami worship without doctrinal concession — honji-suijaku placed kami as traces of buddhas, but operationally Buddhism gained infrastructure and patronage. Bore maintenance costs of dual ritual performance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, institutional_buddhism, beneficiary,
    institutional, generational, constrained, national).

% Accessed Buddhist organizational infrastructure (clergy, liturgy, parish management) through shrine-temple complexes. The arrangement let shrines maintain local autonomy while borrowing Buddhist institutional capacity. When separation came, many shrines lost Buddhist clergy and had to reconstitute independent ritual staff.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, local_shrine_networks, beneficiary,
    organized, biographical, constrained, regional).

% Performed dual observance — Buddhist funerals, Shinto festivals, ancestral rites at both temple and shrine — as the socially expected norm. Exit was identity-locked: abandoning either tradition risked social ostracism, ancestral disconnection, and cosmological anxiety. Bore time, cognitive, and material costs of maintaining two ritual grammars without concentrated benefit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, practitioners, payer,
    moderate, biographical, identity_locked, local).

% Produced and maintained honji-suijaku frameworks (original enlightenment / manifested trace) that papered over the ontological gap. Their interpretive labor sustained the arrangement's legitimacy. When Meiji separation demanded doctrinal purity, many theologians were marginalized or forced to choose sides — their exit was constrained by professional identity fused to the syncretic system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, syncretic_theologians, agenda_setter,
    organized, biographical, constrained, national).

% Sees the full 1200-year trajectory: pragmatic coordination calcifying into performative maintenance, ontological questions deferred until state-building forced resolution. The analytical seat reads the arrangement as a piton — a once-functional coordination mechanism (shared protection, economic symbiosis) that atrophied into institutional theater, persisting by inertia until a new agenda (Meiji nation-building) harvested its latent separability.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduced transaction costs of religious practice in fragmented political conditions: shared ritual space, mutual protection, economic symbiosis between temples and shrines, common parishioner management. Solved the problem of 'how do local communities maintain ritual life without a unified religious authority?'
% TRANSFER_FUNCTION: Moves performative compliance (dual ritual observance, maintenance of two clergy systems, duplicated infrastructure) from practitioners and local institutions to the arrangement's own perpetuation. No concentrated rent flow — extraction is diffuse, accruing to the arrangement's self-maintenance as theater.
% ABSENT_VOICES: Pure-practice Buddhist monastics who rejected kami worship as heterodox; nativist (kokugaku) scholars who saw kami worship as corrupted by Buddhism; peripheral communities where temple-shrine complexes never formed. These voices were excluded by the arrangement's convenience — the coordination function worked well enough that dissent remained marginal until Meiji.
% DISAPPEARANCE_RATIONALE: If the tolerated incoherence vanished overnight (as it effectively did in 1868), the religious landscape reorganizes: Buddhism loses shrine infrastructure and parish ties; shrines lose Buddhist clergy and liturgy; practitioners must choose or recombine traditions; the state gains a separable symbolic field for nation-building. The Meiji separation demonstrates this rearrangement empirically.
% FOUNDING_PROBLEM: Coordinating local religious practice and resource flows under fragmented political authority (pre-Tokugawa) where no single tradition could provide complete ritual coverage or institutional stability.
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa bakufu records (terauke system, shūmon aratame) attest that by ~1650 the state had assumed the coordination function (population registration, religious certification) that shinbutsu-shugo once provided. Meiji reformers (Kido Takayoshi, Ōkubo Toshimichi) explicitly cited the arrangement's separability as an asset for State Shinto construction — corroboration from outside the benefiting parties (state-builders, not Buddhist or Shinto institutions).
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint extracted performative compliance — maintaining two ritual grammars, dual clergy, duplicated infrastructure — without a concentrated rent flow to a single captor. Suppression is low (0.22) because alternatives (pure Buddhist practice, pure kami practice) remained available at the margins; the constraint operated through convenience and institutional momentum, not coercion. Theater ratio rises from 0.08 to 0.31 over the interval: early shinbutsu-shugo was functionally coordinative (shared protection, economic symbiosis), but honji-suijaku metaphysics and combinatory rituals became increasingly performative as the coordination function attenuated. Accessibility collapse is low (0.34) — practitioners could and did maintain single-tradition practice throughout. Resistance is low (0.28) — the arrangement was broadly convenient until state-building required its dissolution.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the constraint feels like a rope — genuine coordination of ritual life, shared festivals, mutual aid. From the Meiji state-builder seat, it reads as a piton — a degraded coordination mechanism that persisted by inertia and dissolved when a new agenda required its removal. The syncretic theologian seat experiences it as scaffold — a transitional metaphysics (honji-suijaku) meant to bridge traditions until deeper integration (which never came). The engine computes these per-seat divergences from the structural data; the authored claim (piton) reflects the analytical observer's assessment of the arrangement's terminal trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Meiji state-builders sit at the beneficiary extreme (d ≈ 0.1): they gained a pre-separated religious landscape they could reorganize for nation-building. Institutional Buddhism and local shrine networks are moderate beneficiaries (d ≈ 0.3): they gained resources and infrastructure but also bore maintenance costs. Practitioners are near-symmetric payers (d ≈ 0.55): they performed dual observance for social convenience, bearing time and cognitive costs without concentrated extraction. Syncretic theologians are agenda_setters (d ≈ 0.4): they produced the interpretive frameworks that sustained the arrangement's legitimacy. The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating local religious practice under fragmented political authority — was live from ~645 to ~1600 (Tokugawa stabilization). After Tokugawa unification, the coordination problem was substantially solved by state administration, but the shinbutsu-shugo arrangement persisted through institutional inertia and performative theology. The Meiji state-builders captured the arrangement's latent separability for nation-building (shinbutsu bunri, 1868), confirming mandatrophy: the arrangement outlived its coordination function by ~250 years. The incoherence reading captures this by showing low suppression throughout — the constraint never needed to suppress alternatives because it was never essential — and rising theater ratio as performative maintenance replaced functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_syncretism_ontology,
    'Was honji-suijaku a genuine metaphysical synthesis or a post-hoc rationalization of pragmatic coordination?',
    'Comparative analysis of honji-suijaku texts against ritual practice records: if texts describe practices that never occurred or contradict attested rituals, rationalization is favored. If texts predict novel ritual forms that were subsequently adopted, synthesis is favored.',
    'If honji-suijaku is genuine synthesis, the syncretic_reading''s claimed_type (rope or mountain) gains support and this reading''s piton classification weakens — the arrangement had deeper integration than incoherence allows. If rationalization, this reading''s low suppression and low accessibility_collapse are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_syncretism_ontology, conceptual, 'Whether the central theological framework of shinbutsu-shugo was ontologically constitutive or institutionally instrumental.').

omega_variable(
    meiji_separation_ease_cause,
    'Did shinbutsu bunri succeed easily because the prior arrangement was incoherent, or because Meiji state coercion was overwhelming?',
    'Counterfactual comparison: examine domains where Meiji state coercion was weak but separation still occurred (e.g., remote mountain shrines, peripheral regions). If separation proceeded without state enforcement there, incoherence is the primary cause.',
    'If state coercion was primary, the arrangement had more structural binding than this reading claims — suppression and accessibility_collapse would be higher. If incoherence was primary, the piton classification and low suppression are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_ease_cause, empirical, 'Whether the rapidity of Meiji separation reflects the arrangement''s internal weakness or external force.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural delta (kernel instability, separation ease, Meiji benefit) relate to the sibling readings'' deltas?',
    'Map each reading''s declared structural delta onto the same historical evidence base (honji-suijaku texts, temple-shrine records, Meiji separation edicts). The reading whose delta requires fewest auxiliary hypotheses to explain the evidence is structurally favored.',
    'If this reading''s delta (instability, ease, Meiji benefit) explains the evidence more parsimoniously than syncretic_reading''s delta (unified cosmology, hard separation) or partition_reading''s delta (clean domains, no integration needed), the incoherence reading is the preferred structural description of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta for this kernel reading vs. siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 645, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t645, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 645, 0.08).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1400, 0.26).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1600, 0.29).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 1868, 0.31).

% Extraction over time
narrative_ontology:measurement(shin_be_t645, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 645, 0.12).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 800, 0.18).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1000, 0.22).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1400, 0.31).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1868, 0.38).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 1200, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__incoherence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__incoherence_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment__partition_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'shinbutsu_ontological_commitment' into three readings with divergent ε: syncretic_reading claims low ε (unified cosmology as genuine coordination), partition_reading claims very low ε (separate domains as natural partition), incoherence_reading claims moderate ε (tolerated incoherence as degraded coordination). The Meiji separation event is the empirical discriminator: easy separation favors incoherence_reading; difficult separation favors syncretic_reading; pre-existing separation favors partition_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, institutional, 0.12).
constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, organized, 0.32).
constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
