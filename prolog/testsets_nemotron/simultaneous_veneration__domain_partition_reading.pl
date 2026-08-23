% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Life/Death Specialization)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This reading holds that kami and buddhas govern separate but
 *   complementary domains — kami preside over this-worldly prosperity,
 *   agriculture, and communal life; buddhas preside over afterlife salvation,
 *   karma, and liberation. Simultaneous veneration is not syncretic confusion
 *   but domain-appropriate specialization: households petition kami for
 *   harvest and health, and buddhas for rebirth and ancestors. The constraint
 *   is the implicit coordination norm that assigns each domain to its proper
 *   specialist. No priesthood enforces exclusivity; the arrangement persists
 *   because it solves two distinct coordination problems (life-flourishing,
 *   death-assurance) with two specialist institutions. The coordination is
 *   voluntary, low-overhead, and participant-recognized.
 *
 * KEY AGENTS:
 *   - householders: Primary coordinators (powerless/mobile) — navigate both domains voluntarily, no extraction
 *   - local_shrine_priests: Life-domain specialists (moderate/constrained) — receive offerings for this-worldly petitions
 *   - funerary_temples: Death-domain specialists (moderate/constrained) — receive offerings for afterlife services
 *   - doctrinal_scholars: Observers (analytical/analytical) — articulate honji-suijaku or partition theories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.05).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.1).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Kami-Buddha Domain Partition (Life/Death Specialization)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'cee31854-6196-4efd-b1af-8d6d3faf308f').
narrative_ontology:cs_kernel_codification('cee31854-6196-4efd-b1af-8d6d3faf308f', distributed).
narrative_ontology:cs_authority_grounding('cee31854-6196-4efd-b1af-8d6d3faf308f', practice).
narrative_ontology:cs_reading_relation('cee31854-6196-4efd-b1af-8d6d3faf308f', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('cee31854-6196-4efd-b1af-8d6d3faf308f', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('cee31854-6196-4efd-b1af-8d6d3faf308f', foundational, kami_govern_this_worldly_prosperity).
narrative_ontology:cs_axiom_status(kami_govern_this_worldly_prosperity, holdable).
narrative_ontology:cs_axiom_grounding('cee31854-6196-4efd-b1af-8d6d3faf308f', kami_govern_this_worldly_prosperity, conventional).
narrative_ontology:cs_axiom('cee31854-6196-4efd-b1af-8d6d3faf308f', foundational, buddhas_govern_afterlife_salvation).
narrative_ontology:cs_axiom_status(buddhas_govern_afterlife_salvation, holdable).
narrative_ontology:cs_axiom_grounding('cee31854-6196-4efd-b1af-8d6d3faf308f', buddhas_govern_afterlife_salvation, conventional).
narrative_ontology:cs_reference_frame('cee31854-6196-4efd-b1af-8d6d3faf308f', practical_domain_partition).
narrative_ontology:cs_drift_state('cee31854-6196-4efd-b1af-8d6d3faf308f', pre_meiji_separation, gap(stable, minor, false)).
narrative_ontology:cs_created_at('cee31854-6196-4efd-b1af-8d6d3faf308f', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, householders).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, local_shrine_priests).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, funerary_temples).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, domain_appropriate_specialization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigate both domains voluntarily: visit shrines for harvest, health, and communal festivals; support temples for funerals, memorial rites, and ancestor care. No penalty for choosing one domain over the other; no enforcement of dual practice. Pay offerings proportionate to petition scope. Can exit either domain at any time without social or spiritual sanction.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, householders, beneficiary,
    powerless, biographical, mobile, local).

% Serve as life-domain specialists: conduct agricultural rites, lifecycle rituals (birth, coming-of-age, marriage), and community festivals. Receive offerings and land support from parishioners. Do not claim authority over death rites or afterlife matters. Their jurisdiction is recognized by custom, not enforced by doctrine. Exit would mean abandoning hereditary role — constrained but not trapped.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, local_shrine_priests, beneficiary,
    moderate, biographical, constrained, local).

% Serve as death-domain specialists: conduct funerals, memorial services, and ancestor rites. Receive offerings, land endowments, and parishioner support (danka system). Do not claim authority over this-worldly prosperity rites. Their jurisdiction is recognized by custom and institutional affiliation. Exit would mean leaving the temple lineage — constrained but not trapped.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, funerary_temples, beneficiary,
    moderate, biographical, constrained, local).

% Articulate theoretical frameworks: honji-suijaku (ontological fusion), domain partition (this reading), or pragmatic incoherence. Their discourse shapes institutional self-understanding but does not directly govern lay practice. No material stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, doctrinal_scholars, observer,
    analytical, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(simultaneous_veneration__domain_partition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates two distinct existential domains (this-worldly flourishing, afterlife assurance) through specialist institutions, allowing households to access appropriate expertise for each domain without requiring a single totalizing authority.
% TRANSFER_FUNCTION: Moves voluntary offerings from households to shrine priests (life domain) and temples (death domain) in proportion to petition scope. No cross-subsidy, no mandatory transfer, no extraction from non-participants.
% ABSENT_VOICES: Those who would reject both domains (pure Buddhists, pure Shintoists, non-religious) are not excluded — they simply do not participate. The constraint does not organize their lives. No voices are structurally silenced.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, households would lose the implicit coordination norm that assigns life petitions to shrines and death petitions to temples. Some would drift to single-domain practice; others would improvise ad hoc. The specialist institutions would lose their recognized jurisdictions and face jurisdictional competition. The arrangement's disappearance would rearrange the religious economy.
% FOUNDING_PROBLEM: Pre-partition Japanese religiosity faced a coordination problem: how to address both this-worldly collective prosperity (agriculture, health, communal harmony) and afterlife individual salvation (karma, rebirth, liberation) without a single doctrine that could competently cover both. The partition emerged as a practical division of labor between kami-cults (collective, this-worldly) and Buddhist institutions (individual, other-worldly).
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic records (Yanagita, Orikuchi), shrine-temple land surveys (Kenmu-Shomu period), and vernacular votive records (ema, kuyo-hi) corroborate the domain distinction from outside the benefiting priesthoods. The partition is attested by lay practice, not only by institutional self-description.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction near zero (0.05): offerings are voluntary, proportional to petition scope, and not compelled. Suppression negligible (0.1): no enforcement against dual practice; exit to single-domain practice is always available. Theater low (0.1): ritual performance matches functional intent. Accessibility collapse low (0.3): alternatives (single-domain devotion, pure Buddhism, pure Shinto) remain conceptually and practically available. Resistance low (0.2): the arrangement meets little active opposition because it imposes no cost on non-participants. Claimed type: rope — pure coordination with minimal coercion, participants are net beneficiaries, alternatives not suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the householder seat: the constraint is invisible coordination — they simply do what works for each domain. From the shrine priest seat: the constraint is a stable institutional niche with recognized jurisdiction. From the temple seat: same. From the doctrinal scholar seat: the constraint is a theoretical puzzle (are these one or two?). The engine computes per-seat types from the structural data; all seats should compute rope or mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Householders are symmetric beneficiaries (d ≈ 0.5): they receive specialized services in both domains and pay voluntarily. Shrine priests and temples are specialized beneficiaries (d ≈ 0.3): they collect offerings in their domain but do not extract from the other domain. No agent is a net payer — the arrangement is not extractive. The domain partition itself is the coordination mechanism; it requires no enforcement because each specialist's legitimacy derives from demonstrated efficacy in its domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating life and death concerns through specialist institutions) remains live — the domains have not disappeared. The arrangement has not atrophied into piton because the coordination function is actively used. No mandatrophy to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the domain_partition_reading a coherent single constraint, or does it decompose into two parallel constraints (kami-life, buddha-death) with independent ε values?',
    'Measure whether the life-domain coordination and death-domain coordination have distinct beneficiary sets, enforcement mechanisms, and drift trajectories. If they diverge structurally, the reading is a constraint family, not a single constraint.',
    'If decomposable, each sub-constraint gets its own classification and ε; the domain_partition_reading as authored would be a natural-language label covering two ropes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the reading instantiates one constraint or two parallel ones').

omega_variable(
    pragmatic_incoherence_challenge,
    'Do practitioners actually hold the domain-partition view as a structured belief, or is the partition a scholarly reconstruction imposed on undifferentiated practice?',
    'Examine vernacular texts, votive records, and shrine-temple inventories for explicit domain demarcation by lay actors. If absent, the rope coordination is analyst-attributed, not participant-endorsed.',
    'If the partition is analyst-only, the coordination function is not participant-recognized — the constraint may be a projected rope rather than a lived one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pragmatic_incoherence_challenge, empirical, 'Whether the domain distinction is participant-structured or analyst-imposed').

omega_variable(
    ontological_fusion_pressure,
    'Does the ontological_fusion_reading (honji-suijaku) create structural pressure that erodes the domain partition over time?',
    'Track institutional discourse: when honji-suijaku doctrine becomes the official temple-shrine framing, does the practical domain demarcation persist, shrink, or dissolve? Measure via ritual manuals and land records.',
    'If fusion doctrine structurally displaces partition practice, the rope constraint has a predictable drift toward tangled_rope (coordination + doctrinal extraction). If partition persists despite fusion doctrine, the rope is empirically robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_fusion_pressure, empirical, 'Whether doctrinal fusion undermines the functional partition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__domain_partition_reading, theater_ratio, 200, 0.07).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__domain_partition_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__domain_partition_reading, theater_ratio, 600, 0.09).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.1).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 200, 0.04).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 400, 0.05).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 600, 0.05).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.05).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__domain_partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(simu_su_t200, simultaneous_veneration__domain_partition_reading, suppression_requirement, 200, 0.07).
narrative_ontology:measurement(simu_su_t400, simultaneous_veneration__domain_partition_reading, suppression_requirement, 400, 0.08).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__domain_partition_reading, suppression_requirement, 600, 0.09).
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__domain_partition_reading, suppression_requirement, 800, 0.1).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel decomposes into three constraint stories with distinct structural profiles. domain_partition_reading = rope (coordination, ε≈0.05). ontological_fusion_reading = tangled_rope (coordination + doctrinal extraction, ε>0.2). pragmatic_incoherence_reading = snare (extraction via enforced incoherence, ε>0.4). All three share the same historical referent (Heian-Edo Japan) but differ in ε, beneficiary/victim structure, and classification. This decomposition follows the ε-invariance principle: changing the structural reading changes the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
