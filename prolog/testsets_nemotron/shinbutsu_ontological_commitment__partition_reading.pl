% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhism Domain Partition (Life-Cycle vs Afterlife)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint story captures the partition reading of the shinbutsu
 *   ontological commitment kernel: Shinto and Buddhism occupy separate ritual
 *   domains (life-cycle events like birth, marriage, purification vs
 *   afterlife/funerary/ancestral rites) without requiring ontological
 *   integration. Practitioners engage both systems pragmatically — Shinto for
 *   this-worldly vitality and communal coherence, Buddhism for afterlife
 *   security and ancestral care. The arrangement persists because it solves a
 *   genuine coordination problem: providing culturally intelligible ritual
 *   pathways for life's major transitions without demanding doctrinal
 *   reconciliation that would fracture either tradition's internal coherence.
 *   No single institution captures the gains; benefits distribute across
 *   household practitioners, temple-shrine networks, and local communities.
 *
 * KEY AGENTS:
 *   - household_practitioners: Primary participants (moderate/constrained) — navigate both domains by life-stage need
 *   - temple_shrine_network: Institutional infrastructure (organized/constrained) — provides ritual services in respective domains
 *   - local_communities: Collective beneficiaries (organized/constrained) — maintain shared ritual calendar and communal cohesion
 *   - scholarly_observers: Analytical seat (analytical/analytical) — reconstruct historical structure from textual and material record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.12).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.08).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhism Domain Partition (Life-Cycle vs Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '8322a00d-6805-405b-8007-83197be29fdb').
narrative_ontology:cs_kernel_codification('8322a00d-6805-405b-8007-83197be29fdb', distributed).
narrative_ontology:cs_authority_grounding('8322a00d-6805-405b-8007-83197be29fdb', practice).
narrative_ontology:cs_interpretation_layer_present('8322a00d-6805-405b-8007-83197be29fdb').
narrative_ontology:cs_reading_relation('8322a00d-6805-405b-8007-83197be29fdb', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('8322a00d-6805-405b-8007-83197be29fdb', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('8322a00d-6805-405b-8007-83197be29fdb', foundational, domain_specific_ritual_efficacy).
narrative_ontology:cs_axiom_status(domain_specific_ritual_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('8322a00d-6805-405b-8007-83197be29fdb', domain_specific_ritual_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('8322a00d-6805-405b-8007-83197be29fdb', foundational, practitioner_autonomy_in_ritual_choice).
narrative_ontology:cs_axiom_status(practitioner_autonomy_in_ritual_choice, holdable).
narrative_ontology:cs_axiom_grounding('8322a00d-6805-405b-8007-83197be29fdb', practitioner_autonomy_in_ritual_choice, conventional).
narrative_ontology:cs_reference_frame('8322a00d-6805-405b-8007-83197be29fdb', pre_meiji_practitioner_equilibrium).
narrative_ontology:cs_drift_state('8322a00d-6805-405b-8007-83197be29fdb', postwar_constitutional_separation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('8322a00d-6805-405b-8007-83197be29fdb', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, household_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, temple_shrine_network).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, local_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, domain_specific_ritual_efficacy).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, practitioner_autonomy_in_ritual_choice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigate Shinto for life-cycle events (birth, coming-of-age, marriage, purification) and Buddhism for funerals, memorial rites, and ancestral care. Choose temples/shrines based on family tradition, location, and perceived efficacy. No doctrinal commitment required; participation is pragmatic and life-stage driven. Exit options include secular ceremonies, new religious movements, or reduced ritual engagement — socially visible but not structurally blocked.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, household_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Buddhist temples monopolize funerary/afterlife domain; Shinto shrines monopolize life-cycle/communal purification domain. Each maintains ritual expertise, physical infrastructure, and parishioner (danka/uji) relationships. Revenue comes from offerings, ritual fees, and parishioner support. Domain separation prevents direct competition but creates mutual dependence: temples need shrines for life-cycle legitimacy, shrines need temples for afterlife completeness. Neither can absorb the other's domain without losing distinct institutional identity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, temple_shrine_network, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__partition_reading, temple_shrine_network, beneficiary).

% Maintain shared ritual calendar (matsuri, obon, hatsumode) that structures communal time and social cohesion. Benefit from both domains' ritual services without bearing full institutional costs. Collective participation reinforces community boundaries and intergenerational continuity. Exit would mean loss of shared ritual infrastructure — possible but socially costly.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, local_communities, beneficiary,
    organized, generational, constrained, local).

% Reconstruct historical structure from textual sources (ritual manuals, doctrinal treatises), material culture (temple-shrine complexes, votive objects), and ethnographic observation. Debate whether partition reflects lived practice or scholarly taxonomy. No stake in the constraint's persistence; analytical exit is costless.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides culturally intelligible, ritually adequate pathways for life's major transitions (birth, marriage, death, ancestral care) by allocating domains to traditions with established expertise — Shinto for this-worldly vitality and communal purification, Buddhism for afterlife security and ancestral pacification — without requiring practitioners to reconcile conflicting ontologies.
% TRANSFER_FUNCTION: Moves ritual fees, offerings, and parishioner support from household practitioners to temple-shrine institutions in exchange for domain-specific ritual services. No cross-domain subsidy; each institution sustains itself from its allocated domain.
% ABSENT_VOICES: Historical practitioners who may have experienced the partition as imposed rather than chosen (e.g., under Meiji shinbutsu bunri enforcement); marginalized groups (burakumin, itinerant performers) whose ritual needs fell outside both domains; contemporary secular Japanese who navigate life transitions without either tradition.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight, practitioners would face a coordination vacuum: no culturally default ritual pathway for major life transitions. Temples and shrines would compete directly for all ritual occasions, likely triggering doctrinal conflict and institutional instability. Local communities would lose shared ritual calendar. New religious movements or secular alternatives would eventually fill the gap, but the transition would rearrange the ritual economy substantially.
% FOUNDING_PROBLEM: Pre-modern Japanese society needed ritually effective pathways for both this-worldly vitality (agricultural fertility, communal purification, life-cycle transitions) and otherworldly security (afterlife destination, ancestral pacification, karmic resolution). No single tradition provided both without doctrinal strain; the partition solved this by allocating domains to the tradition with established ritual technology for each.
% FOUNDING_PROBLEM_CORROBORATION: Folklorists (Yanagita Kunio, Orii Shigeo) document the lived partition in village practice; historians (Kuroda Toshio, Hardacre Helen) note the Meiji state's attempt to impose separation confirms the partition's pre-existence as practitioner equilibrium. Buddhist and Shinto institutional histories attest complementary domain specialization. No single beneficiary group monopolizes the corroboration.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.12) reflects the absence of mandatory tithing, doctrinal enforcement, or institutional monopoly — practitioners engage each domain voluntarily by need. Low suppression (0.08) because alternatives (Christianity, new religions, secular rites) coexisted without systematic exclusion; the constraint's persistence derives from cultural fit, not coercion. Moderate theater ratio (0.22) captures the performative dimension of maintaining 'separate domains' as an explicit ideology (e.g., Meiji-era shinbutsu bunri rhetoric) against the messier historical reality of shinbutsu-shugo blending. Accessibility collapse (0.35) and resistance (0.40) are moderate: the partition is culturally salient but not cognitively inescapable — practitioners could and did adopt alternative frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner seat, the partition is invisible infrastructure — they simply use the right ritual for the right life moment. From the institutional seat, domain separation protects jurisdictional boundaries and revenue streams. From the analytical seat, the partition is a historically contingent equilibrium, not a natural law. The engine computes these divergences from the structural data; the partition reading's claimed_type=rope reflects the practitioner-seat experience of genuine coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Household practitioners are near-symmetric (d ~ 0.5): they receive ritual services in both domains and contribute offerings, with constrained but real exit options (secular rites, new religions). Temple-shrine networks are mild beneficiaries (d ~ 0.3): they collect offerings and maintain institutional relevance, but domain separation prevents monopolistic capture — each institution depends on the other's complementary domain. Local communities are beneficiaries (d ~ 0.2): they gain shared ritual calendar and social cohesion without bearing institutional maintenance costs. No agent is a structural victim — the arrangement lacks the asymmetric extraction required for tangled_rope or snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing ritually adequate pathways for life-cycle and afterlife transitions without demanding doctrinal unification — remains live (founding_problem_status: contested). The Meiji state's forced separation (shinbutsu bunri) attempted to convert this rope into a state-managed scaffold with sunset (State Shinto), but the partition reasserted itself post-1945 as practitioner-driven coordination. No mandatrophy resolution: the arrangement persists because the coordination problem it solves has not disappeared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the partition reading represent a genuine historical structure of practice, or is it a retrospective rationalization imposed on fluid historical practice?',
    'Comparative analysis of pre-Meiji ritual manuals, household registers, and temple-shrine administrative records to determine whether domain assignments were prescriptive norms or descriptive patterns.',
    'If prescriptive, the constraint has genuine coordination function; if retrospective, the low extraction/suppression metrics may reflect modern scholarly projection rather than historical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the partition reading captures lived structure or scholarly reconstruction').

omega_variable(
    beneficiary_structure_ambiguity,
    'Do temple-shrine complexes benefit from maintaining domain separation, or does the partition simply reflect practitioner preference without institutional capture?',
    'Economic analysis of temple-shrine revenue streams across domains (life-cycle rituals vs funerary/afterlife services) and historical records of inter-institutional negotiation over ritual jurisdiction.',
    'If institutions capture rents from domain monopoly, the constraint has extractive structure masked as coordination; if not, the rope classification is structurally stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether domain partition serves institutional extraction or genuine practitioner coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t0, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t25, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t25, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t50, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t50, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t75, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t75, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t100, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t0, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t25, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 25, 0.1).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t25, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t50, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 50, 0.11).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t50, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t75, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 75, 0.12).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t75, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t100, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_partition_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t0, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t25, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 25, 0.06).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t25, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t50, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 50, 0.07).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t50, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t75, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 75, 0.08).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t75, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t100, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 100, 0.08).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% Part of the shinbutsu_ontological_commitment constraint family. The partition reading (this story) models domain-separated coordination with ε≈0.12; the syncretic reading models unified cosmology under honji-suijaku with higher doctrinal integration demands; the incoherence reading models absence of stable commitment. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
