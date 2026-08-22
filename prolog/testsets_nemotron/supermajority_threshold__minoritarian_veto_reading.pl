% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto Lock-In
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   A supermajority threshold (e.g., 2/3 or 3/4 for constitutional
 *   amendments) is presented as a safeguard for deep consensus. Under the
 *   minoritarian_veto_reading, the same threshold operates as a permanent
 *   veto for organized minorities who benefit from the status quo. The
 *   constraint extracts from contemporary majorities who cannot enact
 *   necessary reforms (climate policy, voting rights, economic
 *   redistribution) because the threshold converts historical
 *   overrepresentation (e.g., equal state suffrage in the US Senate,
 *   malapportioned upper chambers) into a durable blocking coalition. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as a consensus
 *   safeguard (rope/tangled_rope framing) while the authored metrics describe
 *   a snare with identifiable beneficiaries (entrenched elites, status quo
 *   beneficiaries) and victims (contemporary majorities blocked from reform).
 *   The engine measures that divergence; do not reconcile the claim to the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.82).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto Lock-In").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '254cb75a-15a9-4ca9-aef6-5369a9ad286d').
narrative_ontology:cs_kernel_codification('254cb75a-15a9-4ca9-aef6-5369a9ad286d', formalized).
narrative_ontology:cs_authority_grounding('254cb75a-15a9-4ca9-aef6-5369a9ad286d', lineage).
narrative_ontology:cs_interpretation_layer_present('254cb75a-15a9-4ca9-aef6-5369a9ad286d').
narrative_ontology:cs_reading_relation('254cb75a-15a9-4ca9-aef6-5369a9ad286d', supermajority_threshold__consensus_safeguard_reading, forecloses).
narrative_ontology:cs_reading_relation('254cb75a-15a9-4ca9-aef6-5369a9ad286d', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('254cb75a-15a9-4ca9-aef6-5369a9ad286d', foundational, supermajority_threshold_entrenches_historical_privilege).
narrative_ontology:cs_axiom_status(supermajority_threshold_entrenches_historical_privilege, holdable).
narrative_ontology:cs_axiom_grounding('254cb75a-15a9-4ca9-aef6-5369a9ad286d', supermajority_threshold_entrenches_historical_privilege, empirically_contingent).
narrative_ontology:cs_axiom('254cb75a-15a9-4ca9-aef6-5369a9ad286d', foundational, blocking_minority_veto_is_anti_democratic_lock_in).
narrative_ontology:cs_axiom_status(blocking_minority_veto_is_anti_democratic_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('254cb75a-15a9-4ca9-aef6-5369a9ad286d', blocking_minority_veto_is_anti_democratic_lock_in, deontological).
narrative_ontology:cs_reference_frame('254cb75a-15a9-4ca9-aef6-5369a9ad286d', founding_consensus_safeguard).
narrative_ontology:cs_drift_state('254cb75a-15a9-4ca9-aef6-5369a9ad286d', contemporary_democratic_erosion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('254cb75a-15a9-4ca9-aef6-5369a9ad286d', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities_blocked_from_reform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, minority_veto_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__minoritarian_veto_reading, constitutional_immutability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold disproportionate influence in the blocking minority coalition (e.g., small-state senators, malapportioned legislative chambers, judicial appointments). They benefit from policy stasis that protects their economic interests and political power. They can exit to other jurisdictions or leverage their position globally; the threshold secures their veto without requiring ongoing effort.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    powerful, generational, arbitrage, national).

% Organized interest groups (industry associations, incumbent firms, religious institutions) that mobilize to defend the threshold because it blocks reforms threatening their rents. They collect the benefits of blocked reform directly; they bear some cost of mobilization but net-benefit from the veto. Exit means abandoning their privileged position in the current system.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, payer).

% Broad popular majorities supporting reforms (climate action, voting rights restoration, wealth taxation, healthcare expansion) that consistently fail at the supermajority threshold. They bear the full cost of policy stasis — preventable deaths, economic inequality, democratic erosion — with no exit from the constitutional framework because their identity as citizens is fused to the polity. The threshold makes their democratic agency conditional on minority consent.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities_blocked_from_reform, payer,
    organized, biographical, identity_locked, national).

% Activists, scholars, and legislators who propose lowering the threshold or bypassing it (e.g., via statute, court packing, or constitutional convention). They are structurally excluded because the threshold itself blocks the meta-reform that would change the threshold. Their exclusion is the constraint's self-entrenchment mechanism.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_reform_advocates, excluded,
    moderate, biographical, constrained, national).

% Study supermajority rules across democracies and track outcomes: frequency of amendment, policy responsiveness, democratic legitimacy. They see the full structure — how the same threshold produces different classifications in different institutional contexts — but lack power to change any single case.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The threshold nominally coordinates by requiring broad agreement for fundamental law changes, preventing a bare majority from entrenching its preferences against a substantial minority.
% TRANSFER_FUNCTION: Moves policy-making authority from contemporary numerical majorities to organized minorities who can sustain a blocking coalition, transferring the power to enact or block reform from the many to the few.
% ABSENT_VOICES: Future generations who inherit the locked-in status quo; citizens in jurisdictions where the threshold does not exist and reforms pass; the poor and marginalized whose needs are most often blocked by veto coalitions representing entrenched wealth and power. They are absent because the threshold operates at the meta-constitutional level — they cannot vote on the rule that silences their vote.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, a wave of reforms blocked for decades (voting rights restoration, climate legislation, campaign finance reform, wealth taxation) would pass through simple-majority channels within months. The policy landscape would reorganize toward the preferences of numerical majorities. Entrenched elites would lose their permanent veto and have to contest each reform on the merits.
% FOUNDING_PROBLEM: Preventing transient majoritarian passion from making irreversible constitutional changes; protecting minority rights against majority tyranny; ensuring stability of the fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: The consensus_safeguard_reading attests the founding problem is live (citing ongoing risks of democratic backsliding). The minoritarian_veto_reading (this reading) and adaptive_gradient_reading attest it is substantially dead — historical analysis shows the threshold was often adopted to entrench slave-state power, property qualifications, or racial hierarchies, not as a neutral consensus tool. Independent constitutional historians (Ackerman, Levinson, Ginsburg) corroborate the genealogy of privilege-entrenchment from outside the beneficiary set.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the threshold allows a minority to block policies that would redistribute resources or power away from incumbent beneficiaries — the veto is the extraction mechanism. Suppression is high (0.82) because the constraint's persistence depends on the formal difficulty of amendment plus the internalized belief that supermajority requirements are normatively legitimate; resistance is substantial (0.72) because majoritarian reform movements repeatedly fail at the threshold. Theater ratio rises (0.45) because the 'consensus safeguard' framing increasingly masks the veto function as the gap between popular majorities and blocking minorities widens. The measurement series shows monotonic extractiveness growth over 25 time units as demographic and ideological sorting makes the blocking minority more cohesive and the blocked majority more numerous.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (entrenched elites), the threshold appears as a legitimate safeguard against transient majoritarian excess — the coordination story is believed. From the victim seat (contemporary majorities), the same structure operates as an anti-democratic lock-in with no legitimate justification. The engine computes this divergence from the structural data; the authored claim (snare) reflects the victim-seat reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites and status quo beneficiaries are structural beneficiaries (d near 0.0) — they collect the veto rents without bearing the cost of blocked reform. Contemporary majorities blocked from reform are structural targets (d near 1.0) — they bear the full cost of policy stasis with no exit from the constitutional framework (identity_locked exit). The analytical observer seat sees the full structure. The threshold's spatial scope is national/continental, amplifying effective extraction for the trapped majority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing hasty constitutional change) is contested — the threshold was often adopted under conditions that entrenched specific historical privileges (slave-state representation, property qualifications). The arrangement persists long after those conditions changed, converting a contingent safeguard into a permanent veto. The mandate has atrophied; the constraint now serves extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is this constraint one reading of the supermajority_threshold kernel, and how does the minoritarian_veto_reading structurally differ from its siblings?',
    'Compare the beneficiary/victim structure, extractiveness profile, and claimed type across all three declared readings (minoritarian_veto_reading, consensus_safeguard_reading, adaptive_gradient_reading). The divergence in ε values and structural relationships is the evidence.',
    'If the kernel frame is rejected, this story reverts to a flat constraint without committer structure; if confirmed, the three readings form a constraint family linked by network.affects_constraints with distinct ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Commiter-frame kernel membership and reading differentiation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (procedural barriers, legal entrenchment) or internalized (democratic actors self-censor because they believe the threshold is legitimate)?',
    'Post-threshold-removal observation: if reform movements accelerate immediately after a threshold is lowered or bypassed, the suppression was partly internalized; if reform remains blocked by other mechanisms, the suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression persists beyond its formal structure — the target carries the veto belief with them. This raises χ for the trapped majority seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in supermajority veto contexts').

omega_variable(
    historical_privilege_conversion,
    'To what extent do current veto-holding minorities trace their blocking power to historical privilege (e.g., slave-state representation, property qualifications, racial exclusions) rather than contemporary democratic legitimacy?',
    'Genealogical analysis of the specific supermajority rule''s adoption and its subsequent application — tracing the continuity between original design intent and present blocking coalitions.',
    'High continuity with historical privilege strengthens the snare classification; a clean break (new threshold adopted by broad consensus for new reasons) would support the consensus_safeguard_reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_privilege_conversion, empirical, 'Whether current veto power derives from historical privilege or contemporary consensus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(supe_tr_t5, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(supe_tr_t15, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(supe_be_t5, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(supe_be_t15, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(supe_su_t5, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(supe_su_t15, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 25, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__minoritarian_veto_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, constitutional_amendment_procedure).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, representative_legislative_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the supermajority_threshold kernel. The minoritarian_veto_reading (this file) treats the threshold as a snare with identifiable beneficiaries and victims. The consensus_safeguard_reading treats it as a genuine coordination mechanism (rope/tangled_rope). The adaptive_gradient_reading treats it as a calibratable scaffold. They differ in ε (0.78 vs ~0.25 vs ~0.45), beneficiary structure, and claimed type. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, organized, 0.15).
constraint_indexing:directionality_override(supermajority_threshold__minoritarian_veto_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
