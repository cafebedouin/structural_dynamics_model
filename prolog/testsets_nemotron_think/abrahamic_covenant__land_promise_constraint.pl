% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise (Ongoing Unconditional Reading)
 *   domain: religious_studies/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   The Abrahamic covenant's territorial grant (Genesis 12, 15, 17) is read
 *   by Israeli state actors and the settler movement as an unconditional,
 *   eternal deed to the Land of Canaan. This reading operates as a
 *   high-extraction constraint: it legitimizes military occupation,
 *   settlement expansion, and the denial of Palestinian return. The
 *   constraint is actively enforced through a matrix of military orders,
 *   planning laws, and citizenship rules. Beneficiaries (Israeli state,
 *   settlers) collect territory and sovereignty; victims (Palestinians) bear
 *   displacement, fragmentation, and statelessness. The reading's extraction
 *   has intensified since 1967 as settlement became state policy. Theater
 *   ratio reflects the gap between the constraint's religious-ritual framing
 *   and its material function as a land-acquisition mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.85).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.9).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.85).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise (Ongoing Unconditional Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, 'c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f').
narrative_ontology:cs_kernel_codification('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', fixed_text).
narrative_ontology:cs_authority_grounding('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', extraction).
narrative_ontology:cs_interpretation_layer_present('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f').
narrative_ontology:cs_reading_relation('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', foundational, land_promise_ongoing_unconditional).
narrative_ontology:cs_axiom_status(land_promise_ongoing_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', land_promise_ongoing_unconditional, theological).
narrative_ontology:cs_reference_frame('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', divine_land_grant_unconditional).
narrative_ontology:cs_drift_state('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', modern_zionist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c1a5a2eb-7a72-45c5-a507-3cb9c0c8d34f', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, settler_movement).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_population).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_authority).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_land_grant_to_abraham_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the land promise reading through military control, legal frameworks, and settlement policy. Uses the covenant narrative to legitimize sovereignty claims over contested territories. Collects territorial control and strategic depth as primary gains.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives land, housing subsidies, and state protection for settlements justified by the covenant reading. Their presence creates facts on the ground that reinforce the constraint. Exit would mean abandoning ideological commitment and material investments.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, settler_movement, beneficiary,
    organized, biographical, constrained, regional).

% Bears the costs of displacement, movement restrictions, resource appropriation, and denied sovereignty. The covenant reading is the theological-legal basis for the regime that extracts their land and autonomy. Exit options are virtually nonexistent — geographic, legal, and economic barriers are total.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_population, payer,
    powerless, generational, trapped, national).

% Refugees and their descendants denied return to lands now held under the covenant justification. Carry intergenerational trauma and statelessness. The constraint's enforcement architecture (citizenship laws, military orders) explicitly bars their return.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinians, payer,
    powerless, generational, trapped, regional).

% Administers truncated autonomy under military occupation. Bears the cost of security coordination that protects the settlement enterprise. Has limited agency to challenge the covenant reading's legal effects; exit would mean collapse of governing structures.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_authority, payer,
    moderate, biographical, constrained, regional).

% Produces legal opinions (ICJ, UN resolutions) that the constraint violates international law, but lacks enforcement leverage. Provides humanitarian aid that mitigates extraction effects without addressing the constraint's root.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The covenant reading coordinates Jewish collective identity and historical continuity, providing a unifying narrative for state-building and territorial claim in the face of diaspora and persecution.
% TRANSFER_FUNCTION: Moves land, water, resources, and political sovereignty from Palestinian population to Israeli state and settlers, justified by divine promise interpreted as an eternal deed.
% ABSENT_VOICES: Palestinian refugees in diaspora (5M+), secular and non-Zionist Jews who reject theological claims as political license, international legal bodies whose rulings are ignored — excluded from the interpretive framework that treats the promise as a real-estate deed.
% DISAPPEARANCE_RATIONALE: The constraint is the theological-legal foundation for the settlement enterprise, exclusive sovereignty claims, and the refusal of Palestinian right of return. Without it, the arrangement would revert to international law (1967 borders, refugee return negotiations) and the military-legal architecture justifying displacement would lose its foundational legitimacy.
% FOUNDING_PROBLEM: Establishing a divine basis for Jewish peoplehood and territorial attachment in the ancient Near East, securing group survival against empire and assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Biblical scholars attest the ancient context was conditional land tenure within a suzerainty treaty framework (Deuteronomy 28). Palestinian historians and international lawyers attest the modern reinterpretation as unconditional deed is a 19th-20th century Zionist construct. Israeli religious nationalists attest the ongoing divine mandate. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint transfers sovereign control of a territory and its resources from one population to another on theological grounds. Suppression is extreme (0.9) because the constraint's persistence depends on military enforcement, legal barriers to return, and the physical prevention of alternatives (Palestinian statehood). Theater ratio (0.4) indicates the religious narrative performs a coordination function for the beneficiary group but the dominant activity is material extraction. Accessibility collapse (0.8) reflects the near-total closure of political alternatives for Palestinians. Resistance (0.7) is high but structurally contained.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint appears as a rope (coordination of Jewish return and sovereignty). From the payer seats, it is a snare (theological cover for dispossession). The engine computes this divergence from the structural data — the claimed_type 'snare' reflects the payer-seat reality, which is the extraction-bearing seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state and settler movement are structural beneficiaries (d near 0): they collect the extraction (land, sovereignty) and control enforcement. Palestinian population and displaced Palestinians are full targets (d near 1): they bear the extraction with trapped exit. Palestinian Authority is a constrained payer (d ~0.7): it administers the occupation's civilian layer but cannot alter the constraint. International community is an analytical observer (d=0.5): it sees the structure but lacks leverage to change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ancient group survival) is contested as live/dead. The state actor treats it as live (redemption incomplete), which prevents mandatrophy resolution. The constraint persists because the beneficiary's identity is fused with the unfinished mandate — identity_locked exit for the beneficiaries themselves, which the engine will reflect in directionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_of_promise,
    'Is the land promise unconditional (eternal deed) or conditional on covenant obedience (Deuteronomy 28)?',
    'Textual-historical analysis of the covenant formulary in its ancient Near Eastern context; theological debate within Judaism (conditional vs. unconditional strands).',
    'If conditional, the snare structure weakens — the constraint''s legitimacy depends on compliance that the beneficiary may not meet, opening space for internal critique. If unconditional, the extraction is theologically insulated from moral challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_of_promise, conceptual, 'Whether the promise''s conditionality undermines the snare''s theological legitimacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (military occupation, legal barriers) or partially internalized (Palestinian acceptance of impossibility, fragmented leadership)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., in Gaza post-2005), reclassify as partially internalized. Longitudinal study of Palestinian political agency.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase effective extraction for the payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a geopolitical constraint.').

omega_variable(
    kernel_reading_identity,
    'Does this reading (land_promise_constraint) logically foreclose the ishmael_covenant_reading, or do they coexist as separate dimensions of the kernel?',
    'Analyze whether the land promise reading''s exclusivity claim (Covenant land for Isaac''s line only) is a necessary entailment or a contingent political addition. Check if Islamic tradition makes a competing territorial claim on Canaan.',
    'If forecloses, the kernel has a structural contradiction that cannot be resolved within a single framework — the engine would compute foreclosure. If coexists_with, the kernel contains multiple independent interpretive axes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between land promise reading and Ishmael lineage reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(abra_tr_t1993, abrahamic_covenant__land_promise_constraint, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.72).
narrative_ontology:measurement(abra_be_t1993, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1993, 0.75).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement(abra_su_t1993, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, israeli_settlement_policy).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, palestinian_displacement_regime).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, jerusalem_status_quo).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the abrahamic_covenant kernel. The isaac_covenant_reading and ishmael_covenant_reading address the lineage transmission axis; this reading addresses the territorial grant axis. All three are structurally linked because the land claim is typically paired with a specific lineage claim in political practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, institutional, 0.1).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, powerless, 0.95).
constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
