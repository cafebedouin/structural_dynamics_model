% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty Reading: Bounded Free Movement for Welfare Protection
 *   domain: political/economic/federalism/migration/welfare_state
 *
 * SUMMARY:
 *   This constraint instantiates the member_sovereignty_reading of the
 *   federation_membership_kernel — the position that free movement rights
 *   must be bounded by national welfare capacity and labor market protection.
 *   It claims to coordinate genuine cross-border solidarity problems
 *   (preventing welfare tourism, preserving actuarial balance) while
 *   extracting asymmetrically from constrained migrants and sending states.
 *   The claimed type is tangled_rope: real coordination function (welfare
 *   system interoperability) combined with asymmetric extraction (mobile
 *   workers and sending states bear the cost of bounded solidarity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.72).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty Reading: Bounded Free Movement for Welfare Protection").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political/economic/federalism/migration/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'a7a4b4d6-005b-4513-9cb8-172e7be4a862').
narrative_ontology:cs_kernel_codification('a7a4b4d6-005b-4513-9cb8-172e7be4a862', formalized).
narrative_ontology:cs_authority_grounding('a7a4b4d6-005b-4513-9cb8-172e7be4a862', lineage).
narrative_ontology:cs_interpretation_layer_present('a7a4b4d6-005b-4513-9cb8-172e7be4a862').
narrative_ontology:cs_reading_relation('a7a4b4d6-005b-4513-9cb8-172e7be4a862', federation_membership_kernel__integration_reading, forecloses).
narrative_ontology:cs_reading_relation('a7a4b4d6-005b-4513-9cb8-172e7be4a862', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('a7a4b4d6-005b-4513-9cb8-172e7be4a862', foundational, national_welfare_autonomy_precedes_supranational_mobility).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_precedes_supranational_mobility, holdable).
narrative_ontology:cs_axiom_grounding('a7a4b4d6-005b-4513-9cb8-172e7be4a862', national_welfare_autonomy_precedes_supranational_mobility, conventional).
narrative_ontology:cs_axiom('a7a4b4d6-005b-4513-9cb8-172e7be4a862', foundational, economically_inactive_exclusion_preserves_solidarity).
narrative_ontology:cs_axiom_status(economically_inactive_exclusion_preserves_solidarity, holdable).
narrative_ontology:cs_axiom_grounding('a7a4b4d6-005b-4513-9cb8-172e7be4a862', economically_inactive_exclusion_preserves_solidarity, instrumental).
narrative_ontology:cs_reference_frame('a7a4b4d6-005b-4513-9cb8-172e7be4a862', treaty_of_rome_worker_mobility_bargain).
narrative_ontology:cs_drift_state('a7a4b4d6-005b-4513-9cb8-172e7be4a862', post_eastern_enlargement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a7a4b4d6-005b-4513-9cb8-172e7be4a862', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_administrations).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_organized_labor).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_social_solidarity_institutions).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, constrained_mobility_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, welfare_state_sustainability_requires_bounded_mobility).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, labor_market_protection_justifies_mobility_restrictions).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, national_solidarity_institutions_precede_supranational_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer national welfare systems protected from cross-border claims by economically inactive migrants. Retain authority to exclude non-contributing claimants and set contribution thresholds. Benefit from reduced fiscal exposure and preserved actuarial balance. Constrained by EU law compliance requirements and judicial review.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_administrations, beneficiary,
    institutional, generational, constrained, national).

% Advocate for labor market protections that limit competition from incoming workers. Use collective bargaining and political influence to shape mobility restrictions. Benefit from protected wage floors and working conditions. Constrained by EU free movement jurisprudence that limits protectionist measures.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_organized_labor, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, receiving_state_organized_labor, agenda_setter).

% Embody the social contract binding national communities — pension systems, healthcare funds, unemployment insurance. Their legitimacy depends on bounded membership. View unrestricted mobility as existential threat to solidarity. Cannot exit the national frame without dissolving their purpose.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_social_solidarity_institutions, beneficiary,
    institutional, generational, identity_locked, national).

% Economically active workers whose mobility is restricted by contribution thresholds, waiting periods, and benefit portability barriers. Bear costs of delayed access, reduced portability, and legal uncertainty. Can move but face friction that reduces effective mobility. Exit options limited by career and family ties.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, constrained_mobility_migrants, payer,
    moderate, biographical, constrained, continental).

% Workers in lower-wage member states whose mobility is restricted by receiving-state barriers. Face brain drain from selective outmigration of skilled workers while low-skilled workers remain trapped. Bear costs of reduced remittances, skill depletion, and demographic decline. Exit requires migration that receiving states block.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_workers, payer,
    powerless, biographical, trapped, continental).

% Retirees, students, caregivers, and family members excluded by 'economically inactive' criteria. Denied equal treatment in welfare access despite formal free movement rights. Bear costs of exclusion from healthcare, social assistance, and family reunification. No leverage to contest; exit means returning to origin state.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, continental).

% Interpret and enforce the tension between free movement and member state derogations. Case law oscillates between expansive citizenship rights and proportionality-based national exceptions. Set the legal boundary that all other actors navigate. Not a direct beneficiary or payer of the constraint's material transfers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, ecj_and_supranational_courts, agenda_setter,
    institutional, generational, analytical, continental).

% Legislate coordination measures and monitor compliance. Political pressure from member states limits supranational harmonization. Observe the constraint's operation through infringement proceedings and policy coordination. Neither collect rents nor bear direct costs.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_commission_and_parliament, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national welfare systems within a common market by defining the boundaries of cross-border solidarity — preventing social dumping and fiscal free-riding while preserving member state welfare design autonomy.
% TRANSFER_FUNCTION: Transfers fiscal risk and administrative burden from receiving-state welfare systems to constrained migrants and sending states: receiving states avoid non-contributor costs; sending states retain workers but lose skilled migrants; constrained migrants bear access delays and portability gaps.
% ABSENT_VOICES: Third-country nationals with no free movement rights; future generations who inherit the demographic and fiscal consequences; mobile workers who never migrate because barriers are too high — they are not in the room because the constraint defines them out of the constituency.
% DISAPPEARANCE_RATIONALE: If bounded mobility restrictions vanished overnight, receiving-state welfare systems would face immediate cross-border claims from economically inactive migrants, triggering either fiscal crisis or rapid supranational harmonization. Labor markets would experience wage and condition convergence pressure. The national welfare state model would require fundamental restructuring or collapse.
% FOUNDING_PROBLEM: Post-war European integration required reconciling free movement of workers with nationally bounded welfare states — the 'social Europe' dilemma. Member states would not accept free movement without guarantees that their welfare systems would not be bankrupted by cross-border claims from non-contributors.
% FOUNDING_PROBLEM_CORROBORATION: Founding treaties and early ECJ case law (e.g., Hoeckx, Van Binsbergen) attest to the original bargain: free movement for workers, not unlimited welfare portability. Receiving-state governments and labor unions corroborate the problem persists — fiscal exposure has grown with enlargement. Integration_reading proponents (Commission, Parliament, mobile citizens' groups) contest that the problem is solved by coordination, not restriction.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the accumulated restriction of mobility rights over decades — from worker-only free movement to increasingly conditioned access for economically inactive persons. Suppression (0.72) captures active enforcement: contribution thresholds, waiting periods, genuine link tests, and judicial proportionality reviews that maintain the boundary. Theater ratio (0.42) rises as 'solidarity' rhetoric increasingly covers what functions as labor market protection and fiscal defense. Accessibility collapse (0.65) is high — alternatives (unrestricted mobility, supranational welfare harmonization) are structurally foreclosed by the reading's own premises. Resistance (0.58) is moderate — mobile workers litigate, sending states negotiate, but the constraint's architecture channels resistance into incremental adjustments rather than structural challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving-state beneficiary seats, the constraint is genuine coordination — it solves the real problem of welfare system sustainability in an open market. From the constrained migrant and sending state payer seats, the same structure operates as enforced extraction — their mobility and development prospects are sacrificed for receiving-state actuarial balance. The engine computes this divergence from the declared structural positions; the claimed type (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state welfare administrations and organized labor are structural beneficiaries (d near 0.1-0.2): they collect protection from cross-border claims and labor market competition. Constrained_mobility_migrants are targets (d ~0.7-0.8): they pay through restricted access and portability gaps. Sending_state_workers and economically_inactive_migrants are deeper targets (d ~0.85-0.95): trapped by structural barriers with no voice in the constraint's design. National_solidarity_institutions are identity-locked beneficiaries — they cannot exit the national frame without dissolving. ECJ sits at analytical d=0.5 but shapes the constraint's effective extraction through proportionality doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling free movement with welfare sustainability) remains live but has mutated: original bargain protected worker mobility; current operation increasingly restricts it. The mandate has not atrophied — welfare systems still need cross-border coordination — but the reading's extraction has accumulated beyond the coordination function. This is not mandatrophy (Piton) but Tangled Rope drift: coordination persists but extraction has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination of national welfare systems structurally inseparable from the asymmetric extraction imposed on constrained migrants, or could a pure coordination mechanism (portability without restrictions) achieve the same welfare sustainability?',
    'Natural experiment from coordination measures that reduce restrictions (e.g., EU Regulations 883/2004, 987/2009) — if welfare systems remain sustainable with fewer mobility barriers, extraction is separable from coordination.',
    'If separable, the constraint is a Snare using coordination as cover; if inseparable, the extraction is the price of coordination and Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether welfare coordination requires mobility restrictions or can function with pure portability.').

omega_variable(
    brain_drain_as_extraction_mechanism,
    'Does the selective mobility permitted by this reading (skilled workers move, low-skilled trapped) function as an extraction mechanism transferring human capital from sending to receiving states, or is it an unintended side effect of legitimate welfare protection?',
    'Longitudinal analysis of skill flows, remittance patterns, and demographic trajectories in sending vs. receiving states under the current regime vs. counterfactual unrestricted mobility.',
    'If brain drain is structural extraction, the constraint transfers developmental capacity from poorer to richer members — a federation-level extraction not captured by national welfare accounting. Would reclassify toward Snare at the federation level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_as_extraction_mechanism, empirical, 'Whether selective mobility constitutes federation-level extraction from sending states.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the federation_membership_kernel admit a single coherent framing, or do the three readings instantiate fundamentally different kernels (citizenship kernel, welfare coordination kernel, sovereignty kernel) that only share a label?',
    'Compare the structural data (ε, beneficiaries, victims, enforcement) across all three readings. If ε values differ by >0.3 and beneficiary/victim sets are disjoint, they are distinct constraints mislabeled as one kernel.',
    'If distinct kernels, the ''contestation'' is a category error — each reading classifies independently without a shared referent. The kernel_id would be analytical fiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three readings share a structural referent or are distinct constraints under a shared label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmk_msr_tr_t1957, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1957, 0.15).
narrative_ontology:measurement(fmk_msr_tr_t1973, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(fmk_msr_tr_t1986, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1986, 0.22).
narrative_ontology:measurement(fmk_msr_tr_t1992, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(fmk_msr_tr_t2004, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(fmk_msr_tr_t2011, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2011, 0.39).
narrative_ontology:measurement(fmk_msr_tr_t2016, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2016, 0.41).
narrative_ontology:measurement(fmk_msr_tr_t2024, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fmk_msr_be_t1957, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1957, 0.25).
narrative_ontology:measurement(fmk_msr_be_t1973, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1973, 0.32).
narrative_ontology:measurement(fmk_msr_be_t1986, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1986, 0.41).
narrative_ontology:measurement(fmk_msr_be_t1992, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1992, 0.48).
narrative_ontology:measurement(fmk_msr_be_t2004, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement(fmk_msr_be_t2011, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2011, 0.63).
narrative_ontology:measurement(fmk_msr_be_t2016, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2016, 0.67).
narrative_ontology:measurement(fmk_msr_be_t2024, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmk_msr_su_t1957, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1957, 0.35).
narrative_ontology:measurement(fmk_msr_su_t1973, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement(fmk_msr_su_t1986, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1986, 0.51).
narrative_ontology:measurement(fmk_msr_su_t1992, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1992, 0.58).
narrative_ontology:measurement(fmk_msr_su_t2004, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(fmk_msr_su_t2011, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2011, 0.69).
narrative_ontology:measurement(fmk_msr_su_t2016, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2016, 0.71).
narrative_ontology:measurement(fmk_msr_su_t2024, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the federation_membership_kernel. The integration_reading claims free movement as fundamental citizenship right (low extraction, Mountain-claimed). The welfare_coordination_reading claims coordination without harmonization (moderate extraction, Rope-claimed). This reading claims bounded mobility for welfare protection (high extraction, Tangled Rope). The three ε values differ substantially (est. 0.15, 0.35, 0.68) confirming distinct structural constraints under a shared label — the BGS decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, organized, 0.2).
constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, moderate, 0.75).
constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
