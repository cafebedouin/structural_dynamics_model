% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Popular Sovereignty Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint describes the 'popular sovereignty' reading of secession
 *   legitimacy, where a democratic majority within a provincial boundary
 *   asserts ultimate sovereignty and the self-legitimating power of a
 *   referendum result. This reading positions federal authority as
 *   subordinate to popular will and validates 'extraction' claims if
 *   perceived by the majority. The constraint is classified as a Snare due to
 *   its high extractiveness from federal and minority populations, and its
 *   reliance on suppressing alternative constitutional or Indigenous claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.85).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, snare).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Popular Sovereignty Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '6a9303f9-e211-4800-91f3-833a805cbaf5').
narrative_ontology:cs_kernel_codification('6a9303f9-e211-4800-91f3-833a805cbaf5', distributed).
narrative_ontology:cs_authority_grounding('6a9303f9-e211-4800-91f3-833a805cbaf5', distributed).
narrative_ontology:cs_reading_relation('6a9303f9-e211-4800-91f3-833a805cbaf5', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('6a9303f9-e211-4800-91f3-833a805cbaf5', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a9303f9-e211-4800-91f3-833a805cbaf5', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('6a9303f9-e211-4800-91f3-833a805cbaf5', foundational, popular_will_is_supreme).
narrative_ontology:cs_axiom_status(popular_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('6a9303f9-e211-4800-91f3-833a805cbaf5', popular_will_is_supreme, deontological).
narrative_ontology:cs_axiom('6a9303f9-e211-4800-91f3-833a805cbaf5', foundational, provincial_boundaries_define_demos).
narrative_ontology:cs_axiom_status(provincial_boundaries_define_demos, holdable).
narrative_ontology:cs_axiom_grounding('6a9303f9-e211-4800-91f3-833a805cbaf5', provincial_boundaries_define_demos, conventional).
narrative_ontology:cs_reference_frame('6a9303f9-e211-4800-91f3-833a805cbaf5', unfettered_popular_sovereignty).
narrative_ontology:cs_drift_state('6a9303f9-e211-4800-91f3-833a805cbaf5', contemporary_federal_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6a9303f9-e211-4800-91f3-833a805cbaf5', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% This group, forming a democratic majority within a provincial boundary, asserts its right to self-determination and unilateral secession based on a referendum result. They perceive federal authority as extractive and believe their popular will supersedes constitutional or federal legal frameworks.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority, agenda_setter,
    organized, generational, constrained, regional).

% The federal government views the province as an integral part of the federation, bound by constitutional law. It faces the challenge of maintaining national unity and constitutional order against a popular sovereignty claim, potentially losing territory and resources.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, civilizational, constrained, national).

% These are citizens within the province who do not support secession. They face the prospect of being involuntarily removed from their national citizenship and legal framework, with limited recourse or exit options if the secession proceeds unilaterally.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    powerless, biographical, trapped, local).

% Indigenous nations whose ancestral lands and treaty rights span both provincial and federal jurisdictions. They assert that their sovereignty predates and is independent of both provincial and federal claims, and that no secession is legitimate without their consent, which is often not sought or respected by the secessionist majority.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, payer,
    organized, generational, identity_locked, local).

% International bodies and states that monitor self-determination movements and secession processes. They evaluate the legitimacy of the claim against international law, human rights, and democratic principles, but have no direct enforcement power.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for a provincial majority to assert its collective will and potentially re-coordinate its political and economic relationships outside the existing federal structure.
% TRANSFER_FUNCTION: Transfers ultimate political authority and control over provincial resources from the federal government to the secessionist provincial majority, potentially at the expense of provincial minorities and Indigenous treaty holders.
% ABSENT_VOICES: The voices of federal constitutionalists, who argue for the supremacy of the existing legal order, and Indigenous nations, whose pre-existing sovereignty is often overlooked, are marginalized or actively suppressed in the popular sovereignty narrative.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty claim vanished, the federal government's authority would be unchallenged within the province, secessionist movements would lose their primary legitimating argument, and the political landscape would stabilize around the existing constitutional framework.
% FOUNDING_PROBLEM: The perceived inability of a provincial majority to exercise full self-determination and control its own destiny and resources within a federal system, leading to feelings of political and economic marginalization.
% FOUNDING_PROBLEM_CORROBORATION: The secessionist provincial majority attests the problem is live, citing historical grievances and economic disparities. Federal constitutional scholars and provincial minorities dispute the severity of the problem or the legitimacy of the proposed solution, arguing that existing democratic mechanisms are sufficient. Indigenous treaty holders corroborate the existence of unresolved grievances but assert their own distinct sovereignty.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading enables a unilateral transfer of power and resources from the federal state and other internal groups to the secessionist majority. Suppression (0.70) is significant as it requires actively overriding or ignoring constitutional precedents, federal legal challenges, and Indigenous treaty rights. The theater ratio is low (0.20) because the claim is actively pursued and enforced through political action, rather than being a mere performance. The metrics reflect a hardening of the popular sovereignty position over time, with increasing perceived extraction and suppression of counter-arguments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the secessionist provincial majority (agenda_setter), this constraint is a legitimate exercise of self-determination, a 'Rope' for their collective action. However, from the federal government, provincial minorities, and Indigenous treaty holders (payers), it operates as a 'Snare' that extracts their rights, resources, and identity without consent. The engine's classification as Snare reflects the structural reality of extraction and suppression from these other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The secessionist provincial majority is the primary beneficiary and agenda-setter (d near 0.0), as the constraint directly empowers their political project. The federal government, provincial minorities, and Indigenous treaty holders are targets (d near 1.0), as they bear the costs of potential fragmentation, loss of rights, and disregard for existing legal frameworks. International observers are analytical, with d near 0.5, as they assess the situation without direct stakeholding.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to enable self-determination) is still 'live' for the secessionist majority. However, the analysis reveals that this mandate is used to justify substantial extraction from other parties, preventing mislabeling it as pure coordination. The persistence of the 'founding problem' is contested, highlighting the ongoing political struggle rather than a clear resolution or atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_popular_sovereignty,
    'Is ultimate sovereignty vested in the constitutional framework or in the democratic will of a provincial majority?',
    'A definitive ruling by a supreme court with recognized jurisdiction, or a constitutional amendment process that explicitly addresses secession.',
    'If constitutional sovereignty is affirmed, the popular sovereignty reading''s legitimacy collapses, reclassifying it as a ''Snare'' based on a false premise. If popular sovereignty is affirmed, the federal government''s position becomes a ''Snare'' against the provincial majority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_popular_sovereignty, conceptual, 'Ambiguity over the ultimate source of political authority in a federal system.').

omega_variable(
    indigenous_consent_threshold,
    'Does the legitimacy of provincial secession require the explicit consent of Indigenous treaty holders whose lands are within the provincial boundaries?',
    'Legal precedent from international courts or domestic supreme courts affirming Indigenous self-determination and treaty primacy in secession contexts.',
    'If Indigenous consent is required, the popular sovereignty reading is fundamentally undermined, as it typically proceeds without such consent, increasing its extractiveness from Indigenous peoples. If not, Indigenous claims remain a separate, but unaddressed, grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_threshold, preference, 'Whether Indigenous sovereignty acts as a legitimate boundary to provincial self-determination.').

omega_variable(
    minority_protection_mechanisms,
    'Are there sufficient mechanisms to protect the rights and interests of provincial minorities who oppose secession, or are they structurally vulnerable to the majority''s will?',
    'Independent human rights assessments and legal frameworks specifically designed to protect minority rights during secession processes.',
    'If minority protections are deemed insufficient, the extractiveness of the popular sovereignty reading from provincial minorities is amplified, reinforcing its ''Snare'' classification. If robust, it might mitigate some of the negative impacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_mechanisms, empirical, 'Vulnerability of provincial minorities in a unilateral secession scenario.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sece_tr_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(sece_tr_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(sece_be_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(sece_su_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_primacy_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
