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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Popular Sovereignty for Secession
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story analyzes the 'popular sovereignty' reading of
 *   secession legitimacy, where a democratic majority within a provincial
 *   boundary is considered to hold ultimate sovereignty, and a referendum
 *   result is self-legitimating. This reading is often invoked by
 *   secessionist movements in federal states, asserting a unilateral right to
 *   exit. The constraint is claimed as a 'snare' because it enables a
 *   provincial majority to extract political and economic control from the
 *   federal state and impose its will on internal minorities and Indigenous
 *   treaty holders, often against existing constitutional frameworks. The
 *   metrics reflect high extractiveness and suppression, as this reading
 *   actively challenges established authority and marginalizes dissenting
 *   voices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.85).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, snare).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Popular Sovereignty for Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '379bf3ee-4a0c-45de-9f89-569ec64e34b8').
narrative_ontology:cs_kernel_codification('379bf3ee-4a0c-45de-9f89-569ec64e34b8', distributed).
narrative_ontology:cs_authority_grounding('379bf3ee-4a0c-45de-9f89-569ec64e34b8', practice).
narrative_ontology:cs_interpretation_layer_present('379bf3ee-4a0c-45de-9f89-569ec64e34b8').
narrative_ontology:cs_reading_relation('379bf3ee-4a0c-45de-9f89-569ec64e34b8', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('379bf3ee-4a0c-45de-9f89-569ec64e34b8', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('379bf3ee-4a0c-45de-9f89-569ec64e34b8', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('379bf3ee-4a0c-45de-9f89-569ec64e34b8', foundational, provincial_majority_holds_ultimate_sovereignty).
narrative_ontology:cs_axiom_status(provincial_majority_holds_ultimate_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('379bf3ee-4a0c-45de-9f89-569ec64e34b8', provincial_majority_holds_ultimate_sovereignty, deontological).
narrative_ontology:cs_axiom('379bf3ee-4a0c-45de-9f89-569ec64e34b8', foundational, referendum_result_is_self_legitimating).
narrative_ontology:cs_axiom_status(referendum_result_is_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('379bf3ee-4a0c-45de-9f89-569ec64e34b8', referendum_result_is_self_legitimating, conventional).
narrative_ontology:cs_reference_frame('379bf3ee-4a0c-45de-9f89-569ec64e34b8', unilateral_provincial_self_determination).
narrative_ontology:cs_drift_state('379bf3ee-4a0c-45de-9f89-569ec64e34b8', contemporary_federal_legal_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('379bf3ee-4a0c-45de-9f89-569ec64e34b8', '').
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

% This group asserts its right to self-determination through a referendum, viewing the provincial boundary as the legitimate unit of democratic decision-making. They benefit from the perceived ability to unilaterally exit and control provincial resources.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority, agenda_setter,
    organized, generational, mobile, regional).

% The federal government views this claim as a threat to national unity and constitutional order. It bears the cost of potential territorial loss, economic disruption, and the erosion of its authority. Its options are limited to negotiation or legal challenge.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, civilizational, constrained, national).

% These groups, often linguistic or cultural minorities within the province, fear being left outside the federal framework and losing protections. They are trapped by the provincial boundary and the majority's decision, with limited recourse.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    powerless, biographical, trapped, local).

% Indigenous nations with pre-existing treaties with the federal government view provincial secession as a violation of their inherent sovereignty and treaty rights. Their identity and land claims are deeply tied to these treaties, making exit from the federal relationship unthinkable without consent.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, payer,
    organized, generational, identity_locked, regional).

% International bodies and states observe the process, weighing principles of self-determination against territorial integrity and constitutional law. Their pronouncements can influence legitimacy but do not directly enforce outcomes.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading attempts to coordinate the will of a provincial majority with the existing federal structure, by asserting the primacy of popular will within a defined territory as the ultimate arbiter of political association.
% TRANSFER_FUNCTION: It transfers ultimate political authority from the federal constitutional framework to the provincial democratic majority, potentially transferring control over resources and territory from the federal state to the new entity.
% ABSENT_VOICES: The voices of federalists within the province, and those who prioritize constitutional legality over popular will, are often marginalized in the immediate aftermath of a referendum. Indigenous treaty holders, whose claims predate both federal and provincial authority, are also often excluded from the framing of 'popular sovereignty' within provincial boundaries.
% DISAPPEARANCE_RATIONALE: If this reading of the legitimacy boundary vanished, the political landscape would fundamentally shift. Provincial majorities would lose a key argument for unilateral secession, strengthening federal authority and constitutional constraints. The debate would revert to constitutional amendment or a higher threshold of grievance, rather than simple majority will.
% FOUNDING_PROBLEM: The constraint was built to resolve the tension between the right to self-determination of a distinct people and the principle of territorial integrity of a federal state, particularly when a provincial majority feels its interests are not adequately represented by the federal government.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by secessionist movements globally, who consistently invoke popular sovereignty as a basis for self-determination. Federal governments and constitutional scholars, while disagreeing on the solution, acknowledge the persistent tension between these principles. International law also grapples with this dilemma, indicating its ongoing relevance.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.85) is high because this reading allows a provincial majority to unilaterally claim resources and territory, imposing significant costs on the federal state and internal minorities. Suppression (0.70) is also high, as this reading inherently suppresses alternative constitutional interpretations and the rights of those who do not consent to secession within the provincial boundaries. The theater ratio is low (0.10) because the claim is a direct assertion of power, with little performative cover; its enforcement is direct political action. The increasing extractiveness and suppression over time reflect the hardening of positions and the growing assertiveness of this reading in political discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the secessionist provincial majority, this is a 'rope' or even a 'mountain' – an unalienable right to self-determination. From the federal government's perspective, it is a 'snare' that threatens national integrity. For provincial minorities and Indigenous treaty holders, it is a 'snare' that extracts their rights and security. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The secessionist provincial majority is the clear beneficiary (d=0.0-0.1), gaining the power to unilaterally determine its future and control resources. The federal government, provincial minorities, and Indigenous treaty holders are all targets (d=0.8-1.0), bearing the costs of potential fragmentation, loss of rights, and violation of treaties. The 'mobile' exit option for the provincial majority reflects their perceived ability to leave the federation, while 'trapped' and 'identity_locked' describe the lack of viable exit for minorities and treaty holders, respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive political claim as mere 'coordination' or 'self-determination' without acknowledging the costs imposed on other parties. By identifying it as a 'snare', the framework highlights the coercive and asymmetric nature of this specific reading of sovereignty, particularly its impact on those who do not consent to the provincial majority's will. The 'live' status of the founding problem, coupled with high extractiveness, indicates that the constraint is actively serving an extractive function, rather than being a vestigial 'piton'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_popular_sovereignty,
    'Is ultimate sovereignty vested in the constitution (as interpreted by federal courts) or in the popular will of a provincial majority?',
    'A definitive ruling by a supreme court that is accepted by all parties, or a constitutional amendment process that explicitly clarifies secession procedures.',
    'If constitutional sovereignty prevails, this reading''s legitimacy collapses, and its extractiveness would be reclassified as illegitimate coercion. If popular sovereignty is affirmed, the federal government''s claims of authority would be weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_popular_sovereignty, conceptual, 'Ambiguity over the ultimate source of political authority in a federal system.').

omega_variable(
    indigenous_treaty_primacy,
    'Do Indigenous treaty rights, which often predate federal and provincial authority, supersede the popular sovereignty claim of a provincial majority?',
    'International legal arbitration or a domestic constitutional court ruling explicitly defining the relationship between Indigenous sovereignty, federal authority, and provincial secession claims.',
    'If treaty primacy is affirmed, the provincial majority''s claim to unilateral secession would be significantly constrained, and Indigenous nations would gain a veto or require explicit consent. If denied, Indigenous rights would be further suppressed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_treaty_primacy, conceptual, 'The unresolved conflict between Indigenous sovereignty and provincial self-determination.').

omega_variable(
    minority_rights_protection,
    'Are the rights of minorities within a seceding province adequately protected by the popular sovereignty reading, or does it inherently lead to their suppression?',
    'Empirical observation of post-secession outcomes in similar contexts, or the establishment of robust, internationally recognized minority protection mechanisms prior to any secession.',
    'If minority rights are systematically suppressed, the extractiveness of this reading would be further amplified, and its legitimacy severely questioned. If robust protections are demonstrated, the suppression metric might decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_rights_protection, empirical, 'The impact of provincial secession on internal minority rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(sece_tr_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(sece_tr_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(sece_be_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1995, 0.8).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(sece_su_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel. Its ε value differs significantly from other readings due to its assertion of unilateral provincial sovereignty and the resulting extraction from federal and minority interests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
