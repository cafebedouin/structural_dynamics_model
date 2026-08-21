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
 *   human_readable: Provincial Popular Sovereignty Secession Claim
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint represents the 'popular sovereignty' reading of secession
 *   legitimacy, where a democratic majority within a provincial boundary
 *   holds ultimate sovereignty and a referendum result is self-legitimating.
 *   This reading asserts the provincial majority's right to unilaterally exit
 *   a federation, subordinating federal authority to popular will. It frames
 *   federal structures as potentially extractive if perceived as such by the
 *   majority. The constraint is classified as a Snare because it extracts
 *   from federal and minority populations, and its persistence relies on
 *   active enforcement of the majority's will, suppressing alternatives for
 *   those who disagree.
 *
 * KEY AGENTS:
 *   - secessionist_provincial_majority: Primary beneficiary (organized/mobile) — asserts self-determination
 *   - provincial_government: Agenda setter (institutional/constrained) — administers secession
 *   - federal_government: Primary payer (institutional/constrained) — loses territory and authority
 *   - provincial_minorities: Primary victims (powerless/trapped) — forced to exit federation
 *   - indigenous_treaty_holders: Excluded (organized/identity_locked) — pre-existing claims ignored
 *   - international_observers: Analytical observer (analytical/analytical) — monitors legitimacy
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
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, snare).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Popular Sovereignty Secession Claim").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '78a35a15-8911-44e1-840b-97f83caa7c8d').
narrative_ontology:cs_kernel_codification('78a35a15-8911-44e1-840b-97f83caa7c8d', distributed).
narrative_ontology:cs_authority_grounding('78a35a15-8911-44e1-840b-97f83caa7c8d', practice).
narrative_ontology:cs_interpretation_layer_present('78a35a15-8911-44e1-840b-97f83caa7c8d').
narrative_ontology:cs_reading_relation('78a35a15-8911-44e1-840b-97f83caa7c8d', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('78a35a15-8911-44e1-840b-97f83caa7c8d', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('78a35a15-8911-44e1-840b-97f83caa7c8d', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('78a35a15-8911-44e1-840b-97f83caa7c8d', foundational, popular_will_is_supreme).
narrative_ontology:cs_axiom_status(popular_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('78a35a15-8911-44e1-840b-97f83caa7c8d', popular_will_is_supreme, deontological).
narrative_ontology:cs_axiom('78a35a15-8911-44e1-840b-97f83caa7c8d', foundational, territorial_integrity_is_subordinate_to_self_determination).
narrative_ontology:cs_axiom_status(territorial_integrity_is_subordinate_to_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('78a35a15-8911-44e1-840b-97f83caa7c8d', territorial_integrity_is_subordinate_to_self_determination, deontological).
narrative_ontology:cs_reference_frame('78a35a15-8911-44e1-840b-97f83caa7c8d', unfettered_popular_sovereignty).
narrative_ontology:cs_drift_state('78a35a15-8911-44e1-840b-97f83caa7c8d', contemporary_federal_legal_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('78a35a15-8911-44e1-840b-97f83caa7c8d', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% This group asserts its right to self-determination through a provincial referendum, believing it can unilaterally withdraw from the federation. They perceive the federal system as extractive or unresponsive to their unique interests.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority, beneficiary,
    organized, generational, mobile, regional).

% The political entity that would administer the secession process, organize the referendum, and claim sovereignty on behalf of the provincial majority. It benefits from increased autonomy and control over provincial resources.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_government, agenda_setter,
    institutional, biographical, constrained, regional).

% The central authority that would lose territory, resources, and political stability if secession occurs. It views unilateral secession as a threat to national integrity and constitutional order, and would bear the costs of fragmentation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Citizens within the seceding province who do not support secession and would be forced to leave the federation against their will. Their rights and identities are subordinated to the provincial majority's will, with limited recourse.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minorities, payer,
    powerless, biographical, trapped, local).

% Indigenous nations whose ancestral lands span provincial and federal boundaries, holding treaties with the federal government. Their consent is often not sought or considered paramount by secessionist movements, despite their pre-existing sovereignty claims.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, excluded,
    organized, civilizational, identity_locked, local).

% Monitor the legitimacy of the referendum process and the human rights implications of secession. Their analysis can influence international recognition but does not directly enforce or prevent secession.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective will of a provincial majority to assert self-determination and establish a new sovereign entity, resolving internal disputes over political allegiance.
% TRANSFER_FUNCTION: Transfers ultimate political authority, control over natural resources, and tax revenues from the federal government to the newly independent provincial government, based on the will of the provincial majority.
% ABSENT_VOICES: Indigenous treaty holders, whose pre-existing sovereignty and treaty rights are often marginalized or ignored in provincial secession debates, would object to any unilateral action that abrogates their agreements with the federal crown. Provincial minorities who wish to remain part of the federation are also often excluded from meaningful participation in the secessionist agenda-setting.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty claim for unilateral secession vanished, the political landscape would fundamentally shift. Provincial majorities would lose a primary tool for asserting independence, federal authority would be strengthened, and the debate would revert to constitutional amendment or grievance-based arguments, fundamentally altering the power dynamics of federalism.
% FOUNDING_PROBLEM: The perceived inability of a distinct provincial population to fully exercise self-determination and control its own destiny within an existing federal structure, often fueled by cultural, linguistic, or economic grievances.
% FOUNDING_PROBLEM_CORROBORATION: The provincial majority and its government attest that the problem is live, citing historical grievances and distinct identity. Federal opposition parties and some international legal scholars corroborate the existence of a genuine, if contested, desire for self-determination, even if they disagree on the legitimacy of unilateral action.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the claim allows a provincial majority to unilaterally appropriate federal assets and impose a new political order on unwilling minorities and Indigenous peoples. Suppression (0.70) is significant as it actively overrides federal constitutional claims and the rights of internal minorities. Theater ratio is low (0.10) because the claim is a direct assertion of power, with little performative cover; its proponents genuinely seek to enact it. Accessibility collapse is moderate (0.40) as alternatives (like constitutional negotiation) are actively dismissed, but not entirely foreclosed. Resistance is high (0.80) from federal and minority groups.
 *
 * PERSPECTIVAL GAP:
 *   The provincial majority and government perceive this as a legitimate exercise of self-determination (closer to a Rope or even Mountain from their seat), while the federal government, provincial minorities, and Indigenous groups experience it as a highly extractive Snare. The engine's classification reflects the latter, as the constraint's operation demonstrably extracts from identifiable victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The secessionist provincial majority and provincial government are beneficiaries (d near 0.0) as they gain autonomy and resources. The federal government, provincial minorities, and Indigenous treaty holders are targets (d near 1.0) as they bear the costs of fragmentation, loss of rights, and abrogation of treaties. The 'mobile' exit option for the provincial majority reflects their perceived ability to unilaterally leave the federation, while 'trapped' for minorities reflects their lack of agency in the process.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it represents an active, contested claim rather than an atrophied function. The classification as a Snare prevents mislabeling this assertion of popular will as a benign coordination mechanism, highlighting its extractive impact on other parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    international_recognition_ambiguity,
    'Would a unilateral secession based on this reading gain widespread international recognition, or would it be largely rejected?',
    'Observation of international diplomatic responses and UN/international court rulings on similar cases.',
    'If widely recognized, the constraint''s effective power and legitimacy would increase, potentially reducing resistance from federal and minority groups. If rejected, the secession would be de-legitimized, increasing the costs of enforcement and potentially leading to re-integration pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_ambiguity, empirical, 'Uncertainty regarding the international community''s acceptance of unilateral secession based on popular sovereignty.').

omega_variable(
    minority_rights_protection,
    'To what extent would the rights and interests of provincial minorities be genuinely protected in a newly independent state formed under this reading?',
    'Analysis of proposed constitutional frameworks for the new state, and independent human rights assessments.',
    'If minority rights are robustly protected, the extractiveness from provincial minorities would decrease. If not, their victim status would be amplified, potentially leading to internal conflict or calls for external intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_rights_protection, preference, 'Uncertainty about the actual protection of minority rights post-secession.').

omega_variable(
    indigenous_consent_necessity,
    'Is Indigenous consent a necessary precondition for legitimate secession, even if not explicitly required by the ''popular sovereignty'' reading?',
    'Legal rulings from international courts on Indigenous self-determination and land rights in secession contexts.',
    'If Indigenous consent is deemed necessary, the ''popular sovereignty'' reading would be fundamentally challenged, potentially foreclosing unilateral secession without such consent. If not, the reading''s legitimacy would be reinforced, but at the cost of further marginalizing Indigenous claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_necessity, conceptual, 'Ambiguity regarding the role of Indigenous consent in the legitimacy of secession under this reading.').


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
narrative_ontology:measurement(sece_be_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(sece_su_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel. Its assertion of popular sovereignty directly challenges and influences other readings of secession legitimacy within federal systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
