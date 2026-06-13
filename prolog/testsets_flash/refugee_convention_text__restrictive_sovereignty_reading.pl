% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention: Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents a 'restrictive sovereignty' reading of the
 *   1951 Refugee Convention, where the Convention is interpreted as setting a
 *   minimum floor for protection while maximizing state discretion in
 *   implementation. This reading emphasizes individualized proof of
 *   persecution, limits 'particular social group' to immutable
 *   characteristics with state awareness, and permits practices like offshore
 *   processing. It is a contested interpretation, often favored by states
 *   seeking to limit asylum claims.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary beneficiary (institutional/arbitrage) — benefits from discretion
 *   - border_control_agencies: Agenda setter (institutional/constrained) — enforces restrictive policies
 *   - asylum_seekers: Primary victim (powerless/trapped) — bears the burden of proof and limited access
 *   - refugee_advocacy_ngos: Payer (organized/constrained) — bears costs of legal challenges and support for rejected asylum seekers
 *   - international_courts: Observer (institutional/analytical) — adjudicates disputes over interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.85).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention: Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'cc88940f-600a-4ec2-be90-a3e2568b3962').
narrative_ontology:cs_kernel_codification('cc88940f-600a-4ec2-be90-a3e2568b3962', fixed_text).
narrative_ontology:cs_authority_grounding('cc88940f-600a-4ec2-be90-a3e2568b3962', extraction).
narrative_ontology:cs_interpretation_layer_present('cc88940f-600a-4ec2-be90-a3e2568b3962').
narrative_ontology:cs_reading_relation('cc88940f-600a-4ec2-be90-a3e2568b3962', refugee_convention_text__expansive_humanitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('cc88940f-600a-4ec2-be90-a3e2568b3962', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('cc88940f-600a-4ec2-be90-a3e2568b3962', foundational, sovereign_discretion_primary).
narrative_ontology:cs_axiom_status(sovereign_discretion_primary, holdable).
narrative_ontology:cs_axiom_grounding('cc88940f-600a-4ec2-be90-a3e2568b3962', sovereign_discretion_primary, conventional).
narrative_ontology:cs_axiom('cc88940f-600a-4ec2-be90-a3e2568b3962', foundational, individualized_persecution_proof_required).
narrative_ontology:cs_axiom_status(individualized_persecution_proof_required, holdable).
narrative_ontology:cs_axiom_grounding('cc88940f-600a-4ec2-be90-a3e2568b3962', individualized_persecution_proof_required, conventional).
narrative_ontology:cs_reference_frame('cc88940f-600a-4ec2-be90-a3e2568b3962', westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('cc88940f-600a-4ec2-be90-a3e2568b3962', contemporary_migration_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cc88940f-600a-4ec2-be90-a3e2568b3962', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_ngos).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from maximum discretion in interpreting and implementing the Refugee Convention, allowing them to control borders and limit asylum claims according to national interests. They leverage this reading to justify restrictive migration policies.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, beneficiary,
    institutional, generational, arbitrage, national).

% Are tasked with enforcing the restrictive interpretation of the Convention, including stringent admissibility screening, individualized persecution proof, and managing offshore processing. Their institutional mandate is aligned with this reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the primary costs of this restrictive reading, facing high burdens of proof, limited access to territory, prolonged detention, and a narrow definition of who qualifies for protection. Their lives are directly impacted by these interpretations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Incur significant costs in providing legal aid, humanitarian assistance, and advocating for asylum seekers who are denied protection under this restrictive reading. They challenge state practices and interpretations in courts and public discourse.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_ngos, payer,
    organized, generational, constrained, global).

% Adjudicate disputes between states and review national decisions concerning refugee status. Their rulings can influence the interpretation of the Convention, but their power is limited by state sovereignty and enforcement mechanisms.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_courts, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to manage migration flows and determine who qualifies for international protection, ensuring some level of order and shared understanding in a complex global challenge.
% TRANSFER_FUNCTION: Transfers the burden of proof and the costs of non-compliance (e.g., detention, deportation) onto asylum seekers and their advocates, while transferring discretion and control to sovereign states.
% ABSENT_VOICES: Asylum seekers who are denied protection and remain in precarious situations, or those who cannot even access the asylum system due to border restrictions, are effectively silenced. Their experiences are often represented by NGOs, but their direct voices are absent from the decision-making processes.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished, states would face immediate pressure to adopt more expansive interpretations, leading to increased asylum claims, changes in border policies, and a significant shift in the balance of power between states and asylum seekers. The global migration governance landscape would be fundamentally altered.
% FOUNDING_PROBLEM: The Refugee Convention was established to provide a legal framework for the protection of individuals fleeing persecution, particularly in the aftermath of World War II, to prevent statelessness and ensure basic human rights.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and border agencies argue the founding problem of managing migration and national security remains live, justifying restrictive measures. Refugee advocacy NGOs and international human rights bodies contend that the original problem of persecution persists, but the restrictive reading has rendered the Convention ineffective, shifting the problem from state persecution to state-imposed barriers to protection. This is corroborated by reports from UNHCR and human rights organizations documenting the challenges faced by asylum seekers.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it ostensibly coordinates international protection (the Convention's purpose) but does so with significant asymmetric extraction. The 'restrictive sovereignty' reading allows states to externalize costs of asylum processing and limit their obligations, while asylum seekers bear the burden of stringent proof and limited access to territory. Extractiveness is high (0.78) due to the high bar for protection and the costs imposed on asylum seekers. Suppression is also high (0.85) as states actively enforce border controls and legal interpretations to limit access. Theater ratio is low (0.20) because the enforcement is genuinely aimed at restricting access, not merely performing a function.
 *
 * PERSPECTIVAL GAP:
 *   Sovereign states and their border agencies experience this reading as a legitimate exercise of national sovereignty and a necessary coordination mechanism for managing migration flows. Asylum seekers and advocacy groups experience it as a highly extractive and suppressive barrier to protection, undermining the Convention's humanitarian purpose. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and border control agencies are beneficiaries (d near 0.0) as they gain maximum discretion and control over borders. Asylum seekers are clear targets (d near 1.0) due to the high burden of proof, limited access, and often precarious legal status. Refugee advocacy NGOs, while not directly paying the state, bear significant costs in supporting asylum seekers and challenging state policies, placing them as payers (d near 0.7).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (genuine coordination) by highlighting the significant extraction and suppression inherent in the 'restrictive sovereignty' reading. It also avoids mislabeling it as a pure Snare by acknowledging the underlying coordination function of the Refugee Convention itself, even if this reading heavily skews its operation. The contest over the Convention's interpretation is precisely about whether its mandate has atrophied into a cover for state interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine interpretation of the Refugee Convention''s text, or a policy choice disguised as legal reading?',
    'Comparative legal analysis across jurisdictions with different readings; historical analysis of drafting intent vs. contemporary state practice.',
    'If a policy choice, the constraint''s claimed ''naturalness'' as a legal interpretation collapses, revealing it as a Snare of state interest. If a genuine interpretation, its extractiveness is inherent to the legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''restrictive_sovereignty_reading'' of the ''refugee_convention_text'' kernel. Sibling readings (''expansive_humanitarian_reading'', ''procedural_integrity_reading'') would yield different classifications and beneficiary/victim sets.').

omega_variable(
    sovereign_discretion_boundary,
    'At what point does ''sovereign discretion'' in interpreting the Convention become a violation of its core non-refoulement principle?',
    'Adjudication by international courts (e.g., ECtHR, ICJ) on specific cases of state practice, establishing precedents for the limits of discretion.',
    'If discretion is found to routinely violate non-refoulement, the constraint''s legitimacy as a ''tangled_rope'' (balancing coordination and extraction) would erode, pushing it towards a ''snare'' of pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_discretion_boundary, empirical, 'The boundary between legitimate sovereign discretion and violation of international law is contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'refugee_convention_text' kernel, each representing a distinct interpretation with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
