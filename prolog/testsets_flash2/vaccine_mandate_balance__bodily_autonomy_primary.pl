% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Medical Mandates
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story represents the 'bodily_autonomy_primary' reading of
 *   the 'vaccine_mandate_balance' kernel. It asserts that individual consent
 *   to medical intervention is inviolable, and the state cannot compel such
 *   intervention, even for collective benefit. When mandates are imposed,
 *   individuals refusing them become victims of a highly extractive and
 *   suppressive system. The immunocompromised, while exposed to risk, are not
 *   considered victims under this reading, as their risk is framed as
 *   inherent to a liberty-prioritizing society.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.9).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy as Primary in Medical Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'd06a6863-cca1-4857-90b5-cd1aa743186f').
narrative_ontology:cs_kernel_codification('d06a6863-cca1-4857-90b5-cd1aa743186f', fixed_text).
narrative_ontology:cs_authority_grounding('d06a6863-cca1-4857-90b5-cd1aa743186f', lineage).
narrative_ontology:cs_interpretation_layer_present('d06a6863-cca1-4857-90b5-cd1aa743186f').
narrative_ontology:cs_reading_relation('d06a6863-cca1-4857-90b5-cd1aa743186f', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('d06a6863-cca1-4857-90b5-cd1aa743186f', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('d06a6863-cca1-4857-90b5-cd1aa743186f', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d06a6863-cca1-4857-90b5-cd1aa743186f', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('d06a6863-cca1-4857-90b5-cd1aa743186f', foundational, state_power_limited_by_individual_rights).
narrative_ontology:cs_axiom_status(state_power_limited_by_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('d06a6863-cca1-4857-90b5-cd1aa743186f', state_power_limited_by_individual_rights, deontological).
narrative_ontology:cs_reference_frame('d06a6863-cca1-4857-90b5-cd1aa743186f', post_nuremberg_code_era).
narrative_ontology:cs_drift_state('d06a6863-cca1-4857-90b5-cd1aa743186f', contemporary_pandemic_response, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d06a6863-cca1-4857-90b5-cd1aa743186f', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the principle that their bodily integrity cannot be violated by state action, regardless of collective benefit. Their identity is often fused with this principle, making 'exit' (i.e., compliance) a profound personal compromise.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    moderate, biographical, identity_locked, national).

% Individuals who face loss of employment, access to public spaces, or other severe penalties for refusing medical intervention. They bear the direct costs of state compulsion, with limited to no exit options once mandates are in place.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, local).

% The governmental entity attempting to compel medical intervention. From this reading's perspective, the state is overstepping its legitimate authority, acting as an extractor of bodily autonomy. Its power is constrained by constitutional limits and public resistance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, the_state, agenda_setter,
    institutional, generational, constrained, national).

% These authorities advocate for collective health measures, but their arguments for mandates are dismissed by this reading as secondary to individual rights. They are excluded from the core decision-making framework of this constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_authorities, excluded,
    organized, biographical, constrained, national).

% Individuals who are at high risk from infectious diseases and rely on herd immunity for protection. From this reading's perspective, their exposure is an inherent risk accepted within a framework of individual liberty, not a cost to be mitigated by compelling others.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_exposed_individuals, observer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint coordinates individual actions by establishing a clear boundary around personal medical decisions, preventing state overreach and ensuring individual liberty in health matters.
% TRANSFER_FUNCTION: It prevents the transfer of individual bodily autonomy to the state for collective benefit, effectively transferring the burden of collective risk management away from individuals and onto the collective's voluntary actions or other non-coercive strategies.
% ABSENT_VOICES: Public health authorities and advocates for vulnerable populations, who would argue for the necessity of collective action to protect the most susceptible, are absent from the core premise of this constraint, which prioritizes individual autonomy above all.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the state's power to compel medical interventions would expand significantly, leading to a fundamental shift in the relationship between the individual and the state regarding health decisions. This would reorganize legal frameworks, public health policy, and individual rights.
% FOUNDING_PROBLEM: The constraint was established to prevent state tyranny and protect individual liberty and bodily integrity from governmental overreach, particularly in medical contexts where historical abuses have occurred.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, civil liberties organizations, and historical records of medical ethics violations corroborate the ongoing relevance of protecting individual autonomy against state compulsion. This corroboration comes from sources independent of those directly asserting autonomy.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the state's action directly infringes upon a fundamental individual right, imposing severe costs (loss of livelihood, social exclusion) on those who refuse. Suppression is also very high (0.90) as the state employs coercive mechanisms to enforce compliance, leaving little to no genuine exit for targeted individuals. The theater ratio is low (0.10) because the enforcement is direct and functional, not performative; the state genuinely intends to compel compliance. Accessibility collapse is moderate (0.70) as alternatives to compliance (e.g., remote work, alternative social structures) are severely limited or non-existent for many. Resistance is high (0.80) reflecting significant public opposition and legal challenges to such mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state or public health authorities, the constraint might be viewed as a necessary 'rope' for collective coordination. However, from the 'bodily_autonomy_primary' reading, and for the individuals targeted by mandates, it is a 'snare' that extracts fundamental rights through coercion. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy are beneficiaries in that the constraint protects their core principle, even if they are not directly 'collecting' anything. Unvaccinated-coerced individuals are clear victims, bearing the direct costs of the state's actions. The state, as the agenda-setter, is the enforcer of the mandate, acting as the primary extractor from the perspective of this reading. Immunocompromised individuals are observers, as their situation is not directly addressed by the core principle of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling state compulsion as mere 'coordination' or 'scaffold' by highlighting the high extractiveness and suppression inherent in overriding individual bodily autonomy. It emphasizes that even if a collective benefit is claimed, the mechanism of compulsion, from this reading's perspective, operates as a snare, not a benign support structure. The 'live' status of the founding problem (preventing state tyranny) reinforces the ongoing relevance of this constraint's protective function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine instantiation of the ''bodily_autonomy_primary'' reading, or does it conflate with elements of ''proportionality_reading''?',
    'Detailed analysis of specific legal arguments and policy implementations: if any mandate allows for robust exemptions or requires strict thresholds for implementation, it leans towards proportionality.',
    'If conflated, the extractiveness and suppression might be overstated, and the classification could shift towards a ''tangled_rope'' or ''scaffold'' if a genuine, albeit flawed, coordination function is present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing pure bodily autonomy from a proportionality framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, job loss) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate-removal analysis: if non-compliance persists after legal penalties are lifted, internalized suppression is a stronger factor.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in medical mandates.').

omega_variable(
    collective_benefit_assessment,
    'Is the ''collective benefit'' claimed by the state a genuine, empirically verifiable outcome, or a rhetorical justification for compulsion?',
    'Independent epidemiological and public health outcome studies, comparing mandated vs. non-mandated populations and their health trajectories.',
    'If the collective benefit is found to be negligible or non-existent, the state''s justification for compulsion collapses, reinforcing the ''snare'' classification. If substantial, it highlights the tension between individual rights and collective welfare, but does not alter this reading''s core premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_benefit_assessment, empirical, 'Verifying the empirical basis of claimed collective health benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 5, 0.11).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 5, 0.83).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 10, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 10, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_balance' kernel, focusing on individual bodily autonomy. Sibling readings include 'public_health_primary' and 'proportionality_reading', which offer alternative framings of the balance between individual rights and collective welfare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
