% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions Protective Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'state-centric' reading of the Geneva
 *   Conventions' protective scope, asserting that full combatant protections
 *   (e.g., POW status) primarily apply to uniformed combatants under
 *   responsible command meeting Article 4 criteria. Unprivileged
 *   belligerents, typically members of non-state armed groups, fall outside
 *   this scope, denying them combatant immunity and subjecting them to
 *   domestic law for acts of war. This reading is a specific interpretation
 *   of the kernel 'geneva_conventions_protective_scope' and is contested by
 *   other readings that advocate for broader protections.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions Protective Scope (State-Centric Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, 'afd22ce9-3f34-4663-b65b-8b1e1dddc172').
narrative_ontology:cs_kernel_codification('afd22ce9-3f34-4663-b65b-8b1e1dddc172', fixed_text).
narrative_ontology:cs_authority_grounding('afd22ce9-3f34-4663-b65b-8b1e1dddc172', lineage).
narrative_ontology:cs_interpretation_layer_present('afd22ce9-3f34-4663-b65b-8b1e1dddc172').
narrative_ontology:cs_reading_relation('afd22ce9-3f34-4663-b65b-8b1e1dddc172', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('afd22ce9-3f34-4663-b65b-8b1e1dddc172', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('afd22ce9-3f34-4663-b65b-8b1e1dddc172', foundational, state_sovereignty_in_warfare).
narrative_ontology:cs_axiom_status(state_sovereignty_in_warfare, holdable).
narrative_ontology:cs_axiom_grounding('afd22ce9-3f34-4663-b65b-8b1e1dddc172', state_sovereignty_in_warfare, conventional).
narrative_ontology:cs_axiom('afd22ce9-3f34-4663-b65b-8b1e1dddc172', foundational, reciprocity_of_combatant_status).
narrative_ontology:cs_axiom_status(reciprocity_of_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('afd22ce9-3f34-4663-b65b-8b1e1dddc172', reciprocity_of_combatant_status, deontological).
narrative_ontology:cs_reference_frame('afd22ce9-3f34-4663-b65b-8b1e1dddc172', post_wwii_state_centric_ihl).
narrative_ontology:cs_drift_state('afd22ce9-3f34-4663-b65b-8b1e1dddc172', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afd22ce9-3f34-4663-b65b-8b1e1dddc172', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a clear distinction between lawful combatants (who receive POW status if captured) and unprivileged belligerents (who do not). This reading simplifies targeting decisions and reduces legal liability for actions against non-state actors, especially in asymmetric conflicts.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, arbitrage, global).

% As the primary signatories and enforcers of the Geneva Conventions, they interpret and apply the treaties. This reading aligns with their interest in maintaining state sovereignty and control over the definition of legitimate warfare, particularly when facing non-state threats.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, mobile, global).

% These individuals, often members of non-state armed groups, are denied combatant immunity and POW status under this reading. They face prosecution under domestic law for acts of war and are subject to detention without the protections afforded to uniformed combatants, bearing the full cost of the constraint's narrow scope.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Their members are largely excluded from the protective scope of the Conventions under this reading, making their operations legally precarious. They bear the cost of their members being treated as criminals rather than combatants, which impacts recruitment, morale, and international legitimacy.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, constrained, regional).

% Interpret and apply international humanitarian law, including the Geneva Conventions. While they operate within the framework, this reading influences the scope of their jurisdiction over individuals involved in armed conflict, particularly regarding the definition of war crimes and combatant status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% Argue for a broader interpretation of protections, emphasizing the inherent human rights of all individuals in armed conflict, regardless of their combatant status. This reading's narrow scope directly contradicts their advocacy for universal application of humanitarian principles.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, state-centric framework for distinguishing between lawful combatants and other participants in armed conflict, providing a basis for reciprocal treatment of captured personnel and reducing ambiguity in military operations for state actors.
% TRANSFER_FUNCTION: Transfers legal protections (POW status, combatant immunity) from unprivileged belligerents to conventional state militaries, effectively shifting the burden of legal vulnerability in asymmetric conflicts onto non-state actors.
% ABSENT_VOICES: Unprivileged belligerents and human rights advocates are largely excluded from the interpretive process that defines the scope of these protections. They would argue for a more inclusive application of humanitarian law, emphasizing the inherent dignity and rights of all persons affected by conflict.
% DISAPPEARANCE_RATIONALE: If this state-centric reading vanished, the legal landscape of armed conflict would be fundamentally altered. State militaries would face increased legal uncertainty regarding targeting and detention, while non-state actors might claim broader protections, leading to a significant re-evaluation of military doctrine and international legal frameworks.
% FOUNDING_PROBLEM: The original Geneva Conventions aimed to codify the laws of war, primarily between states, to mitigate suffering and ensure humane treatment of captured combatants, particularly after the experiences of World War I and II.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and military historians corroborate the historical problem of regulating inter-state warfare. However, the 'live' status of the problem in its original form is contested by those who argue that contemporary conflicts (often involving non-state actors) have rendered the state-centric framework insufficient, as attested by human rights organizations and some legal experts outside state military circles.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it denies fundamental protections to a significant class of actors in modern conflicts, shifting legal vulnerability. Suppression (0.75) is high due to the active legal and military enforcement of this distinction, which includes detention without POW status and prosecution. Theater ratio (0.20) is relatively low, as the distinction is genuinely applied, though its justification is increasingly debated. The rise in extractiveness and suppression post-9/11 reflects the intensification of asymmetric conflicts and the legal responses to non-state actors.
 *
 * PERSPECTIVAL GAP:
 *   State actors perceive this reading as a necessary framework for maintaining order and distinguishing legitimate warfare, while non-state actors and human rights advocates view it as an extractive mechanism that denies fundamental rights and exacerbates suffering. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and governments are beneficiaries (d near 0.0) as this reading provides legal clarity and reduced liability for their operations. Unprivileged belligerents and non-state armed groups are clear victims (d near 1.0) as they are denied protections. International criminal courts and human rights advocates act as observers or excluded parties, respectively, influencing or challenging the interpretation without directly benefiting or paying in the same structural sense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_centric_necessity,
    'Is the state-centric distinction between combatants and unprivileged belligerents a structural necessity for maintaining the integrity and reciprocity of international humanitarian law, or is it a policy choice that serves state interests?',
    'Analysis of historical and contemporary conflicts where alternative frameworks (e.g., universal human rights law) have been applied, assessing their impact on conflict dynamics, reciprocity, and civilian protection.',
    'If a structural necessity, the extraction is an unavoidable cost of a functional system; if a policy choice, the extraction is a discretionary imposition that could be mitigated by alternative legal frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_necessity, conceptual, 'Whether the state-centric scope is a structural necessity or a policy choice.').

omega_variable(
    effectiveness_in_asymmetric_conflict,
    'Does the narrow, state-centric reading of protective scope effectively deter non-state armed groups from violating humanitarian law, or does it incentivize them to operate outside established norms due to lack of reciprocal protections?',
    'Empirical studies on the behavior of non-state armed groups in conflicts where this reading is applied, comparing outcomes with contexts where broader protections are afforded or claimed.',
    'If it deters, the suppression is effective; if it incentivizes non-compliance, the suppression is counterproductive, potentially increasing overall suffering and undermining the Conventions'' goals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_in_asymmetric_conflict, empirical, 'Impact of narrow scope on non-state actor compliance with IHL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement(gene_tr_t1991, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1977, 0.55).
narrative_ontology:measurement(gene_be_t1991, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1977, 0.65).
narrative_ontology:measurement(gene_su_t1991, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1991, 0.7).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.8).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
