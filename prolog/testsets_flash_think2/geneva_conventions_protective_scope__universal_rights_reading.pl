% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Protections: Universal Rights Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universal rights' reading of the Geneva
 *   Conventions' protective scope, asserting that protections extend to all
 *   persons affected by armed conflict, regardless of combatant status, with
 *   Common Article 3 and human rights law establishing a universal floor.
 *   This reading significantly expands the victim set and imposes substantial
 *   restrictions on state military operations, often against state
 *   resistance. The claimed type is 'rope' reflecting the ideal of universal
 *   coordination for protection, but the metrics reflect the ongoing
 *   contestation and extraction from state operational flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.7).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.65).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Protections: Universal Rights Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '71678c12-0f18-4cdf-9178-7c0433070493').
narrative_ontology:cs_kernel_codification('71678c12-0f18-4cdf-9178-7c0433070493', fixed_text).
narrative_ontology:cs_authority_grounding('71678c12-0f18-4cdf-9178-7c0433070493', lineage).
narrative_ontology:cs_interpretation_layer_present('71678c12-0f18-4cdf-9178-7c0433070493').
narrative_ontology:cs_reading_relation('71678c12-0f18-4cdf-9178-7c0433070493', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_reading_relation('71678c12-0f18-4cdf-9178-7c0433070493', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_axiom('71678c12-0f18-4cdf-9178-7c0433070493', foundational, human_dignity_is_universal_and_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_is_universal_and_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('71678c12-0f18-4cdf-9178-7c0433070493', human_dignity_is_universal_and_inalienable, deontological).
narrative_ontology:cs_axiom('71678c12-0f18-4cdf-9178-7c0433070493', foundational, common_article_3_is_minimum_floor_for_all_conflicts).
narrative_ontology:cs_axiom_status(common_article_3_is_minimum_floor_for_all_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('71678c12-0f18-4cdf-9178-7c0433070493', common_article_3_is_minimum_floor_for_all_conflicts, conventional).
narrative_ontology:cs_reference_frame('71678c12-0f18-4cdf-9178-7c0433070493', post_wwii_human_rights_era).
narrative_ontology:cs_drift_state('71678c12-0f18-4cdf-9178-7c0433070493', contemporary_asymmetric_warfare_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('71678c12-0f18-4cdf-9178-7c0433070493', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, unprivileged_belligerents).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, human_rights_advocates).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, states_seeking_operational_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These bodies (e.g., ICRC, UN Human Rights Council, ICC) actively interpret and promote the expansive application of IHL and human rights law, pushing for universal protections regardless of status. They issue guidelines, monitor compliance, and adjudicate violations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_legal_bodies, agenda_setter,
    institutional, generational, constrained, global).

% NGOs and legal scholars who champion the universal application of human rights in armed conflict. This reading provides the legal framework for their advocacy, allowing them to challenge state practices and demand accountability for all individuals.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Members of non-state armed groups, often denied combatant status by states, benefit from this reading's extension of protections, particularly Common Article 3, which mandates humane treatment for all persons hors de combat. Without it, they would have no legal floor.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    powerless, immediate, trapped, local).

% Civilians caught in armed conflict benefit from the universal floor of protection, which reinforces their immunity from direct attack and ensures basic humane treatment, even in non-international conflicts where traditional IHL might be less robust.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% State military forces bear the costs of this expansive interpretation through increased restrictions on targeting, detention, interrogation, and overall operational flexibility. They must adapt their rules of engagement and training to comply with a broader set of obligations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_militaries, payer,
    institutional, biographical, constrained, global).

% Governments that prioritize military effectiveness and national security often resist this reading, viewing it as an undue constraint on their ability to conduct operations, particularly in counter-terrorism contexts. They bear the cost of reduced strategic options.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, states_seeking_operational_flexibility, payer,
    powerful, generational, constrained, national).

% Legal scholars and government advisors who adhere strictly to a state-centric view of IHL, arguing that human rights law has a limited or no role in armed conflict. They are often excluded from the mainstream discourse that promotes this universal rights reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, hardline_state_centric_scholars, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline for humane treatment and protection for all persons affected by armed conflict, regardless of their status, thereby preventing a race to the bottom in conflict conduct and ensuring a minimum standard of human dignity.
% TRANSFER_FUNCTION: Transfers legal and moral obligations for humane treatment from a narrow class of privileged combatants to all persons, and transfers operational flexibility from states to protected individuals, limiting state actions in favor of individual rights.
% ABSENT_VOICES: Hardline state-centric legal scholars and governments who reject the expansive role of human rights law in armed conflict are structurally excluded from the interpretive consensus that drives this reading. They would argue for a narrower application of IHL.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, protections for non-state actors and civilians in non-international armed conflicts would significantly degrade. States would face fewer legal constraints, potentially leading to increased abuses and a more brutal conduct of hostilities, reorganizing the legal and ethical landscape of conflict.
% FOUNDING_PROBLEM: The historical failure of traditional IHL to adequately protect civilians and non-state combatants, particularly in non-international armed conflicts, and the need to prevent atrocities and ensure human dignity in all forms of armed violence.
% FOUNDING_PROBLEM_CORROBORATION: International human rights reports, UN resolutions, extensive academic legal scholarship, and testimony from victims of conflict consistently corroborate the ongoing need for robust, universal protections in armed conflict, supporting the continued relevance of this reading's founding problem.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.70) is high because this reading imposes significant legal and operational costs on states, limiting their freedom of action in conflict. Suppression (0.65) is also high, reflecting the active legal and political pressure from international bodies and human rights advocates to enforce this interpretation against state resistance. Theater ratio (0.20) is relatively low, as the intent behind this reading is genuinely protective, though some states may engage in performative compliance. Resistance (0.80) is very high, as states actively contest this expansive interpretation, particularly in asymmetric warfare contexts. Accessibility collapse (0.40) is moderate, as states still find legal and political avenues to challenge or circumvent full adherence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates and protected populations, this reading is a vital coordination mechanism for universal protection. From the perspective of state militaries, it is a highly extractive constraint that unduly limits their operational effectiveness. The engine's computation will highlight this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   International legal bodies and human rights advocates act as agenda-setters and beneficiaries, pushing for and benefiting from the expansion of protections. Non-state armed groups, civilian populations, and unprivileged belligerents are direct beneficiaries, gaining a legal floor of protection. State militaries and states seeking operational flexibility are the primary payers, bearing the costs of increased legal obligations and reduced tactical options. Hardline state-centric scholars are excluded, as their views are marginalized by this reading's proponents.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_universal_rights,
    'Is this constraint accurately identified as the ''universal_rights_reading'' of the ''geneva_conventions_protective_scope'' kernel?',
    'Analysis of legal scholarship and state practice to confirm the distinct interpretive framework and its core tenets.',
    'If misidentified, the analysis of inter-reading relations and axiom status would be incorrect, leading to flawed kernel-level classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_universal_rights, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    state_centric_delta,
    'How would the ''state_centric_reading'' structurally alter the victim set and extractiveness on state militaries?',
    'Comparative legal analysis of the ''state_centric_reading'' to quantify its narrower scope of protected persons and reduced obligations on states.',
    'The ''state_centric_reading'' would significantly narrow the victim set to privileged combatants and reduce extractiveness on state militaries, leading to a lower epsilon and potentially a different classification for that reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_delta, empirical, 'Structural impact of the state-centric sibling reading.').

omega_variable(
    hybrid_proportionality_delta,
    'How would the ''hybrid_proportionality_reading'' structurally alter the universal floor of protection?',
    'Comparative legal analysis of the ''hybrid_proportionality_reading'' to quantify how its scaling of protections by conflict type impacts the universal floor asserted by this reading.',
    'The ''hybrid_proportionality_reading'' would introduce a scaling of protections based on conflict type, potentially reducing the universal floor asserted by this reading and leading to a more nuanced, context-dependent application of IHL.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_proportionality_delta, empirical, 'Structural impact of the hybrid-proportionality sibling reading.').

omega_variable(
    disagreement_location,
    'Where is the core disagreement between this reading and its siblings located structurally?',
    'Detailed analysis of the interpretive fault lines in legal texts and state practice, identifying specific clauses or principles under contestation.',
    'Pinpointing the disagreement clarifies the precise structural elements that differentiate the readings, informing future policy interventions or legal reforms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Identifies the specific structural element readings differ on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.3).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'geneva_conventions_protective_scope' kernel, each representing a distinct interpretation of the scope of protections in armed conflict. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
