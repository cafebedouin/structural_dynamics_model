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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions Protective Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the state-centric reading of the Geneva
 *   Conventions' protective scope, which limits full combatant protections
 *   (like POW status) to uniformed state actors under responsible command, as
 *   per Article 4. Unprivileged belligerents, typically members of non-state
 *   armed groups, fall outside this scope, making them legitimate targets
 *   without corresponding immunities. This reading is a key point of
 *   contention in modern asymmetric conflicts, where state militaries benefit
 *   from the narrowed victim set, while non-state actors bear significant
 *   costs. The claimed type is 'tangled_rope' because it provides a
 *   coordination function for states (clarity in targeting) but extracts
 *   heavily from non-state actors through the same structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions Protective Scope (State-Centric Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba').
narrative_ontology:cs_kernel_codification('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', fixed_text).
narrative_ontology:cs_authority_grounding('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', lineage).
narrative_ontology:cs_interpretation_layer_present('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba').
narrative_ontology:cs_reading_relation('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', foundational, state_sovereignty_in_conflict_regulation).
narrative_ontology:cs_axiom_status(state_sovereignty_in_conflict_regulation, holdable).
narrative_ontology:cs_axiom_grounding('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', state_sovereignty_in_conflict_regulation, conventional).
narrative_ontology:cs_axiom('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', foundational, distinction_between_combatants_and_civilians_is_paramount).
narrative_ontology:cs_axiom_status(distinction_between_combatants_and_civilians_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', distinction_between_combatants_and_civilians_is_paramount, deontological).
narrative_ontology:cs_reference_frame('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', traditional_state_centric_ihl).
narrative_ontology:cs_drift_state('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', contemporary_asymmetric_conflict_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6b73b502-ef57-4a1f-bcf6-009c4cb3d7ba', '').
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

% Benefit from a clear distinction between combatants and non-combatants, allowing them to target unprivileged belligerents without granting POW status. This reduces legal and operational constraints in asymmetric conflicts.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, constrained, global).

% Advocate for and enforce this reading, which preserves state sovereignty and the traditional jus in bello framework. They define who qualifies for protection and who does not, often to their strategic advantage in counter-insurgency operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, constrained, global).

% Bear the full cost of this interpretation, being denied combatant immunity and POW status, making them legitimate targets without corresponding protections. Their options are to cease hostilities or face severe consequences without legal recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Their members are largely classified as unprivileged belligerents, exposing them to targeting without POW protections. This interpretation delegitimizes their struggle within the IHL framework, increasing their operational risk and legal vulnerability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, constrained, regional).

% Argue for a broader application of protections based on universal human rights, challenging the state-centric view. They are excluded from the primary interpretive authority of states but exert moral and political pressure.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_human_rights_advocates, excluded,
    organized, generational, analytical, global).

% Interprets and applies IHL in cases of war crimes, but its jurisdiction and interpretive authority are often contested by states adhering to the state-centric reading. Its judgments can influence, but not unilaterally redefine, the scope of protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_court, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit narrow, framework for distinguishing between lawful combatants and those who do not enjoy combatant immunity, aiming to reduce ambiguity in targeting decisions for state militaries.
% TRANSFER_FUNCTION: Transfers legal immunity and protection from unprivileged belligerents and non-state armed groups to conventional state militaries, allowing states greater latitude in prosecuting armed conflicts.
% ABSENT_VOICES: Unprivileged belligerents and their advocates, who would argue for a more inclusive definition of combatant status or universal human rights protections in armed conflict, are largely absent from the state-led interpretive processes.
% DISAPPEARANCE_RATIONALE: If this state-centric reading vanished, state militaries would face immense legal uncertainty regarding targeting and detention, potentially leading to a broader application of POW status or a complete breakdown of IHL distinctions. The conduct of armed conflict would fundamentally reorganize.
% FOUNDING_PROBLEM: The original Geneva Conventions aimed to regulate warfare by establishing clear rules for the treatment of wounded, sick, and prisoners of war, primarily between signatory states and their uniformed forces.
% FOUNDING_PROBLEM_CORROBORATION: State governments and conventional militaries attest that the problem of regulating inter-state conflict and defining combatant status remains live. International human rights advocates, while disagreeing with the scope, acknowledge the historical problem of regulating warfare but argue the solution is outdated for modern conflicts.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because this reading significantly reduces protections for a large class of combatants, allowing states to operate with fewer legal constraints. Suppression (0.78) is also high, as states actively enforce this interpretation through legal frameworks, military doctrine, and diplomatic pressure, suppressing alternative readings. Theater ratio is moderate (0.20) because while the IHL framework is genuinely applied, the state-centric interpretation often serves to legitimize actions that would otherwise be highly contested under a broader human rights lens. The post-9/11 period saw a spike in extractiveness and suppression as states aggressively asserted this reading in the 'War on Terror'.
 *
 * PERSPECTIVAL GAP:
 *   State actors perceive this as a necessary and legitimate interpretation for maintaining order and security, a 'rope' that coordinates state behavior. Non-state actors and human rights advocates, however, experience it as a 'snare' that legitimizes their targeting and denies fundamental rights. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and state governments are clear beneficiaries (low directionality) as this reading grants them strategic flexibility and reduces their legal obligations. Unprivileged belligerents and non-state armed groups are the primary targets (high directionality), as they are denied protections and face severe consequences. International human rights advocates are excluded, while the ICC acts as an observer, attempting to apply IHL within these contested boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    applicability_to_asymmetric_conflict,
    'Is the state-centric reading of Geneva Conventions'' protective scope still fit for purpose in modern asymmetric conflicts involving non-state actors?',
    'Empirical analysis of conflict outcomes, civilian casualties, and recidivism rates in conflicts where this reading is strictly applied versus those with more expansive interpretations.',
    'If found unfit, it would strengthen arguments for reinterpreting or amending IHL to include broader protections, potentially reclassifying the constraint as more extractive or a snare. If found effective in limiting harm, it would reinforce the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(applicability_to_asymmetric_conflict, empirical, 'Assesses the practical efficacy and ethical implications of the state-centric reading in contemporary warfare.').

omega_variable(
    human_rights_ihl_convergence,
    'To what extent do international human rights law (IHRL) and international humanitarian law (IHL) converge or diverge in their application to unprivileged belligerents, and how does this affect the state-centric reading?',
    'Legal scholarship and jurisprudence from international courts clarifying the interplay and hierarchy of IHRL and IHL in armed conflict, particularly concerning detention and targeting of non-state actors.',
    'Greater convergence would challenge the state-centric reading''s narrow scope, potentially forcing states to grant more protections. Persistent divergence would reinforce the current interpretation''s legitimacy within its own framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_rights_ihl_convergence, conceptual, 'Examines the relationship between human rights and IHL in defining protective scope.').

omega_variable(
    natural_law_vs_conventional_law,
    'Is the distinction between combatants and unprivileged belligerents a necessary feature of regulating armed conflict (natural law), or a conventional construct serving state interests (conventional law)?',
    'Philosophical and legal debate on the foundations of jus in bello, examining whether any universal moral principles necessitate such a distinction, or if it''s purely a product of state-centric legal positivism.',
    'If a ''natural law'' distinction, the constraint''s extractiveness might be seen as an unavoidable cost of order. If purely conventional, its extractive nature becomes more salient, highlighting its role in power dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_conventional_law, conceptual, 'Explores the ontological status of combatant distinctions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, rules_of_engagement_doctrine).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, detention_policies_in_armed_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'geneva_conventions_protective_scope' kernel. Its state-centric interpretation directly influences and is influenced by other readings of the same kernel, as well as downstream operational constraints like rules of engagement and detention policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
