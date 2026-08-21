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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions' State-Centric Protective Scope
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'state-centric reading' of the Geneva
 *   Conventions' protective scope, which strictly limits combatant immunity
 *   and POW status to uniformed combatants under responsible command meeting
 *   Article 4 criteria. Unprivileged belligerents (e.g., non-state armed
 *   groups, terrorists) fall outside this treaty scope. This reading is
 *   actively enforced by many states, particularly in asymmetric conflicts,
 *   and is contested by human rights advocates and some international legal
 *   scholars who argue for a broader application of IHL and human rights law.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: Agenda setter/Beneficiary (institutional/arbitrage)
 *   - state_governments: Beneficiary (institutional/arbitrage)
 *   - unprivileged_belligerents: Payer/Victim (powerless/trapped)
 *   - human_rights_advocates: Payer/Excluded (organized/constrained)
 *   - international_criminal_courts: Observer (institutional/analytical)
 *   - international_committee_of_the_red_cross: Observer (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.75).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.8).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions' State-Centric Protective Scope").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, 'b0e115c7-313d-4404-bbe7-78b548a4e747').
narrative_ontology:cs_kernel_codification('b0e115c7-313d-4404-bbe7-78b548a4e747', fixed_text).
narrative_ontology:cs_authority_grounding('b0e115c7-313d-4404-bbe7-78b548a4e747', lineage).
narrative_ontology:cs_interpretation_layer_present('b0e115c7-313d-4404-bbe7-78b548a4e747').
narrative_ontology:cs_reading_relation('b0e115c7-313d-4404-bbe7-78b548a4e747', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('b0e115c7-313d-4404-bbe7-78b548a4e747', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('b0e115c7-313d-4404-bbe7-78b548a4e747', foundational, state_sovereignty_primary_source_of_law).
narrative_ontology:cs_axiom_status(state_sovereignty_primary_source_of_law, holdable).
narrative_ontology:cs_axiom_grounding('b0e115c7-313d-4404-bbe7-78b548a4e747', state_sovereignty_primary_source_of_law, conventional).
narrative_ontology:cs_axiom('b0e115c7-313d-4404-bbe7-78b548a4e747', foundational, combatant_status_derived_from_state_affiliation).
narrative_ontology:cs_axiom_status(combatant_status_derived_from_state_affiliation, holdable).
narrative_ontology:cs_axiom_grounding('b0e115c7-313d-4404-bbe7-78b548a4e747', combatant_status_derived_from_state_affiliation, conventional).
narrative_ontology:cs_reference_frame('b0e115c7-313d-4404-bbe7-78b548a4e747', post_wwii_state_centric_ihl).
narrative_ontology:cs_drift_state('b0e115c7-313d-4404-bbe7-78b548a4e747', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0e115c7-313d-4404-bbe7-78b548a4e747', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply IHL in a manner that prioritizes state security and military effectiveness, benefiting from the narrow definition of combatant status which reduces legal constraints in asymmetric conflicts. They actively enforce this interpretation through military doctrine and rules of engagement.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).

% Legally justify actions against non-state armed groups by denying them POW status, thereby avoiding obligations under GCIII and allowing for detention under domestic law or direct targeting without combatant immunity. They benefit from reduced accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, global).

% Denied combatant immunity and POW protections under this reading, they are subject to domestic criminal law, indefinite detention, or direct targeting without the safeguards afforded to lawful combatants. Their legal status is precarious, and alternatives for humane treatment are collapsed.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Bear the cost of violated principles and legal vacuums created by this narrow interpretation. They advocate for broader application of IHL and human rights law to all persons in armed conflict, but their arguments are often dismissed or marginalized by states adhering to the state-centric view.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates, excluded).

% Interpret and apply IHL in cases of war crimes and crimes against humanity, but their jurisdiction and ability to enforce broader interpretations are often limited by state cooperation and the prevailing legal consensus among powerful states.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% Monitors compliance with IHL and advocates for its humane application, but operates within the framework of state interpretations. They provide humanitarian assistance but cannot unilaterally alter the legal status of individuals.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_committee_of_the_red_cross, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit narrow, legal framework for states to categorize individuals in armed conflict, distinguishing between those who receive POW status (uniformed combatants under responsible command) and those who do not (unprivileged belligerents), thereby coordinating military operations and legal accountability within a state-centric paradigm.
% TRANSFER_FUNCTION: Transfers the burden of combatant immunity and POW protections away from states when dealing with non-state armed groups, effectively transferring legal vulnerability and the risk of harsh treatment to unprivileged belligerents.
% ABSENT_VOICES: Unprivileged belligerents themselves are structurally excluded from the legal and political processes that define their status. While human rights organizations and some international legal scholars voice objections, their influence is often limited against the consolidated power of states.
% DISAPPEARANCE_RATIONALE: If this state-centric reading vanished overnight, states would face immense pressure to extend POW-like protections or at least more robust human rights standards to all persons affected by armed conflict, regardless of their affiliation. This would fundamentally alter military doctrine, detention practices, and accountability frameworks, leading to a significant reorganization of international humanitarian law and state practice.
% FOUNDING_PROBLEM: To regulate warfare between states by establishing clear categories of combatants and non-combatants, ensuring humane treatment for those who follow the rules of war, and providing a basis for prosecuting war crimes, primarily in the context of inter-state conflicts.
% FOUNDING_PROBLEM_CORROBORATION: Conventional state militaries and governments argue the problem of distinguishing lawful combatants from terrorists and other non-state actors is still live and requires this strict interpretation to maintain order and security. Human rights organizations and some international legal scholars attest that the original problem has evolved to include non-state actors, and the current interpretation creates a legal vacuum that enables abuses, citing numerous cases of lack of accountability and inconsistent application of justice. This is corroborated by independent legal analyses and reports from international bodies.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.75) because this reading permits states to deny fundamental protections to a significant class of actors in armed conflict, effectively extracting their legal status and rights. Suppression is also high (0.8) as it relies on the coercive power of states to enforce this interpretation and deny alternatives for unprivileged belligerents. Theater ratio is low (0.2) because this is a live, actively applied legal interpretation, not a performative maintenance of an atrophied function. Accessibility collapse is high (0.85) for unprivileged belligerents, as their legal alternatives for humane treatment are severely limited. Resistance is substantial (0.7) from human rights groups and affected parties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of conventional state militaries and governments, this reading provides necessary clarity and legal justification for operations against non-state threats, upholding the traditional state-centric order of IHL. From the perspective of unprivileged belligerents and human rights advocates, it creates a legal black hole, enabling abuses and denying fundamental human dignity. The engine's classification will highlight this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and state governments are clear beneficiaries (low directionality) as this interpretation reduces their legal obligations and enhances their operational flexibility. Unprivileged belligerents are the primary targets (high directionality) as they bear the full cost of being excluded from protective frameworks. Human rights advocates are also targets, bearing the cost of the erosion of universal rights principles. International bodies like the ICC and ICRC act as observers, attempting to apply or influence the interpretation from an analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy; it represents a live and highly contested legal interpretation. The founding problem of regulating warfare remains, but its scope and application to modern conflicts involving non-state actors are fiercely debated. The persistence of this reading is due to active state enforcement and its perceived benefits for state security, not institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine, universally accepted interpretation of the Geneva Conventions, or one specific reading of a contested kernel?',
    'Analysis of state practice, international jurisprudence, and scholarly consensus across different legal traditions. The presence of robust sibling readings (universal_rights_reading, hybrid_proportionality_reading) indicates it is a reading, not a universal truth.',
    'If it were a universally accepted truth, its extractiveness would be inherent to the law itself. As a reading, its extractiveness is contingent on the interpretive framework, allowing for alternative, less extractive classifications under different readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''state_centric_reading'' of the ''geneva_conventions_protective_scope'' kernel.').

omega_variable(
    universal_rights_reading_delta,
    'What would be the structural impact if the ''universal_rights_reading'' of the Geneva Conventions'' protective scope were adopted?',
    'Comparative legal analysis of proposed frameworks that extend IHL or human rights law to all persons in armed conflict, regardless of combatant status. Examination of jurisdictions that have adopted more expansive protections.',
    'If the ''universal_rights_reading'' were adopted, the victim set would significantly expand to include all persons affected by armed conflict, and the extractiveness from unprivileged belligerents would decrease as they would gain a universal floor of protection. This reading ''forecloses'' the universal rights reading due to its direct contradiction on the scope of protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_rights_reading_delta, conceptual, 'Impact of adopting the universal_rights_reading.').

omega_variable(
    hybrid_proportionality_reading_delta,
    'What would be the structural impact if the ''hybrid_proportionality_reading'' of the Geneva Conventions'' protective scope were adopted?',
    'Analysis of legal frameworks that apply AP I standards for international armed conflict and AP II/Common Article 3 for non-international conflicts, with proportionality analysis determining application. Examination of state practice in such hybrid approaches.',
    'If the ''hybrid_proportionality_reading'' were adopted, the application of protections would become more nuanced, potentially extending some IHL standards to unprivileged belligerents based on conflict type and proportionality. This reading ''coexists_with'' the hybrid proportionality reading, as it represents a different, albeit often conflicting, approach to IHL application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_proportionality_reading_delta, conceptual, 'Impact of adopting the hybrid_proportionality_reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal/military enforcement) or internalized (lack of agency/knowledge among unprivileged belligerents)?',
    'Post-conflict legal aid and education programs for former unprivileged belligerents: if their understanding of rights and legal recourse increases, and they are able to challenge their treatment, it suggests a component of internalized suppression. If structural barriers remain insurmountable, it confirms structural suppression.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. If purely structural, removing the legal/military enforcement would immediately alter the constraint''s impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for unprivileged belligerents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1969, 0.16).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2004, 0.19).
narrative_ontology:measurement(gene_tr_t2014, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.55).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1969, 0.6).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1989, 0.68).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2004, 0.72).
narrative_ontology:measurement(gene_be_t2014, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1969, 0.65).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1989, 0.72).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2004, 0.78).
narrative_ontology:measurement(gene_su_t2014, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2014, 0.79).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, rules_of_engagement_for_asymmetric_warfare).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
