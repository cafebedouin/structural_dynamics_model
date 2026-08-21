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
 *   human_readable: Geneva Conventions' State-Centric Protective Scope
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the state-centric reading of the Geneva
 *   Conventions, which strictly limits combatant immunity and POW status to
 *   uniformed, state-affiliated forces meeting specific criteria (e.g.,
 *   Article 4 of GCIII). Under this interpretation, 'unprivileged
 *   belligerents' (e.g., members of non-state armed groups, terrorists) fall
 *   outside treaty scope for these protections, making them subject to
 *   domestic criminal law and lawful targeting without combatant immunity.
 *   This reading is often advanced by states facing asymmetric conflicts and
 *   is contested by human rights advocates and some legal scholars.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: Agenda-setter/Beneficiary (institutional/mobile)
 *   - state_governments: Beneficiary (institutional/mobile)
 *   - unprivileged_belligerents: Payer/Victim (powerless/trapped)
 *   - human_rights_advocates: Payer/Excluded (organized/constrained)
 *   - international_criminal_courts: Observer (institutional/analytical)
 *   - universal_rights_scholars: Observer (moderate/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.85).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.9).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, snare).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions' State-Centric Protective Scope").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '54f34050-bd1f-4641-915f-547e56b83b5b').
narrative_ontology:cs_kernel_codification('54f34050-bd1f-4641-915f-547e56b83b5b', fixed_text).
narrative_ontology:cs_authority_grounding('54f34050-bd1f-4641-915f-547e56b83b5b', lineage).
narrative_ontology:cs_interpretation_layer_present('54f34050-bd1f-4641-915f-547e56b83b5b').
narrative_ontology:cs_reading_relation('54f34050-bd1f-4641-915f-547e56b83b5b', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('54f34050-bd1f-4641-915f-547e56b83b5b', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('54f34050-bd1f-4641-915f-547e56b83b5b', foundational, state_sovereignty_in_conflict).
narrative_ontology:cs_axiom_status(state_sovereignty_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('54f34050-bd1f-4641-915f-547e56b83b5b', state_sovereignty_in_conflict, conventional).
narrative_ontology:cs_axiom('54f34050-bd1f-4641-915f-547e56b83b5b', foundational, reciprocity_of_privilege).
narrative_ontology:cs_axiom_status(reciprocity_of_privilege, holdable).
narrative_ontology:cs_axiom_grounding('54f34050-bd1f-4641-915f-547e56b83b5b', reciprocity_of_privilege, conventional).
narrative_ontology:cs_reference_frame('54f34050-bd1f-4641-915f-547e56b83b5b', post_wwii_state_centric_framework).
narrative_ontology:cs_drift_state('54f34050-bd1f-4641-915f-547e56b83b5b', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('54f34050-bd1f-4641-915f-547e56b83b5b', '').
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

% Benefits from a clear legal framework that allows targeting of non-state actors without granting them prisoner of war (POW) status or combatant immunity. This reduces legal and operational constraints in asymmetric conflicts.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter,
    institutional, generational, mobile, global).

% Supports and enforces this interpretation, as it provides flexibility in responding to non-state threats and avoids the political and logistical burdens of treating all captured combatants as POWs.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_governments, beneficiary,
    institutional, generational, mobile, global).

% Are denied POW status and combatant immunity, making them subject to criminal prosecution under domestic law and lawful targeting without the protections afforded to regular armed forces. Their legal vulnerability is extreme.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Bear the cost of reduced protections for individuals in armed conflict, as this interpretation limits the scope of humanitarian law. They actively resist this reading through legal challenges, advocacy, and public discourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates, payer,
    organized, generational, constrained, global).

% Adjudicate violations of international humanitarian law, often operating within the established legal interpretations, which tend to reflect this state-centric view. Their rulings reinforce the constraint.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% Critically analyze the state-centric interpretation, arguing for a broader application of human rights and humanitarian law to all persons affected by armed conflict, regardless of their formal combatant status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, universal_rights_scholars, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit narrow, framework for state actors to distinguish between legitimate combatants (who receive POW status) and those who do not, aiming to reduce ambiguity in military operations and legal accountability for state forces.
% TRANSFER_FUNCTION: Transfers legal protections and immunities from unprivileged belligerents to conventional state militaries, allowing states greater latitude in targeting and detention practices in asymmetric conflicts.
% ABSENT_VOICES: Non-state armed groups and individuals designated as 'unprivileged belligerents' are structurally excluded from the interpretive process. They would argue for universal application of basic protections and recognition of their agency in conflict.
% DISAPPEARANCE_RATIONALE: If this strict state-centric interpretation vanished overnight, state militaries would face profound legal uncertainty regarding targeting and detention, potentially leading to a significant expansion of protections for non-state actors and a complete re-evaluation of engagement rules in asymmetric conflicts. The legal and operational landscape of armed conflict would be fundamentally altered.
% FOUNDING_PROBLEM: To regulate warfare between states by establishing clear categories of combatants and non-combatants, and ensuring humane treatment for those captured, primarily focusing on inter-state conflicts and the principle of reciprocity.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and some legal scholars argue the problem of regulating conflict and ensuring state security is still live, especially with the rise of non-state actors. Human rights groups and other scholars attest that the original problem (inter-state war) has evolved, and this interpretation now creates new problems by denying protections to vulnerable groups; independent legal analyses and reports from international bodies support the contested status.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because this reading denies fundamental protections to a significant class of individuals in armed conflict, shifting the burden of vulnerability. Suppression is very high (0.90) as states actively enforce this interpretation through military doctrine, legal frameworks, and actual targeting practices, suppressing alternative views. Theater ratio is moderate (0.40): while there's genuine legal justification and historical precedent, some arguments for strict interpretation become performative when used to legitimize controversial targeting practices. Accessibility collapse is high (0.92) for unprivileged belligerents, as alternatives to being treated as criminals or unlawful combatants are almost completely foreclosed. Resistance is high (0.75) from human rights groups and scholars who challenge this narrow scope.
 *
 * PERSPECTIVAL GAP:
 *   The state-centric reading is experienced as a necessary legal clarity by state militaries and governments, enabling effective operations. For unprivileged belligerents and human rights advocates, the same structure is experienced as a severe denial of fundamental protections, leading to extreme vulnerability and a perceived erosion of humanitarian principles.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and state governments are clear beneficiaries, gaining legal flexibility and reduced obligations (low d). Unprivileged belligerents are the primary targets, losing protections and facing severe legal consequences (high d). Human rights advocates also bear costs by seeing their advocacy for universal protections undermined (high d). International courts and scholars act as observers, with varying degrees of influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare prevents mislabeling this constraint as a Rope (pure coordination) or even a Tangled Rope (coordination with asymmetric extraction). While it offers 'coordination' for state militaries, this function is largely a cover for the severe extraction of protections from unprivileged belligerents, maintained through active suppression of alternative interpretations and practices. The high extractiveness and suppression, coupled with identifiable victims, clearly point to a Snare, not a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''state_centric_reading'' of the ''geneva_conventions_protective_scope'' kernel?',
    'Comparative textual analysis of legal scholarship and state practice against the defined characteristics of the state-centric reading and its siblings.',
    'If misidentified, the entire analysis of inter-reading relations and axiomatic grounding would be flawed, leading to incorrect classification of the kernel''s overall dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed within the kernel context.').

omega_variable(
    victim_set_legitimacy_ambiguity,
    'Is the exclusion of unprivileged belligerents from POW status a necessary consequence of the Geneva Conventions'' original structure and intent, or an interpretive choice that could be revised without undermining the Conventions'' core principles?',
    'Historical legal analysis of the drafting of the Conventions, combined with contemporary international legal consensus-building and potential state practice shifts.',
    'If it''s a revisable interpretive choice, the constraint''s extractiveness is more clearly a policy outcome rather than an inherent structural feature, strengthening arguments for reform. If necessary, it highlights a fundamental tension within IHL.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of denying protections to unprivileged belligerents.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., legal frameworks, military doctrine) or internalized (e.g., lack of awareness of rights, fear of reprisal) for unprivileged belligerents?',
    'Post-conflict interviews with former unprivileged belligerents and analysis of their legal awareness and perceived options. If suppression persists after the immediate threat, it suggests internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This implies different intervention strategies for human rights advocates.',
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
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1969, 0.18).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1989, 0.25).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement(gene_tr_t2012, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2012, 0.37).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.6).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1969, 0.68).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1989, 0.75).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.8).
narrative_ontology:measurement(gene_be_t2012, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2012, 0.83).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.7).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1969, 0.75).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1989, 0.8).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(gene_su_t2012, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
