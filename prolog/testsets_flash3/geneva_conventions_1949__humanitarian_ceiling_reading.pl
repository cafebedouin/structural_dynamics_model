% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions (Humanitarian Ceiling Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian ceiling' reading of the 1949
 *   Geneva Conventions, which posits that the conventions establish absolute
 *   minimum standards of humane conduct in armed conflict, binding on all
 *   state parties regardless of adversary compliance or reciprocity. This
 *   reading emphasizes expansive protections for civilians and detainees, and
 *   places an asymmetric burden of restraint on state militaries. It is a
 *   contested reading, particularly in asymmetric conflicts where state
 *   actors face non-state adversaries who do not adhere to IHL.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions (Humanitarian Ceiling Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '0d58f483-63ce-49fd-90b8-10534a6528c2').
narrative_ontology:cs_kernel_codification('0d58f483-63ce-49fd-90b8-10534a6528c2', fixed_text).
narrative_ontology:cs_authority_grounding('0d58f483-63ce-49fd-90b8-10534a6528c2', lineage).
narrative_ontology:cs_interpretation_layer_present('0d58f483-63ce-49fd-90b8-10534a6528c2').
narrative_ontology:cs_reading_relation('0d58f483-63ce-49fd-90b8-10534a6528c2', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d58f483-63ce-49fd-90b8-10534a6528c2', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('0d58f483-63ce-49fd-90b8-10534a6528c2', foundational, human_dignity_is_absolute).
narrative_ontology:cs_axiom_status(human_dignity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0d58f483-63ce-49fd-90b8-10534a6528c2', human_dignity_is_absolute, deontological).
narrative_ontology:cs_axiom('0d58f483-63ce-49fd-90b8-10534a6528c2', foundational, military_necessity_is_subordinate_to_humanity).
narrative_ontology:cs_axiom_status(military_necessity_is_subordinate_to_humanity, holdable).
narrative_ontology:cs_axiom_grounding('0d58f483-63ce-49fd-90b8-10534a6528c2', military_necessity_is_subordinate_to_humanity, deontological).
narrative_ontology:cs_reference_frame('0d58f483-63ce-49fd-90b8-10534a6528c2', post_wwii_universal_humanitarian_consensus).
narrative_ontology:cs_drift_state('0d58f483-63ce-49fd-90b8-10534a6528c2', contemporary_asymmetric_conflict_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0d58f483-63ce-49fd-90b8-10534a6528c2', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_organizations).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, political_leaders).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, jus_in_bello_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, human_dignity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of compliance, often at perceived tactical disadvantage, by restricting targeting, treatment of prisoners, and choice of weapons, even when facing non-state actors who do not reciprocate. Their operational flexibility is constrained by these absolute minimums.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_forces, payer,
    institutional, biographical, constrained, global).

% Are constrained in their strategic choices and justifications for military action, as the conventions limit the permissible means and ends of warfare, regardless of security imperatives. They face international legal and political costs for non-compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, political_leaders, payer,
    institutional, generational, constrained, national).

% Receive extensive protections from direct attack, indiscriminate targeting, and collective punishment, regardless of the actions of combatants. Their survival and well-being are prioritized above military necessity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Are guaranteed humane treatment, access to medical care, and due process, even if they are irregular fighters not accorded POW status. Their basic human rights are protected regardless of their combatant status or the nature of the conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants, beneficiary,
    powerless, immediate, trapped, local).

% Are empowered to provide aid and monitor compliance, with protected access to victims and detainees. Their mandate is strengthened by the absolute nature of the conventions, allowing them to operate even in highly contested environments.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_organizations, beneficiary,
    organized, biographical, mobile, global).

% Interpret, apply, and enforce the conventions, holding states accountable for violations. They act as custodians of the humanitarian ceiling, resisting attempts to dilute its absolute character.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_legal_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane conduct in armed conflict, providing a common framework for states to limit suffering and protect non-combatants, even when direct reciprocity is absent or unreliable.
% TRANSFER_FUNCTION: Transfers the burden of restraint and asymmetric compliance onto state military forces and political leaders, in exchange for enhanced protection and dignity for civilian populations and detained combatants.
% ABSENT_VOICES: Those who advocate for 'total war' or 'security at any cost' are structurally excluded from the framing of the conventions, as their arguments for unlimited violence are directly contradicted by the humanitarian ceiling. They would argue for greater flexibility based on existential threats.
% DISAPPEARANCE_RATIONALE: If the humanitarian ceiling vanished, state violence would escalate dramatically, civilian protections would collapse, and the treatment of detainees would revert to pure expediency. The international legal and moral landscape would be fundamentally altered, leading to widespread suffering and a breakdown of any shared norms in conflict.
% FOUNDING_PROBLEM: The horrors of World War II, particularly the systematic targeting of civilians and inhumane treatment of prisoners, revealed the inadequacy of prior conventions and the need for absolute, non-reciprocal humanitarian protections.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian organizations, international legal scholars, and historical records consistently corroborate the founding problem and its ongoing relevance, citing persistent violations and the need for the conventions' continued enforcement. While states may contest specific applications, the underlying problem of limiting suffering in conflict remains live.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading demands significant operational and strategic costs from state military forces, even when it appears to offer no direct reciprocal benefit from non-state adversaries. Suppression (0.78) is also high, reflecting the active legal and political enforcement required to maintain these absolute standards against strong security rationales. The theater ratio (0.20) is relatively low, as the core function of protecting human dignity is genuinely pursued, though often with significant resistance. The claimed type is 'tangled_rope' because it genuinely coordinates a universal humanitarian baseline while simultaneously extracting significant costs from state actors through asymmetric obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state military forces and political leaders, this reading of the conventions can feel highly extractive, imposing burdens without clear reciprocal benefits, especially when facing non-state actors. For civilian populations and humanitarian organizations, it is a vital protective 'rope' that establishes a necessary floor for human dignity in conflict. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State military forces and political leaders are the primary targets (payers) of this constraint, bearing the costs of asymmetric compliance. Civilian populations, detained combatants, and humanitarian organizations are the beneficiaries, receiving protections and an empowered mandate. International legal bodies act as agenda-setters, interpreting and enforcing the absolute nature of the conventions.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively resists mandatrophy by asserting the enduring and absolute nature of humanitarian principles, even as the nature of conflict evolves. It prevents the reclassification of humanitarian protections as mere 'peacetime aspirations' or 'conditional agreements' by emphasizing their foundational role in limiting suffering, regardless of changing security contexts. The 'live' status of the founding problem, despite ongoing contestation, reinforces this resistance to atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_absolutism,
    'To what extent can the Geneva Conventions effectively function as absolute humanitarian minimums without any expectation of reciprocity from all parties to a conflict?',
    'Empirical study of compliance rates and humanitarian outcomes in conflicts involving non-state actors who explicitly reject reciprocity, compared to conflicts with reciprocal state actors.',
    'If non-reciprocity consistently leads to a breakdown of compliance and worse humanitarian outcomes, it would weaken the ''humanitarian ceiling'' reading and lend support to conditional reciprocity arguments. If compliance holds despite non-reciprocity, it strengthens this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_absolutism, empirical, 'The practical viability of absolute humanitarian minimums in non-reciprocal conflict environments.').

omega_variable(
    security_humanitarian_balance,
    'Is the ''asymmetric burden'' on state militaries, as required by this reading, genuinely sustainable in the long term without undermining state security or public support for IHL?',
    'Longitudinal analysis of military doctrine, public opinion, and national security outcomes in states that consistently adhere to the humanitarian ceiling in asymmetric conflicts.',
    'If adherence demonstrably leads to unsustainable security risks or erosion of public support, it would create pressure to re-evaluate the ''humanitarian ceiling'' reading in favor of security maximization. If it proves sustainable or even enhances long-term security/legitimacy, it reinforces this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_humanitarian_balance, empirical, 'The long-term sustainability of the asymmetric burden imposed by the humanitarian ceiling reading on state security.').

omega_variable(
    interpretation_drift_pressure,
    'How much has the ''humanitarian ceiling'' reading itself drifted in response to new forms of warfare (e.g., cyber warfare, autonomous weapons) or the rise of non-state actors?',
    'Comparative textual analysis of international legal judgments, military manuals, and academic interpretations over time, specifically tracking how new challenges are integrated or resisted by this reading.',
    'Significant unacknowledged drift would suggest a ''practice_drift'' or ''axiom_overriding'' in the reading itself, potentially weakening its claim to absolute status. Active resistance to dilution would reinforce its stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_drift_pressure, conceptual, 'Tracking the internal evolution of the humanitarian ceiling reading in response to external pressures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1969, 0.15).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(gene_tr_t2012, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.6).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1969, 0.62).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1989, 0.65).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(gene_be_t2012, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.7).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1969, 0.72).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1989, 0.75).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2001, 0.8).
narrative_ontology:measurement(gene_su_t2012, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2012, 0.79).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, rules_of_engagement_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
