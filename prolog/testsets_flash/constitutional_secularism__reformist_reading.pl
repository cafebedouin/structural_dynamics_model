% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'reformist reading' of constitutional
 *   secularism, which posits an affirmative duty for the state to intervene
 *   in and eliminate religious practices deemed oppressive to marginalized
 *   groups. This reading prioritizes social justice and equality over claims
 *   of religious autonomy, leading to significant state intervention in
 *   personal laws and community customs. It is one reading of the broader
 *   'constitutional_secularism' kernel, which also includes
 *   'strict_neutrality_reading' and 'principled_intervention_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.85).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.75).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Constitutional Secularism: Affirmative Duty to Eliminate Oppressive Religious Practices").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '1db9d3f2-151c-4694-8e75-8a1c8b875fd5').
narrative_ontology:cs_kernel_codification('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', formalized).
narrative_ontology:cs_authority_grounding('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', lineage).
narrative_ontology:cs_interpretation_layer_present('1db9d3f2-151c-4694-8e75-8a1c8b875fd5').
narrative_ontology:cs_reading_relation('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', foundational, equality_supersedes_religious_autonomy).
narrative_ontology:cs_axiom_status(equality_supersedes_religious_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', equality_supersedes_religious_autonomy, deontological).
narrative_ontology:cs_axiom('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', foundational, state_as_social_reformer).
narrative_ontology:cs_axiom_status(state_as_social_reformer, holdable).
narrative_ontology:cs_axiom_grounding('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', state_as_social_reformer, instrumental).
narrative_ontology:cs_reference_frame('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', post_independence_constitutional_vision).
narrative_ontology:cs_drift_state('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', contemporary_global_human_rights_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1db9d3f2-151c-4694-8e75-8a1c8b875fd5', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_within_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, secular_reform_advocates).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservative_leaders).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, traditionalist_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional duty, enacting laws and issuing judgments that override religious personal laws and practices deemed oppressive. Faces political resistance but is structurally empowered to act.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_legislature_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from state interventions that dismantle discriminatory religious practices (e.g., caste-based exclusion in temples, denial of property rights). Their identity is often fused with their community, making exit from oppressive practices difficult without external intervention.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    powerless, generational, identity_locked, national).

% Benefit from reforms addressing gender-discriminatory religious practices (e.g., unequal inheritance, divorce laws, restrictions on public roles). Their social and familial ties often make direct defiance or exit prohibitive.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_within_religious_communities, beneficiary,
    powerless, biographical, identity_locked, national).

% Bear the direct costs of state intervention, losing authority over community practices and facing legal challenges to traditional norms. They mobilize resistance, arguing for religious freedom and non-interference.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservative_leaders, payer,
    organized, generational, constrained, national).

% Experience the erosion of long-standing religious and social structures. Their identity is deeply intertwined with these traditions, making compliance with state-mandated reforms a profound challenge to their self-conception.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, traditionalist_religious_communities, payer,
    moderate, generational, identity_locked, local).

% Advocate for the state's active role in social reform through secular law, aligning with this reading. They gain legitimacy and influence when the state acts on this duty.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, secular_reform_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue for a strict separation of state and religion, where the state does not interfere with religious practices, even if they are deemed discriminatory by some. Their arguments are often overridden by the reformist reading's emphasis on social justice.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse religious communities under a common framework of constitutional values, ensuring that religious practices do not violate fundamental rights, particularly for vulnerable groups. It seeks to establish a baseline of social justice across religious lines.
% TRANSFER_FUNCTION: Transfers authority over certain religious and social practices from religious institutions and leaders to the secular state. It also transfers rights and protections to marginalized groups within these communities, often at the expense of traditional autonomy claims.
% ABSENT_VOICES: Strict religious autonomy advocates, who believe the state should maintain a 'hands-off' approach to religious affairs, are largely excluded from the framing of this duty. They would argue that state intervention, even for reform, constitutes an infringement on religious freedom.
% DISAPPEARANCE_RATIONALE: If the state's affirmative duty to eliminate oppressive religious practices vanished, marginalized groups would lose a critical avenue for redress against discrimination. Religious conservative leaders would likely reassert traditional, often discriminatory, practices, leading to a significant rollback of social reforms and a rearrangement of power dynamics within communities.
% FOUNDING_PROBLEM: The persistence of deeply entrenched discriminatory practices within religious communities (e.g., caste discrimination, gender inequality in personal laws) that violate fundamental rights, despite constitutional guarantees of equality.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, academic scholars of social justice, and advocacy groups for scheduled castes and women consistently corroborate that these problems remain live and require state intervention. While religious conservative leaders dispute the 'oppressive' label, the lived experience of marginalized groups, documented by independent bodies, confirms the problem's persistence.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading mandates direct state interference in deeply held religious practices, imposing secular norms. Suppression (0.75) is also high, as the state must actively enforce these reforms against significant resistance from traditionalist communities. The theater ratio (0.20) is low, indicating that the state's actions are genuinely aimed at reform, not merely symbolic. Resistance (0.90) is very high, reflecting the profound challenge to religious authority and identity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups and secular reform advocates, this constraint is a necessary mechanism for justice and equality, a 'rope' pulling them out of oppression. From the perspective of religious conservative leaders and traditionalist communities, it is a 'snare' that unjustly infringes on their fundamental religious freedoms and identity. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (legislature/judiciary) acts as the agenda-setter, directing the reform. Scheduled castes and women within religious communities are primary beneficiaries, as the constraint aims to dismantle practices that oppress them. Religious conservative leaders and traditionalist communities are the primary payers, bearing the costs of enforced change to their practices. Secular reform advocates also benefit by seeing their agenda advanced. Religious autonomy advocates are excluded, as their arguments for non-interference are overridden by this reading's priorities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_oppressive_practice,
    'What constitutes an ''oppressive'' religious practice, and who legitimately defines it?',
    'Judicial precedent, legislative definition, or evolving social consensus, particularly from the affected marginalized groups themselves.',
    'A narrow definition would reduce the scope of state intervention and lower extractiveness; a broad definition would expand it, increasing extractiveness and suppression. The legitimacy of the definitional authority is key.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_oppressive_practice, conceptual, 'Ambiguity in the criteria for state intervention in religious practices.').

omega_variable(
    state_capacity_for_reform,
    'Does the state possess the institutional capacity and political will to effectively implement and sustain these reforms against entrenched resistance, or does it lead to performative interventions?',
    'Empirical analysis of reform outcomes, enforcement rates, and long-term social change versus continued resistance and non-compliance.',
    'If state capacity is low, the constraint''s effective extractiveness might be lower than intended (due to non-compliance), and its theater_ratio could rise as interventions become more symbolic than substantive. If high, the extractiveness and suppression are fully realized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_for_reform, empirical, 'The actual effectiveness of state-mandated religious reforms.').

omega_variable(
    identity_fusion_vs_coercion,
    'To what extent is resistance to reform driven by genuine identity fusion with traditional practices versus a coercive imposition by religious authorities?',
    'Sociological studies on individual agency within communities, post-reform exit patterns, and the presence of internal dissent within traditionalist groups.',
    'If resistance is primarily identity-driven, the ''payer'' seats'' exit options are more ''identity_locked'', making effective extraction higher. If it''s coercive, the state''s intervention is more clearly liberating, but also requires higher suppression against the coercing authorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_coercion, empirical, 'Distinguishing genuine identity-based resistance from coerced compliance within religious communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_secularism__reformist_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1970, constitutional_secularism__reformist_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(cons_tr_t1990, constitutional_secularism__reformist_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cons_tr_t2010, constitutional_secularism__reformist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(cons_tr_t2024, constitutional_secularism__reformist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__reformist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__reformist_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(cons_be_t1990, constitutional_secularism__reformist_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__reformist_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__reformist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__reformist_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__reformist_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_secularism__reformist_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__reformist_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__reformist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'reformist_reading' of the 'constitutional_secularism' kernel. It is linked to 'strict_neutrality_reading' and 'principled_intervention_reading' as sibling readings of the same kernel, each with distinct structural properties and implications for religious autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
