% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism (Strict Neutrality Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strict neutrality' reading of
 *   constitutional secularism, where the state maintains equal distance from
 *   all religions, offering no preferential treatment or interference. It
 *   aims to ensure religious freedom and equality by preventing state
 *   entanglement with religious affairs. This reading prioritizes religious
 *   autonomy and non-discrimination, but can limit the state's capacity to
 *   intervene in religious communities for social reform.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.15).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.05).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism (Strict Neutrality Reading)").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '2e147a46-3fe7-471a-aaca-a38d8f2877cb').
narrative_ontology:cs_kernel_codification('2e147a46-3fe7-471a-aaca-a38d8f2877cb', formalized).
narrative_ontology:cs_authority_grounding('2e147a46-3fe7-471a-aaca-a38d8f2877cb', lineage).
narrative_ontology:cs_interpretation_layer_present('2e147a46-3fe7-471a-aaca-a38d8f2877cb').
narrative_ontology:cs_reading_relation('2e147a46-3fe7-471a-aaca-a38d8f2877cb', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e147a46-3fe7-471a-aaca-a38d8f2877cb', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('2e147a46-3fe7-471a-aaca-a38d8f2877cb', foundational, state_non_interference_in_religion).
narrative_ontology:cs_axiom_status(state_non_interference_in_religion, holdable).
narrative_ontology:cs_axiom_grounding('2e147a46-3fe7-471a-aaca-a38d8f2877cb', state_non_interference_in_religion, deontological).
narrative_ontology:cs_axiom('2e147a46-3fe7-471a-aaca-a38d8f2877cb', foundational, equal_treatment_of_all_religions).
narrative_ontology:cs_axiom_status(equal_treatment_of_all_religions, holdable).
narrative_ontology:cs_axiom_grounding('2e147a46-3fe7-471a-aaca-a38d8f2877cb', equal_treatment_of_all_religions, deontological).
narrative_ontology:cs_reference_frame('2e147a46-3fe7-471a-aaca-a38d8f2877cb', founding_constitutional_principles).
narrative_ontology:cs_drift_state('2e147a46-3fe7-471a-aaca-a38d8f2877cb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2e147a46-3fe7-471a-aaca-a38d8f2877cb', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, majority_religious_groups).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, equality_before_the_law).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, religious_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for upholding the principle of strict neutrality, ensuring no religion receives preferential treatment or faces interference. This requires active monitoring and enforcement to prevent state entanglement with religious affairs.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from protection against state-sponsored discrimination or coercion, ensuring their practices are not suppressed by a majority religion. However, they may find the state unwilling to intervene to protect them from internal community pressures.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    moderate, generational, constrained, national).

% Bear the cost of not receiving preferential treatment or state endorsement, which they might otherwise expect given their numerical strength. They must operate within the same legal framework as all other religious groups.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_groups, payer,
    powerful, generational, constrained, national).

% Benefit from a state that does not impose religious norms or fund religious institutions, aligning with their worldview. They are protected from religious interference in public life.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Would argue for state intervention in religious communities to address issues like gender inequality or caste discrimination. Under strict neutrality, their calls for intervention are often rejected as violating religious autonomy, leaving them without a state mechanism for reform.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, social_reform_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's relationship with diverse religious and non-religious populations by establishing a common framework of non-interference and equal treatment, preventing religious conflict and ensuring public order.
% TRANSFER_FUNCTION: Transfers the burden of self-governance and self-funding to all religious groups, while transferring the benefit of non-discrimination and non-coercion to all citizens, regardless of religious affiliation.
% ABSENT_VOICES: Advocates for social reform within religious communities, particularly those seeking state intervention to protect marginalized groups (e.g., women, lower castes), are often excluded. Their voices are sidelined by the emphasis on religious autonomy and non-interference.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished, the state would likely begin to favor a majority religion or intervene selectively, leading to widespread social unrest, discrimination against minorities, and a fundamental shift in the relationship between citizens and the state.
% FOUNDING_PROBLEM: To prevent religious conflict, ensure civil peace, and protect individual liberty by establishing a state that does not favor or disfavor any religion, following historical experiences of religious wars and persecution.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists widely corroborate the founding problem, citing the historical context of religious strife and the philosophical arguments for state neutrality. Religious minorities and secular citizens also attest to its ongoing relevance for their protection.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint primarily coordinates non-interference rather than extracting resources. Any 'extraction' is the cost of not receiving state patronage for majority groups. Suppression is also low (0.05) as it primarily involves the state refraining from action, rather than actively coercing religious practice. Theater ratio is low (0.1) as the state's commitment to neutrality is generally genuine, though some performative aspects may exist in balancing diverse demands. Accessibility collapse is high (0.8) because the principle of state neutrality is deeply embedded and alternatives (state endorsement of religion) are largely foreclosed. Resistance is low (0.1) as the principle is widely accepted, though specific applications may be contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious minorities and secular citizens, this constraint is a protective rope, ensuring equality and freedom. From the perspective of majority religious groups, it might be seen as a mild form of extraction, denying them the benefits of state endorsement. Social reform advocates experience it as a snare, as it prevents the state from addressing internal community injustices.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions are the agenda-setters, tasked with upholding neutrality. Religious minorities and secular citizens are beneficiaries, protected from state-sponsored religious imposition. Majority religious groups are payers, as they forgo potential state patronage. Social reform advocates are excluded, as their desire for state intervention in religious affairs conflicts with the strict non-interference principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing religious conflict and ensuring equality) remains live. The strict neutrality reading prevents mandatrophy by ensuring the state does not overstep its bounds into religious affairs, which could lead to new forms of extraction or suppression. However, it faces challenges from readings that argue for more active state roles in religious reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_neutrality_vs_social_reform,
    'Does strict neutrality inadvertently protect oppressive practices within religious communities by preventing state intervention for social reform?',
    'Comparative legal analysis of jurisdictions with different secularism models, examining outcomes for marginalized groups within religious communities.',
    'If strict neutrality is found to consistently shield oppressive practices, its classification might shift towards a Tangled Rope or Snare for specific vulnerable groups, as its coordination function would be undermined by its extractive side-effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_neutrality_vs_social_reform, empirical, 'The tension between state non-interference and the need for social reform within religious communities.').

omega_variable(
    majority_norm_vulnerability,
    'Does strict neutrality, in practice, leave religious minorities vulnerable to the informal norms and pressures of a majority religion, even without direct state interference?',
    'Sociological studies and ethnographic research on the lived experiences of religious minorities in strictly neutral states.',
    'If minorities face significant informal pressure, the effective suppression for them might be higher than the base metric suggests, pushing their seat classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_norm_vulnerability, empirical, 'Vulnerability of minorities to majority norms under strict neutrality.').

omega_variable(
    conceptual_framing_of_neutrality,
    'Is ''strict neutrality'' a genuinely neutral stance, or does it implicitly favor a secular worldview by treating all religious claims as equally private and non-public?',
    'Philosophical and political theory analysis of the underlying assumptions of strict neutrality and its impact on public discourse.',
    'If strict neutrality is found to implicitly favor secularism, its claimed ''rope'' status might be challenged as a conceptual cover for a more extractive or suppressive outcome for religious citizens, particularly those who seek public expression of their faith.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_framing_of_neutrality, conceptual, 'Whether strict neutrality is truly neutral or implicitly secular.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1947, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(cons_tr_t1967, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1967, 0.07).
narrative_ontology:measurement(cons_tr_t1987, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1987, 0.08).
narrative_ontology:measurement(cons_tr_t2007, constitutional_secularism__strict_neutrality_reading, theater_ratio, 2007, 0.09).
narrative_ontology:measurement(cons_tr_t2024, constitutional_secularism__strict_neutrality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1947, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(cons_be_t1967, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1967, 0.12).
narrative_ontology:measurement(cons_be_t1987, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1987, 0.13).
narrative_ontology:measurement(cons_be_t2007, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 2007, 0.14).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1947, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1947, 0.03).
narrative_ontology:measurement(cons_su_t1967, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1967, 0.04).
narrative_ontology:measurement(cons_su_t1987, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1987, 0.04).
narrative_ontology:measurement(cons_su_t2007, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 2007, 0.05).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_secularism' kernel. Its strict non-interference approach contrasts with sibling readings that advocate for state intervention in religious affairs for social reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
