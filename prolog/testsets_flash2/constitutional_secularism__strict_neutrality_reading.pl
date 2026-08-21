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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strict neutrality' reading of
 *   constitutional secularism, where the state maintains an equal distance
 *   from all religions, neither favoring nor interfering with any. It is one
 *   interpretation of the broader kernel of 'constitutional_secularism'. This
 *   reading prioritizes state non-involvement in religious affairs, aiming to
 *   protect religious freedom and prevent state-sponsored discrimination. Its
 *   structural delta is uniform constraint application across communities,
 *   limits on state capacity for religious reform, and preserved minority
 *   autonomy, though with potential vulnerability to majority norms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.3).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.2).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'e60cabb9-e362-417b-8129-2ed4952a5bc1').
narrative_ontology:cs_kernel_codification('e60cabb9-e362-417b-8129-2ed4952a5bc1', fixed_text).
narrative_ontology:cs_authority_grounding('e60cabb9-e362-417b-8129-2ed4952a5bc1', lineage).
narrative_ontology:cs_interpretation_layer_present('e60cabb9-e362-417b-8129-2ed4952a5bc1').
narrative_ontology:cs_reading_relation('e60cabb9-e362-417b-8129-2ed4952a5bc1', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('e60cabb9-e362-417b-8129-2ed4952a5bc1', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('e60cabb9-e362-417b-8129-2ed4952a5bc1', foundational, state_non_interference_in_religion).
narrative_ontology:cs_axiom_status(state_non_interference_in_religion, holdable).
narrative_ontology:cs_axiom_grounding('e60cabb9-e362-417b-8129-2ed4952a5bc1', state_non_interference_in_religion, deontological).
narrative_ontology:cs_axiom('e60cabb9-e362-417b-8129-2ed4952a5bc1', foundational, equal_treatment_of_all_faiths).
narrative_ontology:cs_axiom_status(equal_treatment_of_all_faiths, holdable).
narrative_ontology:cs_axiom_grounding('e60cabb9-e362-417b-8129-2ed4952a5bc1', equal_treatment_of_all_faiths, deontological).
narrative_ontology:cs_reference_frame('e60cabb9-e362-417b-8129-2ed4952a5bc1', founding_constitutional_text).
narrative_ontology:cs_drift_state('e60cabb9-e362-417b-8129-2ed4952a5bc1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e60cabb9-e362-417b-8129-2ed4952a5bc1', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_majorities).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, state_neutrality_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__strict_neutrality_reading, religious_freedom_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for upholding the principle of strict neutrality, ensuring no religion receives preferential treatment or suffers interference. This involves refraining from funding religious institutions, avoiding religious symbols in public spaces, and not legislating on religious doctrine. Their capacity for social reform through religious channels is limited.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from protection against state-sponsored discrimination or coercion by a majority religion. Their autonomy in religious practice is preserved, but they may feel vulnerable to majority social norms if the state cannot intervene to protect them from internal community practices.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    moderate, generational, constrained, national).

% Bear the cost of not having their religious norms or institutions preferentially supported by the state, which they might view as a historical right or a reflection of national identity. They must fund their own institutions and cannot rely on state endorsement for their practices.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_majorities, payer,
    powerful, generational, constrained, national).

% Benefit from a public sphere free from religious imposition and state endorsement of specific faiths. They experience the state as a neutral arbiter, aligning with their worldview of a secular public order.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Would argue for state intervention in religious communities to address issues like gender inequality or caste discrimination, which they see as human rights violations. Under strict neutrality, the state's capacity to intervene in such matters is severely limited, leaving these groups without a direct state mechanism for redress within religious communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, social_reform_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a diverse citizenry by ensuring the state does not favor any particular religion, thereby preventing religious conflict and fostering a sense of equal citizenship among adherents of different faiths and non-believers.
% TRANSFER_FUNCTION: Transfers the burden of religious establishment and maintenance from the state to individual religious communities, and transfers the right to define public morality from religious institutions to a secular, pluralistic public sphere.
% ABSENT_VOICES: Advocates for state intervention in religious affairs to protect vulnerable groups (e.g., women, LGBTQ+ individuals within religious communities) are structurally excluded from the policy conversation, as strict neutrality limits the state's ability to engage with internal religious practices.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished, the state would likely begin to favor a majority religion or specific religious institutions, leading to increased religious conflict, discrimination against minorities, and a reordering of public life around religious tenets. The balance of power and rights would fundamentally shift.
% FOUNDING_PROBLEM: To prevent religious persecution, state-sponsored discrimination, and inter-religious conflict by establishing a secular state that treats all religions equally and refrains from interference.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and human rights organizations attest that the problem of religious discrimination and potential state overreach remains live, particularly in diverse societies. Religious minorities and secular citizens also corroborate the ongoing need for state neutrality to protect their interests.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.3) is moderate, reflecting the 'cost' to majority religions of not receiving state endorsement, and the indirect costs to social reform advocates who cannot leverage state power for internal religious reform. Suppression (0.2) is low, as the constraint primarily operates through non-interference rather than active coercion, though it does suppress state actions that would favor religion. Theater ratio (0.1) is low, indicating the state genuinely attempts to uphold neutrality, with minimal performative actions. Accessibility collapse (0.7) is relatively high, as the state's non-intervention limits alternatives for religious communities seeking state support or for reformers seeking state-backed changes within religious structures. Resistance (0.15) is low, as the principle is widely accepted, though specific applications may face resistance from religious majorities or social reformers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious minorities and secular citizens, this is a protective 'rope' ensuring equality. From the perspective of religious majorities, it can feel like a 'snare' that denies them their rightful place in the public sphere. Social reform advocates might see it as a 'piton' that maintains harmful religious practices by preventing state intervention. The engine's classification will reflect these divergences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions are the agenda-setters, tasked with upholding neutrality. Religious minorities and secular citizens are beneficiaries, gaining protection from discrimination and a secular public sphere. Religious majorities are payers, as they forgo state endorsement and funding. Social reform advocates are excluded, as their desired state interventions are curtailed by this reading of secularism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent religious conflict and ensure equality remains live. The 'strict neutrality' reading prevents mislabeling genuine coordination (peaceful coexistence) as extraction, but also risks obscuring the 'extraction' of state power from social reformers who seek to address internal religious inequalities. The omegas address this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_intervention_legitimacy,
    'Does strict neutrality inadvertently legitimize harmful practices within religious communities by preventing state intervention, or is non-intervention essential for religious autonomy?',
    'Comparative legal analysis of jurisdictions with different secularism models, assessing outcomes for vulnerable groups within religious communities.',
    'If non-intervention is found to enable harm, the ''strict neutrality'' reading''s extractiveness for vulnerable groups would be higher, potentially reclassifying it as a ''tangled_rope'' or ''snare'' from their seat. If essential for autonomy, its ''rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_legitimacy, conceptual, 'The tension between state neutrality and protection of vulnerable groups within religious communities.').

omega_variable(
    majority_norm_vulnerability,
    'To what extent does the absence of state endorsement for any religion lead to the de facto dominance of majority social norms, making religious minorities vulnerable despite formal neutrality?',
    'Sociological studies on the lived experience of religious minorities in strictly neutral states, examining social pressure and informal discrimination.',
    'If significant vulnerability is found, the ''strict neutrality'' reading''s ''suppression'' and ''extractiveness'' for religious minorities would be higher than currently assessed, indicating a failure to fully protect their interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_norm_vulnerability, empirical, 'Whether strict neutrality adequately protects religious minorities from majority social norms.').

omega_variable(
    reading_conflict_resolution,
    'Given the conflicting interpretations of constitutional secularism (strict neutrality vs. interventionist readings), which reading is most structurally coherent with the foundational constitutional principles of the state?',
    'Deep historical and jurisprudential analysis of the constitutional text, debates, and evolving legal precedent, aiming to identify the most consistent interpretation.',
    'Resolution would clarify the ''correct'' structural form of secularism, potentially foreclosing other readings as inconsistent with the constitutional kernel. This would shift the ''claimed_type'' of the state''s actual practice if it deviates from the coherent reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_conflict_resolution, conceptual, 'The fundamental conceptual conflict between different readings of constitutional secularism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1947, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(cons_tr_t1960, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(cons_tr_t1980, constitutional_secularism__strict_neutrality_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(cons_tr_t2000, constitutional_secularism__strict_neutrality_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(cons_tr_t2024, constitutional_secularism__strict_neutrality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1947, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(cons_be_t1960, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(cons_be_t1980, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(cons_be_t2000, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1947, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1947, 0.15).
narrative_ontology:measurement(cons_su_t1960, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1960, 0.17).
narrative_ontology:measurement(cons_su_t1980, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(cons_su_t2000, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_secularism' kernel. This 'strict neutrality' reading emphasizes state non-interference. It coexists with 'principled_intervention_reading' and 'reformist_reading', which advocate for different levels of state engagement in religious affairs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
