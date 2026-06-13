% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Principled Intervention in Religious Affairs for Social Reform
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the 'principled intervention' reading of
 *   constitutional secularism, where the state actively intervenes in
 *   religious affairs to advance social reform and protect vulnerable groups
 *   within communities. It is a contested interpretation that allows for
 *   differential treatment of religious groups based on reform objectives,
 *   expanding state authority into domains traditionally governed by
 *   religious law. The claimed type is Tangled Rope, reflecting both a
 *   genuine coordination function (reconciling religious practice with
 *   constitutional values) and significant asymmetric extraction (from groups
 *   whose practices are targeted for reform).
 *
 * KEY AGENTS:
 *   - state_legislature: Agenda setter (institutional/constrained) — enacts reform laws
 *   - social_reform_advocates: Beneficiary (organized/mobile) — lobby for intervention
 *   - religious_minorities_resisting_reform: Payer (powerless/identity_locked) — bear costs of intervention
 *   - traditionalist_factions: Payer (moderate/constrained) — resist reforms
 *   - dominant_religious_groups_aligned_with_reform: Beneficiary (powerful/arbitrage) — may align with state
 *   - constitutional_courts: Observer (institutional/analytical) — adjudicate challenges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.65).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.7).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Principled Intervention in Religious Affairs for Social Reform").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '14421c2e-1171-413b-9b88-aebf12f6ebbf').
narrative_ontology:cs_kernel_codification('14421c2e-1171-413b-9b88-aebf12f6ebbf', formalized).
narrative_ontology:cs_authority_grounding('14421c2e-1171-413b-9b88-aebf12f6ebbf', lineage).
narrative_ontology:cs_interpretation_layer_present('14421c2e-1171-413b-9b88-aebf12f6ebbf').
narrative_ontology:cs_reading_relation('14421c2e-1171-413b-9b88-aebf12f6ebbf', constitutional_secularism__strict_neutrality_reading, influences).
narrative_ontology:cs_reading_relation('14421c2e-1171-413b-9b88-aebf12f6ebbf', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('14421c2e-1171-413b-9b88-aebf12f6ebbf', foundational, state_has_duty_to_reform_social_ills).
narrative_ontology:cs_axiom_status(state_has_duty_to_reform_social_ills, holdable).
narrative_ontology:cs_axiom_grounding('14421c2e-1171-413b-9b88-aebf12f6ebbf', state_has_duty_to_reform_social_ills, deontological).
narrative_ontology:cs_axiom('14421c2e-1171-413b-9b88-aebf12f6ebbf', foundational, religious_autonomy_subordinate_to_social_justice).
narrative_ontology:cs_axiom_status(religious_autonomy_subordinate_to_social_justice, holdable).
narrative_ontology:cs_axiom_grounding('14421c2e-1171-413b-9b88-aebf12f6ebbf', religious_autonomy_subordinate_to_social_justice, deontological).
narrative_ontology:cs_reference_frame('14421c2e-1171-413b-9b88-aebf12f6ebbf', constitutional_commitment_to_social_reform).
narrative_ontology:cs_drift_state('14421c2e-1171-413b-9b88-aebf12f6ebbf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('14421c2e-1171-413b-9b88-aebf12f6ebbf', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, state_legislature).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, social_reform_advocates).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, dominant_religious_groups_aligned_with_reform).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_minorities_resisting_reform).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, traditionalist_factions).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, individuals_whose_religious_autonomy_is_curtailed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts laws that intervene in religious practices, justifying them as necessary for social reform and protection of vulnerable groups. Benefits from expanded authority and political capital from reform movements.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Actively lobby for state intervention to address perceived injustices within religious communities. Benefit from the state's willingness to use its power to advance their reform agenda.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, social_reform_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of state intervention, which may include changes to their religious practices, loss of autonomy, or legal penalties. Their identity is often deeply intertwined with their traditions, making exit from the community or the constraint difficult.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_minorities_resisting_reform, payer,
    powerless, generational, identity_locked, local).

% Experience state intervention as an infringement on religious freedom and communal self-governance. They resist reforms that challenge long-standing practices, facing legal and social pressure.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, traditionalist_factions, payer,
    moderate, generational, constrained, regional).

% May align with state-led reforms, especially if those reforms do not significantly impact their own practices or if they see an opportunity to gain influence. They benefit from the state's legitimization of certain reformist interpretations.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, dominant_religious_groups_aligned_with_reform, beneficiary,
    powerful, generational, arbitrage, national).

% Adjudicate challenges to state interventions, balancing religious freedom with social reform objectives. Their rulings shape the boundaries and legitimacy of state action in religious affairs.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate diverse religious practices with evolving social norms and constitutional principles of equality and justice, preventing internal community practices from undermining broader societal reforms.
% TRANSFER_FUNCTION: Transfers authority over certain religious practices from religious communities to the state, and potentially transfers social capital and legitimacy from traditional religious authorities to reform-minded factions or state-aligned groups.
% ABSENT_VOICES: Religious communities or individuals whose practices are deemed 'unreformed' or 'backward' by the state or dominant social groups. Their perspectives on religious autonomy and the definition of 'reform' are often marginalized or dismissed in the policy-making process.
% DISAPPEARANCE_RATIONALE: If the state's power to intervene for social reform in religious affairs vanished, many ongoing social reforms would stall or reverse within religious communities. The balance of power between traditionalists and reformers would shift dramatically, and the state would lose a key tool for advancing its vision of a secular society.
% FOUNDING_PROBLEM: The challenge of reconciling religious diversity and autonomy with the state's commitment to social justice, equality, and the protection of vulnerable individuals within religious communities, particularly when religious practices are seen to perpetuate inequality or oppression.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by social reformers, human rights organizations, and constitutional scholars who highlight ongoing issues of gender inequality, caste discrimination, or other forms of social injustice within religious communities. While religious traditionalists dispute the state's right to intervene, the existence of the underlying social problems is generally acknowledged by independent observers.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because state intervention often imposes significant costs on religious communities, forcing changes to deeply held practices. Suppression (0.70) is high due to the state's coercive power and the limited exit options for religious groups from their traditions. Theater ratio (0.20) is relatively low, as the state's reform agenda is genuinely pursued, though it may serve to legitimize expanded state power. Accessibility collapse (0.40) is moderate; while direct alternatives to state law are limited, communities may find ways to adapt or resist. Resistance (0.75) is high, reflecting strong opposition from affected religious groups.
 *
 * PERSPECTIVAL GAP:
 *   The state legislature and social reform advocates perceive this as a legitimate and necessary coordination mechanism for a just society. In contrast, religious minorities and traditionalist factions experience it as an extractive and suppressive imposition on their autonomy and identity. The engine's per-seat classification should reflect this divergence, with beneficiaries experiencing a Rope-like function and payers experiencing a Snare-like function.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legislature and social reform advocates are clear beneficiaries, as the constraint expands their power and advances their agenda (low directionality). Religious minorities and traditionalist factions are targets, bearing the costs of intervention and having limited exit options (high directionality, especially for identity-locked groups). Dominant religious groups aligned with reform may also benefit, experiencing lower directionality. Constitutional courts are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine social reform efforts as pure extraction by acknowledging the coordination function of reconciling diverse practices with constitutional values. However, it also guards against mislabeling extraction as coordination by highlighting the coercive aspects and the identifiable victims, especially when state intervention risks majoritarian capture or disproportionately impacts vulnerable religious minorities. The 'contested' status of the founding problem further signals potential drift towards extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_capture_risk,
    'Does the ''principled intervention'' reading disproportionately benefit dominant religious or social groups by aligning state reform efforts with their values, thereby marginalizing genuine minority religious practices?',
    'Empirical analysis of intervention outcomes: track whether reforms consistently align with the values of dominant groups and whether minority groups experience disproportionate negative impacts or loss of autonomy.',
    'If majoritarian capture is confirmed, the constraint''s effective extractiveness and suppression for minority groups would be higher, pushing its classification closer to a Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Risk of state intervention being co-opted by dominant groups.').

omega_variable(
    religious_autonomy_boundary,
    'At what point does state intervention for social reform cross the line from protecting vulnerable individuals to infringing on legitimate religious autonomy and self-governance?',
    'Conceptual clarification through jurisprudential debate and comparative constitutional analysis, establishing clear criteria for ''essential religious practice'' versus ''social evil'' that are not solely determined by state power.',
    'A clearer boundary would reduce the perceived extractiveness for religious communities by limiting arbitrary state action; an ambiguous boundary allows for greater state discretion and thus higher effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_autonomy_boundary, conceptual, 'Defining the limits of state intervention in religious affairs.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''principled_intervention_reading'' of the ''constitutional_secularism'' kernel. What structural elements would change if a ''strict_neutrality_reading'' or ''reformist_reading'' were adopted?',
    'Comparative legal analysis of jurisdictions adopting different readings, or counterfactual modeling of policy outcomes under alternative constitutional interpretations.',
    'A ''strict_neutrality_reading'' would reduce state extractiveness and suppression in religious affairs, potentially shifting this constraint towards a Rope or even a Mountain (for the principle of non-interference itself). A ''reformist_reading'' would likely increase state extractiveness and suppression, potentially pushing this constraint towards a Snare, as it mandates more aggressive state action.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative readings of constitutional secularism on state intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_secularism__principled_intervention_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1970, constitutional_secularism__principled_intervention_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cons_tr_t1990, constitutional_secularism__principled_intervention_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_secularism__principled_intervention_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(cons_tr_t2024, constitutional_secularism__principled_intervention_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(cons_be_t1990, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(cons_su_t1990, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_secularism' kernel. It focuses on the state's power to intervene for social reform, which differs from a 'strict_neutrality_reading' (state non-interference) and a 'reformist_reading' (affirmative state duty to eliminate oppressive practices). Each reading constitutes a distinct constraint with different extractiveness and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
