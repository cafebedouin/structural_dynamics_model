% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional/political/religious
 *
 * SUMMARY:
 *   This constraint instantiates the strict neutrality reading of
 *   constitutional secularism: the state must maintain equal distance from
 *   all religions, offering no preferential treatment and exercising no
 *   interference. It is one reading of a contested kernel that also supports
 *   principled intervention and reformist readings. Under strict neutrality,
 *   courts enforce a hard boundary between state and religion that preserves
 *   minority autonomy and prevents state establishment of a majority faith.
 *   Simultaneously, it constrains the state's capacity to intervene in
 *   oppressive internal religious practices and leaves minority communities
 *   vulnerable to majoritarian social norms that the state is powerless to
 *   counteract.
 *
 * KEY AGENTS:
 *   - Religious minorities: beneficiaries of non-interference but vulnerable to majority norms (organized/constrained)
 *   - Religious institutions: beneficiaries retaining authority without state reform (powerful/mobile)
 *   - Intra-religious marginalized: payers denied state protection from oppressive practices (powerless/trapped)
 *   - Reformist advocates: payers blocked from state-backed reform (moderate/constrained)
 *   - State apparatus: agenda-setter bound by and enforcing neutrality (institutional/constrained)
 *   - Constitutional scholars: observers analyzing the doctrine (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.58).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.62).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional/political/religious").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'e4cb3314-bb37-461a-aa3f-b3c999587822').
narrative_ontology:cs_kernel_codification('e4cb3314-bb37-461a-aa3f-b3c999587822', formalized).
narrative_ontology:cs_authority_grounding('e4cb3314-bb37-461a-aa3f-b3c999587822', lineage).
narrative_ontology:cs_interpretation_layer_present('e4cb3314-bb37-461a-aa3f-b3c999587822').
narrative_ontology:cs_reading_relation('e4cb3314-bb37-461a-aa3f-b3c999587822', constitutional_secularism__principled_intervention_reading, forecloses).
narrative_ontology:cs_reading_relation('e4cb3314-bb37-461a-aa3f-b3c999587822', constitutional_secularism__reformist_reading, forecloses).
narrative_ontology:cs_axiom('e4cb3314-bb37-461a-aa3f-b3c999587822', foundational, strict_non_interference).
narrative_ontology:cs_axiom_status(strict_non_interference, holdable).
narrative_ontology:cs_axiom_grounding('e4cb3314-bb37-461a-aa3f-b3c999587822', strict_non_interference, conventional).
narrative_ontology:cs_axiom('e4cb3314-bb37-461a-aa3f-b3c999587822', foundational, equal_distance_imperative).
narrative_ontology:cs_axiom_status(equal_distance_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e4cb3314-bb37-461a-aa3f-b3c999587822', equal_distance_imperative, deontological).
narrative_ontology:cs_reference_frame('e4cb3314-bb37-461a-aa3f-b3c999587822', equal_distance_constitutional_framework).
narrative_ontology:cs_drift_state('e4cb3314-bb37-461a-aa3f-b3c999587822', contemporary_majoritarian_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4cb3314-bb37-461a-aa3f-b3c999587822', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_religious_marginalized).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, reformist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from state establishment of a majority religion and from direct state interference in their religious affairs. They gain constitutional autonomy to maintain religious practices, educational institutions, and personal laws. However, they remain exposed to majoritarian social norms and intra-community oppression without state recourse to reform those internal practices.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    organized, generational, constrained, national).

% Retain authority over religious doctrine, internal governance, and customary law without state reform intervention. Conservative majoritarian institutions particularly benefit from the absence of state oversight. They can resist internal reform demands by invoking constitutional protection from state interference.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_institutions, beneficiary,
    powerful, generational, mobile, national).

% Women, lower castes, and other vulnerable members within religious communities who seek state protection from discriminatory religious practices. Under strict neutrality, the state refuses to intervene in religious affairs, leaving them subject to traditional authorities. Exit from their religious community is often socially and economically impossible, and the state will not override community norms on their behalf.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_religious_marginalized, payer,
    powerless, biographical, trapped, local).

% Social reformers and civil society actors who advocate for state intervention to eliminate discriminatory religious practices. Their legislative and judicial agenda is blocked by the non-interference principle. They can operate through persuasion within communities but lack the state coercion needed to override conservative religious authority.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reformist_advocates, payer,
    moderate, biographical, constrained, national).

% Constitutionally and judicially constrained from intervening in religious affairs or showing preferential treatment to any faith. Courts actively enforce this by striking down laws that interfere with religion or favor one community. The state loses capacity to pursue social reform through religious regulation and must perform equal distance even when majoritarian pressures demand otherwise.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Analyze and debate the coherence and consequences of strict neutrality compared to principled intervention and reformist alternatives. They document the tension between religious autonomy and social reform imperatives without being directly governed by the constraint.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents state capture by any single religion and reduces inter-religious conflict by mandating that the government maintain equal distance from all faiths, creating a neutral framework for plural coexistence.
% TRANSFER_FUNCTION: Transfers autonomy and non-interference protection to religious institutions and minorities; transfers the power and obligation to intervene in oppressive religious practices away from the state and reformist actors; leaves intra-religious marginalized groups without external protection.
% ABSENT_VOICES: Marginalized members within religious communities who need state intervention against oppressive practices but are silenced by the autonomy framework; majoritarian nationalists who would prefer explicit state alignment with the majority religion and resent enforced equal distance.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished, state institutions would regain capacity to intervene in religious affairs for reform purposes, religious majorities might capture state power to favor their traditions, and minority protections would shift from autonomy-based to potentially interventionist or majoritarian frameworks. The constitutional order around religion-state relations would reorganize entirely.
% FOUNDING_PROBLEM: Religious conflict, persecution of minorities by state-established churches, and the need to prevent state capture by a dominant religion in deeply plural societies.
% FOUNDING_PROBLEM_CORROBORATION: Minority communities and liberal constitutionalists attest the problem of majoritarian state capture remains live. Reformist advocates and marginalized groups within religions attest that the founding problem has evolved: non-interference now protects oppressive internal practices and leaves minorities vulnerable to majoritarian social dominance. Comparative constitutional scholarship from outside the immediate beneficiary communities corroborates the unresolved tension.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial cost borne by marginalized community members who lose state intervention against oppressive practices. Suppression (0.62) captures the active judicial and legislative suppression of reformist demands and majoritarian preferences alike. Theater_ratio (0.42) registers that equal-distance claims increasingly perform neutrality while the state manages religious institutions selectively. Accessibility_collapse (0.70) indicates that alternative frameworks like principled intervention are doctrinally available but collapsed under this reading. Resistance (0.55) reflects ongoing reformist and majoritarian challenge to the strict boundary. The temporal series show accumulation: extraction and theater rise as majoritarian pressures make strict neutrality harder to maintain authentically.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state institutions) experiences the constraint as a self-imposed limit that prevents both capture and reform; it computes as coordination-heavy. The beneficiary seats (minorities and religious institutions) experience protection from state interference, though minorities also bear vulnerability to social majoritarianism. The payer seats (marginalized members and reformists) experience the same structure as an enforced denial of protection and state capacity. The engine should compute divergent seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minorities and institutions are declared beneficiaries: they receive non-interference protection, giving them low directionality toward beneficiary status. Intra-religious marginalized groups and reformist advocates are declared victims: they bear the cost of foregone state intervention and reform capacity, giving them high directionality toward target status. The state apparatus sits near symmetric: it both administers and is bound by the constraint. The beneficiary/victim declarations drive the engine's directionality derivation without override.
 *
 * MANDATROPHY ANALYSIS:
 *   Strict neutrality prevents mislabeling by requiring both genuine coordination (prevention of state religious capture, pluralist coexistence) and identifiable asymmetric extraction (denial of reform intervention to marginalized groups). If the coordination story were accepted without the victim structure, the constraint would misclassify as rope. If the extraction story were accepted without the coordination function, it would misclassify as snare. The tangled rope classification is warranted because the same structure that coordinates religious coexistence simultaneously extracts from vulnerable community members by foreclosing state reform capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majority_norms_vulnerability,
    'Does strict neutrality protect minorities from state oppression or leave them exposed to majoritarian social norms without state recourse?',
    'Comparative analysis of minority outcomes under strict neutrality versus principled intervention regimes; longitudinal study of minority rights indicators and hate-crime data in majoritarian contexts.',
    'If exposure to majority norms is severe, the cost side of the tangled rope is heavier and the constraint tilts toward snare-like extraction from minority positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_norms_vulnerability, empirical, 'Ambiguity about whether neutrality protects or harms minorities in majoritarian contexts').

omega_variable(
    reform_intervention_compatibility,
    'Can state intervention to reform oppressive religious practices be reconciled with strict non-interference, or do they represent mutually exclusive constitutional visions?',
    'Jurisprudential analysis of constitutional frameworks that attempt to combine anti-establishment with reform intervention; examination of doctrinal coherence in hybrid models across jurisdictions.',
    'If mutually exclusive, strict neutrality forecloses reformist readings completely; if reconcilable, the reading''s boundary with principled intervention is porous and the classification softens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_intervention_compatibility, conceptual, 'Whether non-interference and reform intervention are logically compatible').

omega_variable(
    state_capacity_reform_cost,
    'Does the constraint''s limitation on state reform capacity constitute extraction from marginalized groups, or is it the necessary price of religious autonomy?',
    'Empirical assessment of reform outcomes in jurisdictions with strict neutrality versus managed intervention; cost-benefit analysis from the perspective of intra-religious marginalized groups.',
    'If the limitation extracts heavily from vulnerable groups, the coordination function is outweighed by asymmetric extraction; if moderate, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_reform_cost, preference, 'Normative ambiguity about whether autonomy or reform protection should prevail').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__strict_neutrality_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(cons_tr_t50, constitutional_secularism__strict_neutrality_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(cons_tr_t70, constitutional_secularism__strict_neutrality_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(cons_be_t50, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(cons_be_t70, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(cons_su_t50, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(cons_su_t70, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
