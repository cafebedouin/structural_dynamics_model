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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Constitutional Secularism: Principled Intervention Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint represents the 'principled intervention' reading of
 *   constitutional secularism, where the state actively intervenes in
 *   religious affairs to advance social reform and protect vulnerable groups.
 *   This reading legitimizes differential treatment of religious groups based
 *   on reform objectives, expanding state authority into domains
 *   traditionally governed by religious autonomy. The metrics reflect a
 *   system with substantial extraction and suppression, as religious
 *   communities are compelled to conform to state-defined social norms. This
 *   is one reading of the 'constitutional_secularism' kernel.
 *
 * KEY AGENTS:
 *   - state_legislature: Agenda setter (institutional/constrained) — enacts reform laws
 *   - judiciary: Agenda setter (institutional/constrained) — interprets and upholds interventions
 *   - social_reform_advocates: Beneficiary (organized/mobile) — benefit from state-led reforms
 *   - religious_minorities: Payer (powerless/trapped) — bear costs of altered practices
 *   - traditionalist_religious_groups: Payer (moderate/constrained) — resist state interference
 *   - religious_autonomy_advocates: Excluded (organized/constrained) — arguments often sidelined
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
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Constitutional Secularism: Principled Intervention Reading").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '8543dffd-557d-4efc-b6ea-4dbbb1f05331').
narrative_ontology:cs_kernel_codification('8543dffd-557d-4efc-b6ea-4dbbb1f05331', formalized).
narrative_ontology:cs_authority_grounding('8543dffd-557d-4efc-b6ea-4dbbb1f05331', lineage).
narrative_ontology:cs_interpretation_layer_present('8543dffd-557d-4efc-b6ea-4dbbb1f05331').
narrative_ontology:cs_reading_relation('8543dffd-557d-4efc-b6ea-4dbbb1f05331', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('8543dffd-557d-4efc-b6ea-4dbbb1f05331', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('8543dffd-557d-4efc-b6ea-4dbbb1f05331', foundational, state_has_duty_to_reform_social_ills).
narrative_ontology:cs_axiom_status(state_has_duty_to_reform_social_ills, holdable).
narrative_ontology:cs_axiom_grounding('8543dffd-557d-4efc-b6ea-4dbbb1f05331', state_has_duty_to_reform_social_ills, deontological).
narrative_ontology:cs_axiom('8543dffd-557d-4efc-b6ea-4dbbb1f05331', foundational, religious_autonomy_is_subordinate_to_social_justice).
narrative_ontology:cs_axiom_status(religious_autonomy_is_subordinate_to_social_justice, holdable).
narrative_ontology:cs_axiom_grounding('8543dffd-557d-4efc-b6ea-4dbbb1f05331', religious_autonomy_is_subordinate_to_social_justice, conventional).
narrative_ontology:cs_reference_frame('8543dffd-557d-4efc-b6ea-4dbbb1f05331', secular_state_as_social_reformer).
narrative_ontology:cs_drift_state('8543dffd-557d-4efc-b6ea-4dbbb1f05331', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8543dffd-557d-4efc-b6ea-4dbbb1f05331', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, state_legislature).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, social_reform_advocates).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_minorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, traditionalist_religious_groups).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts laws that intervene in religious practices, justifying them as social reform or protection for weaker sections. Benefits from expanded legislative scope and moral authority.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets and upholds state interventions, balancing religious freedom with social reform objectives. Benefits from expanded interpretive power and the ability to shape social norms.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for state intervention to address perceived injustices within religious communities. Benefit from the state's willingness to enact their desired reforms.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, social_reform_advocates, beneficiary,
    organized, biographical, mobile, national).

% Are often the target of interventions, experiencing their religious practices curtailed or altered by state mandates. Bear the cost of adapting to new laws or facing legal penalties, with limited recourse.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_minorities, payer,
    powerless, generational, trapped, local).

% Resist state interference in their established religious customs and laws. Bear the cost of legal challenges, social pressure, and the erosion of their autonomy.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, traditionalist_religious_groups, payer,
    moderate, generational, constrained, regional).

% Argue for strict non-interference by the state in religious matters, emphasizing the right of communities to self-govern. Their arguments are often sidelined in favor of reformist agendas.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's role in balancing religious freedom with broader social justice and equality goals, providing a framework for legislative and judicial action in complex inter-community disputes.
% TRANSFER_FUNCTION: Transfers authority over certain religious practices from religious communities to the state, enabling the state to reallocate rights and obligations within those communities to achieve social reform.
% ABSENT_VOICES: Strict religious autonomy advocates, who would argue that state intervention, even for reform, undermines the fundamental right to religious self-governance, are often marginalized in the discourse.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the state would lose its justification for intervening in religious affairs, leading to a significant shift in legal and political approaches to religious communities, potentially empowering traditionalist groups and altering the landscape of social reform efforts.
% FOUNDING_PROBLEM: To address historical injustices and inequalities perpetuated within religious communities, particularly against women, lower castes, or other marginalized groups, where religious personal laws or customs were seen as barriers to social progress.
% FOUNDING_PROBLEM_CORROBORATION: Social reform movements and human rights organizations attest that the founding problem remains live, citing ongoing discrimination and inequality within religious communities. Traditionalist groups and religious autonomy advocates contest the state's framing of these issues as requiring intervention, arguing for internal community-led reform or non-interference.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is driven by the state's power to redefine and enforce religious practices, imposing costs on communities that prefer traditional autonomy. Suppression (0.70) is high due to the active legal and political mechanisms used to enforce interventions and limit resistance from religious groups. The theater ratio (0.20) is relatively low, as the interventions are genuinely aimed at reform, though the justification may sometimes mask majoritarian preferences. The increasing trend in extractiveness and suppression over time reflects the expanding scope of state intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and social reform advocates, this constraint is a necessary mechanism for progress and justice. From the perspective of religious communities, particularly minorities and traditionalists, it is an imposition that erodes their autonomy and extracts conformity, often without their consent. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legislature and judiciary are clear beneficiaries, gaining expanded authority and legitimacy. Social reform advocates also benefit as their goals are advanced. Religious minorities and traditionalist groups are targets, bearing the direct costs of altered practices and suppressed autonomy. Religious autonomy advocates are excluded, their arguments for non-interference largely unheeded.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling state intervention as pure coordination by highlighting the asymmetric extraction from religious communities. It also avoids mislabeling it as a pure snare by acknowledging the genuine coordination function of balancing competing rights and the stated goal of social reform. The 'contested' status of the founding problem indicates an ongoing debate about whether the original mandate is still being genuinely addressed or if it has drifted into rent-seeking by the state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_capture_risk,
    'Does the ''principled intervention'' framework disproportionately target minority religious practices, reflecting majoritarian preferences rather than universal reform principles?',
    'Empirical analysis of intervention patterns: if interventions consistently align with the norms of the dominant religious group while targeting minorities, it indicates majoritarian capture.',
    'If majoritarian capture is confirmed, the effective extractiveness and suppression for religious minorities would be higher, reclassifying the constraint closer to a Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Risk of state intervention being driven by majoritarian religious norms.').

omega_variable(
    legitimacy_of_state_defined_reform,
    'Is the state the legitimate arbiter of ''social reform'' within religious communities, or should reform efforts originate internally?',
    'Conceptual analysis of sovereignty and autonomy: if religious communities are deemed sovereign in internal matters, state-defined reform lacks legitimacy.',
    'If state-defined reform is deemed illegitimate, the coordination function of this constraint collapses, and its classification shifts closer to a Snare for all religious groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_state_defined_reform, conceptual, 'Whether the state''s definition of social reform is legitimate for religious communities.').

omega_variable(
    intervention_necessity_vs_alternatives,
    'Are state interventions truly necessary to protect weaker sections, or do viable internal community-led reform mechanisms exist that are suppressed by state action?',
    'Comparative case studies of communities with and without state intervention, assessing outcomes for weaker sections and the presence of internal reform movements.',
    'If effective internal mechanisms are found to be suppressed, the constraint''s suppression metric would be higher, and its coordination function would be undermined, pushing it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_necessity_vs_alternatives, empirical, 'Necessity of state intervention versus internal reform alternatives.').


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
narrative_ontology:measurement(cons_tr_t2010, constitutional_secularism__principled_intervention_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cons_tr_t2024, constitutional_secularism__principled_intervention_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(cons_be_t1990, constitutional_secularism__principled_intervention_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(cons_be_t2024, constitutional_secularism__principled_intervention_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_secularism__principled_intervention_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_secularism__principled_intervention_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'constitutional_secularism' kernel, focusing on principled state intervention. It is structurally distinct from the 'strict_neutrality_reading' and 'reformist_reading' of the same kernel, which emphasize different state-religion relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
