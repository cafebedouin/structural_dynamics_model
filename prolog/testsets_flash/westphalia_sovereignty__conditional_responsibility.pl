% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty (Responsibility to Protect)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint represents the 'conditional responsibility' reading of
 *   Westphalian sovereignty, asserting that states forfeit their territorial
 *   inviolability when they fail to protect their populations from mass
 *   atrocities. This reading, often associated with the Responsibility to
 *   Protect (R2P) doctrine, posits that sovereignty is not absolute but
 *   entails obligations to one's own population, and failure to meet these
 *   obligations can legitimize external intervention. This is a contested
 *   interpretation of sovereignty, challenging the traditional 'absolute
 *   non-intervention' view.
 *
 * KEY AGENTS:
 *   - states_failing_to_protect: Primary target (institutional/constrained) — bears extraction and potential intervention
 *   - populations_under_atrocity_regimes: Primary victim (powerless/trapped) — suffers atrocities, but also potential beneficiary of intervention
 *   - humanitarian_intervention_coalitions: Primary beneficiary (organized/arbitrage) — gains legitimacy and authority to intervene
 *   - global_governance_institutions: Agenda setter (institutional/analytical) — adjudicates and legitimizes interventions
 *   - states_upholding_absolute_non_intervention: Excluded (institutional/constrained) — would object to intervention but are overridden by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.65).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.75).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty (Responsibility to Protect)").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '1fc119b8-33a0-46af-a9ca-99ceed43d8dc').
narrative_ontology:cs_kernel_codification('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', formalized).
narrative_ontology:cs_authority_grounding('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', lineage).
narrative_ontology:cs_interpretation_layer_present('1fc119b8-33a0-46af-a9ca-99ceed43d8dc').
narrative_ontology:cs_reading_relation('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', foundational, sovereignty_entails_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', sovereignty_entails_responsibility, deontological).
narrative_ontology:cs_axiom('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', secondary, international_community_has_right_to_intervene).
narrative_ontology:cs_axiom_status(international_community_has_right_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', international_community_has_right_to_intervene, conventional).
narrative_ontology:cs_reference_frame('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', post_cold_war_humanitarian_consensus).
narrative_ontology:cs_drift_state('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', contemporary_geopolitical_realignment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1fc119b8-33a0-46af-a9ca-99ceed43d8dc', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are states whose internal conduct, specifically their failure to prevent or stop mass atrocities, triggers the conditional aspect of their sovereignty. They face potential external intervention, loss of territorial inviolability, and possibly regime change. Their 'exit' is to comply with international norms, which may involve significant internal political costs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_failing_to_protect, payer,
    institutional, generational, constrained, national).

% These populations are the direct victims of mass atrocities perpetrated or allowed by their own state. While they are the ultimate beneficiaries of the 'protection' aspect of the constraint, they bear the immediate costs of the atrocities and the potential disruption of intervention. Their options are limited to survival or flight.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, beneficiary).

% These are groups of states or international organizations that gain the moral and legal justification to intervene in the internal affairs of other states under this doctrine. They benefit from the expanded scope of legitimate action and the ability to project power under a humanitarian banner. Their 'arbitrage' comes from choosing when and where to intervene based on strategic interests and capacity.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    organized, biographical, arbitrage, global).

% Organizations like the UN Security Council or the International Criminal Court are tasked with adjudicating when a state has failed its responsibility to protect and legitimizing intervention. They gain increased authority and a mandate to shape international norms and enforce compliance. Their 'analytical' exit reflects their role in defining and interpreting the constraint itself.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% These states adhere to a more traditional, absolute view of sovereignty, where external interference is illegitimate regardless of internal conduct. They are often excluded from the decision-making process regarding interventions under this doctrine, or their objections are overridden. Their 'constrained' exit means they can protest but often cannot prevent interventions by powerful coalitions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, states_upholding_absolute_non_intervention, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international action and legitimize intervention in cases where states fail to protect their populations from mass atrocities, thereby preventing or halting genocide, war crimes, ethnic cleansing, and crimes against humanity.
% TRANSFER_FUNCTION: Transfers the right to territorial inviolability from states failing to protect their populations to the international community, which gains the authority to intervene. It also transfers the burden of intervention (military, financial, political) to intervening states and the costs of conflict to affected populations.
% ABSENT_VOICES: States that strongly uphold the principle of absolute non-intervention, often those with histories of colonial exploitation or concerns about sovereignty violations, are often marginalized or overridden in discussions and decisions regarding interventions under this doctrine. They would argue for stricter adherence to non-interference and alternative, non-coercive means of atrocity prevention.
% DISAPPEARANCE_RATIONALE: If this conditional understanding of sovereignty vanished, the international legal landscape would revert to a more absolute non-interventionist stance. States would regain full, unconditional territorial inviolability, potentially leading to an increase in unchecked mass atrocities within borders, and a significant reduction in the legitimacy of humanitarian interventions. The global governance architecture would need to fundamentally re-evaluate its role in human rights protection.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, leading to a moral and legal imperative to re-evaluate the limits of state sovereignty in the face of such crimes.
% FOUNDING_PROBLEM_CORROBORATION: The problem of mass atrocities remains live, as evidenced by ongoing conflicts and human rights violations globally. International human rights organizations, UN reports, and academic scholars (outside the direct beneficiaries of intervention) consistently corroborate the continued existence of the problem, even while debating the efficacy and legitimacy of the 'conditional responsibility' response.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely aims to coordinate international action to prevent atrocities (a coordination function) but also involves significant asymmetric extraction from states deemed to have failed their populations, and from the populations themselves who bear the costs of both atrocities and interventions. Active enforcement is required to overcome traditional notions of absolute sovereignty. Extractiveness is high (0.65) due to the loss of sovereign control and potential for regime change. Suppression is high (0.75) as it overrides traditional state prerogatives. Theater ratio is low (0.20) as interventions, when they occur, are typically real and consequential, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of humanitarian intervention coalitions and global governance institutions, this is a necessary coordination mechanism to uphold human rights. From the perspective of states targeted for intervention, it is a violation of sovereignty and a form of extraction. Populations under atrocity regimes are victims of their own state but potential beneficiaries of intervention, creating a complex, often tragic, perspectival split.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanitarian intervention coalitions and global governance institutions are beneficiaries (d near 0.0) as they gain authority and legitimacy to act. States failing to protect are targets (d near 1.0) as they face the highest costs, including loss of territorial inviolability. Populations under atrocity regimes are victims (d near 1.0) of their own state, but also potential beneficiaries of the constraint's operation if intervention occurs, making their directionality complex and context-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the coercive extraction from targeted states) or a pure Snare (ignoring the genuine coordination function of atrocity prevention). The 'contested' status of the founding problem reflects the ongoing debate about whether the original problem of mass atrocities is being genuinely addressed or if the doctrine is being used as a pretext for other geopolitical interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine evolution of international law or a justification for selective intervention?',
    'Analysis of intervention patterns: if interventions are consistently applied regardless of geopolitical interest, it supports genuine evolution; if selectively applied, it supports a justification for power projection.',
    'If a genuine evolution, the constraint moves towards a Rope; if a justification, it moves towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''conditional_responsibility'' reading of the ''westphalia_sovereignty'' kernel. Sibling readings include ''absolute_non_intervention'' and ''graded_sovereignty''. This reading lowers the intervention threshold and grants adjudicative authority to the international community.').

omega_variable(
    intervention_threshold_ambiguity,
    'What constitutes ''failing to protect populations from mass atrocities'' and who adjudicates this failure?',
    'Development of clear, universally accepted criteria for atrocity crimes and a neutral, authoritative international body for their adjudication.',
    'Clearer criteria and neutral adjudication would reduce the ''suppression'' and ''extractiveness'' by limiting arbitrary application, moving the constraint closer to a Rope. Ambiguity allows for selective application, increasing extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_threshold_ambiguity, conceptual, 'The precise threshold for intervention and the authority for its determination remain contested, leading to potential for abuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.25).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.22).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.21).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalia_sovereignty' kernel. Other readings include 'absolute_non_intervention' and 'graded_sovereignty', each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
