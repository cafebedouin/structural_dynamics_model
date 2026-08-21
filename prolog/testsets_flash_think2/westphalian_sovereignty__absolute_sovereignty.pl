% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute State Sovereignty (Westphalian Reading)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'absolute sovereignty' reading of the
 *   Westphalian system, asserting that states possess unconditional authority
 *   over their domestic affairs and that external interference is
 *   categorically illegitimate. It is often invoked by states to prevent
 *   scrutiny of internal policies, including human rights records. While
 *   presented as a foundational principle for international stability, its
 *   operation often results in significant extraction from domestic
 *   populations under repressive regimes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.75).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute State Sovereignty (Westphalian Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '5c623a0a-71cf-415a-9272-48225c3bb4eb').
narrative_ontology:cs_kernel_codification('5c623a0a-71cf-415a-9272-48225c3bb4eb', formalized).
narrative_ontology:cs_authority_grounding('5c623a0a-71cf-415a-9272-48225c3bb4eb', lineage).
narrative_ontology:cs_interpretation_layer_present('5c623a0a-71cf-415a-9272-48225c3bb4eb').
narrative_ontology:cs_reading_relation('5c623a0a-71cf-415a-9272-48225c3bb4eb', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('5c623a0a-71cf-415a-9272-48225c3bb4eb', westphalian_sovereignty__graduated_sovereignty, forecloses).
narrative_ontology:cs_axiom('5c623a0a-71cf-415a-9272-48225c3bb4eb', foundational, state_non_interference_absolute).
narrative_ontology:cs_axiom_status(state_non_interference_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5c623a0a-71cf-415a-9272-48225c3bb4eb', state_non_interference_absolute, deontological).
narrative_ontology:cs_axiom('5c623a0a-71cf-415a-9272-48225c3bb4eb', foundational, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('5c623a0a-71cf-415a-9272-48225c3bb4eb', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('5c623a0a-71cf-415a-9272-48225c3bb4eb', post_westphalian_order).
narrative_ontology:cs_drift_state('5c623a0a-71cf-415a-9272-48225c3bb4eb', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5c623a0a-71cf-415a-9272-48225c3bb4eb', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, states_seeking_non_interference).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, liberal_democracies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These regimes actively invoke and defend the principle of absolute sovereignty to shield their domestic policies, including human rights abuses, from external scrutiny or intervention. They benefit from the non-interference norm, which grants them impunity.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary).

% States that prioritize their own autonomy and non-interference in their internal affairs, often due to historical grievances or a desire to avoid external pressure, benefit from this reading of sovereignty. They may not be repressive but value the shield.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, states_seeking_non_interference, beneficiary,
    institutional, generational, mobile, global).

% These populations bear the direct costs of their governments' abuses, with the principle of absolute sovereignty often used to deny them any external recourse or protection. Their suffering is effectively 'internalized' by the state.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Organizations and individuals who champion human rights find their efforts to protect vulnerable populations severely hampered by the absolute sovereignty principle, which delegitimizes external intervention even in cases of severe atrocities. They are often excluded from formal state-centric discussions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates, excluded,
    organized, biographical, constrained, global).

% These states often face a dilemma: uphold the non-interference principle or intervene to prevent human rights violations. Upholding absolute sovereignty can come at the cost of their moral standing and long-term strategic interests, creating a 'cost of inaction'.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democracies, payer,
    institutional, generational, constrained, global).

% Academics and legal experts who analyze the evolution and application of international law. They observe the tension between state sovereignty and emerging norms like the Responsibility to Protect, often providing critical analysis of the constraint's effects.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international order by delineating clear boundaries of state authority, thereby reducing interstate conflict arising from intervention in internal affairs.
% TRANSFER_FUNCTION: Transfers the right to self-determination and non-interference to states, from external actors and, implicitly, from the domestic populations who might otherwise seek external protection.
% ABSENT_VOICES: Domestic populations under repressive regimes are largely absent from the international legal discourse that upholds absolute sovereignty; their voices would challenge the legitimacy of non-interference in cases of severe human rights abuses.
% DISAPPEARANCE_RATIONALE: If the principle of absolute sovereignty vanished overnight, the international system would undergo a profound rearrangement. States would lose their primary shield against external scrutiny, potentially leading to increased interventions, a redefinition of statehood, and a more fluid, less state-centric global governance structure.
% FOUNDING_PROBLEM: The principle was established to end the religious wars and constant interventions that plagued Europe, creating a stable international order based on mutual recognition of state authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Westphalian system and proponents of state-centric international relations corroborate the founding problem. Critics, including human rights organizations and liberal international law scholars, argue that the problem has evolved, and the absolute principle now creates new problems.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is substantial because the principle, in this absolute form, allows states to externalize the costs of internal repression onto their populations without international accountability. Suppression (0.75) is high as it actively delegitimizes and resists any attempts at external intervention or even strong diplomatic pressure. The theater ratio (0.40) reflects that while the principle is genuinely invoked, its application often serves to mask or justify actions that contradict broader international norms, making its 'coordination' function partly performative. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the hardening of this position in response to growing international human rights norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this constraint is a legitimate 'rope' for maintaining state stability and national self-determination. From the perspective of domestic populations under repression, it functions as a 'snare' that traps them in abusive situations. The engine's classification as a Tangled Rope reflects this dual function: it coordinates non-interference among states but extracts heavily from vulnerable populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states prioritizing non-interference are clear beneficiaries, as the constraint shields them from external accountability. Domestic populations under repression and human rights advocates are victims, as their ability to seek redress or protection is severely curtailed. Liberal democracies act as payers, bearing the moral and strategic costs of non-intervention. International law scholars serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of preventing interstate wars of intervention is still live, but its status is contested. Critics argue that the absolute interpretation of sovereignty has outlived its original function and now primarily serves to protect repressive regimes, indicating a potential shift towards mandatrophy. The high extractiveness and suppression, coupled with the contested founding problem status, suggest that the constraint's original coordination function is now significantly intertwined with, or overshadowed by, its extractive effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_definition_ambiguity,
    'What constitutes ''interference'' or ''intervention'' in domestic affairs? Does it include economic sanctions, diplomatic pressure, or only military action?',
    'Consensus-building within international legal bodies or a clear, universally adopted definition in a new international treaty.',
    'A narrow definition would reduce the scope of the absolute sovereignty shield, potentially lowering its effective suppression. A broad definition would reinforce the shield, increasing suppression and extractiveness for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_definition_ambiguity, conceptual, 'Ambiguity in what actions are considered illegitimate interference.').

omega_variable(
    state_stability_vs_human_rights,
    'Is the absolute non-interference principle genuinely necessary for global stability, or does it primarily serve to protect state power at the expense of human rights?',
    'Empirical studies on the long-term effects of intervention vs. non-intervention on regional stability and human development, alongside philosophical re-evaluation of the ''order vs. justice'' dilemma.',
    'If found to primarily protect state power, the constraint''s extractiveness would be re-evaluated upwards, and its coordination function would be seen as a cover. If found essential for stability, its coordination value would be affirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_stability_vs_human_rights, empirical, 'Tension between state stability and human rights protection.').

omega_variable(
    kernel_reading_divergence,
    'Is this constraint a genuine ''absolute sovereignty'' principle, or is it better understood as a ''conditional sovereignty'' principle where conditions are simply unstated or highly permissive?',
    'Analysis of state practice and international legal judgments: if states consistently act as if conditions exist, even while denying them, the ''conditional_sovereignty'' reading gains empirical support.',
    'If reclassified as ''conditional_sovereignty'', the constraint''s extractiveness and suppression would likely decrease, as the possibility of legitimate intervention would be acknowledged, even if rarely exercised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'This constraint is one reading of the ''westphalian_sovereignty'' kernel. Sibling readings (conditional_sovereignty, graduated_sovereignty) would alter the beneficiary/victim structure and the level of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1648, 0.2).
narrative_ontology:measurement(west_tr_t1750, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(west_tr_t1850, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1850, 0.3).
narrative_ontology:measurement(west_tr_t1950, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1648, 0.45).
narrative_ontology:measurement(west_be_t1750, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1750, 0.48).
narrative_ontology:measurement(west_be_t1850, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1850, 0.5).
narrative_ontology:measurement(west_be_t1950, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1648, 0.6).
narrative_ontology:measurement(west_su_t1750, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1750, 0.65).
narrative_ontology:measurement(west_su_t1850, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement(west_su_t1950, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
