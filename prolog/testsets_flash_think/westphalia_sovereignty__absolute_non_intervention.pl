% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Principle (Westphalian Sovereignty Reading)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint describes the 'absolute non-intervention' reading of
 *   Westphalian sovereignty, where external interference in a state's
 *   domestic affairs is considered illegitimate regardless of internal
 *   conduct. It is a foundational principle of the modern interstate system,
 *   often invoked by states to shield themselves from external scrutiny or
 *   intervention, even in cases of severe human rights abuses. This reading
 *   prioritizes state stability and territorial integrity over individual
 *   human rights or international accountability for domestic governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.85).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.9).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.85).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, snare).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Principle (Westphalian Sovereignty Reading)").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '025f342a-5b1e-41ea-aefb-178e4b279435').
narrative_ontology:cs_kernel_codification('025f342a-5b1e-41ea-aefb-178e4b279435', formalized).
narrative_ontology:cs_authority_grounding('025f342a-5b1e-41ea-aefb-178e4b279435', practice).
narrative_ontology:cs_interpretation_layer_present('025f342a-5b1e-41ea-aefb-178e4b279435').
narrative_ontology:cs_reading_relation('025f342a-5b1e-41ea-aefb-178e4b279435', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('025f342a-5b1e-41ea-aefb-178e4b279435', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('025f342a-5b1e-41ea-aefb-178e4b279435', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('025f342a-5b1e-41ea-aefb-178e4b279435', state_territorial_integrity_absolute, conventional).
narrative_ontology:cs_axiom('025f342a-5b1e-41ea-aefb-178e4b279435', foundational, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('025f342a-5b1e-41ea-aefb-178e4b279435', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('025f342a-5b1e-41ea-aefb-178e4b279435', post_westphalian_state_order).
narrative_ontology:cs_drift_state('025f342a-5b1e-41ea-aefb-178e4b279435', post_cold_war_humanitarian_intervention_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('025f342a-5b1e-41ea-aefb-178e4b279435', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, sovereign_states_generally).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites directly benefit from the principle, as it grants them categorical immunity from external interference, allowing them to maintain power and control over their populations without accountability for domestic conduct. They actively invoke and defend this principle in international forums.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, beneficiary).

% All states benefit from the general protection against external meddling, which this principle provides. It underpins the stability of the interstate system, even if some states selectively challenge its application in specific cases.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, sovereign_states_generally, beneficiary,
    institutional, generational, mobile, global).

% These populations bear the primary cost of the principle, as it denies them any legitimate avenue for external protection or intervention when their own state commits atrocities or systematically violates their rights. Their alternatives are completely collapsed by this constraint.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule, excluded).

% These groups actively resist the absolute non-intervention principle, arguing it shields human rights abusers. They face structural barriers in their efforts to mobilize international action due to the principle's strong normative and legal standing.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, human_rights_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, human_rights_advocates, excluded).

% Organizations like the UN are formally bound by this principle (e.g., UN Charter Article 2(7)), which limits their ability to intervene in domestic affairs. They often try to navigate this constraint by reinterpreting 'domestic affairs' or promoting alternative principles like R2P, but face strong resistance from states upholding absolute non-intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_organizations, agenda_setter,
    institutional, generational, constrained, global).

% These states often find themselves in a moral and political bind, wishing to intervene in humanitarian crises but constrained by the absolute non-intervention principle. They bear the political cost of inaction or the diplomatic cost of challenging the principle.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, liberal_interventionist_states, observer,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for interstate relations by defining the boundaries of state authority and preventing constant external interference in internal affairs, thereby promoting a degree of stability in the international system.
% TRANSFER_FUNCTION: Transfers the exclusive right to determine internal affairs to the sovereign state, removing external accountability for domestic conduct and effectively granting a monopoly on violence and governance within its borders.
% ABSENT_VOICES: Populations suffering under oppressive regimes are structurally excluded from the international conversation about intervention. If present, they would unequivocally object to the principle, advocating for external protection and accountability for their governments.
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention principle vanished overnight, the international system would face profound instability. States would constantly challenge each other's internal legitimacy, leading to widespread interventions, proxy conflicts, and a fundamental redefinition of statehood and international order.
% FOUNDING_PROBLEM: The principle emerged from the chaos of post-Reformation Europe, particularly the Thirty Years' War, where internal religious and political conflicts frequently spilled over into devastating interstate wars. It aimed to stabilize the nascent state system by establishing clear boundaries of sovereign authority.
% FOUNDING_PROBLEM_CORROBORATION: Traditional international relations scholars and many state elites (especially those in authoritarian regimes) argue the founding problem of interstate chaos and war remains live, necessitating the principle. Human rights organizations and some liberal states contend the original problem is largely superseded by new challenges (e.g., mass atrocities) and the principle now primarily serves to protect oppressive regimes; this is supported by independent analyses of state behavior and human rights records.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.85) because the principle effectively grants state elites a monopoly on power and resource allocation within their borders, free from external checks, allowing them to extract from their populations. Suppression is also very high (0.90) as the principle legitimizes and protects state-led suppression of internal dissent by denying external intervention. The theater ratio is low (0.20) because the principle is functionally enforced through diplomatic norms, international law, and military deterrence, not merely performatively maintained. Accessibility collapse is high for victim populations, as it removes any viable external alternative to their situation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state elites, this principle is a necessary foundation for international order and self-determination. From the perspective of victim populations and human rights advocates, it is a snare that shields oppressive regimes and perpetuates suffering. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian state elites are clear beneficiaries and agenda-setters, as the principle directly protects their power. Sovereign states generally also benefit from the systemic stability it provides. Populations under authoritarian rule are the primary victims, as the principle denies them external recourse. Human rights advocates and liberal interventionist states bear the costs of trying to challenge or navigate this deeply entrenched norm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_affairs_boundary,
    'What constitutes ''domestic affairs'' versus matters of ''international concern'' (e.g., mass atrocities, cross-border environmental damage)?',
    'Evolution of customary international law, UN Security Council resolutions, and the practice of states in defining the scope of intervention.',
    'If the boundary shifts to include more internal conduct as international concern, the principle''s effective suppression and extractiveness would decrease, potentially reclassifying it towards a Tangled Rope or even Rope for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_affairs_boundary, conceptual, 'Ambiguity in the scope of non-intervention.').

omega_variable(
    coordination_vs_extraction_priority,
    'Is the primary function of this principle to coordinate interstate stability, or to protect state elites from accountability and facilitate extraction from their populations?',
    'Empirical analysis of state behavior: do states primarily invoke the principle to prevent genuine interstate conflict, or to deflect criticism of internal governance? Examination of the correlation between invocation and internal human rights records.',
    'If the latter is dominant, the Snare classification is strongly reinforced; if the former, the coordination function might be more genuinely acknowledged, potentially shifting towards a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_priority, empirical, 'The true purpose of the non-intervention principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(west_tr_t1750, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1750, 0.12).
narrative_ontology:measurement(west_tr_t1850, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.6).
narrative_ontology:measurement(west_be_t1750, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1750, 0.65).
narrative_ontology:measurement(west_be_t1850, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1850, 0.7).
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.75).
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.65).
narrative_ontology:measurement(west_su_t1750, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1750, 0.7).
narrative_ontology:measurement(west_su_t1850, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, international_humanitarian_law).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, responsibility_to_protect_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
