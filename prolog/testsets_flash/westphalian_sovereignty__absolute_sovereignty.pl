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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'absolute sovereignty' reading of
 *   Westphalian sovereignty, where states possess unconditional authority
 *   over their domestic affairs and external interference is categorically
 *   illegitimate. It is claimed as a tangled_rope because it provides a
 *   coordination function (interstate stability) but also enables significant
 *   extraction from domestic populations under repressive regimes, requiring
 *   active enforcement (diplomatic and military non-intervention) to hold.
 *   The metrics reflect this: moderate extractiveness (0.55) and high
 *   suppression (0.7) due to the active defense of non-interference by
 *   states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '1cd13416-62d8-411e-9626-daca008a58b4').
narrative_ontology:cs_kernel_codification('1cd13416-62d8-411e-9626-daca008a58b4', formalized).
narrative_ontology:cs_authority_grounding('1cd13416-62d8-411e-9626-daca008a58b4', lineage).
narrative_ontology:cs_interpretation_layer_present('1cd13416-62d8-411e-9626-daca008a58b4').
narrative_ontology:cs_reading_relation('1cd13416-62d8-411e-9626-daca008a58b4', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('1cd13416-62d8-411e-9626-daca008a58b4', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('1cd13416-62d8-411e-9626-daca008a58b4', foundational, state_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(state_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('1cd13416-62d8-411e-9626-daca008a58b4', state_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('1cd13416-62d8-411e-9626-daca008a58b4', foundational, non_interference_is_primary_international_norm).
narrative_ontology:cs_axiom_status(non_interference_is_primary_international_norm, holdable).
narrative_ontology:cs_axiom_grounding('1cd13416-62d8-411e-9626-daca008a58b4', non_interference_is_primary_international_norm, conventional).
narrative_ontology:cs_reference_frame('1cd13416-62d8-411e-9626-daca008a58b4', post_westphalian_state_system).
narrative_ontology:cs_drift_state('1cd13416-62d8-411e-9626-daca008a58b4', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1cd13416-62d8-411e-9626-daca008a58b4', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the non-interference shield, allowing them to maintain internal control without external accountability for human rights abuses. They actively invoke this principle to deflect criticism and intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% Benefits from the principle as a defense against unwanted external influence, even if their domestic governance is generally legitimate. They value the freedom from intervention as a core aspect of statehood.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy, beneficiary,
    powerful, generational, mobile, national).

% Bears the cost of unchecked state power, as the principle shields their governments from accountability for abuses. Their suffering is often invisible or dismissed as an 'internal affair'.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Faces significant obstacles in advocating for intervention or accountability, as the principle of absolute sovereignty is used to delegitimize their efforts. They bear the cost of limited avenues for redress.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates, payer,
    organized, biographical, constrained, global).

% Administers international law, including the principle of sovereignty. They are often caught between upholding non-interference and responding to humanitarian crises, leading to internal tension and selective enforcement.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_organizations, agenda_setter,
    institutional, generational, constrained, global).

% Often publicly espouse human rights norms but are constrained by the principle of sovereignty in their foreign policy. They observe and debate the tension between these principles, sometimes selectively intervening or condemning.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for stable interstate relations by defining clear boundaries of authority and preventing constant external meddling in domestic affairs, thereby reducing interstate conflict.
% TRANSFER_FUNCTION: Transfers the right to internal self-determination and non-interference from the international community to individual states, in exchange for states respecting each other's territorial integrity.
% ABSENT_VOICES: Victims of state repression and those advocating for universal human rights are often marginalized or silenced in international forums where this principle is invoked, as their claims are deemed 'internal matters'.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty vanished, the international system would undergo a profound reorganization. States would face constant scrutiny and potential intervention, leading to either a more rights-respecting global order or widespread instability and conflict as states resist external pressures.
% FOUNDING_PROBLEM: The problem of incessant interstate warfare and religious conflicts in Europe, where external powers routinely interfered in the internal affairs of other states, leading to prolonged instability and devastation.
% FOUNDING_PROBLEM_CORROBORATION: States, particularly those with authoritarian tendencies, argue the problem of external interference remains live. Human rights organizations and some liberal states argue the original problem has been superseded by the need to protect populations from their own governments; academic international relations scholars provide corroboration for both perspectives, highlighting the historical evolution of the concept.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is driven by the cost borne by populations whose governments are shielded from accountability. Suppression is high because the principle requires active diplomatic and sometimes military non-intervention to prevent external actors from challenging state authority. The theater ratio is low (0.2) as the principle is genuinely invoked and defended, not merely performed. The historical measurements show a slight increase in extractiveness and suppression post-WWII, reflecting the rise of human rights norms that this reading actively suppresses, followed by a slight decline as the 'Responsibility to Protect' doctrine gained some traction, though it remains contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, this is a legitimate rope ensuring state stability. From the perspective of repressed populations, it is a snare that enables their suffering. The engine's classification as tangled_rope captures this dual nature, acknowledging both the coordination function for states and the asymmetric extraction from populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states prioritizing autonomy are clear beneficiaries (low directionality), as the principle grants them a shield against external scrutiny. Domestic populations under repression and human rights advocates are clear victims/payers (high directionality), as their avenues for redress are blocked. International organizations and liberal democracies occupy more complex positions, often acting as agenda-setters or observers caught between competing principles.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_non_interference,
    'Is the categorical illegitimacy of external interference a universal moral principle, or a historically contingent norm designed to protect state power?',
    'Philosophical analysis of foundational political ethics, combined with historical-sociological study of the origins and evolution of the Westphalian system.',
    'If a universal moral principle, the extractiveness from repressed populations is a tragic but unavoidable consequence of a higher good (state stability). If historically contingent, the extraction is a remediable design flaw of the international system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_non_interference, conceptual, 'The moral grounding of the non-interference principle.').

omega_variable(
    effectiveness_of_non_interference_for_stability,
    'Does absolute non-interference genuinely lead to greater international stability, or does it merely defer and intensify conflicts by allowing internal repression to fester?',
    'Empirical study of historical cases: comparing regions with strict non-interference regimes to those with more conditional approaches, measuring long-term conflict incidence and severity.',
    'If it leads to greater stability, the coordination function is strong. If it leads to deferred conflict, the coordination function is weak or illusory, pushing the constraint closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_non_interference_for_stability, empirical, 'The actual impact of non-interference on international stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(west_tr_t1800, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(west_tr_t1900, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1648, 0.45).
narrative_ontology:measurement(west_be_t1800, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement(west_be_t1900, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.58).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.56).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1648, 0.6).
narrative_ontology:measurement(west_su_t1800, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(west_su_t1900, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Westphalian sovereignty kernel. Its structural properties (high extraction from domestic populations, strong non-interference shield for states) contrast with sibling readings that emphasize conditional or graduated sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
