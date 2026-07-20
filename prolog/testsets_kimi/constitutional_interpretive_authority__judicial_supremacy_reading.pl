% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   contested kernel constitutional_interpretive_authority. Under this
 *   reading, courts possess the final say on constitutional meaning,
 *   legitimated by their role as guardians of fundamental rights. The
 *   legislature is structurally subordinated: its acts are subject to
 *   nullification. This creates both a coordination function (rights
 *   protection, constitutional stability) and an asymmetric extraction of
 *   democratic authority from the legislative and electoral sphere. The
 *   judiciary gains institutional power and interpretive prestige;
 *   rights-bearing citizens gain a veto channel; legislatures and popular
 *   majorities bear the cost of subordination.
 *
 * KEY AGENTS:
 *   - Judiciary: agenda_setter and beneficiary (institutional/constrained) â gains interpretive authority and institutional prestige
 *   - Legislature: payer (institutional/constrained) â bears cost of nullification and subordination
 *   - Rights-bearing citizens: beneficiary (moderate/constrained) â receive rights protection channel
 *   - Popular majorities: excluded (moderate/constrained) â democratic will overridden
 *   - Constitutional scholars: observer (analytical) â provide external critique and corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.63).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '235aceaf-7bfc-4656-bc1d-e3f5582cd905').
narrative_ontology:cs_kernel_codification('235aceaf-7bfc-4656-bc1d-e3f5582cd905', formalized).
narrative_ontology:cs_authority_grounding('235aceaf-7bfc-4656-bc1d-e3f5582cd905', lineage).
narrative_ontology:cs_interpretation_layer_present('235aceaf-7bfc-4656-bc1d-e3f5582cd905').
narrative_ontology:cs_reading_relation('235aceaf-7bfc-4656-bc1d-e3f5582cd905', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('235aceaf-7bfc-4656-bc1d-e3f5582cd905', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('235aceaf-7bfc-4656-bc1d-e3f5582cd905', foundational, constitutional_text_allocates_final_authority_to_judiciary).
narrative_ontology:cs_axiom_status(constitutional_text_allocates_final_authority_to_judiciary, holdable).
narrative_ontology:cs_axiom_grounding('235aceaf-7bfc-4656-bc1d-e3f5582cd905', constitutional_text_allocates_final_authority_to_judiciary, conventional).
narrative_ontology:cs_axiom('235aceaf-7bfc-4656-bc1d-e3f5582cd905', foundational, fundamental_rights_require_non_majoritarian_guardian).
narrative_ontology:cs_axiom_status(fundamental_rights_require_non_majoritarian_guardian, holdable).
narrative_ontology:cs_axiom_grounding('235aceaf-7bfc-4656-bc1d-e3f5582cd905', fundamental_rights_require_non_majoritarian_guardian, deontological).
narrative_ontology:cs_reference_frame('235aceaf-7bfc-4656-bc1d-e3f5582cd905', judicial_guardianship_framework).
narrative_ontology:cs_drift_state('235aceaf-7bfc-4656-bc1d-e3f5582cd905', contemporary_rights_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('235aceaf-7bfc-4656-bc1d-e3f5582cd905', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearing_citizens).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, popular_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses final interpretive authority over constitutional text and nullifies legislative acts found incompatible with fundamental rights. Derives institutional prestige, role-definition, and agenda-setting capacity from this authority. Cannot unilaterally exit the constitutional framework but occupies its apex.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, beneficiary).

% Enacts legislation subject to judicial nullification. Bears the direct cost of subordination when statutory provisions are invalidated. Structurally constrained by the constitutional framework it cannot easily amend.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Receive the coordination benefit of constitutional rights guardianship. Can bring claims that trigger judicial nullification of legislation. Depend on the judiciary for the enforcement of their rights against legislative majorities.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearing_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Their collective preferences enacted through legislation can be overridden by judicial interpretation. Present in the electoral process but absent from the interpretive room where constitutional meaning is fixed.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, popular_majorities, excluded,
    moderate, biographical, constrained, national).

% Analyze the legitimacy and effects of judicial supremacy. Provide external corroboration or critique of the constraint's founding problem and current operation without being bound by the institutional roles.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, final arbiter for constitutional meaning, protecting fundamental rights against transient majoritarian pressures and ensuring consistent application of constitutional limits across jurisdictions and time.
% TRANSFER_FUNCTION: Transfers interpretive authority and final decision-making power over constitutional validity from the legislature and the democratic majorities it represents to the judiciary; moves the power to nullify legislation from the legislative sphere to the judicial sphere.
% ABSENT_VOICES: Popular majorities whose legislative preferences are nullified are present in the electoral process but excluded from the interpretive process that overrides their enacted will; legislatures are in the room but structurally subordinated.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the constitutional order would shift toward legislative supremacy or coordinate construction; rights would be enforced through political rather than judicial channels; the legislature would regain final authority; previously voided legislation would become operative and the judiciary would lose its apex interpretive role.
% FOUNDING_PROBLEM: How to prevent legislative majorities from violating fundamental rights and constitutional limits while maintaining a stable constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative constitutional scholars outside the judiciary attest that the founding problem of majoritarian tyranny was genuine at inception. Political scientists and legislative scholars attest that the current arrangement has shifted the problem rather than solving it, creating a counter-majoritarian difficulty.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because democratic authority is systematically transferred from elected bodies to an unelected judiciary; suppression is moderate-high (0.63) because alternatives (legislative override, jurisdiction stripping, constitutional amendment) are legally possible but politically and structurally collapsed in most modern systems. Theater ratio is moderate (0.38) because judicial reasoning performs legitimacy but also delivers functional outcomes. Accessibility collapse (0.60) reflects that while theoretical alternatives exist, the norm of compliance is deeply internalized. Resistance (0.45) captures ongoing political and academic contestation of judicial supremacy. The measurement series share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and rights-bearing citizens experience this constraint as necessary coordination against majoritarian overreach; the legislature and popular majorities experience it as the extraction of their democratic authority. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a beneficiary (low d) due to its institutional authority gain, though it is also agenda-setter. Rights-bearing citizens are beneficiaries (low d) through the rights-protection channel. The legislature and popular majorities are victims (high d) because the constraint extracts democratic decision-making power from them and channels it to courts. Exit options are constrained for all institutional parties because departure requires constitutional rupture or amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) because a genuine coordination functionârights guardianship against majoritarian overrideâis structurally present and valued by rights-bearing citizens. It prevents mislabeling as pure coordination (rope) because the asymmetric subordination of the legislature and the democratic majority is not reciprocal; the judiciary does not bear a comparable cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_contingency,
    'Is judicial supremacy a necessary structural feature of written constitutionalism, or a contingent political settlement that could be arranged differently without constitutional collapse?',
    'Comparative constitutional analysis showing stable democracies with weak-form review or parliamentary sovereignty; historical tracing of when supremacy was asserted versus assumed.',
    'If contingent, the constraint''s extraction is higher (it is a chosen power allocation, not a functional necessity); if necessary, extraction is lower (it is the price of constitutional order).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_contingency, conceptual, 'Whether judicial supremacy is necessary or contingent').

omega_variable(
    rights_majoritarian_tradeoff,
    'Does the constraint genuinely protect minority rights, or does it primarily transfer political contention to a venue less accountable to democratic majorities?',
    'Empirical mapping of judicial nullification outcomes against rights protection metrics and minority status.',
    'If rights protection is weak, the coordination story is cover for extraction; if strong, the coordination function is genuine and justifies the asymmetric cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_majoritarian_tradeoff, empirical, 'Empirical efficacy of rights guardianship').

omega_variable(
    kernel_reading_contest,
    'Does the judicial supremacy reading foreclose coordinate construction in practice, or do the readings co-exist within the same constitutional order?',
    'Analysis of institutional practice: do coordinate branches actually treat judicial interpretations as final, or do they engage in ongoing interpretive contestation (departmentalism, popular constitutionalism)?',
    'If co-existence is the actual practice, this constraint is less extractive than its theory suggests; if finality is real, extraction matches the authored metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Practical finality versus contested co-existence of readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(cons_tr_t80, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 80, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(cons_be_t80, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 80, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement(cons_su_t80, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 80, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel constitutional_interpretive_authority. The judicial_supremacy_reading claims final interpretive authority for courts; siblings claim parliamentary finality (parliamentary_supremacy_reading) and inter-branch dialogue (coordinate_construction_reading). The epsilon values differ because the beneficiary/victim structure and the locus of final authority differ structurally across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
