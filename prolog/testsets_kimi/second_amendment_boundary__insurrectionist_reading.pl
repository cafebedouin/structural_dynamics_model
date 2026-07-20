% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint instantiates the insurrectionist reading of the Second
 *   Amendment boundary kernel: the claim that the right to keep and bear arms
 *   exists to preserve individual and collective capacity for armed
 *   resistance against tyrannical government. Under this reading, individual
 *   possession is instrumentally protected for its potential role in
 *   overthrow, and state disarmament efforts are presumptively treated as
 *   tyranny precursors. The reading expands the protected domain to include
 *   military-grade-capable arms, imposes asymmetric costs on the state
 *   security apparatus and civilians, and is enforced by federal judicial
 *   review that strikes down legislative restrictions. It is one of three
 *   structurally distinct readings of the same textual kernel.
 *
 * KEY AGENTS:
 *   - Armed citizens (organized/powerful beneficiaries): Receive constitutional protection for broad arms possession and claim deterrent legitimacy against tyranny.
 *   - Federal judiciary (institutional agenda setter): Interprets and enforces the Second Amendment boundary to protect individual possession against legislative disarmament.
 *   - State security apparatus (institutional payer): Bears the cost of a constitutionally eroded monopoly on organized violence and elevated lethal risk.
 *   - Civilians at risk (powerless payer): Bear the diffuse physical and psychological costs of widespread militarized arms circulation with blocked democratic remedy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.62).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.55).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '3cd411a1-03b2-4ad0-be7d-b73d72252391').
narrative_ontology:cs_kernel_codification('3cd411a1-03b2-4ad0-be7d-b73d72252391', fixed_text).
narrative_ontology:cs_authority_grounding('3cd411a1-03b2-4ad0-be7d-b73d72252391', lineage).
narrative_ontology:cs_interpretation_layer_present('3cd411a1-03b2-4ad0-be7d-b73d72252391').
narrative_ontology:cs_reading_relation('3cd411a1-03b2-4ad0-be7d-b73d72252391', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cd411a1-03b2-4ad0-be7d-b73d72252391', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('3cd411a1-03b2-4ad0-be7d-b73d72252391', foundational, individual_arms_instrumental_to_resistance).
narrative_ontology:cs_axiom_status(individual_arms_instrumental_to_resistance, holdable).
narrative_ontology:cs_axiom_grounding('3cd411a1-03b2-4ad0-be7d-b73d72252391', individual_arms_instrumental_to_resistance, instrumental).
narrative_ontology:cs_axiom('3cd411a1-03b2-4ad0-be7d-b73d72252391', foundational, state_disarmament_presumption_of_tyranny).
narrative_ontology:cs_axiom_status(state_disarmament_presumption_of_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('3cd411a1-03b2-4ad0-be7d-b73d72252391', state_disarmament_presumption_of_tyranny, empirically_contingent).
narrative_ontology:cs_reference_frame('3cd411a1-03b2-4ad0-be7d-b73d72252391', revolutionary_militia_ideal).
narrative_ontology:cs_drift_state('3cd411a1-03b2-4ad0-be7d-b73d72252391', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3cd411a1-03b2-4ad0-be7d-b73d72252391', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Private citizens who possess firearms under the claimed constitutional protection of an individual right to armed resistance against tyranny. They receive legal shelter for broad weapon possession, including military-grade-capable arms, and claim deterrent legitimacy against government overreach. Their political identity is frequently fused with armed status, and they organize collectively to defend the constitutional boundary.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens, beneficiary,
    organized, biographical, constrained, national).

% Federal courts, especially the Supreme Court, that interpret the Second Amendment to invalidate legislative restrictions on individual weapon possession. In this reading, they enforce a constitutional boundary that treats disarmament efforts as precursors to tyranny and protects individual possession of arms sufficient for potential government overthrow.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Law enforcement and military agencies whose monopoly on organized violence and capacity to secure environments is constitutionally undermined by a recognized private right to possess arms sufficient for government overthrow. They bear elevated lethal risk and operational constraints without democratic recourse to disarmament.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, trapped, national).

% Residents of communities exposed to elevated firearm violence risk, including mass-casualty events, stemming from the widespread legal protection of military-grade-capable arms. They bear the diffuse physical and psychological costs of armed conflict potential while democratic demands for disarmament are judicially blocked.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_at_risk, payer,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, armed_citizens).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a distributed, decentralized capacity for armed collective resistance against government tyranny, operating without centralized command by embedding the deterrent in widespread individual possession.
% TRANSFER_FUNCTION: Transfers legal protection for military-grade-capable individual arms possession to private citizens; transfers the cost of a lost state monopoly on violence to law enforcement and military personnel; transfers the risk of armed conflict and mass violence onto the general civilian population.
% ABSENT_VOICES: Victims of gun violence and communities experiencing concentrated firearm harm are structurally under-weighted in the interpretive framework; their safety demands are treated as secondary to the anti-tyranny function. Democratic majorities favoring comprehensive disarmament find their legislative preferences constitutionally excluded.
% DISAPPEARANCE_RATIONALE: If this constitutional boundary vanished, federal and state legislatures would move to restrict or ban military-grade weapon possession; the state's monopoly on organized violence would reassert through democratic process; and the political identity and social movement organized around armed resistance would reorganize around other constitutional claims or collapse.
% FOUNDING_PROBLEM: Founding-era fear of standing armies and centralized British tyranny; perceived need for state militias and an armed populace as a final check on federal government overreach.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians attest the original concern was federalism and militia-based state defense, not individual insurrection against the elected US government. State security agencies attest the contemporary threat of tyranny does not justify the armed-resistance reading. Gun rights organizations attest the anti-tyranny function remains live. No neutral corroboration exists that the contemporary US government constitutes a tyranny requiring an armed populace.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the reading transfers substantial protective and coercive capacity to private actors while imposing non-consensual risk on the broader public and constraining state security operations. Suppression (0.55) reflects active judicial suppression of legislative alternatives, though political contestation remains live. Theater ratio (0.45) captures the genuinely felt political identity around resistance while recognizing that much of the anti-tyranny discourse operates as performance relative to actual state military capacity. Accessibility collapse (0.40) is partial: comprehensive disarmament alternatives are visible and pursued but increasingly blocked by constitutional doctrine. Resistance (0.70) is high due to sustained political opposition, academic critique, and state-level litigation against the reading's expansion. The measurement series run on a single shared grid to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the armed citizen seat, the constraint is existential liberty and a necessary deterrent against historically demonstrated tyranny; from the state security seat, it is a professionally lethal undermining of legitimate state authority; from the civilian-at-risk seat, it is an unchosen exposure to violence with democratic recourse foreclosed. The judiciary experiences it as a neutral interpretive duty. These divergences are structural and should produce different computed seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens are the declared beneficiaries (low d) because the constraint subsidizes their possession capacity and shields them from disarmament. State security apparatus and civilians at risk are victims (high d) because the constraint extracts their safety and operational authority to feed the protected armed status. The federal judiciary sits at near-symmetric analytical distance (analytical exit), though its institutional role in enforcement gives it a mild directional tilt toward the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling the constraint as a pure snare: the anti-tyranny coordination claim is a genuinely held and historically rooted political theory, not mere cover, so pure extraction is descriptively wrong. It also prevents mislabeling as rope: the asymmetric cost imposition on non-consenting civilians and the state security apparatus is real, structural, and enforced, so pure coordination is also descriptively wrong. The engine should detect this hybridity from the co-presence of beneficiary and victim arrays plus active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insurrectionist_empirical_premise,
    'Does widespread individual possession of military-grade arms actually deter tyranny, or does it increase the risk of civil conflict and state destabilization without corresponding democratic benefit?',
    'Comparative political science analysis of armed societies versus unarmed democracies on metrics of tyranny onset, civil conflict incidence, and democratic stability.',
    'If empirically false, the coordination claim collapses and the constraint reads as pure extraction (snare); if true, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurrectionist_empirical_premise, empirical, 'Whether the insurrectionist reading''s central empirical claim is true.').

omega_variable(
    military_grade_scope,
    'Does the insurrectionist reading''s logic extend to all military-grade weaponry (explosives, crew-served weapons, etc.) or is it artificially bounded at small arms?',
    'Judicial doctrine development: if courts consistently stop at semi-automatic small arms despite the insurrectionist premise, the reading contains an unacknowledged limiting principle that contradicts its own logic.',
    'Unbounded extension would raise extractiveness toward total state incapacitation; bounded application reveals the reading as selectively deployed, suggesting extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_grade_scope, conceptual, 'Logical scope of protected arms under the insurrectionist premise.').

omega_variable(
    kernel_reading_originality,
    'Is the insurrectionist reading a recovery of original constitutional meaning, or a modern ideological projection onto an ambiguous kernel?',
    'Historical-linguistic analysis of founding-era ''well regulated Militia'' discourse and its relationship to individual insurrection against the federal government.',
    'If a modern projection, the cs_structure authority grounding shifts from lineage to extraction; the commitment system stabilizes a constructed reading as if it were textual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_originality, conceptual, 'Whether the insurrectionist reading is original or projected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(seco_tr_t6, second_amendment_boundary__insurrectionist_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(seco_tr_t12, second_amendment_boundary__insurrectionist_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(seco_tr_t18, second_amendment_boundary__insurrectionist_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(seco_tr_t22, second_amendment_boundary__insurrectionist_reading, theater_ratio, 22, 0.45).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(seco_be_t6, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(seco_be_t12, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(seco_be_t18, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(seco_be_t22, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 22, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(seco_su_t6, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(seco_su_t12, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(seco_su_t18, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(seco_su_t22, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 22, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment boundary decomposes into three readings of a single textual kernel: individual_right_reading (pre-existing individual right, prefatory clause non-limiting), insurrectionist_reading (resistance capacity, individual possession instrumental to overthrow), and militia_conditioned_reading (collective defense context, comprehensive regulation permitted). Each reading instantiates a structurally distinct constraint with independent epsilon, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
