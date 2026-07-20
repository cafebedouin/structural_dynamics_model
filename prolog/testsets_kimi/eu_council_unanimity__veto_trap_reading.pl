% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity â Veto Trap Reading
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The Council of the European Union requires unanimous consent among member
 *   states for decisions in sensitive policy areas such as taxation, foreign
 *   policy, and enlargement. This constraint story instantiates the veto-trap
 *   reading of the eu_council_unanimity kernel: the unanimity rule is read
 *   not as sovereign protection but as a structural vulnerability that
 *   enables single states to extract concessions from a coalition majority
 *   through credible blocking threats. The reading treats the veto not as a
 *   legitimate defensive shield but as an offensive leverage mechanism that
 *   systematically transfers policy value from the majority to the holdout.
 *   The claim is tangled_rope because the rule still coordinates (it forces
 *   negotiation and prevents unilateral imposition), but the extraction is
 *   asymmetric and actively enforced: the blocking state captures opt-outs
 *   and side-payments while the majority bears the cost of diluted or delayed
 *   collective action.
 *
 * KEY AGENTS:
 *   - blocking_state: Minority member state exercising veto threat to extract concessions (institutional/constrained)
 *   - coalition_majority: Majority of member states forced to concede to unblock policy (institutional/constrained)
 *   - european_commission: Proposes policy but excluded from final veto point (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.78).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.62).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity â Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "political/institutional").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'e08c4a27-6d52-4777-957f-41e8d127042f').
narrative_ontology:cs_kernel_codification('e08c4a27-6d52-4777-957f-41e8d127042f', formalized).
narrative_ontology:cs_authority_grounding('e08c4a27-6d52-4777-957f-41e8d127042f', lineage).
narrative_ontology:cs_interpretation_layer_present('e08c4a27-6d52-4777-957f-41e8d127042f').
narrative_ontology:cs_reading_relation('e08c4a27-6d52-4777-957f-41e8d127042f', eu_council_unanimity__sovereignty_guarantor_reading, influences).
narrative_ontology:cs_reading_relation('e08c4a27-6d52-4777-957f-41e8d127042f', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('e08c4a27-6d52-4777-957f-41e8d127042f', foundational, veto_as_extraction_device).
narrative_ontology:cs_axiom_status(veto_as_extraction_device, holdable).
narrative_ontology:cs_axiom_grounding('e08c4a27-6d52-4777-957f-41e8d127042f', veto_as_extraction_device, empirically_contingent).
narrative_ontology:cs_axiom('e08c4a27-6d52-4777-957f-41e8d127042f', foundational, unanimity_masking_coercion).
narrative_ontology:cs_axiom_status(unanimity_masking_coercion, holdable).
narrative_ontology:cs_axiom_grounding('e08c4a27-6d52-4777-957f-41e8d127042f', unanimity_masking_coercion, deontological).
narrative_ontology:cs_reference_frame('e08c4a27-6d52-4777-957f-41e8d127042f', unanimous_consent_baseline).
narrative_ontology:cs_drift_state('e08c4a27-6d52-4777-957f-41e8d127042f', post_lisbon_enlargement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e08c4a27-6d52-4777-957f-41e8d127042f', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A single member state that credibly threatens to veto Council decisions in areas of unanimity, extracting policy opt-outs, budget rebates, regulatory dilution, or side-payments from the majority in exchange for lifting the block. It benefits from the constraint because the rule gives its refusal decisive weight it would lack under majority voting.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_state, beneficiary,
    institutional, generational, constrained, continental).

% The larger group of member states whose preferred policy direction is blocked unless they concede to the blocking state's demands. They bear the cost of delayed, weakened, or fragmented collective action and must transfer value through explicit or implicit concessions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority, payer,
    institutional, generational, constrained, continental).

% Proposes legislation and advances the general EU interest, but in unanimity areas it lacks a vote and cannot override a blocking minority. Its policy objectives are frequently held hostage to intergovernmental bargaining, yet it has no formal seat at the final veto point.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_commission, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Requires unanimous consent among sovereign member states for collective decisions in sensitive policy areas, ensuring that no state is bound against its will and that all must agree before moving forward.
% TRANSFER_FUNCTION: Moves policy concessions, financial side-payments, regulatory opt-outs, and diluted legislative ambition from the coalition majority to the blocking minority in exchange for lifting the credible veto threat.
% ABSENT_VOICES: The European Parliament and affected EU citizens are structurally excluded from the Council veto point; they would object to minoritarian extraction but have no vote. Majority coalition members are physically present but their preference is suppressed by the unanimity rule itself.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement disappeared overnight, majority coalitions would impose their preferred policies in currently blocked areas, the extraction of concessions by holdout states would cease, and the EU would shift toward majoritarian or qualified-majority governance with a corresponding redistribution of bargaining power and policy outputs.
% FOUNDING_PROBLEM: Preventing majoritarian coercion of smaller or sovereignty-sensitive member states in a heterogeneous union of sovereign nations, ensuring that collective action implicating core national interests proceeds only with full consent.
% FOUNDING_PROBLEM_CORROBORATION: Small member states and federalism scholars attest the problem is still live, citing sovereignty risks. Majority coalition governments and comparative political economists attest the problem has been subverted: unanimity now functions as a mechanism for any single state to extract rents rather than protecting vulnerable minorities, and reform is blocked because abolishing unanimity itself requires unanimity. Peer-reviewed bargaining studies from outside the benefiting parties document systematic minoritarian extraction.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because empirical evidence shows systematic minoritarian extraction in unanimity areas (budget rebates, opt-outs, policy dilution). Suppression is 0.62 because the unanimity rule suppresses majority alternatives by treaty design; majority states cannot override the blocker without treaty change, which is itself blocked by unanimity. Theater ratio is 0.40 because the public justification remains sovereign equality and consensus, while the operative function is increasingly extractive bargaining. Accessibility collapse is 0.65 because the alternative (qualified majority voting) is institutionally visible but locked out by the same unanimity rule that would need to be abolished. Resistance is 0.55 because majority states and the Commission constantly push for QMV expansion, generating sustained but unsuccessful resistance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (the Council procedure itself) experiences the constraint as a necessary sovereignty-sensitive coordination device. The beneficiary seat (blocking state) experiences it as legitimate defensive leverage. The payer seat (coalition majority) experiences the identical procedure as coercive extraction. The engine computes this divergence from the structural asymmetry in declared roles combined with the fact that all states share equally constrained exit options (EU membership is costly to exit), meaning the asymmetry must come from the beneficiary/victim structure rather than mobility differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   Blocking states are declared beneficiaries (low directionality) because the constraint channels concessions and side-payments to them. Coalition majority states are declared victims/payers (high directionality) because they bear the cost of extracted transfers through weakened policy. The European Commission sits outside the beneficiary/victim axis but is structurally subject to the constraint's veto dynamic; its exclusion from the veto point gives it a moderate-high directionality as its agenda is overridden by extraction, though it is not the primary payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The unanimity rule was founded to solve the problem of majoritarian coercion in a union of sovereign states. Under the veto-trap reading, that founding problem has been subverted: the mechanism now protects any single state's ability to extract rents rather than protecting vulnerable minorities from domination. Classifying it as tangled_rope (rather than snare) preserves the genuine coordination function that still exists â the rule does force negotiation and prevents purely unilateral action â while registering the asymmetric extraction that rides on the same structure. A snare classification would erase the coordination component entirely; a rope classification would erase the extraction. Tangled rope is the only category that captures the hybrid reality without mandatrophy mislabeling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_trap_reading_validity,
    'Does the veto trap reading capture the dominant empirical use of unanimity, or is it an edge-case pattern blown out of proportion by high-profile disputes?',
    'Systematic quantitative analysis of Council negotiating records across all unanimity areas to measure the baseline rate of minoritarian extraction versus consensual agreement.',
    'If extraction is dominant, the high Îµ and tangled_rope classification are warranted; if rare, the constraint might compute closer to rope with occasional snare incidents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_trap_reading_validity, empirical, 'Whether minoritarian extraction is routine or episodic under unanimity.').

omega_variable(
    coordination_extraction_separability,
    'Can the sovereignty-protection coordination function of unanimity be structurally separated from the minoritarian extraction mechanism, or are they inseparable features of the same rule?',
    'Comparative institutional analysis of decision-rules in other international organizations and federal systems that protect minorities without granting single-actor veto power.',
    'If separable, the tangled_rope classification holds and reform is conceivable; if inseparable, the extraction may be an unavoidable cost of the coordination, raising the floor estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components are structurally separable.').

omega_variable(
    reform_paradox_lock_in,
    'Is the inability to abolish unanimity without unanimous consent a logical paradox that permanently locks in the extraction mechanism?',
    'Historical comparison of treaty revision episodes to identify whether break-out clauses, passerelle provisions, or political crises have ever circumvented the unanimity lock.',
    'If the lock is absolute, the constraint''s persistence is structurally guaranteed and resistance is futile within the rules; if bypassable, the accessibility_collapse metric may be overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_paradox_lock_in, conceptual, 'Whether unanimity reform is institutionally self-locking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_ut_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eu_ut_tr_t5, eu_council_unanimity__veto_trap_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(eu_ut_tr_t10, eu_council_unanimity__veto_trap_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(eu_ut_tr_t15, eu_council_unanimity__veto_trap_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(eu_ut_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(eu_ut_tr_t25, eu_council_unanimity__veto_trap_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(eu_ut_tr_t30, eu_council_unanimity__veto_trap_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(eu_ut_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(eu_ut_be_t5, eu_council_unanimity__veto_trap_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(eu_ut_be_t10, eu_council_unanimity__veto_trap_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(eu_ut_be_t15, eu_council_unanimity__veto_trap_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(eu_ut_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(eu_ut_be_t25, eu_council_unanimity__veto_trap_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(eu_ut_be_t30, eu_council_unanimity__veto_trap_reading, base_extractiveness, 30, 0.78).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__veto_trap_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint is the veto-trap reading of the EU Council unanimity kernel, which decomposes into structurally distinct claims: sovereignty guarantor (protective), diplomatic capital (legitimacy-building coordination), and veto trap (extractive). Each reading has a distinct epsilon, stakeholder geometry, and normative evaluation. They are linked as a constraint family because they share the same institutional kernel (the unanimity rule) but instantiate different constraints depending on which structural claim is evaluated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
