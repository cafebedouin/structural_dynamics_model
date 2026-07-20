% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity â Sovereignty Guarantor Reading
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_guarantor_reading of the
 *   eu_council_unanimity kernel. It models the EU Council's unanimity
 *   requirement in sovereignty-sensitive domains as a procedural safeguard
 *   that protects small member states against majoritarian coercion by larger
 *   states. Under this reading, the veto is not a bargaining chip or an
 *   extraction device but an irreducible right of sovereign equality. The
 *   same procedural rule is read by sibling constraints as either a
 *   consensus-building mechanism (diplomatic_capital_reading) or a structural
 *   vulnerability enabling minoritarian extraction (veto_trap_reading). This
 *   story authors the structural data for the sovereignty-guarantor reading
 *   alone, with metrics and claimed type independent of the sibling readings.
 *
 * KEY AGENTS:
 *   - small_member_states: Primary beneficiary (organized/constrained) â gain blocking leverage protecting sovereignty
 *   - large_member_states: Primary payer (powerful/constrained) â bear coordination costs and diluted majoritarian capacity
 *   - european_commission: Institutional observer (institutional/analytical) â proposes policy but cannot override unanimity
 *   - eu_citizens: Excluded voice (organized/constrained) â preferences for collective action subject to single-state veto without direct representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.2).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity â Sovereignty Guarantor Reading").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, 'd3b68fac-2d03-4c3d-b119-b59e070593f7').
narrative_ontology:cs_kernel_codification('d3b68fac-2d03-4c3d-b119-b59e070593f7', formalized).
narrative_ontology:cs_authority_grounding('d3b68fac-2d03-4c3d-b119-b59e070593f7', lineage).
narrative_ontology:cs_interpretation_layer_present('d3b68fac-2d03-4c3d-b119-b59e070593f7').
narrative_ontology:cs_reading_relation('d3b68fac-2d03-4c3d-b119-b59e070593f7', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3b68fac-2d03-4c3d-b119-b59e070593f7', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('d3b68fac-2d03-4c3d-b119-b59e070593f7', foundational, consent_as_irreducible_sovereignty_limit).
narrative_ontology:cs_axiom_status(consent_as_irreducible_sovereignty_limit, holdable).
narrative_ontology:cs_axiom_grounding('d3b68fac-2d03-4c3d-b119-b59e070593f7', consent_as_irreducible_sovereignty_limit, deontological).
narrative_ontology:cs_reference_frame('d3b68fac-2d03-4c3d-b119-b59e070593f7', state_consent_sovereignty_baseline).
narrative_ontology:cs_drift_state('d3b68fac-2d03-4c3d-b119-b59e070593f7', post_lisbon_integration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d3b68fac-2d03-4c3d-b119-b59e070593f7', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each small state holds a veto over Council decisions implicating sovereignty. This ensures they cannot be outvoted by larger states on matters touching core state functions, preserving formal equality and sovereign autonomy within the Union.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    organized, generational, constrained, continental).

% Must secure consent from all states to advance collective action in sensitive domains. While they retain a veto themselves, their ability to translate demographic and economic weight into policy outcomes is structurally diluted by the unanimity requirement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer,
    powerful, generational, constrained, continental).

% Proposes legislation and seeks to build consensus but cannot override a unanimity block. Its policy agenda is contingent on achieving unanimous Council agreement in reserved domains.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, observer,
    institutional, generational, analytical, continental).

% Are not represented in the Council chamber where unanimity is exercised; their preferences for collective action can be blocked by a single national veto without direct democratic recourse.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, excluded,
    organized, generational, constrained, continental).

narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian coercion in a heterogeneous union by ensuring no state can be bound against its will on matters touching sovereignty, thereby maintaining the voluntary basis of integration.
% TRANSFER_FUNCTION: Moves decision-making leverage from aggregate voting power to individual state consent in sovereignty-sensitive domains; small states gain blocking leverage, large states lose the ability to translate demographic weight into majoritarian outcomes.
% ABSENT_VOICES: EU citizens and transnational civil society actors who favor deeper integration or common policies are structurally absent from the Council chamber; their interests can be vetoed by a single national government without direct democratic accountability.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight, small states would lose their structural protection against majoritarian override. The resulting insecurity would likely trigger defensive coalitions, institutional deadlock, or withdrawal pressures, fundamentally rearranging the integration bargain and shifting power toward the largest members.
% FOUNDING_PROBLEM: How to maintain a voluntary union of sovereign states with highly unequal size and power without the largest members dominating the smallest on core state functions.
% FOUNDING_PROBLEM_CORROBORATION: Small member states and neutral academic observers of comparative federalism attest that power asymmetry remains a live structural feature of the EU. Large member states occasionally contest the framing but do not deny the underlying asymmetry; independent federalism scholarship corroborates the risk of majoritarian dominance in a union of unequals.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set to 0.35 (moderate) because achieving unanimity among twenty-seven heterogeneous states imposes genuine coordination costs (delay, package deals, preference aggregation), but these costs are symmetrically borne and do not amount to systematic extraction. Suppression is low (0.20) because the constraint is procedural and self-enforcing; no external enforcement is needed to maintain a state's block, and the alternative (QMV) is not actively suppressed but procedurally excluded. Theater ratio is low (0.10â0.16) because veto use in this reading is substantive sovereignty defense rather than performative obstruction. Accessibility collapse is moderately high (0.65) because once the integration path is understood, the alternative of majoritarian override in sovereignty domains is institutionally closed. Resistance is moderate (0.45) reflecting ongoing federalist and large-state pressure to expand QMV. The temporal series show a modest rise in extractiveness as EU enlargement increased coordination complexity, not as a rise in rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   Small states experience the constraint as protective rope: their seat computes low directionality because the constraint subsidizes their sovereignty. Large states experience it as a coordination cost that dampens their voting power; their seat computes higher directionality (around 0.55â0.65) because the constraint blocks their capacity to translate power into policy outcomes, though they are not declared victims. The European Commission observes from an analytical seat with near-zero directionality. The engine's per-seat classification will therefore diverge: small states see rope, large states may see tangled rope or rope depending on threshold, reflecting the same structural rule from different power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Small member states are declared beneficiaries, placing their derived d near the beneficiary end (â0.15â0.25) because the constraint structurally subsidizes their sovereignty. Large member states are declared payers but not victims; their d derives from their powerful/constrained position, yielding a mid-range value (â0.55) because they bear coordination costs without being extraction targets. No directionality overrides are needed because the structural derivation (beneficiary + power + exit) captures the asymmetry. EU citizens are excluded and receive high d if treated as hypothetical targets, but they are not governed by the constraint directly.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy mislabeling by grounding the constraint in a live founding problem (protecting small states in an asymmetric union) and identifying net beneficiaries among the structurally vulnerable party. Without these declarations, the unanimity rule could be mistaken for a piton (atavistic procedural relic) or a tangled rope (coordination masking extraction). The absence of declared victims, the presence of declared beneficiaries among the powerless, and the low suppression metric jointly prevent the engine from classifying it as a snare. If the veto were systematically traded for side-payments or used to protect domestic rents, the veto_trap reading would supersede this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unanimity_reading_underdetermination,
    'Does the unanimity rule structurally function as a sovereignty guarantor, a diplomatic capital builder, or a veto trap?',
    'Cross-reading empirical comparison of veto use patterns against objective sovereignty threats, negotiation intensity, and side-payment incidence.',
    'If veto use correlates with sovereignty threats, the sovereignty_guarantor reading holds; if with iterative bargaining quality, diplomatic_capital; if with domestic rent-seeking, veto_trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_reading_underdetermination, conceptual, 'Which reading of the unanimity kernel best fits the structural evidence').

omega_variable(
    coordination_cost_vs_extraction,
    'Are the costs large states bear under unanimity mere coordination friction, or do they constitute asymmetric extraction of policy leverage?',
    'Counterfactual policy analysis comparing outcomes under unanimity versus qualified majority voting in the same domains.',
    'If large-state preferences are systematically blocked beyond proportionality, the constraint may be tangled_rope rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction, empirical, 'Whether coordination costs cross into extraction').

omega_variable(
    scope_creep_of_unanimity,
    'Has the expansion of QMV and the contraction of unanimity areas altered the character of the remaining unanimity rule?',
    'Track treaty revisions from the Single European Act to Lisbon and correlate with veto incidence in remaining domains.',
    'If remaining unanimity has become a residual veto over narrow sovereignty cores, its coordination function may have strengthened; if it has expanded into regulatory domains, the sovereignty reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_of_unanimity, empirical, 'Whether treaty drift has purified or contaminated the unanimity rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(eu_c_tr_t50, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement(eu_c_tr_t60, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 60, 0.16).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(eu_c_be_t50, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement(eu_c_be_t60, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 60, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__sovereignty_guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, identity_coordination).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'EU Council unanimity' conflates three structurally distinct constraints: the sovereignty_guarantor_reading (rope, protective veto), the diplomatic_capital_reading (rope/scaffold, consensus-building), and the veto_trap_reading (tangled_rope/snare, minoritarian extraction). Each reading has a distinct beneficiary structure, epsilon profile, and stakeholder directionality. They are linked as a constraint family because they share the same procedural kernel but instantiate different constraints depending on how veto power is exercised and interpreted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
