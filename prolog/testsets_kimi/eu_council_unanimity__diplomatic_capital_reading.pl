% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity â Diplomatic Capital Reading
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the diplomatic capital reading of the
 *   EU Council unanimity kernel. It treats the unanimity requirement not as a
 *   veto trap or a rigid sovereignty guarantee, but as a coordination
 *   mechanism whose friction generates legitimacy. The claim is that
 *   iterative negotiation under unanimity produces buy-in that reduces
 *   downstream defection and makes policy more durable than QMV impositions
 *   would be. The story is authored as a rope: low extractiveness, low
 *   suppression, and no fixed victim structure, because the costs and
 *   benefits of the arrangement are broadly shared among the negotiating
 *   parties.
 *
 * KEY AGENTS:
 *   - eu_member_states: Primary agenda-setters and beneficiaries (institutional/constrained) â they enforce the unanimity rule through their vetoes and collect legitimacy from it.
 *   - european_commission: Institutional observer (institutional/constrained) â operates within the unanimity constraint when proposing legislation.
 *   - eu_citizens: Diffuse beneficiaries (organized/constrained) â receive the legitimacy and stability benefits of unanimous decisions without direct participation.
 *   - qmv_advocates: Excluded voices (moderate/mobile) â argue for majority voting but are structurally outside the Council voting logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.22).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity â Diplomatic Capital Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '46171311-75a6-464f-b7d1-db4b80411963').
narrative_ontology:cs_kernel_codification('46171311-75a6-464f-b7d1-db4b80411963', formalized).
narrative_ontology:cs_authority_grounding('46171311-75a6-464f-b7d1-db4b80411963', lineage).
narrative_ontology:cs_interpretation_layer_present('46171311-75a6-464f-b7d1-db4b80411963').
narrative_ontology:cs_reading_relation('46171311-75a6-464f-b7d1-db4b80411963', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('46171311-75a6-464f-b7d1-db4b80411963', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('46171311-75a6-464f-b7d1-db4b80411963', foundational, universal_consent_generates_legitimacy).
narrative_ontology:cs_axiom_status(universal_consent_generates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('46171311-75a6-464f-b7d1-db4b80411963', universal_consent_generates_legitimacy, instrumental).
narrative_ontology:cs_axiom('46171311-75a6-464f-b7d1-db4b80411963', secondary, iterative_negotiation_produces_durability).
narrative_ontology:cs_axiom_status(iterative_negotiation_produces_durability, holdable).
narrative_ontology:cs_axiom_grounding('46171311-75a6-464f-b7d1-db4b80411963', iterative_negotiation_produces_durability, empirically_contingent).
narrative_ontology:cs_reference_frame('46171311-75a6-464f-b7d1-db4b80411963', consensus_based_legitimacy).
narrative_ontology:cs_drift_state('46171311-75a6-464f-b7d1-db4b80411963', post_enlargement_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('46171311-75a6-464f-b7d1-db4b80411963', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, intergovernmental_consensus_legitimacy).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, sovereign_equality_in_integration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the Council of the EU where they must reach unanimous agreement in designated sensitive policy areas. They bear the cost of prolonged negotiation and complex package deals, but in return no decision is imposed without their explicit consent, and policies emerging from unanimity carry enhanced domestic legitimacy.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, beneficiary).

% Drafts legislative proposals that must ultimately secure unanimous Council agreement in sensitive domains. Must anticipate veto-sensitive red lines and engage in pre-negotiation with member states, which constrains the range of initiatives it can formally table.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, observer,
    institutional, generational, constrained, continental).

% Are the ultimate subjects of EU legislation. Unanimous Council decisions signal broad sovereign consent, which tends to produce more stable and uniformly implemented policies across member states, though citizens have no direct access to closed Council negotiations.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    organized, generational, constrained, continental).

% Argue that qualified majority voting would accelerate integration and improve democratic responsiveness. They are structurally excluded from the Council's treaty-bound voting procedures and from the intergovernmental negotiations where the unanimity rule is defended.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, qmv_advocates, excluded,
    moderate, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of securing sovereign state consent in politically sensitive integration domains by requiring iterative negotiation until all parties agree, thereby preventing unilateral imposition.
% TRANSFER_FUNCTION: Transfers time and diplomatic effort from member states into prolonged negotiation; transfers policy durability and perceived legitimacy from the Council to the domestic implementation stage.
% ABSENT_VOICES: Federalist reformers, qualified-majority expansion advocates, and some Members of the European Parliament who view unanimity as democratically inefficient. They are present in public and parliamentary debate but excluded from the Council's intergovernmental voting logic.
% DISAPPEARANCE_RATIONALE: Removing the unanimity requirement would shift Council dynamics toward QMV coalition-building, eliminate the formal veto leverage of individual member states, reduce the bargaining incentive for package deals, and strip decisions in sensitive areas of the legitimacy shield derived from universal consent.
% FOUNDING_PROBLEM: How to construct a durable European union among sovereign states with divergent interests without coercing unwilling parties into compliance on matters implicating core sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and Council secretariat officials attest to the ongoing need for sovereign consent mechanisms. Comparative federalism scholars and EU constitutional lawyers outside the immediate beneficiary set corroborate that the tension between integration and sovereignty remains structurally unresolved.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the 'cost' of unanimity is distributed negotiation friction rather than concentrated rent extraction. Suppression is low (0.15) because alternatives such as QMV are openly debated and used in other EU domains; within unanimity areas, the alternative is structurally inaccessible only at the treaty-revision threshold. Theater ratio is low (0.10) because the negotiation function is genuine, not performative. Accessibility collapse is moderate (0.40) because while the rule is constitutive for the domains it governs, the broader EU system provides visible alternatives. Resistance is low (0.20) because the rule is treaty-embedded and broadly accepted, though periodically criticized by large states and federalists.
 *
 * PERSPECTIVAL GAP:
 *   From the member-state seat, unanimity appears as a rope: a jointly maintained procedure that distributes costs and benefits symmetrically. From the excluded QMV-advocate seat, the same structure can appear as a piton or snare â an inefficient relic that blocks integration. The engine computes this divergence from the structural data (beneficiary vs. excluded roles, institutional vs. moderate power, constrained vs. mobile exit) rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states are declared beneficiaries with constrained exit and institutional power, placing them near the beneficiary end of directionality (low d, low effective extraction). Citizens are diffuse beneficiaries with constrained exit and organized power, also near the beneficiary end. The Commission is structurally close to symmetric: it neither collects the coordination gain nor pays extraction, but its proposal autonomy is bounded by the rule. QMV advocates are excluded and mobile, sitting outside the constraint's directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The diplomatic capital reading prevents mislabeling unanimity as pure extraction (snare) by foregrounding the non-excludable legitimacy benefit it produces. It also prevents mislabeling it as a mountain by refusing to treat the rule as inevitable or naturally emergent: it is a constructed procedural choice that serves a coordination function. If the legitimacy payoff were empirically absent and the rule persisted purely to enable blocking coalitions, the reading would fail and the veto-trap reading would be more appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_payoff_empirical,
    'Do unanimous Council decisions actually exhibit higher domestic compliance and durability than QMV decisions in comparable policy domains?',
    'Comparative empirical study of implementation deficits across unanimity-governed and QMV-governed EU directives, controlling for policy salience and member-state preference heterogeneity.',
    'If no durability or compliance advantage is found, the coordination justification for the diplomatic capital reading weakens, pushing classification toward piton or veto-trap dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_payoff_empirical, empirical, 'Whether the claimed legitimacy payoff of unanimity is empirically realized.').

omega_variable(
    coordination_vs_sovereignty_separability,
    'Is the consensus-building function of unanimity structurally separable from its role as a sovereignty guarantee for member states?',
    'Comparative institutional analysis: identify decision-rules in other international organizations that achieve consensus without granting individual veto rights, and assess whether they produce comparable legitimacy outcomes.',
    'If inseparable, the diplomatic capital reading collapses into the sovereignty guarantor reading; if separable, the rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_sovereignty_separability, conceptual, 'Whether the coordination and sovereignty-protection functions of unanimity are distinct or conjoined.').

omega_variable(
    veto_masking_dynamics,
    'To what extent does the diplomatic capital framing (consensus language) obscure actual veto-threat extraction dynamics described in the veto-trap reading?',
    'Qualitative process-tracing of Council negotiations in unanimity areas: code whether apparent ''consensus'' outcomes were reached under credible veto threats that extracted side-payments or policy concessions.',
    'If veto masking is pervasive, the low theater ratio and low extractiveness scores in this reading may understate the effective extraction, suggesting a reclassification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_masking_dynamics, empirical, 'Whether consensus language hides extraction via veto threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(eu_c_tr_t50, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(eu_c_be_t50, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 50, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, veto_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel eu_council_unanimity. The kernel decomposes into at least three structurally distinct constraints: diplomatic_capital_reading (coordination/legitimacy frame), sovereignty_guarantor_reading (sovereignty-protection frame), and veto_trap_reading (extraction frame). Each has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
