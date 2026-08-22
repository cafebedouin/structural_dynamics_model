% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity as Sovereignty Guarantor
 *   domain: political/economic/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_guarantor_reading of
 *   the eu_council_unanimity kernel. Under this reading, unanimity in the
 *   European Council (and Council of Ministers for sensitive domains) is a
 *   foundational protection against majoritarian coercion: each member state
 *   must consent to collective action that implicates core sovereignty
 *   (taxation, foreign policy, treaty change, constitutional architecture).
 *   The constraint coordinates by ensuring no state is bound by decisions it
 *   has not authorized; it extracts only the transaction costs of
 *   consensus-building (delay, negotiation overhead, suboptimal compromise),
 *   not systematic rents. Veto use is framed as legitimate rights-exercise,
 *   not obstruction. The beneficiary set is all states, with small and medium
 *   states as primary beneficiaries because they lack alternative leverage
 *   against majoritarian coalitions of large states. The veto_trap_reading
 *   and diplomatic_capital_reading are sibling constraints — distinct
 *   structural claims about the same institutional rule — not alternative
 *   perspectives on this constraint.
 *
 * KEY AGENTS:
 *   - small_member_states: Primary beneficiary (moderate/constrained) — protected from majoritarian imposition on sovereignty
 *   - medium_member_states: Beneficiary (moderate/constrained) — same structural position as small states but with greater diplomatic capacity
 *   - large_member_states: Payer/coordinator (powerful/mobile) — bear coordination costs of consensus-building, constrained by need for unanimity
 *   - european_commission: Agenda setter (institutional/arbitrage) — proposes legislation but cannot override Council unanimity requirement
 *   - european_parliament: Excluded observer (institutional/analytical) — co-legislator in ordinary procedure but no formal role in unanimity domains
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.25).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity as Sovereignty Guarantor").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "political/economic/institutional").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '44c8c792-3596-46c8-a6b9-6e1c0a468d77').
narrative_ontology:cs_kernel_codification('44c8c792-3596-46c8-a6b9-6e1c0a468d77', formalized).
narrative_ontology:cs_authority_grounding('44c8c792-3596-46c8-a6b9-6e1c0a468d77', lineage).
narrative_ontology:cs_interpretation_layer_present('44c8c792-3596-46c8-a6b9-6e1c0a468d77').
narrative_ontology:cs_reading_relation('44c8c792-3596-46c8-a6b9-6e1c0a468d77', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('44c8c792-3596-46c8-a6b9-6e1c0a468d77', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('44c8c792-3596-46c8-a6b9-6e1c0a468d77', foundational, sovereign_consent_required_for_sovereignty_implicated_action).
narrative_ontology:cs_axiom_status(sovereign_consent_required_for_sovereignty_implicated_action, holdable).
narrative_ontology:cs_axiom_grounding('44c8c792-3596-46c8-a6b9-6e1c0a468d77', sovereign_consent_required_for_sovereignty_implicated_action, deontological).
narrative_ontology:cs_axiom('44c8c792-3596-46c8-a6b9-6e1c0a468d77', secondary, unanimity_as_non_derogable_structural_guarantee).
narrative_ontology:cs_axiom_status(unanimity_as_non_derogable_structural_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('44c8c792-3596-46c8-a6b9-6e1c0a468d77', unanimity_as_non_derogable_structural_guarantee, conventional).
narrative_ontology:cs_reference_frame('44c8c792-3596-46c8-a6b9-6e1c0a468d77', westphalian_sovereign_equality_in_union).
narrative_ontology:cs_drift_state('44c8c792-3596-46c8-a6b9-6e1c0a468d77', post_lisbon_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('44c8c792-3596-46c8-a6b9-6e1c0a468d77', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, medium_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, consent_based_governance).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, subsidiarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with populations under 10M (e.g., Malta, Luxembourg, Estonia, Latvia, Slovenia). Unanimity gives them a veto on sovereignty-implicated domains (tax, foreign policy, treaty change) that they would lose under QMV. They lack the economic weight or diplomatic networks to shape outcomes through influence alone. Exit via Article 50 is legally possible but politically and economically prohibitive. They use the veto sparingly but credibly — the threat structures negotiations even when not exercised.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% States with populations 10-30M (e.g., Netherlands, Belgium, Sweden, Austria, Portugal, Greece, Ireland). Same structural protection as small states but with greater diplomatic capacity to build coalitions and shape agendas. They benefit from unanimity but can sometimes achieve aims through QMV coalition-building. More likely to exercise veto as strategic leverage than pure defense. Exit cost similar to small states.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, medium_member_states, beneficiary,
    moderate, generational, constrained, continental).

% France, Germany, Italy, Spain, Poland. Bear the coordination costs of unanimity: prolonged negotiations, watered-down proposals, inability to advance integration in sovereignty domains. They have alternative leverage (economic weight, bilateral channels, intergovernmental formats) and could pursue integration outside EU treaties (mobile exit). But they also benefit from the Union's legal unity and single market — unanimity protects the framework they depend on. Secondary beneficiary role reflects this dual position.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary).

% Proposes legislation and guards the treaties. In unanimity domains, it must build consensus before proposing; its agenda-setting power is constrained by the veto threat. It can shift work to QMV domains (arbitrage) and uses 'package deals' to link unanimity and QMV items. Does not collect extraction from unanimity — its interest is legislative throughput and treaty fidelity.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Co-legislator under ordinary legislative procedure (QMV), but has only consultative role in unanimity domains (tax, foreign policy, treaty change). Would object to marginalization but has no formal blocking power. Its influence is informal: shaping narratives, building public pressure, threatening institutional conflict. Exit is constrained — it is an EU institution with no external platform.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_parliament, excluded,
    institutional, biographical, constrained, continental).

% Sees the full structure across all three readings. Observes that veto use is rare but structuring; that small states do not systematically extract side-payments; that coordination costs are real but not extractive; that the constraint's persistence depends on treaty-embedded self-reinforcement (unanimity required to change unanimity).
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian coercion in a union of formally sovereign states by requiring consent for collective action that implicates core sovereignty (taxation, foreign policy, treaty change, constitutional architecture). Solves the credible commitment problem: small states join and remain only if they cannot be outvoted on existential domains.
% TRANSFER_FUNCTION: Moves policy autonomy from large-state coalitions to individual member states — each state retains a veto on sovereignty-implicated decisions. The 'transfer' is negative: it blocks the transfer of authority that QMV would effect. Coordination costs (delay, compromise) are distributed across all participants; no party receives a concentrated gain.
% ABSENT_VOICES: Citizens of member states — especially in large states — who would prefer faster integration and democratic majoritarianism but have no direct vote on Council voting rules. Subnational regions (Catalonia, Scotland, Flanders) that might seek distinct representation but are excluded by the state-centric design. Future generations who inherit the coordination costs of gridlock.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight, QMV would immediately apply to tax, foreign policy, and treaty change. Large-state coalitions could impose fiscal harmonization, common foreign policy positions, and treaty revisions without small-state consent. Small states would lose their structural protection; the Union's character would shift from a union of sovereign states to a majoritarian federation. Several small states might trigger Article 50. The institutional equilibrium would fundamentally rearrange.
% FOUNDING_PROBLEM: The founding problem (1957-1992, from Treaty of Rome through Maastricht) was creating a European union that respects the sovereign equality of member states while enabling collective action. Unanimity was the mechanism that made the original Six — and every subsequent accession cohort — willing to pool sovereignty: no state would be bound on core sovereign domains without its consent. The problem was credible commitment to non-coercion.
% FOUNDING_PROBLEM_CORROBORATION: Small-state diplomatic corps and constitutional courts (e.g., German Constitutional Court's Lisbon judgment, Irish Supreme Court's Crotty judgment) attest the problem remains live: majoritarian coercion is a structural risk in any union with power asymmetries. Large-state governments and federalist scholars attest the problem is substantially solved: QMV with qualified majorities and democratic legitimacy (EP co-decision) provides sufficient protection. Treaty law scholars (Craig, de Búrca, Klamert) corroborate from outside the beneficiary set that the unanimity rule's protective function is actively invoked in practice (vetoes on tax directives, foreign policy positions, treaty change ratification), not atrophied.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.35) because the constraint imposes real coordination costs: negotiation time, lowest-common-denominator outcomes, and policy gridlock on sovereignty-implicated domains. These are genuine coordination overhead, not extraction — no party systematically collects rents from the unanimity requirement. Suppression is low (0.25) because the constraint does not coerce compliance; it requires consent. States that object are not suppressed — their objection blocks the action. Theater ratio is low (0.15) because the unanimity rule is genuinely invoked and respected, not performatively maintained. Accessibility collapse is moderate (0.45): alternatives (qualified majority voting) exist for many domains but are structurally blocked for sovereignty-implicated areas by treaty design. Resistance is moderate (0.40): large states periodically push for QMV expansion, but treaty change itself requires unanimity, creating a self-reinforcing structure.
 *
 * PERSPECTIVAL GAP:
 *   From the small/medium state seat, the constraint is a protective mountain — without it, they would be subject to majoritarian coercion. From the large state seat, it is a rope with rising coordination costs as the Union expands — they pay in delayed action and compromised policy. From the Commission seat, it is a scaffold-like constraint: a coordination mechanism that complicates the legislative agenda but legitimizes the Union's authority. The engine computes these divergences from the structural data; this reading's claim (rope) reflects the coordination-cost-weighted average.
 *
 * DIRECTIONALITY LOGIC:
 *   Small and medium states are declared beneficiaries because the unanimity rule structurally protects their sovereignty from majoritarian coalitions — they gain the right to consent/block. Large states are payers in the coordination sense: they bear the cost of negotiating consensus rather than imposing majority will, but they also benefit from the Union's cohesion (secondary beneficiary role). The Commission is agenda_setter: it proposes but cannot force adoption. The Parliament is excluded from unanimity domains — it would object to its marginalization but has no formal standing there. Exit options: small/medium states are constrained (treaty exit is Article 50, politically costly); large states are mobile (they could pursue intergovernmental alternatives outside EU framework); Commission has arbitrage (can shift agenda to QMV domains).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing majoritarian coercion in a union of sovereign states — remains live (founding_problem_status: contested). Large states argue the problem is solved by democratic legitimacy of QMV; small states argue the threat persists. Corroboration comes from outside beneficiaries: treaty law scholars, small-state diplomatic corps, and constitutional courts have attested the continuing necessity. The constraint is not mandatrophic: its protective function is actively invoked (vetoes on tax, foreign policy, treaty change), not atrophied. Theatricality is low because the constraint's operation is substantive, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement,
    'Is EU Council unanimity a sovereignty guarantor (this reading), a diplomatic capital mechanism, or a veto trap enabling minoritarian extraction?',
    'Track veto usage patterns: if vetoes correlate with sovereignty-implicated domains and small-state protection, this reading holds; if vetoes cluster on narrow economic interests with side-payments, veto_trap_reading gains support; if vetoes trigger iterative negotiation that improves policy quality, diplomatic_capital_reading gains support.',
    'Determines whether the constraint''s ε is moderate coordination cost (this reading ~0.35), higher with extractive component (veto_trap_reading ~0.6), or lower with legitimacy dividend (diplomatic_capital_reading ~0.25). Classification shifts between rope, tangled_rope, and mountain-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement, conceptual, 'Structural disagreement across the three readings of the eu_council_unanimity kernel.').

omega_variable(
    coordination_cost_vs_extraction_boundary,
    'Are the transaction costs of unanimity (delay, lowest-common-denominator outcomes) inherent coordination overhead or evidence of extractive structure?',
    'Compare policy domains: sovereignty-implicated areas (tax, foreign policy, treaty change) should show high coordination cost but low extraction if this reading is correct; economic regulation domains should show different patterns if veto_trap_reading captures the structure.',
    'If coordination costs are separable from extraction, this reading''s ε remains moderate; if inseparable, the constraint may be tangled_rope with coordination function masking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction_boundary, empirical, 'Whether the costs of unanimity are genuine coordination overhead or contain an extractive component.').

omega_variable(
    small_state_coalition_power,
    'Do small states actually exercise collective blocking power, or is their ''protection'' theoretical given great power agenda-setting?',
    'Analyze voting records and negotiation outcomes: do small states successfully block or modify sovereignty-implicated proposals, or do they concede under great power pressure?',
    'If small states effectively exercise veto, beneficiary declaration is validated and this reading''s rope classification holds; if vetoes are theoretical, the constraint may be snare or piton for small states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_state_coalition_power, empirical, 'Whether the declared beneficiaries (small/medium states) actually benefit from the unanimity rule in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_council_unanimity_sg_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_tr_t0, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_tr_t10, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_tr_t20, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_tr_t30, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_tr_t40, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_tr_t40, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_tr_t50, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(eu_council_unanimity_sg_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_be_t0, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_be_t10, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_be_t20, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_be_t30, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_be_t40, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_be_t40, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_be_t50, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_council_unanimity_sg_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_su_t0, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_su_t10, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_su_t10, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_su_t20, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_su_t20, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_su_t30, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_su_t40, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_su_t40, observed).
narrative_ontology:measurement(eu_council_unanimity_sg_su_t50, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement_basis(eu_council_unanimity_sg_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_qualified_majority_voting_expansion).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_treaty_change_procedure).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_enhanced_cooperation_mechanism).

% DUAL FORMULATION NOTE:
% Part of eu_council_unanimity constraint family (kernel decomposition). This reading (sovereignty_guarantor_reading) asserts moderate coordination cost, no extraction. veto_trap_reading asserts higher ε with extractive component. diplomatic_capital_reading asserts lower ε with legitimacy dividend. The three readings share the same institutional rule but instantiate structurally distinct constraints with different ε, beneficiary/victim structures, and types. Linked via network.affects_constraints to downstream constraints that depend on unanimity domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, powerful, 0.55).
constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, moderate, 0.25).
constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
