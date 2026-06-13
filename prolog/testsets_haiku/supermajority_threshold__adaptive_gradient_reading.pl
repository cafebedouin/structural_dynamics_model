% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold (Adaptive Gradient Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   In constitutional systems with supermajority amendment thresholds, this
 *   reading frames the threshold as a tool whose legitimacy depends on
 *   calibration to the polity's actual consensus formation rate and the
 *   reversibility costs of different amendment classes. Unlike the
 *   consensus-safeguard reading (which treats the threshold as intrinsically
 *   protective of democratic integrity) or the minoritarian-veto reading
 *   (which treats it as structural empowerment of blocking minorities), the
 *   adaptive-gradient reading holds that legitimacy is empirical: too low a
 *   threshold produces instability (the coordination problem re-emerges as
 *   rope tension when majorities can reverse constitutional commitments
 *   faster than social costs reset); too high a threshold produces
 *   ossification (a genuine snare where the requirement for consensus blocks
 *   even reforms that do reflect sustained majorities). The constraint's
 *   extractiveness is moderate because calibration can align the threshold to
 *   actual consensus dynamics; suppression is low because the mechanism
 *   itself is transparent; theater increases over time as the reading becomes
 *   dominant in constitutional practice, displacing the intrinsic-value
 *   framings.
 *
 * KEY AGENTS:
 *   - constitutional_convention_authority: The agenda-setting seat that interprets the kernel and sets the threshold. In the adaptive reading, authority is grounded in measuring consensus dynamics, not in defending a particular threshold principle.
 *   - institutional_stability_constituency: Genuine beneficiaries of a supermajority requirement, but only when calibrated. If the threshold is set too high (misaligned with actual consensus formation), they are harmed by ossification — a structural distinction from the minoritarian-veto reading, where even high thresholds benefit the blocking minorities.
 *   - majoritarian_reform_constituencies: Pay the cost of supermajority requirements, but the magnitude depends on calibration. This reading acknowledges variable extraction: at an optimal threshold they pay for coordination; at a too-high threshold they pay for pure ossification.
 *   - empirical_consensus_researchers: Central to this reading's legitimacy claim. The researchers provide the evidence that would answer whether a given threshold matches the polity's actual consensus formation rate.
 *   - historical_privilege_holders: Benefit when supermajority requirements prevent constitutional changes disadvantaging them, but this benefit is not attributed to the threshold's intrinsic virtue (as in consensus-safeguard) or to structural empowerment of minorities (as in minoritarian-veto), but to misalignment between the threshold and actual consensus dynamics — a cost-side distinction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.38).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.22).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '2375cace-d12b-4674-be06-632a9340ba80').
narrative_ontology:cs_kernel_codification('2375cace-d12b-4674-be06-632a9340ba80', fixed_text).
narrative_ontology:cs_authority_grounding('2375cace-d12b-4674-be06-632a9340ba80', lineage).
narrative_ontology:cs_interpretation_layer_present('2375cace-d12b-4674-be06-632a9340ba80').
narrative_ontology:cs_reading_relation('2375cace-d12b-4674-be06-632a9340ba80', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('2375cace-d12b-4674-be06-632a9340ba80', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('2375cace-d12b-4674-be06-632a9340ba80', foundational, threshold_is_tool_not_principle).
narrative_ontology:cs_axiom_status(threshold_is_tool_not_principle, holdable).
narrative_ontology:cs_axiom_grounding('2375cace-d12b-4674-be06-632a9340ba80', threshold_is_tool_not_principle, instrumental).
narrative_ontology:cs_axiom('2375cace-d12b-4674-be06-632a9340ba80', foundational, legitimacy_grounded_in_measurable_performance).
narrative_ontology:cs_axiom_status(legitimacy_grounded_in_measurable_performance, holdable).
narrative_ontology:cs_axiom_grounding('2375cace-d12b-4674-be06-632a9340ba80', legitimacy_grounded_in_measurable_performance, empirically_contingent).
narrative_ontology:cs_reference_frame('2375cace-d12b-4674-be06-632a9340ba80', empirically_calibrated_consensus_threshold).
narrative_ontology:cs_drift_state('2375cace-d12b-4674-be06-632a9340ba80', contemporary_post_empirical_measurement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2375cace-d12b-4674-be06-632a9340ba80', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, institutional_stability_constituency).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, majoritarian_reform_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, minoritarian_blocking_coalition).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, historical_privilege_holders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, minoritarian_blocking_coalition).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, threshold_is_tool_not_principle).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, evidence_based_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the supermajority threshold through formal constitutional procedures and interprets its legitimacy. In the adaptive-gradient reading, they treat the threshold as a policy instrument whose justification depends on empirical measurement of the polity's consensus formation rate and reversibility costs. They commission empirical research, interpret its findings, and may adjust the threshold based on performance data. Their analytical exit means they can step outside the constraint's operation to measure it — they are not bound by the supermajority requirement in the way other constituencies are.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_convention_authority, agenda_setter,
    institutional, generational, analytical, national).

% Includes institutional actors (courts, civil service, central banks) and political coalitions whose interests depend on predictable, slowly-changing constitutional rules. They benefit from a supermajority requirement that prevents rapid amendment cycles, but only if the requirement is calibrated to actual consensus rates. If the threshold is set too high, they also pay the cost of ossification — constitutional change becomes impossible even when sustained majorities support it. Their constrained exit means they cannot opt out of the constitutional order while remaining within the polity.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_stability_constituency, beneficiary,
    organized, generational, constrained, national).

% Political coalitions advocating constitutional reforms to address contemporary problems. They pay the cost of supermajority requirements: their policy preferences require broader consensus than simple majorities provide. The cost is variable in the adaptive-gradient reading — if the threshold is optimally calibrated, the cost reflects genuine consensus-formation difficulty; if the threshold is set too high, the cost includes pure ossification unrelated to consensus dynamics. Their constrained exit means they cannot avoid the supermajority requirement, though they can attempt to change the threshold itself (a higher-order reform).
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, majoritarian_reform_constituencies, payer,
    moderate, biographical, constrained, national).

% Political minorities positioned to block constitutional changes by assembling supermajority opposition. They benefit from veto power when supermajority requirements reflect genuine consensus-formation difficulty — in that case, their blocking power aligns with the constraint's coordination function. They pay when the threshold is set so high that it produces pure ossification unrelated to consensus dynamics — then their veto power is decoupled from consensus-formation legitimacy and becomes arbitrary blocking. The adaptive-gradient reading distinguishes this seat's benefit from that of historical privilege holders: the minoritarian blocking coalition's benefit is contingent on calibration, whereas historical privilege holders benefit from high thresholds regardless of calibration.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, minoritarian_blocking_coalition, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, minoritarian_blocking_coalition, beneficiary).

% Academics, policy analysts, and institutional researchers who measure actual consensus formation rates, reversibility costs, and outcomes of different threshold settings. They provide the empirical evidence base for threshold calibration. In the adaptive-gradient reading, they are central to the constraint's legitimacy — the threshold's justification depends on their findings about whether it matches the polity's actual ability to form sustained majorities. Their analytical exit means they are outside the extraction flow but central to its justification.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, empirical_consensus_researchers, observer,
    analytical, generational, analytical, national).

% Social groups or institutional arrangements whose constitutional status would be threatened by majoritarian constitutional reform. They benefit when supermajority requirements prevent constitutional changes that would disadvantage them, but in the adaptive-gradient reading their benefit is explicitly distinguished from institutional stability benefit: they benefit from misalignment between the threshold and actual consensus rates — they have an interest in a threshold set too high, creating ossification. Their civilizational time horizon and trapped exit mean they cannot abandon the polity, but they can fight to maintain a high threshold regardless of whether it matches consensus formation rates.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, historical_privilege_holders, beneficiary,
    powerful, civilizational, trapped, national).

% Groups whose formation or interests are not yet recognized in the polity — they cannot participate in constitutional amendment processes or consensus formation measurement. They are excluded from calculating whether the threshold reflects actual consensus dynamics. In the adaptive-gradient reading, they represent a structural blind spot: their future interests cannot be represented in the empirical calibration, yet the threshold determines their ability to reshape constitutional arrangements once they mobilize. Their powerless position and trapped exit mean they have no institutional voice in the constraint's operation or measurement.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, excluded_emergent_constituencies, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, institutional_stability_constituency).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constitutional amendment by mere temporary majority preference, ensuring amendments reflect sustained consensus that survives generational transitions and policy reversals. Solves the coordination problem of making constitutional commitment credible and durable across different political coalitions. Legitimacy is grounded in calibration to the polity's actual consensus formation rate and reversibility costs — the threshold solves the coordination problem only if tuned to real consensus dynamics, not if set arbitrarily high or low.
% TRANSFER_FUNCTION: Moves veto power from simple majorities to broader coalitions by requiring supermajority assent. Transfers constitutional change capacity from majoritarian constituencies to stability-oriented or blocking minorities. The magnitude and direction of transfer depend on calibration: at optimal calibration, it reflects genuine consensus-formation difficulty; at too-high calibration, it is pure ossification extracting from majoritarian constituencies to benefit privilege holders.
% ABSENT_VOICES: Emergent constituencies — groups whose interests will become politically salient after the current amendment cycle — cannot participate in measuring whether the threshold tracks genuine consensus dynamics. Their future interests are structurally excluded from the empirical calibration. They would object to a threshold that prevents constitutional change once they mobilize, but they have no voice in the consensus formation measurement that supposedly justifies the threshold.
% DISAPPEARANCE_RATIONALE: If supermajority thresholds disappeared and constitutional amendment reverted to simple majority, the constitutional stability function would be lost — amendments could reverse rapidly with electoral coalitions, making constitutional commitment less credible. But the world would not simply regress: new forms of constitutional entrenchment might emerge (reliance on non-amendable constitutional interpretation, informal norms, two-level games between federal levels). Empirical consensus research on threshold calibration would become unnecessary infrastructure. The polity would need alternative mechanisms for solving the coordination problem supermajority requirements address.
% FOUNDING_PROBLEM: Simple-majority amendment procedures enabled constitutional instability in early democratic experience: rapid oscillation between constitutional frameworks as political coalitions shifted, making fundamental law unreliable. Supermajority requirements addressed this by requiring broader consensus, but the optimal threshold depends on measurable characteristics of the polity's actual consensus formation rate and the reversibility costs of different amendment classes — not on intrinsic principles or structural empowerment claims.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians attest that simple-majority amendment procedures enabled instability in some historical contexts but not others — the problem is context-dependent and not universal. Empirical researchers outside any benefiting party have measured consensus formation rates in multiple polities and found them to vary widely. Political scientists have documented cases where supermajority thresholds set far above actual consensus rates produced ossification rather than stability. The consensus-safeguard and minoritarian-veto readings each claim the founding problem as settled in their favor but without the kind of external corroboration the adaptive-gradient reading invokes — they rely on normative claims about what legitimacy should consist of (intrinsic principle, structural empowerment) rather than empirical measurements of consensus dynamics.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).
:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.38) reflects moderate asymmetry: the threshold transfers veto power from simple majorities to broader coalitions, but the transfer is justified if it prevents both instability (low threshold) and ossification (high threshold). A well-calibrated threshold is a genuine coordination mechanism; a miscalibrated one is extractive. Suppression (0.22) is low because the supermajority requirement is transparent and does not require active coercion to maintain — the formal rule is self-enforcing. Theater (0.18 baseline, rising to 0.19 mid-interval, settling at 0.18) reflects increasing emphasis on empirical calibration rhetoric while actual threshold-setting practices often rely on traditional or political factors. Resistance (0.52) is moderate because majoritarian constituencies contest the requirement but do not actively undermine the formal rule itself — they instead dispute whether the threshold is calibrated correctly. The measurement series spans 40 time units to capture the period during which empirical consensus research matured and the adaptive-gradient framing gained institutional currency, with stabilization around t=25 when the reading became canonical.
 *
 * PERSPECTIVAL GAP:
 *   The institutional stability beneficiaries and empirical researchers compute this constraint very differently from majoritarian reform constituencies. Beneficiaries see a well-calibrated tool; reformists see an obstacle. The agenda-setter (constitutional convention authority) computes based on its own reading of the evidence — which reading (adaptive-gradient, consensus-safeguard, minoritarian-veto) it adopts determines how it interprets measurement data. A measured consensus formation rate that 'should' set the threshold at 60% would be seized by adaptive-gradient advocates as calibration guidance, by consensus-safeguard advocates as evidence that 70% is safer, and by minoritarian-veto advocates as proof that 60% is too low. The engine computes effective extraction (χ) per-seat from the structural data; the reading-dependent interpretation of that data determines which seat dominates the narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional stability constituency: d ≈ 0.3–0.4 (beneficiary with constrained exit; benefits from coordination but also from high thresholds that may exceed optimal calibration). Majoritarian reform constituencies: d ≈ 0.6–0.7 (targets of the requirement; their exit is trapped within the polity; benefit only if threshold is optimally calibrated, otherwise pay pure extraction). Minoritarian blocking coalition: d ≈ 0.45–0.55 (sit near symmetric in this reading — they benefit from veto power, but only insofar as the veto reflects real consensus dynamics, not structural privilege). Historical privilege holders: d ≈ 0.8 (targets of majoritarian reform; benefit from high thresholds that prevent constitutional change against them). Empirical researchers: d ≈ 0.0 (analytical seat, not in the flow of extraction; their position enables but does not benefit from the constraint). Directionality is context-dependent in this reading: the same agent's d shifts if empirical research shows the threshold is miscalibrated, because their structural relationship to the constraint changes — a feature that distinguishes this reading from the other two, where directionality is treated as fixed.
 *
 * MANDATROPHY ANALYSIS:
 *   The supermajority threshold's founding problem (preventing amendment instability) is live in polities with fractious coalition histories but arguably dead in polities with stable consensus on core constitutional matters. The constraint is vulnerable to mandatrophy reading if the empirical consensus formation rate is measured and found to be far below the instituted threshold — then the requirement persists not because the founding problem is live, but because beneficiaries (privilege holders, stability constituencies) have political power to maintain it regardless of performance. The adaptive-gradient reading is designed to detect and flag this: if measured consensus rates diverge consistently from the threshold, the reading's legitimacy claim is falsified, and the constraint reclassifies toward snare (pure ossification) or piton (inert maintenance). This is the reading's strength and weakness: it is testable, which means it can be proven wrong, which means it is vulnerable if the evidence turns against it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_formation_rate_measurement,
    'What is the polity''s actual consensus formation rate — the speed at which policy preferences converge across diverse constituencies? How should reversibility costs (the cost of undoing constitutional changes) be measured and weighted?',
    'Empirical time-series analysis of amendment adoption rates, coalition formation patterns across constitutional debates, and outcome data on amended vs. unamended constitutional provisions. Comparative institutional analysis across polities with different thresholds to measure whether threshold differences correlate with amendment rates and stability.',
    'If measured consensus formation rates are substantially below the instituted threshold, the constraint reclassifies toward snare/ossification; if substantially above, toward rope/instability. The threshold that matches empirical rates would support the adaptive-gradient reading; divergence supports the consensus-safeguard (threshold should be higher) or minoritarian-veto (threshold is too low) readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_formation_rate_measurement, empirical, 'Whether empirical consensus formation rates can be measured reliably and whether the instituted threshold matches them.').

omega_variable(
    reversibility_cost_calibration,
    'Should supermajority requirements vary by amendment class — tighter for structural changes (executive power, territorial scope, fundamental rights) and looser for procedural changes? If so, how should reversibility cost be measured and used to set class-specific thresholds?',
    'Case analysis of constitutional amendments that proved reversible vs. permanent, measurement of institutional and social costs associated with reversal, and empirical analysis of whether amendment classes with high reversibility costs show different adoption rates under different thresholds.',
    'If reversibility cost should drive class-specific calibration, a single supermajority threshold is suboptimal and the adaptive-gradient reading requires refinement into a multi-threshold framework. This would increase theater (more institutional machinery to maintain) and may reduce extractiveness if differentiation prevents misalignment. If reversibility cost is not measurable or should not drive threshold-setting, the adaptive-gradient reading''s empirical ambition collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_cost_calibration, conceptual, 'Whether supermajority requirements should be differentiated by amendment class and how reversibility cost should be measured.').

omega_variable(
    excluded_constituencies_future_interests,
    'How should supermajority calibration account for constituencies whose interests are not yet salient in current politics — groups that will mobilize for constitutional change after the current amendment cycle? Is there a method for including future interests in the consensus formation measurement?',
    'Theoretical work on dynamic constitutional legitimacy and intergenerational representation. Historical analysis of how excluded groups'' past interests were represented (or excluded) in earlier constitutional amendment cycles. Prospective institutional design that creates mechanisms for anticipating emergent constituencies.',
    'If future interests cannot be represented in calibration, the adaptive-gradient reading''s empirical legitimacy claim is incomplete — it optimizes for current consensus formation but may entrench exclusions that future majorities would reverse. This is a conceptual limit of the reading that neither of the other readings shares (consensus-safeguard treats all threshold-setting as already representing all interests; minoritarian-veto treats exclusion as a feature, not a bug).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_constituencies_future_interests, conceptual, 'Whether empirical consensus formation can ever include future interests, or whether the adaptive-gradient reading necessarily inherits constitutional exclusions.').

omega_variable(
    reading_dependent_empirical_interpretation,
    'Will empirical measurements of consensus formation rates be interpreted differently by agents holding different readings of the kernel? Is there a measurement that would convince a consensus-safeguard advocate to lower the threshold, or a minoritarian-veto advocate to raise it?',
    'Meta-analysis of how different constitutional traditions have interpreted the same empirical data (amendment rates, coalition formation patterns, stability outcomes) through different theoretical frameworks. Test whether specific empirical findings have ever caused a shift in reading adoption.',
    'If empirical measurements cannot arbitrate between readings, the adaptive-gradient reading''s claim that legitimacy is empirical is itself undermined — the readings remain live because they interpret data through incompatible frames. This would suggest the kernel contest is fundamentally conceptual/preference-based, not empirical, and the adaptive-gradient reading''s apparent empiricism is theater (a meta-level mandatrophy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dependent_empirical_interpretation, empirical, 'Whether empirical consensus formation data can arbitrate between competing readings of the supermajority threshold kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t5, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(supe_tr_t5, observed).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(supe_tr_t10, observed).
narrative_ontology:measurement(supe_tr_t15, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(supe_tr_t15, observed).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(supe_tr_t20, observed).
narrative_ontology:measurement(supe_tr_t25, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(supe_tr_t25, observed).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(supe_tr_t30, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(supe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t5, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(supe_be_t5, observed).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(supe_be_t10, observed).
narrative_ontology:measurement(supe_be_t15, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(supe_be_t15, observed).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(supe_be_t20, observed).
narrative_ontology:measurement(supe_be_t25, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(supe_be_t25, observed).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement_basis(supe_be_t30, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(supe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t5, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(supe_su_t5, observed).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(supe_su_t10, observed).
narrative_ontology:measurement(supe_su_t15, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(supe_su_t15, observed).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement_basis(supe_su_t20, observed).
narrative_ontology:measurement(supe_su_t25, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 25, 0.23).
narrative_ontology:measurement_basis(supe_su_t25, observed).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(supe_su_t30, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(supe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, constitutional_amendment_stability).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, majoritarian_legislative_authority).

% DUAL FORMULATION NOTE:
% The supermajority_threshold kernel decomposes into three structurally distinct constraint stories grounded in different readings of the same constitutional text and institutional practice. The adaptive_gradient_reading treats the threshold as an empirically calibrated tool and holds that legitimacy depends on measurable performance. The consensus_safeguard_reading treats the threshold as intrinsically protective of democratic integrity. The minoritarian_veto_reading treats the threshold as structural empowerment of blocking minorities. These are not the same constraint viewed from different angles — they have different ε values (extractiveness depends on whether the threshold is calibrated to actual consensus formation rates), different beneficiary/victim structures (who benefits depends on which reading's core claim is true), and different failure modes (instability, ossification, entrenchment). The three stories are linked via network.affects_constraints because each reading cites the others' core premises as evidence for or against its own claims, creating a constraint family where measuring one reading's performance feeds back into the others' legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
