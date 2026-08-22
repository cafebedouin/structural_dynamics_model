% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: Capacity-Based State Classification
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The graduated sovereignty reading classifies states on a spectrum of
 *   capacity and governance legitimacy, with international institutions and
 *   powerful states retaining discretion to intervene, monitor, or place
 *   low-capacity states under partial external administration. Presented as a
 *   refinement of Westphalian sovereignty that acknowledges state failure and
 *   protects populations from abuse, this reading generates the structural
 *   conditions for neo-colonial extraction: weak states become objects of
 *   continuous reclassification; external interveners hold the authority to
 *   declare when a state 'fails' the legitimacy test; the categories shift
 *   with geopolitical interest. The constraint operates as a snare because
 *   the classification machinery itself becomes an instrument of
 *   extraction—the framework that justifies intervention is the same
 *   framework that denies victim states the exit option of autonomous
 *   governance.
 *
 * KEY AGENTS:
 *   - High-capacity states (permanent UN Security Council members, G7 economies): establish the legitimacy criteria and retain discretion to apply them; d near 0.0 (beneficiary seat)
 *   - International institutions (IMF, World Bank, UN governance bodies): administer capacity assessments and conditionality; d near 0.1 (agenda-setter collecting administrative authority)
 *   - Low-capacity states (fragile, postcolonial, conflict-affected): subject to reclassification and intervention; d near 1.0 (target seat, extraction victims)
 *   - Weak-governance regimes (failed states, authoritarian regimes with limited capacity): bear the suppression directly; d = 1.0 (principal target)
 *   - Postcolonial nations: experience the constraint as a reinstatement of hierarchical international order; d near 0.95 (victim seat with deep structural trapping)
 *   - External interveners (military coalitions, occupation forces): derive authority to operate from the capacity classification; d near 0.0 (beneficiary of the discretion the framework grants)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.71).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: Capacity-Based State Classification").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '84cd7529-d259-42f3-a160-cd50154e8de2').
narrative_ontology:cs_kernel_codification('84cd7529-d259-42f3-a160-cd50154e8de2', fixed_text).
narrative_ontology:cs_authority_grounding('84cd7529-d259-42f3-a160-cd50154e8de2', extraction).
narrative_ontology:cs_interpretation_layer_present('84cd7529-d259-42f3-a160-cd50154e8de2').
narrative_ontology:cs_reading_relation('84cd7529-d259-42f3-a160-cd50154e8de2', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('84cd7529-d259-42f3-a160-cd50154e8de2', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('84cd7529-d259-42f3-a160-cd50154e8de2', foundational, sovereignty_conditional_on_capacity).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_capacity, holdable).
narrative_ontology:cs_axiom_grounding('84cd7529-d259-42f3-a160-cd50154e8de2', sovereignty_conditional_on_capacity, empirically_contingent).
narrative_ontology:cs_axiom('84cd7529-d259-42f3-a160-cd50154e8de2', foundational, external_discretion_legitimizes_assessment).
narrative_ontology:cs_axiom_status(external_discretion_legitimizes_assessment, holdable).
narrative_ontology:cs_axiom_grounding('84cd7529-d259-42f3-a160-cd50154e8de2', external_discretion_legitimizes_assessment, instrumental).
narrative_ontology:cs_reference_frame('84cd7529-d259-42f3-a160-cd50154e8de2', westphalian_equality_formal_doctrine).
narrative_ontology:cs_drift_state('84cd7529-d259-42f3-a160-cd50154e8de2', contemporary_capacity_assessment_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84cd7529-d259-42f3-a160-cd50154e8de2', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, high_capacity_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, external_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_institutions).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, postcolonial_nations).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_governance_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, populations_under_intervention).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, populations_under_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish the criteria for state capacity and governance legitimacy; retain discretion to apply them selectively; benefit from the authority to classify others' sovereignty and intervene when classification justifies it. Can disengage from the framework or reshape it through institutional power. Collect diplomatic influence, resource access, and strategic positioning from the capacity-assessment regime. Include permanent UN Security Council members, G7 states, and NATO core members.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, high_capacity_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, high_capacity_states, beneficiary).

% Administer capacity assessments, conditionality programs, and monitoring regimes; expand mandate authority by operationalizing the graduated framework; craft the metrics and benchmarks that define capacity and legitimacy. Institutional exit is difficult—reforming or abandoning the framework would mean loss of accumulated authority and budget. Includes IMF, World Bank, UN agencies, OECD, regional development banks.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Subject to capacity assessment and reclassification by external actors; constrained in sovereign choice-making by conditionality programs; must negotiate with international institutions to access resources (debt financing, aid, investment). Exit options are nil: withdrawing from the international system carries economic devastation; accepting the framework means accepting the extraction of rent through conditionality and governance restructuring. Include fragile states, postcolonial nations without hard-power backing, and states dependent on international financing.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, low_capacity_states, payer,
    powerless, biographical, trapped, global).

% Bear the direct suppression of the capacity-classification regime: face international intervention, external administration, or military occupation justified by capacity deficits; lose autonomous control of policy-making to external overseers; experience the constraint as occupation dressed in legitimacy language. No exit available; survival depends on compliance with external governance regimes or successful armed resistance (itself requiring external support to succeed).
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_governance_regimes, payer,
    powerless, immediate, trapped, national).

% Experience the graduated sovereignty framework as reinstatement of colonial hierarchy: the capacity metrics replicate colonial-era judgments about who is 'fit for self-government'; the conditionality regimes replicate colonial-era resource extraction; the international institutions replicate colonial-era administrative authority. Exit is identity-locked because rejecting the framework means rejecting participation in the international system built on it, which is existentially difficult for postcolonial states that built their legitimacy on recognition within that system. Excluded from meaningful voice in setting the criteria that classify them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, postcolonial_nations, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, postcolonial_nations, excluded).

% Military coalitions, occupation forces, and administrative authorities derive their authority to operate in foreign states from the capacity-classification framework. Benefit from the discretion to define capacity deficits that justify deployment. Can choose whether to intervene, can frame interventions as capacity-building, can extract political or resource concessions under the guise of governance assistance. Exit at will; intervention is voluntary from the intervener's perspective.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, external_interveners, beneficiary,
    powerful, biographical, arbitrage, global).

% Experience the graduated framework simultaneously as potential protection (humanitarian intervention may prevent atrocity) and as occupation justification (intervention enables resource extraction, colonial administration, and cultural imposition). Genuinely trapped—cannot exit intervention zones, cannot credibly opt out of the framework. Some benefit from improved security or governance services; all experience loss of autonomy and externally-imposed policy choices.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, populations_under_intervention, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, populations_under_intervention, beneficiary).

% Alternative readings (absolute sovereignty, conditional sovereignty) and rival institutional frameworks (Non-Aligned Movement, BRICS development bank, African Union autonomy initiatives) are structurally excluded from setting the global capacity-assessment regime. Would argue for alternative metrics, peer-based evaluation, or genuine state equality. Their exclusion is maintained by the suppression machinery (institutional authority, resource control, military backing) that sustains the graduated framework.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, rival_capacity_frameworks, excluded,
    moderate, generational, constrained, global).

% Academic analysis, civil society monitoring, and independent expertise can document divergence between capacity criteria and actual intervention patterns; can expose reclassification as pretextual; can track the theater_ratio rise as genuine problem-solving capacity declines. Occupy the analytical seat; their capacity to influence the constraint operates through discourse and indirect institutional pressure, not through direct authority.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, high_capacity_states).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for identifying states whose governance failures generate spillover effects (refugee flows, disease, weapons proliferation, terrorism) and justifies coordinated international response to prevent regional destabilization. Ostensibly solves the collective-action problem of state failure: no single external power has capacity or legitimacy to unilaterally intervene, but coordinated capacity-based assessment provides a shared criterion for when intervention is justified.
% TRANSFER_FUNCTION: Moves sovereignty authority from low-capacity states to international institutions and high-capacity-state coalitions; moves resource control from local actors to external administrators; moves policy-making discretion from domestic governments to international overseers. In monetary terms: moves capital (debt servicing conditionality, resource concessions to interveners) and moves access to markets (trade relationships renegotiated under conditionality). The payers are low-capacity states, weak-governance regimes, and populations under intervention; the receivers are high-capacity states (geopolitical influence, resource access), international institutions (expanded mandate authority, administrative fee collection), and external interveners (strategic positioning, extraction of concessions).
% ABSENT_VOICES: States that reject the graduated framework's legitimacy (China, Russia, postcolonial non-aligned blocs) are structurally excluded from capacity-standard-setting while remaining subject to its application. Populations under intervention have no meaningful voice in assessment of their own 'capacity' or design of governance restructuring. Academic and civil-society critics of the framework's extractive function are excluded from institutional decision-making. Traditional authorities, indigenous governance systems, and non-Westphalian sovereignty models are systematically absent from the capacity metrics themselves.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty framework disappeared overnight, the international system would restructure substantially: external interventions would lose their legitimacy cover; IMF conditionality would be unmoored from capacity-assessment justification; international administrations would become occupied territories rather than governance missions. States would revert to either absolute-sovereignty frameworks (high-capacity states reasserting non-interference) or to alternative coordination mechanisms (regional treaties, mutual defense pacts without capacity conditionality). Resource flows would shift; extraction mechanisms based on conditionality would collapse; postcolonial nations would gain regained-exit options toward alternative international arrangements. The constraint's disappearance would require renegotiation of power relationships that currently operate within its framework.
% FOUNDING_PROBLEM: State failure and governance collapse generate spillover effects that destabilize regions and threaten international security: civil wars produce refugee populations, weak states become havens for terrorism and weapons trafficking, economic collapse in strategic regions triggers regional wars and migration crises. The capacity-assessment framework was built to coordinate international response to these problems by providing a shared criterion (state capacity and legitimacy) for when intervention is justified and by pooling resources for stabilization.
% FOUNDING_PROBLEM_CORROBORATION: High-capacity states and international institutions affirm the founding problem is live: they cite ongoing state failures in Syria, Yemen, Somalia, Afghanistan, and Haiti as evidence that capacity assessment remains necessary. Low-capacity states, postcolonial analysts, and non-aligned blocs contest the problem's scope and the framework's response: they argue that spillover effects have been exaggerated to justify extraction; that many states classified as 'failed' are functioning according to non-Westphalian governance models; that intervention frequently exacerbates state failure rather than reversing it. Independent researchers document cases (Rwanda post-2000, post-Taliban Afghanistan) where genuine state capacity improved under international engagement, and cases (Libya, Iraq, Syria) where intervention degraded state capacity substantially. The corroborating testimony outside the beneficiary set concludes that the founding problem (legitimate spillover effects requiring coordination) is real but substantially overstated, and that the framework's application has diverged from the founding problem toward extraction.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end because the constraint operates on two levels simultaneously: (1) it legitimizes interventions that extract resources, impose governance regimes, and restructure institutions in ways favorable to external powers; (2) it encodes into international law the principle that sovereignty is conditional on meeting criteria set by those same external powers. Rising extractiveness over time (0.42→0.65) reflects the progressive entrenchment of capacity-assessment mechanisms and the accumulation of precedents where capacity judgments justified intervention. Suppression runs higher (0.71) because the constraint's persistence depends on actively preventing alternative frames (absolute sovereignty, shared legitimacy of all states) from competing in international discourse. Theater ratio (0.48) indicates that roughly half the activity around capacity assessment is genuine governance oversight (identifying real state failure, coordinating humanitarian response) and half is performance that justifies extraction (reclassifying states to justify resource extraction, imposing IMF conditionality unrelated to stated capacity concerns). The measurement series tracks a single shared time grid across all three metrics at t∈{0,4,8,12,16,20}.
 *
 * PERSPECTIVAL GAP:
 *   The high-capacity-state seat and the low-capacity-state seat compute radically different types from the same structural data. From the perspective of the U.S., EU, or permanent UN members: the graduated framework is a genuine coordination mechanism—it identifies which states need assistance, prevents refugee crises by stabilizing weak governance, and protects populations from authoritarian abuse. The constraint appears as rope (coordination with real beneficiaries) or tangled_rope (coordination with some asymmetry). From the perspective of postcolonial states, occupied territories, or regions under external administration: the same framework is an instrument of domination—the capacity measures are pretexts, the 'assistance' rewrites their constitutions, and the exit option (genuine autonomy) is foreclosed by the same mechanism that classifies them. The constraint appears as snare. The engine computes per-seat classification from the structural data (exit options, power asymmetries, beneficiary/victim declarations); the divergence between the stated framing and the computed types is itself the measurement that distinguishes coordination from extraction. This story claims snare because the structural data (power asymmetry, trapped exit for victims, asymmetric beneficiary protection, active suppression of alternatives) supports it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-capacity states benefit from the discretion the framework grants (d→0.0): they set the criteria, determine when other states fail them, and retain the authority to intervene. Their exit is 'arbitrage'—they can engage or disengage from the framework as their interests shift; their power is institutional; their time horizon is civilizational (the framework is built to persist through their institutional forms). Low-capacity states bear the costs of the framework (d→1.0): they are subject to reclassification, constrained in their sovereign choices by conditionality imposed by external actors, and trapped because rejecting the framework's authority carries sanctions while accepting it carries extraction. Their exit is 'trapped'—they cannot exit the international system itself, and alternatives to the graduated framework (absolute sovereignty, non-interference) are actively suppressed. International institutions sit between (d~0.3)—they benefit from the framework's authority to administer assessments and conditionality, but they are ultimately tools of high-capacity-state interests. The beneficiary/victim declarations and the exit-options asymmetry drive this directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the graduated sovereignty reading is genuine: state failure produces spillover effects (refugee flows, disease, weapons proliferation, terrorism) that destabilize regions and threaten international stability. This problem was live and acute in the 1990s-2000s (Somalia, Rwanda, Afghanistan) and remains contested today (Syria, Yemen, Haiti). However, the constraint's persistence does not track the founding problem's trajectory. In cases where genuine state capacity improves (Rwanda post-2000, post-Taliban Afghanistan), the external intervention and capacity monitoring have not diminished proportionally—instead, they have shifted focus to new capacity concerns (democratic standards, women's rights, anti-corruption) and new victim states. This is mandatrophy: the constraint's founding problem (preventing state failure spillover) has been partially solved or has plateaued, but the constraint persists and expands because the beneficiary set (high-capacity states, international institutions) now depends on it for reasons divorced from the founding problem (resource extraction, geopolitical positioning, bureaucratic expansion). The rising theater_ratio (0.35→0.51) is diagnostic: an increasing share of capacity-assessment activity is performance justifying continued intervention rather than genuine problem-solving response to acute state failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_discretion,
    'Who determines what counts as ''state capacity'' and ''governance legitimacy''? Are these measurements objective and universal, or do external interveners retain discretion to reclassify states based on shifting criteria?',
    'Comparative analysis of capacity assessments applied to similarly-situated states over time; examination of whether reclassifications track capacity changes or political relationships with intervening powers.',
    'If discretion is high, the constraint operates as neo-colonial extraction: reclassification becomes a tool to justify selective intervention. If discretion is low, the constraint is a genuine graduated framework. The ε difference is substantial (0.35 vs 0.70).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_measurement_discretion, empirical, 'Whether capacity measures are standardized or subject to intervener discretion').

omega_variable(
    graduated_vs_absolute_reading_contestation,
    'Is the graduated reading a refinement of Westphalian sovereignty or a replacement that strips away its core protection? The absolute reading treats capacity assessment itself as illegitimate interference; the graduated reading normalizes it as necessary oversight.',
    'Examine the historical transition of doctrine: when and why did international law shift from categorical non-interference to graduated-capacity framing? Was the shift driven by changed empirical conditions (state failure, refugee crises) or by power shifts allowing stronger states to encode discretion?',
    'If the shift was empirically driven (genuine problem emergence), the graduated reading constrains extraction. If driven by power consolidation, it enables extraction. The reading''s legitimacy hinges on which.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_vs_absolute_reading_contestation, conceptual, 'Whether graduated sovereignty is a legitimate evolution or a doctrinal capture').

omega_variable(
    structural_asymmetry_in_classification,
    'Why do high-capacity states never face capacity-based reclassification (external trusteeship), while low-capacity states routinely do? Is this asymmetry intrinsic to the framework, or a manifestation of power inequality?',
    'Inventory historical cases where high-capacity states were assessed for capacity deficits (e.g., during financial crises, governance failures); compare intervention outcomes to low-capacity state interventions. If asymmetry is universal, it is structural to the reading.',
    'Structural asymmetry is evidence the constraint operates as extraction: the beneficiary set is protected by the same mechanism that victimizes the payer set. This strengthens the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_asymmetry_in_classification, empirical, 'Directionality asymmetry: whether capacity assessment applies universally or selectively').

omega_variable(
    internalized_capacity_narrative,
    'To what extent have weak-state actors internalized the capacity narrative—believing they genuinely lack legitimacy and deserve intervention—versus structurally resisting the reclassification as illegitimate?',
    'Examination of state discourse: do low-capacity states'' own leadership narratives embrace or reject the capacity-based framing? Post-intervention surveys of occupied or monitored populations about perceived legitimacy of external governance.',
    'Internalization raises suppression (the constraint is self-enforcing rather than externally coerced), which is diagnostic for snares. Resistance lowers it, suggesting the reclassification lacks compliance even with the rules of the constraint itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_capacity_narrative, empirical, 'Internalization vs. structural resistance to capacity-based legitimacy denial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(west_tr_t4, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 4, 0.39).
narrative_ontology:measurement(west_tr_t8, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 8, 0.43).
narrative_ontology:measurement(west_tr_t12, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 12, 0.46).
narrative_ontology:measurement(west_tr_t16, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 16, 0.48).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 20, 0.51).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(west_be_t4, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(west_be_t8, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(west_be_t12, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(west_be_t16, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(west_su_t4, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(west_su_t8, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(west_su_t12, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(west_su_t16, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__graduated_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, international_conditionality_framework).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, structural_adjustment_programs).

% DUAL FORMULATION NOTE:
% The Westphalian sovereignty kernel decomposes into three constraint stories, one per reading. Graduated_sovereignty is the reading that operationalizes extraction via capacity-based classification. The constraint family traces the institutional path from formal equality (absolute reading) through principled intervention doctrine (conditional reading) to discretionary capacity assessment (graduated reading). Each reading has the same referent (the Westphalian principle) but different ε values: absolute_sovereignty ε≈0.05 (Mountain, governance principle), conditional_sovereignty ε≈0.35 (Tangled Rope, coordination + asymmetric intervention), graduated_sovereignty ε≈0.62 (Snare, discretionary classification enabling extraction). The graduated reading influences downstream constraints on IMF structural adjustment and international trusteeship regimes by providing the legitimacy framework those constraints operate within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__graduated_sovereignty, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
