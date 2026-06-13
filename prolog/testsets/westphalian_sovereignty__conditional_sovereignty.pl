% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Responsibility-Based Intervention Rights
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The conditional-sovereignty doctrine asserts that state sovereignty
 *   entails responsibility: systematic human rights violations trigger
 *   legitimate external intervention. This is ONE READING of the contested
 *   Westphalian sovereignty kernel. It competes with absolute-sovereignty
 *   readings (which deny external intervention legitimacy) and
 *   graduated-sovereignty readings (which tie sovereignty to state capacity
 *   rather than violation response). This constraint story instantiates ONLY
 *   the conditional-sovereignty reading: the view that violations create
 *   intervention rights. The kernel itself — what sovereignty IS and what it
 *   permits — remains contested across the sibling readings; this story does
 *   not reconcile them. The constraint operates as a snare on targeted
 *   states: it constrains their autonomy by making them conditional
 *   duty-bearers, and it benefits intervention advocates by legitimizing
 *   their authority. The measurement trajectory shows rising extractiveness
 *   and theater as the doctrine matured: from a nascent post-WWII principle
 *   to an established but selectively enforced framework.
 *
 * KEY AGENTS:
 *   - intervention_advocates (institutional agenda-setters): Western democracies, UN organs, humanitarian bodies that define violations thresholds and authorize intervention
 *   - targeted_sovereign_states (victims, trapped): states accused of violations; lose autonomy and face sanctions/intervention
 *   - state_populations_under_intervention (powerless payers, identity-locked): citizens bearing intervention costs while trapped by citizenship
 *   - hrn_advocacy_networks (organized beneficiaries): human rights NGOs gaining authority and policy influence through the doctrine
 *   - absolute_sovereignty_advocates (excluded): states and scholars rejecting the doctrine; structurally outside definition-setting
 *   - graduated_sovereignty_proponents (observers): academic alternative framing; externally analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.52).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Responsibility-Based Intervention Rights").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '0bb2c3dc-8c1c-44a0-8e76-54b354c19b02').
narrative_ontology:cs_kernel_codification('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', distributed).
narrative_ontology:cs_authority_grounding('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', extraction).
narrative_ontology:cs_interpretation_layer_present('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02').
narrative_ontology:cs_reading_relation('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', foundational, sovereignty_entails_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', sovereignty_entails_responsibility, deontological).
narrative_ontology:cs_axiom('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', foundational, violations_trigger_intervention_legitimacy).
narrative_ontology:cs_axiom_status(violations_trigger_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', violations_trigger_intervention_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', humanitarian_responsibility_frame).
narrative_ontology:cs_drift_state('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0bb2c3dc-8c1c-44a0-8e76-54b354c19b02', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, hrn_advocacy_networks).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, state_populations_under_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, state_populations_under_intervention).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, intervention_implementation_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Western liberal democracies (US, EU states, UK, Canada), UN Security Council permanent members, humanitarian organizations (UN OHCHR, Human Rights Watch, Amnesty International), and advocacy coalitions that define what constitutes systematic violations and determine when external intervention becomes legitimate. They author interpretations of the threshold, propose interventions, authorize multinational responses, and refer cases to international courts. They benefit by gaining authority to shape other states' internal affairs under the legitimacy shield of human rights protection.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocates, agenda_setter,
    institutional, generational, arbitrage, global).

% States accused of systematic human rights violations (Syria, Myanmar, Venezuela, North Korea, Afghanistan under Taliban, etc.). They lose decision-making autonomy over internal governance when violations are determined to meet intervention thresholds. They face economic sanctions, arms embargoes, referrals to international courts, military intervention, and pressure to reform institutions per external standards. They are trapped because formal exit from the international legal system is not viable; non-compliance triggers escalated intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_states, payer,
    institutional, generational, trapped, national).

% Civilians living in targeted states. They are the formal justification for intervention (the doctrine exists to protect populations from abuse by their own government). But they also bear the costs: military strikes, displacement, economic sanctions that reduce available food/medicine, institutional disruption, occupation governance, and prolonged instability. They cannot exit citizenship even when intervention occurs. Many experience both the original violation AND the intervention response as harms.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, state_populations_under_intervention, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, state_populations_under_intervention, beneficiary).

% International human rights organizations and advocacy networks. They benefit by gaining institutional authority: their documentation of violations becomes the evidential basis for intervention decisions, their investigative authority is recognized by powerful states, their policy proposals (Responsibility to Protect doctrine, International Criminal Court referrals) are institutionalized, and their access to decision-makers increases. The doctrine amplifies their voice and converts their evidence into binding state action.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, hrn_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% States and scholars that reject conditional sovereignty on grounds that it violates the fundamental principle of non-interference and state autonomy (Russia, China, many developing states, sovereignty-realist scholars). They argue the doctrine is a pretext for powerful states to project authority over weak states' affairs. They are systematically excluded from the institutions that define the doctrine's thresholds and authorize interventions — the Security Council, General Assembly committees, and international courts where the doctrine is operationalized. Their exclusion is structural: the doctrine's legitimacy depends on treating their view as illegitimate.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, absolute_sovereignty_advocates, excluded,
    institutional, generational, trapped, global).

% Academic institutions, policy think tanks, and moderate state officials exploring graduated-sovereignty frameworks (sovereignty tied to state capacity and governance legitimacy rather than violation response). They are analytically positioned outside this constraint's operation but represent an alternative institutional framing that would restructure intervention conditions. They observe and document the contested nature of the conditional-sovereignty doctrine.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, graduated_sovereignty_proponents, observer,
    moderate, generational, analytical, global).

% Military forces (US, NATO, EU, UN), multinational peacekeeping corps, occupation administrations, and humanitarian response organizations that execute interventions. They implement intervention mandates but also bear costs: military casualties, indefinite commitment (peacekeeping missions often persist far longer than intended mandates), operational complexity in active conflict zones, and moral friction when humanitarian protection requires killing. They are constrained because political mandate withdrawal does not automatically end on-ground obligations to the populations they have begun supporting.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_implementation_forces, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, intervention_implementation_forces, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, intervention_advocates).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective international accountability: when states systematically violate human rights, external actors gain legitimacy to intervene. Theoretically solves the collective-action problem in human rights protection by creating conditions under which the default norm of non-interference can be overridden for humanitarian justification. The coordination problem framed: how can the international system respond to internal atrocities without resorting purely to geopolitical calculation?
% TRANSFER_FUNCTION: Transfers decision-making authority over internal state affairs from sovereign governments to international bodies (UN Security Council, General Assembly, International Criminal Court, regional organizations). Also transfers compliance costs: targeted states must accept external monitoring by UN bodies and human rights rapporteurs, reform domestic institutions per international standards, submit to economic sanctions, or accept military intervention. The mechanism moves authority from national level to international level and moves responsibility allocation from individual state officials to international courts and commissions.
% ABSENT_VOICES: Absolute-sovereignty advocates (Russia, China, developing-state governments) are structurally absent from the doctrine's threshold-definition process, though they oppose it in the General Assembly and refuse to comply with international court referrals. Populations of targeted states have limited voice in whether intervention serves their actual interests or simply substitutes one form of external authority for another. Scholars emphasizing state autonomy and cautious intervention are marginalized in the institutional conversation, though present in academic venues and dissenting General Assembly statements.
% DISAPPEARANCE_RATIONALE: If conditional-sovereignty doctrine disappeared overnight, the default international legal regime would revert to absolute-sovereignty non-interference norms. Weak states would regain decision-making autonomy over internal affairs (though not from their own governments). Intervention would need to be justified on other grounds: strategic interest, territorial disputes, international security threats. International humanitarian organizations would lose the legal framework that legitimizes their involvement in state governance. Powerful states would lose the humanitarian framing for interventions pursued for geostrategic reasons. The global governance structure would reorganize around either pure state sovereignty or a different legitimacy structure for international oversight (e.g., graduated sovereignty tied to state capacity, or purely interests-based alliance intervention without humanitarian language).
% FOUNDING_PROBLEM: The founding problem: post-WWII international consensus that systematic mass atrocities create a moral and political obligation for the international community to prevent or stop killing of civilians at scale. Early formulation in the Genocide Convention (1948): signatories pledge to 'prevent and punish' genocide. Later expansion through the Responsibility to Protect doctrine (2005): sovereignty entails responsibility; when a state fails to protect its population from mass atrocities, the responsibility transfers to the international community.
% FOUNDING_PROBLEM_CORROBORATION: Intervention advocates and humanitarian organizations attest the founding problem is live: systematic mass atrocities continue (Myanmar Rohingya, Syrian civil war chemical attacks, Yemen cholera from conflict-caused disruption), and international response capacity remains chronically under-resourced and inconsistently applied. Absolute-sovereignty advocates and targeted states attest the founding problem has been weaponized: intervention authority is applied selectively (the constraint applies to weak, non-aligned states; not to powerful states or allies of powerful states), many interventions occur in contexts where violation claims were contested or exaggerated, and intervention often generates new atrocities rather than stopping them. Independent media analysis and academic scholarship document both empirical points: real mass atrocities exist (supporting founding problem is live) AND selective application across state power differentials (supporting contestation). No external corroborator consensus; the founding problem's status is itself fundamentally disputed along lines of which doctrine reading one accepts.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.15 in 1948) because the doctrine was nascent, contested, and rarely invoked. It rises to 0.38 by 2024 as the framework matured and interventions became more frequent, but levels off because the doctrine's selective application (enforced on weak states, not powerful ones) and ongoing resistance from absolute-sovereignty advocates constrain full extraction potential. Theater rises sharply from 0.10 to 0.42 as the doctrine accumulates rhetorical cargo: humanitarian language expands while strategic intervention motives persist. The constraint requires active enforcement (0.52 suppression at interval end) because targeted states continuously resist the doctrine's application and seek to reassert absolute sovereignty. The measurement grid is shared across all three metrics — every time point appears for every metric — so the temporal pattern captures both the doctrine's maturation and the mounting tension between stated principle and actual application. Theater's rise above 0.40 signals that performative justification (human rights framing) increasingly substitutes for functional enforcement (actual protection of populations).
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setters (intervention advocates) experience this constraint as enabling legitimate oversight; payers (targeted states) experience it as sovereignty violation; populations experience it as humanitarian framing obscuring military intervention. These gaps are not resolvable within a single reading — they are expressions of fundamentally opposed views about what sovereignty permits. The engine's per-seat classification should capture this: the constraint is snare from the payer's perspective (coercive, extractive), rope or coordination from the beneficiary's perspective (enabling, legitimate), and contested from the population's perspective (protective but disruptive).
 *
 * DIRECTIONALITY LOGIC:
 *   The conditional-sovereignty doctrine benefits those who wield intervention authority (Western states, UN bodies, humanitarian networks) by legitimizing their involvement in other states' affairs. It extracts from targeted states by constraining their autonomy: they become conditional duty-bearers, meaning they must meet external standards or face sanctions/intervention. Powerless populations fused with state identity pay through conflict, disruption, and externally imposed policy. The doctrine extracts because it uses humanitarian language to justify what is structurally a transfer of authority from national to international bodies, concentrated in powerful states' hands. Suppression operates through both structural mechanisms (weaker states have no force to resist) and internalized mechanisms (the doctrine's humanitarian framing causes many to accept the constraint as legitimate even while bearing its costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The conditional-sovereignty doctrine faces a mandatrophy risk: the founding problem (protection from mass atrocities) persists, but the implementation increasingly operates as pure extraction (authority transfer to intervention advocates) rather than as solution to the founding problem. The measurement data shows this: theater rises from 0.10 to 0.41 while extractiveness plateaus at 0.38. This suggests that the doctrine's function is gradually being replaced by its form: the humanitarian language persists and expands, but actual protection does not. A state or population experiencing intervention is often subjected to both (the violation it faces AND the intervention response), so the doctrine does not always solve the founding problem — it sometimes doubles the harm. The mandatrophy question: Is this constraint solving the atrocity-response problem (live mandate) or merely legitimizing geopolitical intervention (dead mandate)? The measurement trajectory suggests the mandate is contested and eroding toward dead even as the constraint persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    violation_threshold_ambiguity,
    'What constitutes ''systematic human rights violations'' sufficient to trigger intervention legitimacy? Who determines the threshold, and is the determination objective or contestable?',
    'Systematic analysis of intervention decisions across cases: do advocates apply the threshold consistently, or do geopolitical factors and power asymmetries drive determination? Do targeted states and independent observers agree with the threshold assessments?',
    'If thresholds are applied consistently and verified independently, the doctrine is constraint on state autonomy (snare). If thresholds are applied selectively and contestable, the doctrine is cover-story for geopolitical intervention (more extractive, less legitimate). High selectivity would raise ε and confirm snare classification; objectivity would lower ε toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(violation_threshold_ambiguity, empirical, 'Whether violation thresholds are objective constraints or subjective covers for strategic intervention.').

omega_variable(
    intervention_outcome_divergence,
    'Do interventions authorized under conditional-sovereignty doctrine actually reduce violations and improve outcomes for targeted populations, or do they generate new harms?',
    'Comparative post-intervention studies: mortality, displacement, institutional capacity, and rights protection measured before and after intervention across cases (Kosovo, Iraq, Libya, Syria, Yemen, etc.).',
    'If interventions reliably improve outcomes, the doctrine''s functional mandate (protecting populations) is live and the constraint is legitimate coordination despite extraction. If outcomes are mixed or negative, the mandate is dead and the doctrine is pure extraction under humanitarian cover (high theater, no functional coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_outcome_divergence, empirical, 'Whether the doctrine''s stated protective function is actually realized.').

omega_variable(
    selective_enforcement_mechanism,
    'Why do powerful states evade the doctrine''s application while weak states face it? Is this structural (powerful states cannot be targeted) or contingent (luck of not being accused)?',
    'Historical analysis of intervention patterns: do intervention advocates apply the violation threshold to powerful states'' internal conduct (suppression in India, mass detention in China, police killings in the US, extrajudicial killing by Israel)? If yes, do they authorize intervention? If no, why does the doctrine not apply?',
    'If the doctrine is structurally power-asymmetric (applies only to weak states), it is a snare capturing weak states for geostrategic extraction. If contingently unenforced against powerful states, the doctrine is latent snare that becomes active if power dynamics shift. The measurement pattern (rising extraction, plateauing at 0.38 rather than 0.60+) suggests structural asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_mechanism, empirical, 'Whether the doctrine''s enforcement is power-invariant or power-asymmetric.').

omega_variable(
    identity_locked_mechanism_for_populations,
    'Populations of targeted states are classified as identity_locked (cannot exit citizenship). How much of the constraint''s suppression operates through structural barriers (armies, sanctions) versus internalized mechanisms (acceptance of sovereignty violation legitimacy)?',
    'Post-intervention surveys and ethnographic research: do populations accept the doctrine''s legitimacy and view intervention as justified, or do they resist it as external imposition? How does acceptance change post-intervention?',
    'If suppression is primarily structural, populations carry the extraction only while the intervention mechanism operates; if internalized, the constraint persists in populations'' self-understanding even after external force leaves (more durable extraction). The measurement shows suppression plateauing; if it were structural only, it should drop sharply post-intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism_for_populations, empirical, 'Ratio of structural to internalized suppression in identity-locked populations.').

omega_variable(
    doctrine_codification_status,
    'Is conditional sovereignty a binding legal principle (codified in hard law) or an emergent practice (codified in soft law and advocacy)?',
    'Legal analysis: what is the enforcement mechanism? UN Charter article 51 authorizes self-defense but not unilateral intervention. The Responsibility to Protect (2005) is a General Assembly resolution (soft law). The Genocide Convention (1948) is hard law but limited to genocide, not all violations. The doctrine is a synthesis across these layers, not a single codified rule.',
    'If soft-law/practice-based, the doctrine''s persistence depends on continued advocacy and consent from powerful states (more extractive, more theater). If hard-law codified, it would be more binding and less contestable. Current codification is mixed, which enables selective enforcement and high theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_codification_status, conceptual, 'Whether conditional sovereignty is hard-law binding or soft-law aspirational.').

omega_variable(
    kernel_reading_ambiguity_absolute_vs_conditional,
    'This constraint instantiates the conditional-sovereignty READING of the Westphalian kernel. The absolute-sovereignty reading (no intervention legitimacy) is logically incompatible with this one — they cannot coexist in a single legal framework. Is this a case of genuine foreclosure, or do they coexist as competing factions'' commitments?',
    'Institutional analysis: do UN bodies, regional organizations, and powerful states ACT as if conditional sovereignty is legitimate (voting for interventions, authorizing referrals to ICC)? Do they ALSO act as if absolute sovereignty constrains them (voting against interventions, refusing ICC referrals)? The answer reveals whether this is foreclosure (only one reading is operative) or coexistence (both readings are live across different actors).',
    'If foreclosure: the kernel has resolved and conditional sovereignty has won (or lost). If coexistence: the kernel remains open and this constraint coexists with absolute-sovereignty constraints structurally. The measurement pattern (extractiveness rising then plateauing, theater rising linearly) suggests coexistence: if conditional had foreclosed absolute, extraction would continue rising; instead, plateauing suggests pushback from absolute-sovereignty advocates that prevents further centralization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity_absolute_vs_conditional, conceptual, 'Whether conditional and absolute sovereignty readings are logically incompatible (foreclosure) or coexist as competing commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1948, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1948, 0.1).
narrative_ontology:measurement_basis(west_tr_t1948, projected).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement_basis(west_tr_t1990, observed).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(west_tr_t2005, observed).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(west_tr_t2015, observed).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(west_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t1948, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement_basis(west_be_t1948, projected).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(west_be_t1990, observed).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement_basis(west_be_t2005, observed).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement_basis(west_be_t2015, observed).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(west_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1948, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement_basis(west_su_t1948, projected).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(west_su_t1990, observed).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement_basis(west_su_t2005, observed).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement_basis(west_su_t2015, observed).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(west_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__conditional_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_humanitarian_law_enforcement).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_security_council_mandate_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Westphalian sovereignty kernel. Absolute sovereignty and graduated sovereignty are sibling readings of the same kernel, instantiated as separate constraint stories. These three readings are structurally incompatible at the kernel level (what sovereignty IS and permits) but coexist as competing institutional and scholarly commitments. The constraint family is linked via affects_constraints to model the shared kernel and competing readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
