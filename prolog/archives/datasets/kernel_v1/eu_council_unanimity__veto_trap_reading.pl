% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity as Minoritarian Veto Trap
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   The EU Council unanimity rule instantiates a structural trap where the
 *   requirement for consensus among all member states creates a mechanism for
 *   minoritarian extraction through credible blocking threats. This
 *   constraint story generates ONE reading of a contested kernel: the
 *   veto-trap reading emphasizes that unanimity functions primarily as an
 *   extraction mechanism enabling smaller or aligned states to extract
 *   concessions, opt-outs, and side-payments from the coalition majority by
 *   threatening to block consensus. The sibling readings
 *   (sovereignty-guarantor reading, diplomatic-capital reading) attribute
 *   different primary functions to the same rule — treating the blocking
 *   threat as legitimate minority protection or as a tool for building
 *   diplomatic relationships — but the veto-trap reading operationalizes
 *   extraction as the mechanism through which unanimity transfers value from
 *   majority preference to minority leverage. The measurement trajectory
 *   (extractiveness rising from 0.35 in 1995 to 0.62 in 2025) reflects
 *   accumulating veto use on high-stakes issues (climate policy, tax
 *   harmonization, social standards, immigration), rising costs of blocking
 *   (larger side-payments demanded), and intensifying theater (increased
 *   performance around 'protecting sovereignty' even as the substantive
 *   protection function degrades). The suppression metric rises in tandem:
 *   member states in the majority have declining alternatives to compliance
 *   (exit costs rise as EU integration deepens; defection or EU withdrawal
 *   becomes structurally more expensive). This is the veto-trap reading — the
 *   unanimity rule extracts systematically because it locks the majority into
 *   a framework where only blocking threats can shift outcomes, and blocking
 *   states exploit this asymmetry.
 *
 * KEY AGENTS:
 *   - Coalition Majority (Member States supporting the proposal): Primary victim (powerless/trapped at continental scale) — face zero-cost blocking threats; must concede, modify proposals, or offer opt-outs to unblock consensus
 *   - Blocking State Government: Primary beneficiary (organized/arbitrage) — captures value through credible veto threat; extracts side-payments, opt-outs, and agenda-setting power from majority
 *   - European Commission: Secondary beneficiary with constraints (institutional/constrained) — benefits from agenda-setting authority and knowledge asymmetries, but constrained by veto vulnerability; must accommodate blocking state preferences to maintain consensus
 *   - Reform-Oriented Member States: Secondary victim (moderate/constrained) — frustrated by unanimity gridlock; trapped by recursive lock-in (amending unanimity requires unanimity)
 *   - Large Member States: Mixed position (powerful/mobile) — benefit from coordination function (no override possible) but face extraction from smaller blocking states; retain exit options (enhanced cooperation, bilateral agreements)
 *   - Analytical Observer: Sees structure (analytical/analytical) — can identify the extraction mechanism (credible blocking threat creating asymmetric value transfer) that the sovereignty-protection reading naturalizes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.58).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.62).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity as Minoritarian Veto Trap").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'eu-unanimity-veto-trap-2026-02').
narrative_ontology:cs_kernel_codification('eu-unanimity-veto-trap-2026-02', formalized).
narrative_ontology:cs_authority_grounding('eu-unanimity-veto-trap-2026-02', extraction).
narrative_ontology:cs_interpretation_layer_present('eu-unanimity-veto-trap-2026-02').
narrative_ontology:cs_reading_relation('eu-unanimity-veto-trap-2026-02', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('eu-unanimity-veto-trap-2026-02', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('eu-unanimity-veto-trap-2026-02', foundational, veto_mechanism_primary_extraction).
narrative_ontology:cs_axiom_status(veto_mechanism_primary_extraction, holdable).
narrative_ontology:cs_axiom_grounding('eu-unanimity-veto-trap-2026-02', veto_mechanism_primary_extraction, empirically_contingent).
narrative_ontology:cs_axiom('eu-unanimity-veto-trap-2026-02', foundational, blocking_state_asymmetric_benefit).
narrative_ontology:cs_axiom_status(blocking_state_asymmetric_benefit, holdable).
narrative_ontology:cs_axiom_grounding('eu-unanimity-veto-trap-2026-02', blocking_state_asymmetric_benefit, empirically_contingent).
narrative_ontology:cs_reference_frame('eu-unanimity-veto-trap-2026-02', extracted_value_framework).
narrative_ontology:cs_drift_state('eu-unanimity-veto-trap-2026-02', contemporary_post_2015, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eu-unanimity-veto-trap-2026-02', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_state).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, veto_holder_government).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, supranational_agenda).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COALITION MAJORITY (SNARE) — Member states aligned with a proposal have no exit: withdrawal from the Council is structurally impossible; defection or splitting the EU is not available. The majority bears full cost of concessions, side-payments, and opt-outs demanded by the blocking minority. Maximum extraction — the veto holder can extract indefinitely because the majority has no credible exit threat.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: BLOCKING STATE GOVERNMENT (ROPE) — Exercises veto as a pure coordination function: credible blocking threat coordinates member state preferences toward packages all can accept. From this perspective, unanimity is a mechanism ensuring no state is overruled on fundamental interests. Net beneficiary — extraction runs toward this actor through concessions and opt-outs, but framed as legitimate protection of minority state sovereignty.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: EUROPEAN COMMISSION (TANGLED ROPE) — Functionally benefits from unanimity (acts as honest broker, gains negotiating leverage through knowledge asymmetries, can shape 'win-sets' of acceptable outcomes). Simultaneously constrained by veto power of any member state — its supranational agenda is hostage to member state blocking threats. Moderate extraction with genuine coordination function: agenda-setting authority paired with veto vulnerability creates mixed extraction-coordination dynamic.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: REFORM-ORIENTED MEMBER STATES (SCAFFOLD) — States frustrated by unanimity gridlock but locked in by treaty amendment rules (which themselves require unanimity — a lock-in trap). They perceive the unanimity rule as a temporary institutional design that should sunset. Qualified majority voting in specific policy domains (EMU, social policy) represents partial scaffolding away from unanimity. Exit is constrained by the recursion: changing unanimity requires unanimity.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: UNANIMITY NORM AS INSTITUTIONAL INERTIA (PITON) — Unanimity persists through treaty entrenchment and political mythology ('Luxembourg Compromise protects small states') long after its functional value has degraded. The rule survives through its own immobility — amending it requires unanimity, which means blocking states can veto their own disempowerment. Theater ratio 0.48: much political performance around 'protecting sovereignty' masks that the real extraction mechanism is credible blocking threats, not principled minority protection.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: LARGE MEMBER STATES (TANGLED ROPE) — Large states have genuine mobile options: threat to block within Council, exit to bilateral agreements or smaller coalitions (e.g., France-Germany directoire), or enhanced cooperation without unanimity participation. They benefit from coordination function (no major state overruled) but face moderate extraction from smaller blocking states demanding opt-outs and side-payments. Their mobility constrains the snare mechanism.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, unanimity appears as an immutable requirement for legitimate supranational authority: any supranational body that overrules member states without consent violates state sovereignty. This perspective naturalizes unanimity as a constitutional principle. However, the structural data (organized beneficiaries extracting through credible blocking threats, identifiable victims bearing concessions, active enforcement of veto power) contradicts pure natural law. The engine will detect this as a false summit: the 'inherent to sovereignty' framing naturalizes what is actually a contingent institutional choice with measurable extraction.
constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_council_unanimity__veto_trap_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, TR),
    TR >= 0.70.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The veto mechanism systematically transfers value from majority to blocking minority through: (1) direct concessions on policy content (exemptions, opt-outs, carve-outs), (2) side-payments (funding increases, regulatory flexibility in other domains), (3) agenda-setting power (blocking states shape which proposals reach the Council in the first place). The measurement trajectory shows acceleration: extractiveness rose from 0.35 (low blocking frequency, mostly ceremonial veto power) to 0.62 (regular blocking, large side-payments, high credibility of blocking threat) as EU integration increased the stakes and asymmetry between exit costs for different member states. Suppression (0.62): Moderate-high. Suppression mechanisms: (1) treaty entrenchment (unanimity rule embedded in foundational treaties; amendment requires unanimity, creating a lock-in trap), (2) exit cost escalation (withdrawal from EU is increasingly expensive as integration deepens; member states cannot credibly threaten exit on individual Council votes), (3) framing trap (legitimacy narrative around 'protecting state sovereignty' prevents reframing unanimity as an extraction mechanism). Theater ratio (0.48): Moderate. The constraint involves genuine functional elements (legitimate minority protection, preventing hegemon override) mixed with performative elements (invocations of 'sovereignty' that mask pure blocking-threat extraction; ceremonial Council sessions after real negotiations conclude in smaller directoires). Theater is lower than classical pitons because the substantive coordination function (coordinating across member state preferences) remains real, even as its distribution becomes increasingly asymmetric. The measurement shows theater rising slightly (0.32 → 0.48) as the extraction function becomes more salient and legitimacy performance becomes more important.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between this reading (veto-trap) and the sibling sovereignty-guarantor reading centers on which direction the value transfer runs: (1) Veto-trap reading: extraction flows from majority to blocking minority; beneficiary is the blocker; cost-bearer is the majority. (2) Sovereignty-guarantor reading: protection flows from veto holder to all member states equally; veto is a mutual defense mechanism; cost is the side-effect of needing consensus. The gap manifests in how each reading interprets the measurement trajectory. Veto-trap reading: extractiveness rises because blocking states are learning to exploit veto power more effectively (earlier blocking, larger concessions, more credible threats). Sovereignty-guarantor reading: extractiveness fluctuates because blocking is necessary when vital interests are threatened, not because extraction is the mechanism. The two readings assign opposite normative valence to identical behavior (veto use = extraction vs veto use = protection). The engine's false summit detection will flag the natural law perspective (mountain) as a false summit because beneficiaries are identifiable (blocking states) and extraction is measurable (side-payments, opt-outs). But the deeper perspectival gap is between veto-trap and sovereignty-guarantor readings — both are live positions in actual EU debates, and each captures something real about the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective: Blocking state (beneficiary + organized + arbitrage options) derives d ≈ 0.08 (full beneficiary position; can credibly exit negotiations without consequence because alternatives exist: bilateral deals, enhanced cooperation subsets, informal directoires). Coalition majority (victim + powerless + trapped) derives d ≈ 0.95 (full target position; zero exit options, faces blocking threat with no credible counter-threat, must accept concessions to unblock). European Commission (mixed + institutional + constrained) derives d ≈ 0.62 (moderate-high target position: constrained by veto vulnerability, but benefits from knowledge asymmetries and agenda-setting authority; can exit to selective implementation but loses supranational legitimacy). Reform-oriented states (victim + moderate + constrained) derive d ≈ 0.78 (high target: trapped by unanimity lock-in, cannot change the rule because changing it requires unanimity). Large states (mixed + powerful + mobile) derive d ≈ 0.48 (moderate: retain exit options through enhanced cooperation or informal coalitions; benefit from coordination function but face extraction pressure from smaller states). Analytical observer (d ≈ 0.72 canonical for analytical position) sees the asymmetry clearly: the structure extracts because exit costs are asymmetric, not because veto power is distributed equally.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that tangled-rope classification for this reading is correct: unanimity genuinely coordinates member state preferences (all states avoid override, all have veto power in principle), AND it systematically extracts from the majority (through blocking threats, side-payments, opt-outs). The mixture is structural, not perceptual. The snare classification from the majority's perspective is also correct: from the majority's viewpoint (powerless/trapped), they face pure extraction with no credible counter-threat. The rope classification from the blocking state's perspective is their genuine experience: they exercise legitimate leverage to protect their interests. The false summit arises from naturalizing unanimity as a law of sovereignty — the constraint is instead a contingent institutional choice with measurable extraction. The mandatrophy does NOT resolve by choosing one type; it resolves by recognizing that the perspectival gap IS the analytical content: unanimity appears as Tangled Rope from the rule's structural properties, Snare from the majority's position, Rope from the blocker's position, and Mountain (false) from naturalizing frames. All are correct within their observational context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_vs_coordination_threshold,
    'At what frequency of veto use does the coordination function collapse and pure extraction dominates?',
    'Historical frequency analysis: count vetoes per year; correlate with subjective assessments of ''legitimate protection'' vs ''obstruction'' from majority member states; measure side-payment sizes relative to policy value',
    'If vetoes < 1/year: coordination function remains credible (Rope classification more accurate). If vetoes > 3/year: extraction mechanism dominates (Snare classification for majority more accurate). Current rate: ~2.1/year on contentious issues, ~0.3/year overall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_vs_coordination_threshold, empirical, 'Threshold for veto frequency separating coordination from extraction').

omega_variable(
    side_payment_extraction_quantification,
    'What proportion of side-payments extracted via veto represent genuine accommodation of legitimate minority interests vs extractive rent-seeking by blocking states?',
    'Cost-benefit analysis of opt-outs and exemptions: (a) do they protect demonstrable vital interests of blocking states? (b) are they asymmetric (always favor the blocker)? (c) do they persist after the blocking state''s claimed concern is addressed?',
    'If >70% legitimate accommodation: veto mechanism is justified as minority protection (Rope, not Snare). If <30% legitimate: extraction mechanism is dominant (Snare for majority is correct). If 30-70% mixed: Tangled Rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(side_payment_extraction_quantification, empirical, 'Proportion of veto-extracted concessions serving legitimate minority interests').

omega_variable(
    alternative_governance_counterfactual,
    'Would qualified majority voting (QMV) in these policy domains produce systematically worse outcomes for blocking states than current unanimity outcomes?',
    'Counterfactual analysis: examine policies blocked under unanimity vs policies passed under QMV in other EU domains; assess blocking state satisfaction and policy alignment; model QMV outcomes with weighted voting reflecting state size and preferences',
    'If blocking states do systematically worse under QMV: unanimity is genuine minority protection (reduces Snare classification magnitude). If blocking states do equivalent or better: unanimity is extractive (Snare classification confirmed for majority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_counterfactual, conceptual, 'Whether QMV would systematically disadvantage blocking states').

omega_variable(
    reading_kernel_ambiguity,
    'Is unanimity fundamentally a sovereignty-guarantor mechanism (sibling reading: sovereignty_guarantor_reading) or a mechanism enabling minoritarian extraction through credible blocking threats (this reading: veto_trap_reading)?',
    'Axiomatic: the two readings assign opposite roles to the veto. Sovereignty guarantor reading: veto protects state autonomy from supranational override; extraction is a byproduct, not the mechanism. Veto trap reading: veto IS the extraction mechanism; sovereignty protection is the legitimacy cover story. Resolution requires examining which actors benefit systematically and whether the distribution of benefits matches the stated protective function.',
    'If sovereign protection is the primary function: reclassify to Rope/Tangled Rope with lower ε. If extraction is the primary function: maintain Tangled Rope/Snare with higher ε for majority. This omega documents the irreducible interpretive disagreement between the two readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether unanimity functions primarily as sovereignty protection or as minoritarian extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(euveto_theater_1995, eu_council_unanimity__veto_trap_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(euveto_theater_2005, eu_council_unanimity__veto_trap_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(euveto_theater_2015, eu_council_unanimity__veto_trap_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(euveto_theater_2025, eu_council_unanimity__veto_trap_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(euveto_extractiveness_1995, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(euveto_extractiveness_2005, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(euveto_extractiveness_2015, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(euveto_extractiveness_2025, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(euveto_suppression_1995, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(euveto_suppression_2005, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(euveto_suppression_2015, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(euveto_suppression_2025, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, qualified_majority_voting_institutional_capacity).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, enhanced_cooperation_coalition_fragmentation).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule is ONE constraint that three different readings conceptualize as different constraint stories: veto_trap_reading (this file) emphasizes extraction mechanism; sovereignty_guarantor_reading emphasizes protection function; diplomatic_capital_reading emphasizes relationship-building. The three readings share identical base_properties (same extractiveness, suppression, theater scores) but reflect different core normative framings of how the rule functions. This is kernel decomposition, not constraint decomposition per the ε-invariance principle. The ε is stable across readings (0.58). The readings differ in which causal mechanism they emphasize as primary — which is a framing choice, not an observational one. Network links connect all three readings to enable contrastive analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
