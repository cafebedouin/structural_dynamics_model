% ============================================================================
% CONSTRAINT STORY: strategic_victory_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strategic_victory_narrative, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: strategic_victory_narrative
 *   human_readable: Strategic Victory Narrative Construction
 *   domain: geopolitical/information_warfare
 *
 * SUMMARY:
 *   The Iranian state constructs and enforces a narrative framing regional
 *   military and economic costs as strategic victories by redefining success
 *   metrics away from verifiable indicators. The constraint coordinates
 *   domestic and regional constituencies around this interpretation while
 *   extracting from factual accountability and from populations bearing the
 *   costs. The claim is tangled_rope (genuine coordination function plus
 *   asymmetric extraction); the metrics describe rising extraction and
 *   theater as the gap between narrative claims and observable reality widens
 *   over the interval.
 *
 * KEY AGENTS:
 *   - iranian_state_apparatus: Agenda-setter (institutional/constrained) — controls narrative construction and enforcement machinery
 *   - domestic_regime_stability: Beneficiary (institutional/identity_locked) — depends on the narrative for cohesion
 *   - regional_proxy_networks: Beneficiary (organized/constrained) — sustained by the victory framing
 *   - iranian_domestic_opposition: Payer (moderate/trapped) — bears economic costs, denied contradictory information
 *   - regional_populations_under_proxy_control: Payer (powerless/trapped) — bears direct costs of the strategy
 *   - factual_accountability: Payer (non-agent/analytical) — epistemic norm systematically violated
 *   - western_intelligence_analysts: Observer (institutional/analytical) — tracks narrative-reality gap
 *   - independent_regional_journalists: Excluded (moderate/constrained) — would report costs, systematically barred
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strategic_victory_narrative, 0.68).
domain_priors:suppression_score(strategic_victory_narrative, 0.72).
domain_priors:theater_ratio(strategic_victory_narrative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strategic_victory_narrative, extractiveness, 0.68).
narrative_ontology:constraint_metric(strategic_victory_narrative, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(strategic_victory_narrative, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(strategic_victory_narrative, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(strategic_victory_narrative, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strategic_victory_narrative, tangled_rope).
narrative_ontology:human_readable(strategic_victory_narrative, "Strategic Victory Narrative Construction").
narrative_ontology:topic_domain(strategic_victory_narrative, "geopolitical/information_warfare").

domain_priors:requires_active_enforcement(strategic_victory_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strategic_victory_narrative, iranian_state_legitimacy).
narrative_ontology:constraint_beneficiary(strategic_victory_narrative, domestic_regime_stability).
narrative_ontology:constraint_beneficiary(strategic_victory_narrative, regional_proxy_networks).
narrative_ontology:constraint_victim(strategic_victory_narrative, factual_accountability).
narrative_ontology:constraint_victim(strategic_victory_narrative, iranian_domestic_opposition).
narrative_ontology:constraint_victim(strategic_victory_narrative, regional_populations_under_proxy_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls state media, educational curriculum, and official historical narrative. Frames military setbacks and economic costs as strategic victories by redefining success metrics from territorial/material outcomes to 'resistance axis cohesion' and 'deterrence credibility'. Enforces the narrative through media censorship, academic gatekeeping, and suppression of contradictory evidence. The narrative is existentially necessary: admitting strategic defeat would delegitimize the revolutionary ideology the regime grounds its authority in.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, iranian_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% The regime's internal cohesion depends on the narrative that its regional strategy is succeeding despite costs. Without the victory framing, domestic constituencies would question why sanctions, economic hardship, and military losses are being sustained. The narrative converts observable costs into investments in a larger strategic picture only the regime can see.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, domestic_regime_stability, beneficiary,
    institutional, biographical, identity_locked, national).

% Hezbollah, Iraqi militias, Houthi forces, and other aligned groups benefit from the narrative that they are part of a winning 'axis of resistance' rather than costly dependencies. The victory framing sustains recruitment, morale, and external funding by portraying setbacks as tactical adjustments within a strategically ascendant position.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, regional_proxy_networks, beneficiary,
    organized, generational, constrained, regional).

% Bear the economic costs of the regional strategy while being denied access to contradictory information. They see sanctions, inflation, and resource diversion but are told these are the price of strategic dominance. Dissent is suppressed; alternative framings are criminalized as foreign propaganda. Their exit options are emigration (costly and selective) or internal silence.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, iranian_domestic_opposition, payer,
    moderate, biographical, trapped, national).

% Lebanese, Syrian, Iraqi, and Yemeni civilians living under Iranian-aligned militias experience the costs of the regional strategy directly: infrastructure destruction, economic collapse, governance failure. The victory narrative is imposed on them by proxy forces; their own assessments of the situation are irrelevant to the narrative's construction and maintenance.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, regional_populations_under_proxy_control, payer,
    powerless, biographical, trapped, regional).

% The epistemic norm that claims should be evaluated against verifiable evidence. The strategic victory narrative systematically decouples claims from observable indicators: military losses are reframed as 'strategic depth', economic costs as 'resistance investment', proxy attrition as 'forward defense'. The constraint extracts from factual accountability by making it structurally irrelevant to the narrative's persistence.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, factual_accountability, payer,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(strategic_victory_narrative, factual_accountability).

% Track the gap between Iranian narrative claims and verifiable military, economic, and political indicators. They produce assessments for policymakers that attempt to separate the performance from the reality, but have no direct stake in the narrative's domestic or regional function.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, western_intelligence_analysts, observer,
    institutional, biographical, analytical, global).

% Would report on the costs and failures the narrative obscures, but are systematically excluded from Iranian state media, face harassment and detention when operating in proxy-controlled areas, and are framed as foreign agents when they contradict the official narrative. Their exclusion is necessary for the narrative's maintenance.
narrative_ontology:constraint_stakeholder(strategic_victory_narrative, independent_regional_journalists, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates domestic and regional constituencies around a shared interpretation of costly events, preventing fragmentation of the resistance axis and maintaining regime legitimacy despite observable setbacks.
% TRANSFER_FUNCTION: Moves the interpretive authority over strategic success from verifiable indicators (territorial control, economic health, military capability) to narrative control (who gets to define what counts as victory), concentrating that authority in the Iranian state apparatus while diffusing the costs across domestic and regional populations.
% ABSENT_VOICES: Independent regional journalists, Iranian domestic opposition with access to contradictory evidence, and populations in proxy-controlled territories who experience the costs directly are systematically excluded from the narrative construction process.
% DISAPPEARANCE_RATIONALE: If the narrative constraint vanished, the Iranian state would face immediate legitimacy crisis as domestic constituencies demanded accountability for costs without the victory framing; regional proxy networks would lose recruitment and morale infrastructure; and the regime would need to either produce verifiable strategic gains or acknowledge the strategy's failure.
% FOUNDING_PROBLEM: Post-revolutionary Iranian state needed to justify regional interventions and their costs to domestic constituencies while maintaining ideological coherence of the resistance narrative in the face of military and economic setbacks.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live as attested by continued regional interventions and their costs. Western intelligence analysts and independent regional observers corroborate that the narrative function is actively maintained because the underlying legitimacy problem persists: the regime cannot withdraw from regional commitments without ideological collapse, and cannot sustain them without the victory framing.
narrative_ontology:disappearance_verdict(strategic_victory_narrative, world_rearranges).
narrative_ontology:founding_problem_status(strategic_victory_narrative, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(strategic_victory_narrative, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2025-01-18',
    'strategic_communications_geopolitical_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(strategic_victory_narrative, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strategic_victory_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(strategic_victory_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(strategic_victory_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the narrative systematically decouples claims from verifiable evidence, concentrating interpretive authority while diffusing costs. Suppression is high (0.72) because the constraint requires active enforcement: media censorship, academic gatekeeping, criminalization of dissent, and exclusion of independent observers. Theater ratio is moderate-high (0.58) because a growing share of the narrative's maintenance is performative: the victory claims become less tethered to observable indicators over time, requiring more elaborate justification and more aggressive suppression of contradictory evidence. Accessibility collapse is moderate (0.48) because alternative framings remain conceptually available to those with access to external information; the constraint's power comes from suppression, not from making alternatives unthinkable. Resistance is high (0.71) because domestic opposition, regional populations, and excluded journalists actively contest the narrative despite suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the iranian_state_apparatus seat, the constraint is existentially necessary coordination: it holds the resistance axis together and maintains regime legitimacy in the face of real external threats. From the payer seats (domestic opposition, regional populations), the same structure operates as enforced extraction: they bear costs while being denied the information that would let them evaluate whether those costs are justified. From the observer seat (western analysts), the constraint is a measurable gap between claims and indicators. The engine computes these divergences from the structural data; the tangled_rope claim reflects that both the coordination function and the asymmetric extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   The iranian_state_apparatus is the primary beneficiary and agenda-setter: it collects legitimacy and maintains authority through the narrative, with exit options constrained by ideological lock-in (abandoning the narrative means abandoning the revolutionary ideology). Domestic_regime_stability and regional_proxy_networks are beneficiaries with identity_locked and constrained exit respectively: their organizational coherence depends on the victory framing. Iranian_domestic_opposition and regional_populations_under_proxy_control are trapped payers: they bear the costs and are denied exit or voice. Factual_accountability is a non-agent victim: the epistemic norm is systematically violated. Independent_regional_journalists are excluded: their participation would undermine the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy: the founding problem (maintaining regime legitimacy and resistance-axis cohesion despite costs) remains live. The narrative is not a vestigial structure persisting after its function died; it is an actively maintained response to an ongoing legitimacy challenge. The rising theater_ratio reflects that the narrative's claims are becoming more decoupled from reality, not that the function has atrophied. This is Goodhart drift (the metric substitution pattern), not piton decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_delusion,
    'Is the strategic victory narrative a functional coordination mechanism that genuinely aligns constituencies around a defensible long-term strategy, or is it a collective delusion that prevents the regime from adjusting to strategic failure?',
    'Longitudinal outcome analysis: if the narrative enables the regime to sustain regional influence and domestic stability over decades despite costs, the coordination function is real; if the narrative leads to catastrophic miscalculation or collapse, it was extractive self-deception.',
    'If the narrative is functional coordination, the extraction is the price of holding a coalition together under adversity; if it is delusion, the extraction is pure cost with no offsetting benefit, and the constraint should reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_delusion, empirical, 'Whether the narrative''s coordination function is genuine or self-deceptive.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression primarily structural (external enforcement preventing access to contradictory information) or internalized (domestic constituencies genuinely believe the narrative and suppress their own doubts)?',
    'Post-regime-change information access: if suppression persists after enforcement machinery is removed, it was substantially internalized; if it collapses immediately, it was structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the identity_locked exit option for domestic_regime_stability is more binding than it appears. If purely structural, the constraint is more fragile than the suppression metric indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    victory_metric_ambiguity,
    'What constitutes ''strategic victory'' in the Iranian framing: is it a coherent alternative metric (influence, deterrence, axis cohesion) or is it an unfalsifiable claim that redefines success post-hoc to match whatever outcomes occur?',
    'Falsifiability test: identify what observable outcomes the Iranian state would accept as strategic defeat; if no such outcomes exist, the victory claim is unfalsifiable and purely extractive.',
    'If the victory metric is coherent and falsifiable, the narrative has genuine epistemic content and the coordination function is stronger; if unfalsifiable, the narrative is pure performance and the theater_ratio should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victory_metric_ambiguity, conceptual, 'Whether the victory framing has falsifiable content or is post-hoc rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strategic_victory_narrative, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stra_tr_t0, strategic_victory_narrative, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stra_tr_t5, strategic_victory_narrative, theater_ratio, 5, 0.43).
narrative_ontology:measurement(stra_tr_t10, strategic_victory_narrative, theater_ratio, 10, 0.48).
narrative_ontology:measurement(stra_tr_t15, strategic_victory_narrative, theater_ratio, 15, 0.52).
narrative_ontology:measurement(stra_tr_t20, strategic_victory_narrative, theater_ratio, 20, 0.55).
narrative_ontology:measurement(stra_tr_t25, strategic_victory_narrative, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(stra_be_t0, strategic_victory_narrative, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(stra_be_t5, strategic_victory_narrative, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(stra_be_t10, strategic_victory_narrative, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(stra_be_t15, strategic_victory_narrative, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(stra_be_t20, strategic_victory_narrative, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(stra_be_t25, strategic_victory_narrative, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stra_su_t0, strategic_victory_narrative, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stra_su_t5, strategic_victory_narrative, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(stra_su_t10, strategic_victory_narrative, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(stra_su_t15, strategic_victory_narrative, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(stra_su_t20, strategic_victory_narrative, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stra_su_t25, strategic_victory_narrative, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(strategic_victory_narrative, proxy_network_legitimacy).
narrative_ontology:affects_constraint(strategic_victory_narrative, domestic_information_control).
narrative_ontology:affects_constraint(strategic_victory_narrative, regional_sectarian_narrative).

% DUAL FORMULATION NOTE:
% This constraint is one component of a larger information-warfare apparatus. It affects proxy_network_legitimacy (the narrative sustains proxy morale and recruitment), domestic_information_control (the narrative requires censorship infrastructure), and regional_sectarian_narrative (the resistance framing is embedded in sectarian identity claims). Each of these could be modeled as separate constraints with their own ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strategic_victory_narrative, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
