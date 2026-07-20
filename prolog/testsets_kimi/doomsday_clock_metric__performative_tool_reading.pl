% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Doomsday Clock as Performative Policy Tool
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This constraint instantiates the performative_tool_reading of the
 *   doomsday_clock_metric kernel. Under this reading, the Bulletin of the
 *   Atomic Scientists' Doomsday Clock is not an objective risk index nor a
 *   legitimate fusion of science and values, but a strategic communication
 *   device whose annual setting is chosen to maximize media impact, policy
 *   mobilization, and institutional survival. The metric extracts epistemic
 *   credibility from the scientific community and the public, converting it
 *   into symbolic capital for policy activism. The constraint's persistence
 *   depends on active narrative maintenance, media coordination, and the
 *   suppression of alternative risk-quantification frameworks that would
 *   expose the clock's disconnection from empirical indicators.
 *
 * KEY AGENTS:
 *   - bulletin_organization: Agenda setter (institutional/constrained) â administers the symbolic metric and captures institutional relevance.
 *   - policy_activism_networks: Primary beneficiary (organized/mobile) â channels the clock's urgency into political leverage and funding.
 *   - global_public: Primary target (powerless/trapped) â receives distorted risk signals and pays with anxiety and eroded trust.
 *   - scientific_epistemic_community: Secondary target (moderate/constrained) â suffers credibility spillover when the theatrics are exposed.
 *   - probabilistic_risk_experts: Excluded voice (moderate/constrained) â crowded out of the discourse by the clock's symbolic monopoly.
 *   - communication_studies_analysts: Analytical observer (analytical/analytical) â documents the rhetorical and institutional mechanics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.68).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, snare).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Doomsday Clock as Performative Policy Tool").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, '4982cd55-9543-4144-baae-9700d64bcf82').
narrative_ontology:cs_kernel_codification('4982cd55-9543-4144-baae-9700d64bcf82', formalized).
narrative_ontology:cs_authority_grounding('4982cd55-9543-4144-baae-9700d64bcf82', extraction).
narrative_ontology:cs_interpretation_layer_present('4982cd55-9543-4144-baae-9700d64bcf82').
narrative_ontology:cs_reading_relation('4982cd55-9543-4144-baae-9700d64bcf82', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('4982cd55-9543-4144-baae-9700d64bcf82', doomsday_clock_metric__hybrid_legitimacy_reading, influences).
narrative_ontology:cs_axiom('4982cd55-9543-4144-baae-9700d64bcf82', foundational, metric_subordinate_to_policy_impact).
narrative_ontology:cs_axiom_status(metric_subordinate_to_policy_impact, holdable).
narrative_ontology:cs_axiom_grounding('4982cd55-9543-4144-baae-9700d64bcf82', metric_subordinate_to_policy_impact, instrumental).
narrative_ontology:cs_axiom('4982cd55-9543-4144-baae-9700d64bcf82', foundational, epistemic_credibility_tradeable_for_mobilization).
narrative_ontology:cs_axiom_status(epistemic_credibility_tradeable_for_mobilization, holdable).
narrative_ontology:cs_axiom_grounding('4982cd55-9543-4144-baae-9700d64bcf82', epistemic_credibility_tradeable_for_mobilization, instrumental).
narrative_ontology:cs_reference_frame('4982cd55-9543-4144-baae-9700d64bcf82', policy_mobilization_instrument).
narrative_ontology:cs_drift_state('4982cd55-9543-4144-baae-9700d64bcf82', contemporary_activism_saturated_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4982cd55-9543-4144-baae-9700d64bcf82', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, policy_activism_networks).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_organization).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, global_public).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, scientific_epistemic_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the annual clock setting through its Science and Security Board, calibrating the symbolic minutes-to-midnight to maximize media uptake and policy leverage. Presents the setting as expert synthesis while strategically managing press cycles, fundraising, and institutional relevance around the announcement.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_organization, agenda_setter,
    institutional, generational, constrained, global).

% Leverage the clock's annual media spectacle to mobilize constituencies, generate donations, and pressure policymakers on nuclear, climate, and AI risk. The metric provides a compressible, emotionally legible symbol that translates complex risk into urgent political demand without requiring technical detail.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, policy_activism_networks, beneficiary,
    organized, biographical, mobile, global).

% Receives the clock setting as a proxy for existential danger, experiencing elevated anxiety and simplified threat perception. Bears the cost of distorted risk priors and declining trust in scientific institutions when the strategic gap between the clock's theatrics and empirical risk indicators becomes visible or when predicted catastrophes fail to materialize.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, global_public, payer,
    powerless, biographical, trapped, global).

% Publishes nuanced, probabilistic, and domain-specific risk assessments that are systematically overshadowed by the clock's single gestalt. Bears the cost of credibility spillover: when the clock is perceived as manipulated, public skepticism generalizes to technical risk science as a whole, eroding the authority of careful empirical work.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, scientific_epistemic_community, payer,
    moderate, generational, constrained, global).

% Produce quantitative forecasts and structured expert elicitations that offer more empirically grounded alternatives to the clock's analog symbolism. Their frameworks are structurally excluded from the dominant news cycle because the clock supplies a pre-packaged visual narrative that newsrooms can deploy without statistical literacy.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, probabilistic_risk_experts, excluded,
    moderate, generational, constrained, global).

% Document the rhetorical mechanics of the clock announcement as a strategic communication event, analyzing the divergence between the Bulletin's expert claims and the institutional incentives governing symbolic setting.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, communication_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, policy_activism_networks).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizing sustained policy attention and collective action across diffuse global publics and institutions on existential risks that lack immediate, visible harms.
% TRANSFER_FUNCTION: Moves media attention, political urgency, public anxiety, and epistemic credibility from the global public and scientific epistemic community to policy activism networks and the administering institution, converting trust into mobilization capital.
% ABSENT_VOICES: Probabilistic risk experts and quantitative forecasters whose nuanced, non-symbolic assessments are crowded out of public discourse by the clock's compressive theatricality; their exclusion sustains the constraint's monopoly on existential-risk signaling.
% DISAPPEARANCE_RATIONALE: Policy activism networks depend on the clock for annual news cycles, fundraising spikes, and symbolic leverage in legislative lobbying; without it, the concentrated media architecture of existential-risk communication would fragment into dispersed technical reports, reducing mass mobilization capacity and forcing the Bulletin to find a new business model.
% FOUNDING_PROBLEM: Cold War nuclear arsenals were expanding with minimal public comprehension or policy urgency, requiring a simple, visceral symbol to communicate imminent danger and spur arms-control action.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of science communication corroborate the original Cold War urgency, but science-studies scholars outside the Bulletin attest that the symbolic framework persisted and expanded beyond its founding nuclear-detente purpose; the Bulletin itself claims the problem is live under new risk domains, but no corroborating source outside the beneficiary set validates this extension as structurally continuous with the founding mandate.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically converts the credibility of science into political mobilization capital, extracting attention and trust without returning proportional empirical accuracy. Suppression (0.68) reflects the crowding-out of probabilistic risk discourse and the delegitimation of quantitative alternatives in mainstream media. Theater ratio (0.75) is high because the clock's primary operation is symbolic staging: the minute hand is a prop calibrated for press coverage rather than a measurement instrument. Resistance (0.45) captures growing criticism from risk quantifiers and science-communication scholars, while accessibility_collapse (0.6) registers that alternatives exist technically but are structurally inaccessible to mass publics due to media gatekeeping. The measurement series show a clear drift from low-extraction warning device (1947) to high-extraction performative institution (2025).
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin and affiliated activism networks experience the constraint as a necessary, legitimate tool for existential mobilization (low effective extraction, high coordination value). The global public and scientific epistemic community experience it as a capture of their attention and credibility for institutional and political gain (high effective extraction). The excluded probabilistic experts experience it as a suppression of their alternative frameworks. This divergence is structural: the beneficiaries control the setting apparatus and media narrative, while the targets have no exit from the information environment.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy activism networks and the Bulletin are declared beneficiaries with mobile or constrained exit options, placing them low on the directionality axis (subsidized by the constraint). The global public is declared a victim with trapped exit, placing it near full target. The scientific epistemic community is declared a victim with constrained exit and moderate power, placing it in the mid-high target range. The engine will amplify effective extraction for the trapped public and damp it for the mobile activism networks.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mislabeling because its founding problem â Cold War nuclear arms-race urgency â is structurally distinct from its current operation. The Bulletin expanded the clock's remit to climate and AI, but the performative logic (set for maximum shock/impact) persists while the original empirical anchor (nuclear minutes-to-midnight calibrated to arsenals) dissolved. Because active beneficiaries (policy activism networks and the Bulletin) continue to capture real gains from its maintenance, it is classified as snare rather than piton: the theater is high, but it is not inertial; it is actively maintained for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_vs_emergent_performativity,
    'Is the strategic manipulation of the clock setting an explicit intention of the Bulletin board, or an emergent institutional property of incentive structures and media dynamics?',
    'Archival analysis of board deliberations, leaked minutes, or whistleblower testimony revealing whether setters explicitly calibrate for media impact versus empirical risk assessment.',
    'If explicit, the constraint shifts toward deliberate institutional extraction or fraud; if emergent, it remains a snare but with systemic rather than agentic culpability, potentially altering the directionality derivation for the board seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_vs_emergent_performativity, empirical, 'Whether performativity is intentional or emergent.').

omega_variable(
    epistemic_cost_quantification,
    'Can the erosion of public trust in scientific authority caused by the clock''s theatrics be isolated and measured independently of broader anti-science sentiment?',
    'Longitudinal trust-in-science surveys correlated with clock-announcement cycles and controlled for political polarization and general media consumption.',
    'If a measurable, clock-specific trust erosion exists, the victim status of the scientific epistemic community is structurally grounded; if not, extraction from that seat may be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_cost_quantification, empirical, 'Measurability of clock-specific epistemic erosion.').

omega_variable(
    reading_indeterminacy,
    'Does the clock''s operation structurally favor the performative reading, or does the performative reading merely represent a critical observer perspective that is not inscribed in the constraint itself?',
    'Comparative analysis of board deliberation records against independent risk-indicator composites to assess whether deviation from empirical baselines is systematic and directional.',
    'If the performative reading is observer-relative rather than structurally inscribed, the constraint could reclassify toward hybrid_legitimacy; if systematic deviation is demonstrated, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indeterminacy, conceptual, 'Whether performativity is structurally inscribed or observer-relative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcptr_tr_t1947, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(dcptr_tr_t1960, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(dcptr_tr_t1984, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1984, 0.25).
narrative_ontology:measurement(dcptr_tr_t1991, doomsday_clock_metric__performative_tool_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(dcptr_tr_t2007, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2007, 0.5).
narrative_ontology:measurement(dcptr_tr_t2015, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2015, 0.65).
narrative_ontology:measurement(dcptr_tr_t2020, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2020, 0.72).
narrative_ontology:measurement(dcptr_tr_t2025, doomsday_clock_metric__performative_tool_reading, theater_ratio, 2025, 0.75).

% Extraction over time
narrative_ontology:measurement(dcptr_be_t1947, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(dcptr_be_t1960, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(dcptr_be_t1984, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1984, 0.35).
narrative_ontology:measurement(dcptr_be_t1991, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(dcptr_be_t2007, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2007, 0.5).
narrative_ontology:measurement(dcptr_be_t2015, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(dcptr_be_t2020, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(dcptr_be_t2025, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dcptr_su_t1947, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(dcptr_su_t1960, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(dcptr_su_t1984, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1984, 0.3).
narrative_ontology:measurement(dcptr_su_t1991, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement(dcptr_su_t2007, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(dcptr_su_t2015, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(dcptr_su_t2020, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(dcptr_su_t2025, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Doomsday Clock' conflates three structurally distinct constraints: an objective risk index (low extraction, expertise grounding), a hybrid legitimacy claim (moderate extraction, entanglement grounding), and a performative policy tool (high extraction, strategic manipulation). Decomposed per the epsilon-invariance principle because each reading carries a different epsilon, different beneficiary/victim structures, and different failure modes. This reading exerts contamination pressure on its siblings by eroding the credibility conditions that sustain them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
