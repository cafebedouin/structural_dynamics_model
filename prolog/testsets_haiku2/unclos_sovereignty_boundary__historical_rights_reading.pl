% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Overlay on UNCLOS EEZ Boundaries
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   The historical-rights reading of UNCLOS sovereignty boundaries asserts
 *   that states with prior occupation, colonial administration, or
 *   long-standing usage of ocean regions retain claims that predate or
 *   supersede the convention's 200-nautical-mile EEZ framework. This reading
 *   benefits expansive claimant states (notably in Southeast Asia, South
 *   Asia, and East Africa) by allowing them to assert extended control; it
 *   harms EEZ-holding coastal states that lose exclusive authority within
 *   their formal zones; and it increases compliance costs and navigational
 *   uncertainty for third-party maritime actors. The constraint is classified
 *   as tangled_rope because it serves a coordination function (recognizing
 *   pre-existing claims within a modern framework) while simultaneously
 *   extracting through asymmetric enforcement. The beneficiary and payer
 *   roles are structurally inverted depending on the observer's seat: from a
 *   claimant state perspective, the reading is a legitimate coordination
 *   mechanism; from an EEZ-holding state perspective, it is pure extraction.
 *
 * KEY AGENTS:
 *   - Expansive claimant states (Vietnam, China, Philippines, India, Indonesia): benefit from historical-rights legitimacy to assert extended control
 *   - EEZ-holding coastal states (Thailand, Malaysia, Myanmar, Bangladesh): pay through loss of exclusive jurisdiction and reduced effective control
 *   - Third-party navigational actors (commercial shipping, naval forces, fishing fleets): face multiplied regulatory burdens and compliance costs
 *   - UNCLOS ratifying states / arbitration bodies: agenda-setters and judges who legitimize or adjudicate historical claims
 *   - Non-ratifying maritime powers: excluded from the formal framework but strategically affected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.72).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Overlay on UNCLOS EEZ Boundaries").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '17b6c968-1b94-43b7-bdeb-82751bdbb479').
narrative_ontology:cs_kernel_codification('17b6c968-1b94-43b7-bdeb-82751bdbb479', fixed_text).
narrative_ontology:cs_authority_grounding('17b6c968-1b94-43b7-bdeb-82751bdbb479', lineage).
narrative_ontology:cs_interpretation_layer_present('17b6c968-1b94-43b7-bdeb-82751bdbb479').
narrative_ontology:cs_reading_relation('17b6c968-1b94-43b7-bdeb-82751bdbb479', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('17b6c968-1b94-43b7-bdeb-82751bdbb479', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('17b6c968-1b94-43b7-bdeb-82751bdbb479', foundational, historical_occupation_precedence).
narrative_ontology:cs_axiom_status(historical_occupation_precedence, holdable).
narrative_ontology:cs_axiom_grounding('17b6c968-1b94-43b7-bdeb-82751bdbb479', historical_occupation_precedence, deontological).
narrative_ontology:cs_axiom('17b6c968-1b94-43b7-bdeb-82751bdbb479', secondary, unclos_framework_absorption).
narrative_ontology:cs_axiom_status(unclos_framework_absorption, holdable).
narrative_ontology:cs_axiom_grounding('17b6c968-1b94-43b7-bdeb-82751bdbb479', unclos_framework_absorption, conventional).
narrative_ontology:cs_reference_frame('17b6c968-1b94-43b7-bdeb-82751bdbb479', pre_unclos_historical_occupation).
narrative_ontology:cs_drift_state('17b6c968-1b94-43b7-bdeb-82751bdbb479', post_unclos_1982_formalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17b6c968-1b94-43b7-bdeb-82751bdbb479', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, historical_occupation_powers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, third_party_navigational_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States claiming historical usage rights (colonial occupation, long-term fishing grounds, traditional trade routes) that predate or extend beyond the 200-nautical-mile EEZ. These claims allow them to assert control over waters nominally outside their EEZ under UNCLOS, extracting rents through licensing, access denial, or military assertion. They benefit from the ambiguity that historical claims create — the uncertainty itself becomes a tool for asserting control without formal sovereignty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, beneficiary,
    institutional, generational, mobile, regional).

% Coastal states that hold formal EEZ rights under UNCLOS Article 57 (200-nautical-mile exclusive economic zones). The historical-rights reading undermines their exclusive control by allowing expansive claimants to overlay historical claims. They cannot exit the framework without abandoning UNCLOS protections entirely; they bear the cost of contested sovereignty and reduced effective exclusivity.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, regional).

% Commercial shipping, naval forces, fishing fleets, and scientific research vessels operating in contested waters. The historical-rights reading expands the number of jurisdictions that can assert control over any given ocean region, increasing compliance costs and navigational risk. They face demands from multiple overlapping authorities and cannot avoid the multiplied regulatory burden.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, third_party_navigational_actors, payer,
    powerful, biographical, constrained, global).

% States that ratified UNCLOS and maintain the convention as the governing framework. They enforce the historical-rights reading through diplomatic recognition of expanded claims, naval presence supporting claimant states, or arbitration rulings that grant deference to historical occupation narratives. They set the terms under which historical claims are adjudicated and legitimated.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_ratifying_states, agenda_setter,
    institutional, generational, mobile, global).

% UNCLOS Annex VII tribunals, ITLOS (International Tribunal for the Law of the Sea), and ad hoc arbitral panels that interpret disputes over EEZ boundaries and historical rights. They hold adjudicative authority but cannot unilaterally enforce verdicts; their role is to legitimize one reading over another through reasoned judgment.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_arbitration_bodies, observer,
    institutional, biographical, analytical, global).

% Major naval powers and maritime trading states that have not ratified UNCLOS (notably the United States). They operate under customary international law and are structurally excluded from the treaty-based dispute resolution framework. They could challenge the historical-rights reading through unilateral enforcement but are not parties to the formal adjudication process.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, non_ratifying_maritime_powers, excluded,
    powerful, generational, arbitrage, global).

% Oceanographic research institutions, fisheries scientists, and environmental monitoring organizations that depend on access to contested waters for data collection. The historical-rights reading expands restrictions on their access and creates permitting uncertainty. They have limited standing in sovereignty disputes and cannot participate in boundary negotiations directly.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, maritime_scientific_community, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recognizes and legitimizes pre-UNCLOS maritime claims (colonial occupation, traditional fishing grounds, historical trade routes) within the modern EEZ framework, providing a mechanism for historical occupants to assert continued rights without requiring formal sovereignty declarations.
% TRANSFER_FUNCTION: Transfers effective control over ocean regions from UNCLOS EEZ-holding coastal states to expansive claimant states; transfers navigational freedom from third-party actors to jurisdictional authorities. Moves compliance costs from claimant states to EEZ-holding states and navigational actors.
% ABSENT_VOICES: Non-ratifying maritime powers (U.S., Russian Federation in some contexts) hold strategic interest but are excluded from UNCLOS arbitration; maritime scientific community cannot participate in boundary adjudication; island nations with limited maritime history have no voice in claims made over their potential EEZ zones; subsistence fishing communities and indigenous maritime cultures whose historical usage might support claims but lack state apparatus to formalize them.
% DISAPPEARANCE_RATIONALE: If the historical-rights reading collapsed overnight, expansive claimants would lose leverage to extend control beyond their formal EEZ; EEZ-holding states would regain exclusive authority over their 200-nautical-mile zones; navigational actors would face a simpler, more predictable regulatory environment with fewer overlapping claimants. The geopolitical order of the contested regions would shift immediately.
% FOUNDING_PROBLEM: UNCLOS EEZ framework (1982) created exclusive 200-nautical-mile zones for coastal states, but did not adequately address claims by states whose historical occupation or usage predated the convention or extended beyond it. Regional powers sought mechanisms to assert historical rights without losing UNCLOS protections.
% FOUNDING_PROBLEM_CORROBORATION: Expansive claimant states (Vietnam, China, Philippines, India, Indonesia) actively assert historical rights in contemporary disputes; international arbitration cases (South China Sea, Indian Ocean) document the persistence of these claims. UNCLOS signatories continue to invoke historical precedent in boundary negotiations. Legal scholars and state practice from Southeast Asia, South Asia, and East Africa confirm the founding problem remains contested and actively litigated.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) measures how completely the constraint redistributes control over ocean regions from formal EEZ holders to historical claimants. The measurement series tracks the gradual hardening of historical-rights claims from 1982 (UNCLOS adoption, weak enforcement) through the 1990s–2010s (increasing assertiveness) to contemporary practice (0.68, approaching but not exceeding pure extraction threshold). Suppression (0.72) reflects active enforcement through naval patrols, port state control, and diplomatic assertion by claimant states against third-party navigators and competing EEZ holders. Theater ratio (0.41, moderate) indicates that while historical-rights assertions are partly theatrical (invoked selectively, exaggerated in scope), they rest on real administrative capacity and naval presence. The series shows rising extractiveness and suppression from 1982 to ~2025, then plateau, suggesting the constraint has hardened into its stable configuration. Accessibility collapse (0.58) is moderate because alternatives exist (formal boundary arbitration, UNCLOS amendment) but are costly to pursue. Resistance (0.69) is substantial because affected states and maritime actors contest the reading through legal action and operational pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant state seat, this is coordination: they are seeking to bring their historical claims within a framework that honors them. From the EEZ-holding state seat, it is extraction: they hold formal rights under UNCLOS but lose effective jurisdiction. From the navigational actor seat, it is a constraint whose burden is multiplied by institutional fragmentation. The engine computes these divergent type assessments per seat from the identical structural data; the claimed-type (tangled_rope) represents the reading's own self-understanding, while the metrics describe how asymmetric the arrangement actually is.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states hold institutional power and benefit directly from the reading — they are positioned as full beneficiaries (d near 0.0). They have mobile exit options because they could abandon historical claims and rely purely on formal EEZ, but choose not to. EEZ-holding coastal states are structurally victimized: they hold institutional power but face extraction (loss of exclusive control), making them partial targets (d mid-range, ~0.5–0.65). Third-party navigational actors (shipping, naval forces) are powerful but face compliance costs multiplied by overlapping claimants — they are targets but with exit options (rerouting, negotiation, non-compliance risk). The directionality derivation from the beneficiary/victim declarations produces this structure naturally: beneficiaries get low d (subdidy-ward), victims get high d (extraction-ward).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy trap because the founding problem (how to honor pre-UNCLOS claims within the modern framework) remains live, not because the problem is freshly pressing. The founding problem status is contested but not dead — Southeast Asian states continue to invoke historical occupation; Indian Ocean states use historical-usage arguments in contemporary disputes; African states claiming extended continental shelves reference historical resource usage. The reading persists not out of institutional inertia (theater ratio is moderate, not high) but because it solves a problem the agenda-setters care about: allowing expansive claimants to assert control without fully exiting UNCLOS. If the founding problem did become dead (i.e., all claimant states formalized their boundaries and stopped invoking history), the constraint would likely decay rapidly because its enforcement would become purely theatrical. The measurement plateau from 2025 onward (extractiveness and suppression flat at ~0.68–0.72) suggests the constraint has reached an equilibrium: hard enough to extract rents but not so hard that it triggers wholesale UNCLOS reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_claim_legitimacy,
    'What counts as sufficient ''historical occupation'' or ''usage'' to override a formal EEZ boundary under international law? When does the burden of proof shift from the claimant to the boundary holder?',
    'International arbitration case law developing explicit evidentiary standards; state practice settling on threshold criteria (e.g., pre-1900 occupation, continuous presence for 100+ years, formal colonial administration vs. sporadic fishing rights).',
    'High threshold (hard evidence of historical occupation required) would constrain the reading and shift beneficiary/victim relationships; low threshold would amplify extraction and expand the beneficiary set to include more historical claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_claim_legitimacy, conceptual, 'Evidentiary standards for historical-rights claims under international law.').

omega_variable(
    enforcement_asymmetry,
    'Why are some historical claims (China, Vietnam, India) enforced through state power and others (small-island nations, former colonized states) not? Is enforcement asymmetry inherent to the reading or contingent on power differentials?',
    'Comparative case analysis of historical claims by weak vs. strong states; investigation of whether weak-state claims receive equal arbitral deference; correlation between enforcement and military/economic capacity.',
    'If enforcement is power-contingent, the reading is classifiable as a snare (extraction via selective assertion by powerful states) rather than tangled rope (genuine coordination with asymmetry). If enforcement is evidentiary (strong claims get enforced regardless of power), the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether enforcement of historical claims correlates with state power or evidentiary strength.').

omega_variable(
    reading_foreclosure_by_arbitration,
    'Can UNCLOS arbitration bodies formally foreclose the historical-rights reading, or is the reading structurally resilient to arbitral verdicts that reject it?',
    'Track whether adverse arbitration rulings (e.g., South China Sea Tribunal 2016 ruling against expansive claims) reduce subsequent historical-rights assertions or whether claimants circumvent them through non-enforcement or alternative doctrine (e.g., sovereign immunity, customary law override).',
    'If arbitration forecloses the reading, the constraint is vulnerable to regime shift; if arbitration is unenforceable, the reading persists despite adverse verdicts and is more deeply entrenched than the legal surface suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_by_arbitration, empirical, 'Whether arbitration can definitively defeat the historical-rights reading or whether it persists through non-compliance.').

omega_variable(
    nested_sovereignty_incoherence,
    'Can two incompatible sovereignty claims (a formal EEZ under UNCLOS + a historical-rights claim under customary law) be held simultaneously by the same state, or does the reading require choosing one framework and abandoning the other?',
    'Doctrinal analysis of state declarations and treaty reservations; observation of how states practice dual claims (e.g., relying on EEZ in some contexts, historical rights in others) without internal contradiction; whether international law develops a coherent rule for nested claims.',
    'If nested claims are coherent, the reading is stable and will persist indefinitely; if incoherent, pressure will mount to abolish the reading and consolidate under one framework (likely strict_eez_reading, as it is more formalized).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nested_sovereignty_incoherence, conceptual, 'Whether the reading''s core premise (overlapping sovereignty claims) is logically sustainable or inherently unstable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(uncl_tr_t25, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(uncl_tr_t30, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(uncl_be_t25, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement(uncl_be_t30, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(uncl_su_t25, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement(uncl_su_t30, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% The historical_rights_reading is one of three structurally distinct readings of the unclos_sovereignty_boundary kernel. The strict_eez_reading treats EEZ boundaries as absolute and permits no historical overlay. The non_ratifier_enforcement_reading asserts that customary freedom-of-navigation law is independently enforceable and provides a counter-claim to historical-rights assertions. All three share the same kernel text (UNCLOS) but instantiate different ε values and different beneficiary/victim structures. The three stories form a constraint family linked via network.affects_constraints; each story is a separate JSON file. The common kernel is the ambiguity in UNCLOS Article 57 and the convention's silence on pre-existing claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
