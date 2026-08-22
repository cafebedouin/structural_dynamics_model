% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Treaty of Tordesillas: Portuguese Exploration Rights Legitimation
 *   domain: international_law/colonial_sovereignty
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) is read here as a PORTUGUESE EXPLORATION
 *   LEGITIMATION — the papal confirmation of Portuguese prior exploratory
 *   investment and navigational discovery along African coasts and eastern
 *   routes, combined with the exclusion of rival European competitors from
 *   the eastern-hemisphere trade routes and landing rights. This reading
 *   treats the treaty as a coordination mechanism among European Christian
 *   powers to prevent mutual warfare (genuine coordination function) layered
 *   over an asymmetric extraction: Portugal gains exclusive maritime access
 *   and trading monopoly in the eastern hemisphere, while rival powers are
 *   locked out and indigenous populations are treated as appropriable
 *   resources without voice. The constraint is CLAIMED as tangled_rope; the
 *   metrics describe an arrangement with real coordination (preventing
 *   European civil war) and substantial asymmetric extraction (monopoly
 *   rents, competitor exclusion, indigenous dispossession). The claim/metric
 *   alignment is deliberate — this reading's structural logic produces a
 *   tangled_rope result. A SIBLING READING (spanish_conquest_legitimation,
 *   separate constraint file) reads the same treaty kernel as a license for
 *   territorial conquest and indigenous subjugation WEST of the line; that
 *   reading would author a different beneficiary set, different victims, and
 *   different extractiveness (higher, focused on indigenous conquest rather
 *   than rival-power exclusion).
 *
 * KEY AGENTS:
 *   - Portuguese Estado da Índia: primary agenda-setter and beneficiary; collects trading monopoly rents and maritime exclusivity
 *   - Rival European powers (France, England, others): payers; locked out of eastern-hemisphere trade routes and markets
 *   - Papacy: co-agenda-setter; issues the papal bull as authority; maintains temporal legitimacy; secondary beneficiary (reinforces papal authority against rising nation-states)
 *   - Indigenous populations of eastern hemisphere: payers and powerless; their sovereignty and territorial claims are excluded from the treaty's legal framework
 *   - Portuguese crown: beneficiary; accrues political prestige and economic returns from the legitimized monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.71).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.62).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Treaty of Tordesillas: Portuguese Exploration Rights Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_sovereignty").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '8d286011-481b-4307-b4ee-4e7111bff041').
narrative_ontology:cs_kernel_codification('8d286011-481b-4307-b4ee-4e7111bff041', fixed_text).
narrative_ontology:cs_authority_grounding('8d286011-481b-4307-b4ee-4e7111bff041', lineage).
narrative_ontology:cs_interpretation_layer_present('8d286011-481b-4307-b4ee-4e7111bff041').
narrative_ontology:cs_reading_relation('8d286011-481b-4307-b4ee-4e7111bff041', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('8d286011-481b-4307-b4ee-4e7111bff041', foundational, prior_exploration_confers_exclusive_trading_rights).
narrative_ontology:cs_axiom_status(prior_exploration_confers_exclusive_trading_rights, holdable).
narrative_ontology:cs_axiom_grounding('8d286011-481b-4307-b4ee-4e7111bff041', prior_exploration_confers_exclusive_trading_rights, conventional).
narrative_ontology:cs_axiom('8d286011-481b-4307-b4ee-4e7111bff041', foundational, papal_authority_binds_european_christian_powers).
narrative_ontology:cs_axiom_status(papal_authority_binds_european_christian_powers, overridden).
narrative_ontology:cs_axiom_grounding('8d286011-481b-4307-b4ee-4e7111bff041', papal_authority_binds_european_christian_powers, theological).
narrative_ontology:cs_reference_frame('8d286011-481b-4307-b4ee-4e7111bff041', papal_temporal_arbitration_of_discovery).
narrative_ontology:cs_drift_state('8d286011-481b-4307-b4ee-4e7111bff041', contemporary_nation_state_sovereignty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8d286011-481b-4307-b4ee-4e7111bff041', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_india).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_eastern_hemisphere).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks papal and international legal confirmation of prior African coastal exploration and trading-post establishment. Uses the treaty line to claim exclusive trading rights and maritime routes eastward to India, the Spice Islands, and beyond. Enforces exclusivity by naval interdiction and diplomatic protest against rival expeditions. Collects trading monopoly rents from spice, silk, and pepper markets; justifies the demarcation as recognition of prior exploratory investment and navigational discovery, not conquest.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_india, agenda_setter,
    institutional, generational, arbitrage, global).

% Are excluded from the eastern hemisphere's maritime trade routes and coastal landing rights. France, England, and other powers must either accept Portuguese monopoly or engage in costly naval conflict and diplomatic violation. Their cost is foregone participation in the spice trade and Indian Ocean commerce during the treaty's tenure. They contest the treaty's legitimacy and seek carve-outs or alternative routes; their exit options narrow when the treaty is diplomatically upheld.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, rival_european_powers, payer,
    powerful, generational, constrained, global).

% Issues the treaty as papal bull, granting temporal authority to divide newly discovered lands between Christian European powers. Justifies this as missionary authority — the role of the Church to direct conversion efforts and prevent Christian-on-Christian conflict over pagan lands. Maintains legitimacy as arbiter of international discovery rights; reinforces papal temporal authority in an era when that authority is contested by rising nation-states.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, beneficiary).

% Are the substance of what is being partitioned — their sovereignty, trade relationships, and territorial claims are entirely excluded from the treaty framework and treated as available for appropriation by whichever European power the demarcation favors. The treaty's legitimacy framework treats their lands as unoccupied or as properly subject to Christian discovery and exploitation. Their resistance is real but structurally unheard in the legal forum where the treaty is recognized.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_populations_eastern_hemisphere, payer,
    powerless, immediate, trapped, universal).

% Accrues the political prestige and economic returns from the maritime monopoly the treaty establishes. Uses papal confirmation to strengthen claims against rival claimants in European diplomacy. Funds further expeditions on the strength of the legitimized trading routes. Benefits from the treaty as a diplomatic tool even when enforcement is imperfect.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary,
    institutional, generational, mobile, national).

% A non-agent entity: the alternative view that discovery rights should be based on actual settlement/occupation rather than papal grant. This interpretation later displaces the Tordesillas reading as nation-states consolidate sovereignty independent of papal authority.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, competing_exploration_paradigm, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, competing_exploration_paradigm).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_india).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents Christian-on-Christian naval conflict over newly discovered eastern-hemisphere trade routes and landing rights by assigning exclusive exploration and trading zones. Provides a unified legal framework that European powers can reference to avoid costly wars of attrition over access to the spice trade.
% TRANSFER_FUNCTION: Moves exclusive maritime access and trading monopoly from the common domain to the Portuguese Estado da Índia; removes rival European powers from competition in eastern-hemisphere trade for the duration the treaty holds. The papal authority grants the legitimacy; Portuguese naval power enforces the exclusion.
% ABSENT_VOICES: Indigenous populations of Africa, the Indian Ocean, India, Southeast Asia, and the Pacific have no seat at the negotiating table. Their territorial claims, existing trade networks, and sovereignty are treated as moot — the treaty presumes the lands are Christian discovery-ready and available for appropriation by papal grant. Rival European powers are excluded from negotiation but their objections are at least heard in subsequent diplomatic channels.
% DISAPPEARANCE_RATIONALE: If the treaty's legitimacy evaporated overnight, Portuguese exclusivity could not be maintained without military force alone. Rival powers would immediately mount competing expeditions; the spice trade would open to multi-power competition; Portuguese Estado da Índia would lose its monopoly rents and its legal basis for exclusion. The eastern-hemisphere trade system would reorganize around open competition rather than papal-legitimated monopoly.
% FOUNDING_PROBLEM: Risk of uncontrolled Christian-on-Christian naval warfare in newly discovered eastern-hemisphere territories; absence of a recognized legal framework to allocate exploration and trading rights without violence; multiple European powers claiming rights to the same territories and trade routes on conflicting grounds.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Portuguese and Spanish diplomatic records affirm the founding problem — the Treaty of Tordesillas was commissioned precisely to prevent naval conflict between Iberian powers that had already erupted in skirmishes over the Atlantic and were heading toward the Indian Ocean. Neutral observers (Venetian merchants, Ottoman authorities) in the period attest to the risk of escalation. However, by the 17th century, rival European powers' diplomatic challenges and the papacy's waning temporal authority suggest the problem was partly solved (Christian-Christian conflict did not escalate to the catastrophic level feared) and partly made obsolete (as nation-state sovereignty displaced papal arbitration, the treaty's legitimacy framework weakened).
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the treaty confers exclusive trading access to the spice trade and Indian Ocean commerce — a genuine asymmetry that extracts from rival European powers by fiat. The measurement series shows rising extractiveness (t=0: 0.45 → t=100: 0.68) as Portuguese enforcement machinery hardens and rival powers mount challenges that increase suppression costs; extractiveness peaks at t=150 (0.72) when enforcement infrastructure is fully developed, then declines (t=200: 0.62) as the treaty's legitimacy framework erodes and papal authority weakens in the face of rising nation-state sovereignty. Suppression follows a similar arc (0.55 → 0.82 at t=150 → 0.71 at t=200): highest when the treaty's authority is most contested and enforcement most costly. Theater ratio rises monotonically from 0.08 to 0.42 over the interval: as the founding coordination problem (preventing Christian-on-Christian war) becomes solved and then obsolete, an increasing share of enforcement activity defends the monopoly extraction rather than the original coordination function. This pattern — rising theater as original function atrophies — is diagnostic of a constraint transitioning from tangled_rope (real coordination + extraction) toward snare (extraction defended by historical legitimacy only).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Portuguese Estado da Índia) and the beneficiary (papacy) should compute as perceiving rope-or-coordination; the payer seats (rival powers and indigenous populations) should compute as perceiving snare-or-extraction. The engine computes this divergence per-seat from the structural data. The agenda-setter's claim (tangled_rope with real coordination) reflects the beneficial-outcome framing; the payer seats' experience (extraction with coordination as cover story) reflects the structural asymmetry. This divergence is exactly what the tangled_rope classification exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Portuguese Estado da Índia sits at the full-beneficiary end (d ≈ 0.1–0.2): it collects the monopoly rents, sets the enforcement rules, and has arbitrage-grade exit (can shift routes or renegotiate); it experiences this constraint as favorable coordination. Rival European powers sit at the full-target end (d ≈ 0.8–0.9): they are excluded from markets by papal and naval force, have only constrained exit (costly naval conflict or diplomatic violation), and experience the constraint as pure extraction. Indigenous populations sit beyond the target end (d ≈ 0.95): they are trapped (no exit at all), have no voice (powerless), and their entire territorial and sovereign claim set is erased by the treaty framework. The papacy sits at moderate-symmetric (d ≈ 0.5): it coordinates (prevents Christian war) and extracts (maintains temporal authority and legitimacy claims). Directionality derivation is from beneficiary/victim + exit_options + power: a powerful institutional actor with arbitrage exit sees this as beneficial coordination; a powerful actor with constrained exit sees it as extraction; a powerless actor with no exit sees it as existential suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'live' at the outset: Christian-on-Christian naval warfare over eastern-hemisphere access is a real risk. By t=100–150, the problem is partially solved (no catastrophic Christian civil war in the Indian Ocean), but the treaty's legitimacy framework is also becoming obsolete as nation-state sovereignty displaces papal authority. By t=200, the problem is both solved (Christian powers eventually compete through colonization and diplomacy, not unlimited warfare) and made irrelevant (the papacy's role as arbiter of international law is superseded). The theater_ratio rise (0.08 → 0.42) and the divergence between founding_problem_status ('live') and the actual functional decay both signal incipient mandatrophy: the treaty persists as a historical artifact and a legitimacy claim long after its original function is accomplished. An explicit mandatrophy_resolved declaration is deferred to the engine's computation (base_properties does not set mandatrophy_resolved=true here), but the measurement trajectory and commentary together flag the pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exploration_discovery_legitimacy,
    'Does prior navigational exploration and discovery of coasts confer a legitimate exclusive claim to trade rights and landing privileges in those regions, independent of actual settlement or indigenous consent?',
    'Historical examination of Portuguese actual presence and settlement on African coasts vs. the territorial scope of the claimed exclusivity; comparison to later colonial practice when nation-states displaced papal authority (discovery doctrine evolved to require ''effective occupation''). If discovery alone was deemed insufficient later, the legitimacy basis of Tordesillas shifts.',
    'If discovery alone proves insufficient as a legitimacy basis, the treaty''s authority rests entirely on papal grant, which weakens when papal temporal power erodes. If discovery is accepted as legitimate ground, the reading''s claim to explore-and-exclude stands; Portuguese prior investment is recognized. The classification (tangled_rope vs. snare) depends partly on whether the beneficiaries genuinely solved a coordination problem (Christian-Christian conflict) or merely seized rents under cover of papal authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exploration_discovery_legitimacy, conceptual, 'Whether discovery legitimizes exclusive appropriation or whether legitimacy requires settlement/occupation.').

omega_variable(
    papal_authority_erosion,
    'Does the papacy''s waning temporal authority after the Reformation and the rise of nation-states render the treaty''s legitimacy framework obsolete even while Portuguese enforcement persists?',
    'Institutional analysis of how the treaty is cited and enforced over time. If European powers cease referencing papal authority and instead appeal to discovery doctrine or nation-state rights, the treaty''s AS-WRITTEN authority has eroded even if Portuguese monopoly endures through military force.',
    'If papal authority erodes (witnessed in the measurement rise of theater_ratio), the constraint transitions from ''legitimate coordination with extraction'' to ''extraction maintained by force and historical inertia'' — a slide from tangled_rope toward snare or piton. The measurements show exactly this transition (theater rises, suppression requirement rises and then declines, indicating the legitimacy framework weakens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_erosion, empirical, 'Whether the papacy''s temporal authority persists as a legitimacy ground or is displaced by nation-state sovereignty.').

omega_variable(
    indigenous_agency_voice,
    'Are indigenous populations of the eastern hemisphere structurally victims of this constraint (treated as appropriable resources in the treaty''s legal framework) or excluded from the constraint''s operative domain because they have no recognized standing in the European legal conversation?',
    'Textual analysis of the treaty''s language: does it explicitly or implicitly treat indigenous populations as objects, resources, or non-entities? Did indigenous powers negotiate or object? Were indigenous claims recognized in diplomatic correspondence?',
    'If indigenous populations are structurally excluded from the constraint (no standing, no voice, no recognized claim to participate), they are victims of a larger colonial apparatus, not of THIS constraint as narrated. If they are treated as objects of appropriation within the treaty''s language, they are primary victims. The classification and mandatrophy analysis shift: if the constraint is defined narrowly (European power coordination only), the theater rises and mandatrophy signals; if defined broadly (indigenous subjugation as part of the legitimacy claim), extractiveness might be higher. This omega flags a reading-specific choice about scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_agency_voice, conceptual, 'Whether indigenous populations are structural victims of the Portuguese Exploration Legitimation reading or victims of a broader colonial apparatus.').

omega_variable(
    measurement_basis_treaty_enforcement,
    'Do the measurement trajectories (rising theater, peak suppression at t=150, then decline) reflect actual historical enforcement variation or are they authored projections about institutional confidence in the treaty''s legitimacy?',
    'Historical data on Portuguese naval interdiction costs, diplomatic conflicts over the treaty''s authority, and rival-power violations (privateering, smuggling expeditions) over the 200-year interval. Cross-check against archival records of enforcement effort and constraint violations.',
    'If measurements are grounded in observed enforcement variation, they support the mandatrophy signal (theater rises as legitimacy erodes). If they are projections, the pattern is hypothetical — useful for counterfactual analysis but not a description of what happened. Basis documentation (observed vs. projected in the measurement entries) makes this distinction explicit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_basis_treaty_enforcement, empirical, 'Provenance of the measurement trajectory: observed historical data vs. authored projection of institutional confidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tord_tr_t25, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 25, 0.12).
narrative_ontology:measurement(tord_tr_t50, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 50, 0.18).
narrative_ontology:measurement(tord_tr_t75, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 75, 0.24).
narrative_ontology:measurement(tord_tr_t100, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 100, 0.3).
narrative_ontology:measurement(tord_tr_t150, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 150, 0.42).
narrative_ontology:measurement(tord_tr_t200, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(tord_be_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tord_be_t25, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(tord_be_t50, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(tord_be_t75, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(tord_be_t100, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(tord_be_t150, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 150, 0.72).
narrative_ontology:measurement(tord_be_t200, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 200, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t0, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(tord_su_t25, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(tord_su_t50, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(tord_su_t75, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(tord_su_t100, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 100, 0.76).
narrative_ontology:measurement(tord_su_t150, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 150, 0.82).
narrative_ontology:measurement(tord_su_t200, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 200, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.12).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% This constraint (portuguese_exploration_legitimation) and its sibling (spanish_conquest_legitimation) decompose a single contested kernel — the Treaty of Tordesillas — into two structurally distinct constraint stories. They share the same referent (the treaty text and its legitimacy claim) but instantiate different readings with different beneficiary/victim structures, different extractiveness foci (trade monopoly vs. conquest license), and different interpretations of who bears the primary cost. The two readings coexist as live positions held by Portugal/Spain respectively; neither logically forecloses the other. Both stories link via network.affects_constraints to signal their kinship and to enable the contamination-propagation analysis (if one reading's legitimacy erodes, the sibling reading's prospects change). Separate files allow independent ε authoring and per-seat type classification; the engine's constraint-family inference uses the network link to identify them as a decomposed pair.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
