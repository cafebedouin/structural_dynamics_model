% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary - Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story captures the 'embedded liberalism' reading of the
 *   NAFTA jurisdictional boundary: the treaty text is interpreted as creating
 *   a framework for cross-border market access that explicitly preserves
 *   domestic regulatory space for legitimate objectives (environment, labor,
 *   health) provided they are non-discriminatory. The reading treats the
 *   constraint as a tangled rope because it simultaneously coordinates market
 *   integration and extracts from domestic regulatory agencies through
 *   litigation costs and regulatory chill. It is one reading of a three-way
 *   kernel contested between capital supremacy, embedded liberalism, and
 *   sovereignty primacy.
 *
 * KEY AGENTS:
 *   - state_parties (agenda_setter/institutional): negotiated the treaty and retain formal authority, constrained by integration lock-in
 *   - cross_border_investors (beneficiary/powerful): gain market access and ISDS enforcement, mobile across jurisdictions
 *   - domestic_regulatory_agencies (payer/institutional): bear litigation costs and regulatory chill within the treaty's legitimate objectives boundary
 *   - labor_environmental_advocates (excluded/organized): affected by regulatory chill but absent from ISDS proceedings
 *   - trade_law_scholars (observer/analytical): track the drift between treaty text and tribunal practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.6).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.5).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary - Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'f9df0b12-05d1-4771-b6f9-cb3a2472e443').
narrative_ontology:cs_kernel_codification('f9df0b12-05d1-4771-b6f9-cb3a2472e443', fixed_text).
narrative_ontology:cs_authority_grounding('f9df0b12-05d1-4771-b6f9-cb3a2472e443', lineage).
narrative_ontology:cs_interpretation_layer_present('f9df0b12-05d1-4771-b6f9-cb3a2472e443').
narrative_ontology:cs_reading_relation('f9df0b12-05d1-4771-b6f9-cb3a2472e443', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9df0b12-05d1-4771-b6f9-cb3a2472e443', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('f9df0b12-05d1-4771-b6f9-cb3a2472e443', foundational, legitimate_objectives_compatible_with_trade).
narrative_ontology:cs_axiom_status(legitimate_objectives_compatible_with_trade, holdable).
narrative_ontology:cs_axiom_grounding('f9df0b12-05d1-4771-b6f9-cb3a2472e443', legitimate_objectives_compatible_with_trade, conventional).
narrative_ontology:cs_axiom('f9df0b12-05d1-4771-b6f9-cb3a2472e443', foundational, regulatory_defense_authority_preserved).
narrative_ontology:cs_axiom_status(regulatory_defense_authority_preserved, holdable).
narrative_ontology:cs_axiom_grounding('f9df0b12-05d1-4771-b6f9-cb3a2472e443', regulatory_defense_authority_preserved, conventional).
narrative_ontology:cs_reference_frame('f9df0b12-05d1-4771-b6f9-cb3a2472e443', embedded_liberalism_equilibrium).
narrative_ontology:cs_drift_state('f9df0b12-05d1-4771-b6f9-cb3a2472e443', contemporary_investment_tribunal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9df0b12-05d1-4771-b6f9-cb3a2472e443', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, cross_border_investors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, state_parties).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_principle).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, legitimate_objectives_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the treaty framework and retain formal amendment authority; bound by dispute outcomes and the political economy of integration, making exit costly and constrained.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, state_parties, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, state_parties, beneficiary).

% Access guaranteed market entry and ISDS enforcement against regulatory measures; benefit when domestic standards are chilled or narrowed by litigation risk.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, cross_border_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Must draft and defend environmental, labor, and health regulations within the treaty boundary; bear direct litigation costs and indirect regulatory chill when standards are challenged.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Advocate for strong domestic standards but lack standing in ISDS proceedings and are often sidelined in state-to-state settlement negotiations that affect regulatory space.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_environmental_advocates, excluded,
    organized, biographical, constrained, national).

% Document and analyze the divergence between the treaty text's embedded liberalism compromise and the actual trajectory of tribunal jurisprudence.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, cross_border_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable framework for cross-border market access while carving out space for non-discriminatory domestic regulation on environmental, labor, and health standards.
% TRANSFER_FUNCTION: Moves litigation risk and defensive costs from investors to domestic regulatory agencies, and moves market access stability from treaty architecture to cross-border investors.
% ABSENT_VOICES: Labor and environmental advocates lack standing in investor-state dispute settlement and are excluded from settlement negotiations that affect regulatory standards.
% DISAPPEARANCE_RATIONALE: If the treaty framework vanished, cross-border investors would lose access guarantees and ISDS enforcement; regulatory agencies would regain full standard-setting authority without litigation exposure; North American supply chains and investment patterns would reorganize around unilateral or bilateral arrangements.
% FOUNDING_PROBLEM: How to integrate North American markets for investment and trade without extinguishing democratic regulatory authority over health, safety, labor, and environment.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars and some state-party negotiators attest the problem was genuine and the embedded liberalism compromise was intentional; investor-side counsel and some economic historians attest the problem was always secondary to capital mobility and the compromise has eroded in practice.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) is moderate but significant: the constraint channels regulatory authority into a litigation framework where agencies must defend standards against investor claims. Suppression (0.50) reflects the active enforcement of dispute settlement and the structural difficulty of regulatory exit. Theater ratio (0.35) captures the growing gap between the treaty's embedded liberalism rhetoric and tribunal practice that increasingly favors capital mobility. Accessibility collapse (0.45) is moderate: alternatives like full regulatory sovereignty or complete capital supremacy exist as political positions but are hard to instantiate once the treaty is in force. Resistance (0.50) comes from regulatory agencies and civil society groups contesting the narrowing of policy space.
 *
 * PERSPECTIVAL GAP:
 *   The state-party seat experiences the constraint as a negotiated compromise it administers; the investor seat experiences it as a guarantee of market access; the regulatory-agency seat experiences it as a litigation risk and authority constraint. The engine will compute high directionality (near target) for domestic_regulatory_agencies and low directionality (near beneficiary) for cross_border_investors, producing divergent per-seat classifications despite the same treaty text.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (cross_border_investors) receive market access stability and ISDS leverage; their exit options are arbitrage-grade (can restructure investments across treaties). Victims (domestic_regulatory_agencies) bear litigation costs and chilled authority; their exit is constrained because they must operate within the treaty boundary. State parties sit between beneficiary and agenda setter: they gain from trade integration but pay in constrained sovereignty. Labor and environmental advocates are excluded from the process entirely, receiving neither coordination benefit nor direct extraction but suffering externalized regulatory chill.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids snare classification because the coordination function is structurally genuine: the treaty does create a real framework for cross-border investment that would be costly to replicate bilaterally. It avoids rope classification because the litigation mechanism asymmetrically extracts from domestic regulators. The embedded liberalism reading specifically is designed to prevent mandatrophy by preserving regulatory space, but the divergence between text and practice (practice drift) creates a latent mandatrophy risk if the legitimate objectives language becomes purely decorative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tribunal_practice_text_drift,
    'Has investor-state tribunal practice drifted toward a capital-supremacy interpretation despite the treaty''s embedded liberalism text?',
    'Systematic coding of tribunal awards measuring the success rate of legitimate-objectives defenses and the narrowing of regulatory space over time.',
    'If practice has substantially drifted, the embedded liberalism reading is more theatrical than operational, raising theater_ratio and shifting the computed type toward extraction-heavy profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_practice_text_drift, empirical, 'Gap between treaty text''s regulatory carve-outs and actual tribunal jurisprudence.').

omega_variable(
    regulatory_chill_mechanism,
    'Does the threat of ISDS litigation structurally suppress domestic regulation, or is the observed chill an epiphenomenon of political preference?',
    'Comparative regulatory-output analysis across jurisdictions with differential ISDS exposure, controlling for political cycles.',
    'If chill is structurally caused by the treaty constraint, domestic_regulatory_agencies are genuine victims with high directionality; if caused by domestic politics, the treaty''s extraction is lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_mechanism, empirical, 'Whether regulatory chill is endogenous to the treaty constraint or domestic politics.').

omega_variable(
    capital_mobility_priority,
    'Was the treaty designed primarily to secure capital mobility, with the embedded liberalism language included as political cover?',
    'Archival analysis of negotiating history and principal-agent models of state delegation to tribunals.',
    'If capital mobility was the true priority, the coordination function is cover and the constraint approaches snare; if the compromise was genuine, it remains tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_mobility_priority, conceptual, 'Founding intent ambiguity: coordination framework versus extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_emb_lib_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nafta_emb_lib_tr_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(nafta_emb_lib_tr_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(nafta_emb_lib_tr_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 18, 0.29).
narrative_ontology:measurement(nafta_emb_lib_tr_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(nafta_emb_lib_tr_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(nafta_emb_lib_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(nafta_emb_lib_be_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(nafta_emb_lib_be_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(nafta_emb_lib_be_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(nafta_emb_lib_be_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(nafta_emb_lib_be_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(nafta_emb_lib_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nafta_emb_lib_su_t6, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(nafta_emb_lib_su_t12, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(nafta_emb_lib_su_t18, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 18, 0.44).
narrative_ontology:measurement(nafta_emb_lib_su_t24, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(nafta_emb_lib_su_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
