% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization as Engineered Ideological Closure
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint story captures the beneficiary_maintained_reading of the
 *   market_as_natural_default kernel: the claim that markets are the natural,
 *   default form of economic organization is not an innocent description but
 *   an actively maintained ideological closure engineered by identifiable
 *   beneficiaries (finance, large corporations, and their intellectual
 *   infrastructure). The naturalization narrative suppresses alternatives not
 *   by forgetting them but by continuous, resourced intervention — think
 *   tanks, media ownership, academic funding, revolving doors, and the
 *   institutionalization of market-mimicking rules (independent central
 *   banks, fiscal straitjackets, investor-state dispute settlement). The
 *   coordination function of actual markets is real but hijacked: the
 *   constraint presents the *ideological naturalization* as inseparable from
 *   the *coordination mechanism*, so that challenging the former appears to
 *   threaten the latter. This is a classic tangled_rope: genuine coordination
 *   (markets do allocate) fused with asymmetric extraction (the
 *   naturalization narrative captures rents for finance/corporate interests).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.68).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization as Engineered Ideological Closure").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'ff2969cf-7eaa-4cd5-b4b2-a88729702723').
narrative_ontology:cs_kernel_codification('ff2969cf-7eaa-4cd5-b4b2-a88729702723', distributed).
narrative_ontology:cs_authority_grounding('ff2969cf-7eaa-4cd5-b4b2-a88729702723', extraction).
narrative_ontology:cs_reading_relation('ff2969cf-7eaa-4cd5-b4b2-a88729702723', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff2969cf-7eaa-4cd5-b4b2-a88729702723', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('ff2969cf-7eaa-4cd5-b4b2-a88729702723', foundational, market_naturalization_is_engineered_closure).
narrative_ontology:cs_axiom_status(market_naturalization_is_engineered_closure, holdable).
narrative_ontology:cs_axiom_grounding('ff2969cf-7eaa-4cd5-b4b2-a88729702723', market_naturalization_is_engineered_closure, empirically_contingent).
narrative_ontology:cs_axiom('ff2969cf-7eaa-4cd5-b4b2-a88729702723', foundational, beneficiary_class_actively_maintains_naturalization).
narrative_ontology:cs_axiom_status(beneficiary_class_actively_maintains_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('ff2969cf-7eaa-4cd5-b4b2-a88729702723', beneficiary_class_actively_maintains_naturalization, empirically_contingent).
narrative_ontology:cs_reference_frame('ff2969cf-7eaa-4cd5-b4b2-a88729702723', engineered_naturalization_narrative).
narrative_ontology:cs_drift_state('ff2969cf-7eaa-4cd5-b4b2-a88729702723', post_2008_polycrisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ff2969cf-7eaa-4cd5-b4b2-a88729702723', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, finance_capital).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, large_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, neoliberal_think_tanks).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, wage_earners).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, precarious_workers).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, global_south_economies).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, alternative_institutional_forms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, mainstream_economic_institutions).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, policymakers).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, tina_narrative).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, shareholder_primacy_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Captures rents through financialization enabled by the naturalization narrative; controls capital allocation and influences policy via revolving doors and lobbying. Exit is trivial — capital is mobile across jurisdictions and asset classes.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, finance_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Uses the naturalization claim to resist regulation, unionization, and alternative governance models; funds think tanks and PR campaigns that reinforce market-as-natural framing. Can relocate production and jurisprudence to favorable regimes.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, large_corporations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, large_corporations, agenda_setter).

% Produces and disseminates the intellectual architecture of market naturalization (policy papers, media commentary, academic funding). Dependent on beneficiary funding; exit means loss of institutional position and funding streams.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, neoliberal_think_tanks, agenda_setter,
    organized, biographical, constrained, global).

% Bears costs of wage suppression, benefit erosion, and precarization justified by market necessity narratives. Union density decline limits collective exit; individual exit requires retraining or migration — both costly and uncertain.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, wage_earners, payer,
    organized, biographical, constrained, national).

% Faces algorithmic management, gigification, and zero-hours contracts framed as 'market flexibility.' No collective representation; exit means income loss with no safety net. Internalizes market discipline as personal failure.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, precarious_workers, payer,
    powerless, immediate, trapped, local).

% Subject to structural adjustment, capital flight threats, and trade rules that enforce market naturalization externally. Policy space constrained by IMF/World Bank conditionality and investor-state dispute settlement. Collective exit attempted via regional blocs but undermined by asymmetric power.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, global_south_economies, payer,
    moderate, generational, constrained, global).

% Cooperatives, commons-based production, public banking, and democratic planning are marginalized as 'inefficient' or 'unrealistic' by the naturalization frame. They exist but are starved of capital, legal recognition, and policy support.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, alternative_institutional_forms, excluded,
    moderate, generational, constrained, global).

% Produces critiques and alternative models but excluded from central bank advisory roles, top journals, and policy circuits. Career advancement requires conformity; exit from the paradigm means professional marginalization.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists, excluded,
    moderate, biographical, constrained, global).

% Central banks, IMF, OECD, and elite economics departments legitimize the naturalization frame through modeling assumptions and policy advice. Beneficiaries of the paradigm's dominance (funding, prestige, policy access) but also its enforcers.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, mainstream_economic_institutions, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, mainstream_economic_institutions, beneficiary).

% Enacts and maintains the legal-institutional architecture of market naturalization (independent central banks, fiscal rules, trade agreements). Constrained by capital mobility and electoral cycles; faces pressure from both beneficiary lobbies and voter backlash.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, policymakers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, policymakers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Markets genuinely coordinate complex production and distribution across strangers without central planning — a real coordination achievement that the naturalization claim appropriates.
% TRANSFER_FUNCTION: Moves political agency and distributive claims from demos to market actors: the frame transfers the power to decide 'what counts as economic necessity' from collective deliberation to putatively neutral market forces, benefiting capital at the expense of labor and public provision.
% ABSENT_VOICES: Workers in the informal sector, climate-displaced communities, future generations, and non-human ecological systems — all systematically excluded from the 'market' constituency that the naturalization claim purports to represent. Heterodox economists and alternative institutional practitioners are professionally excluded.
% DISAPPEARANCE_RATIONALE: If the naturalization constraint vanished overnight, the ideological barrier to alternatives (public ownership, planning, commons, degrowth) would collapse. Policy space would open for democratic economic governance. Capital would resist violently, but the coordinate system of 'there is no alternative' would be broken.
% FOUNDING_PROBLEM: Post-WWII reconstruction required a coordination framework for global trade and domestic stability. The neoliberal project (Mont Pelerin onward) reframed this as a problem of constraining state power and entrenching market mechanisms as the only legitimate allocator — solving the founding problem of capitalist class power restoration after the Keynesian compromise.
% FOUNDING_PROBLEM_CORROBORATION: Quinn Slobodian (Globalists), Philip Mirowski (Never Let a Serious Crisis Go to Waste), and Nancy Fraser (Cannibal Capitalism) document from outside the beneficiary set that the neoliberal project was a deliberate class project, not a spontaneous market emergence. The Mont Pelerin Society's own archives corroborate the intentional construction of the naturalization narrative.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).
:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate-high: the naturalization frame enables financialization, wage suppression, and privatization rents, but the market mechanism itself generates real surplus. Suppression (0.68) is high: alternatives are not merely neglected but actively marginalized through funding denial, legal barriers, and discursive delegitimization. Theater ratio (0.52) exceeds 0.5: more than half the constraint's visible operation is performative maintenance of the naturalization story (conferences, op-eds, 'technocratic' rule-making) rather than the coordination function it claims to protect. Accessibility collapse (0.42) is moderate: alternatives exist and are thinkable but rendered 'unrealistic' by the frame. Resistance (0.58) is significant: labor movements, global justice movements, heterodox economics, and recent policy shifts (industrial policy return, wealth tax debates) show the constraint is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the finance/corporate seat, the constraint appears as a rope — a coordination mechanism they built and maintain for global prosperity. From the precarious worker seat, it is a snare — an extraction mechanism with no exit. From the heterodox economist seat, it is a false mountain — a constructed claim masquerading as natural law. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real and fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Finance capital and large corporations are structural beneficiaries (d near 0.0): they collect rents from the frame, control its reproduction, and have arbitrage-grade exit. Think tanks are agenda_setters with constrained exit (dependent on beneficiary funding). Wage earners and precarious workers are payers: the former have constrained exit (organized but weakened), the latter are trapped (individualized, no safety net). Global South economies face constrained exit at structural level. Alternative institutional forms and heterodox economists are excluded — their voices would challenge the frame but are kept out of authoritative circuits. Mainstream institutions are observers who benefit from the paradigm (funding, prestige) but also enforce it. Policymakers are dual-positioned: they administer the constraint but face electoral pressure from its victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII coordination + class power restoration) is dead — the Keynesian compromise is gone, global trade is integrated, and the original Mont Pelerin agenda has been realized. Yet the constraint persists and intensifies (rising theater_ratio, sustained suppression). This is mandatrophy: the mandate (coordination) has atrophied but the constraint (naturalization) expands because it now serves pure extraction. The classification prevents mislabeling by detecting the coordination-extraction fusion (tangled_rope) and the theater_ratio > 0.5 signaling performative maintenance of a depleted function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engineered_vs_emergent_naturalization,
    'To what extent is market naturalization a deliberate, resourced project versus an emergent property of market societies?',
    'Network analysis of funding flows (think tanks, academic chairs, media ownership), documentary evidence of strategic planning (Mont Pelerin archives, Powell Memo, etc.), and counterfactual modeling of ideological diffusion without centralized funding.',
    'If predominantly engineered, the constraint is a tangled_rope with high suppression; if substantially emergent, it trends toward rope (coordination) or piton (inertial persistence). The beneficiary_maintained_reading commits to the engineered pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineered_vs_emergent_naturalization, conceptual, 'Whether the naturalization frame is a constructed project or an emergent cultural pattern.').

omega_variable(
    extraction_attribution_boundary,
    'How much of the measured extraction derives from the naturalization narrative specifically, versus from the market structure it naturalizes?',
    'Counterfactual policy simulations: remove the naturalization frame (allow democratic economic planning, public banking, etc.) while preserving market exchange mechanisms; measure distributive shift.',
    'If extraction drops sharply without the narrative, the frame is the primary extractive mechanism (supporting snare/tangled_rope). If extraction persists, the market structure itself is the main driver (supporting rope/mountain readings of markets).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_attribution_boundary, empirical, 'Disentangling the ideological frame''s extractive contribution from the underlying market mechanism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives primarily structural (funding barriers, legal constraints, institutional rules) or internalized (ideological capture, professional socialization, identity fusion with market frame)?',
    'Post-exit suppression trajectory: track heterodox economists and alternative practitioners who leave mainstream institutions — does their capacity to articulate alternatives recover, or does internalized suppression persist?',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint colonizes the cognitive space of its victims. If structural, exit (institutional or geographic) more fully restores agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the ideological domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_naturalization_bmr_tr_t1970, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(market_naturalization_bmr_tr_t1980, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(market_naturalization_bmr_tr_t1990, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(market_naturalization_bmr_tr_t2000, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(market_naturalization_bmr_tr_t2008, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2008, 0.51).
narrative_ontology:measurement(market_naturalization_bmr_tr_t2015, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(market_naturalization_bmr_tr_t2024, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(market_naturalization_bmr_be_t1970, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(market_naturalization_bmr_be_t1980, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1980, 0.31).
narrative_ontology:measurement(market_naturalization_bmr_be_t1990, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(market_naturalization_bmr_be_t2000, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(market_naturalization_bmr_be_t2008, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2008, 0.44).
narrative_ontology:measurement(market_naturalization_bmr_be_t2015, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement(market_naturalization_bmr_be_t2024, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(market_naturalization_bmr_su_t1970, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(market_naturalization_bmr_su_t1980, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(market_naturalization_bmr_su_t1990, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement(market_naturalization_bmr_su_t2000, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(market_naturalization_bmr_su_t2008, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(market_naturalization_bmr_su_t2015, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement(market_naturalization_bmr_su_t2024, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.08).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, financialization_as_extraction_mechanism).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, central_bank_independence_as_depoliticization).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, investor_state_dispute_settlement).

% DUAL FORMULATION NOTE:
% This reading (beneficiary_maintained) and the lapsed_alternative_reading decompose the kernel 'market as natural default' into two structurally distinct constraints: one where naturalization is active engineering (high ε, identifiable beneficiaries, active suppression) and one where it is passive forgetting (lower ε, no clear beneficiary class, decay rather than enforcement). The hybrid_amnesia_reading sits between them. All three share the kernel_id market_as_natural_default but instantiate different constraints with different ε, beneficiaries, and enforcement profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, institutional, 0.15).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, organized, 0.65).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
