% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization Actively Maintained by Beneficiaries
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This constraint instantiates the BENEFICIARY-MAINTAINED READING of the
 *   contested kernel 'market as natural default.' In this reading, markets
 *   are not merely historically dominant or intellectually trendy — they are
 *   actively defended as natural by identifiable beneficiary classes
 *   (financial elites, multinational corporations, neoliberal institutional
 *   networks) through institutional capture, educational gatekeeping, and
 *   discursive framing. Alternatives are not forgotten (as in the
 *   lapsed_alternative_reading); they are actively suppressed through
 *   delegitimation. The founding problem (labor power and state capacity
 *   post-1970s) prompted the active construction of market naturalization as
 *   a legitimation device. The constraint is a TANGLED ROPE: it genuinely
 *   provides coordination (markets do aggregate information and enable
 *   exchange) AND operates as extractive (naturalizing mechanisms that
 *   concentrate power and suppress alternatives). This reading attributes
 *   market dominance not to inevitable efficiency discovery but to engineered
 *   closure maintained by beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.67).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization Actively Maintained by Beneficiaries").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, '85461f97-3766-499b-b3ee-afb35f0b70f5').
narrative_ontology:cs_kernel_codification('85461f97-3766-499b-b3ee-afb35f0b70f5', distributed).
narrative_ontology:cs_authority_grounding('85461f97-3766-499b-b3ee-afb35f0b70f5', extraction).
narrative_ontology:cs_interpretation_layer_present('85461f97-3766-499b-b3ee-afb35f0b70f5').
narrative_ontology:cs_reading_relation('85461f97-3766-499b-b3ee-afb35f0b70f5', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('85461f97-3766-499b-b3ee-afb35f0b70f5', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('85461f97-3766-499b-b3ee-afb35f0b70f5', foundational, market_naturalization_actively_constructed).
narrative_ontology:cs_axiom_status(market_naturalization_actively_constructed, holdable).
narrative_ontology:cs_axiom_grounding('85461f97-3766-499b-b3ee-afb35f0b70f5', market_naturalization_actively_constructed, empirically_contingent).
narrative_ontology:cs_axiom('85461f97-3766-499b-b3ee-afb35f0b70f5', foundational, beneficiary_class_conscious_defense).
narrative_ontology:cs_axiom_status(beneficiary_class_conscious_defense, holdable).
narrative_ontology:cs_axiom_grounding('85461f97-3766-499b-b3ee-afb35f0b70f5', beneficiary_class_conscious_defense, empirically_contingent).
narrative_ontology:cs_axiom('85461f97-3766-499b-b3ee-afb35f0b70f5', secondary, alternatives_actively_suppressed_not_forgotten).
narrative_ontology:cs_axiom_status(alternatives_actively_suppressed_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('85461f97-3766-499b-b3ee-afb35f0b70f5', alternatives_actively_suppressed_not_forgotten, empirically_contingent).
narrative_ontology:cs_reference_frame('85461f97-3766-499b-b3ee-afb35f0b70f5', labor_power_eroded_post1970s_justification_required).
narrative_ontology:cs_drift_state('85461f97-3766-499b-b3ee-afb35f0b70f5', contemporary_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85461f97-3766-499b-b3ee-afb35f0b70f5', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_elites).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, neoliberal_think_tanks).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_alternative_imagination).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_movements).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, post_colonial_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control capital flows and investment direction globally. Benefit from the constraint that frames markets as natural and inevitable because it legitimates their position as stewards of inevitable forces rather than makers of consequential choices. Fund media, think tanks, and academic infrastructure that reinforce market naturalization narratives. Set the terms of public economic debate through institutional capture of regulatory bodies and policy research.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_elites, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, financial_elites, beneficiary).

% Operate under a regime where their structural dominance is framed as market outcome rather than path-dependent accumulation and political choice. Benefit from suppression of alternative ownership models (cooperatives, state enterprises, commons) that would constrain their expansion. Extract value through this framing; their scale and reach appear inevitable rather than constructed.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Funded primarily by the beneficiary class above; operate as intellectual gatekeepers and framers. Produce research, policy papers, and public narratives that present market mechanisms as natural outcomes of human nature and material scarcity rather than as constructed institutional arrangements. Their primary enforcement activity is discursive: reframing historical alternatives as failed or naive, redefining dissent as economic illiteracy.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, neoliberal_think_tanks, agenda_setter,
    institutional, biographical, constrained, global).

% Bear the cost of naturalized markets through wage suppression, precarity, and loss of bargaining power. The framing that labor markets are natural rather than constructed prevents policy alternatives (full employment guarantees, sectoral bargaining, cooperative ownership) from entering serious political consideration. Their countervailing power is weakened by the belief that alternatives are unworkable rather than merely politically defeated.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_movements, payer,
    organized, generational, constrained, global).

% The capacity to imagine and articulate alternatives to market-based resource allocation. Suppressed through educational curricula that treat markets as natural, media ownership concentrated in beneficiary hands, and institutional closure of spaces where alternative economics can be articulated. The suppression is not violence but cognitive: alternatives are not forbidden, they are rendered literally unthinkable within respectable discourse.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_alternative_imagination, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__beneficiary_maintained_reading, public_alternative_imagination).

% Constrained by the globalized constraint that market mechanisms are natural and therefore deviation requires exceptional justification. Inherited extraction hierarchies are locked in place by this framing: their position within global supply chains appears as market outcome rather than as colonial institutional structure persisting post-independence. Attempts at state-directed development or alternative ownership models are delegitimized by invocation of market naturalism.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, post_colonial_states, payer,
    moderate, generational, trapped, national).

% Excluded from major journals, funding bodies, and policy-advising positions. Their work documenting the constructed nature of markets and historical contingency of current arrangements is systematically deprioritized in academic prestige hierarchies controlled by mainstream institutions. They exist as permanent dissent, constantly having to prove that alternatives deserve consideration rather than being able to debate alternatives on level ground.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, heterodox_economists, excluded,
    moderate, biographical, constrained, global).

% Capable of empirically documenting how market naturalization operates and who benefits. They inhabit the analytical seat, able to see both the constraint's structure and its operation but without direct power to override it. Their analysis either feeds into elite think tanks (as technical refinement of existing frames) or circulates in heterodox channels with limited policy influence.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, policy_researchers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, financial_elites).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a price-signal mechanism that aggregates dispersed information and enables bilateral exchange without central planning; voluntary participation means both parties see gain; competition disciplines efficiency and innovation.
% TRANSFER_FUNCTION: Transfers legitimacy, policy space, and institutional authority from labor, state capacity, and alternative-ownership advocates toward financial beneficiaries and corporate actors. Moves the framing of distributional questions into efficiency questions. Redirects the burden of proof: alternatives must be justified; markets need only be refined.
% ABSENT_VOICES: Labor movements whose earlier proposals for coordinated economies and sectoral bargaining frameworks are excluded from serious policy consideration by being delegitimized as economically naive. Post-colonial economic development theorists who developed state-directed frameworks are archived as historical relics rather than live resources. Heterodox economists who document market contingency are excluded from major journals and policy circles. Cooperative and commons-based ownership advocates are systematically deprioritized in business and policy schools.
% DISAPPEARANCE_RATIONALE: If the constraint (active beneficiary defense of market naturalization) vanished, policy space would immediately open for serious debate about alternatives in specific domains. Housing, healthcare, education, and development could be approached as domains where alternatives (public provision, cooperative ownership, commons management) might be appropriate. The distribution of existing wealth would remain, but beneficiaries would face explicit competition against articulated alternatives rather than relying on naturalization. This is precisely why beneficiaries defend the constraint: because its disappearance enables redistribution without requiring expropriation — just different institutional choices.
% FOUNDING_PROBLEM: Post-1970s: labor's bargaining power eroded as manufacturing declined and global supply chains emerged; state capacity to redistribute was under attack from fiscal pressures and ideological resistance; a new intellectual framework was needed to justify rapid financialization, deregulation, and labor precarity. Market naturalization provided that framework: if markets are natural, deregulation is recovery of reality; if alternatives are naive, no justification is required.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and heterodox economists (Piketty, Mirowski, Graeber, Varoufakis) document the active construction of neoliberal narratives post-1970s funded by beneficiary-class institutions (Powell Memo, heritage foundation, etc.). By 2010, labor had been substantially defeated globally, state capacity rolled back, and alternative traditions archived — the original problem (needing new legitimation for labor defeat) was solved. Yet market naturalization persists and is actively defended even though its founding justification lapsed. This is mandatrophy: the constraint is maintained purely for extraction, no longer for coordination. Mainstream economists and beneficiary-class institutions attest the problem remains live (regulation and state capacity remain threats). Labor historians and heterodox economists attest it was solved through constructed narrative, not through genuine alternatives failing.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__beneficiary_maintained_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.28 (moderate-low in period 0, reflecting the constraint's coordination functions) and rises to 0.48 by period 50 — a slow, steady accumulation as the intellectual infrastructure defending naturalization becomes more elaborate and more tightly coupled to financial interests. Theater rises faster (0.42 to 0.58), indicating that as extractiveness accumulates, an increasing fraction of enforcement activity is dedicated to maintaining the naturalization story (think-tank publication, curriculum influence, media framing) rather than to the genuine coordination function itself. Suppression requirement rises most directly (0.52 to 0.67), showing that active suppression of alternatives must intensify as their costs become visible and heterodox critiques accumulate. The plateau at t=32+ (leveling of extractiveness and theater) suggests the beneficiary class reached saturation in institutional capture and media dominance — further gains require more direct political action (exemplified by post-2008 austerity discourse), which encounters higher resistance. This reading treats market naturalization as a CONSTRUCTED INSTITUTIONAL ARRANGEMENT defended by NAMED BENEFICIARIES through ACTIVE SUPPRESSION of alternatives, not as a natural discovery or a lapsed memory.
 *
 * PERSPECTIVAL GAP:
 *   From the financial_elites and beneficiary-class seats: this is genuine coordination. Markets work because people respond to incentives and voluntary exchange benefits both parties. Regulation is the artificial distortion; naturalization is recognizing reality. The constraint appears as the RESTORATION of natural order, not as construction. From the labor_movements and post_colonial_states seats: this is pure extraction dressed as nature. The naturalization actively prevents asking whether markets are appropriate in health, education, housing, or development — domains where alternatives could work but are ruled out by framing as naive. The constraint appears as CONSTRUCTED CLOSURE. From the policy_researcher seat: both are partially right. Markets do coordinate; beneficiaries do benefit from naturalization; and alternatives are actively suppressed despite real potential. The engine computes different types from these seats because the structural asymmetries are real: beneficiaries genuinely face low extraction, targets genuinely face high suppression. This is the classification's job — to make visible what the constraint's naturalization hides.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial_elites and multinational_corporations seats compute d near the beneficiary end (d ≈ 0.15–0.25): they define the constraint's terms, collect the legitimacy it provides, and face minimal exit barriers (arbitrage mobility). The neoliberal_think_tanks seat is instructive: they have role=agenda_setter (they set the frames) but their exit_options=constrained — they are funded by beneficiaries and depend on that funding stream. From their seat, the constraint is both genuinely believed (orthodoxy is not a lie) and strategically useful (they capture prestige within it). Their d sits mid-range (0.35–0.45): they're not full targets, but they're not independent either. The labor_movements and post_colonial_states seats compute high d (0.75–0.85): they bear costs (suppressed alternatives, weakened bargaining power, constrained policy space) and have low exit options (trapped and constrained). The public_alternative_imagination seat is pathological: it's not an agent (it's a capacity), yet it bears the highest cost (identity_locked suppression through education and media). This asymmetry is exactly why the constraint is extractive: it naturalizes arrangements that benefit institutional actors while suppressing the cognitive capacity to imagine different arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor power eroding post-1970s, needing new legitimation) was LIVE and genuine. The response (constructing market naturalization infrastructure) was functional — it did justify deregulation and financialization. By period 16–24 (roughly 2000–2010), the founding problem was increasingly DEAD or at least SUBSTANTIALLY SOLVED from the beneficiary perspective: labor had been thoroughly defeated, state capacity had been rolled back globally, and alternative economic traditions had largely disappeared from elite discourse. Yet the constraint persists, and theater_ratio RISES precisely as extractiveness plateaus and the founding problem dies. This is the mandatrophy signature: the constraint is maintained long after its founding justification lapsed because beneficiaries now capture value directly from it. It's no longer 'we need naturalization to justify necessary changes' and has become 'we maintain naturalization because it captures rents.' The classification PREVENTS the false reading of eternal natural law: by showing rising theater and plateauing extractiveness, it marks the constraint as a CONSTRUCTED APPARATUS now maintained purely for extraction, no longer for coordination. The TANGLED ROPE classification captures this exactly: real coordination function + active extraction + enforcement. The theater trajectory reveals the coordination function degrading over time relative to the extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_intentionality,
    'Are beneficiaries actively, consciously engineering market naturalization as a strategy, or are they defending it post-hoc after intellectual elites converged on it for other reasons?',
    'Historical archive analysis of funding decisions, policy papers, and internal correspondence from beneficiary-class institutions (foundations, corporate offices, think tank leadership) documenting intentional narrative construction vs. opportunistic adoption.',
    'If intentional: supports the BENEFICIARY-MAINTAINED reading (this constraint). If post-hoc adoption: moves toward HYBRID_AMNESIA (beneficiaries seized on a lapsed consensus). Neither rules out that active maintenance is now underway, but intentionality matters for accountability narratives and for understanding how solidified the coalition is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality, empirical, 'Whether beneficiary defense of naturalization is deliberate strategy or opportunistic rationalization of convergent intellectual trends.').

omega_variable(
    suppression_internalization_balance,
    'What fraction of the measured suppression (0.67) is structural (external barriers to heterodox work, institutional gatekeeping) vs. internalized (belief that alternatives are genuinely inferior)?',
    'Post-reform suppression trajectory: if institutional barriers to heterodox publication and funding were suddenly removed (academic publishing deconcentration, alternative funding sources), what fraction of orthodox dominance persists due to genuine belief vs. barrier removal?',
    'If suppression is mostly structural: removing beneficiary-controlled institutions (think tanks, major journals, funding gatekeepers) would rapidly diversify discourse. If mostly internalized: decades of re-education and re-socialization would be required. The balance determines both the feasibility of constraint dissolution and the temporal profile of resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_balance, empirical, 'Structural vs. internalized components of market-naturalization suppression.').

omega_variable(
    coordination_extraction_separability,
    'Are the genuine coordination functions of markets structurally inseparable from the naturalization that suppresses alternatives, or could one have coordination without the naturalization?',
    'Comparative institutional analysis: do societies that acknowledge market mechanisms as tools chosen for specific purposes (rather than natural defaults) lose the coordination benefits? Do heterodox-friendly economies show coordination deficits?',
    'If separable: the constraint is pure extraction riding on real functions; alternative framings could preserve coordination while enabling choice about domains/mechanisms. If inseparable: suppressing naturalization might degrade genuine coordination benefits. The classification depends on this: a TANGLED ROPE requires both real coordination AND extraction; a SNARE would have neither genuine coordination nor alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether market coordination and market naturalization are structurally coupled or separable.').

omega_variable(
    kernel_reading_underdetermination,
    'Is the distinction between BENEFICIARY-MAINTAINED vs. LAPSED-ALTERNATIVE vs. HYBRID-AMNESIA framings empirically determinable, or does it depend on narrative choice about which causal factor predominates?',
    'Historical causality analysis: document the timeline of (1) labor defeat, (2) think-tank funding expansion, (3) intellectual orthodox shift, (4) regulatory rollback. If (1) and (2) precede (3), beneficiary maintenance is primary. If (3) precedes (2), lapse + opportunism is primary. If (2) and (3) are tightly coupled and hard to temporally order, hybrid amnesia is primary.',
    'The three readings coexist within the same historical record. This omega acknowledges that framing choice may be underdetermined by evidence — different researchers, with different narrative commitments, might legitimately read the same history as primarily beneficiary-driven vs. primarily lapsed. The classification does not hinge on resolving this (each reading gets its own constraint story); the omega documents the underdetermination rather than hiding it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the beneficiary-maintained vs. lapsed vs. hybrid framing is empirically determined or narratively underdetermined by evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mark_tr_t8, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(mark_tr_t16, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(mark_tr_t32, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 32, 0.59).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mark_be_t8, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(mark_be_t16, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(mark_be_t32, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mark_su_t8, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(mark_su_t16, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(mark_su_t24, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(mark_su_t32, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 50, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.2).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The kernel 'market_as_natural_default' decomposes into three structurally distinct constraint stories, each instantiating a different reading of how market mechanisms came to dominate economic imagination. The BENEFICIARY-MAINTAINED reading (this story) emphasizes intentional institutional capture and active suppression. The LAPSED_ALTERNATIVE reading emphasizes historical forgetting of genuine alternatives. The HYBRID_AMNESIA reading emphasizes how initial lapse created vulnerability to capture. All three share the same referent (the standing dominance of market framing) but have different ε values and beneficiary structures because they attribute causality differently. Links: this reading influences both siblings by establishing the beneficiary institutional infrastructure that siblings must account for; the lapsed reading influences this one by establishing that forgetting preceded capture; the hybrid reading is influenced by both (it combines mechanisms from both). These are not the same constraint viewed from different angles — they are genuinely different constraints with different metrics and stakeholder asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
