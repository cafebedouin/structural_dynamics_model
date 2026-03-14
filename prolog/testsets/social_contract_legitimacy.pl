% ============================================================================
% CONSTRAINT STORY: social_contract_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_contract_legitimacy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_contract_legitimacy
 *   human_readable: Social Contract Legitimacy as Mixed Coordination-Extraction
 *   domain: political_philosophy/institutional_governance
 *
 * SUMMARY:
 *   The social contract legitimacy constraint represents the foundational
 *   claim that political obligation and state authority rest on consent of
 *   the governed. This constraint exhibits the full spectrum of DR
 *   classifications precisely because it is structurally ambiguous: it is
 *   simultaneously a genuine coordination mechanism solving collective action
 *   problems (national defense, property protection, infrastructure), a mixed
 *   coordination-extraction hybrid (benefits unequally distributed), a
 *   performative legitimacy ritual (theater masking coercion), and
 *   potentially a pure extraction mechanism (when exit is foreclosed and
 *   consent is manufactured). The constraint's evolution shows theater
 *   increasing over time (0.35 → 0.68 over ~300 years of democratic ideology
 *   development) and extractiveness rising correspondingly, suggesting that
 *   as the coordination function has been increasingly performative, the
 *   extraction mechanism has become more efficient. The modern social
 *   contract operates largely through manufacturing legitimacy rather than
 *   achieving it, making the Piton classification increasingly accurate at
 *   civilizational timescales.
 *
 * KEY AGENTS:
 *   - Subordinated Populations: Primary victims (powerless/trapped) — compelled membership with minimal exit options; experience maximum suppression through legal and social enforcement
 *   - Emergent Middle Class: Secondary victims/moderate beneficiaries (moderate/constrained) — benefit from institutional order (property rights, contract enforcement) while bearing asymmetric extraction (progressive taxation, regulatory burden); mixed experience creates Tangled Rope classification
 *   - Institutional Power Holders: Primary beneficiaries (institutional/arbitrage) — state apparatus, executive leadership, property-owning classes with ability to negotiate favorable terms or restructure institutions; arbitrage exit reduces experienced extraction
 *   - Reform Coalition: Organized agents (organized/constrained) — labor movements, civil rights organizations seeking to expand political participation and reduce extraction; provide scaffolding perspective showing conditional sunset
 *   - Legitimacy Ritual System: Institutional mechanism (institutional/arbitrage) — constitutional ceremonies, electoral systems, patriotic mythology that perform legitimation function increasingly through theater rather than functional consent-gathering
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating social-order necessity as natural law, naturalizing contingent institutional arrangements rather than examining their extraction mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_contract_legitimacy, 0.58).
domain_priors:suppression_score(social_contract_legitimacy, 0.65).
domain_priors:theater_ratio(social_contract_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_contract_legitimacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(social_contract_legitimacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(social_contract_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_contract_legitimacy, tangled_rope).
narrative_ontology:human_readable(social_contract_legitimacy, "Social Contract Legitimacy as Mixed Coordination-Extraction").
narrative_ontology:topic_domain(social_contract_legitimacy, "political_philosophy/institutional_governance").

domain_priors:requires_active_enforcement(social_contract_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_contract_legitimacy, institutional_power_holders).
narrative_ontology:constraint_beneficiary(social_contract_legitimacy, order_beneficiaries).
narrative_ontology:constraint_victim(social_contract_legitimacy, subordinated_populations).
narrative_ontology:constraint_victim(social_contract_legitimacy, excluded_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED POPULATION (SNARE) — Citizens without meaningful exit options (geographic immobility, resource barriers, legal constraint) experience the social contract as pure extraction masked as consent. Birth assigns membership; exit is legally prohibited or practically impossible. The contract enforces obligations (taxation, military service, compliance) while participation in legitimation is compulsory. Maximum experienced extraction — no genuine alternative and suppression through legal and social enforcement.
constraint_indexing:constraint_classification(social_contract_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMERGENT MIDDLE CLASS (TANGLED ROPE) — Constrained exit (emigration is costly but possible for some; internal mobility exists but carries penalties). Experiences both genuine coordination (property rights, contract enforcement, rule of law enable commerce) and extraction (progressive taxation, regulatory burden, compliance costs). Benefits from institutional order while bearing asymmetric obligations relative to power holders. The constraint both enables and constrains their flourishing.
constraint_indexing:constraint_classification(social_contract_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL POWER HOLDER (ROPE) — State apparatus, executive leadership, property-owning class. Arbitrage exit: can relocate capital, negotiate special tax treatment, or restructure institutions if the contract becomes unfavorable. Experiences the social contract primarily as coordination mechanism solving collective action problems (national defense, infrastructure, contract enforcement) that enable their wealth accumulation. Net beneficiary — extraction flows toward them. High f(d) minimization through arbitrage options.
constraint_indexing:constraint_classification(social_contract_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Labor movements, civil rights organizations, progressive political parties. Organized agents with constrained exit (can participate in the political system, can lobby for reform, but cannot unilaterally exit). See the social contract as a temporary institutional arrangement with a sunset clause: expanding political participation, redistribution mechanisms, and rights recognition represent scaffolding toward a more legitimate contract. The constraint persists during the transition but with declining suppression as norms shift. Theater remains high during this phase but with recognition that the performance is contested.
constraint_indexing:constraint_classification(social_contract_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VESTIGIAL LEGITIMACY RITUAL (PITON) — From a civilizational view, the social contract's legitimacy narrative (popular sovereignty, consent of the governed, general will) has become substantially performative. Constitutional ceremonies, electoral rituals, patriotic mythology persist through institutional inertia despite the underlying coordination mechanisms operating through coercion and property law. The legitimacy claim persists not because it works but because alternatives haven't fully replaced it. Theater ratio (0.68) reflects that legitimacy ceremonies dominate over functional consent-gathering. This is a degraded institutional form.
constraint_indexing:constraint_classification(social_contract_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of social contract is structurally necessary for any multi-agent system to solve the collective action problem and prevent war of all against all. The gap between stated legitimacy and functional order is an inherent property of governance itself. However, this perspective risks naturalizing what is actually a contingent institutional arrangement. The engine's false summit detector will identify this as naturalization of extraction mechanisms rather than genuine natural law.
constraint_indexing:constraint_classification(social_contract_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_contract_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_contract_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_contract_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_contract_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_contract_legitimacy, TR),
    TR >= 0.70.

:- end_tests(social_contract_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The social contract generates surplus value through institutional order (security, property rights, infrastructure, coordination of production) but the distribution of this surplus is radically asymmetric. Subordinated populations bear disproportionate coercive obligations (taxation, military service, compliance) relative to their share of institutional benefits. The value 0.58 reflects that extraction is substantial but not total — institutional order does provide genuine benefits (even if unequally distributed), and the constraint solves real collective action problems. Suppression (0.65): Moderate-high. Legal barriers to exit (denaturalization, deportation), economic barriers (restricted mobility, credential non-recognition), and social barriers (family separation, ostracism) make exit systematically difficult. Information control and identity fusion add psychological suppression mechanisms. Theater ratio (0.68): High and rising. Legitimacy is increasingly produced through electoral rituals, constitutional symbolism, patriotic mythology, and media framing rather than through genuine deliberative participation. As the coordination function has matured, the legitimacy narrative has become more necessary as a cover for the extraction mechanism. The rising trajectory (0.35 → 0.68) suggests that theater has become the dominant mechanism maintaining the contract.
 *
 * PERSPECTIVAL GAP:
 *   The maximal perspectival gap occurs between the subordinated population's Snare and the institutional power holder's Rope. Both experience the same constraint and the same base_properties (ε=0.58, suppression=0.65), but their classifications differ by two full types. This gap is not measurement error — it reflects genuine structural reality. The subordinated population's d ≈ 0.95 produces high f(d) ≈ 1.42; the power holder's d ≈ 0.12 produces negative f(d) ≈ -0.01. The difference is 1.43 points of f(d), corresponding to a difference in experienced extraction from unbearable to invisible. The reform coalition's Scaffold perspective demonstrates that the perspectival gap can narrow over time: as exit options materialize (through political organization, expansion of rights, or alternative institutions), trapped agents can move toward constrained and eventually mobile/arbitrage positions, shifting their classification from Snare through Tangled Rope toward Rope. The Piton perspective recognizes that the constraint's legitimacy narrative is increasingly performative, meaning the extraction mechanism relies on theater rather than genuine consent. The analytical Mountain perspective is a false summit — it naturalizes what is actually a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective follows the structural relationship of that agent to the constraint. Subordinated populations: beneficiary=no, victim=yes (primary), exit_options=trapped → d is derived as 0.95 (full target). Emergent middle class: beneficiary=partial (property rights), victim=yes (extraction), exit_options=constrained → d is derived as 0.68 (partial target). Institutional power holders: beneficiary=yes (primary), victim=no, exit_options=arbitrage → d is derived as 0.12 (partial beneficiary). Reform coalition: beneficiary=conditional (if reforms succeed), victim=yes (current extraction), exit_options=constrained, power=organized → d is derived as 0.55 (symmetric). The directionality pipeline: (beneficiary/victim + exit_options + power_atom) → d value → f(d) sigmoid → χ = ε × f(d) × σ(S). For social contracts at national scope, σ(S=national) = 1.0, so χ = ε × f(d). The variations in d explain why chi ranges from ~0.82 (subordinated: 0.58 × 1.42) to ~-0.01 (power holder: 0.58 × -0.01), despite identical ε and perspective domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The social contract legitimacy constraint resolves its mandatrophy through perspectival decomposition. The mandatrophy question is: 'Is this constraint coordination (Rope/Scaffold) or extraction (Snare/Tangled Rope)?' The analytical resolution is that it is genuinely both. From the institutional power holder's perspective, the constraint solves real collective action problems and enables wealth accumulation — genuine coordination. From the subordinated population's perspective, the constraint is compulsory membership with asymmetric extraction — genuine snare. Neither perspective is false. The constraint is actually a coordination mechanism that is used extractively. The theater ratio (0.68 and rising) indicates that as the coordination function has matured and stabilized, the legitimacy narrative has become increasingly theatrical — performing consent rather than achieving it. The Piton classification captures this insight: the constraint's functional form is Tangled Rope (genuine coordination + asymmetric extraction), but the legitimacy ritual that maintains it is increasingly performative. The measured trajectory of theater rising from 0.35 to 0.68 while extractiveness rises from 0.42 to 0.58 confirms this pattern: theater is being substituted for functional legitimation. The mandatrophy is resolved by recognizing that the constraint is Tangled Rope with a degenerating legitimacy mechanism (Piton elements increasing), requiring either genuine legitimacy reforms (scaffold sunset path) or systemic crisis (snare exit mechanism forcing renegotiation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_coercion_boundary,
    'What observable threshold distinguishes genuine political participation from performative consent manufacture?',
    'Empirical analysis of policy reversal rates when preferences diverge from institutional behavior; measurement of actual vs nominal participation in policy formation; comparison of stated preferences in surveys vs revealed preferences in exit/loyalty choices',
    'If threshold shows genuine participation: social contract is more Rope than Snare from moderate populations. If threshold shows minimal reversal capacity: contract is primarily Snare with performative consent theater. Changes classification distribution across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_coercion_boundary, empirical, 'Observable threshold distinguishing genuine consent from coerced compliance').

omega_variable(
    legitimacy_surplus_allocation,
    'Is the surplus value generated by institutional order (security, property rights, infrastructure) distributed proportionally to the groups whose obligation compliance creates it, or is distribution radically asymmetric?',
    'Comparative institutional analysis: measure welfare changes at each power level when order mechanisms are introduced vs withdrawn; track allocation of institutional benefits (security, education, infrastructure) by class/status group; compute ratio of coercive obligation burden to institutional benefit received',
    'If proportional: contract is primarily coordination with distribution conflicts (Tangled Rope becomes dominant). If radically asymmetric: extraction mechanism is confirmed (Snare becomes dominant). Changes claimed_type and measurement trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_surplus_allocation, empirical, 'Whether institutional surplus is distributed proportionally or asymmetrically across groups').

omega_variable(
    exit_option_materiality,
    'Are exit options (emigration, rebellion, opt-out) genuinely available at stated costs or are they materially foreclosed despite formal permission?',
    'Measurement of actual exit rates vs stated willingness; documentation of hidden costs (family separation, social ostracism, credential non-recognition); comparison of exit rates across income/status levels; analysis of legal barriers (exit taxes, deportation, denaturalization)',
    'If exit is genuinely available: classification shifts toward Rope/Scaffold (constrained/mobile exit options become real). If exit is systematically foreclosed: classification deepens toward Snare (trapped exit options are structurally true). Affects directionality derivation for all non-institutional agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_materiality, empirical, 'Whether stated exit options are materially available or systematically foreclosed').

omega_variable(
    legitimacy_production_mechanism,
    'Is legitimacy produced through genuine deliberative processes or manufactured through information control, emotional framing, and identity fusion?',
    'Analysis of media framing patterns; measurement of epistemic autonomy (information access, fact-checking capacity); identification of identity-fusion mechanisms in political socialization; comparison of stated vs actual grounds for compliance across populations',
    'If genuine deliberation: legitimacy is real coordination function (Rope elements are authentic). If manufactured: legitimacy is performative cover for coercion (Piton/Snare elements dominate). Changes mandatrophy resolution and theater ratio interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_production_mechanism, conceptual, 'Whether legitimacy is deliberatively produced or manufactured through information control').

omega_variable(
    identity_lock_depth,
    'To what degree are subordinated populations identity-locked into the social contract (internalized the legitimacy narrative as self-concept) versus externally constrained?',
    'Longitudinal analysis of preference changes in communities exposed to counter-narrative literature; identification of shifts in identity-fusion markers (patriotic identity, national belonging) in diaspora communities; measurement of suppression persistence after material barriers are removed',
    'If deep identity lock: exit options are identity_locked rather than trapped/constrained for many victims (changes classification for moderate power perspectives). If shallow: exit is genuinely constrained by material barriers (trapped/constrained exit options are accurate). Affects analysis of suppression mechanism and omega variables on internalization vs structuralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Degree of identity fusion with the social contract among subordinated populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_contract_legitimacy, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soccon_tr_t0, social_contract_legitimacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(soccon_tr_t150, social_contract_legitimacy, theater_ratio, 150, 0.52).
narrative_ontology:measurement(soccon_tr_t300, social_contract_legitimacy, theater_ratio, 300, 0.68).
narrative_ontology:measurement(soccon_tr_t450, social_contract_legitimacy, theater_ratio, 450, 0.74).

% Extraction over time
narrative_ontology:measurement(soccon_be_t0, social_contract_legitimacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(soccon_be_t150, social_contract_legitimacy, base_extractiveness, 150, 0.51).
narrative_ontology:measurement(soccon_be_t300, social_contract_legitimacy, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(soccon_be_t450, social_contract_legitimacy, base_extractiveness, 450, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_contract_legitimacy, enforcement_mechanism).
narrative_ontology:affects_constraint(social_contract_legitimacy, state_monopoly_violence).
narrative_ontology:affects_constraint(social_contract_legitimacy, property_rights_enforcement).
narrative_ontology:affects_constraint(social_contract_legitimacy, political_participation_barriers).
narrative_ontology:affects_constraint(social_contract_legitimacy, legitimacy_narrative_production).

% DUAL FORMULATION NOTE:
% The social contract legitimacy is upstream of specific institutional mechanisms (state violence, property enforcement, political exclusion, narrative production). This story treats legitimacy as a meta-constraint coordinating acceptance of those mechanisms. Downstream stories examine the specific extraction mechanisms (coercive enforcement, property concentration, participation barriers, information control) that the social contract legitimacy constraint legitimizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_contract_legitimacy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
