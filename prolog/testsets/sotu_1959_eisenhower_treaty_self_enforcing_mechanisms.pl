% ============================================================================
% CONSTRAINT STORY: sotu_1959_eisenhower_treaty_self_enforcing_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, []).

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
 *   constraint_id: sotu_1959_eisenhower_treaty_self_enforcing_mechanisms
 *   human_readable: Self-Enforcing Mechanisms Requirement in Cold War Treaties with Communist States
 *   domain: foreign_policy/international_agreements
 *
 * SUMMARY:
 *   Eisenhower's 1959 SOTU address establishes a structural constraint on
 *   U.S. treaty-making with Communist adversaries: any agreement must contain
 *   internal self-enforcing mechanisms (inspection rights, automatic
 *   penalties, technical verification) rather than relying on good faith
 *   compliance. This reflects Cold War security doctrine that treats
 *   Communist states as inherently untrustworthy. The constraint coordinates
 *   U.S. security interests with the ability to negotiate agreements despite
 *   mutual distrust, but extracts significant diplomatic costs: it narrows
 *   the menu of possible agreements, precludes confidence-building measures
 *   that might reduce adversarial relationships, and establishes an
 *   institutional precedent that makes faith-based cooperation appear naive.
 *   The constraint exhibits mixed characteristics: it solves a genuine
 *   coordination problem (how to make agreements with adversaries) while
 *   imposing extractive constraints (U.S. maintains verification dominance,
 *   Soviet Union accepts transparency subordination). The theater ratio
 *   increases over the interval as the ideological framing ('Communist
 *   untrustworthiness') becomes more entrenched despite evidence of mixed
 *   compliance across different Communist states and periods.
 *
 * KEY AGENTS:
 *   - U.S. Executive and Strategic Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains verifiable agreements and security assurance during verification window
 *   - Soviet Union (Negotiating Counterparty): Primary victim (powerless/trapped) — must accept self-enforcing mechanisms as precondition; experiences asymmetric transparency requirement
 *   - Soviet Diplomatic Apparatus: Secondary institutional actor (institutional/constrained) — constrained by verification requirement; also benefits from enabling serious negotiations
 *   - Allied States and Non-Aligned Nations: Secondary victims (moderate/constrained) — benefit from U.S. commitment to verifiable agreements but bear costs of precedent-setting constraint on their own flexibility
 *   - International Verification Regime: Organized institutional developer (organized/mobile) — scaffold perspective; sees self-enforcing mechanisms as temporary until multilateral verification bodies mature
 *   - Cold War Ideological System: Institutional maintenance mechanism (institutional/arbitrage) — sustains constraint through framing of inherent Communist untrustworthiness; theater ratio increases as empirical grounding weakens
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, 0.52).
domain_priors:suppression_score(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, 0.65).
domain_priors:theater_ratio(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, tangled_rope).
narrative_ontology:human_readable(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, "Self-Enforcing Mechanisms Requirement in Cold War Treaties with Communist States").
narrative_ontology:topic_domain(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, "foreign_policy/international_agreements").

domain_priors:requires_active_enforcement(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, u_s_strategic_security).
narrative_ontology:constraint_beneficiary(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, verification_technology_development).
narrative_ontology:constraint_victim(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, diplomatic_flexibility).
narrative_ontology:constraint_victim(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, confidence_building_measures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET NEGOTIATING POSITION (SNARE) — Structurally trapped. Must accept self-enforcing verification mechanisms as precondition for any U.S. treaty, yet these mechanisms extract asymmetric transparency and constraint on future flexibility. No exit option: rejection means no agreement at all, but acceptance subordinates sovereignty to embedded verification regimes. High suppression: inability to negotiate away the verification requirement. Maximum extraction from the Soviet perspective — the constraint is a precondition imposed without reciprocal structure.
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED AND NON-ALIGNED STATES (TANGLED ROPE) — Constrained agents with mixed incentives. Benefit from U.S. commitment to verifiable agreements (coordination function: reliable counterparty reduces war risk). But also bear costs: the self-enforcing mechanism precedent limits their own diplomatic flexibility, raises verification burdens on their agreements, and signals to U.S. that faith-based commitments are insufficient. Moderate extraction: these agents both benefit from and bear costs under the constraint.
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: U.S. STRATEGIC SECURITY (ROPE) — Primary beneficiary with full arbitrage capacity. The constraint solves a genuine coordination problem: how to make agreements with adversaries who have demonstrated treaty violation. Self-enforcing mechanisms (inspection rights, automatic penalties, technical verification) provide coordination between U.S. security interests and agreement stability. Net beneficiary — extraction runs toward this agent. Classification as Rope reflects that the constraint addresses a real problem and provides genuine coordination value, not pure extraction.
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET DIPLOMATIC APPARATUS (TANGLED ROPE) — Institutional actor constrained by the U.S. requirement. Must accept verification mechanisms to achieve any agreement on nuclear arms, spheres of influence, or strategic parity. The constraint extracts transparency and limits future treaty evasion capacity — genuine extraction. But it also coordinates on mutual verification, reducing war-by-miscalculation risk and enabling negotiations that would otherwise be impossible. Mixed: extraction is real and asymmetric, but coordination function is also genuine. Soviet diplomats experience this as the price of being taken seriously as a negotiating partner.
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL VERIFICATION REGIME (SCAFFOLD) — Organized actors (IAEA, emerging arms control bodies, UN verification mechanisms) see the self-enforcing requirement as a temporary scaffold that builds toward a more robust international legal order. As verification technology matures and multinational verification bodies normalize (IAEA succeeds, arms control bodies expand), the self-enforcing requirement becomes less necessary because verification becomes institutionalized and generalized. Sunset logic: the constraint has a built-in obsolescence as the international system develops more neutral verification capacity. Mobile exit: as multilateral verification regimes mature, the bilateral self-enforcing mechanisms can be phased out.
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR IDEOLOGICAL SYSTEM (PITON) — The constraint is maintained through ideological theater: 'Communism is inherently untrustworthy; therefore all agreements must be self-enforcing.' The theatrical component persists even after evidence shows that some communist states (Yugoslavia, China after 1971) do honor agreements, and some U.S. allies (France, Pakistan) do not. The constraint's justification relies on ideological characterization rather than empirical verification behavior. Theater ratio is high: the self-enforcing requirement is maintained partly through narrative (threat inflation) rather than demonstrable necessity. Yet the mechanism persists through institutional inertia — once embedded in foreign policy doctrine, it becomes tradition rather than reasoned policy.
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint coordinates genuine security interests (verification solves a real problem: how to trust adversaries) but extracts significant costs: diplomatic flexibility is constrained, trust-building measures are precluded, and the constraint naturalizes adversarial relationships by making them permanent fixtures of treaty structure. At civilizational scale, the analytical view sees both the coordination function (verification is necessary) and the extractive mechanism (the requirement is inflexible and asymmetrically applied). The constraint prevents worse outcomes (war through miscalculation) but also prevents better outcomes (transformation of adversarial relationships through confidence-building).
constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, TR),
    TR >= 0.70.

:- end_tests(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint asymmetrically benefits U.S. security apparatus while imposing transparency and verification subordination on negotiating partners. The 35% initial value reflects that Cold War treaty-making was genuinely needed and could not proceed without some verification mechanism; the rise to 52% reflects accumulating evidence that the requirement exceeds empirical necessity and serves partly ideological purposes. Suppression (0.65): High. Negotiating partners face severe barriers to requesting faith-based agreements; the self-enforcing mechanism is a non-negotiable precondition. This suppression enables U.S. to impose its preferred verification regime without reciprocal constraint. Theater ratio (0.58): Moderate-high. The ideological framing ('Communism equals untrustworthiness') carries significant performative content — the constraint is maintained through narrative rather than demonstrable necessity. As the Cold War decades progress, evidence of differential treaty compliance by different Soviet-bloc states and periods contradicts the monolithic ideological framing, yet the institutional requirement persists through inertia. The rising theater ratio indicates increasing gap between stated rationale and actual enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap between U.S. and Soviet perspectives reveals the asymmetry. U.S. sees coordination (Rope); Soviet sees extraction (Snare/Tangled Rope). Allied states occupy intermediate position: they see coordination value (agreement reliability) and extraction cost (procedural constraint). The scaffold perspective from international verification bodies suggests the constraint could transition to Rope if multilateral verification becomes normalized and depoliticized. The piton perspective from the ideological system shows how theater (the 'Communist untrustworthiness' narrative) sustains the constraint despite weakening empirical grounding. The analytical observer's Tangled Rope classification captures both the genuine coordination function (verification solves real problems) and the genuine extraction (asymmetric transparency, constrained diplomacy, prevented trust-building).
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. security apparatus occupies beneficiary + arbitrage position: low d, negative effective extraction, sees constraint as beneficial coordination. Soviet Union occupies victim + trapped position: high d (approaching 1.0), maximum effective extraction, experiences constraint as coercive precondition. Allied states occupy moderate victim + constrained position: moderate d (0.55-0.65), moderate effective extraction, experience mixed coordination benefit (reliable U.S. partner) and extraction cost (precedent for their agreements). Soviet diplomatic apparatus occupies institutional victim + constrained position: d ~0.35-0.45 (not fully trapped, can negotiate some terms), moderate extraction, experience both coordination benefit (enabling negotiation) and extraction cost (verification subordination). International verification bodies occupy organized beneficiary + mobile position: low d, experience constraint as enabling institutional development (scaffold perspective). Cold War ideological system is institutional beneficiary + arbitrage: low d, sustains itself through being naturalized as security necessity rather than political construction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: The constraint is neither pure Rope nor pure Snare, but Tangled Rope from multiple perspectives — it coordinates genuine security interests (verification prevents miscalculation, makes agreements possible) while extracting significant asymmetric costs (U.S. maintains verification dominance, Soviet Union accepts transparency subordination, allied states pay precedent costs, diplomatic flexibility is constrained). The classification system prevents mischaracterization as either 'enlightened security policy' (false Rope) or 'oppressive Cold War imperialism' (false Snare). The constraint contains both characteristics. The mandatrophy is resolved by recognizing that both the coordination function (real) and the extraction mechanism (real) are structural features that cannot be separated. The rising theater ratio (0.42→0.58) indicates that the ideological justification becomes increasingly performative as empirical grounds weaken — the constraint persists through inertia rather than demonstrated necessity. This is diagnostic for Piton degradation: the institutional justification remains, but the functional necessity declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_asymmetry_measurement,
    'Is the self-enforcing mechanism truly reciprocal (both U.S. and Soviet subjects to verification) or does asymmetric verification effectively target the weaker party?',
    'Comparative analysis of verification mechanisms in actual Cold War treaties (NPT, SALT I/II, ABM Treaty): inspection rights, reporting requirements, penalty structures — do they apply equally to both parties?',
    'If reciprocal: constraint is genuine coordination (both parties trusted equally little). If asymmetric: constraint is extraction disguised as verification (U.S. maintains verification dominance while Soviet Union accepts transparency constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_asymmetry_measurement, empirical, 'Whether verification mechanisms are reciprocal or asymmetrically applied').

omega_variable(
    ideological_vs_empirical_basis,
    'Is the ''Communism is inherently untrustworthy'' framing empirically grounded or ideological?',
    'Statistical comparison of treaty compliance rates: Communist vs. non-Communist states; U.S.S.R. vs. individual Soviet-bloc states vs. China; correlation between ideology and treaty violation.',
    'If empirically grounded: the constraint is justified by demonstrated behavior. If ideological: the constraint naturalizes a political framing and may be applied to non-Communist adversaries less rigorously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_vs_empirical_basis, empirical, 'Whether distrust of Communist states is empirically grounded or ideologically driven').

omega_variable(
    diplomatic_flexibility_cost,
    'What is the magnitude of diplomatic flexibility lost by requiring self-enforcing mechanisms as a precondition?',
    'Historical counterfactual analysis: which agreements failed to materialize because the self-enforcing requirement was unacceptable? Comparison with agreements that succeeded with confidence-building instead of verification.',
    'If significant flexibility cost: the constraint extracts meaningful policy options. If minimal: the constraint is binding only on intractable negotiations that would have failed anyway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diplomatic_flexibility_cost, empirical, 'Cost of lost diplomatic flexibility due to self-enforcing mechanism requirement').

omega_variable(
    trust_building_prevention,
    'Does the institutional requirement for self-enforcing mechanisms prevent the development of trust that could later enable faith-based cooperation?',
    'Longitudinal analysis of U.S.-Soviet relations: did periods of repeated successful treaty compliance under verification enable later agreements with reduced verification? Do post-Cold War agreements with Russia show evidence of accumulated trust enabling lighter verification structures?',
    'If mechanisms prevent trust-building: the constraint is self-perpetuating and may lock in adversarial relationship structures. If trust can accumulate despite verification: the constraint is neutral on longer-term transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_building_prevention, conceptual, 'Whether self-enforcing mechanisms prevent development of trust-based cooperation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_1959_tr_t0, sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_1959_tr_t8, sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, theater_ratio, 8, 0.52).
narrative_ontology:measurement(sotu_1959_tr_t16, sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(sotu_1959_be_t0, sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_1959_be_t8, sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(sotu_1959_be_t16, sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, nuclear_non_proliferation_verification_requirements).
narrative_ontology:affects_constraint(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, salt_bilateral_verification_architecture).
narrative_ontology:affects_constraint(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, cuban_missile_crisis_inspection_protocols).

% DUAL FORMULATION NOTE:
% This constraint represents a specific policy instantiation of the broader structural problem: how to negotiate agreements with distrusted counterparties. The self-enforcing mechanism requirement is downstream of the verification technology development constraint (separate story: verification_technology_development, ε=0.18, Rope) and upstream of specific arms control treaties (SALT I, NPT, ABM Treaty) that implement this architectural principle. Each downstream treaty has its own ε value reflecting the specific agreement's balance of coordination and extraction; this story models the institutional requirement that preconditions all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, institutional, 0.38).
constraint_indexing:directionality_override(sotu_1959_eisenhower_treaty_self_enforcing_mechanisms, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
