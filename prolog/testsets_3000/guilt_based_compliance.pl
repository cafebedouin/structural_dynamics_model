% ============================================================================
% CONSTRAINT STORY: guilt_based_compliance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guilt_based_compliance, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: guilt_based_compliance
 *   human_readable: Guilt-Based Compliance as Extraction and Coordination Mechanism
 *   domain: social/psychological/institutional
 *
 * SUMMARY:
 *   Guilt-based compliance is a hybrid extraction and coordination mechanism
 *   that operates by activating the target agent's internalized moral
 *   obligation to comply with demands that may not align with their material
 *   interests. The constraint appears across domains: parental expectations
 *   on adult children, employer demands on workers (unpaid overtime framed as
 *   'dedication' or 'team commitment'), nonprofit solicitation of volunteers,
 *   government policies framed as 'shared sacrifice,' church tithing and
 *   service obligations, and caregiver labor in families. The mechanism is
 *   powerful precisely because it is internalized: the compliance officer is
 *   the agent themselves. Suppression operates through identity fusion
 *   (non-compliance would make me a bad person), not through external force.
 *   Over the 30-year interval measured here, the effective extractiveness has
 *   increased as institutional authority has learned to fine-tune guilt
 *   activation, while the theater ratio has risen as the moral rhetoric has
 *   become increasingly divorced from reciprocal accountability structures.
 *   The constraint exhibits all six DR types from different perspectives,
 *   revealing how the same psychological mechanism appears as pure
 *   coordination (rope from the beneficiary's view), degraded ritual (piton
 *   from the traditional moral discourse apparatus), temporary institution
 *   (scaffold from reform movements), mixed extraction-coordination (tangled
 *   rope from constrained but partially aware agents), pure extraction (snare
 *   from identity-locked agents), and possibly natural law (mountain from
 *   civilizational analytical views that risk naturalizing contingent
 *   institutional design).
 *
 * KEY AGENTS:
 *   - Guilt-Responsive Agent: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with moral obligation; bears emotional and material costs of compliance
 *   - Compliance Demander: Primary beneficiary (institutional/arbitrage) — employer, government, church, nonprofit extracting labor/resources through guilt activation
 *   - Partially Aware Complier: Secondary actor (moderate/constrained) — recognizes guilt mechanism but faces social costs to exit; has some negotiation capacity
 *   - Moral Discourse Apparatus: Institutional actor (institutional/arbitrage) — traditional religious/philosophical frameworks that provide language and cultural legitimacy for guilt mechanisms
 *   - Reform Coalition: Organized opposition (organized/constrained) — labor movements, advocacy organizations building alternative coordination mechanisms
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable features of social life
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guilt_based_compliance, 0.58).
domain_priors:suppression_score(guilt_based_compliance, 0.68).
domain_priors:theater_ratio(guilt_based_compliance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guilt_based_compliance, extractiveness, 0.58).
narrative_ontology:constraint_metric(guilt_based_compliance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(guilt_based_compliance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guilt_based_compliance, tangled_rope).
narrative_ontology:human_readable(guilt_based_compliance, "Guilt-Based Compliance as Extraction and Coordination Mechanism").
narrative_ontology:topic_domain(guilt_based_compliance, "social/psychological/institutional").

domain_priors:requires_active_enforcement(guilt_based_compliance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guilt_based_compliance, compliance_demanders).
narrative_ontology:constraint_beneficiary(guilt_based_compliance, institutional_authority).
narrative_ontology:constraint_victim(guilt_based_compliance, guilt_responsive_agents).
narrative_ontology:constraint_victim(guilt_based_compliance, collective_capacity_for_refusal).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GUILT-RESPONSIVE AGENT (SNARE) — Structurally mobile but identity-locked through internalized moral obligation. The agent believes non-compliance would make them a 'bad person' (selfish, uncaring, irresponsible). Exit would require abandoning their moral self-concept, not merely paying a material cost. Suppression operates through internalized voices (parental, institutional, peer expectations) rather than external barriers. Maximum extraction because the agent polices themselves and cannot perceive the constraint as contingent.
constraint_indexing:constraint_classification(guilt_based_compliance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMPLIANCE DEMANDER (ROPE) — Institutional actor (employer, government, church, nonprofit) experiences guilt-based compliance as an efficient coordination mechanism. Guilt activates compliance without explicit enforcement costs; moral obligation aligns agent behavior with institutional goals. Arbitrage available: if guilt fails, alternative enforcement methods exist (legal, economic, social). Net beneficiary with low effective extraction because they have substitution options and benefit from the coordination function.
constraint_indexing:constraint_classification(guilt_based_compliance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PARTIALLY AWARE COMPLIER (TANGLED ROPE) — Agent recognizes the guilt mechanism (knows they feel obligated) but cannot easily exit because social costs are real: family judgment, peer ostracism, professional consequences. Has some agency (can negotiate terms, seek alternatives, organize with peers) but faces high exit costs. Experiences both genuine coordination function (shared norms do produce collective goods) and asymmetric extraction (unequal burden distribution).
constraint_indexing:constraint_classification(guilt_based_compliance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MORAL DISCOURSE APPARATUS (PITON) — Religious and philosophical traditions that originally embedded guilt-based compliance in genuine moral community (shared sacrifice, mutual accountability) now operate largely as theatrical maintenance. The rhetoric of moral obligation persists; the underlying community structure that made it reciprocal has atrophied. Modern guilt appeals activate legacy programming without the reciprocal accountability structures. Theater ratio high because moral language is performed; functional moral coordination has degraded.
constraint_indexing:constraint_classification(guilt_based_compliance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, guilt-based compliance appears as an immutable feature of human social organization: all functioning societies rely on internalized moral obligation to sustain collective life. This perspective risks naturalizing what is actually a contingent institutional design choice. The false summit occurs when structural alternatives (explicit negotiation, transparent incentive alignment, genuine democratic consent) are treated as impossible rather than merely costly.
constraint_indexing:constraint_classification(guilt_based_compliance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents (labor movements, advocacy organizations, regulatory bodies) recognize guilt-based compliance as a temporary institutional arrangement with a sunset clause. Transparency requirements, wage justice movements, and worker protections create alternative coordination mechanisms that don't rely on guilt activation. Low effective extraction despite high suppression because organized agents have agency and can see an exit trajectory. Sunset: estimated 20-40 years for norms to mature in developed economies.
constraint_indexing:constraint_classification(guilt_based_compliance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guilt_based_compliance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guilt_based_compliance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guilt_based_compliance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(guilt_based_compliance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(guilt_based_compliance, TR),
    TR >= 0.70.

:- end_tests(guilt_based_compliance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The guilt mechanism extracts material labor and emotional energy without proportional compensation or reciprocal obligation. The 30-year trajectory shows accumulation: initial guilt appeals were framed within genuine moral communities (0.32 at T=0); as those communities degraded, the same guilt language operated as pure extraction (0.58 at T=30). The measurement shows how institutional learning amplified the mechanism's extractiveness over time. Suppression (0.68): High. Barriers to exit include identity fusion (non-compliance feels immoral), social penalties (family/peer judgment), economic dependency, and institutional control over alternative narratives. Suppression is not absolute because some agents do develop awareness and refuse, but the barriers are substantial. Theater ratio (0.65): Moderately high. Guilt appeals use moral language (duty, responsibility, team, family, service) that activates the agent's own values, but this language increasingly masks asymmetric benefit distribution. The theater has increased as institutions have learned guilt activation without reciprocal accountability. True coordination would include transparent negotiation and burden-sharing; guilt mechanisms skip negotiation and rely on internalized obligation.
 *
 * PERSPECTIVAL GAP:
 *   The guilt-responsive agent sees a snare (pure extraction dressed in moral language). The compliance demander sees coordination (an efficient solution to the motivation problem). The partially aware complier sees tangled rope (genuine coordination needs mixed with asymmetric extraction). The reform coalition sees a scaffold (a temporary institution being replaced by explicit contracts and transparency). The moral discourse apparatus sees a piton (its own language persisting through inertia after its community foundation degraded). The civilizational observer risks seeing a mountain (guilt as inherent to all social cooperation), but the structural data reveals this as a false summit: guilt-based compliance is a contingent institutional design choice, not a law of nature. The perspectival gaps widen as the constraint's theater ratio rises — the moral language diverges further from material reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural relationship to the guilt mechanism. For the guilt-responsive agent at powerless + identity_locked: they experience high d (close to 1.0) because they are the target of extraction and their identity prevents perception of exit (the highest suppression state). For the compliance demander at institutional + arbitrage: they experience low d (close to 0.0) because they benefit from the mechanism and have substitution options if guilt fails. For the partially aware complier at moderate + constrained: they experience intermediate d because they can see the extraction but face material costs to exit, giving them some agency but substantial constraint. The identity_locked exit option is critical here: it represents the binding mechanism that distinguishes guilt-based compliance from material poverty or legal prohibition. The agent could physically leave or refuse, but their identity structure prevents them from experiencing refusal as legitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through the identity_locked exit option at the powerless power atom. The snare perspective (guilt-responsive agent at powerless + identity_locked) is not a misclassification of coordination as extraction, nor is the rope perspective (compliance demander at institutional + arbitrage) a misclassification of extraction as coordination. Both are accurate readings of the same structural fact: the guilt mechanism both coordinates behavior AND extracts value asymmetrically. The distinction is perspective-dependent. The mandate is resolved by noting that guilt-based compliance is genuinely hybrid (tangled rope is the middle reading), and the snare/rope split reflects different structural positions within the same constraint. The false summit (mountain) is diagnostic: it appears when the analyst naturalizes the guilt mechanism rather than examining whether explicit alternatives exist. They do — transparent negotiation, explicit performance incentives, democratic consent to burden distribution — which proves the guilt mechanism is institutional design, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism_ambiguity,
    'Is the guilt-responsive agent''s constraint structural (material barriers to exit) or cognitive (identity fusion preventing perception of exit)?',
    'Post-exit behavior tracking: if the agent reports guilt reduction and capacity for refusal after exiting the compliance relationship, reclassify suppression as partially internalized. If guilt persists or intensifies, reclassify as deeply internalized.',
    'If internalized: effective suppression is higher than measured structural suppression suggests — the agent carries the constraint with them. If structural: measured suppression is adequate and relief mechanisms are primarily removal of external barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in guilt-based compliance').

omega_variable(
    coordination_function_reality,
    'Is the guilt mechanism producing genuine coordination of shared goods or merely enforcing compliance with beneficiary-preferred distribution?',
    'Comparative analysis: does guilt-based compliance produce more equitable burden distribution than alternatives? Do agents maintain compliance because shared norms align incentives or because refusal entails social penalty?',
    'If genuine coordination: tangled rope classification sustained; extraction component is justified overhead. If enforcement of asymmetric distribution: classification should shift toward snare from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_reality, empirical, 'Whether guilt produces coordination or merely enforces beneficiary preferences').

omega_variable(
    alternative_compliance_sufficiency,
    'Can transparent negotiation and explicit incentive alignment (wage increases, work-life boundaries, performance metrics) replace guilt-based motivation without losing compliance?',
    'Field experiments in organizational settings; comparison of compliance rates under guilt-based vs explicit-contract models controlling for compensation.',
    'If alternatives are sufficient: guilt mechanism is unnecessary extraction (snare from more perspectives). If guilt proves superior at lower cost: tangled rope classification is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_compliance_sufficiency, empirical, 'Whether explicit alternatives can replace guilt-based compliance').

omega_variable(
    cultural_variation_in_guilt_responsiveness,
    'Do guilt-based compliance mechanisms operate uniformly across cultural contexts or is responsiveness to guilt a culturally contingent trait that can be shaped by socialization?',
    'Cross-cultural comparison of guilt activation effects; longitudinal tracking of guilt responsiveness shifts in populations exposed to alternative institutional models.',
    'If uniform: guilt mechanism may be closer to mountain (inherent to social organization). If contingent: classification supports scaffold (temporary, can be sunset through cultural change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_variation_in_guilt_responsiveness, empirical, 'Cultural contingency of guilt-based compliance responsiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guilt_based_compliance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(guilt_tr_t0, guilt_based_compliance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(guilt_tr_t10, guilt_based_compliance, theater_ratio, 10, 0.52).
narrative_ontology:measurement(guilt_tr_t20, guilt_based_compliance, theater_ratio, 20, 0.65).
narrative_ontology:measurement(guilt_tr_t30, guilt_based_compliance, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(guilt_be_t0, guilt_based_compliance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(guilt_be_t10, guilt_based_compliance, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(guilt_be_t20, guilt_based_compliance, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(guilt_be_t30, guilt_based_compliance, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guilt_based_compliance, attachment_coordination).
narrative_ontology:boltzmann_floor_override(guilt_based_compliance, 0.12).
narrative_ontology:affects_constraint(guilt_based_compliance, unpaid_care_labor).
narrative_ontology:affects_constraint(guilt_based_compliance, volunteer_extraction).
narrative_ontology:affects_constraint(guilt_based_compliance, wage_suppression_through_moral_framing).

% DUAL FORMULATION NOTE:
% Guilt-based compliance is the meta-constraint that enables multiple domain-specific extraction mechanisms (unpaid care, volunteerism, wage suppression). Each domain story has its own ε reflecting domain-specific empirical status; guilt_based_compliance represents the psychological mechanism that activates across domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(guilt_based_compliance, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
