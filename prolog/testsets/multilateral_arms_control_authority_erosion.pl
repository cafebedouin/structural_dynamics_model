% ============================================================================
% CONSTRAINT STORY: multilateral_arms_control_authority_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_multilateral_arms_control_authority_erosion, []).

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
 *   constraint_id: multilateral_arms_control_authority_erosion
 *   human_readable: Multilateral Arms Control Authority Erosion
 *   domain: international_security/institutional_governance
 *
 * SUMMARY:
 *   The multilateral arms control regime, anchored by the Nuclear
 *   Non-Proliferation Treaty (1970), Biological Weapons Convention (1972),
 *   and Chemical Weapons Convention (1997), exhibits a characteristic pattern
 *   of authority erosion over three decades: great powers maintain formal
 *   institutional participation while selectively enforcing rules against
 *   weaker states, modernizing their own arsenals with impunity, and
 *   exploiting ambiguities in treaty language to legitimize weapons
 *   development. This constraint is a canonical tangled_rope at the system
 *   level: it solves a genuine coordination problem for great powers (how to
 *   signal restraint while enabling modernization) while extracting
 *   compliance costs from non-aligned states and vulnerable populations who
 *   cannot exit. The institutional structure performs genuine coordination
 *   (verification infrastructure, transparency requirements, diplomatic
 *   forums that reduce security uncertainty) alongside systematic asymmetric
 *   extraction (selective enforcement, dual-use technology loopholes,
 *   verification obstruction). The theater_ratio has risen from 0.42 (1995:
 *   substantive negotiations and verification capacity-building) to 0.68
 *   (2025: performative compliance statements and symbolic treaty reviews
 *   while actual verification mechanisms atrophy). The constraint exhibits
 *   different classifications from different structural positions: powerless
 *   non-aligned states experience a snare (trapped in non-proliferation; no
 *   exit), great powers experience rope (coordinated signaling with arbitrage
 *   options), middle-power verification communities experience tangled rope
 *   (institutional benefit mixed with asymmetric extraction), and the regime
 *   itself is degraded piton (maintains theater while functional enforcement
 *   capacity decays).
 *
 * KEY AGENTS:
 *   - Great Powers (USA, Russia, China, France, UK): Primary beneficiaries (institutional/arbitrage) — extract strategic advantage from modernizing arsenals while maintaining legitimacy through treaty participation; control verification outcomes through political obstruction
 *   - Non-Aligned States (NNWS): Primary victims (powerless/trapped) — bound by non-proliferation commitments while great powers modernize; no alternative security architecture; systematic verification failures disproportionately affect non-aligned proliferation accusations
 *   - Vulnerable Populations (conflict zones, border regions): Secondary victims (powerless/trapped) — bear security costs of proliferation by great powers and regional powers exploiting regime ambiguities
 *   - Middle-Power Coalitions (Brazil, South Africa, Japan, EU): Organized actors (organized/constrained) — benefit from verification institutions and institutional roles, but constrained by great-power obstruction of inspections; face suppression of stronger enforcement advocacy
 *   - Verification Communities (IAEA, OPCW, CTBT): Institutional actors (institutional/constrained) — maintain verification infrastructure and profess independence, but systematically obstructed from investigating great-power compliance; constrained by political will dependency
 *   - Analytical Observer: System-level view (analytical/analytical) — sees regime as engineered structure that solves great-power coordination while extracting from weaker states; authority erosion is functional (regime working as designed) not pathological
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multilateral_arms_control_authority_erosion, 0.58).
domain_priors:suppression_score(multilateral_arms_control_authority_erosion, 0.62).
domain_priors:theater_ratio(multilateral_arms_control_authority_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multilateral_arms_control_authority_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(multilateral_arms_control_authority_erosion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(multilateral_arms_control_authority_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multilateral_arms_control_authority_erosion, tangled_rope).
narrative_ontology:human_readable(multilateral_arms_control_authority_erosion, "Multilateral Arms Control Authority Erosion").
narrative_ontology:topic_domain(multilateral_arms_control_authority_erosion, "international_security/institutional_governance").

domain_priors:requires_active_enforcement(multilateral_arms_control_authority_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(multilateral_arms_control_authority_erosion, great_power_arsenals).
narrative_ontology:constraint_beneficiary(multilateral_arms_control_authority_erosion, strategic_advantage_seekers).
narrative_ontology:constraint_victim(multilateral_arms_control_authority_erosion, non_proliferation_regime).
narrative_ontology:constraint_victim(multilateral_arms_control_authority_erosion, non_aligned_states).
narrative_ontology:constraint_victim(multilateral_arms_control_authority_erosion, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED STATES (SNARE) — Cannot exit the arms control regime; bound by NPT restrictions while great powers modernize arsenals with impunity. Verification mechanisms are selectively enforced. No alternative security architecture. Maximum extraction: constrained to non-proliferation; great powers exempt from reciprocal restraint.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-POWER COALITIONS (TANGLED ROPE) — Organized actors (IAEA inspectors, OPCW, verification experts) benefit from institutional roles and legitimacy, but face systematic constraints from great-power obstruction. Genuine coordination function exists (verification infrastructure), but asymmetric extraction occurs when great powers block inspections or manipulate data. High suppression: institutional rules exist but great powers override them with impunity.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GREAT POWERS—REGIME PARTICIPATION (ROPE) — At the formal institutional level, great powers experience the arms control regime as pure coordination: NPT participation legitimizes their arsenals, enables treaty interpretation favoring their interests, and provides institutional cover ('we are compliant members'). The regime solves their coordination problem: signal restraint while enabling modernization. Arbitrage options abundant — unilateral withdrawal is costly but possible.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GREAT POWERS—ARSENAL MODERNIZATION (TANGLED ROPE) — Simultaneously, the same actors experience modernization as extraction—they extract strategic advantage from the regime while bearing minimal compliance cost. They benefit from others' restraint (coordination function) while escaping their own restraint (extraction function). This is the hybrid: genuine coordination at the institutional level masks asymmetric extraction at the operational level. Suppression is high because dissent from treaty interpretation is suppressed through diplomatic channels.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL REGIME STRUCTURE (PITON) — The regime itself is degraded. Formal verification mechanisms persist (IAEA, OPCW, CTBT), but their functional enforcement capacity has atrophied as great powers selectively block inspections (Syria chemical weapons, Iran accusations, Russian claims). The institutions maintain theater—annual reviews, committee meetings, compliance statements—while actual verification and enforcement have decayed. Theater ratio reflects performative compliance rhetoric masking systematic non-compliance.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the regime exhibits the classic structure of institutionalized extraction: formal rules that benefit rule-breakers more than rule-followers, asymmetric verification, and selective enforcement. The regime solves great-power coordination (how to modernize while maintaining legitimacy) while extracting compliance costs from non-aligned states. Authority of the regime has eroded not because it is broken, but because it is functioning as designed: great powers built a structure that extracts from weaker actors while protecting stronger ones.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multilateral_arms_control_authority_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(multilateral_arms_control_authority_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(multilateral_arms_control_authority_erosion, TR),
    TR >= 0.70.

:- end_tests(multilateral_arms_control_authority_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Great powers extract strategic advantage by modernizing arsenals while maintaining institutional legitimacy, and they extract compliance costs from non-aligned states through selective enforcement and verification asymmetry. The value reflects the mixed coordination-extraction hybrid: genuine coordination benefits exist (reduced security uncertainty, institutional transparency), but extraction flow is asymmetric (non-aligned states bear full compliance costs; great powers bear minimal modernization constraints). The trajectory from 0.35 (1995: pre-withdrawal period when compliance was more symmetric) to 0.58 (2025: post-withdrawal/obstruction period) reflects systematizing of extraction mechanisms. Suppression (0.62): High. The regime suppresses dissent through diplomatic pressure against enforcement advocates, blocks inspections through procedural obstruction (great powers can veto intrusive verification), and suppresses visibility of non-compliance by great powers (Syria chemical weapons case study: Russia blocked fact-finding missions; no enforcement followed). The suppression has intensified as great powers have consolidated veto power. Theater ratio (0.68): High. Performative content is substantial: annual Review Conferences produce rhetorical commitments without enforcement; compliance statements are issued while arsenals are modernized; verification committees meet but cannot enforce; diplomatic summits celebrate agreements that are simultaneously undermined. The theater reflects that institutional participation itself provides value (legitimacy) independent of actual compliance. Claimed type (Tangled Rope): Justified by presence of both genuine coordination (verification infrastructure, transparency mechanisms, security dialogue) and asymmetric extraction (selective enforcement, compliance asymmetry, dual-use technology loopholes). Requires active enforcement and declares beneficiaries (great powers) and victims (non-aligned states).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival gap: powerless non-aligned states see a snare (trapped, no exit, extraction only); great powers see rope (coordination benefit, arbitrage exit, net positive). The gap reveals that the regime's fundamental function differs by position: for great powers, it is a coordination mechanism (reduces security uncertainty, enables strategic signaling); for non-aligned states, it is an extraction mechanism (constrains options while providing no reciprocal constraint on beneficiaries). The middle-power organized perspective (tangled rope) occupies the hybrid: they staff the institutions, benefit from institutional roles, but face suppression when they advocate stronger enforcement. The piton perspective reveals that even from inside the institutional structure, actors recognize degradation—verification communities explicitly acknowledge that functional verification capacity has atrophied while theater persists. The analytical observer resolves the gap by showing that all perspectives are correct: the regime is designed as a tangled rope (coordination + extraction hybrid) and is functioning as designed; authority erosion is not pathological failure but engineered equilibrium where great-power benefits and institutional persistence are decoupled from actual verification and enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the extraction flow. Great powers as beneficiaries with arbitrage options (can withdraw but prefer participation for legitimacy) have low d (~0.15-0.25), producing negative effective extraction (they benefit). Non-aligned states as victims with trapped exit options have high d (~0.85-0.95), producing maximum effective extraction. Middle-power organized actors with constrained options have moderate d (~0.55-0.65). The piton perspective (regime degradation) emerges not from high d but from theater_ratio gate: performative content overwhelms functional content regardless of directionality. The analytical perspective operates at d~0.72 (observational position) but reveals the structural incentive landscape: the regime was designed to solve great-power coordination (all perspectives consistent with that function) while the asymmetry in extraction emerges from asymmetry in power to obstruct verification (great powers can block inspections; non-aligned states cannot). This is not individual malice but structural incentive outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the 'erosion' in the constraint name is internal to the regime's functioning, not external to it. The mandatrophy would be: 'Is this regime coordination (rope) or extraction (snare)?' The analytical resolution is: it is both, and the dual function is the source of its persistence and its degradation. The regime persists because great powers benefit from it (coordination + legitimacy + extraction). Authority erodes because non-aligned states and verification communities are trapped in a structure where their advocacy for enforcement is systematically suppressed and their enforcement actions are blocked by actors they cannot constrain. The mandatrophy is resolved by the tangled rope classification: acknowledge that genuine coordination exists (theater is not all-consuming) while maintaining that asymmetric extraction is real (suppression is not incidental). The regime is not 'really a snare masquerading as rope' (which would justify unilateral action) nor 'really rope degraded by bad actors' (which would justify reform from inside). It is a hybrid where both functions are necessary conditions for its continued existence, and reform requires changing great-power structural incentives (high barrier) rather than fixing institutional procedures (low barrier).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    great_power_intent_vs_structural_incentive,
    'Do great powers intentionally exploit the regime''s asymmetries, or do asymmetries emerge from the structural incentive landscape independent of deliberate strategy?',
    'Documentary analysis of strategic planning documents, declassified NSC/Ministry of Defense reviews; correlation between arsenal modernization timelines and treaty interpretation shifts; interviews with negotiators comparing stated intent vs revealed preferences',
    'If intentional exploitation: classification stable (Tangled Rope). If structural emergence: regime design itself (not actor behavior) is the constraint — may reclassify to a different underlying architecture constraint. If mixed: directionality values shift but classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_intent_vs_structural_incentive, empirical, 'Whether great-power exploitation is intentional strategy or structural incentive outcome').

omega_variable(
    verification_mechanism_actual_effectiveness,
    'How much of the perceived regime failure is due to verification mechanisms actually being ineffective vs mechanisms being systematically obstructed by political will?',
    'Comparative analysis: verification success rates when great powers cooperate vs when they obstruct; technical assessment of IAEA/OPCW detection thresholds vs actual arsenal changes; historical case studies of enforcement when political will existed (Iraq 1990s) vs when it did not (Syria, Iran)',
    'If mechanisms are inherently ineffective: regime is Piton or Scaffold (not sufficiently functional for Tangled Rope). If obstruction is the constraint: the political suppression is the actual extraction mechanism, not the institutional rules — may reframe as separate suppression constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_mechanism_actual_effectiveness, empirical, 'Whether regime failure is mechanism ineffectiveness or political obstruction').

omega_variable(
    non_proliferation_counterfactual_outcomes,
    'Would non-aligned states proliferate at higher rates without the NPT framework, even with selective enforcement, or is proliferation restraint driven by security partnerships and strategic calculation independent of the regime?',
    'Game-theoretic analysis of security payoffs with/without regime; historical comparison of proliferation rates before (1950-1970) and after (1970-present) NPT; analysis of non-signatory states'' proliferation vs signatory states controlling for security partnerships and strategic environment',
    'If regime prevents proliferation even with selective enforcement: coordination function is real despite extraction asymmetry (Tangled Rope confirmed). If proliferation would be similar regardless: regime provides no coordination benefit and is pure extraction (Snare). If proliferation decisions are orthogonal to regime: regime is theater masking structural security dynamics (Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_proliferation_counterfactual_outcomes, empirical, 'Whether NPT prevents non-proliferation or merely documents strategic choices').

omega_variable(
    sunset_clause_structural_possibility,
    'Is the arms control regime approaching a structural breakdown point where it will collapse, or is it stable in a degraded equilibrium indefinitely?',
    'Analysis of critical junctures: withdrawal thresholds, verification failure cascades, institutional capacity collapse; game-theoretic models of regime stability; extrapolation of theater_ratio and authority_erosion trajectories',
    'If unstable with clear failure mode: Scaffold reclassification possible if a succeeding structure is emerging (e.g., tech-enabled verification, regional frameworks). If stable indefinitely: regime is permanent Tangled Rope or Piton. If collapse is endogenous to great-power strategy: regime is engineered instability (strategic Snare variant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_clause_structural_possibility, conceptual, 'Whether arms control regime is approaching structural breakdown or stable in degradation').

omega_variable(
    emerging_verification_technology_substitution,
    'Do satellite surveillance, AI-enabled analysis, and decentralized verification technologies enable alternative arms control architectures that could bypass great-power institutional obstruction?',
    'Technical assessment of detection capabilities of NRO/Copernicus/commercial satellites; feasibility studies of blockchain-based verification escrow; pilot projects with non-state verification organizations; analysis of verification protocols that don''t require treaty-signatory cooperation',
    'If technologies enable distributed verification: new constraint (technological_arms_verification_asymmetry) emerges as upstream. Scaffold sunset becomes real—institutional arms control authority is temporary as tech-enabled alternatives mature. If technologies are insufficient: regime remains institutional bottleneck (Tangled Rope stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_verification_technology_substitution, empirical, 'Whether emerging tech enables alternative verification architectures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multilateral_arms_control_authority_erosion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(macae_tr_t0, multilateral_arms_control_authority_erosion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(macae_tr_t10, multilateral_arms_control_authority_erosion, theater_ratio, 10, 0.55).
narrative_ontology:measurement(macae_tr_t20, multilateral_arms_control_authority_erosion, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(macae_be_t0, multilateral_arms_control_authority_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(macae_be_t10, multilateral_arms_control_authority_erosion, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(macae_be_t20, multilateral_arms_control_authority_erosion, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(macae_su_t0, multilateral_arms_control_authority_erosion, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(macae_su_t10, multilateral_arms_control_authority_erosion, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(macae_su_t20, multilateral_arms_control_authority_erosion, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(multilateral_arms_control_authority_erosion, enforcement_mechanism).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, nuclear_modernization_acceleration).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, biological_weapons_research_asymmetry).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, chemical_weapons_dual_use_ambiguity).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, great_power_verification_obstruction).

% DUAL FORMULATION NOTE:
% This constraint is the institutional architecture housing multiple domain-specific asymmetries (nuclear, biological, chemical). Each domain has its own constraint story with distinct ε values and verification mechanisms. This story models the institutional erosion pattern common to all three regimes, while domain stories model specific weapons-class asymmetries. Upstream influence: great-power strategic incentives determine institutional design. Downstream influence: institutional choices constrain domain-specific verification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(multilateral_arms_control_authority_erosion, institutional, 0.18).
constraint_indexing:directionality_override(multilateral_arms_control_authority_erosion, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
