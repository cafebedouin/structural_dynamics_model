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
 *   The multilateral arms control regime, centered on the Nuclear
 *   Non-Proliferation Treaty (1970), the Biological Weapons Convention
 *   (1972), and the Chemical Weapons Convention (1997), has systematically
 *   eroded over the past three decades. This erosion exhibits a
 *   characteristic pattern: as great powers modernize their own arsenals and
 *   pursue strategic advantage, they simultaneously maintain formal
 *   institutional participation in arms control frameworks while exploiting
 *   selective enforcement. Small states and non-aligned nations remain bound
 *   by treaty commitments with asymmetric vulnerability to enforcement, while
 *   great powers maintain optionality through technical ambiguities,
 *   withdrawal clauses, and covert development programs. The regime exhibits
 *   both genuine coordination function (mutual deterrence stabilization among
 *   great powers) and systematic extraction (selective enforcement that
 *   protects great-power violations while sanctioning non-aligned
 *   development). The rising theater ratio (0.38 → 0.73 over 30 years)
 *   indicates degradation — verification apparatus becomes increasingly
 *   elaborate (compliance reports, inspection protocols, forensic analysis)
 *   while actual constraint enforcement declines, suggesting institutional
 *   inertia masking functional erosion.
 *
 * KEY AGENTS:
 *   - Global Nonproliferation Regime: Primary victim (powerless/trapped) — abstract collective good that cannot organize or exit; bears full cost of violation asymmetry
 *   - Non-Military-Aligned States: Secondary victim (moderate/constrained) — bound by treaty commitments with selective enforcement vulnerability; face sanctions for violations while great powers evade
 *   - Revisionist Nuclear Powers: Primary beneficiary (institutional/arbitrage) — maintain exit optionality through technical ambiguities and covert programs; benefit from regime's appearance while pursuing strategic advantage
 *   - Military-Industrial Complex: Beneficiary (powerful/arbitrage) — profiteers from sustained military modernization justified by nonproliferation failures and emerging threats
 *   - Regional Security Coalitions: Mixed position (organized/constrained) — derive deterrence benefit from regime but constrained by alliance politics and escalation fears
 *   - UN Verification Apparatus (IAEA, OPCW): Institutional actor (institutional/arbitrage) — maintains compliance theater through inspection rituals; authority eroded but processes elaborate
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees simultaneous coordination and extraction, rising theater masking constraint failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multilateral_arms_control_authority_erosion, 0.58).
domain_priors:suppression_score(multilateral_arms_control_authority_erosion, 0.65).
domain_priors:theater_ratio(multilateral_arms_control_authority_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multilateral_arms_control_authority_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(multilateral_arms_control_authority_erosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(multilateral_arms_control_authority_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multilateral_arms_control_authority_erosion, tangled_rope).
narrative_ontology:human_readable(multilateral_arms_control_authority_erosion, "Multilateral Arms Control Authority Erosion").
narrative_ontology:topic_domain(multilateral_arms_control_authority_erosion, "international_security/institutional_governance").

domain_priors:requires_active_enforcement(multilateral_arms_control_authority_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(multilateral_arms_control_authority_erosion, military_industrial_complex).
narrative_ontology:constraint_beneficiary(multilateral_arms_control_authority_erosion, military_planners).
narrative_ontology:constraint_beneficiary(multilateral_arms_control_authority_erosion, revisionist_states).
narrative_ontology:constraint_victim(multilateral_arms_control_authority_erosion, global_nonproliferation_regime).
narrative_ontology:constraint_victim(multilateral_arms_control_authority_erosion, non_military_aligned_states).
narrative_ontology:constraint_victim(multilateral_arms_control_authority_erosion, civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The abstract collective good of the nonproliferation regime is locked in extraction. Small states and non-aligned nations have no escape option — they remain bound by NPT commitments while great powers systematize violations. The regime cannot organize, cannot exit, and cannot enforce against powerful violators. Maximum extraction experience from a powerless perspective.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% A middle-income state faces constrained exit. Theoretically can withdraw from arms control treaties, but faces economic sanctions, diplomatic isolation, and security vulnerability if it does. Continues participating in the regime while watching great powers violate with impunity. Mixed: genuine coordination of stability exists alongside asymmetric extraction where enforcement applies selectively to the weak.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% A great power benefits from the constraint structure while maintaining exit optionality. Can exploit treaty ambiguities (ICBM vs. cruise missile definitions), conduct covert development, or withdraw with minimal consequence. Experiences the regime as enabling coordination through the performative compliance theater — appears committed while pursuing strategic advantage. Net beneficiary with strategic freedom.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Allied states bound in collective security arrangements see the regime as coordinating deterrence (genuine rope function) while simultaneously extracting security costs through regional burden-sharing agreements and technological dependency. Organized enough to negotiate but constrained by alliance politics and escalation fears. Active enforcement of mutual commitments sustains the constraint structure.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The IAEA, OPCW, and verification bodies maintain performative compliance theater. The apparatus goes through elaborate inspection rituals, verification protocols, and compliance reports while fundamental enforcement authority has atrophied. States know inspections can be evaded or politicized. The institutions persist through bureaucratic inertia rather than functional verification capacity. Theater ratio dominates — elaborate process theater replaces actual constraint.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From civilizational scope, the arms control regime reveals both genuine coordination (mutual deterrence stabilization) and systematic extraction (great powers exploiting selective enforcement against the weak). The constraint requires active institutional enforcement to function, has clear beneficiaries (military planners, defense contractors) and victims (non-aligned states, civilians at risk). The theater ratio indicates degradation — compliance theater increases as real verification capacity declines.
constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multilateral_arms_control_authority_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multilateral_arms_control_authority_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderately high, reflecting that the regime structure systematically transfers security risk to non-aligned states while great powers maintain strategic freedom. This is below snare threshold (0.66+) because genuine deterrence coordination still functions at great-power level — the constraint is not pure extraction, but asymmetric extraction. Suppression (0.65): Moderately high. Significant barriers include: (1) technical verification difficulty for novel weapon designs; (2) great-power veto over enforcement mechanisms; (3) economic and diplomatic sanctions against defectors that exceed transparent enforcement of treaty terms; (4) intelligence compartmentalization that prevents shared verification data. These barriers are intentional institutional features, not technical accident. Theater ratio (0.68): High and rising. Verification apparatus has become increasingly elaborate (more inspectors, more protocols, more forensic analysis) while actual constraint enforcement has declined. The 0.38→0.73 trajectory shows theater substituting for function — inspection rituals become the primary activity rather than consequential constraint. This is the diagnostic signature of a regime in piton phase — institutional inertia maintains the apparatus after functional capacity has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival polarization. The revisionist power sees rope — the regime coordinates mutual deterrence and enables strategic planning through predictable violation bounds. The non-aligned state sees snare — locked in formal commitment with zero exit optionality and selective enforcement asymmetry. The verification apparatus sees piton — elaborate compliance theater that persists through bureaucratic inertia. The regional coalition sees tangled rope — genuine deterrence coordination entangled with asymmetric burden-sharing. The global regime (abstract agent) sees snare — trapped, cannot exit, cannot enforce against powerful violators. The analytical observer sees tangled rope — legitimate coordination function exists alongside systematic extraction mechanism. The perspectival gap is driven by differential exit optionality (arbitrage vs. trapped) and institutional position (beneficiary vs. victim). This is an inter-institutional constraint where the structural relationship determines experience, not nominal power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position: beneficiary/victim status, exit options, and institutional power. Revisionist powers have beneficiary status + arbitrage exit → low d → negative effective extraction (they experience the regime as enabling). Non-aligned states have victim status + trapped exit → high d → high effective extraction. Regional coalitions have mixed status (coordinating benefit + constrained exit) → moderate d. The verification apparatus benefits from continued institutional relevance → low d. The global nonproliferation regime has victim status but analytical exit options → moderate d at analytical perspective. The great-power beneficiary perspective sees low extraction (rope) because their d value is low; the trapped non-aligned perspective sees high extraction (snare) because their d value is high. The directionality derivation explains the perspectival gap without requiring different metrics — the same constraint structure produces different experiences based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies tangled rope gates: (1) beneficiaries exist (military planners, revisionist states, defense contractors) — genuine coordination benefit accrues to aligned parties; (2) victims exist (non-aligned states, civilian populations at risk, global nonproliferation regime) — systematic extraction from the weak; (3) active enforcement required — enforcement asymmetry is institutional choice, not technical accident. The mandatrophy arises from the appearance of both 'this is pure coordination' (rope) and 'this is pure extraction' (snare) from different perspectives. The piton perspective (verification apparatus) threatens to resolve it as degraded rope — 'used to coordinate, now just theater.' The analytical perspective resolves it as tangled rope — both functions exist simultaneously: the coordination function (deterrence stabilization among great powers) is real and structural; the extraction function (selective enforcement against the weak) is equally real and structural. Neither can be removed without destroying the regime's stability function for those it was designed to stabilize. This is the hallmark of tangled rope: the extraction is necessary for the coordination to function (deterrence requires credible asymmetry), but the extraction itself is unjust and unsustainable. The rising theater ratio indicates trajectory toward piton — the coordination function may be degrading faster than institutional authority, leaving pure theater and selective extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_authenticity_threshold,
    'At what verification gap rate does the nonproliferation regime transition from ''imperfect but functional constraint'' to ''pure compliance theater masking extraction''?',
    'Quantitative analysis of actual violations caught vs. violations post-hoc attributed to intelligence; comparison of treaty-mandated inspections vs. covert development discovered through intelligence channels; tracking of consequence severity for comparable violations by aligned vs. non-aligned states',
    'If gap < 10%: regime retains mountain-like constraint property. If gap > 40%: regime functions primarily through theater and selective enforcement — classification shifts decisively toward snare. Current structural data suggests gap ~35-45%, placing regime at boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_authenticity_threshold, empirical, 'Verification authenticity threshold determining regime functionality').

omega_variable(
    great_power_concert_stability,
    'Do great powers'' mutual interest in deterrence stability outweigh their incentive to exploit asymmetric enforcement, or is the deterrence benefit eroding?',
    'Historical pattern analysis of P5 voting behavior in UNSC on nonproliferation enforcement; correlation between states'' own military modernization and their tolerance of peer violations vs. non-aligned violations; simulation of stability metrics under different enforcement asymmetry levels',
    'If deterrence incentive dominates: regime remains tangled rope (mixed coordination-extraction). If exploitation incentive dominates: regime degrades toward pure extraction (snare). Rising theater ratio suggests exploitation is gaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_concert_stability, conceptual, 'Whether deterrence stability outweighs exploitation incentives').

omega_variable(
    alternative_coordination_emergence,
    'Are regional security arrangements and bilateral deterrence frameworks emerging as functional replacements for multilateral arms control, or are they amplifying extraction through fragmentation?',
    'Mapping of regional security agreements post-NPT framework erosion; measurement of regional arms race escalation rates; tracking of civilian security outcomes in regions with bilateral vs. multilateral arrangements',
    'If alternatives are functional: constraint family decomposes into regional tangled ropes, and multilateral regime becomes a piton. If alternatives amplify extraction: global regime erosion increases civilian vulnerability, classification solidifies as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_emergence, empirical, 'Whether alternative coordination frameworks are functional replacements').

omega_variable(
    enforcement_selectivity_driver,
    'Is selective enforcement driven by capacity limits (inability to verify compliance uniformly) or by intentional structural choice (great powers choosing to enforce against adversaries but not allies)?',
    'Forensic analysis of P5 justifications for enforcement decisions; comparison of violations by aligned vs. adversary states holding technical detectability constant; interviews with verification professionals on capacity vs. choice constraints',
    'If capacity-driven: suppression is structural/technical, and regime could be repaired through better verification. If choice-driven: suppression is intentional, extraction mechanism is explicit, and regime erosion reflects deliberate design, not dysfunction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_driver, empirical, 'Root cause of selective enforcement: capacity vs. intentional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multilateral_arms_control_authority_erosion, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mult_tr_t0, multilateral_arms_control_authority_erosion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mult_tr_t10, multilateral_arms_control_authority_erosion, theater_ratio, 10, 0.52).
narrative_ontology:measurement(mult_tr_t20, multilateral_arms_control_authority_erosion, theater_ratio, 20, 0.68).
narrative_ontology:measurement(mult_tr_t30, multilateral_arms_control_authority_erosion, theater_ratio, 30, 0.73).

% Extraction over time
narrative_ontology:measurement(mult_be_t0, multilateral_arms_control_authority_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mult_be_t10, multilateral_arms_control_authority_erosion, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(mult_be_t20, multilateral_arms_control_authority_erosion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(mult_be_t30, multilateral_arms_control_authority_erosion, base_extractiveness, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(multilateral_arms_control_authority_erosion, enforcement_mechanism).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, nuclear_technology_dual_use).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, great_power_deterrence_stability).
narrative_ontology:affects_constraint(multilateral_arms_control_authority_erosion, regional_arms_race_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is upstream of regional arms race dynamics and downstream of great-power deterrence competition. The erosion of multilateral authority directly enables regional proliferation cycles. Should be linked with constraint stories for specific regional dynamics (Middle East nuclear ambitions, Indo-Pacific modernization) where multilateral regime erosion is a structural driver.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(multilateral_arms_control_authority_erosion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
