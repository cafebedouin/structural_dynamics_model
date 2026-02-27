% ============================================================================
% CONSTRAINT STORY: nuclear_vacuum_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_vacuum_2026, []).

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
 *   constraint_id: nuclear_vacuum_2026
 *   human_readable: The New START Expiration (Post-Arms Control Era)
 *   domain: political/geopolitical/nuclear_security
 *
 * SUMMARY:
 *   The expiration of the New START treaty on February 5, 2026, ended fifty
 *   years of continuous U.S.-Russian nuclear arms control and inaugurated
 *   what strategists term the 'post-arms control era.' The constraint
 *   operates as a structural absence: the collapse of verification
 *   infrastructure, mutual transparency commitments, and negotiated force
 *   deployment limits creates a vacuum in which strategic opacity replaces
 *   confidence-building. This constraint exemplifies how the removal of an
 *   institutional framework — the negation of a rope — can generate a snare:
 *   without the coordination mechanism that provided mutual reassurance, both
 *   great powers face heightened first-strike incentives, reduced warning
 *   time, and increased accident risk. Non-nuclear nations, the
 *   non-proliferation regime, and the arms control epistemic community bear
 *   extraction costs they cannot mitigate. The constraint's expiration was
 *   not inevitable — it reflects deliberate policy choices by the U.S. and
 *   Russia to prioritize operational freedom over strategic stability — yet
 *   analytical observers often naturalize the outcome as a law of great power
 *   competition. The theater ratio (0.55) reflects ongoing diplomatic
 *   performativity: both powers maintain rhetoric of arms control interest
 *   while conducting force modernization incompatible with negotiated
 *   constraints. The extractiveness trajectory shows acceleration: as the
 *   treaty wind-down period progresses and replacement negotiations stall,
 *   the extraction mechanism intensifies.
 *
 * KEY AGENTS:
 *   - Global Strategic Stability: Primary victim (powerless/trapped) — bears full cost of opacity and first-strike incentive misalignment; cannot exit the strategic environment
 *   - Non-Nuclear Nations and Civilian Populations: Primary victims (powerless/trapped) — bear geopolitical instability risk without voice in constraint's terms
 *   - Arms Control Epistemic Community: Secondary victim (moderate/constrained) — loses professional legitimacy and funding as field becomes marginal to policy
 *   - U.S. and Russian Strategic Commands: Primary beneficiaries (institutional/arbitrage) — experience constraint's collapse as liberation of strategic agency and force modernization freedom
 *   - Non-Proliferation Regime: Structural victim (institutional/constrained) — loses enforceability and moral authority with great-power arms control collapse
 *   - Allied Nuclear Powers (France, UK, Japan): Mixed (organized/constrained) — benefit from extended deterrence but lose leverage over U.S. strategic decisions
 *   - International Law and Diplomacy Institutions: Institutional actor (institutional/arbitrage) — maintain performative function (negotiation theater) while losing substantive enforcement capacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent strategic choice as immutable law of great power competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_vacuum_2026, 0.68).
domain_priors:suppression_score(nuclear_vacuum_2026, 0.72).
domain_priors:theater_ratio(nuclear_vacuum_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_vacuum_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_vacuum_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_vacuum_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_vacuum_2026, snare).
narrative_ontology:human_readable(nuclear_vacuum_2026, "The New START Expiration (Post-Arms Control Era)").
narrative_ontology:topic_domain(nuclear_vacuum_2026, "political/geopolitical/nuclear_security").

domain_priors:requires_active_enforcement(nuclear_vacuum_2026).

% --- Structural relationships ---
narrative_ontology:constraint_victim(nuclear_vacuum_2026, global_strategic_stability).
narrative_ontology:constraint_victim(nuclear_vacuum_2026, non_nuclear_nations).
narrative_ontology:constraint_victim(nuclear_vacuum_2026, civilian_populations).
narrative_ontology:constraint_victim(nuclear_vacuum_2026, arms_control_epistemic_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL STRATEGIC STABILITY (SNARE) — The epistemic commons of confidence-building measures, mutual verification, and transparent force posturing has no exit. With NEW START expired, the verification infrastructure (inspections, data exchanges, notifications) collapses immediately. Strategic opacity replaces transparency. Conventional and nuclear forces blur without treaty-mandated boundaries. Non-nuclear-armed states cannot exit this regime; they bear the extraction cost as strategic instability increases — heightened first-strike incentives, reduced warning time, increased accident risk. This agent is trapped.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-NUCLEAR NATIONS AND CIVILIAN POPULATIONS (SNARE) — Cannot opt out of the strategic environment created by U.S. and Russian nuclear postures. Bear all costs of instability — regional conflict escalation, proxy war intensity, reduced deterrent effect of arms control norms — with zero voice in the constraint's terms. Extraction is maximal: the constraint's collapse increases their risk while they have no structural agency to influence outcomes. Trapped by geography and power asymmetry.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ARMS CONTROL EPISTEMIC COMMUNITY (SNARE) — Researchers, negotiators, and advocates for arms control have constrained exit: they can publish and argue for revival, but lack structural power to reimpose constraints. The field's intellectual capital (50 years of verification protocols, confidence-building measures, modeling frameworks) becomes depreciated as the practical institutional framework collapses. Significant extraction: professional legitimacy declines, funding shifts to deterrence and nuclear modernization, their analytical frameworks become marginal to policy. Constrained but not fully trapped — some mobility through academic careers and policy advisory roles.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. AND RUSSIAN STRATEGIC COMMANDS (ROPE) — From the military strategic perspective, NEW START expiration solves a coordination problem: both powers want operational freedom to modernize forces, conduct deployments, and hedge against third-actor nuclear emergence without treaty constraints. The constraint's collapse is experienced as liberation of strategic agency. Net benefit: unencumbered force development, deployment flexibility, operational secrecy. These institutional actors have maximum exit options (arbitrage) — they can and have walked away from the treaty framework. The classification is rope because the institutional design problem it solved was genuine: coordinating strategic forces to prevent miscalculation. Now those institutions are eliminated rather than reformed.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED NUCLEAR POWERS AND EXTENDED DETERRENCE BENEFICIARIES (TANGLED ROPE) — These nations benefit from the strategic stability created by U.S.-Russian arms control (reduced chance of nuclear exchange that would destabilize their regions) AND suffer from the extraction hidden within: they must accept U.S. strategic decisions (force posture changes, extended deterrent strength, risk calculations) without contractual influence. They are constrained but organized — they can pressure for new agreements through NATO and diplomatic forums, but their exit is limited by dependence on U.S. deterrence. The constraint combines coordination function (strategic stability benefits) with asymmetric extraction (inability to influence terms). Active enforcement required to maintain extended deterrence commitments despite the transparency vacuum.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-PROLIFERATION REGIME (SNARE) — The NPT and associated institutions (IAEA, CTBT Organization) derive authority from the established great-power commitment to arms control exemplified by NEW START. With that treaty expired and no replacement, the regime's enforcement against new proliferators weakens — the great powers have lost moral authority to demand others forgo nuclear weapons while abandoning mutual restraint. The regime has no exit: it is bound by its charter to prevent proliferation. But its structural capacity to enforce has declined. Extraction is substantial: the regime must absorb increased proliferation pressure, reduced state compliance incentives, and delegitimization without possessing coercive tools. The regime is constrained and suffering degradation.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL LAW AND DIPLOMACY INSTITUTIONS (PITON) — The institutional framework of arms control negotiations, UN Security Council coordination, and international legal authority persists but has lost primary function. The theater of diplomatic engagement continues (summits, talk-of-talks, back-channel negotiations) but the binding constraint mechanism is gone. These institutions maintain performative function (demonstrating commitment to negotiation) while losing substantive enforcement power. Theater ratio rises as institutions perform without producing binding outcomes. The institutional inertia is substantial — the framework persists through organizational momentum and diplomatic habit rather than effectiveness. Piton: degraded but maintained through institutional theater.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / REALIST NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint's expiration reflects an immutable law of strategic interaction: great powers with nuclear weapons cannot be permanently constrained by treaties when their security interests diverge. The 50-year arms control era was a temporary alignment of incentives (Cold War stability, arms race cost avoidance, technology pause). When those incentives shifted (Chinese emergence, Russian sphere reassertion, technology acceleration), the constraint became unsustainable. This view naturalizes the outcome as inevitable — a realist law of great power competition, not a contingent institutional choice. However, the engine's false summit detector will identify this as naturalization of what is actually a contingent strategic choice: the constraint DID work for 50 years, and its expiration reflects deliberate policy decisions, not immutable law.
constraint_indexing:constraint_classification(nuclear_vacuum_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_vacuum_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_vacuum_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_vacuum_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_vacuum_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_vacuum_2026, TR),
    TR >= 0.70.

:- end_tests(nuclear_vacuum_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, rising. The constraint mechanism is the strategic opacity that emerges from the collapse of treaty-mandated verification and force transparency. Base extraction starts at 0.35 (during the wind-down period when some verification remained) and rises to 0.68 as institutions fully close. The extraction is substantial because non-nuclear nations and the arms control community absorb all costs (heightened instability, regime delegitimization, professional marginalization) while the extractors (U.S. and Russian military commands) gain operational freedom. Suppression (0.72): High. Multiple barriers prevent exit or mitigation: non-nuclear nations cannot develop their own deterrents without triggering NPT violations and sanctions; the arms control community cannot reconstruct verification infrastructure unilaterally; global strategic stability has no institutional voice. The suppression mechanism is primarily structural (asymmetric power) rather than coercive, but it is binding. Theater ratio (0.55): Moderate-high. Diplomatic performativity remains high — summits continue, talk-of-talks are announced, both powers profess interest in arms control — but these performances produce no binding constraints. The theater is functional cover for strategic competition rather than substantive negotiation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint reveals a fundamental asymmetry in structural experience. The U.S. and Russian strategic commands see rope (coordination problem solved, strategic freedom regained) or even positive utility (unencumbered modernization). Their allies and the global strategic stability commons see snare (trapped in an increasingly opaque and unstable environment). The non-proliferation regime and arms control scholars occupy an intermediate position — they experience both extraction (loss of enforceability and authority) and constrained agency (unable to build alternative frameworks without great-power support). The piton classification of diplomatic institutions captures the performative maintenance of negotiation theater after the binding institutional framework has collapsed. The false summit (mountain perspective) reflects realist naturalization: scholars and strategists often claim that arms control treaties are ultimately unsustainable because great powers' strategic interests diverge. But this naturalizes what is actually a contingent choice: the 50-year duration of NEW START and its predecessors (SALT I/II, ABM treaty continuity) demonstrates that constraints CAN persist when institutional incentives are aligned. The expiration reflects deliberate policy shifts, not immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural position relative to the extraction mechanism. Global strategic stability and non-nuclear nations are full targets (d → 1.0): they bear costs with zero arbitrage options. The arms control community is a high-target beneficiary group (d → 0.85): they are victims of the regime's collapse but have modest professional exit options through policy advisory roles. U.S./Russian strategic commands are beneficiaries with full arbitrage (d → 0.05): they experience the constraint's expiration as liberation. Allied nuclear powers experience d → 0.55 (symmetric): they benefit from stability but suffer from asymmetric decision-making by the guarantor. The non-proliferation regime and international law institutions experience d → 0.70 (targets of delegitimization but with some institutional persistence). The engine derives d from the beneficiary/victim declarations and exit options in each perspective, producing the observed perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The classification as snare is robust across the constraint's lifecycle and resolves potential misclassification. Early realist claims that 'arms control is coordination' (rope) are refuted by the asymmetric extraction pattern: the beneficiaries (strategic commands) have arbitrage and gain from the collapse; the victims (stability commons, non-nuclear states, non-proliferation regime) are trapped and suffer. If the constraint were pure coordination (rope), all perspectives would classify similarly, and expiration would be neutral or beneficial. Instead, the perspectives diverge sharply — snare from victims' viewpoints, rope from beneficiaries' viewpoints — revealing that coordination function masked asymmetric extraction. The mandatrophy is resolved by tracking the perspectival distribution: snare is the canonical classification because it captures the structural reality that the constraint's collapse imposes concentrated costs on dispersed powerless agents while concentrating benefits on organized powerful actors. The piton classification of diplomatic institutions shows institutional inertia (theater maintenance after functional collapse). The false summit detects realist naturalization. All six types are legitimate readings, but snare is the structurally accurate classification for understanding the constraint's extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proliferation_cascade_threshold,
    'Will the absence of U.S.-Russian arms control trigger a nuclear proliferation cascade among threshold states (Iran, Saudi Arabia, South Korea, Poland)?',
    'Monitoring of enrichment programs, weapons program indicators, delivery system development; diplomatic signaling analysis; timeline correlation between NEW START expiration and state-level nuclear decisions',
    'If cascade occurs: snare classification confirmed — the constraint''s collapse directly increases victim exposure. If contained: extraction mechanism weakens — non-proliferation regimes maintain some hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_cascade_threshold, empirical, 'Whether treaty expiration triggers proliferation cascade').

omega_variable(
    strategic_stability_measurement,
    'How should ''strategic stability'' be measured to distinguish legitimate force modernization from destabilizing capability shifts?',
    'First-strike incentive modeling; strategic force composition analysis; launch-on-warning posture changes; development of new delivery systems (hypersonics, AI-enabled targeting); comparison to Cold War crisis stability metrics',
    'If instability measurable: snare extraction is quantifiable. If stability metrics degrade: victim exposure increases. If metrics remain stable: constraint''s loss has lower extractive cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_stability_measurement, empirical, 'Measurement framework for strategic stability post-START').

omega_variable(
    replacement_treaty_feasibility,
    'Is a negotiated replacement arms control regime possible within the next 5-10 years, or is the constraint''s collapse permanent?',
    'Diplomatic track analysis; great-power strategic interest convergence; third-actor nuclear emergence rates; arms race cost models; political will assessments in Moscow and Washington',
    'If replacement feasible: constraint is temporary (scaffold interpretation becomes stronger). If replacement impossible: constraint is structural snare (victim exposure persistent). If partial replacement (bilateral U.S.-Russian only): tangled rope with asymmetric benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(replacement_treaty_feasibility, conceptual, 'Whether New START replacement is achievable').

omega_variable(
    verification_technology_substitution,
    'Can intelligence-based verification (national technical means, satellite, signals intelligence) effectively replace treaty-mandated inspections and data exchanges for confidence-building?',
    'Comparison of verification confidence during NEW START era (inspections + NTM) vs post-expiration (NTM only); assessment of opacity increase; measurement of false-alarm rates and strategic surprise risk',
    'If substitution effective: strategic stability loss is moderate. If ineffective: opacity surge increases first-strike incentives (snare deepens). If partially effective: tangled rope classification strengthens (asymmetric vulnerability by state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_technology_substitution, empirical, 'Whether NTM can substitute for treaty verification').

omega_variable(
    crisis_stability_under_opacity,
    'During a U.S.-Russian political or military crisis, how will decision-makers assess escalation risk without treaty-mandated transparency of force postures and movement notifications?',
    'Crisis simulation analysis; assessment of Cold War-era crisis stability mechanisms without verification; red-team analysis of miscalculation pathways; modeling of confidence-building alternative mechanisms',
    'If opacity dramatically increases escalation risk: snare is deepened and accelerated. If alternative signaling mechanisms stabilize: scaffold or rope interpretation becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_stability_under_opacity, conceptual, 'Crisis stability mechanisms post-NEW START expiration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_vacuum_2026, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nustart_tr_t0, nuclear_vacuum_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nustart_tr_t12, nuclear_vacuum_2026, theater_ratio, 12, 0.45).
narrative_ontology:measurement(nustart_tr_t24, nuclear_vacuum_2026, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(nustart_be_t0, nuclear_vacuum_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nustart_be_t12, nuclear_vacuum_2026, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(nustart_be_t24, nuclear_vacuum_2026, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_vacuum_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_vacuum_2026, npat_regime_delegitimization).
narrative_ontology:affects_constraint(nuclear_vacuum_2026, regional_nuclear_proliferation).
narrative_ontology:affects_constraint(nuclear_vacuum_2026, us_russia_strategic_relations).
narrative_ontology:affects_constraint(nuclear_vacuum_2026, extended_deterrence_credibility).

% DUAL FORMULATION NOTE:
% The New START expiration is decomposed from the broader U.S.-Russian strategic competition constraint. Upstream: strategic competition (ε ≈ 0.45, tangled rope). Downstream: specific nuclear vacuum (ε ≈ 0.68, snare). The upstream constraint created conditions for the downstream constraint's emergence. NPT regime delegitimization is a causal consequence — with arms control norms failing, the NPT's asymmetric bargain (non-nuclear nations forgo nuclear weapons while nuclear powers pursue arms control) loses enforceability. Extended deterrence credibility is affected because U.S. commitments to nuclear umbrellas are now unencumbered by arms control constraints, creating asymmetric risk for allies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_vacuum_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
