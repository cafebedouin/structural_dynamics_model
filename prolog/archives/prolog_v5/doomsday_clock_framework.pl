% ============================================================================
% CONSTRAINT STORY: doomsday_clock_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_framework, []).

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
 *   constraint_id: doomsday_clock_framework
 *   human_readable: Global Catastrophic Risk Management Framework
 *   domain: geopolitical/existential_risk
 *
 * SUMMARY:
 *   The global catastrophic risk management framework — anchored by the
 *   Bulletin of the Atomic Scientists' Doomsday Clock symbolism,
 *   operationalized through the Non-Proliferation Treaty, Comprehensive Test
 *   Ban Treaty, climate accords, and emerging biosecurity norms — represents
 *   a institutional attempt to coordinate existential risk reduction among
 *   actors with fundamentally misaligned interests. The framework exhibits a
 *   stable tangled-rope structure: it provides genuine coordination benefits
 *   (prevents arms races, establishes shared verification language, creates
 *   focal points for constraint-setting) while simultaneously enabling
 *   asymmetric extraction by established powers (enforcement bias toward
 *   maintaining strategic status quo, restricted access to dual-use
 *   technology for developing states, minimal voice for non-nuclear actors in
 *   existential-risk governance). The constraint's theater ratio (0.68)
 *   reflects that verification mechanisms have become increasingly
 *   performative as detection gaps widen and treaty enforcement relies on
 *   mutual forbearance rather than technical certainty. The extractiveness
 *   trajectory (0.35 → 0.58 over 50 years) shows degradation as dual-use
 *   technology diffuses, making strict non-proliferation geometrically harder
 *   to enforce while the theater of inspection and compliance rituals
 *   continues. This is not a collapsing system but a deteriorating one — the
 *   framework persists through accumulated institutional inertia, threat of
 *   reversion to worse alternatives (uncontrolled arms race), and genuine
 *   mutual interest in avoiding civilization-ending war, despite increasing
 *   evidence that technical verification cannot deliver the certainty formal
 *   treaties imply.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (US, Russia, China, UK, France): Primary beneficiaries (institutional/arbitrage) — design frameworks, enforce asymmetrically, retain deterrent arsenals justified by treaty exceptions
 *   - Non-Nuclear States: Primary victims (powerless/trapped) — mandatory compliance with non-proliferation while lacking security guarantees; constrained exit from framework
 *   - Developing Nations: Secondary victims (moderate/constrained) — face dual-use technology restrictions; benefit from climate finance but constrained by enforcement asymmetries
 *   - International Institutions (IAEA, UNFCCC, UN Security Council): Institutional enforcers (institutional/arbitrage) — maintain verification fiction; amplify enforcement against weak states while tolerating violations by powerful ones
 *   - Climate Action Coalition: Organized agents (organized/constrained) — see climate agreements as temporary scaffolds toward renewable transition; pushing for sunset clauses in nuclear deterrence logic
 *   - Treaty Architecture (Cold War Legacy Structures): Institutional inertia (institutional/arbitrage) — persist through habit and fear of uncertainty; increasingly performative as technical verification gaps widen
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes that framework is neither pure coordination nor pure extraction but irreducibly mixed, with stability dependent on unstable mutual forbearance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_framework, 0.58).
domain_priors:suppression_score(doomsday_clock_framework, 0.62).
domain_priors:theater_ratio(doomsday_clock_framework, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_framework, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_framework, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_framework, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_framework, "Global Catastrophic Risk Management Framework").
narrative_ontology:topic_domain(doomsday_clock_framework, "geopolitical/existential_risk").

domain_priors:requires_active_enforcement(doomsday_clock_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, established_power_structure).
narrative_ontology:constraint_beneficiary(doomsday_clock_framework, international_institutions).
narrative_ontology:constraint_victim(doomsday_clock_framework, non_nuclear_states).
narrative_ontology:constraint_victim(doomsday_clock_framework, developing_nations).
narrative_ontology:constraint_victim(doomsday_clock_framework, future_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES (SNARE) — Nations without nuclear arsenals are structurally trapped in a framework that offers participation in verification and treaty adherence without meaningful exit or negotiating power. They bear the suppression (strict non-proliferation enforcement) while nuclear powers retain arsenals justified by 'minimum deterrence.' Full extraction experienced: constrained security options, compliance costs with asymmetric enforcement, no voice in existential risk governance.
constraint_indexing:constraint_classification(doomsday_clock_framework, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS (TANGLED ROPE) — Mixed structural position: constrained by non-proliferation regimes but coordinating on climate agreements, technology transfer, and capacity-building frameworks. Experience both coordination benefits (access to dual-use technology under IAEA safeguards, climate finance mechanisms) and extraction (restricted access to enrichment/reprocessing, compliance verification overhead, limited voice in regime design). Constrained exit: cannot simply withdraw without international isolation.
constraint_indexing:constraint_classification(doomsday_clock_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NUCLEAR WEAPON STATES (ROPE) — Institutional actors with arbitrage options experience the framework primarily as coordination: Non-Proliferation Treaty, Comprehensive Test Ban Treaty, and climate accords enable signaling of intentions, management of strategic competition, and maintenance of existing power hierarchies. Benefits from enforcement asymmetry (can conduct subcritical testing while prohibiting others from enrichment). Net beneficiary — extraction runs toward this agent through regime design and enforcement priorities.
constraint_indexing:constraint_classification(doomsday_clock_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE ACTION COALITION (SCAFFOLD) — Organized coalitions (IPCC, UNFCCC, youth climate movements, progressive governments) see both nuclear and climate frameworks as temporary coordination scaffolds with sunset clauses: renewable energy transition will eventually obsolete nuclear deterrence logic; carbon pricing, divestment, and renewable deployment create exit paths from fossil fuel dependency. Theater moderating as technical alternatives mature. Organized agents have agency and see genuine alternatives emerging within 20-50 year horizon.
constraint_indexing:constraint_classification(doomsday_clock_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TREATY ARCHITECTURE (PITON) — The institutional structure of nuclear arms control treaties (ABM Treaty, INF Treaty, New START) and non-proliferation regimes persists largely through inertia despite degraded function. Theatre_ratio elevated (0.68) because verification is increasingly performative: CTBT monitoring cannot detect all subcritical activities; IAEA inspections face access and cooperation limits; climate agreements lack enforcement teeth. The institutional system maintains itself through diplomatic ritual and threat of reversion to uncertainty, not through active deterrence function. Strategic stability persists more from mutual restraint and fear of alternatives than from treaty provisions themselves.
constraint_indexing:constraint_classification(doomsday_clock_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a universal/civilizational analytical stance, the global catastrophic risk framework exhibits genuine coordination functions (preventing arms races, establishing verification norms, coordinating on shared existential threats) AND irreducible asymmetric extraction (enforcement biased toward maintaining status quo, restricted access to dual-use technology, voice allocation in treaty bodies correlating with military power). This is structurally tangled rope, not false mountain or degraded piton — the extractiveness (0.58) reflects that both coordination and extraction are real and necessary features of the system.
constraint_indexing:constraint_classification(doomsday_clock_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(doomsday_clock_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(doomsday_clock_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(doomsday_clock_framework, TR),
    TR >= 0.70.

:- end_tests(doomsday_clock_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The framework systematically advantages established nuclear powers in three ways: (1) enforcement prioritizes preventing proliferation by weak states while ignoring maintenance of strong-state arsenals; (2) access to dual-use technology (uranium enrichment, advanced materials) is restricted asymmetrically; (3) voice in existential-risk governance correlates with military power, not affected-population size. The extractiveness has grown over 50 years as proliferation pathways have multiplied and detection gaps have widened, forcing reliance on diplomatic pressure and sanctions rather than technical verification. Suppression (0.62): High. Multiple barriers constrain non-nuclear and developing states: IAEA inspection protocols, technology export controls (Nuclear Suppliers Group), sanctions regimes against violations, and the structural constraint that nuclear deterrence monopoly creates security dependency on NPT protection. However, suppression is not total — covert programs succeed, sanctions can be evaded, and weaker states retain meaningful agency. Theater ratio (0.68): High and increasing. Verification mechanisms rely on inspections that are often cooperative (host country grants access), satellite surveillance that can miss shielded facilities, and signature analysis that expert adversaries can defeat. The CTBT's monitoring cannot detect all subcritical testing; IAEA safeguards face recurring access disputes and cannot verify past military programs. The framework's stability increasingly depends on trust and mutual restraint rather than technical certainty, yet formal language persists in describing verification as technically robust. This gap between form and function defines the piton-pattern theater increase.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence across all six types. Nuclear powers' Rope classification reflects their genuine experience of coordination and deterrence stability. Non-nuclear states' Snare classification reflects genuine structural vulnerability and dependency. The scaffold classification validates the climate coalition's real observation that alternatives are emerging. The piton classification captures the true degradation of verification as technical gaps widen. The tangled-rope classifications (developing nations and analytical observer) refuse both false optimization (pure coordination) and false doom (pure extraction). This perspectival spread is not a failure of the framework to classify — it is the framework's honest structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations and exit options. Nuclear weapon states are beneficiaries with arbitrage options: they can exit the framework (revert to uncontrolled deterrence) but choose to stay because the framework constrains competitors while preserving their own arsenal. This produces low d → negative/low effective extraction (Rope experience). Non-nuclear states are victims with trapped options: they cannot credibly acquire weapons (sanctions, isolation, existential risk) and they benefit from non-proliferation guarantees (attackers lack nuclear backing), so they are bound to the framework. This produces high d → high effective extraction (Snare experience). Developing nations are mixed: they can aspire to weapons (costs high but not impossible) and they benefit from technology transfer and climate finance, producing moderate d and tangled-rope experience. The climate coalition has organized capacity and real alternatives (renewable transition), constraining but not trapping their exit — moderate d, moderate extraction. Treaty architecture persists through inertia and threat of reversion — its benefits are not primary (would still exist in some form without current treaties) but institutional self-preservation is real. The analytical observer, by definition, has analytical exit and sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies how mandatrophy (false coordination versus false extraction) is resolved by accepting structural complexity rather than forcing consensus. The framework IS genuine coordination (prevents arms races, establishes verification norms, enables signaling of intentions, reduces uncertainty). The framework IS genuine asymmetric extraction (enforces non-proliferation asymmetrically, restricts technology access, privileges nuclear powers in existential-risk governance). Both are simultaneously true. The mandatrophy resolves by refusing to pick one: the system is fundamentally tangled rope, not a coordination mechanism disguised as extraction (false narrative) or an extraction regime disguised as coordination (cynical narrative). The analytical observer perspective validates this tangling: extractiveness of 0.58 means genuine mixed structure, not a measurement error or classification ambiguity. The theater ratio (0.68) reflects that the framework increasingly relies on mutual restraint and institutional inertia rather than technical verification capability. The Piton pattern (verification theater increasing) does not change the core classification — verification degradation is a symptom of the tangled structure, not evidence of false classification. As technology advances and detection gaps widen, the framework's extractiveness may increase (toward 0.70+) while theater remains high, potentially transitioning toward Snare dominance if mutual restraint fails. This trajectory is tracked through measurement data: extractiveness rising from 0.35 → 0.58 over 50 years indicates institutional drift toward higher asymmetry, while theater (0.42 → 0.68) indicates functional degradation. Mandatrophy is resolved: the system is tangled because it must be — any governance system for existential risks operating among unequal powers will be both coordination (preventing mutual destruction) and extraction (privileging certain actors' risk preferences).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credible_commitment_sufficiency,
    'Are existing verification mechanisms (IAEA inspections, CTBT monitoring, satellite surveillance) sufficient to credibly commit signatories to non-proliferation, or do they primarily serve as political theater while sophisticated weapons programs remain undetectable?',
    'Forensic analysis of nuclear programs that evaded detection (Iran pre-JCPOA, North Korea, Libya); assessment of detection sensitivity gaps relative to realistic proliferation pathways; simulated penetration testing of inspection regimes',
    'If sufficient: framework is coordination mechanism with modest extraction overhead (Rope from more perspectives). If theater-dominated: framework is extraction regime hiding behind verification language (Snare prevalence increases, piton classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credible_commitment_sufficiency, empirical, 'Whether verification mechanisms provide credible commitment or primarily theater').

omega_variable(
    deterrence_stability_vs_mutual_restraint,
    'Does strategic stability derive from the structure of the non-proliferation treaty framework itself, or from deeper mutual interest in avoiding nuclear exchange regardless of formal agreements?',
    'Counterfactual analysis: what prevents proliferation in absence of treaties? Correlation of treaty signing with actual proliferation rates vs. geopolitical interest in nuclear weapons. Historical analysis of violations and consequences (Iran JCPOA, North Korea, India/Pakistan non-signatory stability).',
    'If treaties are essential: they are true coordination (Rope, Scaffold legitimate). If restraint is mutual and treaty-independent: framework is largely performative (Piton, Snare extraction becomes visible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_mutual_restraint, empirical, 'Whether deterrence stability requires formal agreements or reflects mutual interest').

omega_variable(
    alternative_catastrophic_risk_governance,
    'Could global catastrophic risk governance be achieved through alternative institutional structures with lower extraction and theater costs (e.g., distributed surveillance networks, cryptocurrency-enforced escrow arrangements, AI-monitored transparency regimes)?',
    'Comparative institutional analysis of existing frameworks vs. proposed alternatives; simulation of detection/deterrence capabilities under different architectural assumptions; case studies of lower-stakes coordination mechanisms that achieved similar verification with lower overhead',
    'If viable alternatives exist: current framework is path-dependent extraction lock-in (Snare from many perspectives). If alternatives fail or are untested: framework''s asymmetries may reflect genuine structural constraints (Tangled Rope justified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_catastrophic_risk_governance, conceptual, 'Whether alternative governance architectures could reduce extraction costs').

omega_variable(
    climate_agreement_enforcement_credibility,
    'Do climate agreements (Paris Accord, Net Zero commitments) function as meaningful coordination mechanisms or primarily as signaling/theater with minimal enforcement leverage?',
    'Tracking of stated NDCs vs. actual emissions reductions; analysis of penalty mechanisms and their application; comparison of voluntary commitment compliance vs. treaty-mandated compliance under other regimes (e.g., trade agreements)',
    'If enforcement credible: climate framework is Scaffold (sunset to renewable transition) validated. If theater-dominated: climate layer is pure Piton, and entire framework''s theater_ratio rises toward snare range.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_agreement_enforcement_credibility, empirical, 'Whether climate agreements provide meaningful enforcement or remain aspirational').

omega_variable(
    future_state_proliferation_inevitability,
    'As materials science and biotech advance, does the non-proliferation regime face structural collapse (impossible to prevent weapons-grade material production without global surveillance state), or can norms and asymmetric enforcement persist indefinitely?',
    'Trajectory analysis of technological accessibility to weapons-relevant materials; assessment of detection gaps under future scenarios; comparison with historical periods of technological proliferation (nuclear, chemical, biological)',
    'If regime faces structural collapse: framework is doomed extraction lock-in (high theater, degraded function, Piton-to-Snare transition). If norms persist: framework achieves Rope legitimacy through sustained coordination despite technology trends.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_state_proliferation_inevitability, empirical, 'Whether non-proliferation regime can survive advancing dual-use technology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_framework, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(doom_tr_t25, doomsday_clock_framework, theater_ratio, 25, 0.58).
narrative_ontology:measurement(doom_tr_t50, doomsday_clock_framework, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(doom_be_t25, doomsday_clock_framework, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(doom_be_t50, doomsday_clock_framework, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(doomsday_clock_framework, nuclear_deterrence_credibility).
narrative_ontology:affects_constraint(doomsday_clock_framework, climate_transition_finance).
narrative_ontology:affects_constraint(doomsday_clock_framework, biosecurity_norm_formation).
narrative_ontology:affects_constraint(doomsday_clock_framework, dual_use_technology_diffusion).

% DUAL FORMULATION NOTE:
% The doomsday_clock_framework decomposes into four structural constraint families: nuclear deterrence (ε≈0.42, stability dependent on mutual forbearance rather than technical verification), climate agreements (ε≈0.35, increasingly scaffolded by renewable transition), biosecurity norms (ε≈0.64, enforcement capability severely limited), and dual-use technology access (ε≈0.51, increasingly difficult to control as manufacturing diffuses). Each has different extractiveness, suppression, and theater profiles. The unified framework story captures their institutional coupling — treaties reference each other, enforcement mechanisms overlap, and strategic logic binds them — but decomposition into family stories enables precise analysis of each component's actual function. The doomsday_clock_framework represents their coherence as a system, while the four downstream constraints model their individual structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_framework, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
