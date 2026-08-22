% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary (Contingent Reachability Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint instantiates the CONTINGENT REACHABILITY READING of the
 *   total war reachability kernel. The reading asserts that reachability of
 *   total war—the actual feasibility of winning a major military conflict at
 *   acceptable cost—is contingent on the current technological equilibrium.
 *   Under this reading, the constraint is a PITON: an atrophied coordination
 *   function (deterrence used to require active strategic choice to maintain;
 *   now it persists by institutional momentum and theatrical doctrine) that
 *   could reverse with technological change. The key diagnostic is the rising
 *   theater_ratio (0.05 at inception to 0.68 at present), indicating that an
 *   increasing share of deterrence activity is performative (declaratory
 *   doctrine, confidence-building measures, arms control negotiations that
 *   achieve little) rather than functional (actual restraint on weapons
 *   development). Beneficiaries are states investing in destabilizing
 *   technologies that could restore reachability; victims are global
 *   populations who depend on the constraint's stability for existential
 *   security. The constraint is NOT claimed as natural law (the contraction
 *   reading) nor as a durable rope (the dropping reading), but as a
 *   contingent coordination that persists by technological accident and could
 *   unravel as technology evolves.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: institutional agents that sustain the doctrine and enforce deterrent rules
 *   - states_investing_destabilizing_tech: beneficiaries of reachability's return; drive toward technological advantage
 *   - global_populations: powerless victims whose existential fate is locked to the constraint's stability
 *   - strategic_technologists: organized actors caught between research incentive (destabilizing) and risk (deterrence failure)
 *   - deterrence_strategists: institutional maintainers of the narrative that reachability is impossible
 *   - emerging_powers: excluded from doctrine consensus but affected by its stability
 *   - disarmament_advocates: excluded voices arguing for permanent contraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.62).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.71).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary (Contingent Reachability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'bb245142-3015-46f3-b35d-10776f92324f').
narrative_ontology:cs_kernel_codification('bb245142-3015-46f3-b35d-10776f92324f', distributed).
narrative_ontology:cs_authority_grounding('bb245142-3015-46f3-b35d-10776f92324f', extraction).
narrative_ontology:cs_interpretation_layer_present('bb245142-3015-46f3-b35d-10776f92324f').
narrative_ontology:cs_reading_relation('bb245142-3015-46f3-b35d-10776f92324f', total_war_reachability_boundary__contraction_reading, influences).
narrative_ontology:cs_reading_relation('bb245142-3015-46f3-b35d-10776f92324f', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('bb245142-3015-46f3-b35d-10776f92324f', foundational, reachability_technology_contingent).
narrative_ontology:cs_axiom_status(reachability_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('bb245142-3015-46f3-b35d-10776f92324f', reachability_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('bb245142-3015-46f3-b35d-10776f92324f', foundational, current_contraction_is_atrophied_piton).
narrative_ontology:cs_axiom_status(current_contraction_is_atrophied_piton, holdable).
narrative_ontology:cs_axiom_grounding('bb245142-3015-46f3-b35d-10776f92324f', current_contraction_is_atrophied_piton, empirically_contingent).
narrative_ontology:cs_reference_frame('bb245142-3015-46f3-b35d-10776f92324f', technological_equilibrium_contingency).
narrative_ontology:cs_drift_state('bb245142-3015-46f3-b35d-10776f92324f', contemporary_2025_destabilization_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bb245142-3015-46f3-b35d-10776f92324f', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_destabilizing_tech).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_populations_deterrence_dependency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, strategic_technologists_military_innovation).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, strategic_technologists_military_innovation).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, emerging_powers_deterrence_outsiders).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, technological_equilibrium_contingency).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, reachability_state_dependence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the nuclear deterrence architecture through strategic doctrine, weapons modernization, and continuous signaling. They administer the constraint by sustaining the technological equilibrium that keeps total war outside the feasible set. No state can unilaterally exit nuclear deterrence; departure signals weakness and invites others to rearm. Their exit is trapped not by external force but by the mutual dependence of all nuclear actors on the same deterrence logic.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_armed_states, agenda_setter,
    institutional, civilizational, trapped, global).

% Invest in technologies (hypersonic delivery, autonomous targeting, AI-accelerated decision cycles, anti-satellite weapons) that erode the stability of the current equilibrium. They benefit from reachability: a world where total war moves back into the strategic feasible set would permit rapid technological advantage before deterrent-level deployment. The constraint persists because these investments remain marginal relative to the deterrent arsenal, but every marginal investment incrementally shifts the strategic landscape. Their exit is trapped by the same logic as agenda-setters: unilateral restraint on destabilizing research signals weakness and invites others to accelerate.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_destabilizing_tech, beneficiary,
    institutional, civilizational, trapped, global).

% Depend entirely on the stability of the reachability constraint to avoid existential risk. They bear the cost of deterrence infrastructure (opportunity cost of military spending, environmental effects of nuclear arsenals, the cognitive burden of living under permanent extinction risk that is not their choice). The constraint persists only because the alternative—reachability of total war—is unthinkable. Their resistance to the constraint is inert; exit is impossible. They are locked into the strategic choices of others.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_populations_deterrence_dependency, payer,
    powerless, civilizational, trapped, global).

% Design and deploy destabilizing technologies. They benefit from research funding and strategic relevance that flows when deterrence is under pressure. They also bear risk: if reachability actually reverses (the piton unravels), they become targets of opposing weapons systems designed to counter their innovations. Their exit is constrained by career path dependence (a technologist cannot simply abandon weapons research without losing professional standing) and institutional lock-in (military research institutions depend on continuous contract funding).
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_technologists_military_innovation, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, strategic_technologists_military_innovation, payer).

% Intellectually maintain the doctrine that total war is unreachable and irrational. They produce the strategic narratives that sustain belief in the constraint's inevitability. Their role is partially theatrical: the doctrine's persuasiveness depends on it being presented as self-evident truth rather than contingent technological equilibrium. If reachability were contested openly, the doctrine's power would erode. They are invested in the piton's continuation because acknowledging its contingency would require openly defending choices that benefit institutional actors.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, deterrence_strategists_doctrine, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, deterrence_strategists_doctrine, observer).

% Are excluded from the club of states whose strategic preferences determine the constraint's persistence. They pay through military spending to acquire nuclear parity, but their voice in sustaining or revising the deterrence doctrine is marginal. They would benefit if reachability reversed because rapid technological change could displace established powers. Their exclusion is structural—the constraint's stability depends on keeping the number of decision-makers small. Their constrained exit reflects the fact that abandoning nuclear acquisition would signal weakness, but developing nuclear weapons invites pressure from established powers.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, emerging_powers_deterrence_outsiders, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, emerging_powers_deterrence_outsiders, excluded).

% Argue that the constraint is illegitimate and should be permanently contracted (moving from piton to stable mountain by eliminating all nuclear arsenals). They are structurally excluded from the consensus that sustains the piton because their foundational claim—that nuclear weapons should not exist—contradicts the agenda-setters' core interest in maintaining deterrent capability. Their constrained exit reflects that advocating for disarmament puts them in opposition to powerful institutional actors who benefit from the status quo.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, disarmament_movements_advocates, excluded,
    moderate, generational, constrained, global).

% Monitors the technological trajectory and classifies the constraint's structural status. From this seat, the piton's stability is contingent on the equilibrium remaining in place; the moment destabilizing technologies shift from marginal to critical, the constraint transforms from piton (atrophied, performatively maintained) into scaffold (temporary coordination under existential pressure) and potentially into snare (if deterrence capability becomes asymmetric). The observer seat is the only one with genuine exit: it can adopt alternative classifications and frameworks. Its position is privileged but also removed from the real strategic choices.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, analytical_observer_seat, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, states_investing_destabilizing_tech).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared strategic narrative that total war is outside the feasible set, because the costs of nuclear exchange exceed any conceivable benefit. This narrative reduces uncertainty about the rationality of nuclear first use and creates mutual incentive to avoid first-strike calculation. Without the shared belief in the constraint's stability, nuclear-armed states would pursue technological advantage through destabilizing research, which would increase the probability of accidents or miscalculation during periods of tension.
% TRANSFER_FUNCTION: Moves the existential risk of nuclear war onto the global population as the price of keeping state-level military competition nonlethal. Populations accept permanent extinction risk (and the opportunity cost of military spending diverted from development) in exchange for states forgoing total war. Strategists and research institutions benefit from career prestige and funding derived from managing the constraint; states investing in destabilizing technologies extract advantage through research breakthroughs that incrementally shift the feasible set.
% ABSENT_VOICES: Disarmament advocates who argue for permanent contraction (full elimination of nuclear arsenals), emerging nuclear powers excluded from the doctrine-setting consensus (India, Pakistan, North Korea, Iran), future generations who inherit the technological risk landscape without choosing it, and Global South populations whose military spending is high relative to development needs. The constraint persists because the excluded voices lack institutional power to alter it.
% DISAPPEARANCE_RATIONALE: Under the contingent-reachability reading, if the constraint disappeared overnight—if reachability of total war returned to the feasible set—the world would rearrange catastrophically: states would pursue destabilizing technologies, deterrence stability would depend on fragile first-strike windows, accidents or miscalculation would become existentially probable, and military competition would return to cost-benefit calculation of winning nuclear war. Under the contraction reading (a sibling), if the constraint disappeared because all nuclear weapons were eliminated, the world would rearrange toward permanent peace. Under the dropping reading (another sibling), the constraint would remain stable because deterrence is a rope (coordination equilibrium), not contingent on continued technological accident.
% FOUNDING_PROBLEM: In the mid-twentieth century, thermonuclear weapons created a novel strategic problem: the cost of total war between industrial superpowers exceeded any achievable objective, making victory impossible in the traditional sense. The founding problem was how to maintain great-power competition, resolve disputes, and preserve the state system without triggering mutual annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear strategists and deterrence scholars (Schelling, Brodie, Jervis, Waltz) attest the founding problem was real and acute during the Cold War. However, disarmament scholars and emerging-power strategists attest the founding problem is being partially resolved through proliferation dynamics (more actors with weapons creates new interdependencies), through institutional development (non-proliferation treaties, confidence-building measures, arms control agreements), and through economic interdependence that makes total war irrational even without nuclear weapons. The contest over founding_problem_status reflects the deeper contest over whether the piton is a stable equilibrium that solves the founding problem or a decaying institutional structure that postpones rather than solves it.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.62) is high and rising because the constraint's operation increasingly serves interests other than collective security: research funding, institutional prestige, strategic advantage through technological edge. The suppression (0.71) is high because the constraint requires active enforcement against proliferation, destabilizing research, and doctrinal dissent. The theater_ratio (0.68) is the diagnostic: it has risen from 0.05 at inception because deterrence has shifted from active mutual restraint to performative confidence-building. At inception (1945), deterrence was an urgent coordination problem requiring continuous negotiation and nuclear restraint—low theater. By 2025, the rules are institutionalized, doctrine is treated as natural truth, and enforcement consists largely of declaratory postures and ritual arms-control negotiations. This rise in theater indicates the piton is atrophying: the original coordination function (preventing total war through credible mutual deterrence) persists, but the mechanism for maintaining it has become inert. The accessibility_collapse (0.45) is moderate because states retain theoretical alternatives (disarmament, arms buildup, preemptive strike) but face institutional and technical barriers to exercising them. The resistance (0.58) is moderate-high because deterrence doctrine meets continuous pressure from disarmament advocates, proliferation dynamics, and technological change that threatens the equilibrium. The measurement series traces the piton's evolution: extractiveness and theater rising in parallel indicates the constraint is becoming hollowed out—gains are accruing to beneficiaries (research funding, strategic positioning) while the functional coordination (preventing war through deterrence) remains stable only by institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The cardinal gap is between institutional actors who benefit from deterrence stability (agenda-setters) and those who benefit from destabilizing it (states investing in eroding tech, strategists whose research attracts funding when deterrence is contested). For the first group, the constraint is nearly rope-like: it coordinates genuine security interests. For the second group, it is snare-like: they extract benefit from its decay. The global population experiences both: genuine security benefit from deterrence's success, but extraction of existential risk and military-spending opportunity cost. This divergence is the piton's signature: a constraint that persists despite serving opposed interests because the cost of maintaining it is diffuse (delegated to states) and the cost of challenging it is catastrophic (reverting to reachable total war). No single seat captures enough benefit to maintain it actively; no single seat bears enough cost to fix it; therefore it persists by institutional autopilot.
 *
 * DIRECTIONALITY LOGIC:
 *   From the nuclear-armed-states agenda-setter seat, the constraint is a legitimate deterrence coordination they administer and benefit from maintaining. From the states-investing-destabilizing-tech beneficiary seat, the constraint is slowly eroding, and they position to exploit the reversal (d moderately toward target—they pay research costs but benefit if reachability returns). From the global-populations seat, the constraint is pure victimization: existential risk in exchange for states maintaining military competition (d fully toward target, trapped exit, civilizational time horizon—maximum vulnerability). The directionality divergence is extreme: from the agenda-setter, this is a rope (genuine coordination benefit). From the payer seats, this is a snare (extraction masked as coordination). The engine computes per-seat classification from the structural data; the claim (piton from this reading) reflects the reading's own analytical position (neither pure coordination nor pure extraction, but a degraded institutional mechanism).
 *
 * MANDATROPHY ANALYSIS:
 *   The piton is the correct classification because the constraint exhibits the three mandatrophy signatures: (1) its founding problem (preventing nuclear war) is contested as to whether it still exists or has been superseded by arms control and economic interdependence; (2) no seat captures concentrated benefit sufficient to defend it actively—deterrence is collectively maintained but individually eroding as each state invests in edge-gaining technologies; (3) the theater_ratio's rise indicates performative maintenance replacing functional coordination. If the founding problem is declared dead (by the contraction reading), the piton should formalize into a mountain (reachability is permanently inaccessible). If the founding problem is declared live but the institutional mechanism is atrophied, it remains piton or shifts to scaffold under pressure (temporary coordination) if states return to active deterrence negotiation. The ambiguity between these outcomes is what the omegas capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_reversal_contingency,
    'Is the current contraction of reachability a structural feature (mountain, impossible to reverse) or a technological contingency (piton, reversible if destabilizing technologies mature)?',
    'Trajectory analysis of destabilizing technologies (hypersonic delivery, autonomous targeting, AI decision-acceleration, anti-satellite systems). If these reach critical mass while strategic warning remains brief, reachability reverses. If they stabilize at marginal levels or if defensive countermeasures mature faster, reachability remains contracted.',
    'If contingent, the piton classification holds and the constraint could unravel. If structural, the constraint should be reclassified as a mountain and the beneficiaries of destabilizing research are engaging in futile endeavor. This is the central contested empirical fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_reversal_contingency, empirical, 'Whether technological change can reverse reachability or whether it is permanently contracted.').

omega_variable(
    deterrence_doctrine_vs_technological_reality,
    'Does the rising theater_ratio indicate that deterrence doctrine has become performative while technological reality diverges, or does the doctrine remain functionally coupled to state behavior?',
    'Behavioral analysis: if states act on the doctrine (restraining proliferation, enforcing no-first-use norms, investing heavily in verification) despite the performative character, the coupling is real. If states ignore the doctrine (pursuing edge-gaining technologies, developing counterforce capabilities, preparing for nuclear-warfighting scenarios), the doctrine is hollow.',
    'If the doctrine is performative and decoupled, the piton is structurally fragile; a single misinterpretation or technological surprise could shatter it. If the coupling is real, the piton is more stable than the high theater_ratio suggests. This affects the reversal timeline and probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_doctrine_vs_technological_reality, empirical, 'Whether deterrence doctrine still governs state behavior or has become pure theater.').

omega_variable(
    beneficiary_stability_paradox,
    'States investing in destabilizing technologies are listed as beneficiaries of reachability. But if reachability reverses, they become targets. Are they truly beneficiaries, or are they trapped actors pursuing incentives that lead to mutual destruction?',
    'Game-theoretic analysis of the incentive structure: if first-mover advantage in destabilizing technology is large enough that states rationally pursue it even knowing it increases mutual risk, they are beneficiaries of the constraint''s erosion (high d toward target, but extracting advantage in the transition window). If the incentive is illusory (the advantage disappears once others follow), they are trapped actors pursuing a race dynamic that benefits no one.',
    'If truly beneficiaries, the constraint''s decay is driven by rational extraction. If trapped, the constraint''s decay is driven by collective action failure. The distinction affects whether the piton can be stabilized through coordination or whether it requires structural change (disarmament or technological containment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_stability_paradox, conceptual, 'Whether destabilizing technology investment is rational benefit-seeking or tragic race dynamics.').

omega_variable(
    founding_problem_status_contest,
    'Is the founding problem (preventing nuclear war) still live, or has it been partially solved by institutions (non-proliferation treaties, confidence-building measures, economic interdependence) that now sustain safety independent of the reachability constraint?',
    'Counterfactual: if the reachability constraint vanished tomorrow but all institutions remained in place, would nuclear war probability remain low? If yes, the founding problem is substantially solved and the constraint is vestigial. If no, the constraint is still functionally essential.',
    'If the founding problem is solved, the piton is formalized into a mountain and mandatrophy is full. If it is still live, the piton classification holds. This directly affects whether permanent contraction (disarmament) is feasible or whether deterrence must persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_status_contest, conceptual, 'Whether institutions have solved the founding problem or whether deterrence remains essential.').

omega_variable(
    reading_boundary_interpretation,
    'Does the contingent-reachability reading''s claim that ''reachability is technology-dependent'' mean that reachability is an inherent property of technology, or that reachability is a strategic interpretation dependent on how states deploy technology?',
    'Definitional: if hypersonic missiles exist but states interpret them as non-destabilizing (due to short flight times making launch-detection-response feasible), is reachability restored? The reading must clarify whether reachability is a physical property (missiles can deliver payloads fast enough to defeat deterrence) or a strategic property (states believe they can win).',
    'If reachability is physical, the constraint''s stability depends on material facts (tech development). If reachability is strategic, it depends on shared interpretation; doctrine could remain stable even as technology changes, if the interpretation framework doesn''t shift. This affects the piton''s vulnerability to technological surprise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_interpretation, conceptual, 'Whether reachability is a technological or interpretive property.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1945, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2025, 0.68).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).
narrative_ontology:measurement(tota_tr_t2050, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2050, 0.75).
narrative_ontology:measurement_basis(tota_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(tota_be_t2025, observed).
narrative_ontology:measurement(tota_be_t2050, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2050, 0.68).
narrative_ontology:measurement_basis(tota_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1945, 0.42).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1980, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement_basis(tota_su_t1980, observed).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(tota_su_t2000, observed).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(tota_su_t2025, observed).
narrative_ontology:measurement(tota_su_t2050, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2050, 0.74).
narrative_ontology:measurement_basis(tota_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, nuclear_proliferation_control).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, strategic_stability_verification_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, first_strike_advantage_calculation).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel has three structurally distinct constraint readings: contingent_reachability_reading (this file) reads the boundary as technology-dependent and contingent, classifying it as piton (atrophied capability). contraction_reading reads the boundary as structurally permanent (mountain) post-thermonuclear era. dropping_reading reads it as a durable coordination (rope) maintained by deterrence equilibrium. Each reading has its own ε (extractiveness), beneficiary/victim structure, and type. They share the same referent (the reachability of total war in current strategic context) but disagree fundamentally on whether it is a natural limit (contraction), a contingent equilibrium (this reading), or a maintained coordination (dropping). The three readings are linked by network.affects_constraints because assertions about reachability directly constrain the plausibility of the others; doctrine in one reading generates strategic pressure in the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
