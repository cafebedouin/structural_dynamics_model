% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability: Contingent Technological Atrophy Reading
 *   domain: international_relations/nuclear_deterrence/strategic_stability
 *
 * SUMMARY:
 *   The total war reachability boundary constrains states' strategic options
 *   and civilian vulnerability in the nuclear era. Under the
 *   contingent_reachability reading, the current configuration — where total
 *   war appears infeasible due to mutual vulnerability, weaponized precision,
 *   and deterrence doctrine — is not a fundamental structural change but a
 *   temporary atrophy of capability maintained by technological impedance and
 *   institutional theater. If destabilizing technologies (hypersonic
 *   delivery, autonomous targeting, artificial intelligence in
 *   decision-making, quantum cryptanalysis breaking current encryption)
 *   overcome current barriers, total war could return to the feasible set.
 *   The constraint is thus a piton: the appearance of permanent contraction
 *   (deterrence stability) masks that the system depends on sustained
 *   technological suppression of reachability. Beneficiaries are states
 *   investing in destabilizing technologies that could restore their
 *   strategic options; victims are civilian populations whose safety depends
 *   on maintaining the current technological equilibrium. The theater
 *   component (0.76) reflects that the 'total war is no longer reachable'
 *   doctrine is institutionalized and enforced through repeated affirmation
 *   of deterrence stability, not through any irreversible structural change.
 *   Military establishments maintain this performance; policy communities
 *   reinforce it; strategic thinkers write it into doctrine. But the
 *   underlying technology is eroding the basis for the performance.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary victims (powerless/trapped) — bear existential risk if reachability expands; no exit from deterrence system
 *   - Deterrence-Maintaining Military Establishments: Institutional actors (institutional/constrained) — maintain performative stability through doctrine enforcement; constrained by need to preserve the theater without admitting its contingency
 *   - States Maintaining Current Technological Equilibrium: Primary beneficiaries (powerful/mobile) — invest in non-proliferation, arms control, and tech restrictions to enforce current reachability contraction; have exit options but choose constraint
 *   - States Pursuing Destabilizing Technologies: Secondary beneficiaries (organized/constrained) — benefit from reachability expansion; constrained by non-proliferation regimes but pursuing AI, hypersonics, autonomous systems that could restore capabilities
 *   - Technology-Neutral Epistemic Communities: Rope coordinators (powerful/arbitrage) — operate with neutral stance on reachability; benefit from consensus and standardization; solve coordination problem without extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the entire system as contingent piton maintained by theater; views the measurement of 'reachability' itself as contestable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.28).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.65).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability: Contingent Technological Atrophy Reading").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/nuclear_deterrence/strategic_stability").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'f4485369-7e52-4444-9211-c7e2c7451545').
narrative_ontology:cs_kernel_codification('f4485369-7e52-4444-9211-c7e2c7451545', distributed).
narrative_ontology:cs_authority_grounding('f4485369-7e52-4444-9211-c7e2c7451545', extraction).
narrative_ontology:cs_reading_relation('f4485369-7e52-4444-9211-c7e2c7451545', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4485369-7e52-4444-9211-c7e2c7451545', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('f4485369-7e52-4444-9211-c7e2c7451545', foundational, reachability_contingent_on_technology).
narrative_ontology:cs_axiom_status(reachability_contingent_on_technology, holdable).
narrative_ontology:cs_axiom_grounding('f4485369-7e52-4444-9211-c7e2c7451545', reachability_contingent_on_technology, empirically_contingent).
narrative_ontology:cs_axiom('f4485369-7e52-4444-9211-c7e2c7451545', foundational, deterrence_stability_theater_dependent).
narrative_ontology:cs_axiom_status(deterrence_stability_theater_dependent, holdable).
narrative_ontology:cs_axiom_grounding('f4485369-7e52-4444-9211-c7e2c7451545', deterrence_stability_theater_dependent, empirically_contingent).
narrative_ontology:cs_reference_frame('f4485369-7e52-4444-9211-c7e2c7451545', technology_limited_reachability_framework).
narrative_ontology:cs_drift_state('f4485369-7e52-4444-9211-c7e2c7451545', contemporary_destabilizing_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f4485369-7e52-4444-9211-c7e2c7451545', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, power_maximizing_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, strategic_stability_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (PITON) — Trapped in a system where deterrence stability depends on a contingent technological equilibrium. The current reachability contraction is performative — deterrence is maintained by ritual adherence to doctrines and force postures that would crumble if technology shifted. The theater of 'stable deterrence' masks that the system's integrity depends wholly on sustained technological impedance. Populations bear the existential risk if reachability expands; they have no exit.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DETERRENCE-MAINTAINING INSTITUTIONS (PITON) — Military establishments invested in stable deterrence see themselves as maintaining a performative balance. They enforce doctrines, readiness protocols, and force postures that are largely theater — they work because everyone acts as if the system is stable, not because the underlying technology has fundamentally changed the feasibility of total war. These institutions are constrained by the need to maintain the performance without admitting its contingency. The extraction here is institutional inertia: resources devoted to maintaining a degraded system.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STATES MAINTAINING EQUILIBRIUM (SCAFFOLD) — Powerful states with interests in sustaining the current technological balance see reachability contraction as a temporary, reversible constraint dependent on sustained technological impedance. This is genuinely a scaffold — a temporary coordination solution with a sunset clause. The beneficiary states invest in arms control agreements, non-proliferation frameworks, and technological restrictions that enforce the current equilibrium. The sunset is implicit: if these investments lapse or are overcome, reachability expands and deterrence fragility emerges. These states have exit options (they could pursue destabilizing technologies) but choose constraint.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: DESTABILIZING TECHNOLOGY INVESTORS (SNARE) — States investing in technologies that could restore total-war reachability (hypersonics, artificial intelligence in targeting, quantum computing for cryptanalysis, autonomous systems) experience the current equilibrium as a suppressive constraint. They benefit from expansion of reachability — it increases their strategic options and bargaining power. The constraint is extractive for them: current technology limitations suppress their capability development. These states bear costs from non-proliferation regimes and technological restrictions. However, they are organized and have constrained (not trapped) exit — they can pursue destabilizing tech at cost of sanctions and escalation risk.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC COMMUNITIES ON TECH (ROPE) — Scientific communities studying weapons effects, strategic stability, and emerging technologies operate with epistemically neutral stance: reachability is what technology permits. From this perspective, the contraction is a coordination achievement — the epistemic community benefits from stable consensus that current tech limits total war and coordinates on the doctrines and measurement standards that enforce this understanding. The rope is pure: the community solves the legitimacy coordination problem (everyone agrees on what 'reachable' means) with minimal extraction.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From the civilizational/universal perspective, the entire current deterrence system is a piton — an atrophied capability maintained by performative consensus that total war is no longer reachable. The theater here is not 'deterrence works' but 'total war is impossible' — a doctrine that persists because it has been institutionalized, not because the underlying physics or technology has fundamentally changed. The analytical observer sees that the contraction is contingent on sustained technological impedance and that shifts in AI, hypersonic delivery, autonomous targeting, or cryptographic breaking could restore reachability within years. The piton classification reflects that the 'it is no longer reachable' framing is increasingly theater — the actual technological barriers are eroding.
constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_reachability_boundary__contingent_reachability_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, TR),
    TR >= 0.70.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The constraint's primary function is suppression of destabilizing technology development and enforcement of reachability contraction, not extraction in the traditional sense. However, states maintaining the equilibrium do extract value by preserving their relative strategic positions and preventing capability shifts that would undermine their deterrent. The beneficiaries (equilibrium-maintaining states) benefit from technological suppression that limits competitors' options. Victims (populations and destabilizing-tech investors) bear costs. The value is set at 0.28 to reflect that this is primarily a suppression mechanism with secondary extractive function. Suppression (0.65): High. The constraint operates by suppressing technological development, restricting proliferation, enforcing export controls, and maintaining doctrinal commitments to mutual vulnerability. These are not voluntary coordination mechanisms but enforced limitations on state behavior. The suppression is substantial but not total — destabilizing technology development continues; non-proliferation regimes have failures; doctrine can shift. Theater ratio (0.76): High. The piton classification is driven by the high theater ratio. The 'total war is no longer reachable' doctrine is maintained through repeated institutional affirmation, doctrine training, force posture displays, and arms control negotiations. But the underlying technological basis for this performance is eroding. The performance persists because institutions, communities, and states have invested in the narrative and the structures that maintain it. If the narrative shifted, those institutions would need to shift their entire operational framework — so the theater is sustained not by function but by institutional path dependence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives show how the same structural constraint (reachability boundary) appears as piton (theater-maintained atrophy) to powerless and institutional observers, as scaffold (temporary tech-dependent constraint with sunset) to states maintaining equilibrium, as snare (suppressive constraints on destabilizing tech) to states pursuing capability expansion, and as rope (pure coordination on doctrinal standards) to epistemic communities. The gap between beneficiaries (who see the equilibrium as valuable scaffold or rope) and victims (who see theater-maintained peril) is substantial. The analytical observer's piton classification emphasizes that the entire system is contingent — the theater of 'reachability contraction' masks that the underlying technological barriers are eroding and that destabilizing technology development could restore total war to the feasible set within years or decades. The reading is that reachability is not permanently contracted but temporarily and precariously suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural positions and beneficiary/victim declarations. States maintaining the technological equilibrium are beneficiaries with mobile exit (they choose to enforce restrictions despite having options) → low d, negative χ. States pursuing destabilizing technologies are victims of suppression despite being potentially powerful → high d, high χ. Civilian populations trapped in deterrence are victims with no exit → maximum d, maximum f(d). Institutions maintaining the performance (military establishments) are constrained beneficiaries — they benefit from the current system but are constrained by the need to maintain its theater → moderate d. Epistemic communities are near-neutral coordinators → d around 0.5. The piton classification derives from the theater ratio gate (≥0.70) and the atrophied coordination function — the system's primary activity is maintaining the narrative rather than solving a genuine coordination problem.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: The contingent_reachability reading resolves its mandatrophy (the apparent paradox of calling something 'reachability contraction' if the underlying technology can restore reachability) by clarifying that the reading is about the *current* operational constraint (technological impedance + institutional theater) not about a *permanent* structural change. The piton classification captures this: the system appears to have permanently contracted reachability (the mountain reading's implication) but is actually maintaining atrophied capabilities through sustained performance. The contraction is real; the permanence is theater. This reading does NOT resolve the sibling readings' contradictions — it simply occupies the ground between them. The contraction reading treats the permanent removal of total war as a structural fact; the dropping reading treats reachability as fixed and probability as variable; this reading treats both reachability and probability as technology-dependent contingencies. The mandatrophy persists at the kernel level because all three readings are observationally consistent with current data — decades of nuclear deterrence stability could be explained by permanent contraction, successful deterrence (dropping), or contingent equilibrium (this reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_impedance_persistence,
    'Will current technological impedance to total-war reachability persist at sufficient levels to maintain deterrence stability, or are destabilizing technologies (hypersonics, AI targeting, autonomous systems, quantum cryptanalysis) closing the reachability window faster than policy countermeasures can establish?',
    'Multi-decade tracking of: (1) technological capability development timelines for destabilizing systems; (2) efficacy of arms control, non-proliferation, and export restrictions in slowing deployment; (3) strategic doctrine evolution in response to emerging threats. Comparison of technology development rates vs. policy adaptation rates.',
    'If impedance persists: current piton classification may be stable (deterrence-by-difficulty remains functional). If impedance erodes faster than policy can offset: piton degrades to snare within 5-15 years (destabilizing tech investors extract via capability expansion; populations bear uncompensated risk). If impedance erodes but policy adaptation succeeds: scaffold classification becomes more accurate (temporary constraint with active sunset mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_impedance_persistence, empirical, 'Whether technological impedance to reachability persists or erodes faster than policy response').

omega_variable(
    reading_boundary_specificity,
    'Is ''total war reachability'' a precise structural claim about weapons effects and doctrine, or a contested abstraction that different readers (military strategists, arms control advocates, technology developers, civilians) operationalize differently?',
    'Cross-community interviews and doctrine analysis: how do military planners, arms control specialists, and technology developers define ''reachable''? Do they converge on a testable threshold (e.g., ''capability to destroy X% of Y population in scenario Z'') or does the term remain essentially contested?',
    'If precise and convergent: reachability boundary is a material constraint subject to technological evolution. If contested: the ''piton'' classification may collapse — the reading is fighting over language rather than describing a structural feature. The kernel contest becomes semantic rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_specificity, conceptual, 'Whether ''total war reachability'' is a precise structural threshold or a contested abstraction').

omega_variable(
    deterrence_functional_dependency,
    'Does current deterrence stability depend fundamentally on technological difficulty (reachability contraction), or does it depend on political commitment, institutional lock-in, and actor expectations regardless of underlying technology?',
    'Counterfactual analysis: If technology suddenly restored reachability (hypothetically), would deterrence collapse, or would political relationships, treaty structures, and mutual vulnerability hold deterrence stable despite expanded technical options?',
    'If tech-dependent: reading is correct — contraction is structural and reversible; piton classification reflects atrophied contingency. If commitment-dependent: reachability status is secondary to political relationships; piton classification is misframed — the constraint is not the technology but the fragile political consensus, which is rope or tangled_rope, not piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_functional_dependency, conceptual, 'Whether deterrence stability is fundamentally technology-dependent or politically maintained').

omega_variable(
    sibling_reading_empirical_adjudication,
    'What empirical observations would distinguish THIS reading (contingent reachability: technology-dependent piton) from the contraction reading (reachability left the feasible set permanently) and the dropping reading (reachability persists but probability dropped)?',
    'Prospective data collection: (1) Rate of destabilizing technology capability development; (2) failure of non-proliferation/arms control regimes; (3) doctrinal shift in major powers toward total-war scenarios; (4) intelligence assessments of reachability windows re-opening. If multiple readings remain empirically consistent indefinitely, the kernel contest is not decidable empirically.',
    'If contingent reading is empirically distinguishable: the reading''s foundational axiom (reachability_contingent_on_technology) is testable. If all three readings remain observationally consistent: the kernel is fundamentally contested and no reading forecloses another — all coexist indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_adjudication, empirical, 'Whether this reading is empirically distinguishable from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twr_cont_tr_t0, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(twr_cont_tr_t5, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 5, 0.67).
narrative_ontology:measurement(twr_cont_tr_t10, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 10, 0.76).

% Extraction over time
narrative_ontology:measurement(twr_cont_be_t0, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(twr_cont_be_t5, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 5, 0.23).
narrative_ontology:measurement(twr_cont_be_t10, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 10, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(twr_cont_su_t0, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(twr_cont_su_t5, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(twr_cont_su_t10, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, hypersonic_delivery_destabilization).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, autonomous_targeting_systems_reachability).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three structurally distinct readings with different ε values and type classifications. This constraint (contingent_reachability_reading) treats reachability as technology-dependent piton (ε≈0.28). The contraction_reading treats reachability contraction as irreversible structural change (ε→0, mountain). The dropping_reading treats reachability as fixed and probability as dropped (ε determined by deterrence doctrine, rope). Each reading is a separate constraint story reflecting different causal theories about what is constraining strategic space. Readers interested in the kernel contest should consult all three stories and the omegas documenting the empirical differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
