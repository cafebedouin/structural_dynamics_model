% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Persistence via Coordination Value (Lapsed Alternatives Reading)
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   QWERTY persistence represents one reading of a contested kernel: does the
 *   standard persist because coordination value is genuine and overwhelming
 *   (this reading, Rope classification), or because incumbent actors have
 *   engineered barriers to alternatives (sibling
 *   incumbent_preservation_reading, mixed or Snare classification)? This
 *   story instantiates the lapsed_alternatives_reading: QWERTY persists
 *   because it solves a real, multi-party coordination problem at global
 *   scale, and alternatives fail not from conspiracy or extraction, but from
 *   their inability to overcome the critical mass threshold required to
 *   establish a competing network effect. Extractiveness is low (0.18)
 *   because the constraint produces net benefit for all parties:
 *   manufacturers coordinate on a single design, software developers assume a
 *   single input standard, training systems concentrate on one layout, and
 *   users benefit from universal hardware interoperability and knowledge
 *   transfer. The theater ratio is minimal (0.25) because the constraint does
 *   what it claims — it coordinates keyboard input effectively. Switching
 *   costs are real but they are coordination costs, not extractive overhead.
 *   The measured increase in extractiveness from 0.08 to 0.18 over 40 years
 *   reflects not the emergence of extraction but the gradual ossification of
 *   the standard as mobile computing, international adoption, and path
 *   dependence deepen the coordination lock. This reading claims that QWERTY
 *   would persist even without incumbent advantage because the coordination
 *   value is intrinsic to how large-scale technical standards work.
 *
 * KEY AGENTS:
 *   - Keyboard Manufacturers (Powerful/Mobile): Benefit from coordination on QWERTY; could theoretically switch layouts but exit is motivated only by coordinating with competitors to shift the standard collectively
 *   - Software Ecosystem (Powerful/Mobile): Develops assuming QWERTY; coordination reduces fragmentation; switching requires rewriting across millions of applications
 *   - End Users (Moderate/Constrained): Benefit from ubiquitous training, cheap hardware, transferable skills; learning alternatives is costly but possible
 *   - Alternative Layout Innovators (Moderate/Mobile): Invent Dvorak (1936), Colemak (2006), other improvements; fail to achieve critical mass despite measurable ergonomic gains; fail not from suppression but from coordination threshold
 *   - Training Institutions (Institutional/Arbitrage): Teach QWERTY because it is universal; coordinate with education systems globally; switching would require retraining entire pedagogical infrastructure
 *   - Analytical Observer (Analytical/Analytical): Sees coordination problem at scale; recognizes that critical mass thresholds are structural features of network effects, not evidence of extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Persistence via Coordination Value (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'a69fde66-92b1-463e-93e4-6eff237b7138').
narrative_ontology:cs_kernel_codification('a69fde66-92b1-463e-93e4-6eff237b7138', implicit).
narrative_ontology:cs_authority_grounding('a69fde66-92b1-463e-93e4-6eff237b7138', practice).
narrative_ontology:cs_interpretation_layer_present('a69fde66-92b1-463e-93e4-6eff237b7138').
narrative_ontology:cs_reading_relation('a69fde66-92b1-463e-93e4-6eff237b7138', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('a69fde66-92b1-463e-93e4-6eff237b7138', foundational, coordination_value_sufficient_for_persistence).
narrative_ontology:cs_axiom_status(coordination_value_sufficient_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('a69fde66-92b1-463e-93e4-6eff237b7138', coordination_value_sufficient_for_persistence, empirically_contingent).
narrative_ontology:cs_axiom('a69fde66-92b1-463e-93e4-6eff237b7138', foundational, critical_mass_threshold_is_structural).
narrative_ontology:cs_axiom_status(critical_mass_threshold_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('a69fde66-92b1-463e-93e4-6eff237b7138', critical_mass_threshold_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('a69fde66-92b1-463e-93e4-6eff237b7138', coordination_consensus_equilibrium).
narrative_ontology:cs_drift_state('a69fde66-92b1-463e-93e4-6eff237b7138', post_personal_computing_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a69fde66-92b1-463e-93e4-6eff237b7138', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KEYBOARD MANUFACTURER (ROPE) — Adopts QWERTY because coordination on a single standard reduces training costs and interoperability friction. The constraint solves a genuine collective action problem: if manufacturers fragment across incompatible layouts, each design is niche, training becomes expensive, and network effects disappear. Coordination produces net benefit. Exit is mobile — could theoretically switch layouts, but the coordination logic locks them in via market size, not force. Experiences constraint as legitimate cooperation.
constraint_indexing:constraint_classification(qwerty_persistence__lapsed_alternatives_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: SOFTWARE ECOSYSTEM (ROPE) — Develops applications (operating systems, text editors, games) assuming QWERTY layout. The constraint is coordination: all developers benefit from assuming a single, predictable keyboard input standard. If developers had to support 10 incompatible layouts, each application becomes fragmented. Exit is mobile but costly — rewriting keyboard handling for multiple layouts is possible but reduces resources for other features. Net benefit from coordination.
constraint_indexing:constraint_classification(qwerty_persistence__lapsed_alternatives_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: END USERS (ROPE) — Learn QWERTY because it is ubiquitous and because learning multiple layouts is costly. The constraint solves their coordination problem: a universal layout means skills transfer across devices, training is concentrated on one system, and keyboard hardware is cheap and interchangeable. Exit is constrained — learning Dvorak is possible but requires months of practice, carries social friction (unusual on shared computers), and provides no economic benefit without ecosystem shift. Benefits from coordination despite apparent inefficiency.
constraint_indexing:constraint_classification(qwerty_persistence__lapsed_alternatives_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE LAYOUT INNOVATOR (ROPE) — Invents improved layouts (Dvorak 1936, Colemak 2006) to solve typing ergonomics. From the innovator's perspective, the constraint is still rope — the innovator needs the existing standard to exist in order to measure improvement against it. The switching cost is not imposed by Dvorak enemies; it is a genuine coordination problem: nobody adopts Dvorak unless others do, so network effects trap the innovation below critical mass. Exit is mobile — Dvorak can be designed and published — but fails at coordination threshold.
constraint_indexing:constraint_classification(qwerty_persistence__lapsed_alternatives_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE / CIVILIZATIONAL) — QWERTY persists because it solves a genuine multi-party coordination problem at scale. The standard coordinates keyboardmakers, software developers, training institutions, and users across billions of devices globally. The extractiveness is low because the constraint produces net benefit for all participants — switching costs are real, but they are the cost of coordination, not the cost of extraction. Theater is minimal: the constraint does what it claims (coordinates input). This reading sees QWERTY as a case study in how coordination values dominate the path.
constraint_indexing:constraint_classification(qwerty_persistence__lapsed_alternatives_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint produces net benefit for all participants because it solves a genuine multi-party coordination problem. Keyboard makers save on design complexity when they can converge on one layout. Software developers save on conditional logic and input handling when they assume one keyboard standard. Training systems are efficient when they teach a single layout globally. Users benefit from universal interoperability — skills transfer across devices, hardware is cheap and standardized, and ecosystem support is concentrated. The low extractiveness reflects that the coordination value is real and broadly distributed. There is no hidden extraction flowing to one party; instead, all parties benefit from reduced friction. The measured increase from 0.08 to 0.18 reflects the constraint deepening over time as path dependence accumulates, not the emergence of extraction. Suppression (0.12): Low. The constraint operates through positive incentives (coordination benefits) rather than coercion or suppression of alternatives. Alternative layouts exist, are freely available, and are technically superior in measurable ways (Dvorak reduces finger travel distance by ~35%, reduces error rate for trained typists by ~5-10%). The reason alternatives fail is not that they are suppressed — they are published, marketed, and available — but that they fail to overcome the critical mass threshold. A single typist switching to Dvorak faces real costs (months of retraining, incompatibility with standard keyboards, social friction on shared computers) while gaining no benefit without widespread adoption. This is not suppression; it is a coordination threshold. Theater ratio (0.25): Low. The constraint's performative content is minimal. QWERTY does what it claims: it coordinates keyboard input. There is no theater hiding the constraint's function. The small theater ratio reflects minor performative elements (keyboard manufacturers may claim QWERTY is 'ergonomically optimized' when it is merely old, training systems may emphasize 'QWERTY mastery' when they are really just teaching the universal standard), but these are peripheral to the constraint's core function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is minimal — all perspectives classify as Rope because the constraint is genuinely coordinative from every angle. The analytical observer sees the same structure as the participants. The beneficiary (manufacturers, developers) see coordination and net benefit. The intermediate parties (users) see coordination with some constraint but overall benefit. The alternative innovator sees coordination as the barrier, not extraction — the innovator needs the standard to exist and needs critical mass to shift, which is a coordination threshold, not suppression. This uniformity is diagnostic: when all perspectives converge on Rope with low extractiveness and no beneficiary/victim split, the constraint is coordination-pure.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint declares no beneficiaries or victims because the coordination benefit is genuinely symmetric across parties. Each party bears switching costs but also reaps coordination benefits. The symmetry is what distinguishes this reading from the incumbent_preservation reading, which would declare beneficiaries (incumbent manufacturers) and victims (alternative innovators, potential switchers). The derivation chain produces d = 0.50 for all parties (symmetric cost-benefit) without explicit override, resulting in f(d) ≈ 0.65 and low effective extractiveness chi even before scope modulation. This symmetric profile is the reading's core claim: QWERTY is not a Snare where one party extracts from another, it is a Rope where coordination value is broadly distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates why mandatrophy resolution is necessary for classification clarity. Without the committer-frame analysis, QWERTY could be classified as either Rope (coordination story) or Snare/Tangled Rope (incumbent preservation story) depending on observer position and framing. The mandatrophy is resolved by declaring which reading is instantiated and populating the CS structure to route the ambiguity to omegas. The lapsed_alternatives_reading resolves the mandatrophy by asserting that coordination value is the mechanism, not incumbent extraction, and by specifying the empirical test that would confirm or refute this claim (do alternatives achieve adoption when switching costs fall?). The sibling incumbent_preservation_reading would resolve it differently (incumbent mechanisms prevent adoption regardless of costs/benefits). The engine cannot choose between them — the committer-axis ambiguity is genuine — but the framework can now make explicit which reading each constraint story embodies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_determinism,
    'Is the critical mass threshold that defeats alternatives a deterministic property of coordination costs, or is it contingent on historical accident (which layout first achieved scale)?',
    'Counterfactual analysis: if Dvorak had been patented 20 years earlier and the keyboard industry was younger, would it have achieved critical mass? Historical analysis of other standards (e.g., metric system adoption, vehicle drive sides) to test whether critical-mass-first always wins regardless of superiority.',
    'If deterministic: QWERTY persistence is a consequence of the mathematical structure of coordination games, not of extractive mechanisms. If contingent: alternative layouts could have won if history had ordered events differently, and the reading shifts toward luck-and-contingency framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_determinism, empirical, 'Whether critical mass threshold is structural or historical-contingent').

omega_variable(
    coordination_versus_lock_in_boundary,
    'At what point does a coordination standard transition from ''coordination tool'' (benefits all parties) to ''lock-in trap'' (extraction mechanism)? Does QWERTY ever cross this boundary?',
    'Measure switching costs over time: did keyboard switching cost decrease (allowing alternatives to emerge) as technology evolved? Did any alternative layout achieve adoption >1% of installed base despite network effects? Did ergonomic innovation occur within QWERTY (split keyboards, columnar layouts on QWERTY) instead of switching layouts entirely?',
    'If switching costs remained prohibitively high: QWERTY is pure coordination, this reading is correct. If switching costs fell but no alternative emerged despite reduced costs: coordination mechanism is stronger than economic incentive, reading is confirmed. If alternatives DID emerge whenever switching costs fell enough: the constraint is not coordination-only, it is mixed or extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_versus_lock_in_boundary, empirical, 'Boundary between coordination and lock-in').

omega_variable(
    kernel_reading_ambiguity,
    'Is QWERTY persistence best explained by the coordination value of standards (this reading) or by mechanisms that preserve incumbent advantage (sibling incumbent_preservation_reading)? What empirical pattern would distinguish these readings?',
    'Compare QWERTY to other standards where coordination succeeded without apparent incumbent advantage (metric system, SI units, ISO shipping containers). Compare QWERTY to standards where switching costs existed but alternatives emerged (VHS vs Betamax, AC vs DC power, gasoline vs other fuels). If standards succeed purely by coordination value, alternatives should emerge when they offer sufficient advantage to overcome switching costs. The incumbent_preservation reading predicts that alternatives fail even with superior utility. This reading predicts that alternatives fail only when switching costs exceed their utility advantage.',
    'If switching-cost comparison supports this reading: QWERTY is Rope (coordination without extraction). If alternatives fail despite superior utility even at zero switching cost: incumbent_preservation reading (Snare or Tangled Rope) is correct. The empirical arbiter is whether alternatives ever achieve significant adoption when their advantage exceeds the measured switching cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Committer-axis ambiguity: coordination vs incumbent preservation').

omega_variable(
    network_effects_versus_ergonomics_tradeoff,
    'Do network effects from QWERTY coordination provide genuine ergonomic value to end users, or do they merely make QWERTY the only accessible option?',
    'Controlled experiment: measure typing speed and error rates on QWERTY vs Dvorak for users with matched training investment. If Dvorak outperforms after equal training, the benefit of QWERTY coordination does not offset its ergonomic cost — extraction may be present (network effects prevent access to objectively superior layouts). If QWERTY matches Dvorak in controlled settings, the coordination benefit (ubiquity, training availability, hardware cost) genuinely compensates for layout suboptimality.',
    'If Dvorak outperforms: QWERTY persistence may involve extraction (coordination value artificially sustains inferior technology). If QWERTY matches or exceeds Dvorak in utility: this reading is strongly supported — coordination value alone explains persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_versus_ergonomics_tradeoff, empirical, 'Whether network effects genuinely compensate for layout ergonomics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lapsed_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(qwerty_lapsed_tr_t20, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(qwerty_lapsed_tr_t40, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(qwerty_lapsed_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(qwerty_lapsed_be_t20, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(qwerty_lapsed_be_t40, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence is modeled as two constraint stories sharing the same observable (global QWERTY dominance) but differing in mechanism and reading. The lapsed_alternatives_reading frames QWERTY as a coordination success where alternatives fail at critical mass thresholds (Rope, low extractiveness). The incumbent_preservation_reading would frame QWERTY as a mechanism protecting incumbent advantage against superior alternatives (Snare or Tangled Rope, high extractiveness). These are not two measurements of the same constraint; they are two structurally distinct claims about what makes QWERTY persist. The omegas (kernel_reading_ambiguity) specify how empirical evidence could distinguish them. Network linking enables the engine to flag that a single natural-language claim (QWERTY persistence) decomposes into competing constraint stories with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
