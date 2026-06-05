% ============================================================================
% CONSTRAINT STORY: governance__constitutional_government
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance__constitutional_government, []).

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
 *   constraint_id: governance__constitutional_government
 *   human_readable: Constitutional Government: Higher Law Binding Governors
 *   domain: political/legal
 *
 * SUMMARY:
 *   Constitutional government, as a constraint on governing authority,
 *   represents one specific reading of the contested governance kernel. This
 *   reading claims that legitimate authority is constituted and limited by a
 *   higher law binding on governors themselves — that the law that governs
 *   the governors survives any particular holder of power and constrains what
 *   they can do. This is ONE reading among five: autocratic rule (unmediated
 *   will), customary rule (immemorial practice), direct democracy (delegated
 *   authority to be minimized), and theocratic rule (divine delegation)
 *   represent alternative legitimation structures. The constitutional
 *   reading's structural signature is distinctive: suppression is redirected
 *   from the governed onto would-be absolute power-holders; the beneficiary
 *   is the governed as a class; extractiveness is intentionally low (designed
 *   to be low by constitutional structure), and the constraint is policed by
 *   institutional entrenchment. The constraint functions primarily as
 *   coordination (enabling stable, predictable governance) with modest
 *   overhead, and as a binding mechanism that prevents any single actor from
 *   exercising unlimited power — a rope that coordinates governors and
 *   governed while protecting both from tyranny.
 *
 * KEY AGENTS:
 *   - Governed Population: Primary beneficiary (moderate/constrained) — protected from arbitrary power; bears modest coordination costs (compliance with law)
 *   - Ruler / Particular Holder of Power: Secondary beneficiary, primary victim (institutional/constrained) — benefits from constitutional stability but constrained from exercising absolute power; cannot violate constitutional limits without cost
 *   - Would-Be Absolute Power-Holder / Absolutist Pretender: Victim (powerful/trapped) — constitutional entrenchment prevents achievement of unlimited authority; maximum extraction from this perspective
 *   - Competing Power Centers: Institutional actors (institutional/arbitrage) — courts, legislatures, military, competing offices, electoral opposition — who enforce constitutional limits against violation
 *   - Constitutional Ritual Maintainers: Institutional inertia (institutional/arbitrage) — actors who preserve constitutional form (elections, courts, written law) after substantive constraint may have eroded; piton perspective
 *   - Institutional Reform Coalition: Organized agents (organized/mobile) — civil society, legislative reformers, constitutional courts — who view constitutionalism as a transitional framework with sunset logic
 *   - Analytical Observer: Civilizational (analytical/analytical) — risks naturalizing the constitutional reading as a universal higher law necessary to governance, obscuring that it is one contested reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance__constitutional_government, 0.18).
domain_priors:suppression_score(governance__constitutional_government, 0.35).
domain_priors:theater_ratio(governance__constitutional_government, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance__constitutional_government, extractiveness, 0.18).
narrative_ontology:constraint_metric(governance__constitutional_government, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(governance__constitutional_government, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance__constitutional_government, rope).
narrative_ontology:human_readable(governance__constitutional_government, "Constitutional Government: Higher Law Binding Governors").
narrative_ontology:topic_domain(governance__constitutional_government, "political/legal").

domain_priors:requires_active_enforcement(governance__constitutional_government).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(governance__constitutional_government, 'b15cf787-2a64-43bb-a16e-5c51219153ac').
narrative_ontology:cs_kernel_codification('b15cf787-2a64-43bb-a16e-5c51219153ac', formalized).
narrative_ontology:cs_authority_grounding('b15cf787-2a64-43bb-a16e-5c51219153ac', lineage).
narrative_ontology:cs_interpretation_layer_present('b15cf787-2a64-43bb-a16e-5c51219153ac').
narrative_ontology:cs_reading_relation('b15cf787-2a64-43bb-a16e-5c51219153ac', governance__autocratic_rule, forecloses).
narrative_ontology:cs_reading_relation('b15cf787-2a64-43bb-a16e-5c51219153ac', governance__customary_rule, coexists_with).
narrative_ontology:cs_reading_relation('b15cf787-2a64-43bb-a16e-5c51219153ac', governance__direct_democracy, influences).
narrative_ontology:cs_reading_relation('b15cf787-2a64-43bb-a16e-5c51219153ac', governance__theocratic_rule, coexists_with).
narrative_ontology:cs_axiom('b15cf787-2a64-43bb-a16e-5c51219153ac', foundational, rulers_bound_by_higher_law).
narrative_ontology:cs_axiom_status(rulers_bound_by_higher_law, holdable).
narrative_ontology:cs_axiom_grounding('b15cf787-2a64-43bb-a16e-5c51219153ac', rulers_bound_by_higher_law, deontological).
narrative_ontology:cs_axiom('b15cf787-2a64-43bb-a16e-5c51219153ac', foundational, entrenchment_survives_succession).
narrative_ontology:cs_axiom_status(entrenchment_survives_succession, holdable).
narrative_ontology:cs_axiom_grounding('b15cf787-2a64-43bb-a16e-5c51219153ac', entrenchment_survives_succession, conventional).
narrative_ontology:cs_reference_frame('b15cf787-2a64-43bb-a16e-5c51219153ac', legal_order_constituted_by_higher_law).
narrative_ontology:cs_drift_state('b15cf787-2a64-43bb-a16e-5c51219153ac', contemporary_administrative_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b15cf787-2a64-43bb-a16e-5c51219153ac', '').
narrative_ontology:cs_kernel_id(governance__constitutional_government, governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance__constitutional_government, governed_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GOVERNED POPULATION (ROPE) — Benefits from constitutional constraints on ruler power. Modest coordination costs (compliance with law) yield significant protection: rulers cannot arbitrarily seize property, punish without process, or rewrite rules to advantage themselves. The constraint is coordination with low extraction — both rulers and ruled benefit from rule-of-law stability, though the distribution favors the governed.
constraint_indexing:constraint_classification(governance__constitutional_government, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: RULER / PARTICULAR HOLDER OF POWER (TANGLED ROPE) — Constrained by constitutional limits that bind them personally and survive their tenure. Genuine coordination benefit: constitutional stability enables long-term planning, commercial investment, and institutional development. But significant extraction cost: the ruler cannot exercise absolute power, seize property without legal process, or rewrite the constitution to suit their interests. The constraint both coordinates (enables stable governance) and extracts (prevents arbitrary power). Exit options are constrained — a ruler cannot simply abandon the constitution without triggering resistance.
constraint_indexing:constraint_classification(governance__constitutional_government, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ABSOLUTIST PRETENDER / WOULD-BE ABSOLUTE POWER-HOLDER (SNARE) — For an agent seeking unlimited power, constitutional constraints function as a snare: the constitution traps the would-be absolutist within rules they did not write and cannot unilaterally revise. The suppression is severe (constitutional entrenchment, competing power centers, judicial review, popular veto through elections or revolt) and the extraction is total (all absolute power is extracted, the pretender retains zero discretionary authority beyond the constitution's allocation). The exit from this perspective is structurally trapped — breaking the constitution risks losing legitimacy, triggering resistance, and collapsing the institutional order.
constraint_indexing:constraint_classification(governance__constitutional_government, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL RITUAL (PITON) — From a civilizational perspective, formal constitutionalism often persists through performative repetition after the real constraints have eroded. Rulers comply with constitutional form (elections, courts, written law) while subverting substantive limits (executive overreach, judicial capture, constitutional amendment, emergency powers). The theater ratio is high: the ritual of constitutional government continues while the actual constraint on power weakens. The piton perspective captures degraded constitutionalism — where the form survives through institutional inertia but the binding function has attenuated.
constraint_indexing:constraint_classification(governance__constitutional_government, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized actors (legislative reformers, constitutional courts, civil society) who view constitutional constraints as temporary frameworks designed to solve a specific institutional problem: preventing absolute power concentration during a transition period. This perspective sees the constitution as having a sunset logic built into its maturation: as norms mature and democratic institutions solidify, formal constitutional constraints become less necessary (moving toward spontaneous order, custom, or direct democratic participation). Low effective extraction because the coalition has agency and perceives an exit path through institutional evolution.
constraint_indexing:constraint_classification(governance__constitutional_government, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HIGHER LAW AS NATURAL NECESSITY (MOUNTAIN) — From a universal and civilizational perspective, constitutional government appears to embody an immutable logical truth: any durable governing system must have some law that binds the governors themselves, else power becomes arbitrary and self-consuming. This perspective sees constitutional constraint not as a contingent institutional design but as a natural law of political sustainability. However, the structural data contradicts this classification — the constraint is enforced through active institutional resistance (courts, legislatures, elections, revolt), not through logical necessity. The engine will identify this as a false summit, revealing that 'higher law is necessary' naturalizes a contested institutional reading.
constraint_indexing:constraint_classification(governance__constitutional_government, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance__constitutional_government_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(governance__constitutional_government, TR),
    TR >= 0.70.

:- end_tests(governance__constitutional_government_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low by design. The constitutional reading explicitly aims to minimize arbitrary power extraction. The ruler cannot arbitrarily seize property, rewrite law to their advantage, punish without due process, or rule by whim. The measured extractiveness reflects modest costs imposed by constitutional compliance and administration. The low value differentiates this reading from autocratic rule (where extractiveness would be 0.65+) and indicates that genuine coordination dominates over extraction. The measurement trajectory (0.12 → 0.15 → 0.18) shows slight rising trend, capturing the common empirical pattern of constitutional creep: over time, rulers gradually expand executive power, courts defer more, and emergency provisions become normalized — but the baseline remains low relative to alternatives. Suppression (0.35): Moderate. The governed face real barriers to complete exit (born into the jurisdiction, exit costs via emigration), but suppression is far lower than in autocratic systems. The constitution creates multiple suppression-reducing mechanisms (free exit via emigration, voice mechanisms through elections and petition, exit via peaceful change). However, suppression is not zero — the law does constrain behavior and non-compliance brings enforcement. The suppression is redirected toward would-be absolute rulers (they cannot exit the constitutional constraint without triggering institutional resistance), not toward the governed. Theater ratio (0.42): Moderate. Constitutional ritual includes genuine functional elements (courts do review executive action, legislatures do resist some ruler initiatives, elections do constrain tenure) but also performative elements (symbolic invocation of higher law, ceremonial constitutionalism, formal procedures that can be bypassed informally). The increasing trajectory (0.28 → 0.35 → 0.42) captures degradation over the interval — as rulers learn workarounds and entrenchment erodes, more of the constitutional activity becomes theatrical while maintaining form.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the very same institutional structure (higher law binding governors) appears as coordination (rope) from the governed's perspective, mixed coordination-extraction (tangled rope) from the ruler's perspective, pure extraction (snare) from the would-be absolutist's perspective, degraded ritual (piton) from the civilizational time horizon, and temporary solution (scaffold) from the institutional reformer's perspective. Most strikingly, the analytical observer risks classifying it as a natural law (mountain) — treating constitutional necessity as an immutable feature of durable governance — when the structural data shows it is an enforced institutional reading with competing alternatives. The perspectival gap reveals that 'higher law' is not a description of nature but a legitimacy claim that some institutional arrangements enforce and others contest. The false-summit detection mechanism is designed to flag exactly this pattern: when an observer declares something a universal law and the structural data shows organized institutional enforcement of one contested reading, the gap signals that naturalization is occurring.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional reading's directionality structure inverts the usual extraction flow: extraction runs from would-be absolute rulers (who cannot achieve unlimited power) toward the governed (who are protected from arbitrary rule). From the governed's perspective (powerless baseline, but constrained-exit beneficiary of the constraint), d is low (beneficiary status + constrained exit = d ≈ 0.25). From the ruler's perspective (institutional, constrained exit, mixed beneficiary-victim status), d is moderate (d ≈ 0.50 — benefits from constitutional stability but loses absolute power discretion). From the absolutist pretender's perspective (powerful, trapped by constitutional entrenchment), d is high (d ≈ 0.90 — all sought-after power is extracted by the constitutional structure). The directionality reversal — extraction flowing from powerful to powerless, not from powerless to powerful — is the reading's defining structural feature. It differentiates constitutional government from snare (where extraction flows from powerless to powerful) and from rope (where extraction is minimal and symmetric). The tangled rope classification for the ruler reflects that d ≈ 0.50 (moderate extraction) combined with genuine coordination function (stability, rule-of-law predictability) and active enforcement (constitutional courts, competing powers, electoral accountability).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by making the kernel context explicit. The constitutional reading claims that higher law binds governors — but 'higher law' is not a self-evident fact of nature. It is one institutionalized reading of the governance kernel, competing with autocratic, customary, direct-democratic, and theocratic readings. Each reading legitimates a different authority structure. The mandatrophy is resolved by recognizing that the classification (rope for the governed, tangled rope for the ruler, snare for the absolutist) follows from the reading's structural properties (entrenchment, competing power centers, legal limits) applied through the indexical tuple. There is no single 'correct' classification because the classification is relative to the observer's structural position and the reading's constraints. The false-summit perspective (mountain) reveals what the mandatrophy resolution obscures: that naturalizing the constitutional reading as a universal higher law erases the institutional work that enforces it and the alternative readings that contest it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitution_enforcement_mechanism,
    'What distinguishes a genuine higher law that binds governors from a merely declared constitution that rulers can ignore with impunity?',
    'Empirical observation of enforcement when rulers attempt constitutional violation: Do competing power centers (courts, legislatures, military, populace) actually resist? Are there costs to the violator? Historical analysis of constitutional breakdowns and their causal triggers.',
    'If enforcement is real: the constraint is a rope (genuine coordination with low extraction). If enforcement is absent or theatrical: the constraint is a piton (degraded ritual). If enforcement is selective (applied asymmetrically to protect rulers from challenge but not to limit them): the constraint is a snare (extraction mechanism, not higher law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitution_enforcement_mechanism, empirical, 'Whether constitutional constraints are enforced by competing power centers').

omega_variable(
    kernel_reading_contest,
    'Is constitutional government a universal binding principle, or one contested reading of the governance kernel among five equally legitimate alternatives?',
    'This omega IS the kernel context itself. The governance kernel admits five readings (autocratic, constitutional, customary, direct democratic, theocratic). The constitutional reading claims that ''higher law binds governors'' — but this claim forecloses autocratic absolute power, coexists with customary and democratic readings, and influences theocratic authority by subordinating divine delegation to legal constraints. The contest is not empirically resolvable; it is a structural feature of how governing authority can be legitimated.',
    'If constitutional government is universal natural law: mountain classification (analytical perspective confirmed). If it is one contested reading: rope or snare classification (constraint is enforced interpretation, not inherent necessity). The reading contest determines the baseline classification — all perspectives are computed relative to how the reading positions the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether constitutional government is one reading of a contested governance kernel').

omega_variable(
    suppression_target_redistribution,
    'Does the constitutional constraint genuinely redirect suppression from the governed onto would-be absolute rulers, or does it simply distribute suppression across all actors while claiming to constrain power?',
    'Comparative analysis of suppression profiles: Is suppression lower for the governed population under constitutionalism than under alternatives (autocracy, theocracy, unmediated direct democracy)? Does the constitution actually prevent the governed from being ruled arbitrarily, or merely channel arbitrariness through constitutional procedures?',
    'If suppression is genuinely redirected: the constraint is a rope with clear beneficiary-victim asymmetry (governed benefit, would-be absolute rulers lose). If suppression is redistributed but not reduced: the constraint is tangled rope or snare, and the ''higher law'' framing masks extraction. If suppression is lowest for the governed: the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_target_redistribution, empirical, 'Whether constitutional constraints reduce suppression for the governed').

omega_variable(
    entrenchment_mechanism_durability,
    'What mechanisms actually entrench the constitution against revision by a current ruler, and how durable are they?',
    'Analysis of entrenchment mechanisms: supermajority amendment requirements, judicial review, bicameral legislatures, separation of powers, federalism, popular ratification, cultural reverence for founding documents. Empirical tracking of constitutional endurance across different entrenchment architectures. Historical analysis of when entrenchment breaks.',
    'If entrenchment is durable: the constraint survives particular rulers (survives any particular holder of power) as claimed. If entrenchment is brittle: the constraint is vulnerable to consolidation (moving toward snare or degraded piton). If entrenchment requires continuous maintenance: the constraint is piton or scaffold (sustained by active institutional will, not passive durability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entrenchment_mechanism_durability, empirical, 'Durability of constitutional entrenchment mechanisms').

omega_variable(
    constitutional_legitimacy_grounding,
    'What grounds the legitimacy of ''higher law'' — from where does the constitution derive its authority to bind even the governor?',
    'Historical and textual analysis of constitutional preambles and founding documents. Identification of grounding claims: popular sovereignty (We the People), natural law, divine providence, historical continuity, expert judgment, or procedural legitimacy. Examination of which grounding is invoked to justify constitutional constraint, and whether different readings invoke different groundings.',
    'Different groundings justify different readings: popular-sovereignty framing enables direct-democratic and constitutional readings. Natural-law framing enables constitutional and theocratic readings. Historical-continuity framing enables customary and constitutional readings. The grounding choice shapes which readings are live and which are foreclosed within a framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_legitimacy_grounding, conceptual, 'Epistemic and normative grounding of constitutional authority').

omega_variable(
    false_summit_constitutional_naturalism,
    'Is the mountain perspective correct that higher law is a natural necessity of durable governance, or does this perspective naturalize a contested institutional reading?',
    'This is the diagnostic signal the framework exists to identify. The mountain perspective claims logical necessity; the rope, tangled rope, and snare perspectives demonstrate that identical institutions are enforced through political resistance and institutional design, not through logical compulsion. The gap between the mountain''s claimed universality and the multiplicity of other readings reveals that ''higher law necessity'' is a legitimacy narrative, not a natural law.',
    'If higher law is genuinely necessary: the mountain classification holds and the false-summit detector should not fire. If higher law is one contested reading: the false-summit detector fires, reclassifying as tangled rope (the constraint combines coordination benefits with asymmetric extraction protecting the governed while constraining absolute power). The reading contest is the trigger for FSM evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_constitutional_naturalism, conceptual, 'False-summit detection for naturalized constitutional theory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance__constitutional_government, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gov_const_tr_t0, governance__constitutional_government, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gov_const_tr_t25, governance__constitutional_government, theater_ratio, 25, 0.35).
narrative_ontology:measurement(gov_const_tr_t50, governance__constitutional_government, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(gov_const_be_t0, governance__constitutional_government, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(gov_const_be_t25, governance__constitutional_government, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(gov_const_be_t50, governance__constitutional_government, base_extractiveness, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance__constitutional_government, enforcement_mechanism).
narrative_ontology:affects_constraint(governance__constitutional_government, governance__autocratic_rule).
narrative_ontology:affects_constraint(governance__constitutional_government, governance__customary_rule).
narrative_ontology:affects_constraint(governance__constitutional_government, governance__direct_democracy).
narrative_ontology:affects_constraint(governance__constitutional_government, governance__theocratic_rule).

% DUAL FORMULATION NOTE:
% The governance kernel admits five readings. This story instantiates constitutional_government. Each reading has its own constraint_id, its own extraction profile, and its own measurement trajectory. They are linked via network.affects_constraints to indicate that each reading's classification depends on how the other readings are instantiated in the system. Changes to one reading (e.g., if theocratic_rule begins to claim entrenchment mechanisms that mimic constitutional constraints) create pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
