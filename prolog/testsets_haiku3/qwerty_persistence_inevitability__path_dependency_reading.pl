% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Keyboard Persistence as Path Dependency (Accident-Driven Reading)
 *   domain: technology/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'QWERTY persistence inevitability.' In the path-dependency reading,
 *   QWERTY keyboard layout persists not because any actor strategically
 *   benefits from or defends it, but because of accident-driven historical
 *   contingency combined with accumulating rational individual choices. The
 *   founding constraint (mechanical typewriter jamming) is genuinely dead
 *   with electric and digital keyboards, yet QWERTY endures because every
 *   typist has internalized it, every training system teaches it, and no
 *   individual typist or manufacturer has incentive to break the coordination
 *   equilibrium unilaterally. The constraint is thus a mountain: inevitable
 *   given initial conditions and the accumulated weight of human capital, not
 *   defended by strategic beneficiaries. This reading explicitly denies that
 *   keyboard manufacturers or training operators extract rents from QWERTY
 *   persistence — they would rationally switch to a superior layout if
 *   collective action could overcome the switching-cost barrier, but no such
 *   coordination exists. The efficiency loss is real but diffuse, unmeasured,
 *   and unappropriated by any named beneficiary.
 *
 * KEY AGENTS:
 *   - Keyboard manufacturers: rationally respond to installed base; observe but do not engineer QWERTY persistence.
 *   - Trained typists: bear identity-locked exit costs from accumulated QWERTY muscle memory; participate in but do not consciously defend the constraint.
 *   - Training systems and pedagogy: institutional infrastructure that amplifies QWERTY lock-in without strategic intent.
 *   - Alternative-layout proponents: excluded by coordination problem, not by suppression.
 *   - Technology researchers: analytical observers documenting path-dependent persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Persistence as Path Dependency (Accident-Driven Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology/economic_history/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '5104005e-7768-4e27-a0f1-a76b00cd79c0').
narrative_ontology:cs_kernel_codification('5104005e-7768-4e27-a0f1-a76b00cd79c0', implicit).
narrative_ontology:cs_authority_grounding('5104005e-7768-4e27-a0f1-a76b00cd79c0', distributed).
narrative_ontology:cs_reading_relation('5104005e-7768-4e27-a0f1-a76b00cd79c0', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('5104005e-7768-4e27-a0f1-a76b00cd79c0', foundational, qwerty_persistence_is_accident_driven).
narrative_ontology:cs_axiom_status(qwerty_persistence_is_accident_driven, holdable).
narrative_ontology:cs_axiom_grounding('5104005e-7768-4e27-a0f1-a76b00cd79c0', qwerty_persistence_is_accident_driven, empirically_contingent).
narrative_ontology:cs_axiom('5104005e-7768-4e27-a0f1-a76b00cd79c0', foundational, no_strategic_beneficiary_maintains_lock_in).
narrative_ontology:cs_axiom_status(no_strategic_beneficiary_maintains_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('5104005e-7768-4e27-a0f1-a76b00cd79c0', no_strategic_beneficiary_maintains_lock_in, empirically_contingent).
narrative_ontology:cs_created_at('5104005e-7768-4e27-a0f1-a76b00cd79c0', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Respond rationally to installed base of trained typists and accumulated QWERTY-specific capital (training systems, touch-typing pedagogy, muscle memory). They have no strategic interest in perpetuating QWERTY per se — they would adopt Dvorak or any superior layout immediately if switching costs evaporated, but the switching costs are real and external to their decision calculus. They observe and accommodate the installed base but do not engineer its persistence.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, observer,
    organized, biographical, constrained, global).

% Have internalized QWERTY muscle memory and trained reflexes over years of use. Switching to an objectively superior layout would require months of retraining and temporary productivity loss. The constraint operates through their accumulated embodied skill, not through external coercion — exit is identity-locked because the skill IS part of their professional identity. The constraint persists because each individual's rational calculation (keep the sunk investment) aggregates into a stable equilibrium.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, trained_typists, observer,
    powerless, biographical, identity_locked, global).

% Institutions and practices optimized for QWERTY keyboard instruction. Not an actor but a structural fact that embeds the layout in educational infrastructure. The pedagogical system responds to demand for QWERTY skills; it does not create the constraint but amplifies and stabilizes it through institutional inertia.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, training_systems_and_pedagogy, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(qwerty_persistence_inevitability__path_dependency_reading, training_systems_and_pedagogy).

% Recognize superior layouts (Dvorak, Colemak, etc.) exist but cannot overcome the coordination problem: the benefit of switching is collective but the cost is individual. They lack the centralized power to mandate layout change or the scale to subsidize massive retraining. Their exclusion is not strategic suppression but structural: no single actor can break the equilibrium and no coalition is available to coordinate the transition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_proponents, excluded,
    powerless, biographical, trapped, global).

% Observe and measure the constraint's persistence, document its path-dependent history (the Hammond typewriter adoption, subsequent lock-in), and analyze why superior alternatives never displace it. They take no action to defend or eliminate the constraint; they study why it is stable despite apparent inefficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, technology_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: QWERTY solves the initial problem of mechanical typewriter key design in the 1870s (minimizing mechanical interference from adjacent keys striking simultaneously), creating a lock-in around a specific keyboard layout. Once typists train on QWERTY, manufacturers rationally maintain it because retraining costs exceed the benefit of switching, even when technically superior layouts exist.
% TRANSFER_FUNCTION: The constraint transfers no wealth or status between parties. It distributes efficiency loss (loss of typing speed and ergonomic benefit from superior layouts) as a diffuse, unmeasured externality across all typists and manufacturers. There is no identifiable capturer of this loss; it simply dissipates as foregone productivity.
% ABSENT_VOICES: Hypothetical future generations who might have benefited from a superior layout were locked out by accident, not by anyone's deliberate choice to exclude them. Alternative-layout communities exist and voice objections to QWERTY's persistence, but they cannot coordinate a transition because the individual cost of switching exceeds the individual benefit. No voice is structurally suppressed; the constraint operates through dispersed rational choices, not through silencing.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared overnight and all keyboards reverted to a random layout, typists would immediately retrain themselves on whatever layout became the new standard. No one depends on QWERTY for livelihood; the constraint is path-dependent infrastructure, not a power relationship. The world rearranges because an alternative coordination equilibrium would form, but the specific arrangement (QWERTY vs. Dvorak) is indifferent to most outcomes — what matters is that everyone coordinates on the same layout, not which layout they choose.
% FOUNDING_PROBLEM: Early mechanical typewriters required a keyboard layout that minimized jamming when adjacent keys struck in rapid succession. QWERTY was designed (by Christopher Latham Sholes, 1873) to space out frequently-used letter pairs, reducing collision frequency and improving reliability of mechanical type-bars. The problem was real: early machines with alphabetical key ordering jammed constantly.
% FOUNDING_PROBLEM_CORROBORATION: Mechanical typewriter jamming ceased being a relevant problem with the shift to electric typewriters (1960s–1970s) and became entirely irrelevant with digital keyboards (1980s onward). Contemporary keyboard technology has zero mechanical constraint — any layout could be implemented without engineering penalty. Technology historians (David, Galler, and other independent researchers outside the keyboard industry) confirm the founding problem is obsolete and the persistence is pure path dependency, not ongoing engineering necessity.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_unchanged).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) and stable across the measurement interval (0 to 150 years): there is no evidence of increasing rent extraction or strategic capture over time. Suppression is near-zero (0.05–0.06): the constraint operates through rational individual choice and accumulated sunk cost, not through active coercion or exclusion of alternatives. Theater ratio is zero: there is no performative activity defending QWERTY; manufacturers and trainers simply continue the equilibrium because breaking it is harder than maintaining it. Accessibility collapse is very high (0.92): once a typist has trained for years on QWERTY, the alternative keyboards are effectively inaccessible through identity-locked exit — the sunk capital is real and large. Resistance is very low (0.08): while alternative-layout communities exist and advocate, they mount no systemic resistance because the constraint is not defended by an opponent (manufacturers would not resist a coordinated switch). The measurement series is flat because the constraint's structure does not change — it is static path dependency, not a dynamic extraction regime. The stability of low extractiveness and suppression across 150 years distinguishes this reading from strategic lock-in, where we would expect to see both metrics rise as manufacturers increasingly exploit their lock-in position.
 *
 * PERSPECTIVAL GAP:
 *   The strategic-lock-in reading would present keyboard manufacturers as powerful beneficiaries engineering the constraint for rent extraction, with a high directionality toward the target (typists). The path-dependency reading presents manufacturers as passive observers responding rationally to typist demand. The gap is not in the metrics but in the reading of agency: one reading sees intentional power, the other sees distributed rationality. Both readings produce the same observed outcome (QWERTY persists despite superior alternatives), but they disagree on WHY and WHETHER IT IS EXTRACTIVE. The engine's per-seat computation will differ between readings precisely because the beneficiary/victim declarations differ: strategic lock-in includes keyboard manufacturers as beneficiaries; path dependency excludes them, treating them as observers. This divergence is the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The authoring position is that this is a mountain: an inevitable structural feature of the technology-human-capital ecosystem, not a power relationship. No override is needed because the stakeholders have no meaningful directionality asymmetry — they are all coordinated by the same accident-driven equilibrium.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical typewriter jamming) is completely dead as of the 1960s–1970s shift to electric keyboards and entirely irrelevant with digital keyboards (1980s onward). The constraint persists long after its founding mandate is satisfied, which is the definition of mandatrophy. However, in the path-dependency reading, the constraint does NOT persist through institutional maintenance or performance — it persists through the autonomous weight of accumulated human capital and rational individual choices. Mandatrophy analysis would normally signal a piton (dead mandate, performed theater) or a snare (dead mandate, extracted rents). Neither applies here: the path-dependency reading denies both performance and extraction. What remains is the constraint's structural inevitability: given that millions of typists have trained on QWERTY and no coordinating authority can mandate a switch, the constraint persists as a solved coordination equilibrium, not as a defended institution. This is compatible with mountain classification because the constraint's persistence follows from initial conditions (accident, human capital), not from ongoing agency. The mandatrophy is present (mandate is dead) but the constraint is NOT a piton because there is no theater and no beneficiary maintaining it — it persists passively. This is a rare case where mandatrophy coexists with mountain-like inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_beneficiary,
    'Is QWERTY persistence driven entirely by accident and accumulated rational individual choices, or do keyboard manufacturers and training-system operators actively engineer its perpetuation to extract rents from switching costs?',
    'Historical analysis of industry communications, patent records, and coordination evidence. If manufacturers coordinated to prevent layout standardization or funded campaigns against alternative layouts, the constraint shifts from path dependency to strategic lock-in. If manufacturers compete within QWERTY but would adopt a superior layout if market demand shifted, the constraint remains accident-driven.',
    'Path dependency reading assigns the constraint to mountain type (inevitability without beneficiaries); strategic lock-in reading reclassifies it as tangled_rope or snare (manufacturers benefit, switching costs are actively defended). This is the core difference between the two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_beneficiary, empirical, 'Whether QWERTY persistence is accident-driven coordination equilibrium or manufactured lock-in.').

omega_variable(
    efficency_loss_diffuseness,
    'Is the efficiency loss from QWERTY persistence (foregone typing speed, ergonomic harm, learning time) genuinely diffuse and unmeasured, or are there identifiable groups bearing concentrated costs?',
    'Quantification of typing-speed loss, carpal-tunnel prevalence attributable to QWERTY, and learning-time costs across populations. If concentrated in occupational groups (data entry workers, journalists) facing training barriers, the constraint may have a victim set even under path-dependency framing.',
    'Diffuse loss supports mountain classification (natural-law-like efficiency externality); concentrated loss would weaken the mountain claim and require beneficiary/victim analysis even in the path-dependency reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficency_loss_diffuseness, empirical, 'Whether efficiency loss from QWERTY is truly diffuse or concentrated in identifiable populations.').

omega_variable(
    reading_committer_disagreement,
    'Can the path-dependency reading and the strategic-lock-in reading coexist as live positions held by different scholarly communities, or does one logically foreclose the other?',
    'Review of academic literature: if both readings are defended in peer-reviewed venues without contradiction being the standard objection, they coexist; if one reading''s advocates argue the other is logically indefensible, they foreclose.',
    'Coexistence supports the declared reading_relations value ''coexists_with''; foreclosure would require ''forecloses''. This determines whether the sibling readings occupy different institutional authorities or compete within the same authority structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_disagreement, conceptual, 'Structural relationship between path-dependency and strategic-lock-in readings of QWERTY persistence.').

omega_variable(
    identity_lock_stability_across_technology_shifts,
    'When keyboard technology shifts (mechanical → electric → digital), does the identity-lock of trained typists persist, weaken, or strengthen the QWERTY constraint?',
    'Observation of technology transition periods: if typists resist retraining during moments of technological change (e.g., mechanical-to-electric transition, computer adoption), identity-lock is persistent; if those moments offer windows for layout change, it is conditional on technology.',
    'Persistent identity-lock supports the mountain claim; conditional lock-in suggests the constraint is more fragile than path-dependency reading asserts. This affects the accessibility_collapse metric: how absolutely is the alternative truly closed off?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_stability_across_technology_shifts, empirical, 'Whether identity-lock around QWERTY skill is robust across technology shifts or vulnerable to change windows.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 25, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t25, observed).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 50, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t50, observed).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 75, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t75, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 150, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 25, 0.16).
narrative_ontology:measurement_basis(qwer_be_t25, observed).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(qwer_be_t50, observed).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement_basis(qwer_be_t75, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.15).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 150, 0.15).
narrative_ontology:measurement_basis(qwer_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t25, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement_basis(qwer_su_t25, observed).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement_basis(qwer_su_t50, observed).
narrative_ontology:measurement(qwer_su_t75, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement_basis(qwer_su_t75, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 100, 0.05).
narrative_ontology:measurement_basis(qwer_su_t100, observed).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 150, 0.05).
narrative_ontology:measurement_basis(qwer_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into two structurally distinct readings: (1) path_dependency_reading — QWERTY persists as accident-driven equilibrium without strategic beneficiaries (mountain type, low extraction); (2) strategic_lock_in_reading — QWERTY persists as manufacturer-engineered lock-in via training cartels (snare/tangled_rope type, high extraction). The readings share a referent (QWERTY's actual persistence) but differ in causal account (distributed rationality vs. coordinated rent-seeking) and beneficiary structure (none vs. keyboard manufacturers). Both are live readings in scholarly and industry discourse; neither forecloses the other within the respective authority structures (academic institutions support both as defensible interpretations). The core empirical questions — whether manufacturers coordinated against alternatives, whether switching-cost defense was strategic or passive — resolve the kernel toward one reading or the other. Each reading authors its own ε, beneficiary/victim set, and constraint type. They are linked here via network.affects_constraints because the strategic reading's existence (and its evidence base) creates downstream pressure on the path-dependency reading to either strengthen its empirical case for accident-driven persistence or yield to the strategic interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
