% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Path Dependency Reading)
 *   domain: technology/political-economy
 *
 * SUMMARY:
 *   QWERTY keyboard layout persistence is analyzed here under the
 *   path-dependency reading: the constraint is understood as an inevitability
 *   given early historical accident and accumulated coordination costs,
 *   without identifiable strategic beneficiaries engineering the lock-in.
 *   Typewriter manufacturers adopted QWERTY for mechanical reasons (avoiding
 *   key-hammer collisions). Touch-typing training accumulated around this
 *   layout in the early 20th century. By mid-century, the trained global
 *   workforce and software platform defaults created a coordination
 *   equilibrium: each individual actor (user, manufacturer, trainer) had
 *   rational incentive to accept QWERTY given everyone else's acceptance, but
 *   the initial layout could have been different. The constraint represents
 *   diffuse externality (efficiency loss from suboptimal layout) rather than
 *   concentrated extraction. This reading competes with the strategic-lock-in
 *   reading, which attributes persistence to manufacturer cartels and
 *   training-institution standardization agreements.
 *
 * KEY AGENTS:
 *   - touch_typists_accumulated_skilled (power: organized, 1.9 billion global users in 2025, identity-locked exit)
 *   - keyboard_manufacturers (power: organized, market-responsive rather than standard-setting)
 *   - software_platform_providers (power: institutional, follow user demand for QWERTY defaults)
 *   - keyboard_training_industry (power: organized, competitive not cartelized, demand-responsive)
 *   - alternative_layout_proponents (power: powerless, excluded by network effects not suppression)
 *   - efficiency_externality_bearers (power: powerless, bear diffuse training and ergonomic costs)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.15).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Layout Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology/political-economy").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6').
narrative_ontology:cs_kernel_codification('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', distributed).
narrative_ontology:cs_authority_grounding('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', practice).
narrative_ontology:cs_reading_relation('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', foundational, path_dependency_determines_layout_persistence).
narrative_ontology:cs_axiom_status(path_dependency_determines_layout_persistence, holdable).
narrative_ontology:cs_axiom_grounding('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', path_dependency_determines_layout_persistence, empirically_contingent).
narrative_ontology:cs_axiom('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', foundational, no_strategic_beneficiary_extraction_on_qwerty).
narrative_ontology:cs_axiom_status(no_strategic_beneficiary_extraction_on_qwerty, holdable).
narrative_ontology:cs_axiom_grounding('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', no_strategic_beneficiary_extraction_on_qwerty, empirically_contingent).
narrative_ontology:cs_reference_frame('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', accident_driven_coordination_equilibrium).
narrative_ontology:cs_drift_state('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', contemporary_digital_era_2025, gap(stable, minor, false)).
narrative_ontology:cs_created_at('2b2e1c31-2b9a-4f1f-aeb9-3ef9df2e65f6', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, touch_typists_accumulated_skilled).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, keyboard_training_industry).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, office_workers_historical).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, efficiency_externality_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Billions of trained touch typists with QWERTY fluency. They benefit from a world where the standard layout they learned remains the global standard; learning an alternative layout would impose retraining costs they have no incentive to bear. Their identity as skilled workers is fused with QWERTY proficiency. Exit from QWERTY means sacrificing their trained skill advantage.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, touch_typists_accumulated_skilled, beneficiary,
    organized, biographical, identity_locked, global).

% Manufacture and distribute physical keyboards. Under path-dependency reading, they respond to user demand for QWERTY layout (the market is already locked in); they have no strategic interest in standardization beyond following dominant user preference. They could produce Dvorak layouts if demand justified it; absence of demand reflects user reluctance to bear switching costs, not manufacturer suppression.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Control OS and application defaults (Microsoft, Apple, Linux distributions). Implement QWERTY as the standard input method in all software. They follow market logic (users expect QWERTY) rather than enforce it strategically; they maintain compatibility with the inherited keyboard layout because the accumulated skilled user base demands it. They could change the default at software level but would face massive user rejection and market loss.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, software_platform_providers, agenda_setter,
    institutional, generational, mobile, global).

% Educational institutions and vocational programs teach touch typing. Under path-dependency reading, they teach QWERTY because it is the market standard students will encounter in the workforce; the training industry responds to labor-market demand, not creates it. They have no cartel power (many competing training providers) and no stake in preventing alternative layouts from emerging should demand shift.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_training_industry, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__path_dependency_reading, keyboard_training_industry, agenda_setter).

% Small communities advocating for Dvorak, Colemak, or other ergonomically optimized layouts. They are excluded from standardization decisions not by manufacturer suppression but by overwhelming network-effect preference for QWERTY among users. Their voice is absent from manufacturing decisions because manufacturers follow market demand (QWERTY), not because their position is suppressed. The constraint they face is the accumulated user base, not coordinated exclusion.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, alternative_layout_proponents, excluded,
    powerless, biographical, constrained, global).

% During the typewriter era (mid-20th century), office workers benefited from a single standardized layout: employers could hire from a large pool of trained typists, workers could find employment across firms with transferable skills. The standard reduced coordination friction. Under path-dependency reading, this benefit was contingent and not strategic—the layout could have been different given different early history, but once established, coordination benefited all participants.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, office_workers_historical, beneficiary,
    moderate, biographical, trapped, national).

% The broader public and future users who bear diffuse efficiency costs: slower typing speeds, higher training time, potential repetitive-strain injuries, and perpetual switching-cost burden that prevents discovery of superior layouts. These costs are real but are external to the coordination mechanism (path-dependency reading) rather than the result of beneficiary extraction. The externality is structural, not strategic.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, efficiency_externality_bearers, payer,
    powerless, biographical, trapped, global).

% Conduct research on typing efficiency, ergonomics, and alternative layouts. Under path-dependency reading, they document the efficiency loss but lack power to change user behavior because the switching cost is individually rational for each trained typist despite being collectively inefficient (prisoner's dilemma). Their analysis is commentary on the constraint, not evidence of strategic design.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, ergonomic_scientists, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single standardized keyboard layout globally, enabling training-cost amortization across the workforce: any typist can work at any organization with compatible skills. Solves the coordination problem of heterogeneous layouts fragmenting the labor market and multiplying training burden.
% TRANSFER_FUNCTION: Transfers the training-cost burden from individual workers to society: each worker must invest time learning QWERTY during education or early career. Manufacturers and platform providers capture no rent from this transfer—they follow market demand. The benefit (coordination) accrues to employers and future employers; the cost (training time) is borne by learners and users. The transfer is diffuse, not concentrated.
% ABSENT_VOICES: Users who never enter the skilled-typing workforce (voice-input users, gesture-interface users, future interface modalities) have no voice in the path-dependency mechanism—they are absent from the historical contingency that locked in QWERTY. Advocates for alternative layouts are structurally excluded not by suppression but by network-effect dominance of the incumbent standard.
% DISAPPEARANCE_RATIONALE: If QWERTY standardization vanished overnight—if keyboard layouts had forked into competing standards in the 1950s—the labor market would have fragmented around multiple typing standards; training would cost more; workers would need multi-layout fluency; coordination friction would increase. The world rearranges because coordination solves a real problem. However, the path-dependency reading claims the rearrangement would not be catastrophic: alternative layouts would have emerged and stabilized instead (the historical contingency matters, but layout choice itself is not unique).
% FOUNDING_PROBLEM: Early typewriter manufacturers built keyboards with key arrangements driven by mechanical constraints (avoiding hammer collisions at high speed). The QWERTY layout was one solution among many mechanical configurations. As touch-typing became a skill in the early 20th century, training infrastructure accumulated around the dominant layout. By mid-century, the accumulated trained workforce created a coordination lock-in: users had no individual incentive to switch because nobody else had, and manufacturers had no individual incentive to produce alternatives because the market demanded QWERTY.
% FOUNDING_PROBLEM_CORROBORATION: The original problem (mechanical key-collision avoidance on mechanical typewriters) is completely obsolete: digital keyboards have no mechanical constraint. Modern alternatives like Dvorak, Colemak, and voice input solve or bypass the founding problem entirely. Ergonomic scientists outside the keyboard industry and keyboard-training vendors affirm the founding problem is dead. Platform manufacturers could change defaults costlessly at the software level if coordination had shifted. The persistence of QWERTY post-solution is the phenomenon the path-dependency reading explains as accident, not strategy.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Under the path-dependency reading, extractiveness is low (0.15) because no identifiable party systemically collects rents—manufacturers produce QWERTY because users demand it, trainers teach it because employers hire for it, platforms implement it by default because users expect it. All actors are responding to incentives, not enforcing a cartel. Suppression is minimal (0.08) because QWERTY's persistence does not require active enforcement against resistance; it is sustained by accumulated coordination equilibrium and individual rational preference for the standard everyone else uses. Theater ratio is zero: there is no performative activity disguising the constraint's function. Accessibility collapse is very high (0.92) because once the trained workforce reaches critical scale (1950+), the switching cost for any individual user becomes prohibitive—the alternative is not accessible without bearing the training burden individually. Resistance is minimal (0.05) because resistance is not a meaningful category here: users do not resist QWERTY; they rationally accept it. The constraint is natural relative to path-dependent initialization, not arbitrary.
 *
 * PERSPECTIVAL GAP:
 *   The accumulated trained typist sits at low d (beneficiary end): they benefit from QWERTY remaining standard because their trained skill is valuable only in a QWERTY world. The alternative-layout proponent sits at high d (target end): they bear costs of market rejection and training inefficiency. Manufacturers and platforms sit near symmetric (d~0.5): they gain nothing from lock-in per se (they could produce alternatives) but also lose nothing by accepting the standard users demand. The engine should compute these divergent directionalites from the structural data: beneficiaries are the trained typist base (who lose if QWERTY vanished); no victims are identified (since extraction is diffuse and unintentional). Under path-dependency reading, this should classify as mountain from the agenda-setter and platform seats (they face no choice cost) and as constrained coordination from the payer seats (trained workers and alternative-layout advocates face switching cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Path-dependency reading declares no beneficiaries in the strategic sense—no actor engineered the lock-in to extract rents. However, the accumulated trained typist base is a de facto beneficiary (they benefit from QWERTY remaining standard) without being an extractor. This creates the unusual structural situation of a mountain that has beneficiaries—the FSM candidate case. The beneficiaries emerge from contingency (historical accident), not design. Victims are diffuse and external (the efficiency loss borne by all keyboard users, new learners, and future interface users who inherit the QWERTY burden). No agent is identified as systematically extracting from the constraint. Directionality should derive low d for the accumulated typist base (they benefit without paying) and moderate-to-high d for alternative-layout users (they face suppressed alternatives). The absence of a strategic beneficiary preventing the engine from computing a concentrated extraction vector is the crux of the path-dependency vs. lock-in readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical key-collision prevention) was critical in the 1870s–1950s. By 2000, the problem was completely dead: digital keyboards have no mechanical constraint, voice and gesture input obviate physical typing, and superior layouts are ergonomically superior. QWERTY persists despite the founding problem's death—the classic mandatrophy signature. The path-dependency reading explains this as natural consequence of accumulated coordination lock-in, not manufactured persistence. The strategic-lock-in reading argues the persistence is engineered by beneficiaries despite the founding problem's irrelevance. The readings diverge exactly on whether the mandatrophy is structural accident or strategic design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is QWERTY persistence the inevitable outcome of path-dependent coordination starting from historical accident, or the result of strategic lock-in engineered by manufacturers and training institutions?',
    'Historical analysis of manufacturer decision-making in the 1950s–1980s regarding alternative layout adoption; examination of training partnership contracts and standardization voting records; natural experiments from jurisdictions that attempted layout transitions (e.g., Turkish F-layout deployment); counterfactual reconstruction of what layout adoption landscape would have been absent early path-dependence.',
    'If path-dependent: this reading is structurally valid; the constraint is a mountain with negligible beneficiary capture. If strategic lock-in: the sibling reading (strategic_lock_in_reading) becomes dominant; manufacturers and training cartels become identifiable beneficiaries; constraint reclassifies as tangled_rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'The core contest between readings: accident-driven inevitability vs. manufactured persistence.').

omega_variable(
    efficiency_loss_quantification,
    'What is the actual efficiency loss (typing speed reduction, training cost, ergonomic harm) from QWERTY relative to optimized layouts like Dvorak under contemporary deployment conditions?',
    'Controlled studies comparing typing speed and error rates on QWERTY vs. Dvorak in matched user populations (separating learning-curve effects from steady-state performance); epidemiological studies on repetitive strain incidence; economic modeling of training cost amortized over lifespan.',
    'If efficiency loss is substantial and measurable: supports the reading''s assertion that QWERTY represents a diffuse externality rather than coordination success. If negligible or negative (QWERTY proves superior under contemporary conditions): the constraint is less extractive than claimed; may reframe as pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_loss_quantification, empirical, 'Quantifying the externality the path-dependency reading attributes to accident rather than strategy.').

omega_variable(
    switching_cost_mechanism,
    'What proportion of QWERTY''s persistence is attributable to accumulated training capital (sunk learning cost) vs. coordination network effects vs. infrastructure lock-in vs. rational preference discovery?',
    'Agent-based modeling of keyboard-choice dynamics under various cost-structure assumptions; longitudinal data on switching behavior when costs drop (e.g., touchscreen input, voice typing); surveys of actual switching resistance and stated reasons.',
    'High sunk-training: supports path-dependency reading; diffuse cost structure. High coordination effects: supports genuinely beneficial coordination claim. High infrastructure lock-in: evidence for strategic beneficiary argument (those who control infrastructure). Rational preference: reframes QWERTY as meeting actual user needs despite surface appearance of accident.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(switching_cost_mechanism, empirical, 'Decomposing the mechanisms that sustain QWERTY persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1870, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1870, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1870, 0.0).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1920, 0.0).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(qwer_tr_t2025, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2025, 0.0).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1870, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1870, 0.0).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1920, 0.08).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(qwer_be_t2025, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1870, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1870, 0.0).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1920, 0.02).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1980, 0.07).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(qwer_su_t2025, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2025, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence decomposes into two structurally distinct constraints under different readings of the kernel 'QWERTY inevitability.' Path-dependency reading: QWERTY as natural outcome of accident + coordination equilibrium, no beneficiary extraction, mountain classification. Strategic-lock-in reading: QWERTY as manufacturer-engineered cartel, substantial extraction, tangled_rope/snare classification. The readings share historical referent (QWERTY persists) and both are empirically contested; they diverge on structural genesis (accident vs. strategy) and therefore on ε, beneficiary/victim structure, and type. Each reading is an ε-invariant constraint with its own stakeholder map.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
