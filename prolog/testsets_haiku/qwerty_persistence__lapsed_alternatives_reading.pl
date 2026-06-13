% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Standard Persistence via Coordination Lock-In
 *   domain: technology/standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists globally as the dominant standard
 *   despite documented alternatives (Dvorak, Colemak, workman layouts) that
 *   offer ergonomic or productivity improvements. This constraint story
 *   instantiates ONE READING of that persistence: the lapsed_alternatives
 *   reading. Under this reading, QWERTY persists not because incumbents
 *   actively defend it against rivals, but because alternatives cannot
 *   achieve the critical mass of adoption required to justify the
 *   coordination cost of learning for new users. Every party (users,
 *   manufacturers, developers) bears symmetric coordination costs and
 *   receives symmetric coordination benefits. The constraint is pure rope:
 *   coordination solves a genuine network externality problem, no party
 *   extracts from another, and alternatives lapse not from active suppression
 *   but from the mathematics of critical mass. The sibling reading
 *   (incumbent_preservation) claims instead that incumbents actively maintain
 *   QWERTY's dominance to protect sunk capital in existing devices and
 *   training infrastructure, making the constraint more snare-like with
 *   identifiable beneficiaries and victims. The two readings have different
 *   epsilon values, different beneficiary structures, and different
 *   structural implications. This story author the lapsed_alternatives
 *   reading as a clean, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - typing_population: All users of keyboards globally; benefit from universal standard; bear coordination cost of learning one layout rather than many.
 *   - keyboard_manufacturers: Benefit from single-design optimization; bear coordination cost of producing one standard rather than customizing.
 *   - software_developers: Benefit from assuming one standard input; bear coordination cost of not supporting layout customization.
 *   - alternative_layout_inventors: Bear the cost of critical-mass failure; their innovations cannot achieve deployment because no population segment will pay the switching cost in isolation.
 *   - ergonomic_researchers: Bear the cost of standard lock-in; empirical improvements cannot translate into market adoption.
 *   - standardization_bodies: Observer seat; codify the standard but do not enforce adoption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.28).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Standard Persistence via Coordination Lock-In").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology/standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'c3ab0761-af35-4567-8b45-43935cd9ec5c').
narrative_ontology:cs_kernel_codification('c3ab0761-af35-4567-8b45-43935cd9ec5c', distributed).
narrative_ontology:cs_authority_grounding('c3ab0761-af35-4567-8b45-43935cd9ec5c', practice).
narrative_ontology:cs_reading_relation('c3ab0761-af35-4567-8b45-43935cd9ec5c', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('c3ab0761-af35-4567-8b45-43935cd9ec5c', foundational, coordination_lock_in_sufficient_for_persistence).
narrative_ontology:cs_axiom_status(coordination_lock_in_sufficient_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('c3ab0761-af35-4567-8b45-43935cd9ec5c', coordination_lock_in_sufficient_for_persistence, empirically_contingent).
narrative_ontology:cs_axiom('c3ab0761-af35-4567-8b45-43935cd9ec5c', foundational, critical_mass_threshold_is_limiting_factor).
narrative_ontology:cs_axiom_status(critical_mass_threshold_is_limiting_factor, holdable).
narrative_ontology:cs_axiom_grounding('c3ab0761-af35-4567-8b45-43935cd9ec5c', critical_mass_threshold_is_limiting_factor, empirically_contingent).
narrative_ontology:cs_reference_frame('c3ab0761-af35-4567-8b45-43935cd9ec5c', symmetric_coordination_equilibrium).
narrative_ontology:cs_drift_state('c3ab0761-af35-4567-8b45-43935cd9ec5c', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c3ab0761-af35-4567-8b45-43935cd9ec5c', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typing_population).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_developers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_inventors).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, ergonomic_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of keyboards benefit from a single dominant standard: they can sit at any keyboard globally and type fluently without relearning. The coordination solves a genuine network externality problem — fragmentation would impose per-device learning costs on every user. This benefit is real and non-extractive; it justifies the adoption of QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typing_population, beneficiary,
    organized, biographical, constrained, global).

% Manufacturers benefit from producing keyboards to a single standard: design, tooling, and supply chains optimize around one layout rather than supporting competing variants. Production cost per unit is lower and inventory complexity is eliminated. The coordination benefit is genuine.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Software developers benefit from assuming a single standard input layout. Applications do not need to support layout switching, customization, or layout-aware help systems. Development cost is lower and cross-platform compatibility is clearer. The coordination simplification is real.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_developers, beneficiary,
    institutional, generational, constrained, global).

% Inventors of alternative keyboard layouts (Dvorak, Colemak, workman layouts) bear the cost of standard entrenchment: their innovations cannot achieve adoption because they lack the critical mass to justify the coordination cost of relearning for even a small population. The existing user base is too large to move; new users find QWERTY everywhere and have no incentive to switch. They face an absolute coordination barrier, not active exclusion.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_inventors, payer,
    powerless, biographical, trapped, global).

% Researchers who demonstrate that alternative layouts reduce repetitive strain or increase typing speed cannot move those findings into practice because no critical mass of adopters will pay the switching cost. Their innovations accumulate in the literature but never reach the market. The coordination lock-in makes deployment of technical improvements impossible, even when those improvements are clearly beneficial.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, ergonomic_researchers, payer,
    moderate, generational, constrained, global).

% International standardization bodies (ISO, ANSI) recognize QWERTY as the global keyboard standard and codify it. They do not enforce its adoption — adoption is driven by coordination value — but the codification makes it durable and difficult to change through any formal process.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, standardization_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, globally recognized keyboard layout that solves the multi-sided network externality problem: users benefit from universal familiarity, manufacturers benefit from single-design optimization, and developers benefit from assuming one standard input. Every party bears coordination costs (learning, tooling, infrastructure), but these costs are distributed rather than extracted from any one party.
% TRANSFER_FUNCTION: QWERTY transfers nothing from one party to another; all parties bear symmetric coordination costs and receive symmetric coordination benefits. The constraint is not a transfer mechanism; it is a pure coordination device.
% ABSENT_VOICES: Future typists who have not yet learned to type are implicitly bound to QWERTY without voice in its selection. Potential discoverers of superior layouts are excluded from the conversation because their work is moot — no market will adopt what they discover. The dominant design forecloses the consideration of alternatives, but does not do so through active gatekeeping; it does so through coordination lock-in.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared overnight and no standard replaced it, manufacurers would fragment production across multiple designs, users would need to carry layout knowledge for every device they encounter, and software developers would need to support customizable input schemes — the coordination problem would return immediately. The world would reorganize around a replacement standard (likely still QWERTY, because it is already entrenched). The constraint is necessary; only its specific incumbent would be replaceable.
% FOUNDING_PROBLEM: Early typewriter designs had no standard: different machines used different mechanical key-to-type mappings, making it impossible for trained operators to switch between machines. Users had to learn each device individually; manufacturers could not leverage operator skill as a market differentiator; the market was fragmented and inefficient.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (fragmentation of typewriter layouts) ceased to exist around 1910 as QWERTY achieved dominance and the market consolidated. Modern testimony from typing researchers, ergonomists, and keyboard manufacturers confirms that the fragmentation problem is solved — no designer wants to fragmentize keyboards again. The original coordination problem is universally acknowledged as solved; the dispute is whether the incumbent standard is the only solution or whether alternatives could achieve the same coordination benefit if given a chance.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.28 at interval end) because the constraint imposes switching costs on any party that wishes to deviate (alternative adopters must learn a new layout), but these costs fall on the deviator, not on incumbents. The constraint extracts in the sense that it forecloses alternatives and locks in switching costs; it does not extract in the sense that incumbents collect rents. Suppression is very low (0.12) because no active enforcement machinery exists: alternatives are not banned, developers are not contractually restricted from supporting them, users are not prevented from learning them. The barrier is structural — the coordination problem itself — not coercive. Theater is minimal (0.05) because what little suppression exists is genuine coordination cost, not performative maintenance. Accessibility collapse is high (0.78) because once the standard is established, alternatives become structurally unavailable to any individual actor: a user cannot coordinate a layout switch in isolation, a manufacturer cannot unilaterally support alternatives without alienating its user base, developers cannot assume non-standard input. The constraint is self-maintaining through coordination physics, not through enforcement or deception. Resistance is low (0.18) because there is no unified actor to resist — each party benefits from the coordination and bears only the coordination cost, which is necessary and symmetric. The measurement series show extractiveness and suppression rising slightly from 1870 to 1920 (as QWERTY achieved dominance and the coordination value became concentrated in one standard) and then plateauing (as the constraint stabilized). The initial rise reflects the transition from fragmentation to standardization; the plateau reflects stable equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   All stakeholder seats should compute toward rope or near-rope from this reading. The typing population and manufacturers benefit and bear symmetric costs; developers benefit and bear symmetric costs; alternative inventors and ergonomic researchers bear asymmetric costs but are external to the beneficiary-victim structure (they are not part of the coordination problem; they are casualties of its solution). The absence of an identified beneficiary set that extracts from victims is the key structural difference from the incumbent_preservation reading. From the incumbent_preservation seat, keyboard manufacturers would be identified as beneficiaries (defending capital investments in QWERTY production) and alternative inventors as victims (actively excluded from markets). From the lapsed_alternatives seat, both alternative inventors and manufacturers bear symmetric costs relative to the coordination value. The engine will compute different d values for each reading depending on whether beneficiaries/victims are declared; this story declares none (beneficiaries field is empty), supporting the rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because this reading frames the constraint as symmetric coordination, not asymmetric extraction. All parties benefit equally from the coordination and bear coordination costs equally. Alternative inventors appear as payer stakeholders, but only because they cannot achieve critical mass, not because incumbents exclude them. If the incumbent_preservation reading were instantiated, manufacturers would appear as beneficiaries (defending investments) and alternative inventors as victims (excluded). The two readings produce different directionality structures from the same set of actors — that structural divergence is the kernel contest. Under this reading, all stakeholder d values should cluster toward symmetric (d ≈ 0.5) because everyone is coordinated and everyone pays the same coordination toll.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (when the mandate outlives its function) is a key diagnostic here. The founding problem (fragmentation of typewriter layouts) was genuine and was solved by QWERTY's dominance around 1910. The mandate to standardize remains in force today. The question is whether the mandate has outlived its function or whether the coordination function is still live. Under the lapsed_alternatives reading, the coordination function is STILL LIVE: removing QWERTY would re-create fragmentation immediately, so the mandate persists because the problem persists. Under the incumbent_preservation reading, the mandate would have become mandatrophy: the founding problem is solved, but the standard persists because incumbents defend capital investments, not because coordination is necessary. This reading's classification as rope (no mandatrophy) reflects the judgment that the coordination function remains real. The high accessibility_collapse (0.78) indicates that alternatives remain inaccessible not due to decay of the constraint's function but due to the stable operation of coordination lock-in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_incumbent_vs_coordination,
    'Does QWERTY persist because incumbents actively defend it against alternatives (incumbent_preservation_reading), or because alternatives cannot achieve the critical mass required for coordination value (lapsed_alternatives_reading)?',
    'Analysis of counterfactual: If active incumbent defense were removed (keyboard manufacturers allowed alternative layouts, software assumed layout flexibility, training decoupled from QWERTY), would alternative layouts achieve adoption? Historical case studies of standards transitions where defense was low but lock-in was high would provide evidence.',
    'If incumbent defense is the primary mechanism, the constraint should be reclassified toward snare/tangled_rope with an identifiable beneficiary set (incumbents) and victim set (alternative inventors). If coordination lock-in is primary, the rope classification and symmetric burden-bearing hold. The two readings produce structurally different constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_incumbent_vs_coordination, conceptual, 'Committer frame: the kernel contest between active incumbent defense and pure coordination lock-in. This reading instantiates the lapsed_alternatives frame; the sibling reading instantiates incumbent_preservation.').

omega_variable(
    critical_mass_threshold_ambiguity,
    'What is the actual critical mass threshold for alternative keyboard layouts to achieve self-sustaining adoption? Is it 5%, 15%, 30% of the user population?',
    'Natural experiment: regions or subpopulations that adopt alternative layouts at scale and measure persistence. Surveys of layout-switchers to measure switching-cost thresholds. Economic modeling of network effects at different adoption levels.',
    'If the threshold is low (5%), then alternatives have failed to cross it because of active incumbent suppression, not pure coordination physics. If the threshold is very high (25%+), then pure coordination lock-in explains the observed outcome. The measured threshold shapes whether the constraint is better read as rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_ambiguity, empirical, 'The switching-cost threshold below which coordination lock-in overwhelms adoption attempts. Determines whether alternatives are locked out by coordination physics or by active defense.').

omega_variable(
    ergonomic_benefit_magnitude,
    'Are the measured ergonomic and productivity improvements from alternative layouts (Dvorak, Colemak) large enough that users would willingly pay the switching cost if they believed alternatives were viable?',
    'Longitudinal studies of users who switched to alternative layouts: what switching cost did they accept, and did they report sustained satisfaction? RCT studies forcing new typists to learn alternative layouts and measuring productivity. Economic contingent-valuation surveys asking users their willingness-to-pay to avoid learning QWERTY.',
    'If improvements are marginal and switching costs are high, users rationally stay with QWERTY regardless of incumbent defense. If improvements are substantial and switching costs are acceptable, user preference would drive switching IF they believed a critical mass would follow — the coordination problem then becomes one of belief formation and expectation coordination, not physical lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_benefit_magnitude, empirical, 'Whether the coordination lock-in is driven by genuine indifference (QWERTY is good enough) or by rational expectations (alternatives would be better if others adopted, but others won''t, so no one will).').

omega_variable(
    digital_transition_opportunity,
    'Digital devices (computers, tablets, phones) decoupled from mechanical typewriter constraints and offered low-cost layout switching via software. Did digital environments lower the critical mass threshold for alternative adoption?',
    'Historical analysis of digital keyboard adoption from 1980s onward: did alternative layouts achieve higher adoption rates on computers (where switching cost dropped to software reconfiguration) than on typewriters? Survey data on why digital users did not adopt alternatives despite reduced switching costs.',
    'If digital transition lowered the threshold and alternatives still failed, the lock-in is not driven by mechanical constraints but by expectation coordination — the coordination problem persists even when technology made alternatives feasible. This would support the lapsed_alternatives reading: even at zero switching cost, the network externality alone sustains QWERTY.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_transition_opportunity, empirical, 'Whether the coordination lock-in is driven by switching-cost physics or by expectation dynamics. The digital transition is a natural experiment that reduced switching cost but did not remove the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 1870, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1870, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1870, 0.0).
narrative_ontology:measurement_basis(qwer_tr_t1870, projected).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1920, 0.02).
narrative_ontology:measurement_basis(qwer_tr_t1920, observed).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement_basis(qwer_tr_t1960, observed).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t1990, observed).
narrative_ontology:measurement(qwer_tr_t2010, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t2010, observed).
narrative_ontology:measurement(qwer_tr_t2025, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 2025, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1870, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1870, 0.15).
narrative_ontology:measurement_basis(qwer_be_t1870, projected).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1920, 0.22).
narrative_ontology:measurement_basis(qwer_be_t1920, observed).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement_basis(qwer_be_t1960, observed).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(qwer_be_t1990, observed).
narrative_ontology:measurement(qwer_be_t2010, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement_basis(qwer_be_t2010, observed).
narrative_ontology:measurement(qwer_be_t2025, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement_basis(qwer_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1870, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1870, 0.05).
narrative_ontology:measurement_basis(qwer_su_t1870, projected).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1920, 0.08).
narrative_ontology:measurement_basis(qwer_su_t1920, observed).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement_basis(qwer_su_t1960, observed).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 1990, 0.11).
narrative_ontology:measurement_basis(qwer_su_t1990, observed).
narrative_ontology:measurement(qwer_su_t2010, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement_basis(qwer_su_t2010, observed).
narrative_ontology:measurement(qwer_su_t2025, qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 2025, 0.12).
narrative_ontology:measurement_basis(qwer_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence kernel has at least two structurally distinct readings. The lapsed_alternatives_reading (this story) frames QWERTY persistence as pure coordination lock-in with symmetric burden-bearing and no identifiable extractors. The incumbent_preservation_reading frames it as active defense of incumbent capital with beneficiary (manufacturers) and victim (alternative inventors) sets. The two readings have different epsilon values: lapsed_alternatives (moderate extractiveness from coordination costs alone) versus incumbent_preservation (higher extractiveness from active suppression and rent defense). They affect each other: if empirical evidence supports the critical-mass hypothesis, the incumbent_preservation reading's foundational axiom (active defense necessary for persistence) becomes overridden; if evidence supports incumbent defense, the lapsed_alternatives reading's axiom (coordination physics sufficient) becomes overridden. Network link indicates these constraints are readings of the same kernel and should be analyzed together for comparative refutation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
