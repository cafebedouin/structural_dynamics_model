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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Standard (Lapsed Alternatives Reading)
 *   domain: technology/standards/path_dependence
 *
 * SUMMARY:
 *   QWERTY keyboard layout persists as the global default not because
 *   incumbents actively defend it against superior alternatives, but because
 *   the coordination structure creates a critical-mass threshold: switching
 *   requires simultaneous adoption across users, devices, software, and
 *   training infrastructure. Individual alternatives (Dvorak, Colemak) offer
 *   measurable efficiency gains but fail to reach the mass adoption needed to
 *   yield individual benefit to any particular adopter. This reading frames
 *   QWERTY as a pure coordination constraint — a solved problem that makes
 *   further optimization locally suboptimal. The sibling reading
 *   (incumbent_preservation_reading) frames it as rent defense — a standard
 *   kept in place by beneficiary action to protect capital. This constraint
 *   instantiates the lapsed_alternatives reading: the persistence mechanism
 *   is coordination structure (critical-mass barrier), not beneficiary
 *   defense.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers: Primary coordinating actor — produce hardware to the established standard, benefit from unified market
 *   - software_developers: Secondary coordinating actor — assume QWERTY in input handling, avoid fragmentation overhead
 *   - end_users: Distributed coordinating actors — learn and embody QWERTY, pay individual switching cost if alternatives are adopted
 *   - alternative_layout_advocates: Excluded — would advocate for Dvorak/Colemak if critical mass could form, but cannot organize that transition
 *   - historical_innovators: Powerless observer — Dvorak, Colemak, and other proposals exist as technical innovations but never achieved critical-mass adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.31).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Standard (Lapsed Alternatives Reading)").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology/standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, '68e3822b-f86c-464c-92a9-02eac81dae9c').
narrative_ontology:cs_kernel_codification('68e3822b-f86c-464c-92a9-02eac81dae9c', distributed).
narrative_ontology:cs_authority_grounding('68e3822b-f86c-464c-92a9-02eac81dae9c', distributed).
narrative_ontology:cs_reading_relation('68e3822b-f86c-464c-92a9-02eac81dae9c', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('68e3822b-f86c-464c-92a9-02eac81dae9c', foundational, coordination_structure_determines_persistence).
narrative_ontology:cs_axiom_status(coordination_structure_determines_persistence, holdable).
narrative_ontology:cs_axiom_grounding('68e3822b-f86c-464c-92a9-02eac81dae9c', coordination_structure_determines_persistence, instrumental).
narrative_ontology:cs_axiom('68e3822b-f86c-464c-92a9-02eac81dae9c', foundational, critical_mass_barrier_is_structural_not_volitional).
narrative_ontology:cs_axiom_status(critical_mass_barrier_is_structural_not_volitional, holdable).
narrative_ontology:cs_axiom_grounding('68e3822b-f86c-464c-92a9-02eac81dae9c', critical_mass_barrier_is_structural_not_volitional, empirically_contingent).
narrative_ontology:cs_reference_frame('68e3822b-f86c-464c-92a9-02eac81dae9c', equilibrium_coordination_model).
narrative_ontology:cs_drift_state('68e3822b-f86c-464c-92a9-02eac81dae9c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('68e3822b-f86c-464c-92a9-02eac81dae9c', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, software_developers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufacture keyboards using the QWERTY standard. Benefit from a coordination equilibrium where keyboard layout is predictable across devices — they can produce a single design for global markets without variant SKUs. Retraining to alternative layouts would fragment their market and increase production complexity. Exit is technically possible but economically constrained by installed base lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_manufacturers, beneficiary,
    organized, generational, constrained, global).

% Build input handling into applications on the assumption that QWERTY is the deployed standard. Benefit from not having to detect, support, or optimize for multiple keyboard layouts. Switching support to Dvorak or Colemak would fragment their user testing, increase support burden, and fragment their market. Exit is technically feasible (layouts are software-configurable) but economically penalized by the installed base.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, software_developers, beneficiary,
    organized, biographical, constrained, global).

% Learn QWERTY typing skills, which become embodied motor memory. Benefit from universal compatibility — every keyboard, every device, every context uses the same layout. Switching to an alternative layout requires unlearning motor patterns, re-training on the new system, and carrying dual competence (QWERTY for other contexts, alternative for their own device) — a cognitive and temporal cost borne entirely by the individual. Most users remain QWERTY despite believing Dvorak or Colemak is more efficient, because the switching cost is personal and irrecoverable.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, end_users, beneficiary,
    moderate, biographical, identity_locked, global).

% Believe QWERTY is biomechanically suboptimal and promote Dvorak, Colemak, or other layouts as superior. They have adopted alternatives on their personal devices, but find no industrial coordination value in doing so — keyboards still ship QWERTY-dominant, software assumes QWERTY, their efficiency gains are personal and unshared. They are excluded from the decision-making process of device manufacturers and software platforms that would need to coordinate on an alternative to make switching worthwhile. Their objection would be: 'We would coordinate on a better standard if adoption could reach critical mass,' but that mass can never form under the current coordination structure.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_advocates, excluded,
    moderate, biographical, mobile, global).

% Dead or marginalized: the inventors of Dvorak (1936), Colemak (2006), and other proposals. Their innovations exist as technical artifacts and demonstrations but never achieved critical-mass adoption. From the standpoint of this reading, their failure is not due to active suppression by QWERTY incumbents, but to the mathematical structure of coordination: any alternative requires simultaneous adoption across many layers (device, OS, application, user training) to yield individual benefit; absent that simultaneity, switching incurs personal cost with no gained efficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, historical_layout_innovators, observer,
    powerless, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single keyboard layout as the default assumption across hardware, software, and user training. This solves the problem of fragmented input devices and incompatible typing systems: a user can move between machines without relearning layout, manufacturers can produce globally compatible hardware, software developers can handle input uniformly. The coordination function is the solved problem: 'One standard input model across all contexts,' not 'QWERTY specifically.'
% TRANSFER_FUNCTION: Moves the burden of layout knowledge from manufacturers and developers (who would otherwise support multiple standards) to end users, who accept QWERTY as the de facto layout. Users pay in embodied motor memory, training time, and forgone efficiency gains from alternative layouts. Manufacturers and developers gain simplified production and development. The transfer is not extraction in the sense of rent collection; it is a coordination cost distributed unequally: some actors benefit from simplification, others bear the cost of standardization.
% ABSENT_VOICES: Alternative-layout advocates and innovators have no seat at the table where device manufacturers and software platforms coordinate on defaults. They would object that the standard persists not because QWERTY is optimal, but because the critical-mass threshold for switching makes coordinating on an alternative prohibitively difficult. Their objection is structural, not operational: they are not suppressed; they are simply unable to organize the simultaneous adoption needed to break the equilibrium. Their voice would shift the debate from 'Is QWERTY good?' to 'Is the coordination structure that makes switching impossible itself extractive?'
% DISAPPEARANCE_RATIONALE: If QWERTY as a standard disappeared overnight and the coordination mechanism with it, alternative layouts would proliferate: users would learn multiple standards, manufacturers would produce region- or segment-specific variants, software would fragment on layout assumptions, and typing efficiency gains would accrue only within closed communities (Dvorak typists among themselves). The world would rearrange into multiple local equilibria, each justified by efficiency or cultural preference, none capable of achieving the cross-context universality QWERTY provided. The disappearance would be deeply disruptive despite QWERTY being 'merely coordination.'
% FOUNDING_PROBLEM: Early typewriter manufacturers and users faced fragmented input methods: different machines used different key arrangements, trained typists could not move between machines, and manufacturers could not assume a standard user skill set. A shared layout would solve interoperability and training efficiency.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (fragmented input standards, interoperability loss, training inefficiency) persists today in any context where manufacturers or software platforms deviate from QWERTY. Mobile devices that experimented with alternative layouts faced user resistance rooted in retraining cost. The problem is attested by the universal deployment of QWERTY across independent manufacturers and software ecosystems as a default assumption — they coordinate on it because fragmentation would cost them. Independent sources: HCI researchers confirm the switching-cost barrier; input-method specialists document the Dvorak/Colemak adoption ceiling at individual enthusiasts, never critical mass. No authoritative beneficiary of QWERTY (manufacturer, software platform) attests that they preserve the standard to protect capital; they attest that they use it because it is *already* the standard and breaking from it incurs unilateral cost.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured as the uncompensated cost borne by users from accepting a suboptimal layout: Dvorak research estimates 10-20% efficiency gain in typing speed and reduced finger movement. Measuring this against the alternative (users coordinating on Dvorak), the actual gap is 0.31 at interval end — modest, because both states are stable equilibria; QWERTY is extractive only relative to the theoretical optimum, not relative to anarchy. Suppression is very low (0.12) because the constraint operates through incentive structure (users cannot gain individually from switching), not active coercion. Theater is minimal (0.08) — the coordination function is genuine, and no substantial performative machinery maintains it. Accessibility_collapse is high (0.78) because once QWERTY reached critical mass in the 20th century, alternatives became effectively unavailable: a user who prefers Dvorak cannot access it without personal retraining and social isolation. Resistance is low (0.22) because QWERTY advocates do not oppose resistance; the constraint is not contested as unjust, only as suboptimal. The measurement trajectory shows a slight rise in extractiveness and theater in early intervals (20th-century typewriter era to computing transition), stabilizing by time 60 (modern era). The stabilization reflects that the critical-mass barrier is now stable — no coalition forms to switch, no external pressure builds.
 *
 * PERSPECTIVAL GAP:
 *   Manufacturers and software developers experience QWERTY as a beneficial coordination standard that simplifies their work; they perceive low extraction (beneficiary seat). Users experience it as a coordination requirement they accept; they perceive higher extraction (symmetric/constrained seat) because they bear the motor-learning and efficiency-loss costs. Alternative-layout advocates experience it as a locked-in standard they cannot escape collectively, even though individually they could adopt alternatives; they perceive extraction concentrated on themselves (victim seat, though they are excluded from decision-making). The engine computes each seat's type from the positional data: the beneficiary seats (manufacturers, developers) see a genuine rope; the constrained symmetric seats (users) see a rope with higher cost-to-benefit ratio; the excluded seats see a snare (locked out of the critical-mass transition that would make alternatives viable). This reading rejects the 'victim' framing — users are not victimized; they are coordinated on a locally rational standard.
 *
 * DIRECTIONALITY LOGIC:
 *   No classical beneficiaries or victims — all parties are coordinated on QWERTY by incentive structure, not by extraction. Manufacturers benefit from unified market but bear the cost of inventory and support if alternatives fragmented. Software developers benefit from uniform input handling but would benefit equally from any stable standard. Users benefit from universal compatibility but bear efficiency loss. Alternative advocates are excluded but not oppressed — they could adopt alternatives anytime; their exclusion is structural (the critical-mass requirement), not active. Directionality on all seats sits near 0.5 (symmetric): all bear coordination cost; none is clearly extractive relative to another. The measurement places extractiveness at 0.31, reflecting the gap between QWERTY's efficiency and theoretical optimum, distributed across the coordinating set. No directional override is needed: the structural data (no beneficiaries, no victims, balanced participation) derives symmetric directionality naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (decay of founding mandate) does not apply to this reading. The founding problem (fragmented input standards, interoperability loss) is live; the coordination structure solves it continuously. This is not a constraint whose function has atrophied. The alternative reading (incumbent_preservation) attributes any mandatrophy to active defense by incumbents — they might argue that QWERTY persists beyond its founding justification and is now defended for rent capture. This reading denies that: QWERTY persists because the founding problem is genuine, solved by coordination, and not cheaper to replace. If a superior standard could be adopted *simultaneously* across all layers, it would be. That simultaneity is the unsolved coordination problem, not the incumbent preservation problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_vs_incumbent_defense,
    'Did alternatives like Dvorak fail to reach adoption because of the mathematical structure of coordination (critical-mass barrier), or because manufacturers actively suppressed them?',
    'Historical analysis of adoption timelines and manufacturer decisions for alternative layouts; interview data from layout advocates in the 1950s-1980s about whether they faced active suppression or coordination failure; counterfactual: if manufacturers had coordinated to switch in 1960, would adoption have succeeded? Natural experiments from software-configurable layouts on modern devices: Colemak availability on phones/tablets, adoption rates, friction points.',
    'If critical-mass barrier is the mechanism, this reading (rope, lapsed alternatives) holds; if active suppression, the incumbent_preservation_reading (snare-flavored tangled rope) holds. The classification diverges materially on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_vs_incumbent_defense, empirical, 'Whether layout persistence is due to coordination structure or incumbent defense.').

omega_variable(
    efficiency_loss_as_extraction,
    'Should the uncompensated efficiency loss from QWERTY (estimated 10-20% in typing speed and ergonomics relative to alternatives) be counted as extraction by the standard, or as the unavoidable cost of coordination?',
    'Compare to other coordination standards: does a universal metric system impose ''extraction'' on populations that adopted local alternatives? Does a shared language impose extraction on speakers? The question is whether every coordination standard that is suboptimal to some theoretical optimum is extractive, or whether extraction requires *asymmetric* benefit (some gain what others lose). Empirical comparison: measure actual user benefit from QWERTY (universal compatibility, no retraining on device switches) against efficiency loss; if the cross-context compatibility benefit exceeds typing-speed loss, net extraction is negative (net coordination benefit).',
    'If efficiency loss is true extraction, extractiveness rises (0.45-0.55); if it is coordination cost, extractiveness stays low (0.20-0.35). This omega addresses whether the reading''s own metric is generous or conservative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_loss_as_extraction, conceptual, 'Whether suboptimality of a coordination standard counts as extraction or unavoidable coordination cost.').

omega_variable(
    identity_lock_vs_mobile_exit,
    'Is end-user attachment to QWERTY due to identity-lock (motor memory is existentially difficult to escape) or to rational mobile exit (switching is optionally available but economically penalized)?',
    'Behavioral data: users who consciously attempted to switch to Dvorak or Colemak, their reported barriers (motor memory interference, cognitive load, social isolation when using an alternative), and their abandonment rates. Neuro-imaging studies of motor retraining in skilled typists. Survey data on user awareness of alternatives and reasons for non-adoption (Do users know Dvorak exists? Do they know efficiency gains are real? How many tried switching and abandoned it?).',
    'If identity_lock dominates (motor memory is neurologically difficult to rewire), exit_options for end_users should be ''identity_locked'' (current) and suppression may be slightly higher (the lock is internal). If mobile exit dominates (switching is possible, just economically penalized), exit_options should be ''constrained'' and suppression is lower. The classification holds as rope either way, but the seat''s extraction level changes (identity_locked users experience higher χ than mobile-exit users).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_mobile_exit, empirical, 'Whether QWERTY persistence reflects neurological identity-lock or economic mobile-exit dynamics.').

omega_variable(
    coordinating_actor_vs_beneficiary,
    'Are manufacturers and software developers truly coordinating actors (equally bearing coordination cost and benefit), or latent beneficiaries who benefit from the standard''s persistence more than they would benefit from switching?',
    'Counterfactual cost analysis: if the industry switched to a fragmented standard (different regions using Dvorak, Colemak, etc.), what would be the net production and support cost to manufacturers and developers? Compare to the coordination benefit of unified QWERTY. Interview data from product managers and engineers about why they maintain QWERTY support and what would motivate a switch. Analysis of any proposals to switch: did companies explore it, and why did it fail (cost, user resistance, or supplier coordination)?',
    'If manufacturers/developers benefit more from staying than from switching, they move toward beneficiary status (d lower, extraction lower for them, higher for users). The reading shifts toward symmetric Rope with slight beneficiary tilt — still rope, but users carry more extraction weight. If the benefits are symmetric (all equally prefer QWERTY to fragmentation), the reading holds as pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinating_actor_vs_beneficiary, empirical, 'Whether coordinating manufacturers and developers are symmetric participants or latent beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(qwer_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence is decomposed into two structurally distinct constraints under the same kernel. The incumbent_preservation_reading frames QWERTY as a defended standard (snare or tangled rope, depending on whether defense is pure extraction or mixed with coordination); this reading frames it as a coordination standard whose alternatives lapsed due to critical-mass barrier (pure rope). The same empirical phenomenon (QWERTY persists globally) supports both readings depending on causal mechanism. The ε value differs: incumbent_preservation_reading scores high extractiveness (rent defense); lapsed_alternatives_reading scores moderate extractiveness (coordination cost + switching inefficiency). Beneficiary sets differ: incumbent_preservation identifies manufacturers/capitalists as concentrated beneficiaries; lapsed_alternatives identifies no concentrated beneficiary, only distributed coordination. The two readings are linked: if empirical investigation confirms active suppression (incumbent_preservation true), this reading's classification would shift; if investigation confirms critical-mass failure is the mechanism (lapsed_alternatives true), the sibling reading dissolves as a false positive. Both stories are live pending resolution of the omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
