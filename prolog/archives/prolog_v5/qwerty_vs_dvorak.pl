% ============================================================================
% CONSTRAINT STORY: qwerty_vs_dvorak
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_vs_dvorak, []).

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
 *   constraint_id: qwerty_vs_dvorak
 *   human_readable: QWERTY vs. Dvorak Keyboard Lock-in
 *   domain: technological/product_standardization
 *
 * SUMMARY:
 *   QWERTY lock-in is a canonical example of technological path dependence
 *   and network effects. The QWERTY layout was designed for mechanical
 *   typewriters to prevent key jamming by spreading commonly-used letters
 *   across the keyboard — it was never optimized for typing speed or
 *   ergonomics. Alternative layouts like Dvorak were designed with efficiency
 *   and ergonomics in mind but failed to achieve widespread adoption despite
 *   clear performance advantages. This constraint exhibits Tangled Rope
 *   properties: QWERTY provides genuine coordination benefits
 *   (interoperability, unified training, software compatibility) that justify
 *   some enforcement, but simultaneously extracts from users who would
 *   benefit from switching by imposing switching costs (relearning time,
 *   social friction, software incompatibility, lack of network density). The
 *   constraint has evolved over 70 years from a weak coordination mechanism
 *   (typewriter era, theater_ratio=0.35) to an increasingly theatrical
 *   lock-in mechanism (digital era, theater_ratio=0.65), where the
 *   enforcement is largely performed rather than functionally necessary —
 *   modern on-screen keyboards and digital remapping could support multiple
 *   layouts with minimal cost, yet QWERTY persists through institutional
 *   inertia.
 *
 * KEY AGENTS:
 *   - Ergonomic Injury Victims: Primary victims (powerless/trapped) — bear extraction costs as repetitive strain injuries accumulate over years, cannot exit without major life disruption
 *   - Speed Optimization Seekers: Secondary victims (moderate/constrained) — would benefit from Dvorak's superior ergonomics/speed but face constrained exit due to network effects and collaboration friction
 *   - Keyboard Manufacturers: Primary beneficiaries (institutional/arbitrage) — benefit from standardization that enables interoperable supply chains and consistent design; have arbitrage exit but choose to maintain QWERTY
 *   - Typing Education System: Secondary beneficiary (institutional/arbitrage) — benefits from unified curriculum and assessment; maintains QWERTY through pedagogical inertia rather than functional necessity
 *   - Legacy Software Ecosystem: Institutional actor (institutional/arbitrage) — QWERTY assumptions embedded in decades of code, driver software, accessibility features; maintains constraint through institutional inertia (piton classification)
 *   - Custom Hardware Experimenters: Marginal actors (powerful/mobile) — can locally exit QWERTY through custom keyboards but still encounter theater in public machines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent path dependence as immutable law of coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_vs_dvorak, 0.38).
domain_priors:suppression_score(qwerty_vs_dvorak, 0.62).
domain_priors:theater_ratio(qwerty_vs_dvorak, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_vs_dvorak, extractiveness, 0.38).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_vs_dvorak, tangled_rope).
narrative_ontology:human_readable(qwerty_vs_dvorak, "QWERTY vs. Dvorak Keyboard Lock-in").
narrative_ontology:topic_domain(qwerty_vs_dvorak, "technological/product_standardization").

domain_priors:requires_active_enforcement(qwerty_vs_dvorak).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, qwerty_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, legacy_software_developers).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, typing_pedagogy_institutions).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, ergonomic_injury_sufferers).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, typing_speed_optimization_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ERGONOMIC INJURY VICTIM (SNARE) — Users with repetitive strain injuries cannot escape QWERTY without bearing massive switching costs: relearning an entire motor skill set during peak work productivity years. The constraint extracts from this agent with full force — trapped in a layout known to produce suboptimal finger motion patterns, unable to exit without career disruption.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPEED OPTIMIZATION SEEKER (TANGLED ROPE) — Typists who invest time learning Dvorak experience coordination benefits (faster typing speed, lower finger motion) but bear significant costs: software incompatibility, social pressure, teaching overhead, and the risk that colleagues cannot use their machine. Constrained exit — they can switch, but costs are substantial and the network effect works against them.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KEYBOARD MANUFACTURERS (ROPE) — For manufacturers, QWERTY is a coordination mechanism: every keyboard shipped uses the same layout, enabling interoperability and reducing production complexity. Manufacturers benefit from the standard with low enforcement costs — the network effect does the work. Arbitrage exit: they could switch to Dvorak production, but maintaining QWERTY ensures market compatibility and sales volume.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TYPING EDUCATION (ROPE) — Schools and typing instructors benefit from QWERTY standardization — one curriculum, one set of teaching resources, one assessment standard across institutions. The constraint is coordination: unified training reduces educational overhead. Exit option is arbitrage — they could shift to Dvorak, but doing so would require rewriting curricula, retraining instructors, and coordinating with other schools. The network effect is the primary enforcement mechanism.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY SOFTWARE (PITON) — Operating systems, software applications, and hardware firmware have accumulated decades of QWERTY assumptions baked into code, keyboard drivers, and accessibility tools. The constraint persists through institutional inertia: QWERTY is no longer functionally optimal for most use cases (on-screen keyboards, voice input, gesture control), yet the software ecosystem maintains it through historical contingency. Theater ratio (0.65) reflects that much of the QWERTY enforcement is now performative — the original efficiency justification has long since degraded. The constraint persists because replacing it would require coordinated rewrites across millions of software projects.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CUSTOM HARDWARE ENTHUSIAST (PITON) — Hobbyists and makers can build or buy custom keyboards with Dvorak layouts for their personal machines. They have high mobility and can exit QWERTY locally without major costs. However, they still encounter QWERTY's theater in public machines, shared workspaces, and when collaborating with others. The constraint degrades for this agent because they have mobile exit, but it persists through performative social coordination — everyone else expects QWERTY, so even a mobile agent faces friction.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED FRAME (MOUNTAIN) — From a civilizational perspective, one might argue that QWERTY is an immutable feature of human-computer interaction: 'Any standardized keyboard layout will face network effects that make alternatives impossible to displace.' This naturalizes the lock-in as a law of coordination. However, the structural data reveals this as a false summit: QWERTY's dominance derives from contingent historical factors (mechanical typewriter jam prevention, not typing efficiency) and active enforcement through software/education systems, not from irreducible physical or logical limits. The mountain classification dissolves under scrutiny.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_vs_dvorak_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_vs_dvorak, TR),
    TR >= 0.70.

:- end_tests(qwerty_vs_dvorak_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint imposes real costs on typists seeking to optimize performance (relearning time, productivity loss during transition) and on injury victims (inability to escape a layout that exacerbates RSI). However, the extraction is not severe because: (1) alternatives exist and are technically accessible; (2) the constraint is enforced primarily through network effects and institutional inertia rather than coercion; (3) many users receive genuine coordination benefits from QWERTY's universality. Suppression (0.62): Moderate-high. Substantial barriers to Dvorak adoption include: retraining requirements (500+ hours to regain speed), software incompatibility on public machines, social coordination problems (shared workspaces expect QWERTY), lack of community density (fewer Dvorak users relative to network size required for tipping), and educational institutions' inertia in not teaching alternatives. But suppression is not total — technical solutions exist, some communities have successfully coordinated Dvorak adoption locally, and the barrier is fundamentally network-based rather than coercive. Theater ratio (0.65): Moderate-high, trending upward. Modern QWERTY enforcement is substantially performative: digital systems could support multiple layouts with minimal cost, on-screen keyboards eliminate the original mechanical jamming justification, and cloud settings sync could eliminate configuration friction. Yet QWERTY persists as a default because changing the default would require coordinated action across millions of software projects and organizations. The theater has increased over time as technological evolution has made the original mechanical justifications obsolete while institutional inertia has kept the constraint in place.
 *
 * PERSPECTIVAL GAP:
 *   Injury victims see pure extraction (Snare) — QWERTY prevents their exit and the constraint extracts health cost. Speed optimization seekers see hybrid coordination-extraction (Tangled Rope) — QWERTY provides real coordination value (universal software support) but simultaneously creates extraction (switching cost). Manufacturers see pure coordination (Rope) — QWERTY solves the legitimate problem of interoperable supply chains with minimal coercion. Educators see pure coordination (Rope) — unified curriculum reduces pedagogical overhead. Legacy software sees degraded constraint (Piton) — QWERTY persists through inertia and embedding rather than active function. Custom hardware makers see the constraint as locally avoided (local mobile perspective), but they still encounter its theater on shared machines. The analytical observer naturalizes the constraint as inevitable law (Mountain), but empirical analysis reveals this as false summit — the constraint's dominance derives from path dependence and institutional inertia, not from irreducible features of keyboard design or human typing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies substantially across agents. Injury victims experience high d (0.90+): they are trapped in a constraint that extracts from them with little exit option, yielding high f(d). Speed optimization seekers experience moderate d (0.55-0.65): they have constrained exit and would benefit from switching, but the network effect partially justifies QWERTY's persistence. Manufacturers experience low d (0.15-0.25): they benefit from the standard and have arbitrage exit, yielding low/negative f(d). The piton perspective for legacy software exhibits moderate d (0.50) with respect to contemporary developers: the constraint is no longer functionally necessary, yet embedded in code through historical contingency, creating a kind of 'forced beneficiary' relationship where continued QWERTY support is expected but provides diminishing value. The analytical false summit perspective attempts high d (0.85+) by naturalizing network effects as irreducible law, but structural analysis reveals this as false — the 'natural law' framing obscures the contingent historical origins and choices that maintain QWERTY.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution hinges on distinguishing genuine coordination value from artificial lock-in. QWERTY provides SOME real coordination benefits — interoperable supply chains, unified software support, universal literacy. These benefits justify SOME enforcement cost. However, the modern landscape (digital keyboards, software remapping, cloud sync) could provide these coordination benefits equally well with Dvorak or other layouts. The constraint's persistence beyond the point where coordination value exceeds switching cost indicates that extraction (not coordination) is now the dominant mechanism — manufacturers and institutions maintain QWERTY not because it is necessary for coordination but because the installed base inertia generates switching costs they benefit from. The Tangled Rope classification captures this: the constraint exhibits both genuine coordination function (universal standard reduces fragmentation) AND asymmetric extraction (maintaining the standard generates switching costs that benefit incumbents). The theater ratio rising from 0.35 to 0.65 indicates that the coordination component is degrading relative to the theatrical/extractive component. If theater_ratio continues to rise past 0.70, the classification would shift toward Piton (institutional inertia replacing functional coordination). If new technologies (AI-powered keyboard translation, universal layout remapping) reduce switching costs, the constraint could shift toward Scaffold with sunset clause (temporary coordination problem being solved by technical/social innovation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_contingency_vs_efficiency,
    'Is QWERTY''s dominance due to superior typing efficiency (which would justify lock-in as efficiency coordination) or to historical accident and path dependence (which would characterize it as extractive lock-in)?',
    'Empirical analysis of typing speed/ergonomic performance across layouts on modern equipment, controlling for user experience and practice hours. Historical record of why QWERTY was adopted on mechanical typewriters (mechanical jam prevention, not efficiency).',
    'If efficiency: constraint reclassifies toward Rope (legitimate coordination). If historical accident: constraint remains Tangled Rope/Snare — the efficiency frame is false naturalization of contingent power structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_vs_efficiency, empirical, 'Whether QWERTY dominance reflects efficiency or historical accident').

omega_variable(
    switching_cost_threshold,
    'At what individual productivity cost does switching to Dvorak become economically rational for a typist, and how many users exceed this threshold?',
    'Longitudinal study of typists switching to Dvorak: measure relearning curve, peak speed recovery timeline, productivity loss, career earnings impact. Compare to typists who remain on QWERTY.',
    'If switching cost is low (<5% career earnings loss): more agents should rationally exit QWERTY, suggesting suppression is artificially high. If high (>20%): suppression is structurally justified, supporting Snare classification for injury victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Economic switching cost threshold for keyboard layout change').

omega_variable(
    network_effect_plasticity,
    'Can modern digital tools (on-screen keyboard remapping, universal driver support, cloud-based settings sync) reduce network switching costs enough to enable Dvorak adoption tipping point?',
    'Deployment studies in organizations offering technical support for layout switching. Track adoption rates before/after providing centralized remapping infrastructure. Test whether tipping point is achievable at organizational scale.',
    'If plasticity is high: constraint could shift to Scaffold with sunset clause (technical solutions enable transition). If plasticity is low: constraint remains Tangled Rope — technical solutions exist but social coordination barriers persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_plasticity, empirical, 'Whether digital infrastructure can reduce network switching costs').

omega_variable(
    injury_prevalence_causation,
    'What fraction of repetitive strain injury in typists is attributable to QWERTY''s ergonomic properties versus to overall typing volume, work posture, and equipment design?',
    'Controlled study comparing injury rates across layouts with equivalent practice hours and equipment. Natural experiments from communities of Dvorak users tracking RSI prevalence.',
    'If QWERTY accounts for >60% of injury risk: victims'' snare classification is strengthened. If <30%: much of the injury cost is orthogonal to keyboard layout, suggesting suppression/extractiveness should be downward-revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(injury_prevalence_causation, empirical, 'Attribution of RSI to QWERTY layout versus other factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_vs_dvorak, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_tr_t0, qwerty_vs_dvorak, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qwerty_tr_t35, qwerty_vs_dvorak, theater_ratio, 35, 0.5).
narrative_ontology:measurement(qwerty_tr_t70, qwerty_vs_dvorak, theater_ratio, 70, 0.65).

% Extraction over time
narrative_ontology:measurement(qwerty_be_t0, qwerty_vs_dvorak, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(qwerty_be_t35, qwerty_vs_dvorak, base_extractiveness, 35, 0.3).
narrative_ontology:measurement(qwerty_be_t70, qwerty_vs_dvorak, base_extractiveness, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_vs_dvorak, information_standard).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, mechanical_typewriter_design).
narrative_ontology:affects_constraint(qwerty_vs_dvorak, software_keyboard_driver_standardization).

% DUAL FORMULATION NOTE:
% QWERTY lock-in decomposes into three distinct structural constraints: (1) mechanical typewriter jam prevention (historical origin, ~1870s); (2) typewriter -> computer transition period where QWERTY inertia prevented migration to ergonomically superior layouts (~1950-1990); (3) modern digital era where QWERTY is purely institutional inertia with negative ergonomic consequences (~1990-present). These are related but structurally distinct claims with different ε values. The story treats them as a unified Tangled Rope constraint exhibiting increasing theater over time (Goodhart drift: coordination function degrading, theatrical function increasing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_vs_dvorak, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
