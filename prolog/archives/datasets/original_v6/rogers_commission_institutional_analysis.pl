% ============================================================================
% CONSTRAINT STORY: rogers_commission_institutional_analysis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_institutional_analysis, []).

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
 *   constraint_id: rogers_commission_institutional_analysis
 *   human_readable: The Silent Safety Silo (NASA Decision-Making Pre-Challenger)
 *   domain: political/technological
 *
 * SUMMARY:
 *   The Challenger disaster on January 28, 1986, was preceded by a
 *   institutional constraint that prevented critical technical data regarding
 *   O-ring erosion in cold temperatures from reaching top-level NASA
 *   management. The Rogers Commission investigation identified a 'silent
 *   safety silo' — a multi-layered organizational filtering mechanism that
 *   was not primarily a failure of individual judgment but a systematic
 *   structural feature of NASA's decision-making architecture. At Morton
 *   Thiokol, engineers possessed detailed evidence that the O-rings would
 *   fail in the 36-degree launch temperature. At NASA, schedule managers
 *   operated under sustained pressure to maintain launch cadence for
 *   political and budgetary reasons. Between these two groups sat an
 *   organizational hierarchy that progressively filtered technical concerns
 *   upward, with each layer summarizing away the dissenting data. The
 *   constraint operated through both active suppression (deliberate
 *   withholding of information at key decision points) and structural
 *   filtering (hierarchical compression of technical detail into
 *   management-friendly abstracts). The Rogers Commission revealed that this
 *   was not a one-time breakdown but an institutionalized pattern — the silo
 *   had existed for years, filtering other technical concerns, operating
 *   invisibly until the failure it enabled became catastrophic. The
 *   constraint exhibits tangled rope characteristics: it had a genuine
 *   coordination function (managing a complex organization with thousands of
 *   contractors and strict timelines), but this coordination was
 *   fundamentally corrupted by asymmetric extraction — the coordination
 *   mechanisms were weaponized to silence dissent and insulate
 *   decision-makers from information that contradicted their preferences.
 *
 * KEY AGENTS:
 *   - Astronaut Crew: Primary victim (powerless/trapped) — no exit from the decision system; dependent on safety data that never reaches them
 *   - Morton Thiokol Engineers: Secondary victim (moderate/trapped) — possess the critical data but cannot exit the organizational hierarchy that filters their warnings
 *   - NASA Mission Management: Primary beneficiary (powerful/constrained) — benefits from schedule certainty and insulation from dissenting data; maintains the filter
 *   - Political/Congressional Oversight: Tertiary beneficiary (institutional/arbitrage) — benefits from positive mission optics; has exit options but does not exercise oversight
 *   - Public Trust: Collective victim (organized/constrained) — benefits from space program achievements but bears extraction through catastrophic failure
 *   - Institutional Safety Theater: Institutional actor (institutional/arbitrage) — maintains the appearance of safety review while the silo disables its function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_institutional_analysis, 0.58).
domain_priors:suppression_score(rogers_commission_institutional_analysis, 0.72).
domain_priors:theater_ratio(rogers_commission_institutional_analysis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, extractiveness, 0.58).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rogers_commission_institutional_analysis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_institutional_analysis, tangled_rope).
narrative_ontology:human_readable(rogers_commission_institutional_analysis, "The Silent Safety Silo (NASA Decision-Making Pre-Challenger)").
narrative_ontology:topic_domain(rogers_commission_institutional_analysis, "political/technological").

domain_priors:requires_active_enforcement(rogers_commission_institutional_analysis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_institutional_analysis, nasa_schedule_managers).
narrative_ontology:constraint_beneficiary(rogers_commission_institutional_analysis, contractor_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, astronaut_safety).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, engineering_integrity).
narrative_ontology:constraint_victim(rogers_commission_institutional_analysis, public_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASTRONAUT CREW (SNARE) — No exit option from the constraint. Trapped within the decision system that filters critical safety data. Bears maximum extraction cost — their lives are at stake. The constraint operates entirely through suppression of information they depend on to make informed consent decisions.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MORTON THIOKOL ENGINEERS (SNARE) — Possess critical technical data (O-ring erosion evidence) but trapped within organizational hierarchy that filters upward communication. Cannot exit — their employment and professional standing depend on the same institution suppressing their warnings. High experienced extraction: they bear the moral cost of suppressed knowledge without authority to act.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: NASA MISSION MANAGEMENT (TANGLED ROPE) — Benefits from the constraint through schedule maintenance and political/budgetary optics. Faces constraints from launch pressure, but maintains exit options through information control. Experiences mixed structure: coordination function (managing complex launch schedule) overlaid with extraction (insulating decision-makers from dissenting data). Active enforcement required — deliberately maintaining communication filters.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL/CONGRESSIONAL OVERSIGHT (ROPE) — Primary beneficiary of schedule certainty and positive mission optics. Has arbitrage options: can allocate resources elsewhere, modify budget priorities, but chooses not to exercise oversight that would disrupt the coordination of launch cadence. Experiences the constraint as pure coordination — communicating the launch schedule enables congressional planning.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC TRUST (ORGANIZED/VICTIM) — Organized collective that benefits from space exploration achievements but bears catastrophic extraction when the silo fails. Constrained by lack of direct information access. The constraint maintains public enthusiasm through selective communication while gambling with public safety.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL SAFETY THEATER (PITON) — The formal safety review processes (Flight Readiness Review, Criticality Analysis) persist as performative ritual despite the information silo negating their function. Theater ratio (0.68) reflects that safety reviews occur but are structurally incapable of accessing the filtered data. The constraint maintains the appearance of safety review while disabling its actual mechanism.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SUNSET LOGIC) — From civilizational perspective, the information silo is a correctable institutional defect rather than a law of nature. Rogers Commission identified specific mechanisms (communication hierarchy, schedule pressure, organizational incentives) that created the silo. The constraint had a built-in sunset: once the failure mode was documented, the organizational structure could be reformed. The silo is degraded institutional design, not inherent to spaceflight.
constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_institutional_analysis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rogers_commission_institutional_analysis, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_institutional_analysis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rogers_commission_institutional_analysis, TR),
    TR >= 0.70.

:- end_tests(rogers_commission_institutional_analysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through information control, maintaining schedule predictability and decision-maker autonomy at the cost of suppressing safety data. The value reflects that this is not the most severe extraction (astronauts and engineers have some limited exit options through whistleblowing or resignation), but the extraction is substantial because the organizational hierarchy prevents normal upward communication. The trajectory from 0.32 to 0.58 reflects the progressive intensification of schedule pressure and the corresponding deepening of the filter. Suppression (0.72): High. The constraint operates primarily through coercion and elimination of alternatives. Engineers have no mechanism to escalate beyond immediate supervisors without severe career risk. Management has no institutional pathway to receive dissenting data. The organizational structure is specifically designed to suppress alternatives to the 'launch as scheduled' priority. Theater ratio (0.68): High-moderate. Safety review processes (Flight Readiness Review, Criticality Analysis) occur but function as theater — they operate on filtered information, making them structurally incapable of detecting the problems they were designed to catch. The silo disables the safety review mechanism while maintaining its appearance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces radically different classifications from different structural positions. The astronaut crew and engineers see a pure extraction mechanism (Snare) because they are trapped within a hierarchy that filters out their knowledge and concerns. NASA mission management sees this as a coordination problem (Tangled Rope) — they genuinely are solving the complex problem of managing a space program, but they have weaponized the organizational structure to filter information. Political oversight sees it as pure coordination (Rope) — schedule certainty enables congressional planning, and they benefit from the positive optics without perceiving the hidden extraction. The analytical observer sees a sunset structure (Scaffold) — the silo is a correctable institutional defect, not an inherent feature of spaceflight. The institutional safety theater (Piton) perspective recognizes that the formal review mechanisms persist despite being structurally disabled. The perspectival gap here is particularly stark because it reveals how the same organizational structure can be experienced as legitimate coordination by those at the top and as coercive suppression by those at the bottom.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain flows from the information silo. Engineers and astronauts have zero exit options within the constraint — they cannot access the decision-making process or communicate dissent without severe consequences. This produces d ≈ 0.95, mapping to high experienced extraction (f(d) ≈ 1.42). NASA management benefits from the silo through schedule certainty and insulation from dissent, and they maintain exit options through their control of the information flow. This produces d ≈ 0.25, mapping to low experienced extraction or coordination benefit (f(d) ≈ 0.25-0.40). The organizational structure is explicitly designed to create this directionality gap — the hierarchy filters upward but not downward, creating asymmetric information flow that extracts from those with knowledge and benefits those making decisions. Political oversight has arbitrage options (they could demand more information, exercise tighter oversight, adjust budget priorities) but chooses not to use them, producing d ≈ 0.10 and experience of pure coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids the false natural law trap through explicit institutional analysis. The Rogers Commission identified specific, correctable mechanisms: organizational hierarchy, communication filters, schedule pressure, contractual incentives, and safety review procedures that functioned as theater. This is not a law of nature — it is an institutional design choice that could be (and was) reformed. The post-Challenger organizational restructuring (Creation of the Office of Safety and Mission Assurance, direct engineering access to decision-makers, two-level safety review with independent authority, and explicit procedures for escalating technical concerns) demonstrates that the silo was not inherent to spaceflight but a specific institutional failure. The constraint transitions from Tangled Rope (before Challenger) to Scaffold (post-Challenger with sunset logic), then to increasingly effective Rope (as the reformed institutional structure matures). The mandatrophy is resolved by recognizing that 'schedule management vs. safety' is not an eternal trade-off but a contingent institutional arrangement that can be restructured to reduce extraction while preserving coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    schedule_pressure_threshold,
    'At what degree of schedule pressure does information filtering transition from unconscious organizational behavior to deliberate suppression?',
    'Analysis of internal NASA communications, meeting transcripts, and decision logs. Correlation between schedule intensity and data-filtering events.',
    'If threshold is low (minor pressure causes filtering): the constraint is largely structural, diffuse responsibility. If threshold is high (only extreme pressure causes filtering): responsibility concentrates on specific decision-makers who knowingly activated the filter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(schedule_pressure_threshold, empirical, 'Threshold at which schedule pressure triggers deliberate information suppression').

omega_variable(
    engineering_signal_clarity,
    'Were the O-ring erosion signals sufficiently unambiguous that they would survive upward communication intact, or were they inherently ambiguous enough that normal organizational summarization would obscure them?',
    'Technical reconstruction of erosion data quality, signal-to-noise ratio, and predictive confidence. Comparison with other engineering warnings that did survive the filter.',
    'If signals were clear: the silo is an intentional extraction mechanism. If inherently ambiguous: the silo is partly a coordination problem (summarization always loses data).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineering_signal_clarity, empirical, 'Clarity of O-ring erosion signals through organizational layers').

omega_variable(
    contractor_incentive_alignment,
    'To what extent did Thiokol''s contractual incentives (penalty for launch delays, reputational/financial dependence on NASA) actively create the silo versus merely exploit an existing institutional weakness?',
    'Contract analysis, deposition testimony, financial modeling of delay penalties. Comparison with other contractor relationships to identify whether this incentive structure was industry-standard or anomalous.',
    'If incentives actively created the silo: extractive snare (Thiokol was forced into silence). If they exploited existing silo: tangled rope structure where contractors benefited from existing hierarchical filtering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_incentive_alignment, empirical, 'Whether contractor incentives actively created or merely exploited the information silo').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_institutional_analysis, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_tr_t0, rogers_commission_institutional_analysis, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rogers_tr_t4, rogers_commission_institutional_analysis, theater_ratio, 4, 0.61).
narrative_ontology:measurement(rogers_tr_t8, rogers_commission_institutional_analysis, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(rogers_be_t0, rogers_commission_institutional_analysis, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rogers_be_t4, rogers_commission_institutional_analysis, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(rogers_be_t8, rogers_commission_institutional_analysis, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_institutional_analysis, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_institutional_analysis, organizational_hierarchy_filtering).
narrative_ontology:affects_constraint(rogers_commission_institutional_analysis, political_schedule_pressure).
narrative_ontology:affects_constraint(rogers_commission_institutional_analysis, safety_review_theater).

% DUAL FORMULATION NOTE:
% The Silent Safety Silo operates as a unified constraint but decomposes into three structurally distinct mechanisms: (1) organizational hierarchy that filters information upward, (2) political/budgetary pressure that creates schedule priority, and (3) formal safety review processes that function as theater. Each mechanism has its own extractiveness value and classification. The unified silo (ε=0.58, Tangled Rope) represents the integrated effect; the component mechanisms have lower individual extractiveness but sum to the observed suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_institutional_analysis, powerful, 0.25).
constraint_indexing:directionality_override(rogers_commission_institutional_analysis, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
