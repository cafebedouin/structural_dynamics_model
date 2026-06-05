% ============================================================================
% CONSTRAINT STORY: quantum_critical_point_superconductivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_critical_point_superconductivity, []).

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
 *   constraint_id: quantum_critical_point_superconductivity
 *   human_readable: Quantum Critical Point Superconductivity Mechanism and Verification
 *   domain: condensed_matter_physics/high_temperature_superconductivity
 *
 * SUMMARY:
 *   The quantum critical point (QCP) superconductivity constraint represents
 *   a foundational paradigmatic structure in condensed matter physics that
 *   has organized theoretical research, shaped funding allocation, and
 *   structured graduate training for approximately two decades (roughly
 *   2003-2023). The core claim — that superconducting pairing in many
 *   high-temperature superconductors and heavy-fermion materials is mediated
 *   by quantum critical fluctuations rather than by conventional phonon
 *   coupling or other mechanisms — has become sufficiently institutionalized
 *   that it now functions both as an active research hypothesis AND as an
 *   unstated background assumption. The constraint exhibits characteristics
 *   of a Tangled Rope: it provides genuine coordination (organizing a complex
 *   theoretical landscape around a unified framework, enabling cross-material
 *   comparisons, generating specific empirical predictions) while
 *   simultaneously extracting asymmetric benefits (career advancement for QCP
 *   theorists, funding concentration, journal gatekeeping that disadvantages
 *   alternative mechanisms). The temporal measurements reveal a concerning
 *   trajectory: base extractiveness has nearly doubled (0.28 → 0.54), theater
 *   ratio has risen 80% (0.38 → 0.68), and suppression requirement has
 *   increased 63% (0.38 → 0.62). This signature suggests drift from genuine
 *   coordination (early normal science) toward performative institutional
 *   maintenance and explicit suppression of alternatives. The framework
 *   remains empirically contested: key predictions (non-Fermi-liquid
 *   behavior, universal scaling, specific high-frequency response signatures)
 *   show mixed confirmation across materials, and alternative mechanisms
 *   (disorder-induced quantum criticality, competing orders coupled to
 *   superconductivity, topological pairing mechanisms) account for comparable
 *   phenomenology in many systems without requiring QCP mediation.
 *
 * KEY AGENTS:
 *   - QCP Theoretical Community: Beneficiary (institutional/arbitrage) — senior theorists, funding administrators, textbook authors who benefit from paradigm consolidation and control interpretation
 *   - Alternative Pairing Mechanism Researchers: Primary victim (powerless/trapped) — researchers pursuing phonon, exciton, topological, or disorder-mediated mechanisms face funding bias, reviewer gatekeeping, recruitment barriers, and career penalties
 *   - Early-Career QCP Researchers: Secondary victim/partial beneficiary (moderate/constrained) — gain career access through QCP-organized funding and departments; face risk if paradigm weakens or if they question assumptions
 *   - High-Resolution Experimental Community: Mixed (organized/constrained) — ARPES, STM, neutron scattering, RF impedance capabilities enable empirical testing of QCP predictions; constrained by interpretive pressure to frame results within QCP language and by funding skew toward QCP theory
 *   - QCP Review and Pedagogical Literature: Institutional piton (institutional/arbitrage) — review articles, textbooks, and pedagogical papers systematize QCP framework; provide real coordination benefit but also performative gatekeeping of narrative
 *   - Field Methodological Diversity: Victim (powerless/trapped) — abstract collective benefit that cannot organize or exit; loses potential methodological pluralism as QCP framework becomes canonical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_critical_point_superconductivity, 0.54).
domain_priors:suppression_score(quantum_critical_point_superconductivity, 0.62).
domain_priors:theater_ratio(quantum_critical_point_superconductivity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_critical_point_superconductivity, extractiveness, 0.54).
narrative_ontology:constraint_metric(quantum_critical_point_superconductivity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quantum_critical_point_superconductivity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_critical_point_superconductivity, tangled_rope).
narrative_ontology:human_readable(quantum_critical_point_superconductivity, "Quantum Critical Point Superconductivity Mechanism and Verification").
narrative_ontology:topic_domain(quantum_critical_point_superconductivity, "condensed_matter_physics/high_temperature_superconductivity").

domain_priors:requires_active_enforcement(quantum_critical_point_superconductivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_critical_point_superconductivity, qcp_theoretical_community).
narrative_ontology:constraint_beneficiary(quantum_critical_point_superconductivity, qcp_funding_programs).
narrative_ontology:constraint_beneficiary(quantum_critical_point_superconductivity, senior_qcp_researchers).
narrative_ontology:constraint_victim(quantum_critical_point_superconductivity, field_methodological_diversity).
narrative_ontology:constraint_victim(quantum_critical_point_superconductivity, alternative_pairing_mechanisms).
narrative_ontology:constraint_victim(quantum_critical_point_superconductivity, early_career_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE PAIRING MECHANISM ADVOCATES (SNARE) — Researchers pursuing non-QCP mechanisms (phonon-mediated, exciton-mediated, topological) face systematic structural suppression: funding skew toward QCP frameworks, journal reviewer gatekeeping favoring QCP interpretation, recruitment difficulties in QCP-dominated departments, career penalties for dissenting explanations. Exit is material and cognitive — cannot leave the field without abandoning their research program. Maximum experienced extraction.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER QCP RESEARCHERS (TANGLED ROPE) — Benefit from training and job market access within QCP-organized departments and funding streams. Also constrained by career risk if QCP framework collapses or if they question core QCP assumptions. Mixed extraction and genuine coordination of research ecosystem.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QCP THEORETICAL LEADERSHIP (ROPE) — Senior theorists and funding administrators benefit from paradigm consolidation: citation advantage, funding concentration, graduate recruitment, institutional power. Experience the constraint as coordination of a research program — their power allows exit at minimal cost, and they genuinely solve the coordination problem of organizing theoretical understanding around a unified framework.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-RESOLUTION EXPERIMENTAL COMMUNITY (TANGLED ROPE) — Possess high-resolution measurement capabilities (STM, ARPES, neutron scattering, RF impedance) that can test QCP predictions. Benefit from research questions the QCP framework poses; constrained by funding allocation favoring theoretical QCP work and interpretive pressure to frame results within QCP language. Can exit by reframing results independently, but face publication and career pressure to adopt QCP interpretation.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: QCP REVIEW LITERATURE (PITON) — The extensive review infrastructure (textbooks, review articles, pedagogical papers) that organizes understanding around QCP framework persists through institutional inertia despite contested empirical status. Theater ratio high (0.68): much effort devoted to systematizing and presenting QCP theory, but core empirical predictions remain unconfirmed. The review literature's primary function (organizing pedagogical knowledge) is genuine; its secondary extraction function (gatekeeping alternative explanations) is performative.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From maximum civilizational/universal perspective, quantum criticality may be a fundamental organizing principle of strongly correlated systems that superconducting pairing mechanisms must satisfy as a constraint. If true, QCP framework reflects natural law. However, structural data reveals beneficiaries and victims — the naturalizing framing obscures that a contingent theoretical choice has been institutionally enforced.
constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_critical_point_superconductivity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quantum_critical_point_superconductivity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantum_critical_point_superconductivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quantum_critical_point_superconductivity, TR),
    TR >= 0.70.

:- end_tests(quantum_critical_point_superconductivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.54): Moderate-high, reflecting significant asymmetric benefits to QCP theorists and substantial costs to alternative-mechanism researchers, but not maximal because the coordination function is genuinely real — the QCP framework does organize theoretical understanding and does generate specific, testable empirical predictions that have partially confirmed. The 0.28→0.54 trajectory shows extraction accumulation as the framework transitions from active hypothesis to crystallized paradigm. SUPPRESSION (0.62): Moderate-high, reflecting structural barriers that are both material (funding allocation, journal editorial practices, department hiring) and cognitive (internalized paradigm assumptions, self-censorship in graduate training). The 0.38→0.62 rise indicates institutionalization of suppression mechanisms as the framework becomes unstated background assumption. THEATER RATIO (0.68): High, reflecting that much current QCP research effort is devoted to systematizing and defending the framework rather than to empirical testing — confirmation bias in literature selection, post-hoc reinterpretation of null results, epicyclic adjustments to accommodate contradictory observations. The 0.38→0.68 trajectory shows theater accumulation characteristic of paradigm approaching crisis (Kuhn's pre-revolution phase). CLAIMED TYPE: Tangled Rope because the constraint simultaneously provides coordination (unifying theoretical framework) and extraction (asymmetric benefit distribution, suppression of alternatives, performative gatekeeping). The beneficiary/victim structure is clear and material, not merely perspectival.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional structure produces radically different experiences from different positions. QCP theorists and funded researchers see a successful paradigm (Rope — experiencing primarily coordination benefits with minimal extraction cost). Early-career researchers committed to QCP see mixed benefits and risks (Tangled Rope — genuine career access but vulnerability to paradigm collapse). Researchers pursuing alternative mechanisms see pure extraction (Snare — funding barriers, reviewer gatekeeping, career penalties). The experimental community sees a generator of interesting research questions (Tangled Rope — genuine coordination benefit alongside interpretive constraints). The QCP review infrastructure sees its own degradation (Piton — theatrical maintenance of a framework whose empirical status is contested). The analytical observer risks naturalizing this institutional arrangement as a law of science itself (Mountain — QCP criticality as universal principle) — the engine's false-summit detector identifies this as naturalization of a contingent choice. The perspectival gaps encode real structural differences in exit options (beneficiaries have arbitrage, victims have none) and power (theorists coordinate institutional resources, alternative researchers organize from the margins).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value (structuring the directionality-dependent chi calculation) derives from the agent's position in the extraction flow and their exit capacity. QCP theorists (beneficiaries with arbitrage exit options) experience low d and minimal f(d) — their effective extraction chi is kept low by their power to reframe or exit. Alternative-mechanism researchers (victims with trapped exit options) experience high d and maximum f(d) — their experienced extraction chi is amplified by their structural immobility. Early-career QCP researchers (partial beneficiaries with constrained exit) experience mid-range d modulated by their career vulnerability. The experimental community (victims with organized collective power) experiences organized d — higher than institutional beneficiaries but lower than isolated individuals. The high-resolution experimental community with measurement capabilities that can test QCP predictions occupies an interesting position: they are victims (interpretive pressure to adopt QCP language) but also organized (collective measurement capacity provides countervailing power). The directionality overrides are not necessary here — the derived d values from beneficiary/victim declarations and exit options produce appropriate perspectival gaps without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint is classified as Tangled Rope rather than defaulting to Rope because the structural data demonstrates both genuine coordination (the QCP framework does organize theoretical understanding and generate testable predictions) AND asymmetric extraction (beneficiary careers, victim suppression, performative gatekeeping). The mandatrophy is resolved by showing that the coordination function cannot be separated from the extraction mechanism — the same institutional structure that enables the theoretical unification also suppresses alternative explanations and concentrates benefit. If the constraint were classified as pure Rope (dismissing the asymmetric extraction), the analysis would miss the structural injustice to alternative-mechanism researchers and the institutional suppression evident in the temporal measurements. If classified as pure Snare (dismissing the genuine coordination), it would overstate the irrationality of the theoretical commitment — the QCP framework does provide real insight and does generate confirmable predictions for some materials. The Tangled Rope classification acknowledges both: the coordination is genuine and the extraction is real. The engine would compute from this base that Mandatrophy remains UNRESOLVED if extractiveness > 0.70, but here ε=0.54 places the constraint below that threshold. However, the temporal trajectory (0.28→0.54 over 20 years, linear extrapolation would reach 0.70 in ~25 years) suggests the constraint may enter mandatory mandatrophy resolution within the next decade as theater and suppression continue rising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qcp_mechanism_empirical_status,
    'Do the key empirical signatures predicted by the QCP framework (non-Fermi liquid behavior, specific heat scaling, neutron scattering response) emerge from QCP-mediated pairing or from alternative mechanisms (disorder, inhomogeneity, competing orders)?',
    'High-resolution angle-resolved photoemission spectroscopy (ARPES) mapping of superconducting gap structure in QCP materials vs non-QCP materials; controlled studies of disorder effects vs quantum criticality; computational studies of competing-order coupling to superconductivity with and without QCP fluctuations',
    'If QCP mechanism confirmed: classification remains Tangled Rope with justified institutional structure. If alternative mechanisms account for signatures: framework becomes extractive Snare with false coordination claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qcp_mechanism_empirical_status, empirical, 'Empirical status of QCP-mediated superconductivity mechanism').

omega_variable(
    paradigm_lock_institutionalization,
    'To what extent has QCP framework been institutionalized as an unstated background assumption (Thomas Kuhn''s paradigm in pre-crisis form) rather than as a falsifiable hypothesis actively tested against alternatives?',
    'Meta-analysis of funding allocation by mechanism class (QCP vs non-QCP) over 20-year interval; citation analysis of high-citation QCP review papers vs high-citation alternative mechanism papers; survey of journal acceptance rates for QCP vs alternative-mechanism papers; interview analysis of graduate training narratives in QCP vs non-QCP groups',
    'If paradigm lock confirmed: suppression and theater values justified; tangled rope classification accurate. If framework remains empirically contested at institutional level: suggests institutional extraction greater than claimed, pushing toward snare from multiple perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paradigm_lock_institutionalization, empirical, 'Degree to which QCP framework operates as unstated paradigm vs. active hypothesis').

omega_variable(
    alternative_mechanism_research_capacity,
    'What fraction of high-quality experimental and theoretical talent has genuine capacity to pursue non-QCP mechanisms without career penalty, and what is that career penalty measured as (grant success rate, publication venue, hiring outcomes)?',
    'Longitudinal career tracking of 50-100 early-career researchers pursuing alternative mechanisms vs QCP mechanisms over 10-year interval; measurement of grant success rates (NIH, NSF, DOE) by mechanism class; journal impact-factor distribution analysis for QCP vs alternative-mechanism publications; department hiring outcomes for non-QCP postdocs',
    'If career penalty is substantial (grant rate 30% below QCP baseline, publication venues 2+ tiers lower): suppression value is accurate and may underestimate structural cost. If penalty is minimal: suggests suppression is more cognitive/framing than material.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_mechanism_research_capacity, empirical, 'Career penalty for pursuing non-QCP mechanisms').

omega_variable(
    false_summit_natural_law_claim,
    'Is the QCP framework''s claim to explain superconductivity mechanistically a genuine physical law (universal principle that any superconductor must satisfy) or a contingent institutional choice that benefits QCP theorists and appears ''natural'' only from within the paradigm?',
    'Demonstration that QCP framework is not required to model any known superconductor (counterexamples where phonon, exciton, or topological mechanisms account for all measured properties); analysis of discovery history showing QCP framework emerged from theoretical convenience rather than empirical necessity; cross-paradigm synthesis showing alternative mechanisms produce equivalent phenomenology',
    'If contingent institutional choice: engine false-summit detector fires, reclassifies mountain perspective as false summit. Constraint moves from potentially natural-law (mountain) toward structurally extractive (tangled rope / snare). If genuine universal principle: mountain perspective confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, empirical, 'Whether QCP framework is natural law or contingent institutional choice').

omega_variable(
    temporal_succession_logic,
    'Are QCP-superconductivity claims currently in normal-science consolidation phase (paradigm deepening) or in pre-crisis phase (persistent failures to confirm key predictions, growing alternative explanations)?',
    'Kuhnian textual analysis of QCP literature 2000-2010 vs 2015-2025: count of confirmed predictions, failed predictions, abandoned predictions, new post-hoc epicycles; measurement of inter-reviewer citation consistency (do reviewers cite the same papers as ''key confirmation''?); analysis of new-entrant theoretical papers citing QCP: do they extend from established core or propose significant modifications?',
    'If normal-science consolidation: tangled rope / rope perspective justified — paradigm provides real coordination even if beneficiary structure is asymmetric. If pre-crisis phase: suppression and theater values may underestimate — framework is sustained by institutional force against empirical pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_succession_logic, empirical, 'Whether QCP research is in normal-science consolidation or pre-crisis phase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_critical_point_superconductivity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qcp_sc_theater_t0, quantum_critical_point_superconductivity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(qcp_sc_theater_t10, quantum_critical_point_superconductivity, theater_ratio, 10, 0.52).
narrative_ontology:measurement(qcp_sc_theater_t20, quantum_critical_point_superconductivity, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(qcp_sc_extract_t0, quantum_critical_point_superconductivity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qcp_sc_extract_t10, quantum_critical_point_superconductivity, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(qcp_sc_extract_t20, quantum_critical_point_superconductivity, base_extractiveness, 20, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(qcp_sc_suppress_t0, quantum_critical_point_superconductivity, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(qcp_sc_suppress_t10, quantum_critical_point_superconductivity, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(qcp_sc_suppress_t20, quantum_critical_point_superconductivity, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_critical_point_superconductivity, information_standard).
narrative_ontology:affects_constraint(quantum_critical_point_superconductivity, heavy_fermion_superconductivity_mechanism).
narrative_ontology:affects_constraint(quantum_critical_point_superconductivity, unconventional_pairing_mechanism_verification).
narrative_ontology:affects_constraint(quantum_critical_point_superconductivity, condensed_matter_paradigm_competition).

% DUAL FORMULATION NOTE:
% The QCP-superconductivity constraint is upstream of constraints about verification of specific pairing mechanisms in individual materials (e.g., iron-based superconductors, cuprates, heavy-fermion systems). The framework shapes how these individual material constraints are interpreted and studied. Alternative mechanism constraints (phonon-mediated pairing, topological pairing, exciton-mediated pairing) exist as sibling competing constraints; their institutional suppression is a direct consequence of QCP framework dominance. Paradigm-competition constraint captures the meta-level dynamics of how institutional resources are allocated between frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quantum_critical_point_superconductivity, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
