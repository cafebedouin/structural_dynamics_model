% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Organizational Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability
 *
 * SUMMARY:
 *   This constraint models the structural relationship between catastrophic
 *   incidents and organizational competence maintenance in safety-critical
 *   systems. The core claim of this reading is that catastrophes provide
 *   irreplaceable selection pressure: peacetime periods necessarily produce
 *   competence decay because without mortality salience and crisis-driven
 *   organizational mobilization, organizations revert to compliance theater
 *   and knowledge atrophy. The constraint exhibits high suppression (0.72)
 *   because organizations are structurally locked into peacetime complacency
 *   — there are no effective mechanisms to maintain competence during calm
 *   periods without catastrophe selection pressure. Extractiveness (0.58)
 *   reflects that this lock constrains organizational learning strategies and
 *   workforce safety. The theater ratio (0.68) indicates that peacetime
 *   safety activities (certifications, audits, training drills) are
 *   substantially performative — they create the appearance of competence
 *   maintenance without providing real stress-testing. This reading
 *   instantiates one specific answer to a contested kernel: the
 *   catastrophe_avoidance_retention kernel can be read as asserting that
 *   catastrophes are (a) necessary for competence retention (this reading),
 *   (b) substitutable by sufficiently high-fidelity simulation
 *   (simulation_as_proxy_catastrophe), or (c) addressable through hybrid
 *   mechanisms combining near-miss learning with organizational design
 *   (hybrid_near_miss_learning). This story models only the first reading.
 *
 * KEY AGENTS:
 *   - Frontline Operators: Primary victims (powerless/trapped) — bear mortality risk from competence decay during peacetime; cannot exit the system or trigger organizational learning proactively
 *   - Safety Engineering Profession: Secondary victims (moderate/constrained) — observe the pattern but lack authority to impose costly improvements during stable periods; professional credibility damaged when peacetime decay causes preventable failures
 *   - Risk-Averse Organizations: Primary beneficiaries (institutional/arbitrage) — extract legitimacy from low peacetime crisis frequency (appears as competence); asymmetric enforcement ensures competence decay remains hidden until catastrophe forces organizational response
 *   - Post-Incident Learning Coalition: Organized advocates (organized/mobile) — benefit from catastrophes as sites of forced learning; mobilizing alternatives through simulation research and near-miss escalation
 *   - Regulatory Compliance Regime: Institutional theater-maintainer (institutional/arbitrage) — maintains peacetime safety certification and inspection protocols despite low functional capacity; persists through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constraint as immutable law rather than contingent institutional failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, snare).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Organizational Competence Retention").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning/high_reliability").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '48ace424-04cc-42c5-9912-c03fdcdd025f').
narrative_ontology:cs_kernel_codification('48ace424-04cc-42c5-9912-c03fdcdd025f', implicit).
narrative_ontology:cs_authority_grounding('48ace424-04cc-42c5-9912-c03fdcdd025f', extraction).
narrative_ontology:cs_reading_relation('48ace424-04cc-42c5-9912-c03fdcdd025f', simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('48ace424-04cc-42c5-9912-c03fdcdd025f', hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('48ace424-04cc-42c5-9912-c03fdcdd025f', foundational, catastrophe_selection_irreducible).
narrative_ontology:cs_axiom_status(catastrophe_selection_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('48ace424-04cc-42c5-9912-c03fdcdd025f', catastrophe_selection_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('48ace424-04cc-42c5-9912-c03fdcdd025f', foundational, peacetime_competence_decay_inevitable).
narrative_ontology:cs_axiom_status(peacetime_competence_decay_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('48ace424-04cc-42c5-9912-c03fdcdd025f', peacetime_competence_decay_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('48ace424-04cc-42c5-9912-c03fdcdd025f', catastrophe_as_system_truth).
narrative_ontology:cs_drift_state('48ace424-04cc-42c5-9912-c03fdcdd025f', contemporary_high_reliability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48ace424-04cc-42c5-9912-c03fdcdd025f', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_perpetuation_interests).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organizational_competence).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, workforce_safety).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, operational_learning_during_peace).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped in a system where competence decay during peacetime creates mortality risk. Cannot exit the constraint; forced to maintain vigilance without active reinforcement. The constraint extracts from this agent (zero agency in when learning occurs), suppresses alternatives (cannot force organizational learning during calm periods), and offers minimal coordination benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SAFETY ENGINEERING PROFESSION (SNARE) — Constrained by organizational resistance to 'crisis-driven' improvements during stable periods. Engineers observe the pattern (competence maintenance requires catastrophe selection pressure) but lack authority to impose costly preventive measures on institutions in peacetime. The constraint suppresses alternative learning mechanisms (simulation, near-miss analysis) as inadequate proxies. Moderate extraction — professional credibility damaged when peacetime competence decay causes preventable failures.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RISK-AVERSE ORGANIZATION (TANGLED ROPE) — Institutional actor with arbitrage (can shift liability burden). Experiences genuine coordination: the constraint ensures that catastrophic failures are rare enough to maintain operational legitimacy between incidents. However, asymmetric extraction occurs: the organization extracts operational benefit from 'proven' peacetime stability (low crisis frequency appears as competence), while competence actually decays silently. Active enforcement required to maintain the appearance of safety during calm periods.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: POST-INCIDENT LEARNING COALITION (ROPE) — Organized agents (accident investigators, safety researchers, unions) who benefit from catastrophes as sites of forced organizational learning. See the constraint as a perverse but real coordination mechanism: 'Only after incidents do organizations invest in genuine competence improvement.' This perspective recognizes the constraint is real while organizing to build alternative pathways. Mobile exit through distributed near-miss reporting, simulation-based training, and simulation validation research.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — Institutional maintenance of peacetime safety theater: certifications, inspections, training programs that create the appearance of competence maintenance without actually providing the selection pressure needed. Theater ratio 0.68 reflects that most peacetime safety activities are performative documentation rather than real competence stress-testing. The regulatory regime persists through institutional inertia despite low functional capacity to prevent competence decay.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, organizational competence decay during peacetime is treated as an immutable property of complex systems: all safety-critical competence atrophies without active reinforcement, and catastrophe is the only reinforcement strong enough. This perspective risks naturalizing what is actually a contingent institutional failure — the inability or unwillingness to design non-catastrophic selection mechanisms.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, TR),
    TR >= 0.70.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from safety-critical competence by forcing organizations to operate at the edge of decay throughout peacetime — learning is deferred and atrophy is inevitable without external triggering. The value reflects that this is not maximal extraction (some peacetime learning occurs through near-miss analysis, simulation, and training), but the constraint prevents systematic competence maintenance. The measurement trajectory shows extractiveness rising from 0.42 to 0.58 as peacetime lengthens and competence decay accumulates — the longer stability persists, the more the constraint extracts from organizational capacity. Suppression (0.72): High. Multiple mechanisms suppress alternatives to catastrophe-driven learning: peacetime resource constraints (safety improvements are luxury costs, not crisis necessities), organizational incentive misalignment (crisis response generates authority and attention; peacetime improvement is invisible), regulatory capture (certification requirements are compliance theater, not competence stress-testing), and epistemic closure (the assumption that peacetime stability equals competence prevents recognition of hidden decay). Theater ratio (0.68): High. Peacetime safety activities are substantially performative: annual certifications test compliance with standards, not actual emergency response capability; simulations are constrained by scenarios organizations are already familiar with; training is procedural rather than stress-testing; incident investigations are case-based rather than system-level. The theater serves to maintain the illusion of competence during the periods when competence is actually most degraded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The frontline operator and safety professional both see a snare — they are trapped in a system where learning cannot occur proactively. The organization sees tangled rope — the constraint enables stable operations while asymmetrically extracting from operator and engineer capacity. The post-incident learning coalition sees rope — the constraint is a real coordination mechanism, albeit perverse. The regulatory system sees piton — its peacetime safety theater is functionally degraded but institutionally persistent. The analytical observer risks seeing a mountain — treating the constraint as an immutable property of safety-critical systems. The key gap: between the snare experienced by powerless agents and the tangled rope experienced by institutional actors. Organizations benefit from the constraint's ability to hide competence decay; operators bear the risk. No single type captures the constraint's full structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by structural position. Frontline operators are full victims (d ≈ 0.92, near-trapped without exit) — experience high effective extraction. Safety engineers are moderate victims with some professional mobility (d ≈ 0.68) — face career penalties and loss of professional authority but can migrate to other domains. Risk-averse organizations are beneficiaries with institutional arbitrage (d ≈ 0.05) — extract legitimacy from peacetime stability without bearing competence maintenance costs; experience negative effective extraction. The post-incident learning coalition has mobile exit (d ≈ 0.45, symmetric cost-benefit) — benefits from research funding and organizational authority following incidents but also invests resources in prevention that often go unheeded. The regulatory system benefits through institutional persistence (d ≈ 0.08) — maintains authority and certification income without bearing competence verification costs. The analytical observer occupies the paradoxical position of seeing the full structure while risking capture in the natural-law frame (d ≈ 0.72, observational target). Derived directionality follows naturally from beneficiary/victim status and exit options without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Snare from powerless perspectives versus Tangled Rope from institutional perspectives directly reveals the mandatrophy: is this constraint pure extraction (catastrophe beneficiaries extracting operator vulnerability) or mixed coordination-extraction (organizations coordinately maintaining peacetime stability while asymmetrically suppressing proactive learning)? Both framings are defensible. The snare reading emphasizes that the constraint forces operators into mortality risk without agency. The tangled rope reading emphasizes that organizations genuinely solve the coordination problem of knowing 'how bad is the competence decay?' by using catastrophe as an auditor. The resolution: both are true from their respective positions. The constraint is a snare for powerless agents and tangled rope for institutions. The mandatrophy dissolves when the perspectival structure is made explicit — the single constraint generates legitimately different types from different observatories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_validity_threshold,
    'At what fidelity does simulation become a true substitute for catastrophe-driven selection pressure? Can high-fidelity simulation replicate the cognitive and organizational dimensions of mortality salience?',
    'Comparative analysis of competence retention in organizations using high-fidelity simulations vs. those using catastrophe-driven learning; measurement of knowledge transfer and behavioral change persistence in simulation-trained cohorts under real-world stress',
    'If simulation can replicate selection pressure: the constraint reclassifies from Snare to Tangled Rope (simulation becomes viable coordination mechanism). If simulation only addresses explicit knowledge: the constraint remains Snare (tacit organizational competence still requires catastrophic triggering).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_validity_threshold, empirical, 'Whether high-fidelity simulation can substitute for catastrophe as selection pressure').

omega_variable(
    peacetime_competence_decay_mechanism,
    'Is the observed decay in organizational competence during peacetime a property of human attention/motivation, of institutional incentive structures, or of how complex safety knowledge is stored and transmitted?',
    'Analysis of turnover, training investment, near-miss reporting rates, and knowledge transmission protocols during stable periods; comparison with periods following incidents; controlled intervention studies introducing artificial selection pressure (high-fidelity simulation, mortality salience priming, near-miss amplification) during peacetime',
    'If property of human attention: organizational design might compensate through distributed knowledge systems and rotational safety leadership. If property of institutional incentives: the constraint reflects regulatory and insurance market failures, not immutable human limits. If property of knowledge transmission: alternatives may be available but require active design investment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peacetime_competence_decay_mechanism, empirical, 'Root mechanism of peacetime competence decay').

omega_variable(
    catastrophe_equivalence_span,
    'What organizational and operational variables must a proxy event (simulated emergency, near-miss escalation, external incident observation) share with actual catastrophe to trigger equivalent competence reinforcement?',
    'Analysis of organizational response patterns to real vs. simulated incidents; identification of minimum sufficient variables (mortality salience, resource mobilization, authority reorganization, public accountability); design and test of ''catastrophe-lite'' protocols that embed these variables without requiring actual harm',
    'If catastrophe is substitutable through specific variables: alternative learning pathways become available and the constraint can be decomposed. If catastrophe is irreducible (only mortality actually triggers competence maintenance): the constraint remains fundamental to high-reliability system dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_equivalence_span, empirical, 'Which variables of catastrophe are functionally necessary for competence reinforcement').

omega_variable(
    reading_kernel_ambiguity,
    'Is this constraint one aspect of an immutable property of safety-critical systems (catastrophes necessarily drive competence maintenance), or one contingent policy choice that industries have made when facing organizational learning challenges?',
    'Historical analysis of industries that successfully decoupled competence decay from catastrophe frequency (nuclear power, aviation post-Tenerife, medicine post-surgical error awareness); identification of structural conditions that enabled alternative learning mechanisms; analysis of whether these industries truly eliminated the catastrophe-selection relationship or merely displaced it to other domains',
    'If immutable: this reading (catastrophe_as_necessary_selector) forecloses simulation_as_proxy_catastrophe and hybrid_near_miss_learning as viable frameworks. If contingent: the three readings coexist as institutional choices, and the constraint''s classification varies by organizational design commitments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether catastrophe-selection is immutable or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catret_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.55).
narrative_ontology:measurement(catret_tr_t3, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 3, 0.63).
narrative_ontology:measurement(catret_tr_t6, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(catret_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(catret_be_t3, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(catret_be_t6, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(catret_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(catret_su_t3, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 3, 0.7).
narrative_ontology:measurement(catret_su_t6, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint family decomposes a single colloquial kernel (catastrophe_avoidance_retention) into three structurally distinct claims with different epsilon values and different reading commitments. The three stories are linked as readings of the same underlying kernel and as network-connected constraints reflecting empirical dependencies: decisions about simulation adequacy in one story affect the empirical status of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
