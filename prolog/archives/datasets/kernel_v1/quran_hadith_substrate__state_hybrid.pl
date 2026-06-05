% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Adoption of Classical Islamic Law
 *   domain: islamic_jurisprudence/legal_theory/religious_authority
 *
 * SUMMARY:
 *   The state hybrid reading of Islamic law represents a structural
 *   compromise between religious legitimacy and institutional sovereignty. A
 *   state claims grounding in quranic and hadith sources while selectively
 *   applying classical jurisprudential rulings in family and criminal domains
 *   and deploying reformist or secular-derived frameworks in commercial and
 *   administrative law. Legitimacy is explicitly grounded in political
 *   sovereignty and regime stability rather than in comprehensive doctrinal
 *   fidelity. This reading coexists with traditionalist readings
 *   (comprehensive application of classical law) and reformist readings
 *   (systematic reinterpretation of sources for contemporary conditions). The
 *   state hybrid differs from both: it rejects the traditionalist claim to
 *   comprehensive application but also rejects the reformist claim to
 *   systematic reinterpretation. Instead, it instrumentalizes selective
 *   rulings to maximize legitimacy in high-salience domains while preserving
 *   institutional flexibility in economically critical domains. The
 *   constraint exhibits extraction (state captures legitimacy benefits while
 *   bearing minimal institutional cost) layered over coordination (how to
 *   maintain dual legitimacy systems without internal contradiction).
 *   Extractiveness has risen over the measurement interval (0.28 → 0.38) as
 *   state capacity to enforce selective application has increased. Theater
 *   ratio has also risen (0.48 → 0.65), reflecting increasing displacement of
 *   substantive jurisprudential decision-making by performative processes.
 *   Suppression has intensified (0.45 → 0.52), particularly on reformist
 *   scholarship that threatens to destabilize the regime's preferred domain
 *   boundaries.
 *
 * KEY AGENTS:
 *   - State Executive Elite: Institutional beneficiary (institutional/arbitrage) — captures legitimacy in Islamic public, preserves policy flexibility in economic domains, manages domain selection to maximize regime stability
 *   - Traditionalist Ulama: Primary victim (powerless/trapped) — comprehensive jurisprudential vision is truncated; institutionalization requires acceptance of state-selected subset of classical rulings
 *   - Reformist Scholars: Secondary victim (moderate/constrained) — critical reinterpretation is suppressed when it threatens regime stability; self-censorship imposed by regime preference and social pressure
 *   - Economic and Commercial Elite: Secondary beneficiary (powerful/mobile) — benefits from predictable secular-derived commercial law while maintaining religious legitimacy through traditionalist family law
 *   - Neighboring State System: Institutional observer (institutional/arbitrage) — benefits from the state's bounded application (coordination mechanism reducing cross-border sharia-destabilization risk)
 *   - Formal Jurisprudential Institutions: Institutional vessel (institutional/arbitrage) — maintains symbolic authority while effective decision-making migrates to state-appointed judges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.52).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Adoption of Classical Islamic Law").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "islamic_jurisprudence/legal_theory/religious_authority").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'edb5451b-bba5-464d-b9fc-70d6a02f6274').
narrative_ontology:cs_kernel_codification('edb5451b-bba5-464d-b9fc-70d6a02f6274', formalized).
narrative_ontology:cs_authority_grounding('edb5451b-bba5-464d-b9fc-70d6a02f6274', extraction).
narrative_ontology:cs_interpretation_layer_present('edb5451b-bba5-464d-b9fc-70d6a02f6274').
narrative_ontology:cs_reading_relation('edb5451b-bba5-464d-b9fc-70d6a02f6274', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('edb5451b-bba5-464d-b9fc-70d6a02f6274', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('edb5451b-bba5-464d-b9fc-70d6a02f6274', foundational, state_sovereignty_permits_selective_instrumentalization).
narrative_ontology:cs_axiom_status(state_sovereignty_permits_selective_instrumentalization, holdable).
narrative_ontology:cs_axiom_grounding('edb5451b-bba5-464d-b9fc-70d6a02f6274', state_sovereignty_permits_selective_instrumentalization, instrumental).
narrative_ontology:cs_axiom('edb5451b-bba5-464d-b9fc-70d6a02f6274', foundational, domain_boundary_preservation_overrides_doctrinal_consistency).
narrative_ontology:cs_axiom_status(domain_boundary_preservation_overrides_doctrinal_consistency, holdable).
narrative_ontology:cs_axiom_grounding('edb5451b-bba5-464d-b9fc-70d6a02f6274', domain_boundary_preservation_overrides_doctrinal_consistency, instrumental).
narrative_ontology:cs_reference_frame('edb5451b-bba5-464d-b9fc-70d6a02f6274', selective_classical_application_with_sovereign_discretion).
narrative_ontology:cs_drift_state('edb5451b-bba5-464d-b9fc-70d6a02f6274', contemporary_high_institutional_capacity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('edb5451b-bba5-464d-b9fc-70d6a02f6274', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_executive_elite).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, economic_policy_actors).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_community).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_scholars).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONALIST ULAMA (SNARE) — Trapped within the constraint's framework. Traditionalists cannot exit the state system's selective application of classical rulings without abandoning institutional legitimacy and public influence. The state instrumentalizes their jurisprudential heritage while truncating the comprehensive vision of sharia they hold. No alternative institutional pathway exists within the national legal domain.
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORMIST SCHOLAR (SNARE) — Constrained by regime stability concerns and elite preference for selective rather than systematic reinterpretation. Reformists face suppression when critical ijtihad (reinterpretation) threatens regime-selected rulings. Exit is possible (emigration, underground scholarship) but at severe cost. The state extracts regime legitimacy from their work while suppressing their full critical apparatus.
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE EXECUTIVE ELITE (TANGLED ROPE) — Primary beneficiary. Gains legitimacy in the Islamic public through selective application of classical rulings in domains (family law, criminal codes) where popular expectation is highest, while preserving institutional flexibility in commercial and administrative law where modern state function requires secular-derived frameworks. This is a genuine coordination mechanism (how to maintain dual legitimacy) layered over asymmetric extraction (which domains are selected is determined by state power, not consensus).
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ECONOMIC AND COMMERCIAL ELITE (TANGLED ROPE) — Secondary beneficiary. Benefits from the state's preservation of reformist/secular frameworks in commercial law (contract enforcement, property rights, corporate regulation) while securing legitimacy through traditionalist rulings in domains that do not constrain economic activity. This agent experiences coordination (predictable commercial law) alongside extraction (must maintain public piety narratives that constrain their actual business operations).
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: NEIGHBORING STATE SYSTEM (ROPE) — Experiences the hybrid approach as coordination mechanism. Neighboring states benefit from the state's bounded use of Islamic law (predictable, selective, not comprehensive). The constraint solves a collective action problem among states: how to maintain Islamic legitimacy without triggering cross-border sharia-based destabilization. Exit is high-cost but available (regional isolation, economic sanctions).
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: FORMAL JURISPRUDENTIAL SYSTEM (PITON) — Classical Islamic jurisprudence (qadi courts, formal taqlid [imitation of precedent], mujtahid [interpreter] authority structures) persists in symbolic form while effective decision-making power migrates to state-appointed judges applying state-selected rulings. The theatrical component is high: formal jurisprudential language and process are maintained while the substance is displaced. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational angle, the tension between religious authority and state sovereignty appears as an immutable structural condition: any polity that grounds legitimacy in both religious tradition and political sovereignty must navigate selective application. This appears as a law of politics rather than a contingent institutional choice. However, the structural data (beneficiaries, victims, active enforcement) indicates this is a false summit — the naturalizing frame obscures the state's deliberate instrumentalization.
constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_hadith_substrate__state_hybrid, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, TR),
    TR >= 0.70.

:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The state benefits from dual legitimacy (Islamic grounding without comprehensive constraints) while bearing institutional cost of maintaining two parallel legal frameworks and managing cognitive dissonance between traditionalist and reformist publics. The extraction is significant but not maximal because (1) traditionalist and reformist populations maintain partial exit options (emigration, underground networks, ideological opposition) and (2) the state must invest substantial enforcement capacity to maintain the selective-application fiction. Suppression (0.52): Moderate-high. Traditionalists are trapped within the state system (no alternative institutional pathway exists in most state contexts), but reformists have constrained but real exit options (emigration, academic exile, publishing abroad). The suppression mechanism is both structural (regime explicitly interdicts certain reinterpretations) and internalized (scholars anticipate regime preferences and self-censor). Theater ratio (0.65): Moderate-high. Classical jurisprudential language and process (fatwa solicitation, qadi appointment, taqlid claims) are maintained while substantive interpretation is displaced by state-appointed judges applying state-selected rulings. The theater ratio has increased over the interval as state institutional capacity has grown — the formal apparatus persists but the interpretive authority has migrated.
 *
 * PERSPECTIVAL GAP:
 *   The state executive perceives the constraint as coordination (tangled_rope: how to maintain legitimacy with both Islamic and economic publics). Traditionalists perceive pure extraction (snare: their comprehensive vision is instrumentalized). Reformists perceive selective suppression (snare: critical reinterpretation is blocked). The commercial elite perceive net benefit (tangled_rope: they gain commercial predictability at the cost of public piety requirements). The analytical observer risks perceiving an immutable condition (mountain: any polity must balance religious and sovereign legitimacy). The formal jurisprudential system persists as a degraded institution (piton: process survives but function is displaced). The perspectival disagreement is not about what is happening (selective application is observable) but about whether this represents a coherent jurisprudential reading or pure instrumentalization — this ambiguity is routed to the kernel_reading_ambiguity omega.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. State elite: beneficiary + arbitrage exit → d ≈ 0.10 (institutional authority, low extraction experienced). Traditionalist ulama: victim + trapped exit → d ≈ 0.92 (maximum extraction experienced). Reformist scholars: victim + constrained exit → d ≈ 0.68 (high extraction, but with partial escape routes). Commercial elite: beneficiary + mobile exit → d ≈ 0.35 (net benefit despite piety cost). The perspectival gap between institutional beneficiaries (who see rope/tangled_rope) and trapped/constrained victims (who see snare) reflects these different directionality values and applied sigmoid transformations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through kernel reading decomposition. The tangled_rope classification (moderate extraction + genuine coordination) is accurate for the state hybrid reading specifically. Traditionalist and reformist readings would classify differently (traditionalist likely sees itself as rope/mountain, reformist as snare/tangled_rope). The apparent mandatrophy (is this coordination or extraction?) dissolves when recognized as perspectival disagreement about which reading of the kernel is legitimate. The state elite sees coordination; traditionalists see extraction; reformists see suppressed capacity. No single type 'solves' mandatrophy — rather, the multiple readings are the analytical output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    traditionalist_legitimacy_trap,
    'Can traditionalist ulama maintain comprehensive fidelity to classical jurisprudence while participating in a state system that instrumentalizes selective rulings?',
    'Historical analysis of traditionalist institutional trajectories: do participating ulama eventually compromise comprehensive doctrine, or do they maintain parallel jurisprudential systems? Interviews with ulama on perceived constraints and intellectual freedom.',
    'If institutionalization requires doctrinal compromise: traditionalists experience maximum extraction (snare classification confirmed). If parallel systems are sustainable: traditionalists have genuine agency within the constraint (rope classification becomes more plausible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_legitimacy_trap, empirical, 'Whether traditionalist participation requires doctrinal compromise').

omega_variable(
    reformist_suppression_mechanism,
    'Is suppression of reformist ijtihad (reinterpretation) structural (regime actively blocks critical readings) or internalized (reformist scholars self-censor due to social pressure and regime preference)?',
    'Analysis of reformist scholarly output under different regime orientations; comparison of self-censorship patterns with explicit regime interdiction; tracking of underground vs. public scholarship.',
    'If structural: suppression metric (0.52) is accurate. If internalized: effective suppression is higher than the metric suggests; reformists carry the suppression with them even if the regime were to lift explicit constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformist_suppression_mechanism, empirical, 'Whether reformist suppression is structural or internalized').

omega_variable(
    kernel_reading_ambiguity,
    'Is the state''s selective application a coherent jurisprudential reading of Islamic law, or merely strategic instrumentalization of doctrinal language?',
    'Structural analysis: does the state''s selection pattern follow any consistent jurisprudential principle (e.g., prioritizing rulings that maximize social cohesion, or rulings that predate specific historical innovations)? Or is selection purely pragmatic (choose whichever ruling maximizes regime stability and economic performance in each domain)?',
    'If coherent reading: the constraint represents a legitimate jurisprudential position (reading_relations coexist or influence). If instrumental only: the state''s position is not a true reading but a performance of reading (piton features increase, theater ratio likely higher).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether state selection follows jurisprudential logic or pure instrumentalization').

omega_variable(
    comprehensive_sharia_incompatibility,
    'Are selective application of classical rulings and comprehensive sharia governance (the traditionalist vision) logically incompatible in a single legal system, or could a framework accommodate both?',
    'Jurisprudential analysis: can classical rulings in family/criminal domains coexist with secular-derived frameworks in commercial law under a unified legitimacy claim? Do any Islamic jurists defend this position systematically?',
    'If incompatible: traditionalist reading forecloses state hybrid reading (or vice versa). If compatible: readings coexist, and the perspectival gap reflects political power differences rather than logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comprehensive_sharia_incompatibility, conceptual, 'Whether selective and comprehensive applications are jurisprudentially compatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qhs_hybrid_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.48).
narrative_ontology:measurement(qhs_hybrid_tr_t10, quran_hadith_substrate__state_hybrid, theater_ratio, 10, 0.58).
narrative_ontology:measurement(qhs_hybrid_tr_t20, quran_hadith_substrate__state_hybrid, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(qhs_hybrid_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qhs_hybrid_be_t10, quran_hadith_substrate__state_hybrid, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(qhs_hybrid_be_t20, quran_hadith_substrate__state_hybrid, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qhs_hybrid_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qhs_hybrid_su_t10, quran_hadith_substrate__state_hybrid, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(qhs_hybrid_su_t20, quran_hadith_substrate__state_hybrid, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quranic_textual_authority).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, formal_qadi_institutions).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_legal_pluralism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_comprehensive_sharia).

% DUAL FORMULATION NOTE:
% This constraint is one reading of quranic_hadith_substrate kernel. Sibling readings (traditionalist_taqlid, reformist_ijtihad) are separate constraint stories with different ε values, perspectives, and beneficiary/victim structures. The state_hybrid reading (ε=0.38) sits between traditionalist (ε~0.05-0.15, mountain/rope) and reformist (ε~0.45-0.65, tangled_rope/snare) in extractiveness. Network edges link all three readings to shared upstream constraints (textual authority) and downstream institutional structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, institutional, 0.08).
constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
