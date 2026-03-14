% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_erosion, []).

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
 *   constraint_id: institutional_legitimacy_erosion
 *   human_readable: Institutional Legitimacy Erosion through Performative Compliance
 *   domain: institutional_governance/legitimacy
 *
 * SUMMARY:
 *   Institutional legitimacy erosion describes the structural process by
 *   which institutions—governments, corporations, educational systems,
 *   healthcare providers—lose stakeholder trust and perceived accountability
 *   while simultaneously increasing their mechanisms of performative
 *   compliance. The constraint exhibits a paradox: institutions respond to
 *   legitimacy crises by adding oversight, consultation procedures,
 *   transparency requirements, and reform initiatives, yet these responses
 *   often become theater that further erodes authentic legitimacy. The
 *   erosion is not random—it systematically benefits institutional leadership
 *   (who can arbitrage between internal power and external legitimacy claims)
 *   while extracting from stakeholders (who bear suppression and lose voice).
 *   The constraint operates through suppression of authentic feedback
 *   channels, substitution of theater for genuine accountability, and
 *   concentration of arbitrage opportunities among leadership. The
 *   theater_ratio rising from 0.35 to 0.68 over 20 time units reflects that
 *   institutional responses to legitimacy crises increasingly consist of
 *   procedural theater rather than genuine power redistribution.
 *   Extractiveness rises from 0.28 to 0.58 as the gap widens between the
 *   institution's legitimacy claims and stakeholder experience.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures legitimacy claims while maintaining power concentration; benefits from theater that absorbs reform pressure
 *   - Stakeholder Constituencies: Primary victims (powerless/trapped) — depend on institutions they cannot exit; experience suppression through administrative complexity and information asymmetry
 *   - Reform-Oriented Middle Managers: Secondary actors (moderate/constrained) — caught between authentic coordination function and suppression of dissent; high cost to exit
 *   - Transparency and Accountability Movement: Organized resistance (organized/mobile) — building decentralized accountability mechanisms as sunset pathways
 *   - Institutional Oversight Systems: Degraded infrastructure (institutional/arbitrage) — legitimacy theater mechanisms (oversight boards, consultations, audits) have atrophied from genuine accountability to procedural ritual
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional dynamics as immutable laws of organizational existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_erosion, 0.58).
domain_priors:suppression_score(institutional_legitimacy_erosion, 0.52).
domain_priors:theater_ratio(institutional_legitimacy_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_erosion, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(institutional_legitimacy_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_erosion, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_erosion, "Institutional Legitimacy Erosion through Performative Compliance").
narrative_ontology:topic_domain(institutional_legitimacy_erosion, "institutional_governance/legitimacy").

domain_priors:requires_active_enforcement(institutional_legitimacy_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_erosion, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_erosion, regulatory_arbitrageurs).
narrative_ontology:constraint_victim(institutional_legitimacy_erosion, public_trust).
narrative_ontology:constraint_victim(institutional_legitimacy_erosion, stakeholder_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STAKEHOLDER CONSTITUENCY (SNARE) — Trapped within institutional systems they depend on but cannot exit (healthcare, education, governance). Experiences erosion as loss of voice and recourse. High suppression through administrative complexity and information asymmetry. No alternative structures available at scale. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_legitimacy_erosion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-ORIENTED MIDDLE MANAGER (TANGLED ROPE) — Constrained by career dependency and organizational hierarchy. Genuine coordination function exists: authentic reform attempts preserve institutional legitimacy. But asymmetric extraction occurs through suppression of dissenting voices and channeling reform energy into theater. Moderate agency but high cost of exit.
constraint_indexing:constraint_classification(institutional_legitimacy_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Experiences the constraint as coordination mechanism: legitimacy theater maintains their authority structure while absorbing reform pressure. Can arbitrage between internal restructuring and external legitimacy claims. Net beneficiary with minimal experienced extraction.
constraint_indexing:constraint_classification(institutional_legitimacy_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSPARENCY AND ACCOUNTABILITY MOVEMENT (SCAFFOLD) — Organized agents (civil society, investigative media, audit bodies) see erosion as a temporary coordination failure solvable through structural reforms: distributed oversight, real-time accountability mechanisms, and decentralized legitimacy verification. Mobile within the constraint and building sunset pathways. The constraint's functional lifespan is explicitly targeted.
constraint_indexing:constraint_classification(institutional_legitimacy_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGITIMACY THEATER SYSTEM (PITON) — Established legitimacy mechanisms (oversight boards, stakeholder consultations, transparency reports) have become largely performative. Their original coordination function (authentic accountability) has atrophied while the ritual persists through institutional inertia. Theater ratio high (0.68) reflects that compliance activities are decoupled from actual decision-making power. System knows it is degraded but maintains theater because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(institutional_legitimacy_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, institutional legitimacy always erodes as institutions age and complexity increases — there is an inherent gap between the institution's self-conception and external perception that cannot be closed. This perspective naturalizes legitimacy erosion as immutable to institutional existence. However, the structural data contradicts this: erosion is driven by contingent choices (suppression of authentic feedback, theater substitution, leadership arbitrage) not inherent laws.
constraint_indexing:constraint_classification(institutional_legitimacy_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_erosion, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint systematically benefits institutional leadership through arbitrage (maintaining power while claiming reform) while extracting from stakeholders through suppression and theater. The extraction is not maximal because: (1) authentic coordination functions still exist—institutions do provide services stakeholders depend on, (2) some reform attempts succeed, (3) alternatives are emerging. Theater ratio (0.68): High. Institutional responses to legitimacy crises increasingly consist of procedural theater: oversight boards that don't oversee, consultation processes that don't influence decisions, transparency reports that obscure through data volume. The ratio has risen because institutions have sophisticated theater production while core accountability mechanisms have atrophied. Suppression (0.52): Moderate-high. Stakeholders face high barriers to exit (institutions are monopolies in their domains) and suppressed voice (feedback mechanisms that don't influence decisions). But suppression is not total—some institutions have genuine stakeholder voice, some stakeholders can exit into alternatives. The constraint's tangled_rope classification requires both coordination (institutions still serve) and extraction (leadership arbitrage). The beneficiary/victim split is clear: institutional leadership benefits from the system that suppresses stakeholder voice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Institutional leadership sees a coordination mechanism (Rope)—they are solving the problem of maintaining stakeholder confidence while managing complex operations. The reform movement sees a solvable temporary problem (Scaffold)—distributed accountability can replace centralized theater. The oversight systems see their own degradation (Piton)—audit committees persist through inertia, not function. Middle managers see mixed coordination and extraction (Tangled Rope)—the system both enables their work and suppresses their authentic influence. Stakeholder constituencies see pure extraction (Snare)—institutional legitimacy theater diverts attention from power concentration with no self-correction mechanism. The civilizational observer risks naturalizing the erosion as inherent to large institutions (Mountain)—'institutions always lose legitimacy as they age'—but the structural data reveals this as false. Erosion is driven by contingent choices: suppression policies, theater substitution, leadership incentive structures that reward arbitrage. Different institutional designs produce different legitimacy trajectories.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from the structural position of each agent relative to the legitimacy erosion constraint. Institutional leadership experiences low d (around 0.15) because they are beneficiaries: the constraint allows them to claim legitimacy while concentrating power. Arbitrage exit options reduce d further—they can escape consequences through turnover or lateral mobility. Stakeholder constituencies experience high d (around 0.85-0.95) because they are victims: they cannot exit the institutions they depend on and experience suppression. Trapped exit options increase d maximally. Reform-minded middle managers experience moderate d (around 0.55) because they have constrained exit options and mixed positions: they benefit from institutional stability but are harmed by suppression of authentic reform. The organized transparency movement experiences moderate d (around 0.45) because they are mobile within and around the constraint—they can build alternatives and see exits, reducing their experienced extraction. The sigmoid f(d) applied to these values produces the perspectival gap: institutional leadership perceives low χ (experiences rope-level coordination), stakeholders perceive high χ (experiences snare-level extraction), reformers perceive moderate χ (experiences tangled_rope with mixed coordination and extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy resolution through perspectival pluralism. The mandatrophy is not 'is this extraction or coordination?' but 'which perspective are we measuring from, and what does that perspective reveal?' The institutional leadership's Rope perspective reveals the genuine coordination function (institutions do coordinate complex activities). The stakeholder's Snare perspective reveals the asymmetric power structure. The reform movement's Scaffold perspective reveals that the erosion is not inevitable—distributed accountability can provide alternatives. The piton perspective reveals that institutional theater has atrophied as a genuine verification mechanism. The mountain perspective is a false summit: it naturalizes as law what is actually institutional design. The tangled rope is the accurate composite—the system genuinely coordinates some functions while extracting asymmetrically. Resolution requires: (1) acknowledging that all perspectives contain truth about the system's structure, (2) identifying which asymmetries (suppression, theater, arbitrage) are contingent and removable, (3) building alternatives (distributed accountability) that change the constraint's type from tangled_rope or snare toward pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentic_reform_threshold,
    'What proportion of reform activity must be genuine (rather than theatrical) to sustain legitimacy above critical erosion threshold?',
    'Longitudinal analysis of reform initiatives: tracking which are implemented vs which are archived; stakeholder perception surveys before/after reforms; comparison of promised vs actual institutional behavior changes',
    'If threshold > 40% authentic: legitimacy erosion is primarily extraction (Snare). If threshold < 10% authentic: even theater can sustain legitimacy temporarily (extends Piton lifespan). If threshold 20-30%: current system is at critical point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_reform_threshold, empirical, 'Minimum authentic reform proportion for legitimacy sustainability').

omega_variable(
    distributed_accountability_sufficiency,
    'Can decentralized accountability mechanisms (citizen oversight, algorithmic auditing, distributed verification) actually replace centralized institutional legitimacy without new extraction mechanisms emerging?',
    'Comparative case analysis of institutions with distributed oversight vs traditional hierarchical oversight; measurement of new extraction patterns in decentralized systems; stakeholder satisfaction metrics across governance models',
    'If sufficient: scaffold sunset is real and achievable. If insufficient: distributed systems recreate asymmetries (coordination problems between oversight bodies, information overload reducing effectiveness). Affects timeline for erosion resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_accountability_sufficiency, empirical, 'Whether distributed accountability can replace institutional legitimacy').

omega_variable(
    suppression_mechanism_type,
    'Is suppression structural (legal/bureaucratic barriers to exit or voice) or internalized (stakeholders have normalized powerlessness)?',
    'Post-barrier-removal experiments: tracking whether suppression persists when formal barriers are removed (e.g., open-forum policies, legal right-to-exit mechanisms); measurement of stakeholder voice activation after structural suppression reduction',
    'If internalized: stakeholders carry suppression with them even after institutional barriers fall — legitimacy erosion reflects deep identity capture. If structural: removing barriers significantly activates voice and restores legitimacy. Affects classification of sustained erosion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    leadership_arbitrage_incentive_removal,
    'Can institutional incentive structures be modified to remove leadership''s arbitrage advantage without collapsing the institution''s coordination function?',
    'Institutional redesign experiments: alignment mechanisms linking leadership compensation to stakeholder satisfaction; transparency that makes arbitrage visible; distributed decision authority that prevents centralized arbitrage. Measure whether coordination function persists without arbitrage.',
    'If removable: institutional Rope classification becomes stable. If not removable: arbitrage is inherent to institutional power concentration. Affects whether erosion is reversible or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_arbitrage_incentive_removal, preference, 'Whether leadership arbitrage incentives can be removed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_erosion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ile_tr_t0, institutional_legitimacy_erosion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ile_tr_t10, institutional_legitimacy_erosion, theater_ratio, 10, 0.52).
narrative_ontology:measurement(ile_tr_t20, institutional_legitimacy_erosion, theater_ratio, 20, 0.68).
narrative_ontology:measurement(ile_tr_t5, institutional_legitimacy_erosion, theater_ratio, 5, 0.43).

% Extraction over time
narrative_ontology:measurement(ile_be_t0, institutional_legitimacy_erosion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ile_be_t10, institutional_legitimacy_erosion, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ile_be_t20, institutional_legitimacy_erosion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ile_be_t5, institutional_legitimacy_erosion, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_erosion, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_erosion, regulatory_capture).
narrative_ontology:affects_constraint(institutional_legitimacy_erosion, expertise_gatekeeping).
narrative_ontology:affects_constraint(institutional_legitimacy_erosion, stakeholder_voice_suppression).

% DUAL FORMULATION NOTE:
% Institutional legitimacy erosion is downstream of specific institutional failures (regulatory capture, expertise gatekeeping) and upstream of stakeholder resistance and alternative structure-building. Each affects_constraint has its own extractiveness reflecting domain-specific dynamics; this story captures the general structural pattern across institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_erosion, institutional, 0.18).
constraint_indexing:directionality_override(institutional_legitimacy_erosion, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
