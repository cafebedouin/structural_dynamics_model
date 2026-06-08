% ============================================================================
% CONSTRAINT STORY: institutional_memory_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_memory_decay, []).

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
 *   constraint_id: institutional_memory_decay
 *   human_readable: Institutional Memory Decay in Safety-Critical Industries
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Institutional memory decay is the structural constraint maintaining
 *   safety competence across decades without recurring real disasters through
 *   simulation and regulatory enforcement. The problem space is: how do
 *   organizations retain the competence to respond to catastrophes they have
 *   successfully prevented for so long that no one in the organization has
 *   lived through the actual event? Aviation and nuclear industries have
 *   engineered a solution — mandatory simulations, crew rotation, documented
 *   procedures, regulatory oversight — that has been remarkably effective: no
 *   major commercial aviation disaster in the US in over a decade, and no
 *   significant nuclear accident in developed nations for decades despite
 *   massive increases in the scale and complexity of these systems. Yet the
 *   constraint is extractive because the solution carries a structural
 *   vulnerability: the competence-maintenance mechanism (simulation) differs
 *   from the competence-testing mechanism (real crisis), and that gap creates
 *   opportunities for extraction. Organizations can declare readiness through
 *   passed simulations while operators remain fundamentally untested in real
 *   conditions. Crisis response organizations benefit from being able to
 *   satisfy regulators through lower-cost simulation rather than bearing the
 *   full cost of real-world testing. Frontline operators are trapped in a
 *   system where competence is maintained through abstract exercise but where
 *   the first real crisis is the first real test — and failure carries
 *   catastrophic consequences. The theater_ratio (0.64) reflects that
 *   simulation exercises have become increasingly elaborate and documented,
 *   but their diagnostic value relative to real competence maintenance has
 *   become harder to verify as the time since the last major incident
 *   lengthens.
 *
 * KEY AGENTS:
 *   - Frontline Operators: Primary victim (powerless/trapped) — mandatory simulation participants whose competence is untested in real crisis; bear full cost of institutional memory failure without agency over memory maintenance
 *   - Crisis Response Organization: Primary beneficiary and enforcer (institutional/constrained) — benefits from simulation-based certification (lower cost than real testing) while enforcing it through mandatory drills and regulatory compliance; both coordinates genuine safety function and extracts through underestimated gap between simulation and real competence
 *   - Regulatory Authority: Secondary beneficiary (institutional/arbitrage) — regulates simulation-based competence verification, benefits from system that doesn't require regulators to incentivize or witness real crises, can exit this regime by demanding alternative proof
 *   - Simulation Industry: Beneficiary maintaining atrophied function (institutional/arbitrage) — maintains elaborate simulation infrastructure whose primary function (testing actual competence) has become secondary while theatrical function (generating certified exercises) has grown; infrastructure inertia prevents switch to alternatives
 *   - Safety Culture Integrity: Primary victim (institutional/constrained but not agent) — abstract collective good; the more the simulation system succeeds at preventing real crises, the more the actual competence may diverge from declared competence without detection
 *   - Post-Disaster Learning Capacity: Secondary victim (institutional/trapped but not agent) — when real crisis does occur after long gap, the organization's capacity to learn from it is constrained by isolation of crisis experience to small group and rapid return to simulation-based training rather than embodied organizational learning
 *   - Safety Modernization Coalition: Organized alternative (organized/constrained) — researchers and advanced technology developers see the simulation/real-crisis binary as resolvable through higher-fidelity alternatives (immersive synthetic environments, continuous operational microcrisis exposure, apprenticeship progression) with plausible sunset within 20–30 years
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_memory_decay, 0.35).
domain_priors:suppression_score(institutional_memory_decay, 0.48).
domain_priors:theater_ratio(institutional_memory_decay, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_memory_decay, extractiveness, 0.35).
narrative_ontology:constraint_metric(institutional_memory_decay, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(institutional_memory_decay, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_memory_decay, tangled_rope).
narrative_ontology:human_readable(institutional_memory_decay, "Institutional Memory Decay in Safety-Critical Industries").
narrative_ontology:topic_domain(institutional_memory_decay, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(institutional_memory_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_memory_decay, crisis_response_organizations).
narrative_ontology:constraint_beneficiary(institutional_memory_decay, regulatory_authority).
narrative_ontology:constraint_beneficiary(institutional_memory_decay, insurance_pools).
narrative_ontology:constraint_victim(institutional_memory_decay, frontline_operators).
narrative_ontology:constraint_victim(institutional_memory_decay, safety_culture_integrity).
narrative_ontology:constraint_victim(institutional_memory_decay, post_disaster_learning_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE OPERATOR (SNARE) — Trapped in a constraint where competence is maintained through simulation, but careers depend on never needing to demonstrate that competence in real crisis. If the organization's memory is accurate, the operator succeeds by never being tested. If the memory has decayed, the first real crisis reveals catastrophic training gaps. No exit: cannot refuse to participate in mandatory simulations, cannot demand real-world testing, cannot accumulate biographical crisis experience without bearing full risk of catastrophic error. Experiences maximum extraction: bears full cost of institutional memory failure while having no say in how memory is maintained.
constraint_indexing:constraint_classification(institutional_memory_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CRISIS RESPONSE ORGANIZATION (TANGLED ROPE) — Coordinates genuine safety function (preparation, drill infrastructure, collective competence maintenance) while simultaneously extracting from the constraint: organizations benefit from being able to declare readiness through low-cost simulation rather than bearing the full cost of real-world testing or accepting genuine operational crisis. The organization both solves a real coordination problem (how do we stay ready without constant real catastrophes?) and extracts (how do we minimize the cost of staying ready while maintaining the credible appearance of competence?). Requires active enforcement: mandatory drills, regulatory compliance, documented procedures. Constrained exit: could theoretically switch to alternative competence models (continuous low-level operations testing, apprenticeship-based progression) but faces path-dependency in training infrastructure and regulatory expectations.
constraint_indexing:constraint_classification(institutional_memory_decay, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Benefits from the simulation-based competence maintenance system without bearing the performance risk. Regulator's job is to specify what safety competence looks like and require demonstration of it. Simulation satisfies the specification at lower cost than real-crisis testing. The regulator has arbitrage exit: could demand different proof (operational testing, apprenticeship progression, continuous low-level crisis simulation) but has no incentive to do so — simulation-based certification is cheaper to verify and creates lower political risk. The constraint solves the genuine coordination problem that regulators face: how to ensure safety readiness while avoiding the moral hazard of creating incentives for real disasters. Net beneficiary because the system lets the regulator declare safety achieved while keeping catastrophe risk out of the regulatory domain.
constraint_indexing:constraint_classification(institutional_memory_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SIMULATION INDUSTRY (PITON) — Maintains elaborate simulation infrastructure whose primary function (testing competence) has atrophied into secondary maintenance of institutional memory, while its actual role (generating credibility theater through certifiable exercise) has expanded. The industry benefits from the constraint but its activity is increasingly performative: simulations have become more elaborate and expensive while their diagnostic value has potentially decreased (if actual crises reveal competence gaps despite passing simulations, then simulation theater has decoupled from real competence). High theater_ratio because the simulation ritual is maintained through regulatory requirement and organizational inertia, not because the simulations are failing diagnostically — but the atrophied function (genuine competence testing) is now maintained by theatrical performance (the simulations happen, records are kept, certifications are issued) rather than by real function. Would persist because regulators mandate it and organizations resist switching, even if the competence-maintenance function has eroded.
constraint_indexing:constraint_classification(institutional_memory_decay, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — The civilizational/universal perspective risks framing institutional memory decay as an immutable law of organizational behavior: humans naturally forget, competence atrophies without repeated practice, and the gap between simulation and real crisis is inherent to catastrophe-avoidance disciplines. This perspective sees the constraint as emerging naturally from cognitive and organizational limits. However, the engine's false summit detection will flag this as naturalization of a constructed arrangement: the gap between simulation and real competence is contingent on institutional choices (how much realism do we engineer into simulation? how much low-level operational testing do we embed? how do we rotate personnel?), not an inevitable feature of human cognition.
constraint_indexing:constraint_classification(institutional_memory_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SAFETY MODERNIZATION COALITION (SCAFFOLD) — Organized actors (safety researchers, some regulatory advocates, advanced simulation technologists) see the simulation-based competence model as temporary and obsolete. The coalition is building higher-fidelity alternatives: synthetic immersive environments, continuous operational microcrisis exposure (controlled low-level incidents that test response without catastrophic risk), apprenticeship-based progression where junior operators experience graduated real-world decision-making, and machine-learning-based crew pairing that surfaces weak competence through algorithmic flagging. The sunset is plausible — within 20–30 years, technical capabilities may enable competence maintenance through mechanisms that don't rely on the simulation/real-crisis binary. Current constraint persists because infrastructure investment in traditional simulation is sunk and regulatory frameworks are optimized for current practice, but the organized actors see an exit path.
constraint_indexing:constraint_classification(institutional_memory_decay, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_memory_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_memory_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_memory_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_memory_decay, TR),
    TR >= 0.70.

:- end_tests(institutional_memory_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts through two mechanisms: (1) organizations benefit from simulation-based certification (they satisfy regulators at lower cost than real testing), and (2) the gap between simulation and real competence creates a vulnerability that only the organization can detect and remediate, meaning the organization has private information about competence adequacy that regulators cannot verify. The extraction is not severe because the simulation system genuinely does maintain substantial safety competence — most crises are prevented, and the simulation-trained operators do respond effectively in real crisis (though possibly with discoverable gaps). The 0.35 value reflects moderate rather than high extraction: coordination function is real, beneficiaries do contribute to the system, victims are not starved of resources. Suppression (0.48): Moderate-high. Frontline operators cannot opt out of participation in simulation (regulatory requirement), cannot demand real-world testing (regulatory regime doesn't enable it), and cannot accumulate biographical crisis experience without catastrophic risk. The suppression is not total because alternative exit paths exist (career change, transfer to non-safety-critical roles, advocacy for regulatory change) but they are costly. Theater ratio (0.64): High and rising. Simulations have become increasingly elaborate, documented, and ceremonial over the interval. Modern full-mission simulations involve state-of-the-art motion platforms, networked synthetic environments, and multiple simultaneous crews. The ratio has increased from 0.42 to 0.64 over the interval because the theatrical production value of the exercises has grown faster than evidence of their diagnostic value — when no major crisis occurs for decades, the theatrical presentation of readiness (the simulation ritual) becomes harder to distinguish from actual readiness. If simulation theater is working perfectly as a memory-maintenance mechanism, theater_ratio rising should not be alarming. But the rising ratio suggests that the performance (simulation is conducted, operators pass, certifications are issued) has become decoupled from verification of the underlying competence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits perspectival divergence across power positions. Frontline operators see snare: they are trapped in a system where competence is declared through abstract exercise but where the first real test might reveal catastrophic gaps. Crisis response organizations see tangled rope: they genuinely coordinate safety preparation while also extracting benefit from simulation-based certification (lower cost, reduced political risk). The regulatory authority sees rope: the system solves the genuine coordination problem of maintaining readiness without incentivizing real crises, and the regulator benefits from low-cost verification without bearing performance risk. The simulation industry sees itself maintaining legitimate infrastructure (piton with functional rationale) but the engine's analysis may reveal it as piton with atrophied function — the theatrical performance of simulations has expanded while their role in genuine competence maintenance has become unverifiable. The civilizational analytical observer risks seeing mountain (memory decay as inevitable law of organizational behavior) but the structural data reveals this as false summit: the gap between simulation and real competence is contingent on institutional choices (what realism is engineered into simulation? how much low-level operational testing is embedded? how is personnel rotation managed?), not inherent to human cognition or organization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim structure and exit options. Frontline operators: trapped exit + victim status → high d → high experienced extraction chi. Crisis response organization: constrained exit + beneficiary status (benefits from low-cost certification) but also victim status (bears responsibility for competence adequacy) → medium d with asymmetric components → medium chi with mixed beneficiary/victim aspects justifying tangled rope classification. Regulatory authority: arbitrage exit (could demand different proof) + beneficiary status (benefits from system that satisfies them cheaply) → low d → negative or low chi (net beneficiary). Simulation industry: arbitrage exit (could transition to alternative approaches) + beneficiary status (contracts, regulatory mandates) → low d → negative or low chi. The tangled rope classification of the crisis response organization reflects that d varies within the agent across different aspects: they benefit from the current regime (beneficiary directionality) while also bearing the structural risk (victim directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to maintain crisis-response competence across periods without real crises. The functional purpose — preventing competence decay that would cause catastrophic failure in rare-event response — remains live. But the mandate has drifted into secondary purposes: (1) satisfying regulatory requirements through documented exercises (procedural compliance), (2) maintaining simulation industry contracts (institutional inertia), and (3) generating organizational credibility theater (demonstrating preparedness to stakeholders). The primary mandate has not been abandoned, but it has been layered with extractive secondary mandates. Mandatrophy is partially resolved through the tangled rope classification: the constraint simultaneously coordinates genuine safety function and enables extraction through the gap between simulation and real competence. Complete mandatrophy resolution would require either (a) demonstrating that the gap between simulation and real competence is negligible (omega variable 1 and 3), or (b) shifting to alternative competence models that don't rely on the simulation/real-crisis binary (the scaffold perspective).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency,
    'At what level of simulation fidelity does training transfer genuinely prevent catastrophic error in real crisis, and how would we detect degradation below that threshold?',
    'Post-incident analysis of actual crises: do the operators'' first actions in real crises match simulation training? Are gap patterns consistent across multiple incidents or incident-specific? Comparison of operators trained under high-fidelity vs lower-fidelity simulation when both encounter real crisis.',
    'If fidelity threshold is lower than current practice: simulation is over-engineered and constraint is primarily theatrical (piton classification confirmed). If fidelity threshold is higher: simulation-based competence is structurally insufficient and the snare classification (operators are trapped in inadequate training) is confirmed. If threshold varies by crisis type: simulation fidelity is not a single scalar and decomposition into constraint families (per-disaster-type) is required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency, empirical, 'Simulation fidelity sufficiency for real-crisis competence transfer').

omega_variable(
    memory_loss_mechanism,
    'What is the primary mechanism through which institutional memory decays — cognitive forgetting by individuals, turnover/loss of personnel, organizational procedures that degrade between use, or something else?',
    'Longitudinal studies tracking specific operational competencies across crisis gaps. Compare organizations with high personnel turnover vs low turnover under identical simulation regimes. Compare competence loss in organizations that maintain active low-level operations (continuous small-scale exercises) vs those that rely solely on large staged drills.',
    'If cognitive forgetting dominates: the constraint is about human limits and simulation can plausibly maintain competence. If turnover dominates: the constraint is about knowledge transfer systems and could be addressed through mentorship, documentation, or organizational redesign. If procedure degradation dominates: the constraint is about institutional maintenance and could be addressed through continuous testing of procedures. Each diagnosis points to different remediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_loss_mechanism, empirical, 'Primary mechanism of institutional memory decay').

omega_variable(
    simulation_versus_lived_kernel,
    'Is the contested kernel here ''what constitutes genuine exercise of competence'' — does simulation count as exercising the competence or only lived crisis?',
    'Conceptual analysis with empirical anchors. When an operator undergoes simulation training and passes, what specific competences have been exercised? Can the same competences be exercised in simulation as in real crisis? What is the structure of the difference? Empirical anchor: do operators who never experience real crisis but pass high-fidelity simulations perform identically to operators who have experienced real crisis, when both face a novel crisis type?',
    'If simulation exercises the same competences: the constraint''s classification shifts from snare (operators are trapped) toward rope (simulation genuinely coordinates competence maintenance). If simulation exercises different competences: the constraint remains snare (operators are trapped in a system that claims to test competence it cannot actually test). If simulation exercises partial competences: the constraint is clearly tangled rope (coordination + extraction through underestimation of what simulation omits).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_versus_lived_kernel, conceptual, 'Contested kernel: does simulation count as genuine exercise of competence?').

omega_variable(
    post_incident_advantage,
    'Do organizations that have experienced a major crisis retain superior competence and institutional memory compared to peer organizations that have not, controlling for training and simulation investment?',
    'Comparative analysis of safety incident rates and response quality across organizations with similar simulation regimes but different crisis history. Did organizations that experienced a crisis decade ago show faster, more effective response to a similar crisis that emerged elsewhere? Do they retain competence advantages in areas directly touched by the past crisis?',
    'If yes (crisis experience confers ongoing advantage): lived crisis is a genuinely different competence mechanism than simulation, suggesting the constraint is snare (operators can''t access the superior competence) or tangled rope (organizations coordinate safety while extracting the advantage of having experienced catastrophe). If no (crisis experience provides no lasting advantage): simulation-based maintenance is sufficient and the constraint is primarily rope or piton (coordination mechanism that has either atrophied or become theatrical). If mixed (advantage persists for 5-10 years then decays): the decay mechanism is the constraint, not the lack of crisis experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_incident_advantage, empirical, 'Whether crisis experience confers lasting competence advantage').

omega_variable(
    false_summit_naturalization,
    'Is the mountain classification (institutional memory decay as inevitable law of organizational behavior) naturalizing what is actually a contingent choice to rely on simulation rather than alternative competence models?',
    'Historical comparison: organizations that have implemented alternatives to simulation (continuous operational testing, apprenticeship progression, algorithmic pairing, etc.). Do they experience memory decay at different rates? Can institutions engineer around the supposed natural law? Survey regulatory history: was simulation chosen as the optimal mechanism or as the politically acceptable compromise?',
    'If alternatives work: mountain classification is false summit (naturalization of institutional choice). If alternatives fail: mountain classification is valid (decay is inherent to competence domains without recurring real crisis). If some alternatives work in some contexts: constraint decomposes into families (per-competence-domain stories with different epsilon values).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'False summit detection: is memory decay natural law or contingent institutional choice?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_memory_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imd_theater_baseline, institutional_memory_decay, theater_ratio, 0, 0.42).
narrative_ontology:measurement(imd_theater_midpoint, institutional_memory_decay, theater_ratio, 5, 0.55).
narrative_ontology:measurement(imd_theater_recent, institutional_memory_decay, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(imd_extraction_baseline, institutional_memory_decay, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(imd_extraction_midpoint, institutional_memory_decay, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(imd_extraction_recent, institutional_memory_decay, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(imd_suppression_baseline, institutional_memory_decay, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(imd_suppression_midpoint, institutional_memory_decay, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(imd_suppression_recent, institutional_memory_decay, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_memory_decay, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_memory_decay, regulatory_capture_in_safety).
narrative_ontology:affects_constraint(institutional_memory_decay, knowledge_transfer_across_generations).
narrative_ontology:affects_constraint(institutional_memory_decay, catastrophe_prevention_paradox).

% DUAL FORMULATION NOTE:
% Institutional memory decay is one reading of a broader constraint family about how organizations maintain competence for rare, catastrophic events. The broader family includes: (1) knowledge_transfer_across_generations (how does specific crisis-response competence transfer when the crisis doesn't occur often enough for natural apprenticeship?), (2) catastrophe_prevention_paradox (success at prevention removes the training mechanism), and (3) regulatory_capture_in_safety (regulators depend on industry to define what safe practice looks like, including what counts as adequate simulation). Each story has different epsilon values: knowledge transfer is primarily coordination (rope), catastrophe prevention paradox is primarily the structural constraint (tangled rope), regulatory capture is primarily extraction (snare). Institutional memory decay focuses on the simulation/real-crisis gap as the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_memory_decay, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
