% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Workers' Councils as Direct Democratic Revolutionary Organs
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the council-communist reading of
 *   revolutionary transformation: workers organize production through
 *   federated democratic assemblies (soviets) that replace both capitalist
 *   state apparatus and vanguard party hierarchy. Authority flows from
 *   workplace assemblies upward through delegates held accountable to the
 *   base, not downward from a central planning bureau or political party.
 *   Within councils, extraction is minimal (ε≈0.25) because surplus goes back
 *   to workers themselves and decisions remain democratic. However, the
 *   reading itself faces high external suppression (suppression≈0.72) from
 *   rival readings and institutional incumbents: capitalists defend private
 *   ownership, state bureaucrats defend state hierarchy, vanguard parties
 *   defend party leadership. The constraint is this decentralized,
 *   democratic, council-based reading itself — the structural arrangement it
 *   envisions — not the reading's defensive position against rivals.
 *
 * KEY AGENTS:
 *   - Autonomous worker collectives: workplace assemblies holding direct democratic control over production and resource allocation
 *   - Federated workplace assemblies: nested coordination structure preserving local autonomy while enabling inter-enterprise planning
 *   - State bureaucratic apparatus: institutional power structure that councils would supersede
 *   - Revolutionary vanguard party: organized revolutionary cadre claiming exclusive access to revolutionary theory — structure councils would dissolve
 *   - Capitalist proprietors: owners of means of production whose private ownership is abolished under the reading
 *   - Individual worker participants: powerless agents gaining voice and agency through participation in democratic assemblies
 *   - Democratic gradualist advocates: excluded from this reading's framework as defenders of electoral reform and capitalist institutional forms
 *   - Analytical observer: position examining the reading as a commitment-system instantiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.72).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Councils as Direct Democratic Revolutionary Organs").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '84098e0c-5f43-4942-8bab-4aa69a1ab946').
narrative_ontology:cs_kernel_codification('84098e0c-5f43-4942-8bab-4aa69a1ab946', distributed).
narrative_ontology:cs_authority_grounding('84098e0c-5f43-4942-8bab-4aa69a1ab946', practice).
narrative_ontology:cs_interpretation_layer_present('84098e0c-5f43-4942-8bab-4aa69a1ab946').
narrative_ontology:cs_reading_relation('84098e0c-5f43-4942-8bab-4aa69a1ab946', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('84098e0c-5f43-4942-8bab-4aa69a1ab946', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('84098e0c-5f43-4942-8bab-4aa69a1ab946', foundational, direct_democracy_mandatory).
narrative_ontology:cs_axiom_status(direct_democracy_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('84098e0c-5f43-4942-8bab-4aa69a1ab946', direct_democracy_mandatory, deontological).
narrative_ontology:cs_axiom('84098e0c-5f43-4942-8bab-4aa69a1ab946', foundational, decentralization_prevents_bureaucracy).
narrative_ontology:cs_axiom_status(decentralization_prevents_bureaucracy, holdable).
narrative_ontology:cs_axiom_grounding('84098e0c-5f43-4942-8bab-4aa69a1ab946', decentralization_prevents_bureaucracy, empirically_contingent).
narrative_ontology:cs_reference_frame('84098e0c-5f43-4942-8bab-4aa69a1ab946', federated_direct_democracy).
narrative_ontology:cs_drift_state('84098e0c-5f43-4942-8bab-4aa69a1ab946', contemporary_post_industrial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84098e0c-5f43-4942-8bab-4aa69a1ab946', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, individual_worker_participant).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucratic_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, revolutionary_vanguard_party).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_proprietors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly control production decisions and coordinate resource allocation through participatory assembly in their workplaces. Under this reading, workers own the means of production collectively and make decisions by democratic vote rather than hierarchical command. They are beneficiaries of the constraint insofar as it establishes their authority; they are agenda-setters insofar as they define the councils' mandate themselves.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% Coordinate production across workplaces through delegates accountable to their home assemblies, creating nested federation rather than centralized bureaucracy. Benefit from the constraint's decentralization principle: delegates serve the collective will and remain subject to recall. The federation structure preserves local autonomy while enabling inter-workplace coordination.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies, beneficiary,
    organized, generational, mobile, regional).

% Under this reading, the state apparatus (including representative electoral systems, police, courts, taxation machinery) would be superseded by council coordination. Bureaucrats whose power derives from state hierarchy face elimination or radical demotion to administrative service roles accountable directly to worker assemblies. Their institutional power is the extraction cost: the constraint requires the subordination or dissolution of state authority structures.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucratic_apparatus, payer,
    institutional, generational, trapped, national).

% Under this reading, a centralized vanguard party (organized cadre with exclusive access to revolutionary theory) is unnecessary and incompatible with direct democracy. Party officials face elimination or dissolution into the general assembly; their claim to monopolize revolutionary consciousness is rejected. The constraint requires the subordination or dissolution of party authority structures alongside state structures.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, revolutionary_vanguard_party, payer,
    institutional, generational, trapped, national).

% Private ownership of means of production is abolished; proprietors lose their claim to profit and capital accumulation. The constraint redistributes productive capacity from individual owners to federated collectives. Proprietors cannot exit without surrendering ownership; their exit option is foreclosed by the reading's core premise.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_proprietors, payer,
    powerful, biographical, trapped, global).

% Participates in workplace assembly decisions with equal voice and voting rights, regardless of technical skill or social standing. Benefits from democratic say in working conditions, production targets, and resource distribution. Constrained exit: can leave the workplace but remains embedded in the federated structure; cannot opt out of democratic participation within their workplace.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, individual_worker_participant, beneficiary,
    powerless, biographical, constrained, local).

% Excluded from this reading's framework: they advocate working within electoral institutions and gradual reform rather than council-based direct democracy and rupture. Under the council-communist reading, their gradualism is seen as prolonging capitalist rule; they are kept out of the canonical interpretation of revolutionary transformation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, democratic_gradualist_advocates, excluded,
    organized, generational, constrained, national).

% Examines the reading as a commitment-system instantiation: how councils ground legitimacy in direct democracy principle; how the reading's authority structure differs from vanguard and gradualist alternatives; what mechanisms would maintain council coordination under challenge.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, analytical_observer_position, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production and resource allocation across workplaces without market pricing or state planning authority. Workers in each enterprise make decisions democratically; delegates communicate across workplaces to resolve dependencies and coordinate complex production. The problem solved: how to allocate resources and organize large-scale production after abolishing both capitalist profit-seeking and hierarchical party command.
% TRANSFER_FUNCTION: Transfers productive capacity from private capitalist ownership and state-controlled bureaucracy to federated assemblies of workers. Transfers authority over working conditions, production targets, and resource distribution from external proprietors and managers to democratic worker collectives. No surplus extraction to a separate beneficiary class — the constraint dissolves the extraction relationship itself by making workers the owners.
% ABSENT_VOICES: Capitalist proprietors are excluded structurally — their voice would defend private ownership and profit, which the constraint rejects as the problem to be solved. Vanguard party officials are excluded — their hierarchical claim to revolutionary leadership contradicts direct democracy. Democratic gradualists are excluded — their defense of electoral reform is seen as compromise with capitalist institutional forms. These are structural exclusions, not accidental omissions.
% DISAPPEARANCE_RATIONALE: If councils were abolished, production would reorganize either under state central planning (vanguard path) or capitalist markets with electoral representative government (gradualist path). The worker assemblies themselves would dissolve; workplace democracy would revert to management hierarchy. The reading's disappearance would trigger a fundamental reorganization of economic and political authority.
% FOUNDING_PROBLEM: Under capitalism, workers lack control over their own productive activity and are subordinated to capital accumulation. Under existing vanguard party systems, workers lack control over revolutionary decision-making and are subordinated to party bureaucracy. The founding problem is: how to organize production and society such that workers themselves hold power and make decisions collectively, without recreating either capitalist or bureaucratic authority.
% FOUNDING_PROBLEM_CORROBORATION: Worker self-organization and council formation have occurred repeatedly in revolutionary moments (Paris Commune, Russian soviets, Hungarian councils, Yugoslav workers' councils). Workers themselves attest to the problem through their repeated attempts at self-governance. Independent historians and revolutionary theorists outside the sphere of vanguard parties (Pannekoek, Gorter, Castoriadis) corroborate that the founding problem persists: workers continue to lack genuine control over production and remain subordinated to capital or party. The corroboration is strongest among autonomous worker movements and council-communist theorists; vanguard parties dispute the problem's formulation and offer instead the party-guided path.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the reading envisions elimination of extraction itself: workers own means of production collectively and distribute surplus democratically rather than surrendering it to proprietors or party elite. No concentrated beneficiary extracts rent. Suppression rises from 0.55 to 0.72 across the interval because the constraint faces intensifying opposition from vanguard parties (claiming councils are insufficiently organized), capitalist states (defending private property), and institutional inertia. Theater is low (0.10–0.18) early on because councils are genuinely democratic decision-making structures with minimal performative overhead. Theater rises slightly mid-interval (0.18 at t=24) as the constraint must defend itself against rival readings' claims that direct democracy is infeasible at scale — some councils develop ritual aspects affirming democratic legitimacy while facing practical coordination challenges. The measurement series track one shared grid: every metric authored at every time point, modeling an interval of 40 years post-founding where suppression intensifies but internal extractiveness remains stable because councils maintain their federated democratic structure. Basis is 'projected' throughout: this reading has not been fully instantiated in any contemporary society; the measurements represent the reading's own self-projection of how councils would operate under sustained pressure.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (worker collectives, individual participants) and the payer seats (state, party, capitalists) compute fundamentally different classifications from the same constraint. From the worker assembly position, the constraint is pure coordination: they solve the production-and-democracy problem directly and benefit equally. From the state bureaucrat position, the constraint is pure extraction: their institutional power is stripped away and subordinated to worker control. From the capitalist position, it is also pure extraction: private ownership is abolished. From the analytical position, the constraint is Rope (genuine coordination with active governance) for participants inside councils, but Snare-from-outside: rival readings (vanguard, gradualist, capitalist) treat councils as naive or threatening and suppress them with institutional and ideological force. The engine computes all seats' types from the structural data; the authored claim (Rope) reflects the reading's own self-understanding, while metrics reflect the actual operational tensions and external pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Worker collectives and assemblies are the structural beneficiaries: they hold power, make decisions, and retain surplus (d near 0.0–0.2, full beneficiary end). Individual workers are also beneficiaries but slightly higher d (0.3–0.4) because their voice is constrained by assembly majority. State bureaucrats, party officials, and capitalists are structural targets (d near 0.8–1.0, full target end) because their institutional power and extraction mechanisms are abolished. Democratic gradualists are payers in the sense that the reading rejects their institutional path and demands rupture instead; they have constrained exit (d≈0.6–0.7) because they can advocate within electoral systems but are excluded from this reading's canonical framework. The derivation is stable across the interval: the reading does not shift its beneficiary or victim structure; the shift is in how much suppression those readings face from external rivals.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is LIVE and the constraint's disappearance verdict is WORLD_REARRANGES: the problem (workers lack control over production) persists as long as capitalism or party bureaucracy dominates. This is NOT mandatrophy; the constraint's founding mandate (direct democratic worker control) remains its operational imperative. However, an omega addresses the risk of mandate drift: if councils begin to calcify into their own bureaucracy (delegateship becoming a career, assemblies becoming ritual, coordination becoming planning), then the functional mandate (democratic decision-making) would atrophy while the structural form (councils) persisted. This is the 'Piton' risk within the reading itself: councils that lose genuine democracy but retain council form. The constraint-as-authored avoids this by specifying that the mandate IS democratic participation and accountability, not council existence per se.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_bureaucracy_risk,
    'Will councils inevitably develop their own bureaucratic hierarchy as they coordinate production at scale, replicating the problem they were meant to solve?',
    'Longitudinal observation of existing worker-council structures (Yugoslav workers'' councils, Argentina factory cooperatives, contemporary horizontal organizations): do councils maintain democratic decision-making over decades, or do delegateship and administrative roles calcify into managerial hierarchy?',
    'If councils invariably bureaucratize, the constraint''s long-term viability (particularly at national/global scale) is compromised and the reading becomes a temporary Scaffold rather than a stable Rope. If councils can maintain democratic accountability through rotation, recall, and transparency, the reading stands as a genuine coordination structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_bureaucracy_risk, empirical, 'Whether council democracy can persist at scale without bureaucratic ossification.').

omega_variable(
    complex_coordination_feasibility,
    'Can federated councils coordinate complex, interdependent production (semiconductors, pharmaceuticals, infrastructure) without either market pricing signals or central planning authority?',
    'Analysis of coordination mechanisms (labor accounting, input-output planning, negotiation protocols) and empirical test sites (intentional economies using council-like structures); comparison with market and planning outcomes on speed, efficiency, and worker satisfaction.',
    'If councils cannot coordinate complexity above some threshold (e.g., national manufacturing), the constraint''s spatial scope is limited and rivals'' claims about councils'' naiveté gain credibility. If councils can coordinate complex production, the reading''s viability extends to the global scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complex_coordination_feasibility, empirical, 'Whether councils can coordinate production without market or planning mechanisms.').

omega_variable(
    reading_vs_rivals_logical_status,
    'Do the council-communist, vanguard, and gradualist readings logically foreclose each other, or do they coexist as distinct strategic choices held by different actors?',
    'Conceptual analysis: Does accepting direct democracy logically forbid accepting temporary party leadership? Does accepting gradualism logically forbid accepting councils? The test is whether any reading''s core premise directly contradicts another''s such that no single party could hold both.',
    'If readings foreclose each other, the constraint faces a binary choice world where only one can ultimately prevail (high external suppression justified by logical incompatibility). If they coexist, suppression is contingent on power struggles, not logical necessity, and all three readings can remain live positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_rivals_logical_status, conceptual, 'Logical status of coexistence vs. foreclosure between reading strategies.').

omega_variable(
    council_vanguard_compatibility,
    'Is there a hybrid structure where a revolutionary vanguard party coordinates WITH councils rather than OVER them — party members as facilitators and educators rather than commanders?',
    'Conceptual and historical: Could a party devoted to democratic centralism operate as an educating force within councils rather than imposing discipline? Did any revolutionary moment attempt this? What prevented it?',
    'If vanguard parties can operate in service to councils (rather than commanding them), the reading might accommodate some party structure without abandoning democratic premise, reducing the external suppression from vanguard actors. If vanguard requires command authority, the readings are foreclosed and opposition is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_vanguard_compatibility, conceptual, 'Whether vanguard and council structures are logically compatible under different forms.').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the measured suppression (0.72) arising from institutional actors'' defense of their power (structural) or from ideological disagreement about the reading''s feasibility (epistemic)?',
    'Post-suppression trajectory: if suppression decreases when vanguard actors lose institutional power or capitalist states collapse, it was structural. If suppression persists among workers and councils themselves (debate about whether direct democracy works), it is ideological.',
    'If structural, the constraint''s long-term suppression depends on the fate of rival institutional powers. If ideological, the constraint faces perpetual internal doubt even without external opposition, and theater will remain elevated as councils defend their democratic legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, empirical, 'Whether suppression is structural institutional opposition or internalized ideological doubt.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(council_communist_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(council_communist_tr_t8, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(council_communist_tr_t16, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(council_communist_tr_t24, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(council_communist_tr_t32, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(council_communist_tr_t40, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(council_communist_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(council_communist_be_t8, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(council_communist_be_t16, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(council_communist_be_t24, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(council_communist_be_t32, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 32, 0.25).
narrative_ontology:measurement(council_communist_be_t40, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 40, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(council_communist_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(council_communist_su_t8, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(council_communist_su_t16, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(council_communist_su_t24, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(council_communist_su_t32, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(council_communist_su_t40, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'manifesto_revolutionary_method'. The council-communist reading specifies decentralized direct democracy through federated worker assemblies. The vanguard_rupture_reading specifies organized party seizure of state power and dictatorship of proletariat. The democratic_gradualism_reading specifies electoral socialist transformation through existing democratic institutions. Each reading instantiates a different constraint with different ε, different beneficiaries/victims, and different suppression mechanics. They coexist as live strategic positions in revolutionary debate; they are not logically foreclosed of each other, but they profoundly influence each other through dispute over the correct revolutionary method. This constraint affects the other two by offering an alternative that claims to avoid both vanguard hierarchy and capitalist electoral constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
