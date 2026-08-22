% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure and Dictatorship of Proletariat (Revolutionary Rupture Reading)
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates the vanguard rupture reading of the
 *   contested kernel 'manifesto_revolutionary_method': the claim that
 *   revolutionary transformation of capitalism requires organized party
 *   seizure of state power and a transitional dictatorship of the proletariat
 *   under party guidance. This reading stands against democratic gradualism
 *   (achieving socialism through electoral majorities and institutional
 *   reform) and council communism (direct workers' democracy through
 *   federated councils replacing both capitalist state and vanguard party).
 *   The vanguard reading authorizes high state centralization, subordination
 *   of autonomous worker organizations to party-controlled structures,
 *   systematic suppression of competing socialist tendencies, and perpetual
 *   emergency powers justified by counter-revolutionary threats. The
 *   measurement series traces how extractiveness plateaus after ~25 years
 *   (the dictatorship fails to wither and calcifies into permanent state
 *   apparatus) while suppression requirement stabilizes at high levels
 *   (emergency powers become routine, not temporary). Theater ratio rises
 *   early then stabilizes, indicating the transition from revolutionary
 *   action to perpetuation of structures justified by revolutionary rhetoric.
 *
 * KEY AGENTS:
 *   - party_central_committee: agenda-setter, institutional power, establishes and directs the revolutionary transformation
 *   - state_planning_apparatus: beneficiary with institutional power, monopolizes economic coordination
 *   - revolutionary_security_apparatus: beneficiary with powerful enforcement capacity, eliminates counter-revolutionary activity
 *   - revolutionary_cadre: beneficiary with identity_locked exit, fused to party mission
 *   - industrial_working_class: nominal beneficiary but actual payer, trapped, subjected to plan discipline and labor conscription
 *   - political_pluralists: payer, moderate power, systematically excluded from political power
 *   - autonomous_worker_organizations: payer, powerless, progressively subordinated to party control
 *   - competing_left_tendencies: payer, moderate power, eliminated from revolutionary process
 *   - international_observer: analytical seat, assesses whether constraint functions as advertised transitional form or as new domination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.79).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure and Dictatorship of Proletariat (Revolutionary Rupture Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '063a01c7-890e-47db-bba3-5fa8a755af69').
narrative_ontology:cs_kernel_codification('063a01c7-890e-47db-bba3-5fa8a755af69', fixed_text).
narrative_ontology:cs_authority_grounding('063a01c7-890e-47db-bba3-5fa8a755af69', lineage).
narrative_ontology:cs_interpretation_layer_present('063a01c7-890e-47db-bba3-5fa8a755af69').
narrative_ontology:cs_reading_relation('063a01c7-890e-47db-bba3-5fa8a755af69', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('063a01c7-890e-47db-bba3-5fa8a755af69', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('063a01c7-890e-47db-bba3-5fa8a755af69', foundational, revolutionary_rupture_necessary).
narrative_ontology:cs_axiom_status(revolutionary_rupture_necessary, holdable).
narrative_ontology:cs_axiom_grounding('063a01c7-890e-47db-bba3-5fa8a755af69', revolutionary_rupture_necessary, empirically_contingent).
narrative_ontology:cs_axiom('063a01c7-890e-47db-bba3-5fa8a755af69', foundational, vanguard_party_sole_capable_leadership).
narrative_ontology:cs_axiom_status(vanguard_party_sole_capable_leadership, holdable).
narrative_ontology:cs_axiom_grounding('063a01c7-890e-47db-bba3-5fa8a755af69', vanguard_party_sole_capable_leadership, deontological).
narrative_ontology:cs_axiom('063a01c7-890e-47db-bba3-5fa8a755af69', secondary, dictatorship_proletariat_transitional_form).
narrative_ontology:cs_axiom_status(dictatorship_proletariat_transitional_form, overridden).
narrative_ontology:cs_axiom_grounding('063a01c7-890e-47db-bba3-5fa8a755af69', dictatorship_proletariat_transitional_form, empirically_contingent).
narrative_ontology:cs_reference_frame('063a01c7-890e-47db-bba3-5fa8a755af69', pre_revolutionary_capitalist_epoch).
narrative_ontology:cs_drift_state('063a01c7-890e-47db-bba3-5fa8a755af69', post_consolidation_zombie_state, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('063a01c7-890e-47db-bba3-5fa8a755af69', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_security_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, competing_left_tendencies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, bourgeois_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_cadre).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leads the revolutionary seizure of state power, establishes the dictatorship of the proletariat as a transitional state form, and directs all state apparatus toward the elimination of class distinctions. Sets policy for the transition period, controls cadre selection, and determines the pace and character of the revolutionary transformation. Justifies centralized party authority as necessary to prevent counter-revolution and accelerate historical development toward classless society.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers the command economy, allocates productive resources according to state plan, and carries out central direction from party committee. Accumulates institutional power and decision-making authority. Benefits from the monopoly on economic coordination and the elimination of market competition. Carries out enforcement against those who violate plan directives.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, agenda_setter).

% Secret police, intelligence services, and state security organs tasked with eliminating counter-revolutionary activity, suppressing autonomous organization, and maintaining internal security. Accumulates institutional power through the expanded definition of 'counter-revolutionary' activity. Benefits from emergency powers and exemption from general law to execute state security functions.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_security_apparatus, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_security_apparatus, agenda_setter).

% Party members who carry out the revolution and staff the new state apparatus. Receive privileged access to resources, authority to direct others, and status as architects of the new society. Identity is fused with the party and its historical mission; exit means loss of vocation and social position. Benefits from the party monopoly on political power and the organizational discipline that secures it.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_cadre, beneficiary,
    organized, biographical, identity_locked, national).

% Nominally the beneficiary on whose behalf the state is constituted (the 'dictatorship of the proletariat'). In practice, they bear the disciplines of forced collectivization, labor conscription, and plan fulfillment quotas. Their autonomous workplace organizations (councils, unions) are subordinated to party-controlled trade unions or dissolved entirely. They are told they are the ruling class while their actual power of decision is minimal.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class, payer).

% Socialist parties, liberal democrats, anarchists, and other left or center-left political tendencies that advocated alternative paths to transformation (gradualism, council democracy, federalism). Systematically excluded from power, their organizations banned, their press suppressed, their leaders imprisoned or executed. Treated as counter-revolutionary despite shared opposition to capitalism. Bear the costs of enforced political monopoly.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Workplace councils, factory committees, local soviets, and independent unions that emerged from revolutionary struggle. Initially presented as the organizational form of the dictatorship of the proletariat. Progressively subordinated to party-controlled unions, subjected to plan discipline from above, and stripped of autonomous decision-making authority. Their representatives replaced with party appointees.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    powerless, biographical, trapped, regional).

% Former property owners, industrial capitalists, merchant classes, and their allies. Face expropriation, dispossession, and often violence. Used as the public justification for emergency powers and secret police. While genuinely opposed to revolution, their constructed threat is amplified and perpetuated even after class basis is eliminated, enabling ongoing security apparatus expansion.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, bourgeois_opposition, payer,
    powerful, biographical, trapped, national).

% Bolshevik factions, Left Communists, Trotskyists, Left Socialist Revolutionaries, and other revolutionary currents within the working-class movement. Participate in the revolution but hold differing views on the proper form of transition, pace of collectivization, international strategy, and role of the party. Systematically eliminated from power, their organizations destroyed, their cadres purged or exiled.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, competing_left_tendencies, payer,
    moderate, biographical, trapped, regional).

% Socialist and communist organizations worldwide that might have advocated alternative revolutionary models or challenged the vanguard party's authority to speak for international socialism. Subordinated to the interests of the Soviet state and its party, their autonomy eliminated, their resources mobilized for state foreign policy rather than independent revolutionary action.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, international_worker_movement, excluded,
    organized, generational, constrained, global).

% Marxist and communist theorists, philosophers, and historians who interpret the revolutionary process and assess whether the vanguard party reading aligns with historical materialist theory. They provide the intellectual legitimation (or critique) of the constraint's theoretical foundation. Their analytical judgment determines whether the constraint is understood as necessary historical development or as a betrayal of revolutionary principle.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, historical_materialist_theorists, observer,
    analytical, generational, analytical, global).

% Later historians, political theorists, and empirical observers evaluating whether the vanguard seizure and dictatorship of the proletariat functioned as a transitional form toward socialism (as claimed) or calcified into a new form of domination (as critics argue). Their assessment depends on whether the transition successfully moved toward the withering away of the state and class elimination, or whether state apparatus and party elite became permanent structures of power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, post_revolutionary_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: Revolutionary transformation of capitalism cannot be achieved through democratic electoral reform or gradual institutional change because the capitalist state is an instrument of bourgeois class rule and will not voluntarily relinquish power. Revolutionary rupture—the forcible seizure of state power by the organized working class under party leadership—is necessary to break the bourgeois state apparatus and direct the transition toward classless society. The dictatorship of the proletariat, exercised through the vanguard party, is a transitional state form that prevents counter-revolution and guides the development of socialist productive relations until classes are eliminated and the state withers away.
% FOUNDING_PROBLEM_CORROBORATION: Vanguard party theorists and Soviet-aligned states argue the founding problem remains live: capitalism continually threatens restoration, imperialist powers intervene, competing socialist tendencies sabotage the transition—permanent vigilance and party direction are necessary. Democratic socialists and evolutionary Marxists argue the problem was resolved by the 1950s: capitalism has been expropriated, class basis for counter-revolution is eliminated, party monopoly now serves elite interests rather than revolutionary necessity (attested in academic comparative politics, East European dissident movements, revisionist Marxist theory). Council communists argue the founding problem was mis-diagnosed from the start: the working class's own autonomous organizations (councils, committees) could have managed the transition without a vanguard monopoly—the claim is contested by historical counterfactuals (no actual council system has survived to test this rigorously). No single observer outside all benefiting parties has witnessed the full lifecycle of a post-revolutionary socialist state claimed to have reached communism and verified the withering of the state; therefore corroboration of the founding_problem_status is irreducibly contested.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.68 at interval end) reflects a high level of extraction from payer seats—political pluralists excluded from power, autonomous worker organizations subordinated, competing leftist tendencies eliminated. The founding claim is coordination (collective revolutionary action requires party discipline), but the structural asymmetry is severe: the party apparatus, security services, and planning bureaucracy accumulate power while the working class in whose name the state exists bears discipline and labor extraction. Suppression is high (0.79) because the constraint's persistence depends on active suppression of political alternatives (banned parties, eliminated councils, imprisoned dissidents), not voluntary participation. The plateau in extractiveness around year 25-30 signals Mandatrophy: the founding problem (overthrow of capitalism by party-led revolution) has been accomplished or resolved, but the emergency powers, party monopoly, and state apparatus persist and calcify. The rising theater_ratio (0.25 to 0.42) indicates growing disjunction between revolutionary rhetoric (the state serves the workers) and actual function (the state apparatus governs the workers). The one shared measurement grid ensures every metric is authored at every examined time point; the trajectory is not tuned to a predicted engine output but rather describes the empirical historical pattern: rapid consolidation of party power (years 0-15), plateau of extraction and suppression (years 15-40), rising rhetoric as action diminishes.
 *
 * PERSPECTIVAL GAP:
 *   The party central committee and state planning apparatus seats should compute very differently from the payer seats. From the agenda-setter seat, the constraint is genuine coordination—party discipline, central planning, and security apparatus are necessary to manage the transition and prevent capitalist restoration. From the payer seats (working class, political pluralists, autonomous organizations), the same structure is enforced extraction—the party monopoly is coercive, alternatives are suppressed, autonomy is denied, labor is conscripted. The engine will compute directionality asymmetrically: the agenda-setter has low d (benefits, controls exits, can arbitrage between revolutionary states), while payers have high d (bear costs, trapped, identity_locked in working class or revolutionary cadre roles). This divergence is the measurement the corpus takes—claimed tangled_rope coordination that computes as partially extractive asymmetry signals a constraint that redistributes power unequally even while achieving some genuine coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The party_central_committee and state_planning_apparatus are structural beneficiaries: they collect authority, set policy, control resource allocation, and face no exit pressure—their organizational survival depends on maintaining party monopoly. Revolutionary_cadre are also beneficiaries but identity_locked: their exit is psychologically and socially impossible because their entire identity is fused with the party and revolutionary mission. The industrial_working_class are nominal beneficiaries in the reading's framing (the dictatorship is 'of the proletariat'), but they are actual payers: they bear labor discipline, collectivization, and conscription; their autonomy is subordinated; their organizations are dismantled or absorbed; they face high suppression if they resist. Political_pluralists and competing_left_tendencies are pure payers: excluded, banned, imprisoned—they hold differing views on the proper revolutionary path but are given no seat at the table. The autonomous_worker_organizations are payers because they are progressively subordinated and then destroyed—replaced by party-controlled unions that serve state plan, not worker interest. This asymmetry is structural, not incidental: the constraint requires that beneficiaries retain monopoly control and that payers be excluded or subordinated to maintain the party's revolutionary authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status measurement is contested precisely because this constraint exhibits classic Mandatrophy dynamics. The founding problem (capitalism cannot be reformed, revolutionary rupture is necessary) is effectively solved by ~year 5-10: the capitalist class is expropriated, capitalist relations are eliminated, the state has seized control of productive means. Yet the dictatorship of the proletariat, presented as a TRANSITIONAL state form that should wither away, does not wither. Instead, it calcifies: suppression requirement plateaus at 0.79, theater_ratio stabilizes at 0.42, extractiveness stabilizes at 0.68. The vanguard reading attributes this continuation to live counter-revolutionary threats (bourgeois restoration, imperialist intervention, competing left ideologies). Critics argue the constraint persists because the party apparatus and planning bureaucracy have become a new ruling class with interests in perpetuating state monopoly—Mandatrophy resolved by elite capture. Post-revolutionary observers dispute the founding_problem_status: if the problem was really 'overthrow capitalism,' status should be 'dead' by year 10, and the persistence of the state form becomes unexplained—a flag for zombie constraint investigation. If the problem was 'prevent capitalist restoration and guide the transition to communism,' status could remain 'live' indefinitely, licensing perpetual emergency. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges indicates a zombie-constraint pathway: the world would rearrange if the constraint disappeared (suggesting it is not natural law), yet the founding problem's resolution or persistence is itself contested (the beneficiaries argue it remains live, the payers argue it was solved decades ago).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_withering_vs_calcification,
    'Is the persistence of the dictatorship of the proletariat beyond ~20-25 years a necessary response to live counter-revolutionary threats, or evidence that the state apparatus has calcified into a new form of domination that the party elite benefits from preserving?',
    'Empirical observation: (a) if post-transition states that claim to reach communism do in fact eliminate state apparatus and party monopoly, the ''transitional'' interpretation is supported; (b) if all such states maintain or intensify state centralization and party monopoly, the ''calcification'' interpretation is supported. Alternatively, detailed historical analysis of whether genuine counter-revolutionary threats persist, or whether threat-inflation serves elite interests.',
    'If calcification, the constraint transitions from tangled_rope (genuine coordination + asymmetric extraction during transition) to snare (the extraction becomes the primary function, coordination is the cover story). The classification changes fundamentally. If withering is possible, the constraint might remain tangled_rope with an extended but still-transitional timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_withering_vs_calcification, empirical, 'Whether the dictatorship persists due to objective counter-revolutionary necessity or elite self-preservation.').

omega_variable(
    party_identity_lock_mechanism,
    'For revolutionary cadre, is the identity_locked exit a structural feature of revolutionary commitment itself, or a deliberate institutional design to prevent exit and lateral movement?',
    'Comparative analysis: (a) examine whether cadre exit is prevented by deliberate institutional barriers (party discipline, surveillance, career destruction) or by internal fusion of self-concept with revolutionary role; (b) observe whether cadre who genuinely doubt the project face security apparatus targeting (external suppression) or self-imposed exile (internalized identity-lock); (c) examine whether cadre socialization emphasizes identity fusion or merely enforces behavioral compliance.',
    'If institutional barriers dominate, the exit_options classification might shift from identity_locked toward trapped, raising effective extraction. If identity fusion is primary, the psychological mechanism for suppression becomes internalized rather than structural, changing the character of suppression without changing the metric but clarifying the mechanism for post-exit trajectories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(party_identity_lock_mechanism, conceptual, 'Whether revolutionary cadre are locked by fused identity or by institutional coercion, or both.').

omega_variable(
    working_class_beneficiary_status,
    'Is the industrial_working_class a genuine beneficiary (as the vanguard reading claims—the dictatorship is ''of the proletariat,'' serving working-class interests through party-mediated action) or a nominal beneficiary masking actual payer status?',
    'Outcome analysis: (a) do workers'' material conditions improve faster under the constraint than under capitalist or alternative-socialist arrangements? (b) do workers exercise actual decision-making power over resource allocation, production priorities, or labor discipline, or merely receive orders from party-appointed managers? (c) do workers retain ability to strike, organize independently, or exit employment (refuse conscription), or are these prohibited? (d) do post-revolutionary observers report workers perceiving themselves as the ruling class, or as subjects of state discipline?',
    'If workers are genuine beneficiaries, the symmetric or near-symmetric directionality supports the tangled_rope claim. If workers are actual payers with nominal beneficiary status, the constraint is more snare-like despite its theoretical framing. This distinction is central to whether the vanguard reading accomplishes what it claims (working-class liberation) or achieves only capitalist expropriation without worker power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(working_class_beneficiary_status, empirical, 'Whether the dictatorship of the proletariat actually serves working-class interests or subordinates workers to party authority.').

omega_variable(
    alternative_reading_logical_status,
    'Given the founding problem (capitalist restoration is an objective threat), does the council_communist reading logically foreclose the vanguard reading, or do both remain structurally possible responses to the same problem?',
    'Logical analysis: Can a federated council system (council reading) provide equivalent defense against capitalist restoration and imperialist intervention without a vanguard party monopoly? If yes, the readings coexist (same founding problem, different solutions); if no, the vanguard reading logically requires party centralization (forecloses council decentralization within a single revolutionary process). Empirical evidence from attempted council systems and their outcomes.',
    'If readings foreclose each other, one reading''s classification might change due to logical necessity claims. If they coexist, the classification contest is purely over which reading better serves the founding problem—an empirical matter of comparative outcomes, not logical structure. The engine cannot compute foreclosure; this omega documents whether foreclosure is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_logical_status, conceptual, 'Whether competing revolutionary readings foreclose each other or represent genuinely available alternatives.').

omega_variable(
    spontaneity_vs_direction_boundary,
    'At what point in the revolutionary process does the need for party direction become justified by objective conditions versus become a tool for suppressing autonomous working-class initiative?',
    'Historical periodization: Examine the early revolutionary period (year 0-5), the consolidation period (year 5-15), and the calcified period (year 15+). In each period, compare (a) documented external threats requiring direction, (b) party suppression of alternatives, and (c) worker initiative outcomes when permitted versus forbidden. Identify the inflection point where direction shifts from coordinating genuine collective action to preventing autonomous organization.',
    'If the shift occurs early and is permanent (by year 5), the constraint is snare-like from early on with a temporary tangled_rope cover story. If the shift is gradual and occurs around year 15-20 (coinciding with Mandatrophy onset), the early period was genuinely tangled_rope and later periods calcify into snare. The classification might be time-dependent: tangled_rope early, snare late.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spontaneity_vs_direction_boundary, empirical, 'Whether the suppression of autonomous organization is transitional or becomes the constraint''s primary function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(mani_tr_t15, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(mani_tr_t25, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mani_be_t15, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(mani_be_t25, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(mani_su_t15, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(mani_su_t25, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 40, 0.79).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(mani_grid_01, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(class), 0, 0.42).
narrative_ontology:measurement(mani_grid_02, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(class), 40, 0.74).
narrative_ontology:measurement(mani_grid_03, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(mani_grid_04, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(mani_grid_05, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(mani_grid_06, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(organizational), 40, 0.81).
narrative_ontology:measurement(mani_grid_07, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(structural), 0, 0.28).
narrative_ontology:measurement(mani_grid_08, manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse(structural), 40, 0.72).
narrative_ontology:measurement(mani_grid_09, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(mani_grid_10, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(class), 40, 0.35).
narrative_ontology:measurement(mani_grid_11, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(individual), 0, 0.73).
narrative_ontology:measurement(mani_grid_12, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(individual), 40, 0.42).
narrative_ontology:measurement(mani_grid_13, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(organizational), 0, 0.81).
narrative_ontology:measurement(mani_grid_14, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(organizational), 40, 0.28).
narrative_ontology:measurement(mani_grid_15, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(mani_grid_16, manifesto_revolutionary_method__vanguard_rupture_reading, resistance(structural), 40, 0.18).
narrative_ontology:measurement(mani_grid_17, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(class), 0, 0.38).
narrative_ontology:measurement(mani_grid_18, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(class), 40, 0.64).
narrative_ontology:measurement(mani_grid_19, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(mani_grid_20, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(individual), 40, 0.71).
narrative_ontology:measurement(mani_grid_21, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(mani_grid_22, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(organizational), 40, 0.73).
narrative_ontology:measurement(mani_grid_23, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(structural), 0, 0.25).
narrative_ontology:measurement(mani_grid_24, manifesto_revolutionary_method__vanguard_rupture_reading, stakes_inflation(structural), 40, 0.62).
narrative_ontology:measurement(mani_grid_25, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(class), 0, 0.54).
narrative_ontology:measurement(mani_grid_26, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(class), 40, 0.78).
narrative_ontology:measurement(mani_grid_27, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(mani_grid_28, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(individual), 40, 0.77).
narrative_ontology:measurement(mani_grid_29, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(mani_grid_30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(organizational), 40, 0.84).
narrative_ontology:measurement(mani_grid_31, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(structural), 0, 0.35).
narrative_ontology:measurement(mani_grid_32, manifesto_revolutionary_method__vanguard_rupture_reading, suppression(structural), 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.15).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% Part of the manifesto_revolutionary_method constraint family: three readings of the contested kernel 'how revolutionary transformation occurs.' This vanguard rupture reading authorizes high state centralization, party monopoly, and suppression of alternatives—benefiting party elite and state apparatus while imposing costs on pluralists and autonomous worker organizations. The sibling council_communist_reading distributes authority through federated councils without party monopoly, producing different beneficiary/victim structures and lower extractiveness through different mechanisms. The sibling democratic_gradualism_reading avoids revolutionary rupture entirely, maintaining pluralist institutions and gradualist transition, with entirely different ε. The three readings are linked via network.affects_constraints because each reading's validity depends partly on critiquing the others: vanguard claims gradualism fails, councils are naive; councils claim vanguard becomes new domination; gradualism claims rupture is unnecessary violence. They are NOT alternative framings of one constraint—they are three distinct constraints arising from three incompatible readings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, organized, 0.72).
constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, moderate, 0.81).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
