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
 *   human_readable: Vanguard Party Seizure of State Power and Dictatorship of the Proletariat
 *   domain: political philosophy/revolutionary theory/historical materialism
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested Manifesto
 *   revolutionary-method kernel: the vanguard-rupture reading, which holds
 *   that revolutionary transformation requires an organized party to seize
 *   state power and administer a transitional dictatorship of the proletariat
 *   on behalf of the working class, dissolving the bourgeois state apparatus
 *   and, per its own theory, withering the state itself as class antagonism
 *   recedes. This reading is generated as a clean, self-contained constraint
 *   — it does not describe or average over the council-communist reading
 *   (direct workers'-council power without a party state) or the
 *   democratic-gradualism reading (electoral-majority transformation of
 *   existing institutions). Those are separate constraints, evaluated in
 *   their own files, with their own ε and stakeholder sets. Here, ε is
 *   authored high because the reading's own historical operation shows
 *   sustained, escalating suppression of alternative organizational forms —
 *   rival socialist factions, autonomous councils, and pluralist institutions
 *   — as a structural feature of maintaining party monopoly during the
 *   'transition,' not as an incidental cost.
 *
 * KEY AGENTS:
 *   - central_committee_leadership: agenda-setter, administers the transitional state and enforces the correct revolutionary line
 *   - party_cadres: primary beneficiary, gains administrative and material position contingent on party loyalty
 *   - state_planning_apparatus: institutional beneficiary, absorbs coordination functions by displacing markets and independent bodies
 *   - political_pluralists / autonomous_worker_organizations / rival_socialist_factions: victims, structurally excluded and suppressed as the price of party unity
 *   - peasant_smallholders: victims, subordinated to industrial planning priorities
 *   - industrial_proletariat: nominal beneficiary whose benefits are entirely mediated through party channels rather than direct control
 *   - historical_materialist_theorists: analytical observers assessing whether the transitional state withers or ossifies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.81).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power and Dictatorship of the Proletariat").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political philosophy/revolutionary theory/historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'c3f88d33-5f47-44c9-9fbd-03db0ea4b323').
narrative_ontology:cs_kernel_codification('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', formalized).
narrative_ontology:cs_authority_grounding('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', lineage).
narrative_ontology:cs_interpretation_layer_present('c3f88d33-5f47-44c9-9fbd-03db0ea4b323').
narrative_ontology:cs_reading_relation('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', foundational, party_vanguard_necessary_for_revolutionary_discipline).
narrative_ontology:cs_axiom_status(party_vanguard_necessary_for_revolutionary_discipline, holdable).
narrative_ontology:cs_axiom_grounding('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', party_vanguard_necessary_for_revolutionary_discipline, instrumental).
narrative_ontology:cs_axiom('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', foundational, transitional_dictatorship_precedes_stateless_communism).
narrative_ontology:cs_axiom_status(transitional_dictatorship_precedes_stateless_communism, holdable).
narrative_ontology:cs_axiom_grounding('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', transitional_dictatorship_precedes_stateless_communism, empirically_contingent).
narrative_ontology:cs_reference_frame('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', pre_seizure_capitalist_state_order).
narrative_ontology:cs_drift_state('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', post_consolidation_party_state_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c3f88d33-5f47-44c9-9fbd-03db0ea4b323', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, central_committee_leadership).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, rival_socialist_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, peasant_smallholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the vanguard party's seizure of state apparatus, defines the line of correct revolutionary consciousness, and administers the transitional dictatorship. Justifies concentration of power as a temporary necessity against counter-revolutionary restoration and imperialist encirclement. Controls appointments, doctrine, and the security apparatus that enforces party discipline.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, central_committee_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain administrative posts, material privileges, and political standing through party membership and demonstrated ideological loyalty. Their advancement depends on the party's monopoly over legitimate political action; leaving the party forecloses the career path entirely.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres, beneficiary,
    organized, biographical, constrained, national).

% Absorbs the economic coordination functions previously distributed across markets and independent institutions, consolidating planning authority under party direction. Its continued existence depends on the suppression of alternative coordination mechanisms it would otherwise compete with.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, agenda_setter).

% Advocate multi-party competition, free press, and contested elections as legitimate expressions of working-class self-rule. Under this reading their organizations are banned as bourgeois deviation or counter-revolutionary. Exit means exile, imprisonment, or silence.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    powerless, biographical, trapped, national).

% Factory committees, independent unions, and workers' councils that organized outside party control are subordinated to party-run unions or dissolved outright. Their direct democratic decision-making is replaced by party-appointed administration; resistance is treated as anarcho-syndicalist or ultra-left deviation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, regional).

% Menshevik, anarchist, left-communist, and social-democratic currents that dispute the vanguard's claim to sole representation of proletarian interest. Excluded from the transitional state's institutions and frequently criminalized once the party consolidates the monopoly on legitimate revolutionary interpretation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, rival_socialist_factions, payer,
    organized, biographical, trapped, national).

% Subjected to forced requisition, collectivization, or grain-price manipulation as the planning apparatus subordinates agrarian production to industrial priorities set by the party. Their traditional land tenure and market access are dissolved by administrative fiat in the name of the transition.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, peasant_smallholders, payer,
    powerless, biographical, constrained, regional).

% Nominally the class in whose name the dictatorship is exercised; gains formal ownership claims over industry and expanded social provision, but exercises no direct control over factory or state decisions once councils are subordinated to party structures. Benefits are real but mediated entirely through party channels.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat, payer).

% Assess whether the vanguard-led transitional state actually withers toward stateless communism or ossifies into permanent party rule. Draw on comparative revolutionary outcomes across the twentieth century to evaluate the reading's own predicted trajectory against its lived history.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, historical_materialist_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a disciplined, unified revolutionary force capable of overcoming the state's monopoly on violence and coordinating rapid post-seizure economic reorganization without the delay and fragmentation of contested pluralist decision-making.
% TRANSFER_FUNCTION: Moves formal political authority, economic planning control, and coercive state capacity from the displaced bourgeois state and from horizontally organized worker and peasant bodies to the party apparatus, which administers them in trust for a proletariat that does not directly exercise them.
% ABSENT_VOICES: Political pluralists, rival socialist tendencies, and autonomous worker councils would object that the dictatorship substitutes party rule for proletarian rule; they are excluded from the transitional state's institutions by design, since the reading treats their continued organizational independence as the primary counter-revolutionary risk to be foreclosed.
% DISAPPEARANCE_RATIONALE: If vanguard party control of the transitional state dissolved, the councils, unions, and rival factions currently subordinated to it would immediately reassert independent decision-making authority, planning would fragment into contested and possibly market-mediated coordination, and the party cadre stratum would lose its administrative monopoly — the entire post-seizure institutional order is built around and depends on the party's exclusive claim to represent the revolutionary class.
% FOUNDING_PROBLEM: Spontaneous mass uprising, left to itself, was held to be insufficiently organized to defeat a coordinated capitalist state apparatus and counter-revolutionary intervention, and insufficiently disciplined to prevent restoration during the vulnerable transitional period before class antagonisms could be dissolved.
% FOUNDING_PROBLEM_CORROBORATION: Party leadership and cadres attest the problem remains live indefinitely given continued external and internal counter-revolutionary threat. Independent historians of the Russian, Chinese, and Cuban revolutions, along with council-communist and social-democratic critics writing from outside the party's own tradition, attest that the 'transitional' emergency justification persisted well past any plausible external threat and functioned to entrench the party's permanent monopoly on power rather than to withdraw the state as originally theorized.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.68 because the reading's operating history (measured against its own stated aim of a state that withers) shows accumulating, not receding, concentration of authority in the party apparatus. Suppression is authored higher still (0.81) because the reading's persistence structurally depends on foreclosing alternative organizational forms — councils, rival parties, factional pluralism — not merely discouraging them. Theater ratio (0.40) reflects that the 'transitional' and 'withering state' framing increasingly functions as legitimating narrative for a state apparatus that in practice consolidates rather than dissolves. All three time series share one grid (t=0,5,10,20,30,40) so the rising trajectories are read as one coherent account of consolidation, not independently drifting metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the central-committee and cadre seats, the arrangement is genuine coordination — the disciplined instrument that alone can defeat organized counter-revolution and rapidly reorganize a shattered economy. From the seats of autonomous worker organizations and rival socialist factions, the identical structure operates as displacement: their own organs of proletarian self-rule are the thing being suppressed in the name of proletarian rule. The engine computes this divergence from the declared power/exit/scope data; the claim (tangled_rope) does not resolve it, it names that both a genuine coordination function AND asymmetric extraction are structurally present simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Party cadres and the state-planning apparatus sit near the beneficiary end: they collect administrative authority and material position, and their exit options are arbitrage-grade (they can move within or above the system they administer). Political pluralists, autonomous worker organizations, and rival socialist factions sit at the target end: trapped exit options (exile, imprisonment, silence), full extraction of their prior organizational autonomy. The industrial proletariat is deliberately dual-coded (beneficiary + payer): the reading claims real material gains accrue to this class, but exercise of power is fully mediated through party structures it does not itself control, which is a real but attenuated benefit alongside a real cost of lost direct agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defeating a coordinated capitalist state and preventing counter-revolutionary restoration during a vulnerable transition — was historically live at the moment of seizure. The R5 corroboration test is the analytical crux: corroboration for continued necessity comes almost entirely from the party's own tradition, while corroboration from outside sources (independent historians, rival left tendencies) attests the founding problem became dead well before the party apparatus dissolved its own authority. That status/verdict mismatch (problem increasingly dead, but disappearance would still rearrange the world because so much institutional power has accreted around the party) is exactly the signature the mandatrophy check is built to catch — the constraint has outlived the emergency that justified it, which the tangled_rope classification (coordination and extraction co-present, requiring active enforcement) reflects better than a pure snare or pure rope label would.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_state_withering_or_ossification,
    'Does the vanguard-led transitional state structurally tend toward withering (per its own theoretical justification) or toward permanent self-perpetuation once the party apparatus is established?',
    'Comparative historical analysis across cases claiming this reading (USSR, PRC, Cuba, Vietnam) tracking whether party-state coercive and administrative capacity contracted or expanded after the initial consolidation period, and whether any case produced a genuine transfer of authority back to non-party worker organs.',
    'If withering is never observed and consolidation is the uniform pattern, the ''transitional'' framing functions as permanent legitimation rather than a genuine developmental stage, supporting reclassification toward snare; if credible partial-withering cases exist, the tangled_rope classification (genuine coordination function coexisting with extraction) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_state_withering_or_ossification, empirical, 'Whether historical instances of this reading show state withering or permanent party entrenchment.').

omega_variable(
    vanguard_representation_claim,
    'Can a party organization legitimately claim to represent proletarian class interest in the absence of direct, contestable mechanisms by which the class can revoke that representation?',
    'Not resolvable empirically alone — depends on a normative theory of representation and legitimate authority; comparative political theory can characterize the tradeoffs but not settle the question.',
    'If representation without revocability is illegitimate, the reading''s coordination claim is substantially undermined and extraction dominates; if disciplined vanguard representation is held legitimate during genuine emergency, the coordination function is real and extraction is the necessary cost of that function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vanguard_representation_claim, conceptual, 'Whether unrevocable vanguard representation of class interest can be normatively legitimate.').

omega_variable(
    committer_structure_disagreement_locus,
    'Where exactly do the three sibling readings of the manifesto_revolutionary_method kernel disagree — is it about the necessity of rupture with the existing state (all three may agree rupture is eventually needed), or specifically about whether a party organ or a council/electoral organ should hold power during and after that rupture?',
    'Textual and doctrinal comparison of the vanguard_rupture, council_communist, and democratic_gradualism traditions to isolate whether the disagreement is over the fact of revolutionary rupture (means) or over which body exercises post-rupture authority (institutional form).',
    'If the disagreement is purely about institutional form after an agreed rupture, this reading and council_communist_reading are closer structural cousins than either is to democratic_gradualism_reading, which disputes the necessity of rupture itself — this would refine the reading_relations declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_disagreement_locus, conceptual, 'Locating precisely where the three kernel readings structurally diverge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the manifesto_revolutionary_method kernel, each authored as a separate ε-invariant constraint per the decomposition principle: vanguard_rupture_reading (this file, ε≈0.68, tangled_rope), council_communist_reading (direct council power, expected lower suppression/extraction, likely rope or tangled_rope depending on federation enforcement), and democratic_gradualism_reading (electoral/institutional reform, expected substantially lower ε, likely rope or scaffold). The three do not average into a single 'Marxist revolutionary theory' constraint; they are structurally distinct claims about who holds power after rupture and by what mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
