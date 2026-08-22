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
 *   domain: political/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint models the vanguard-rupture reading of revolutionary
 *   method as theorized in the Manifesto tradition and elaborated by
 *   subsequent party-centric currents: revolutionary transformation requires
 *   an organized, disciplined party to seize state power on behalf of the
 *   working class, and to hold that power through a transitional
 *   'dictatorship of the proletariat' that suppresses counter-revolutionary
 *   and rival organizational forms until class antagonism dissolves. This is
 *   ONE of three structurally distinct readings of the shared kernel
 *   manifesto_revolutionary_method; the council communist reading
 *   (soviets/workplace assemblies as the durable organ of power, no vanguard
 *   intermediary) and the democratic gradualism reading (socialism through
 *   existing electoral/parliamentary structures) are separate constraints
 *   with their own ε and stakeholder sets, linked here via
 *   network.affects_constraints. The measured extraction here is high because
 *   the transitional state form has, in every historically observed instance,
 *   functioned as a durable seizure of political and economic authority by
 *   the party apparatus rather than a genuinely temporary scaffold — the
 *   founding problem it claims to solve becomes, empirically, a justification
 *   for permanent rule.
 *
 * KEY AGENTS:
 *   - party_cadres: Primary beneficiary (organized/mobile) — gains organizational authority and administrative position through the seizure
 *   - state_planning_apparatus: Primary beneficiary (institutional/arbitrage) — absorbs coordination functions with sweeping new authority
 *   - central_committee_leadership: Agenda setter (institutional/arbitrage) — defines legitimate revolutionary action and enforces discipline
 *   - political_pluralists: Primary target (moderate/trapped) — organizing recast as counter-revolutionary, excluded from legal politics
 *   - autonomous_worker_councils: Primary target (organized/constrained) — subordinated from autonomous organs to transmission belts
 *   - rival_left_factions: Primary target (moderate/trapped) — purged or repressed once party consolidates
 *   - peasant_smallholders: Diffuse target (powerless/trapped) — bear costs of requisitioning and collectivization
 *   - revolutionary_theorists: Analytical observer — assesses whether the transitional state withers as theorized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.82).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power and Dictatorship of the Proletariat").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '1b1b12bf-48bc-4bbf-a2e8-77f36e57845c').
narrative_ontology:cs_kernel_codification('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', formalized).
narrative_ontology:cs_authority_grounding('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', lineage).
narrative_ontology:cs_interpretation_layer_present('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c').
narrative_ontology:cs_reading_relation('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', foundational, party_vanguard_necessary_for_state_seizure).
narrative_ontology:cs_axiom_status(party_vanguard_necessary_for_state_seizure, holdable).
narrative_ontology:cs_axiom_grounding('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', party_vanguard_necessary_for_state_seizure, instrumental).
narrative_ontology:cs_axiom('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', foundational, transitional_dictatorship_justifies_suppression_of_pluralism).
narrative_ontology:cs_axiom_status(transitional_dictatorship_justifies_suppression_of_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', transitional_dictatorship_justifies_suppression_of_pluralism, instrumental).
narrative_ontology:cs_axiom('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', secondary, state_form_withers_after_class_antagonism_resolves).
narrative_ontology:cs_axiom_status(state_form_withers_after_class_antagonism_resolves, overridden).
narrative_ontology:cs_axiom_grounding('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', state_form_withers_after_class_antagonism_resolves, empirically_contingent).
narrative_ontology:cs_reference_frame('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', vanguard_party_as_necessary_organ_of_class_power).
narrative_ontology:cs_drift_state('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', post_twentieth_century_consolidation_record, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1b1b12bf-48bc-4bbf-a2e8-77f36e57845c', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, central_committee_leadership).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_councils).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, rival_left_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, peasant_smallholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy positions of organizational authority won through disciplined participation in the vanguard party. Direct the seizure of state apparatus, staff the resulting institutions, and administer the transitional dictatorship. Their status, security, and historical role depend on the party's monopoly over revolutionary interpretation and state administration.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres, agenda_setter).

% Newly constituted central planning bodies absorb functions formerly distributed across markets and independent associations. They gain sweeping authority to allocate resources, direct labor, and adjudicate economic disputes, justified as necessary for the transition period's coordination demands.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Sets doctrine on what counts as legitimate revolutionary action, defines the boundary of the transitional state, and enforces party discipline against deviation. Determines when — or whether — the state apparatus withers, and adjudicates who counts as a genuine proletarian representative versus a class enemy or opportunist.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, central_committee_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Advocate for multi-party competition, independent press, and electoral contestation as checks on state power. Under the vanguard reading their organizing is treated as bourgeois restorationism or counter-revolutionary factionalism; they face suppression, exclusion from legal politics, or imprisonment, with no institutional channel to contest the party's monopoly.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Workplace assemblies and soviets that organized independently during the revolutionary period find their decision-making authority subordinated to party direction once the vanguard consolidates state power. Their councils persist in form but lose substantive control over production and political representation, becoming transmission belts for party policy rather than autonomous organs.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_councils, payer,
    organized, biographical, constrained, regional).

% Anarchists, left-communists, and rival socialist tendencies who participated in the revolutionary rupture but reject vanguard party monopoly find themselves reclassified as deviationists once the party consolidates. Many are purged from coalition government, banned from organizing, or subjected to direct repression by the new state's security apparatus.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, rival_left_factions, payer,
    moderate, biographical, trapped, national).

% Rural landholders whose property relations are subordinated to centralized planning priorities. They bear the costs of forced requisitioning, collectivization pressure, or price controls set by the planning apparatus, with no independent political organization permitted to represent their interests against the transitional state's demands.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, peasant_smallholders, payer,
    powerless, generational, trapped, national).

% The generations meant to inherit the stateless, classless society the dictatorship is theorized to produce. They have no voice in whether the transitional state's indefinite extension serves or forecloses that promised outcome; their interests are asserted on their behalf by the party rather than represented by them.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, future_working_class, excluded,
    powerless, civilizational, analytical, national).

% Study historical instances of vanguard-led revolutions to assess whether the transitional state form actually withers as theorized or ossifies into a permanent apparatus. Draw on comparative history across the twentieth century's revolutionary states.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of organizing a fragmented, spontaneous uprising into a disciplined force capable of actually displacing the existing state apparatus, coordinating economic reconstruction in the immediate aftermath, and defending the revolution against counter-revolutionary and foreign intervention before class antagonisms have been resolved.
% TRANSFER_FUNCTION: Moves political decision-making authority, control over the means of production, and the legitimate use of coercive force from a plurality of contending revolutionary actors and prior propertied classes to the party apparatus and the state institutions it directs, in the name of the proletariat as a class.
% ABSENT_VOICES: Autonomous worker councils, rival left factions, and political pluralists were often active participants in the revolutionary rupture itself and would object that the vanguard reading substitutes party rule for the direct rule of the working class it claims to represent; once the party consolidates, these voices are excluded from legal political participation and cannot contest the arrangement from within.
% DISAPPEARANCE_RATIONALE: If the vanguard party's monopoly on state power were removed, the array of revolutionary actors it displaced or subordinated — worker councils, rival socialist tendencies, pluralist factions — would immediately re-enter contestation over the shape of the post-revolutionary order; the centralized planning apparatus would fragment or be forced to negotiate authority rather than direct it.
% FOUNDING_PROBLEM: Fragmented, spontaneous working-class uprisings had historically been defeated by organized counter-revolutionary force (state militaries, capital flight, foreign intervention) because they lacked centralized command, a coherent program, and the discipline to hold state power once seized.
% FOUNDING_PROBLEM_CORROBORATION: Party leadership and state-planning apparatus attest the founding problem remains live indefinitely — citing continued external threat and the unfinished character of the transition. Independent historians of twentieth-century revolutionary states, dissident Marxist currents (council communists, left oppositionists), and former party members who broke with the apparatus attest that the 'transitional' state form in every historically observed case failed to wither and instead consolidated as a permanent ruling apparatus — corroboration from outside the beneficiary set converges on founding_problem_status being effectively dead while the arrangement persists as entrenched rule.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction rises from 0.35 to 0.68 across the interval because the initial rupture genuinely coordinates a fragmented uprising against a common adversary (the ancien régime or capitalist state), but as the party consolidates, extraction shifts from external adversaries to internal rivals and prior allies — the coordination function that justified initial extraction does not scale down as the external threat recedes. Suppression rises faster and higher than extraction (0.4 to 0.82) because holding the arrangement requires actively suppressing exits: banning rival parties, subordinating councils, criminalizing factional organizing. Theater ratio climbs to 0.4 as 'transitional' vocabulary (workers' state, temporary dictatorship, withering away) persists rhetorically while the apparatus's actual function shifts toward permanent rule — this is the mismatch the founding_problem_status field flags as contested/effectively dead.
 *
 * PERSPECTIVAL GAP:
 *   From the party cadre and central committee seats, the arrangement reads as necessary revolutionary discipline against an ongoing counter-revolutionary threat — genuine coordination under siege conditions. From the political pluralist, autonomous council, and rival faction seats, the identical structure reads as one revolutionary current's seizure of the fruits of a collective uprising, enforced by the same coercive apparatus once trained on the old regime. The engine computes both seats' types from the same structural data; the divergence is not resolved by either side's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Party cadres, central committee leadership, and the state-planning apparatus are declared beneficiaries because they gain concrete organizational authority, administrative positions, and control over allocation decisions that did not exist in that concentrated form before the seizure — their directionality sits near the beneficiary end with mobile-to-arbitrage exit (they can convert political capital into institutional position). Political pluralists, autonomous worker councils, rival left factions, and peasant smallholders are declared victims because the same structure removes their pre-existing organizational autonomy or subordinates their political voice, with trapped or constrained exit options reflecting that no legal channel remains once the party consolidates — this pushes their directionality toward the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing is essential here: the vanguard party's coordination function (organizing scattered revolutionary energy into a force capable of holding state power against counter-revolution) was genuinely necessary in specific historical windows — this prevents flatly mislabeling the entire arrangement as pure extraction from inception. But the founding_problem_status is authored as contested rather than live because the corroborating evidence (independent historians, dissident Marxist currents, defectors from the apparatus) converges on the transitional state form failing to wither in every observed instance — the 'transition' becomes the permanent condition. This is precisely the classification the Tangled Rope category exists to capture: real coordination function at founding, sustained extraction thereafter, requiring active enforcement to hold once the founding threat recedes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vanguard_reading_within_kernel,
    'This constraint instantiates the vanguard_rupture_reading of the shared kernel manifesto_revolutionary_method. The council_communist_reading holds that federated workplace assemblies should exercise power directly with no vanguard intermediary; the democratic_gradualism_reading holds that existing electoral structures are the legitimate vehicle for working-class power. Which reading correctly identifies the necessary vehicle for revolutionary transformation?',
    'This is not empirically resolvable by data internal to any one reading — it depends on contested premises about whether spontaneous working-class self-organization can hold state power without centralized party direction, and whether existing liberal-democratic institutions can be captured for socialist transformation without a rupture. Comparative historical analysis of instances where councils held power without vanguard party direction (e.g., early soviets, some anarchist territories) versus vanguard-party consolidations is suggestive but contested on interpretation.',
    'If the council communist reading is correct, the vanguard party''s seizure of the councils'' authority is not a necessary transitional cost but the specific extraction event that the coordination story exists to launder — the tangled_rope classification would shift toward snare, since the coordination the party performs (defeating counter-revolution) could have been performed by the councils themselves. If the democratic gradualism reading is correct, the entire premise of forcible state seizure is unnecessary, and the suppression of pluralists is pure cost with no offsetting coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vanguard_reading_within_kernel, conceptual, 'Structural location of disagreement among the three kernel readings: whether the vanguard party is a necessary organizational vehicle, an unnecessary displacement of council power, or an unnecessary rupture with legal-democratic alternatives.').

omega_variable(
    transitional_state_withering_ambiguity,
    'Is the dictatorship of the proletariat''s failure to wither, in every historically observed instance, evidence that the transitional state form is inherently self-perpetuating once vested with concentrated power, or evidence that no historically observed instance was implemented under conditions the theory actually specified (e.g., simultaneous revolution across multiple advanced industrial states)?',
    'Would require either a historical instance implemented under the theory''s specified conditions (which has never occurred) or a structural argument for why concentrated coercive and economic authority systematically resists voluntary dissolution regardless of the theorized conditions being met.',
    'If the failure-to-wither pattern is inherent to concentrated power rather than an artifact of unfavorable historical conditions, the tangled_rope classification understates the case — the arrangement would be better modeled as a snare wearing transitional coordination language as permanent cover. If the pattern is an artifact of conditions, the classification''s coordination component is more defensible and the extraction is better read as a correctable implementation failure rather than a structural feature of the reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_state_withering_ambiguity, empirical, 'Whether the observed non-withering of transitional states is structural to concentrated power or contingent on unfavorable implementation conditions.').

omega_variable(
    beneficiary_versus_class_representation,
    'Do party cadres and the state-planning apparatus benefit AS a distinct stratum with interests separable from the proletariat they claim to represent, or does their concentrated authority remain, in some meaningful sense, the proletariat exercising power through its organized vanguard?',
    'Track material and political divergence between party/state cadre living standards, career security, and political voice versus the broader working class over the interval; sustained divergence indicates stratum formation rather than representation.',
    'If cadres form a distinct beneficiary stratum, the beneficiary declaration (party_cadres, state_planning_apparatus) is correct as authored and the extraction is genuinely asymmetric. If cadre and class interests remain substantively fused, the beneficiary declaration overstates the separation and the constraint is closer to a rope with unusually high enforcement cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_versus_class_representation, empirical, 'Whether party/state beneficiaries constitute a distinct extracting stratum or remain a genuine representative organ of the class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the shared kernel manifesto_revolutionary_method, per the ε-invariance principle: 'the correct revolutionary method' is not one constraint but three structurally distinct claims about the necessary vehicle for working-class power (vanguard party seizure, federated council power, electoral-gradualist capture), each with its own beneficiary/victim structure and its own ε. This reading (vanguard_rupture) authors the highest ε (0.68) of the three owing to its structural requirement of active suppression of both the council-communist and democratic-gradualist pathways once state power is seized — the forecloses relations in cs_structure reflect that a party holding state power under this reading cannot simultaneously permit federated council sovereignty or multi-party electoral contestation without abandoning the reading's own foundational axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
