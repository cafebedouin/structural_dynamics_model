% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure of State Power and the Party-Guided Transitional State (Dictatorship of the Proletariat)
 *   domain: political philosophy / revolutionary theory / historical materialism
 *
 * SUMMARY:
 *   The vanguard_rupture reading of the manifesto_revolutionary_method kernel
 *   instantiates a specific constraint: proletarian emancipation is held to
 *   require a disciplined revolutionary party that seizes the existing state
 *   apparatus, breaks its parliamentary machinery, and exercises the
 *   dictatorship of the proletariat as a centralized, party-guided
 *   transitional state. The arrangement solves real coordination problems -
 *   defense under siege, rapid accumulation, administration of scarcity -
 *   through the same hierarchy that suppresses every independent political
 *   voice: rival parties banned, factions outlawed, factory committees
 *   subordinated, the peasantry requisitioned. Beneficiaries are the party
 *   leadership, the planning apparatus, and the nomenklatura; victims are
 *   political pluralists, autonomous worker organizations, the working class
 *   in its workshop autonomy, and the peasantry. This story is one member of
 *   a three-story constraint family decomposing the kernel. The sibling
 *   readings are separate constraints authored in their own files with their
 *   own epsilon values: democratic_gradualism_reading operates through
 *   consent-bearing electoral mechanisms, so pluralists appear as
 *   participants rather than suppressed victims and epsilon is substantially
 *   lower; council_communist_reading locates coordination in federated
 *   workplace assemblies, so party cadres drop out of the beneficiary set
 *   entirely and autonomous worker organizations move from victim to
 *   coordinated subject. Those differences in beneficiary/victim structure -
 *   not differences of opinion about a shared object - are why the readings
 *   are modeled as distinct constraints linked by network edges. KEY AGENTS
 *   (by structural relationship): - vanguard_party_leadership: Agenda setter
 *   (institutional / identity_locked) - administers the party-state, directs
 *   enforcement, collects the concentrated gains - state_planning_apparatus:
 *   Primary beneficiary (powerful / constrained) - converts party priorities
 *   into plans; authority exists only inside the system -
 *   party_cadres_nomenklatura: Beneficiary (organized / identity_locked) -
 *   staffs the command chain; receives preferential access and lifetime
 *   status - political_pluralists: Payer (moderate / trapped) - rival
 *   socialists, liberals, anarchists; arrested, exiled, or driven underground
 *   - autonomous_worker_organizations: Payer (organized / trapped) - factory
 *   committees, independent unions, garrison soviets; subordinated or crushed
 *   - industrial_working_class: Payer with secondary beneficiary position
 *   (powerless / constrained) - nominal sovereign bearing labor discipline
 *   and lost voice, receiving literacy, employment, staples - peasantry:
 *   Payer (powerless / trapped) - requisitioned and collectivized; finances
 *   industrialization - left_oppositionists: Excluded (moderate / trapped) -
 *   internal critics criminalized by the 1921 faction ban; objections survive
 *   in emigre and samizdat channels - revolutionary_historians: Analytical
 *   observer (analytical / analytical) - reconstructs the arrangement from
 *   archives without a stake in its persistence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.74).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.8).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power and the Party-Guided Transitional State (Dictatorship of the Proletariat)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political philosophy / revolutionary theory / historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '5db24a6f-65b0-4907-8ea7-56452a3b7d59').
narrative_ontology:cs_kernel_codification('5db24a6f-65b0-4907-8ea7-56452a3b7d59', fixed_text).
narrative_ontology:cs_authority_grounding('5db24a6f-65b0-4907-8ea7-56452a3b7d59', lineage).
narrative_ontology:cs_interpretation_layer_present('5db24a6f-65b0-4907-8ea7-56452a3b7d59').
narrative_ontology:cs_reading_relation('5db24a6f-65b0-4907-8ea7-56452a3b7d59', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_reading_relation('5db24a6f-65b0-4907-8ea7-56452a3b7d59', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('5db24a6f-65b0-4907-8ea7-56452a3b7d59', foundational, state_seizure_by_vanguard_party_required).
narrative_ontology:cs_axiom_status(state_seizure_by_vanguard_party_required, holdable).
narrative_ontology:cs_axiom_grounding('5db24a6f-65b0-4907-8ea7-56452a3b7d59', state_seizure_by_vanguard_party_required, instrumental).
narrative_ontology:cs_axiom('5db24a6f-65b0-4907-8ea7-56452a3b7d59', foundational, party_guided_transitional_dictatorship_legitimate).
narrative_ontology:cs_axiom_status(party_guided_transitional_dictatorship_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5db24a6f-65b0-4907-8ea7-56452a3b7d59', party_guided_transitional_dictatorship_legitimate, instrumental).
narrative_ontology:cs_axiom('5db24a6f-65b0-4907-8ea7-56452a3b7d59', secondary, transitional_state_withering_prediction).
narrative_ontology:cs_axiom_status(transitional_state_withering_prediction, holdable).
narrative_ontology:cs_axiom_grounding('5db24a6f-65b0-4907-8ea7-56452a3b7d59', transitional_state_withering_prediction, empirically_contingent).
narrative_ontology:cs_reference_frame('5db24a6f-65b0-4907-8ea7-56452a3b7d59', party_guided_transitional_dictatorship).
narrative_ontology:cs_drift_state('5db24a6f-65b0-4907-8ea7-56452a3b7d59', contemporary_post_1991, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5db24a6f-65b0-4907-8ea7-56452a3b7d59', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_leadership).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres_nomenklatura).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_centralism_doctrine).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, two_stage_transition_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central committee and politburo members set policy for party and state, appoint and dismiss officials, direct the security services, and adjudicate doctrine. They concentrate decision rights over production targets, personnel, and permissible opinion, and they receive the arrangement's most concentrated rewards: unchecked authority, historical standing, and material privilege. Departure from the center has historically meant political annihilation or execution; their identities are fused with the revolutionary project they administer.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_leadership, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_leadership, beneficiary).

% Economic ministries and planning agencies convert party priorities into output quotas, allocate investment and labor, and manage chronic shortage. Planning posts confer authority, housing access, and career advancement unavailable outside the apparatus, and their expertise is legible only within the planned-economy system, so leaving means abandoning professional standing altogether.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    powerful, biographical, constrained, national).

% Full-time officials, regional secretaries, and listed-appointment holders staff the party's chain of command from province to factory. They receive preferential access to goods, schooling, and promotion through the nomenklatura system. Decades of party service constitute their social identity; dissent or departure forfeits status accumulated over a lifetime and, in harsher periods, physical security.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres_nomenklatura, beneficiary,
    organized, biographical, identity_locked, national).

% Members of rival socialist parties, liberal constitutionalists, anarchists, and religious political movements operated legally in the revolution's early months and were progressively arrested, exiled, or driven underground as the party consolidated its monopoly. Their newspapers were closed and their organizations dissolved; continued political activity inside the country carried imprisonment, camp, or exile.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Factory committees, independent unions, and garrison soviets that arose in 1917 claimed direct control over production and local governance. Between 1918 and 1921 they were subordinated to party-appointed managers and official union bureaucracies; the Kronstadt garrison's 1921 demand for soviet democracy was suppressed by force. Members who asserted independence faced dismissal, arrest, or dispersal.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, national).

% Urban workers are the arrangement's proclaimed sovereign - the dictatorship is announced as their class rule - while in practice they labor under one-man management, forbidden strikes, and compulsory labor directives. They receive literacy campaigns, guaranteed employment, housing queues, and subsidized staples; they surrender workplace autonomy, geographic mobility (later restricted by internal passports), and any independent political voice.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_working_class, beneficiary).

% The rural majority supplied grain requisitions under War Communism, then collectivized labor and procurement quotas, financing industrialization through extracted agricultural surplus. Resistance met deportation of labeled households and famine. Collective-farm registration and internal passports bound them to the land; exit meant illegal flight to the cities.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, peasantry, payer,
    powerless, biographical, trapped, national).

% Party members who authored internal platforms - the Workers' Opposition, the Democratic Centralists, later the United Opposition - argued inside the party that the emergency had passed and soviet democracy should be restored. The 1921 ban on factions criminalized their platforms; successive waves were expelled, exiled, or shot. Their critique survives in smuggled texts and emigre publication rather than in any forum the arrangement tolerates.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, left_oppositionists, excluded,
    moderate, biographical, trapped, national).

% Comparative historians and political scientists reconstruct the arrangement's operation from opened archives, emigre records, and demographic data. They hold no stake in the arrangement's persistence and can compare implementations across countries and decades.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_leadership).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates command over armed force, production, and distribution in a single disciplined hierarchy so that a revolutionary minority can defend the new regime against counterrevolution and foreign intervention, compel capital accumulation out of a predominantly agrarian economy, and administer scarcity - tasks that fragmented soviets, market exchange, and parliamentary bargaining could not perform at the required speed or scale.
% TRANSFER_FUNCTION: Moves decision-making authority from autonomous soviets, factory committees, unions, and rival parties to the party center; moves agricultural surplus and industrial labor product from countryside and shop floor to state-directed heavy-industry accumulation; moves appointment power, housing, and consumption privileges down the nomenklatura ladder as rewards for loyal administration.
% ABSENT_VOICES: Expelled factionalists, banned rival socialists, anarchists, and the peasant representatives who won the 1917 Constituent Assembly vote would object that the working class's own organs were dismantled in its name; they speak from prison camps, emigre presses, and underground circles outside the borders the arrangement controls.
% DISAPPEARANCE_RATIONALE: Defense command, allocation of scarce goods, administrative appointments, and doctrinal legitimacy all hang from the party-state hierarchy; overnight removal would force simultaneous reorganization of military command, supply, and governance. The 1991 dissolution is the observed test: the successor rearrangement took years and permanently changed property relations and borders.
% FOUNDING_PROBLEM: How a revolutionary minority can seize and hold state power against armed counterrevolution, foreign intervention, and economic collapse long enough to transform property relations - consolidation of the revolution under siege.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the 1920-21 platforms of the Workers' Opposition and the Democratic Centralists (loyal party members attesting the emergency had passed and party rule over the soviets had outlived it), by emigre Menshevik and anarchist analysis through the 1920s, and by post-1991 archival historiography; the benefiting parties themselves insist the class enemy keeps the problem permanently live, which is the expected self-serving attestation.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (epsilon 0.74, anchored to the mature 1937-1968 plateau the series shows) because the same hierarchy that performs defense and accumulation strips decision rights from every body beneath it: the transfer runs from workshops, villages, and rival parties to the party center, decoupled from any consent mechanism. Suppression (0.80) is structural and intrinsic - the arrangement cannot persist while pluralism exists, so enforcement capacity (the Cheka-to-KGB lineage) is a standing requirement rather than an emergency measure; the scalar represents the standing requirement across the arrangement's life while the series traces its phases. Theater (0.58) is substantial in the mature phase: single-candidate elections at near-unanimous turnout, ritual congresses, staged unanimity, and the cult apparatus performed mass sovereignty while the politburo decided; early soviet politics (contested soviets, real faction debate) began far less theatrical. Accessibility collapse (0.72) is high but incomplete: the electoral road, council democracy, and anarchism were closed inside the arrangement's territory yet survived in emigration and memory - unlike a natural law, the closure required continuous policing. Resistance (0.62) is sustained: Kronstadt, the Makhnovshchina, the Workers' Opposition, Hungary 1956, Prague 1968, Solidarity. The measurement series shows a cyclical thaw-and-repression pattern (NEP relaxation then collectivization terror; post-1953 thaw then the Brezhnev freeze; glasnost opening then collapse) rather than monotonic drift, and the oscillation is partly functional: periodic relaxation co-opts discontent and surfaces opponents, and each tightening follows an opening that threatened control, making the cycle itself a stabilization mechanism. All three tracked metrics share one eight-point grid so no metric's row is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the leadership seat the arrangement is the revolution itself: coercion registers as class necessity, identity is locked to the project (exit equals betrayal, and historically execution), and the payer seats' complaints register as enemy activity or backwardness. From the planner seat it is a career-coordinating machine conferring authority unavailable elsewhere. From the payer seats the identical hierarchy is dispossession: the Kronstadt sailors demanded the soviets they had built; the working class was told it already ruled. The nominal-sovereignty gap is the arrangement's signature perspectival fracture - the doctrine assigns the working class the ruler's position while the structure assigns it the payer's - which is why the same institution computes as emancipation-from-above at one seat and extraction-from-below at another. Coalition potential among the powerless was systematically foreclosed: differentiated treatment of workers and peasants, and of each socialist tendency separately, prevented the joint action that either alone could not mount.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. The leadership, planning apparatus, and cadres sit at the beneficiary end (low d): the arrangement subsidizes them with authority, careers, and consumption privileges, and their identity-locked or system-bound exits deepen the subsidy. Political pluralists, autonomous worker organizations, and the peasantry sit at the target end (high d): they bear arrests, requisitions, and lost autonomy with trapped exits. The industrial working class is authored as payer with a secondary beneficiary position: its net structural relationship is target-side (labor discipline, immobility, and silenced voice outweigh the transfers), so its derived d lands high despite the doctrine's contrary claim, while the secondary_role records the genuine transfers without flipping the seat. Scope is national: compliance verification was dense and personal (block wardens, informant networks, personnel files), which the engine weighs when scaling effective extraction. Suppression enters the computation unscaled - it is a raw structural property - while extractiveness is scaled by directionality and scope; the commentary keeps that division explicit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - consolidating a besieged revolution - was solved by roughly 1921: the intervention ended, internal rebellion was broken, property relations had been transformed. Every element of the arrangement that answered to that problem (War Communism, the faction ban, requisitioning) then persisted and hardened for seventy years under transitional rhetoric. The classification work here keeps two errors apart. Reading the arrangement as pure snare erases the genuine coordination it performed - defense, universal literacy, breakneck industrialization, administration of scarcity - which real participants experienced as real. Reading it as rope or scaffold erases the asymmetric extraction: the transitional label wore scaffold clothing, but the garment had no fastener - no declared conditions, no enforceable endpoint, no institutionalized withering mechanism (has_sunset_clause is false precisely because the withering was predicted, never scheduled). With the founding problem dead and the world still arranged around the structure, the R5 mismatch (dead status paired with a world_rearranges verdict) flags the zombie dynamic: a consolidation machine running decades after the siege ended, maintained because dismantling it would dissolve the beneficiaries' position - and fixing is prohibitive for the only actor with the power to fix it. Identity-lock dynamics matter here: cadre persistence ran on professional identity (career equals party service), ideological identity (exit unthinkable as betrayal), and institutional identity (the party 'became' the revolution); when the belief frame broke in 1989-91, the arrangement dissolved with remarkable speed, confirming how much of its persistence the fused identity was carrying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the vanguard_rupture_reading of the manifesto_revolutionary_method kernel; how would instantiating a sibling reading change the structural data?',
    'Author and compile the sibling stories (democratic_gradualism_reading, council_communist_reading) and compare computed classifications across the family; the disagreement is located in the institutional form of proletarian power - party-state versus electoral institutions versus federated councils.',
    'Under democratic_gradualism, pluralists become participants rather than victims and the planning apparatus drops out, lowering epsilon substantially; under council_communism, party cadres leave the beneficiary set and autonomous worker organizations move from payer to coordinated subject, changing the victim set wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    transitional_sunset_reality,
    'Is the transitional character of the party-guided state a genuine sunset commitment anywhere in the tradition''s operative doctrine, or legitimation rhetoric for indefinite rule?',
    'Survey implemented constitutions and party programs for any enforceable withering conditions, review criteria, or exit triggers; none has been found in any full implementation to date.',
    'If a genuine sunset mechanism existed in some variant, that variant would carry scaffold structure and warrant separate authoring; its absence confirms the transitional label as unscheduled rhetoric and supports the tangled_rope reading over scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_sunset_reality, empirical, 'Whether the transitional mandate ever carried enforceable sunset structure.').

omega_variable(
    siege_conditions_counterfactual,
    'How much of the measured extraction is attributable to besieged-consolidation conditions (civil war, intervention, blockade) rather than constitutive of the vanguard design?',
    'Compare implementations that consolidated power without comparable siege conditions (post-1945 Eastern Europe, post-1949 interior consolidation in China) and measure whether suppression of pluralists and autonomous worker organizations recurred absent an equivalent existential threat.',
    'If suppression recurs without siege, extraction is constitutive of the design and epsilon stands as measured; if it tracks siege intensity, part of epsilon is circumstantial and the design''s intrinsic extraction is lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(siege_conditions_counterfactual, empirical, 'Separating situational from constitutive extraction in the vanguard model.').

omega_variable(
    cadre_identity_lock_source,
    'Does cadre persistence rest on ideological fusion with the revolutionary mission or on material interest in nomenklatura privilege - and does the answer change the identity_locked exit coding?',
    'Compare behavior across belief-collapse episodes: the 1956 and 1968 crises, and above all 1989-91, where elites who abandoned the ideology converted office into property at speed.',
    'If material interest dominates, the identity_locked coding overstates lock-in and elite exit is cheaper than modeled, shortening the persistence horizon; if ideology dominates, lock-in is deep and collapse requires belief failure, as the 1991 pattern suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cadre_identity_lock_source, empirical, 'Source of cadre lock-in: ideology versus nomenklatura interest.').

omega_variable(
    working_class_net_position,
    'Do the transfers the working class received (literacy, employment guarantees, subsidized staples) offset the extraction it bore (labor discipline, immobility, silenced voice), or does the payer position dominate?',
    'Longitudinal accounting combining welfare gains against coercion costs, weighted by the class''s own revealed valuations in unrest data (strike waves, 1953 East Germany, 1956 Hungary, 1970 Poland, Solidarity).',
    'If transfers dominate for a period, the seat trends toward symmetric and effective extraction falls; if coercion dominates, the payer reading stands and the nominal-sovereignty gap widens. The weighting is irreducibly evaluative, so resolution may settle the facts without settling the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(working_class_net_position, preference, 'Net structural position of the nominal ruling class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 1917, 1991).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vanguard_rupture_tr_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1917, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1921, 0.25).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1921, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1928, 0.35).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1928, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1937, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1937, 0.6).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1937, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1953, 0.5).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1953, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1968, 0.58).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1968, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1985, 0.52).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1985, observed).
narrative_ontology:measurement(vanguard_rupture_tr_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1991, 0.4).
narrative_ontology:measurement_basis(vanguard_rupture_tr_t1991, observed).

% Extraction over time
narrative_ontology:measurement(vanguard_rupture_be_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1917, 0.42).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1917, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1921, 0.6).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1921, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1928, 0.68).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1928, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1937, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1937, 0.8).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1937, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1953, 0.71).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1953, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1968, 0.74).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1968, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1985, observed).
narrative_ontology:measurement(vanguard_rupture_be_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement_basis(vanguard_rupture_be_t1991, observed).

% Suppression requirement over time
narrative_ontology:measurement(vanguard_rupture_su_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1917, 0.55).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1917, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1921, 0.78).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1921, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1928, 0.72).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1928, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1937, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1937, 0.92).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1937, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1953, 0.65).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1953, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1968, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1985, observed).
narrative_ontology:measurement(vanguard_rupture_su_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement_basis(vanguard_rupture_su_t1991, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, council_communist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Manifesto's revolutionary method' conflates three structurally distinct claims about how proletarian power is constituted and exercised. Decomposed per the epsilon-invariance principle: this story (vanguard rupture) authors the party-seizure and party-guided-transition arrangement with high epsilon; democratic_gradualism_reading authors the electoral-reform arrangement, where pluralists participate and epsilon is substantially lower; council_communist_reading authors the federated-council arrangement, where worker organizations are the coordinated subject rather than victims and party cadres are absent from the beneficiary set. The vanguard reading exerted decisive downstream pressure on the siblings' operating environments - its implementations suppressed council experiments and forced the social-democratic split - which the network edges register as influence between distinct constraints without merging them into one measurable object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
