% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Vanguard Party Seizure of State Power as Revolutionary Method
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint instantiates the vanguard_rupture_reading of the
 *   manifesto_revolutionary_method kernel: the claim that revolutionary
 *   transformation requires an organized vanguard party to seize state power
 *   and exercise a dictatorship of the proletariat through party guidance.
 *   The reading presents this as a necessary coordination mechanism for
 *   overthrowing capitalist state power and administering the transition to
 *   socialism. The structural reality over 1917–1991 shows a constraint that
 *   began with genuine coordination function (overthrowing the Provisional
 *   Government, defending against counter-revolution) but rapidly accumulated
 *   extractive overhead: party cadres became a privileged stratum, the
 *   state-planning apparatus extracted surplus from workers and peasants, and
 *   alternative revolutionary pathways (workers' councils, left oppositions,
 *   democratic socialist currents) were systematically suppressed. The
 *   claimed_type is tangled_rope because the constraint retains a genuine
 *   coordination function (state power seizure and administration) while
 *   simultaneously extracting asymmetrically from political pluralists and
 *   autonomous worker organizations.
 *
 * KEY AGENTS:
 *   - party_cadres: Primary beneficiary (institutional/arbitrage) — controls state apparatus, accesses privileges, career advancement through party
 *   - state_planning_apparatus: Primary beneficiary (institutional/arbitrage) — administers economy, controls resource allocation, staffed by party loyalists
 *   - party_central_committee: Agenda setter (institutional/arbitrage) — sets political line, controls appointments, enforces discipline
 *   - political_pluralists: Primary victim (powerless/trapped) — banned parties, suppressed publications, excluded from soviets
 *   - autonomous_worker_organizations: Primary victim (organized/trapped) — factory committees, independent unions subordinated or dissolved
 *   - independent_trade_unions: Victim (organized/constrained) — brought under party control, struck-breaking prohibited
 *   - left_opposition_factions: Victim (powerless/trapped) — expelled, exiled, executed (Trotskyists, Workers Opposition, etc.)
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.72).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.85).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power as Revolutionary Method").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '8b2cd977-d4c0-4e11-a066-233a331545f5').
narrative_ontology:cs_kernel_codification('8b2cd977-d4c0-4e11-a066-233a331545f5', formalized).
narrative_ontology:cs_authority_grounding('8b2cd977-d4c0-4e11-a066-233a331545f5', extraction).
narrative_ontology:cs_interpretation_layer_present('8b2cd977-d4c0-4e11-a066-233a331545f5').
narrative_ontology:cs_reading_relation('8b2cd977-d4c0-4e11-a066-233a331545f5', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('8b2cd977-d4c0-4e11-a066-233a331545f5', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('8b2cd977-d4c0-4e11-a066-233a331545f5', foundational, vanguard_party_necessary_for_revolution).
narrative_ontology:cs_axiom_status(vanguard_party_necessary_for_revolution, holdable).
narrative_ontology:cs_axiom_grounding('8b2cd977-d4c0-4e11-a066-233a331545f5', vanguard_party_necessary_for_revolution, instrumental).
narrative_ontology:cs_axiom('8b2cd977-d4c0-4e11-a066-233a331545f5', foundational, dictatorship_of_proletariat_equals_party_dictatorship).
narrative_ontology:cs_axiom_status(dictatorship_of_proletariat_equals_party_dictatorship, holdable).
narrative_ontology:cs_axiom_grounding('8b2cd977-d4c0-4e11-a066-233a331545f5', dictatorship_of_proletariat_equals_party_dictatorship, conventional).
narrative_ontology:cs_axiom('8b2cd977-d4c0-4e11-a066-233a331545f5', secondary, democratic_centralism_as_organizational_principle).
narrative_ontology:cs_axiom_status(democratic_centralism_as_organizational_principle, holdable).
narrative_ontology:cs_axiom_grounding('8b2cd977-d4c0-4e11-a066-233a331545f5', democratic_centralism_as_organizational_principle, conventional).
narrative_ontology:cs_reference_frame('8b2cd977-d4c0-4e11-a066-233a331545f5', leninist_vanguardism_1917).
narrative_ontology:cs_drift_state('8b2cd977-d4c0-4e11-a066-233a331545f5', post_stalin_thaw_1956, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8b2cd977-d4c0-4e11-a066-233a331545f5', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, independent_trade_unions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, left_opposition_factions).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_necessity_thesis).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, dictatorship_of_proletariat_as_party_rule).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_centralism_organizing_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Full-time party functionaries who control appointments, access special stores/housing/medical care, and advance through nomenklatura system. Their material interests align with party-state preservation. Exit within system: move between party, state, economic posts. Exit from system: defection (rare, high cost) or waiting for collapse.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres, beneficiary,
    institutional, biographical, arbitrage, national).

% Gosplan and ministry officials who allocate resources, set production targets, control distribution. Staffed by party members; their authority derives from plan fulfillment. They benefit from control over material flows and privilege access. Exit: lateral moves within planning system; outside exit blocked by specialization and political vetting.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, agenda_setter).

% Top decision-making body setting political line, controlling appointments, enforcing democratic centralism. Members are the supreme beneficiaries of the constraint — they design and administer the system they benefit from. Exit: virtually none (purges, not resignations). Their situation is the constraint's designer-administrator seat.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_central_committee, agenda_setter,
    institutional, generational, arbitrage, national).

% Members of banned parties (SRs, Mensheviks, anarchists, liberal democrats), independent journalists, dissident intellectuals. Face arrest, exile, psychiatric incarceration, job loss, social isolation for political activity. No legal organizational space. Exit: emigration (restricted), silence, or resistance (high cost). Their political agency is the extraction target.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    powerless, biographical, trapped, local).

% Factory committees, workplace assemblies, independent unions (e.g., Kronstadt sailors, Workers Opposition, Solidarity in Poland). Initially part of revolutionary base; subordinated when they challenged party decisions. Their organizational autonomy is extracted; they become transmission belts for party directives. Exit: dissolution, co-option, or underground resistance (crushed).
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, local).

% Trade unions brought under party control (All-Union Central Council of Trade Unions). Strikes prohibited; unions become welfare/administration arms of management. Workers pay dues but get no collective bargaining. Exit: none legally; wildcat strikes occur but are repressed. Their dues and labor discipline are extracted.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, independent_trade_unions, payer,
    organized, biographical, constrained, national).

% Bolshevik factions opposing party line from left (Trotskyists, Workers Group, Democratic Centralists). Expelled from party, exiled, imprisoned, executed. Their revolutionary credentials make them priority targets — they contest the vanguard's monopoly on revolutionary legitimacy. Exit: capitulation (recantation), exile, or death.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, left_opposition_factions, payer,
    powerless, biographical, trapped, national).

% Historical analyst examining the 1917–1991 trajectory across all seats. Sees the full structural pattern: genuine coordination function at founding, extraction accumulation, suppression intensification, theater ratio rise, eventual collapse. No material stake; classification is the output.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of overthrowing a capitalist state and defending the revolution against counter-revolutionary and imperialist forces through centralized command, and administers rapid industrialization of a backward agrarian economy via state planning.
% TRANSFER_FUNCTION: Moves political autonomy, organizational independence, and surplus labor from the working class and its autonomous organizations to the party-state apparatus; moves material privileges, decision-making power, and career advancement from the party-state to party cadres and planning officials.
% ABSENT_VOICES: Workers' councils (soviets) as autonomous organs — suppressed 1917–1921. Peasant communes and cooperatives — collectivized by force 1929–1933. Anarchist and left-communist currents — excluded from the revolutionary tradition. Their absence is structural: the constraint's enforcement machinery exists to keep them out.
% DISAPPEARANCE_RATIONALE: If the vanguard party constraint vanished overnight (as in 1989–1991), the world rearranges: party cadres scramble for new positions (many become oligarchs), planning apparatus dissolves into market mechanisms, political pluralists emerge from underground, autonomous worker organizations reappear (Solidarity), left oppositions publish openly. The entire political-economic structure reorganizes.
% FOUNDING_PROBLEM: Overthrowing the bourgeois state and defending the revolution in a backward, war-torn country; industrializing rapidly to survive imperialist encirclement; creating a socialist economy without capitalist developmental stage.
% FOUNDING_PROBLEM_CORROBORATION: Party loyalists (official histories, communist parties) attest the problem remains live: imperialism persists, capitalist restoration is the threat. Victims and independent historians (Getmanov, Figes, Kotkin, Applebaum) attest the founding military-industrial problem was solved by 1953; the arrangement persisted as bureaucratic self-preservation. No corroboration outside the beneficiary set for the claim that the founding problem required the specific form of one-party dictatorship through 1991.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.72) is high: the party-state extracts surplus labor, political autonomy, and organizational independence from the working class it claims to represent, while concentrating material privileges and decision-making power in the party apparatus. Suppression (0.85) is very high: alternative revolutionary pathways are not merely discouraged but criminalized; the one-party monopoly is maintained by secret police, show trials, and institutional exclusion. Theater ratio (0.38) reflects that the coordination function (revolutionary defense, industrialization) is real but increasingly performed by a structure whose primary orientation is self-preservation. Accessibility collapse (0.78) is high but not total: council communist and democratic socialist alternatives persisted intellectually and in exile, but were structurally inaccessible within the constraint's scope. Resistance (0.62) is substantial: workers' uprisings (Kronstadt 1921, East Germany 1953, Hungary 1956, Poland 1980) show the constraint meets active opposition, though repression contains it.
 *
 * PERSPECTIVAL GAP:
 *   From the party_central_committee seat (agenda_setter, institutional, arbitrage exit), the constraint appears as necessary coordination: the only viable path to defend the revolution and build socialism. From political_pluralists and autonomous_worker_organizations (victims, powerless/trapped), the same structure appears as snare: extraction of their revolutionary agency and organizational forms. From party_cadres (beneficiary, institutional, arbitrage), the constraint delivers real material benefits (privileges, career, status) alongside ideological commitment — a genuine beneficiary seat. The engine computes per-seat classification from these structural positions; the claimed_type does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: party_cadres, state_planning_apparatus, party_central_committee — these groups control the constraint's operation and capture its material returns (privileges, resources, power). Victims declared: political_pluralists, autonomous_worker_organizations, independent_trade_unions, left_opposition_factions — these groups bear the costs (suppression, exclusion, extraction of autonomy) and have trapped or constrained exit. The party_central_committee is the agenda_setter (d ≈ 0.05). Party_cadres and state_planning_apparatus are beneficiaries with arbitrage-grade exit within the system (d ≈ 0.15). Political_pluralists and left_opposition_factions are identity-locked or trapped targets (d ≈ 0.95). Autonomous_worker_organizations and independent_trade_unions are organized but constrained targets (d ≈ 0.80). No directionality overrides needed; beneficiary/victim + exit derives correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overthrowing capitalism, defending revolution, industrializing backward economies) was live in 1917. By 1953 the military-industrial defense problem was substantially solved; by 1985 the industrialization problem was solved. The arrangement persisted as extraction (nomenklatura privileges, bureaucratic self-preservation) without its founding coordination function. The founding_problem_status is contested: party loyalists claim the problem remains live (imperialist encirclement, ideological struggle); victims and analysts say dead. The constraint reclassified from tangled_rope toward snare/piton over the interval as coordination function atrophied and extraction persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vanguard_reading_vs_kernel_ambiguity,
    'Is the vanguard_rupture_reading a structurally distinct constraint from the manifesto_revolutionary_method kernel, or does it inherit the kernel''s ambiguity about whether party seizure of power is necessary coordination or extractive capture?',
    'Decompose the kernel into its three readings (this one, council_communist_reading, democratic_gradualism_reading) and measure ε, beneficiaries, victims for each independently. If this reading''s ε and victim structure differ systematically from the siblings'', it is a distinct constraint.',
    'If distinct, the kernel label ''manifesto_revolutionary_method'' is a false summit covering three constraints with different classifications. If not distinct, the readings are measurement perspectives on one constraint — but the ε-invariance principle says different ε = different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vanguard_reading_vs_kernel_ambiguity, conceptual, 'Whether the kernel conflates structurally distinct constraints').

omega_variable(
    coordination_function_persistence,
    'Did the constraint retain any genuine coordination function after 1953 (post-Stalin, post-industrialization), or had it become pure extraction (snare) or inertial performance (piton)?',
    'Measure whether the party-state solved coordination problems that no other structure could (e.g., nuclear deterrence, space program, disaster response) versus problems it created to justify itself. Compare outcomes in sibling-reading regimes (democratic socialist, council communist) where they existed.',
    'If coordination function persisted, tangled_rope classification holds through the interval. If not, the constraint drifted to snare (active suppression of alternatives) or piton (theatrical maintenance). The temporal measurements show rising theater_ratio and stable high extractiveness — suggestive of drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_persistence, empirical, 'Whether the tangled_rope''s coordination component atrophied').

omega_variable(
    suppression_mechanism_internalized,
    'Was the suppression of political pluralists and autonomous worker organizations primarily structural (secret police, bans) or also internalized (workers believing party rule is their only defense, ideological identification with the vanguard)?',
    'Post-1991 trajectory: if suppression persists as ideological self-censorship or nostalgia for strong leadership after structural apparatus dissolves, internalized component was significant. Compare with council_communist_reading regimes (none survived) and democratic_gradualism_reading (suppression lower, internalized component different).',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint''s extraction persists after formal collapse. Affects classification of post-1991 successor constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression in vanguard party rule').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 1917, 1991).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1917, 0.12).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1921, 0.18).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1928, 0.25).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1936, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1936, 0.42).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1953, 0.45).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1968, 0.4).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1985, 0.39).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_tr_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1991, 0.38).

% Extraction over time
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1921, 0.52).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1928, 0.58).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1936, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1936, 0.71).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1953, 0.75).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1968, 0.73).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_be_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1991, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1917, 0.65).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1921, 0.72).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1928, 0.81).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1936, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1936, 0.92).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1953, 0.88).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1968, 0.85).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1985, 0.86).
narrative_ontology:measurement(manifesto_revolutionary_method__vanguard_rupture_reading_su_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1991, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% The manifesto_revolutionary_method kernel decomposes into three constraint stories with distinct ε, beneficiaries, victims. This reading (vanguard_rupture) has high ε (0.72), party beneficiaries, pluralist victims. Council communist reading has lower ε, council beneficiaries, party victims. Democratic gradualism has lowest ε, parliamentary beneficiaries, revolutionary victims. All three linked as constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
