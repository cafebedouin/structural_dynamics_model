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
 *   human_readable: Vanguard Party Seizure and Party-Guided Proletarian Dictatorship (Transitional State Form)
 *   domain: political philosophy/revolutionary theory/historical materialism
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the manifesto_revolutionary_method
 *   kernel: the vanguard-rupture reading — revolutionary transformation
 *   requires organized party seizure of state power, exercised as a
 *   dictatorship of the proletariat under party guidance during the
 *   transition. The sibling readings (democratic_gradualism_reading,
 *   council_communist_reading) are separate constraints in the same family
 *   and are deliberately NOT described or averaged into this file; per the
 *   epsilon-invariance principle this story authors one stable epsilon for
 *   the standing arrangement this reading produces: the party-guided
 *   transitional state as historically instantiated across the Soviet record
 *   (1917-1991). That arrangement holds a genuine coordination function —
 *   consolidating power against armed enemies, directing industrialization
 *   and war mobilization — fused with a strongly asymmetric structure: party
 *   cadres and the planning apparatus concentrate decision rights, status,
 *   and material privilege, while pluralist currents, autonomous worker
 *   organizations, the peasantry, and in practice the proletariat itself bear
 *   the costs. The claimed type (tangled_rope) and the metrics below are
 *   authored independently: the metrics describe the arrangement's actual
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and any divergence between the claim and a computed seat
 *   is the measurement this corpus exists to take. KEY AGENTS (by structural
 *   relationship): - vanguard_party_cadres: Agenda-setter and primary
 *   beneficiary (institutional / identity_locked) — sets and enforces the
 *   arrangement, collects status and material privilege through party rank -
 *   state_planning_apparatus: Secondary beneficiary (institutional /
 *   constrained) — administers plan fulfillment, collects administrative
 *   authority and sectoral command - soviet_proletariat: Formal sovereign,
 *   effective payer (organized / trapped) — bears labor discipline and plan
 *   targets while receiving the social wage - soviet_peasantry: Primary
 *   surplus payer (moderate / trapped) — surplus requisitioned and
 *   collectivized, mobility restricted for a generation -
 *   political_pluralists: Suppressed alternative (powerless / constrained) —
 *   bear arrest, exile, silencing; emigration is the costly partial exit -
 *   autonomous_worker_organizations: Suppressed rival coordination (organized
 *   / trapped) — councils and strike committees dismantled, leaders jailed -
 *   counterrevolutionary_opponents: Emergency justification seat (organized /
 *   mobile) — the referent of extraordinary measures; exits by emigration -
 *   comparative_historians_of_revolution: Analytical observer (analytical /
 *   analytical) — sees the full structure, collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.74).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure and Party-Guided Proletarian Dictatorship (Transitional State Form)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political philosophy/revolutionary theory/historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d').
narrative_ontology:cs_kernel_codification('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', fixed_text).
narrative_ontology:cs_authority_grounding('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', lineage).
narrative_ontology:cs_interpretation_layer_present('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d').
narrative_ontology:cs_reading_relation('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_reading_relation('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', foundational, organized_seizure_necessity).
narrative_ontology:cs_axiom_status(organized_seizure_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', organized_seizure_necessity, instrumental).
narrative_ontology:cs_axiom('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', foundational, party_guided_proletarian_dictatorship).
narrative_ontology:cs_axiom_status(party_guided_proletarian_dictatorship, holdable).
narrative_ontology:cs_axiom_grounding('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', party_guided_proletarian_dictatorship, instrumental).
narrative_ontology:cs_axiom('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', secondary, transitional_state_withering).
narrative_ontology:cs_axiom_status(transitional_state_withering, holdable).
narrative_ontology:cs_axiom_grounding('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', transitional_state_withering, empirically_contingent).
narrative_ontology:cs_reference_frame('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', vanguard_party_transitional_dictatorship).
narrative_ontology:cs_drift_state('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', late_soviet_mature_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c08b08a4-cf9c-4ff3-a68a-13090b1c3e8d', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, soviet_peasantry).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, soviet_proletariat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, soviet_proletariat).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, counterrevolutionary_opponents).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, class_instrument_state_theory).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_centralism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Full-time party functionaries staffing the Central Committee, regional committees, and ideological apparatus. They set policy, control appointments through the nomenklatura system, and enforce discipline from the center to the provinces. Material privileges — special housing, closed stores, privileged healthcare — attach to party rank, and careers advance only inside the party. Leaving means losing position, livelihood, and social identity, and at certain periods personal safety; membership is the frame through which they understand their own lives.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, agenda_setter,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, beneficiary).

% Gosplan officials, ministers, and sectoral administrators who translate party decisions into plan targets and administer their fulfillment. They receive command authority over entire industries and the status that accompanies control of resources. Their careers exist only inside the state-economic machine; they can rotate among posts but cannot take their function outside it.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, agenda_setter).

% Industrial workers, the class in whose name the arrangement claims to act. They receive the social wage — guaranteed employment, housing, education, healthcare — and the formal status of the leading class. Within the first years they lost the strike weapon and the independent factory committees they had built; labor is directed by plan targets and managerial authority backed by party control. They cannot exit: internal passports, labor discipline, and the absence of anywhere else to go bind them in place.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, soviet_proletariat, payer,
    organized, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, soviet_proletariat, beneficiary).

% The rural majority. Surplus moves from the countryside to the state through requisitioning and then collectivization, enforced by requisition detachments, deportation of 'kulak' households, and the machine-tractor station system. Peasants received internal passports last and could not leave their villages freely for a generation. Their horizon is the land across generations; their resistance — Tambov, passive slowdown, slaughter of livestock — met deportation and famine.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, soviet_peasantry, payer,
    moderate, generational, trapped, continental).

% Members of banned or tolerated-then-crushed parties and currents — Mensheviks, Right and Left SRs, later dissidents and human-rights monitors — who hold that political competition and legal opposition should continue inside or alongside the transformation. They face arrest, exile, or psychiatric detention. Some emigrate at the cost of homeland and language; the rest go silent, or into internal exile.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    powerless, biographical, constrained, continental).

% Independent councils, factory committees, and strike committees holding that workers should rule directly through their own assemblies rather than through a guiding party. The Kronstadt soviet, the Workers' Opposition, and later strike committees are instances. Their organizations are dismantled, their leaders jailed or exiled, and their members cannot leave — the factory and the territory are the whole world they have.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, continental).

% Armed and political opponents of the revolutionary order — White armies and their civilian backers, intervention forces, later wartime collaborators. They are the referent of the emergency that justifies extraordinary measures. Most of the founding generation exits by emigration; those who remain face tribunals and confiscation. As that generation dies off, the category is maintained by doctrine rather than by present danger.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, counterrevolutionary_opponents, payer,
    organized, biographical, mobile, continental).

% Scholars and analysts outside the arrangement who compare its trajectory with other revolutionary and reformist paths across archives, emigre testimony, and comparative cases. They see the full structure from the outside, collect nothing from it, and bear none of its costs.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, comparative_historians_of_revolution, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a revolutionary class seizes and holds state power against armed counter-revolution, foreign intervention, and economic collapse: decisions about war, production, and political survival are made centrally by a disciplined party rather than through pluralist competition, market allocation, or federated direct democracy. The same machinery coordinates rapid industrialization and continental-scale war mobilization.
% TRANSFER_FUNCTION: Moves political decision rights from pluralist institutions, councils, and the population at large to the party center; moves economic surplus from the countryside and the workforce to state-directed industrialization; and moves status and material privilege to those holding party rank.
% ABSENT_VOICES: The council-communist current and the democratic-gradualist current are the structurally absent voices. Both were present in the early revolutionary moment — Menshevik and SR deputies, the Workers' Opposition inside the party, the Kronstadt soviet — and were suppressed, exiled, or silenced. In the mature arrangement they hold no seat; their objection, that the party-guided state replaces worker power rather than embodying it, survives only in emigration and samizdat.
% DISAPPEARANCE_RATIONALE: If the party-guided state arrangement vanished overnight, the entire political and economic structure built on it would rearrange: the planned economy loses its command center, the nomenklatura loses its position, the suppressed currents re-enter politics, and the union's constituent nations reorganize their governance. The sequence of 1989-1991 demonstrated exactly this rearrangement when enforcement lapsed.
% FOUNDING_PROBLEM: How does a revolutionary class consolidate power long enough to transform society when facing armed counter-revolution, foreign intervention, economic collapse, and the demonstrated fragility of council-based direct democracy under emergency conditions? The arrangement was built to answer: through a disciplined party that seizes the state and exercises dictatorial power during the transition.
% FOUNDING_PROBLEM_CORROBORATION: The party attests the emergency remained live (capitalist encirclement, then war). Outside the beneficiary set: the council-communist critique from inside and outside the party (Workers' Opposition documents, Kronstadt resolutions, later Castoriadis and Solidarity groups), Menshevik emigre analysis, and the mainstream historiography of the period all attest that the founding emergency — armed counter-revolution and collapse — was resolved by roughly 1921 while the arrangement persisted another seventy years; the party's own 1956 secret speech conceded terror excesses without conceding the point. Corroboration for the 'problem resolved, arrangement persisted' reading is broad and independent; the 'emergency remained live' reading is attested almost solely by the arrangement's own beneficiaries.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.68: the arrangement's costs are decoupled from the coordination it funds — political monopoly, plan targets, and requisitioning move surplus and decision rights to the apparatus regardless of service rendered. Suppression 0.74 is authored as a raw structural value, unscaled by power or scope: the arrangement's persistence required the Cheka-OGPU-NKVD line, the 1921 ban on factions, the one-party monopoly, and the dismantling of independent councils — suppression is what holds the structure, not an incidental byproduct. Theater_ratio 0.56: the 'dictatorship of the proletariat' label persisted for decades after the proletariat held no direct power; single-candidate elections, rubber-stamp soviets, and ideological ritual performed a consent the structure no longer generated — though the state's administrative and war-fighting functions remained real, keeping the ratio below the purely performative range. Accessibility_collapse 0.60: within the territory, alternative pathways (other parties, independent councils, market coordination) collapsed almost completely, but the sibling readings survived in emigration, samizdat, and foreign parties, so collapse is high without being natural-law complete. Resistance 0.62: Kronstadt, Tambov, the Workers' Opposition, East Germany 1953, Hungary 1956, Czechoslovakia 1968, and the 1980s strike movement — the arrangement met sustained resistance across its whole life. The base values reflect the mature-phase plateau (roughly 1929-1968), not the 1917 founding or the 1991 collapse endpoints; the measurement series carries the trajectory on one shared time grid. Claimed type tangled_rope: the arrangement holds BOTH a real coordination function (no rival arrangement industrialized under siege conditions comparably) AND asymmetric extraction through the same structure, held by active enforcement — all three tangled-rope structural conditions are authored (beneficiaries, victims, requires_active_enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute different types from the same structure. From the cadres' seat the arrangement is the necessary instrument of transformation and the source of everything they are — identity, career, meaning; exit is unthinkable without self-annihilation. From the pluralist and worker-organization seats the same structure is the enforced removal of their coordination forms: the party-guided state is precisely what replaced their councils. Inter-institutional dynamics: the party and the planning apparatus are distinct institutional seats on the same side with different exits (identity_locked versus constrained) and different relationships to the agenda — the party sets, the apparatus administers and collects; the apparatus's loyalty depends on the plan's continuation, the cadres' on the party's monopoly. Same-level actor dynamics: political_pluralists (powerless) and autonomous_worker_organizations (organized) occupy the same suppressed side at different power levels — organization capacity is what got the worker organizations crushed first, while the pluralists' atomization left them harassed rather than destroyed; and the two 'organized' seats on opposite sides (worker organizations versus the proletariat formally) are differentiated by role and exit, not by global standing. The proletariat's seat is internally split — formally the sovereign class, actually subordinated — so its computed position should sit between the apparatus and the suppressed currents.
 *
 * DIRECTIONALITY LOGIC:
 *   Party cadres are declared beneficiaries holding the agenda-setter role with identity-locked exit: their derived directionality sits near the beneficiary end, though not at zero — the terror consumed cadres too, and their net position is beneficiary-with-risk. The planning apparatus is a declared beneficiary with constrained exit: low d, slightly above the cadres' because it collects status and authority without setting the political agenda. Political pluralists, autonomous worker organizations, the peasantry, and the proletariat are declared victims; trapped or constrained exit pushes the worker organizations, peasantry, and proletariat toward the full-target end, while the pluralists' emigration possibility moderates theirs slightly. Counter-revolutionary opponents are authored as a stakeholder but deliberately left out of the victim arrays: they are the referent of the founding emergency rather than parties to the standing arrangement, and their seat is what the suppression's justification points at. The analytical observer's seat is neutral. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct structure without them, and the two 'organized' seats on opposite sides are disambiguated by their array declarations rather than by power-atom overrides, which could not distinguish them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — consolidating revolutionary power against armed counter-revolution and collapse — was resolved by roughly 1921; the arrangement persisted seventy more years under successively reframed mandates (socialist construction, encirclement, the Great Patriotic War, 'developed socialism'). The doctrine itself contained the obsolescence promise: the transitional state was supposed to 'wither away,' which functions as an unoperationalized sunset clause — no mechanism, no conditions, no date, and every internal attempt to act on the promise (the Workers' Opposition, Bukharin's line, reform communism, the Prague Spring) was crushed; correspondingly, has_sunset_clause is authored false. The tangled_rope classification is what prevents mislabeling in both directions: a pure-coordination reading would erase the pluralists, worker organizations, and peasantry who paid through the same structure that coordinated; a pure-extraction reading would erase the genuine coordination — defense against armed enemies, industrialization at siege speed, war mobilization — that no rival arrangement replicated. The classification holds both truths in one structure, which is what the historical record shows; the founding_problem_status 'contested' plus the disappearance verdict 'world_rearranges' carries the mandatrophy signal that the superseded boolean field once did.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vanguard_reading_contest_omega,
    'Is the vanguard-rupture form entailed by the revolutionary-method kernel itself, or is it one contested reading whose adoption is a structural choice that creates this arrangement''s specific beneficiary and victim sets?',
    'Comparative analysis across the kernel''s three readings where each was attempted: the council experiments of 1918-1921, electoral-socialist trajectories in Scandinavia and interwar Czechoslovakia, and the vanguard instantiation — tracking which beneficiary and victim structures each reading actually produced.',
    'If the kernel does not entail the vanguard form, this arrangement''s measured extraction is attributable to this reading''s structural choices (centralization, suppression of the sibling readings) rather than to revolutionary transformation as such; the family''s other two files carry the counterfactual structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vanguard_reading_contest_omega, conceptual, 'This constraint is one reading of a contested kernel; sibling readings instantiate different constraints with different epsilon and different party structures.').

omega_variable(
    founding_emergency_necessity_ambiguity,
    'Was the suppression of pluralist and autonomous-worker alternatives a genuine necessity of the founding emergency, or did the emergency become a justification that outlived its referent and hardened into the arrangement''s maintenance cost?',
    'Correlate suppression intensity against measured external threat across the interval: the 1921 ban on factions preceded any renewed emergency; the NEP years tolerated markets while banning factions; the 1936 ''victory of socialism'' declaration coincided with the terror''s peak rather than its relaxation.',
    'If necessity, part of the measured suppression is the price of the coordination itself and net extraction falls; if justification-outliving-referent, the suppression is enforcement of the apparatus''s position and the arrangement drifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_emergency_necessity_ambiguity, empirical, 'Structural necessity versus cover story for the suppression machinery.').

omega_variable(
    withering_promise_status,
    'Is the ''withering away of the state'' a genuine transitional trajectory the arrangement was on, or a promise with no operational mechanism — making the transitional self-description part of the arrangement''s maintenance performance?',
    'Track the doctrine''s own withering indicators (coercion scale, party monopoly scope, state personnel growth) against its predictions at each declared milestone (1936 constitution, 1961 party programme): every milestone declared the transition advanced while coercion and personnel grew.',
    'If self-perpetuating, the transitional framing is theatrical maintenance and the arrangement is a steady-state structure misdescribed as transitional; if genuinely transitional, part of the measured theater reflects an unrealized rather than false promise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(withering_promise_status, conceptual, 'Whether the transitional self-description tracks any real trajectory.').

omega_variable(
    cadre_persistence_mechanism,
    'Is cadre persistence driven by material interest (nomenklatura privilege, career dependence) or by identity fusion (party membership as the constitutive self-concept), and in what proportion?',
    'Post-1991 cadre trajectory: if former cadres preserved the arrangement''s logic in new institutional clothing (conversion of position into property, successor-party continuity), identity fusion dominated; if they dispersed into ordinary careers, material interest dominated.',
    'If identity-fused, the arrangement''s persistence exceeds what its material rewards alone explain and the identity_locked exit atom is load-bearing for the per-seat computation; if material, dismantling the privilege system would have sufficed to dissolve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cadre_persistence_mechanism, empirical, 'Material versus identity mechanism for cadre persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 1917, 1991).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1917, 0.12).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1917, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1921, 0.3).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1921, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1929, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1929, 0.42).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1929, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1937, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1937, 0.55).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1937, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1953, 0.5).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1953, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1968, 0.54).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1968, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1985, 0.58).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1985, observed).
narrative_ontology:measurement(vanguard_rupture_reading_tr_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1991, 0.56).
narrative_ontology:measurement_basis(vanguard_rupture_reading_tr_t1991, observed).

% Extraction over time
narrative_ontology:measurement(vanguard_rupture_reading_be_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1917, 0.4).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1917, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1921, 0.54).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1921, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1929, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1929, 0.66).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1929, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1937, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1937, 0.78).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1937, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1953, 0.73).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1953, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1968, 0.71).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1968, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1985, 0.69).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1985, observed).
narrative_ontology:measurement(vanguard_rupture_reading_be_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1991, 0.68).
narrative_ontology:measurement_basis(vanguard_rupture_reading_be_t1991, observed).

% Suppression requirement over time
narrative_ontology:measurement(vanguard_rupture_reading_su_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1917, 0.45).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1917, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1921, 0.62).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1921, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1929, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1929, 0.74).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1929, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1937, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1937, 0.85).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1937, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1953, 0.72).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1953, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1968, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1968, 0.68).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1968, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1985, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1985, observed).
narrative_ontology:measurement(vanguard_rupture_reading_su_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement_basis(vanguard_rupture_reading_su_t1991, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto's revolutionary method' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This file (vanguard_rupture_reading) carries the high-epsilon instantiation: the party-guided transitional state, with party cadres and the planning apparatus as beneficiaries and pluralists, autonomous worker organizations, the peasantry, and the proletariat as victims. The sibling files carry different epsilon and different party structures: democratic_gradualism_reading (no seizure apparatus forms; the extraction profile stays closer to ordinary democratic politics) and council_communist_reading (federated workplace councils hold power; the cadre beneficiary class this file creates becomes the suppressed seat instead). The upstream/downstream structure runs from this file to both siblings: the vanguard reading historically suppressed the council reading's institutional expression wherever it held power (subordination of the soviets, Kronstadt) and discredited the gradualist reading inside the movement as revisionism — so this file's network edges point at both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
