% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism Reading of the Revolutionary Method Question
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This story authors the democratic gradualism reading of the contested
 *   'revolutionary method' kernel drawn from the Manifesto and its subsequent
 *   political traditions: the claim that socialism is achievable through
 *   winning electoral majorities and reforming existing state institutions
 *   from within, rather than through party-led seizure of state power (the
 *   vanguard rupture reading, a separate constraint) or replacement of the
 *   state by federated workers' councils (the council communist reading, also
 *   separate). Each reading is authored as its own ε-invariant constraint
 *   with its own beneficiary/victim structure per the ε-invariance principle;
 *   this file covers only the gradualist reading.
 *
 * KEY AGENTS:
 *   - social_democratic_parties: agenda-setting institutional beneficiary administering the electoral strategy
 *   - trade_union_bureaucracies: organized beneficiary whose bargaining role depends on labor peace
 *   - parliamentary_left_politicians: powerful beneficiary whose careers are staked on the gradualist path
 *   - revolutionary_militant_factions: powerless payer, denounced and sometimes suppressed as adventurist
 *   - unorganized_precarious_workers: powerless payer excluded from the organized constituency the strategy serves
 *   - colonized_and_peripheral_workers: powerless global payer outside the metropolitan electorate entirely
 *   - capitalist_class_and_state_apparatus: excluded structural constraint the reading works within rather than confronts
 *   - labor_historians_and_political_theorists: analytical observer assessing the empirical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism Reading of the Revolutionary Method Question").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '5ec0ce3c-ce07-4582-a12d-39f11207f68e').
narrative_ontology:cs_kernel_codification('5ec0ce3c-ce07-4582-a12d-39f11207f68e', fixed_text).
narrative_ontology:cs_authority_grounding('5ec0ce3c-ce07-4582-a12d-39f11207f68e', lineage).
narrative_ontology:cs_interpretation_layer_present('5ec0ce3c-ce07-4582-a12d-39f11207f68e').
narrative_ontology:cs_reading_relation('5ec0ce3c-ce07-4582-a12d-39f11207f68e', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ec0ce3c-ce07-4582-a12d-39f11207f68e', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('5ec0ce3c-ce07-4582-a12d-39f11207f68e', foundational, existing_state_institutions_are_neutral_instruments).
narrative_ontology:cs_axiom_status(existing_state_institutions_are_neutral_instruments, holdable).
narrative_ontology:cs_axiom_grounding('5ec0ce3c-ce07-4582-a12d-39f11207f68e', existing_state_institutions_are_neutral_instruments, empirically_contingent).
narrative_ontology:cs_axiom('5ec0ce3c-ce07-4582-a12d-39f11207f68e', foundational, electoral_majority_is_sufficient_transfer_of_class_power).
narrative_ontology:cs_axiom_status(electoral_majority_is_sufficient_transfer_of_class_power, holdable).
narrative_ontology:cs_axiom_grounding('5ec0ce3c-ce07-4582-a12d-39f11207f68e', electoral_majority_is_sufficient_transfer_of_class_power, empirically_contingent).
narrative_ontology:cs_reference_frame('5ec0ce3c-ce07-4582-a12d-39f11207f68e', mass_suffrage_parliamentary_socialism).
narrative_ontology:cs_drift_state('5ec0ce3c-ce07-4582-a12d-39f11207f68e', post_welfare_state_retrenchment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ec0ce3c-ce07-4582-a12d-39f11207f68e', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_left_politicians).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militant_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, unorganized_precarious_workers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, colonized_and_peripheral_workers).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, peaceful_transition_thesis).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, institutional_neutrality_of_the_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the electoral apparatus, party discipline, and the doctrine that socialism is achieved through winning parliamentary majorities and administering existing state institutions. They set which tactics count as legitimate (electoral campaigning, coalition-building) and which are 'adventurist' (strikes escalating to insurrection, dual-power organs). Their organizational survival depends on continued participation in the electoral system they administer.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, beneficiary).

% Negotiate wages and conditions through institutionalized collective bargaining that depends on labor peace and legal recognition. Benefit from the gradualist framework because it legitimizes their mediating role between workers and capital; a revolutionary rupture would bypass or dissolve their negotiating function entirely.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, beneficiary,
    organized, biographical, constrained, national).

% Build careers, salaries, and institutional standing within legislatures on the premise that meaningful change flows through electoral office. Their personal and professional trajectories are staked on the gradualist path being correct; a revolutionary reading would render their entire vocation obsolete or complicit.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_left_politicians, beneficiary,
    powerful, biographical, mobile, national).

% Organize for direct confrontation with state power — strikes escalating to occupation, armed self-defense, dual-power councils. Are routinely denounced by the gradualist parties and unions as 'adventurist,' 'ultra-left,' or agents provocateurs, isolated from mainstream labor support, and sometimes handed over to state repression by the same parties that claim to represent the working class.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militant_factions, payer,
    powerless, biographical, trapped, national).

% Work outside union coverage — gig, informal, migrant, and casualized labor. The gradualist strategy is built around and negotiates for the organized unionized core, leaving these workers without electoral or bargaining representation; gradual reform's pace and priorities are set by constituencies that do not include them.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, unorganized_precarious_workers, payer,
    powerless, immediate, trapped, national).

% Live under imperial or neocolonial relations that metropolitan social-democratic reform leaves largely intact, since parliamentary majorities are won within national electorates that have no mandate over colonies or dependent economies. Gradualist governments have historically maintained or managed colonial arrangements rather than dismantling them, since doing so was not required to win domestic elections.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, colonized_and_peripheral_workers, payer,
    powerless, generational, trapped, global).

% Not a party to the kernel dispute in the sense of holding a reading, but structurally shapes its outcome: they retain control of the coercive apparatus, capital flight options, and constitutional veto points, meaning gradualist electoral victories can be reversed, hollowed out, or subjected to capital strikes without a single vote changing. Their continued institutional position is what the gradualist reading takes as fixed and works within rather than confronts.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, capitalist_class_and_state_apparatus, excluded,
    institutional, civilizational, analytical, national).

% Study the empirical record of social-democratic governments (interwar Europe, postwar welfare states, Popular Front experiments) to assess whether gradual reform achieved structural transformation of ownership relations or primarily redistributed within a stable capitalist framework.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, labor_historians_and_political_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared electoral strategy that lets a dispersed working class translate numerical majority into state power without triggering civil war, capital flight panic, or foreign intervention that a frontal seizure of power would likely provoke; it also coordinates labor and party organizations around a single legitimate channel of action, avoiding fragmentation across competing tactics.
% TRANSFER_FUNCTION: Moves organizational resources, working-class electoral loyalty, and legitimacy claims toward parties and unions that administer the gradualist strategy, and moves the cost of enforcing tactical discipline (denunciation, isolation, sometimes state collaboration against militants) onto factions that reject the electoral-only frame.
% ABSENT_VOICES: Revolutionary militants are present but delegitimized rather than genuinely absent from the room; colonized and peripheral workers are structurally absent because the electorate the strategy is built around is the metropolitan national electorate, which has no representative mechanism for them at all.
% DISAPPEARANCE_RATIONALE: If the gradualist reading disappeared as the dominant strategic frame within the workers' movement, party and union structures built around electoral participation and collective bargaining would lose their strategic rationale overnight; militant and council-based factions currently marginalized as 'adventurist' would face a legitimacy vacuum they could contest for, and the historical alliance between organized labor and parliamentary politics would need to be renegotiated or replaced.
% FOUNDING_PROBLEM: Late-19th and early-20th century mass suffrage extension created a genuine question the Manifesto's insurrectionary language had not anticipated: could a numerically dominant working class simply outvote the bourgeoisie, making armed seizure of power unnecessary or even counterproductive given the risks of repression and civil war?
% FOUNDING_PROBLEM_CORROBORATION: Social-democratic parties and union federations attest the strategy remains live and vindicated by postwar welfare-state achievements. Independent labor historians outside these organizations (e.g., analyses of the SPD's 1918-1919 role in suppressing the German revolution, and comparative studies of Popular Front governments) attest that gradualist parties, once positioned within state power, frequently used that position to demobilize or suppress the revolutionary wing of their own movement rather than to advance beyond capitalism — corroboration that is genuinely external to the beneficiary parties and points toward the founding problem being reframed rather than resolved.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε = 0.40, moderate per the expected structural delta) reflects that gradualism does deliver real, measurable gains to organized labor through legislation and collective bargaining — this is not zero-sum extraction — but it also systematically channels working-class political energy and legitimacy toward electorally-oriented institutions at the cost of tactics and constituencies outside that frame. Suppression (0.45) is moderate and enforced institutionally (expulsion from party lists, denunciation in labor press, occasional active collaboration with state repression against militants) rather than through direct coercion — this is suppression of an internal rival tendency, not suppression of an external enemy. Theater ratio rises over the interval (0.20 to 0.42) reflecting the historical pattern where gradualist parties increasingly substitute rhetorical commitment to eventual socialism for programs that meaningfully move ownership relations, especially once the party has become invested in maintaining its position within existing state structures. Suppression requirement falls in the middle of the interval (representing periods of genuine coalition-building and electoral success, e.g. postwar welfare-state construction) before rising again toward the end as electoral gains stagnate and internal discipline against dissenting factions hardens.
 *
 * DIRECTIONALITY LOGIC:
 *   Social-democratic parties, union bureaucracies, and parliamentary politicians are declared beneficiaries because the gradualist frame is the basis of their institutional existence, revenue, and legitimacy — d sits near the beneficiary end. Revolutionary militants are declared victims not because gradualism directly extracts economic value from them but because the doctrine's enforcement (denunciation, isolation, occasional handover to state repression) actively closes off their preferred tactical path and treats their organizing as illegitimate — trapped exit options and powerless power atom push d toward the target end. Unorganized precarious and colonized/peripheral workers are victims through structural absence rather than active suppression: the gradualist strategy's design center (the organized national electorate) does not include them, so gains flow past them regardless of the strategy's success on its own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — whether mass suffrage makes armed seizure of power unnecessary — was genuinely live at the point of origin (universal male suffrage extension, first workers' parties entering parliaments) and remains contested rather than dead: electoral socialist parties have won and held state power in multiple contexts, which is real evidence for the reading's viability, but the historical record of those parties suppressing or demobilizing their own revolutionary wings once in power (the founding_problem_corroboration) suggests the arrangement may function to convert working-class movements into stable partners of the existing state order rather than as a transitional path beyond it. This is exactly the divergence the tangled_rope classification is built to hold: a genuine coordination function (avoiding catastrophic civil conflict, translating majority into policy) coexists with asymmetric extraction (marginalizing tactics and constituencies that fall outside the electorally-organized core) enforced through active institutional discipline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gradualism_vanguard_reading_choice,
    'Is the democratic gradualism reading of the revolutionary method question a genuinely viable independent strategic path, or is it structurally a captured/domesticated variant of the vanguard rupture reading that emerges only once revolutionary energy has already been defeated or absorbed?',
    'This is the committer-axis choice itself: which reading of the kernel a given historical social-democratic party or theorist is instantiating. Resolvable in a given historical case by examining whether the party''s electoral turn preceded or followed a defeated insurrectionary attempt, and whether the party actively participated in suppressing that attempt (as with the SPD and the Spartacist uprising).',
    'If gradualism is shown to characteristically follow and help suppress prior revolutionary attempts rather than being an independently chosen strategy from the outset, its coordination-function claim weakens substantially relative to its extraction/suppression function, pushing the classification toward snare from the militants'' seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gradualism_vanguard_reading_choice, conceptual, 'Whether gradualism is an independent strategic reading or a post-defeat domestication of the revolutionary reading.').

omega_variable(
    electoral_reversibility_ambiguity,
    'Given that capitalist states retain constitutional veto points, capital flight capacity, and coercive apparatus regardless of electoral outcomes, can electoral majorities alone ever achieve socialism, or does the gradualist reading''s success depend on a tacit agreement never to test that limit?',
    'Historical case analysis of instances where socialist electoral governments attempted structural transformation beyond redistribution (nationalization of core industry, worker control of production) and tracking whether they were reversed through capital strikes, coups, or constitutional crisis (e.g. Chile 1973, comparative European postwar nationalization retreats).',
    'If electoral socialist governments are reliably reversed or constrained whenever they approach genuine transformation of ownership relations, the gradualist reading''s ε is understated at 0.40 — the coordination function may be cover for a structural ceiling that the reading''s proponents have not tested and cannot test without ceasing to be gradualist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_reversibility_ambiguity, empirical, 'Whether electoral gradualism has an untested structural ceiling enforced by non-electoral capitalist state power.').

omega_variable(
    national_electorate_scope_ambiguity,
    'Is the gradualist reading''s exclusion of colonized and peripheral workers a contingent historical failure of particular social-democratic parties, or a structural feature of any strategy organized around winning majorities within a bounded national electorate?',
    'Comparative analysis of whether any social-democratic government pursuing the gradualist strategy has ever extended its transformative program to colonial or dependent territories outside its own electorate, versus always treating such territories as outside the scope of domestic reform.',
    'If the exclusion is structural rather than contingent, colonized_and_peripheral_workers should be understood as permanent, definitional victims of this reading rather than victims of implementation failure — strengthening the tangled_rope classification''s asymmetric extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_electorate_scope_ambiguity, conceptual, 'Whether the reading''s neglect of colonial/peripheral workers is structural or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(mani_tr_t60, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(mani_tr_t80, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(mani_tr_t100, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(mani_be_t60, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(mani_be_t80, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(mani_be_t100, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(mani_su_t60, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(mani_su_t80, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(mani_su_t100, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the manifesto_revolutionary_method kernel. The vanguard_rupture_reading and council_communist_reading are separate constraint files with their own ε values, beneficiary/victim structures, and classifications. This file's gradualist reading treats revolutionary militants as victims of enforced tactical discipline; the vanguard_rupture_reading, by contrast, would treat gradualist reformists as the obstacle to genuine transformation, with a correspondingly different beneficiary/victim structure and likely higher ε given its more overtly coercive theory of transition. The council_communist_reading rejects both party forms (electoral and vanguard) in favor of council federation, and would author yet a third distinct beneficiary/victim structure. All three share the historical raw material of debates within the workers' movement but are structurally distinct constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
