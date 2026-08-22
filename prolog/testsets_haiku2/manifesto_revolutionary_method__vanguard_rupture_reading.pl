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
 *   human_readable: Vanguard Party Revolutionary Seizure and Dictatorship of the Proletariat
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates the vanguard reading of the contested kernel
 *   'manifesto_revolutionary_method': the assertion that revolutionary
 *   transformation of capitalism into socialism requires organized seizure of
 *   state power by a disciplined communist party, which then exercises
 *   dictatorship of the proletariat as a transitional form to administer the
 *   restructuring of productive relations. The reading treats the working
 *   class as the nominal beneficiary of revolutionary transformation and the
 *   vanguard party as the agent that executes that transformation on the
 *   working class's behalf. However, the constraint's actual operation
 *   extracts substantial costs from autonomous working-class organizations,
 *   political pluralists, and the former capitalist class, while
 *   concentrating power and privilege in the party apparatus and
 *   state-planning cadres. The measurement trajectory shows extraction
 *   accumulating over the revolutionary period (from 0.45 to 0.68) and
 *   suppression intensifying sharply (from 0.55 to 0.81) before stabilizing,
 *   consistent with a revolution consolidating power and eliminating rivals.
 *   The theater ratio climbs from 0.25 to 0.42, suggesting an increasing
 *   divergence between the revolutionary transformation's ideological
 *   justification and its actual operational structure — the constraint
 *   becomes more about defending the party's monopoly than achieving the
 *   founding problem of working-class emancipation.
 *
 * KEY AGENTS:
 *   - vanguard_party_apparatus: agenda-setter; organizes and executes the revolutionary seizure, administers the dictatorship, monopolizes political authority — institutional power, trapped exit (the party's survival depends on maintaining the dictatorship)
 *   - organized_working_class: nominal beneficiary and practical payer; simultaneously the constituency the revolution claims to liberate and the population whose autonomous organizing is suppressed — organized power but identity-locked exit (rejecting the revolution means rejecting the historical agency the ideology assigns them)
 *   - political_pluralists: payers and victims; lose all institutional voice and are classified as class enemies — powerful actors but trapped by the constraint (emigration is the only exit and is often forbidden)
 *   - autonomous_worker_organizations: payers; councils, syndicates, independent unions are subordinated or destroyed — organized power but constrained exit (adapt by accepting subordination or be suppressed)
 *   - petty_bourgeoisie and former_capitalist_class: victims; expropriated, liquidated as classes — powerful actors but trapped by the constraint (no exit within the territory)
 *   - state_planning_cadres: beneficiaries; new administrative elite gaining authority over centralized production — institutional power, trapped exit (their privilege depends on the constraint's persistence)
 *   - international_capitalist_states: excluded parties; treat the vanguard seizure as existential threat and mount counter-revolutionary intervention — institutional power but excluded from the constraint's decision space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.81).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Revolutionary Seizure and Dictatorship of the Proletariat").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '7ab76857-511e-471e-9498-3765b2d94ea8').
narrative_ontology:cs_kernel_codification('7ab76857-511e-471e-9498-3765b2d94ea8', fixed_text).
narrative_ontology:cs_authority_grounding('7ab76857-511e-471e-9498-3765b2d94ea8', lineage).
narrative_ontology:cs_interpretation_layer_present('7ab76857-511e-471e-9498-3765b2d94ea8').
narrative_ontology:cs_reading_relation('7ab76857-511e-471e-9498-3765b2d94ea8', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ab76857-511e-471e-9498-3765b2d94ea8', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('7ab76857-511e-471e-9498-3765b2d94ea8', foundational, party_vanguard_necessity).
narrative_ontology:cs_axiom_status(party_vanguard_necessity, holdable).
narrative_ontology:cs_axiom_grounding('7ab76857-511e-471e-9498-3765b2d94ea8', party_vanguard_necessity, empirically_contingent).
narrative_ontology:cs_axiom('7ab76857-511e-471e-9498-3765b2d94ea8', foundational, dictatorship_proletariat_transition).
narrative_ontology:cs_axiom_status(dictatorship_proletariat_transition, holdable).
narrative_ontology:cs_axiom_grounding('7ab76857-511e-471e-9498-3765b2d94ea8', dictatorship_proletariat_transition, deontological).
narrative_ontology:cs_reference_frame('7ab76857-511e-471e-9498-3765b2d94ea8', working_class_revolutionary_emancipation).
narrative_ontology:cs_drift_state('7ab76857-511e-471e-9498-3765b2d94ea8', late_20th_century_historical_assessment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ab76857-511e-471e-9498-3765b2d94ea8', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_apparatus).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_cadres).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, petty_bourgeoisie).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, former_capitalist_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, organized_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, organized_working_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seizes state power through organized revolutionary action, claims the mandate of historical materialism and working-class liberation. Administers the dictatorship of the proletariat, centralizes all productive means, and directs the transition to communism. The party's organizational coherence and monopoly on revolutionary legitimacy is the mechanism through which the transformation executes. The apparatus expands as revolutionary functions accumulate — secret police, planning bureaucracy, military command — concentrating power in the party's cadre structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Is the claimed beneficiary of the revolutionary rupture: liberation from wage labor, abolition of the capitalist class, movement toward a classless society. Simultaneously bears the costs of the dictatorship — restrictions on autonomous organizing, subordination to party discipline, labor under state direction rather than capitalist direction. The working class is theoretically sovereign (dictatorship of the proletariat is ostensibly their rule) but practically governed by the vanguard party claiming to represent their historical interests. Exit is identity-locked: rejecting the revolution means rejecting the historical agency and emancipatory role the ideology assigns them.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, organized_working_class, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, organized_working_class, payer).

% Lose all political standing and institutional voice. Liberal democrats, social democrats, and other non-Leninist leftists are classified as class enemies or reformists. Their political organizations are dissolved, their members imprisoned or exiled. They bear the cost of political liquidation — the foreclosure of constitutional frameworks, competitive elections, and autonomous party formation. No exit exists within the territory; emigration is the only option and is often forbidden.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    powerful, biographical, trapped, national).

% Councils, syndicates, and independent trade unions that existed before or emerged during the revolution are subordinated to or destroyed by the vanguard party. Their autonomy is the cost extracted: they become transmission belts for party directives rather than independent organs of worker power. Wildcat strikes, autonomous initiatives, and horizontal organizing are treated as counter-revolutionary. They can adapt by accepting subordination or be suppressed.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, constrained, national).

% Small traders, artisans, and kulaks are expropriated. Their property is nationalized, their economic independence eliminated, their social class liquidated. They are forced into state employment or collectivized production. Resistance results in imprisonment or execution. The petit-bourgeois strata effectively disappears as a social force under the dictatorship.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, petty_bourgeoisie, payer,
    moderate, biographical, trapped, national).

% Is the target of revolutionary expropriation. Their property, enterprises, and political power are seized. They face political elimination, execution, or exile. Those who remain are classified as class enemies and are subject to restriction, surveillance, and potential violence. The constraint's enforcement machinery exists partly to eliminate the capitalist class as a political and economic force.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, former_capitalist_class, payer,
    powerful, biographical, trapped, national).

% The administrative apparatus managing centralized production and resource allocation. They gain authority over economic decisions previously held by dispersed capitalists and markets. They form a new institutional elite, distinct from the working class they ostensibly serve. Career advancement, status, and material privilege flow through the planning hierarchy. They are beneficiaries of the constraint's operation insofar as they occupy monopoly positions on economic decision-making.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_cadres, beneficiary,
    institutional, generational, trapped, national).

% Are excluded from the decision space of the revolutionary state but treat the vanguard seizure as an existential threat to world capitalism. They mount counter-revolutionary intervention, economic blockade, and military pressure to reverse or contain the revolution. Their exclusion from political voice within the revolutionary state does not prevent them from attempting to undo it from without.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, international_capitalist_states, excluded,
    institutional, generational, trapped, global).

% Theorists, historians, and analysts who study the vanguard model, assess its historical implementation, and debate its coherence and outcomes. They provide external analysis of the constraint's structure, beneficiaries, victims, and mandates. Some are partisans of the reading; others are critics; all are positioned outside the operative constraint itself.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_intellectuals, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_apparatus).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how working-class liberation can be achieved from within a capitalist-dominated global order: the vanguard party provides the organized revolutionary force capable of seizing state power and disarticulating capitalist production. Without centralized party organization, the reading asserts, the working class remains fragmented and unable to execute the transformation from capitalism to socialism.
% TRANSFER_FUNCTION: Transfers political power, productive capacity, and economic sovereignty from the capitalist class and their state to the vanguard party administering a planned economy on behalf of the working class. The transfer is enforced: property is seized, bourgeois political structures are dissolved, autonomous working-class organizations are subordinated or destroyed, and alternative political voices are liquidated.
% ABSENT_VOICES: Council communists, who argue workers' direct democracy through federated councils is both the means and end of liberation; democratic socialists, who contend working-class power can be built through electoral majorities and gradual reform; anarcho-syndicalists, who insist autonomous worker organization, not party seizure, is the path to liberation. These readings and the movements they inspire are structurally excluded — their organizational autonomy is treated as counter-revolutionary and is suppressed by the vanguard apparatus.
% DISAPPEARANCE_RATIONALE: If the vanguard party lost its monopoly on state power and the dictatorship of the proletariat were dissolved, the entire structure of centralized planning, party discipline, and revolutionary organization would collapse. Capital would seek restoration, democratic pluralism would re-emerge as a political demand, and autonomous worker organizations would resurface. The revolutionary state apparatus, the planned economy, and the party's institutional dominance are all sustained by this constraint; its removal precipitates wholesale reorganization.
% FOUNDING_PROBLEM: The revolutionary movement faced the strategic question: how can the working class overcome the combined force of the capitalist state, the capitalist class, and the international capitalist order, all of which resist their emancipation? The vanguard reading answers: only through a disciplined, centralized party capable of seizing state power and using that power to restructure the entire social order.
% FOUNDING_PROBLEM_CORROBORATION: Historical materialist theorists and vanguard practitioners attested the founding problem as live and urgent throughout the 20th century. However, historians and critics outside the Leninist tradition — council communists, democratic socialists, anarchists, liberal democrats — dispute whether the founding problem's framing is accurate (whether working-class emancipation REQUIRES revolutionary seizure or can proceed through other means) and whether the vanguard solution actually solves it (whether the dictatorship persists as a transition or becomes a terminal mode of class rule by the new apparatus). The corroboration is heavily one-sided: the beneficiary seats (vanguard party, planning apparatus) assert the problem; the victim seats deny both the problem's framing and the solution's efficacy.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at interval end) because the constraint's operation systematically subordinates working-class autonomous organizing, eliminates political alternatives, and concentrates power in the party apparatus and planning cadres. While the reading frames this as necessary for liberation, the structural reality is that the dictatorship imposes severe costs on multiple constituencies and benefits identifiable power-holders (party apparatus, planning bureaucrats). Suppression is higher still (0.81 at interval end) because the constraint's persistence depends on active coercion: liquidating the bourgeoisie, subordinating autonomous worker organizations, eliminating political pluralism, and defending the party's monopoly on power. The measurements show a sharp intensification of suppression from the initial seizure (0.55) to consolidation (0.81), indicating that as the revolutionary transformation proceeds, active suppressive force must increase to maintain control. Theater ratio grows from 0.25 to 0.42, suggesting the constraint increasingly relies on revolutionary ideology and solidarity theater (mass meetings, party solidarity, historical necessity narratives) to compensate for the growing divergence between the transformation's stated purpose (working-class emancipation) and its actual operation (party and bureaucratic apparatus rule). The claim is tangled_rope (genuine coordination problem solved + asymmetric extraction), and the metrics support this: there is a real coordination function (organizing the revolutionary seizure and restructuring production), but it is bundled with substantial extraction (suppression of alternatives, concentration of power). The beneficiary set is clear and distinct: the vanguard party apparatus and state-planning cadres capture the constraint's operation. The payer set is equally clear: political pluralists, autonomous worker organizations, the petty-bourgeoisie, and the former capitalist class all bear identifiable costs. The working class is theoretically sovereign but practically governed by the apparatus claiming to represent their interests.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (vanguard party) and the identified beneficiaries (party apparatus, planning cadres) experience the constraint as the revolutionary seizure and dictatorship that liberate humanity from capitalist exploitation — a necessary, justified, and temporary state form. From these seats, extraction is invisible; what appears is the coordination function (revolutionary organization and central planning) and the vindication of historical materialism. The payers (political pluralists, autonomous worker organizations, former capitalists) and the identified victims experience the same constraint as coercive suppression of alternatives, concentration of power, and liquidation of rival visions of emancipation. From these seats, the extraction is undeniable; the revolutionary ideology is a cover story for a new form of class rule. The working class (nominal beneficiary + practical payer) is the hinge: ideologically, they are the sovereign beneficiaries of the dictatorship; structurally, they are governed by an apparatus whose interests diverge from theirs. The engine computes these divergences per-seat from the power atoms and exit options; the seat-dependent classification is the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   From the vanguard party's perspective (agenda_setter, institutional power), directionality is near the beneficiary end (d near 0.1–0.3): the party monopolizes political authority, executes the revolution, administers planning, and the apparatus expands as revolutionary functions accumulate. Their exit options are trapped — the revolution's survival depends on maintaining their authority. From the political pluralists' perspective (payer, powerful power but excluded from decision space), directionality is near the target end (d near 0.8–0.9): they are classified as class enemies, liquidated politically, and have no voice in the constraint's operation. Their only exit is emigration, which is often forbidden. From the autonomous worker organizations' perspective (payer, organized power), directionality is also high-target (d near 0.75–0.85): they are subordinated or destroyed, their autonomy is extracted as the cost of the revolution, and their exit is constrained to adaptation (acceptance of subordination) or suppression. From the organized working class's perspective (nominal beneficiary + practical payer), directionality is ambiguous and identity-locked (d near 0.5, with high identity-locking): they are theoretically the beneficiaries of the dictatorship but practically governed by it; rejecting the revolution means rejecting the historical agency the ideology assigns them, so exit is identity-locked even though costs are real. State planning cadres (beneficiary, institutional) occupy a beneficiary position (d near 0.2–0.4) analogous to the vanguard party. The engine computes these divergences from the stakeholder power atoms, exit options, and beneficiary/victim declarations; the authored claim (tangled_rope) and metrics (high extractiveness, high suppression) are independent and structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to overcome capitalist domination through working-class emancipation) is theoretically live from the vanguard perspective — the party maintains that the dictatorship is a transitional state form moving toward that goal. However, from the victim and observer seats, the founding problem's status is contested and arguably dead: the dictatorship persists and shows no trajectory toward withering away; working-class autonomy has been liquidated, not liberated; and alternative visions of emancipation have been suppressed rather than transcended. The measurement trajectory (extraction rising from 0.45 to 0.68, suppression rising from 0.55 to 0.81) shows the constraint accumulating extraction and suppressive force rather than transitioning to a lower-extraction regime. This contradicts the claim that the dictatorship is temporary; the data suggests it is terminal or at least far more durable than the reading predicts. The mandatrophy emerges here: the founding problem's function (organizing the transition from capitalism to socialism) is claimed but not evidenced; what persists is the extractive apparatus (party rule, suppression of alternatives, concentration of power in planning cadres). The theater ratio's rise (0.25 to 0.42) indicates increasing performance around the founding problem: revolutionary legitimacy and historical necessity narratives must be invoked more aggressively as the actual operation diverges from the stated purpose. This is a classic mandatrophy signature — the constraint persists past the point where the founding problem is solved, and maintenance requires theatrical invocation of that problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_or_terminal_dictatorship,
    'Is the dictatorship of the proletariat a transitional state form that withers away as class distinctions dissolve, or does it become a terminal mode of party rule?',
    'Historical trajectory of vanguard states: if power diffuses and state apparatus contracts over decades, the transition reading holds; if the party apparatus persists and centralizes further, the terminal reading gains support. No vanguard state has yet dissolved its dictatorship in the theoretically predicted manner.',
    'If terminal, the constraint reclassifies from transitional scaffolding to durable extraction (snare or piton). If genuinely transitional, the high suppression is justified as temporary; if terminal, it becomes unjustifiable extraction masked by revolutionary ideology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_or_terminal_dictatorship, empirical, 'Whether the dictatorship persists as a transition or becomes the permanent structure of class rule.').

omega_variable(
    party_representation_authenticity,
    'Does the vanguard party authentically represent working-class interests, or does it constitute a new ruling class with distinct interests divorced from the workers it claims to represent?',
    'Examination of whose interests the party apparatus actually serves: whether party cadres'' material conditions and decision-making power remain aligned with the working class, or diverge into a separate privileged stratum. Worker testimony, party composition analysis, and deviation between party theory and actual outcomes.',
    'If authentically representative, the extraction is justified as the cost of revolution and planning. If the party constitutes a new ruling class, the constraint reclassifies as a snare: the beneficiaries (party apparatus, planning cadres) are distinct from the nominal beneficiaries (working class), and suppression serves the new class''s domination, not emancipation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_representation_authenticity, empirical, 'Whether the vanguard apparatus remains aligned with working-class interests or constitutes a new exploiting class.').

omega_variable(
    reading_vs_sibling_foundational_incommensurability,
    'Does the vanguard reading''s core assertion—that revolutionary seizure of state power by a disciplined party is necessary and desirable—logically foreclose the council communist or democratic gradualist readings, or do these readings remain live options that different parties simply choose differently?',
    'Logical analysis of the readings'' foundational premises. If vanguardism asserts that ONLY party seizure can achieve emancipation (council communism and gradualism are objectively impossible), then it forecloses. If vanguardism asserts it is the BEST path but acknowledges alternatives are theoretically viable (merely suboptimal), then it coexists.',
    'If foreclosure: the kernel cannot sustain multiple readings within any coherent framework—one reading must be true and the others false by logical necessity. If coexistence: the readings remain live positions held by different revolutionary movements, and their competition is a feature of the contested kernel, not a resolution of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foundational_incommensurability, conceptual, 'The logical structure of vanguardism''s relationship to sibling revolutionary readings.').

omega_variable(
    class_composition_during_dictatorship,
    'Under the dictatorship of the proletariat, what is the class composition of the vanguard party apparatus? Are its members drawn from the working class and subject to its discipline, or do they constitute a separate administrative stratum?',
    'Sociological study of party membership, cadre origins, material privileges, career trajectories, and disciplinary structure. Do party members live and work as workers, or as a privileged bureaucratic elite?',
    'If the party apparatus is continuous with the working class, the beneficiary/victim split is less clear. If the apparatus is a separate elite, the identification of beneficiaries shifts from ''working class'' to ''party apparatus,'' and the constraint''s extraction becomes more apparent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_composition_during_dictatorship, empirical, 'Whether the vanguard party apparatus is composed of workers or a distinct administrative class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(mani_tr_t15, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(mani_tr_t25, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(mani_be_t15, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(mani_be_t25, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(mani_su_t15, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(mani_su_t25, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.18).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'manifesto_revolutionary_method'. The kernel hosts three competing claims about how revolutionary emancipation is to be achieved. This story (vanguard_rupture_reading) asserts party seizure and transitional dictatorship. council_communist_reading asserts workers' councils and direct democracy. democratic_gradualism_reading asserts electoral majority and parliamentary reform. Each has distinct ε values, beneficiary structures, and suppression profiles. They are related by common origin (all read the same kernel) and structural influence (each reading shapes how the others must defend themselves), but they are NOT variations on a single constraint — they are distinct constraints generated by distinct readings, with distinct classifications. Link them via network.affects_constraints to enable the engine's contamination propagation and comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
