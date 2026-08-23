% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualist Path to Socialism
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   The democratic gradualist reading of revolutionary method claims
 *   socialism is achievable through electoral majority and institutional
 *   reform within liberal democracy. It emerged from the Second
 *   International's adaptation to universal suffrage and legal party
 *   activity. The constraint coordinates working-class politics through
 *   parliamentary institutions but extracts by suppressing revolutionary
 *   alternatives and forcing compromise with bourgeois state forms.
 *   Beneficiaries are the social democratic parties, unions, and politicians
 *   who gain institutional positions; victims are revolutionary militants and
 *   radical factions marginalized as 'adventurist.' The ε-moderate (0.40)
 *   reflects genuine coordination (peaceful reforms won) combined with
 *   asymmetric extraction (radical energy captured, transformation deferred).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualist Path to Socialism").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '268a8659-9bf0-4538-9ccc-bc541cf9e3c3').
narrative_ontology:cs_kernel_codification('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', fixed_text).
narrative_ontology:cs_authority_grounding('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', lineage).
narrative_ontology:cs_interpretation_layer_present('268a8659-9bf0-4538-9ccc-bc541cf9e3c3').
narrative_ontology:cs_reading_relation('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', foundational, parliamentary_road_viable).
narrative_ontology:cs_axiom_status(parliamentary_road_viable, holdable).
narrative_ontology:cs_axiom_grounding('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', parliamentary_road_viable, empirically_contingent).
narrative_ontology:cs_axiom('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', foundational, reform_accumulates_to_transformation).
narrative_ontology:cs_axiom_status(reform_accumulates_to_transformation, holdable).
narrative_ontology:cs_axiom_grounding('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', reform_accumulates_to_transformation, empirically_contingent).
narrative_ontology:cs_reference_frame('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', second_international_orthodoxy).
narrative_ontology:cs_drift_state('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', post_1914_collapse, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('268a8659-9bf0-4538-9ccc-bc541cf9e3c3', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, reformist_politicians).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_voters).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, radical_left_factions).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_organizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, bourgeois_parties).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_voters).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_road_to_socialism).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, reformist_transformation_thesis).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_majority_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set electoral strategy and legislative agenda; control party machinery and parliamentary fractions; justify compromise as realism. Their institutional position depends on maintaining the electoral route as the only legitimate path.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).

% Gain legal recognition, collective bargaining rights, and social legislation through reformist politics. Their leaderships are integrated into the party apparatus; radical rank-and-file currents are disciplined through bureaucratic structures.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Build careers within parliamentary systems; acquire ministerial positions and state patronage. Their political capital depends on demonstrating that socialism is achievable through legislation, not rupture.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, reformist_politicians, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, reformist_politicians, agenda_setter).

% Receive incremental reforms (wage increases, social insurance, labor protections) but face constrained political choices: vote reformist or risk reaction. Their radical impulses are channeled into electoral mobilization for parties that limit demands to the achievable.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_voters, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_voters, payer).

% Organize outside or against the parliamentary road; labeled 'adventurist' or 'ultra-left' by reformist leadership. Face expulsion from parties, exclusion from unions, and police repression that reformist governments tolerate or enable. Their identity fuses with the rupture they advocate — exit means abandoning their self-conception as the vanguard.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    moderate, biographical, identity_locked, national).

% Operate within or adjacent to social democratic parties pushing for more radical policies. Subject to disciplinary procedures, marginalization in candidate selection, and rhetorical denunciation as 'irresponsible.' Their exit options are constrained by the absence of viable alternative organizations at scale.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, radical_left_factions, payer,
    moderate, biographical, constrained, national).

% Build workplace councils and federated assemblies as dual power structures. Denounced by both reformists and vanguard parties as 'syndicalist deviation.' Their suppression is structural: the electoral route they reject is the only one with state recognition and resources.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_organizers, excluded,
    powerless, biographical, trapped, local).

% Accept limited reforms to preserve the capitalist state form; use the gradualist constraint as a pressure valve that deflects revolutionary rupture. Their cooperation is the price of legislative passage — the constraint cannot function without their periodic acquiescence.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, bourgeois_parties, beneficiary,
    institutional, generational, arbitrage, national).

% Analyze the constraint's historical trajectory from Bernstein through the Second International's collapse to contemporary social democracy. They see the full structure: coordination function (peaceful reform), extraction (suppressed alternatives), and the identity locks that bind militants to defeated strategies.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, political_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates working-class political action through electoral institutions, providing a peaceful, legal path to incremental reform that avoids civil war and counter-revolutionary violence.
% TRANSFER_FUNCTION: Moves political capital and institutional access from radical alternatives to reformist parties; moves policy concessions from bourgeois parties to working class through legislative compromise; moves organizational resources from autonomous workplace struggle into party/union bureaucracies.
% ABSENT_VOICES: Revolutionary militants, council communists, and anarchist groups are structurally excluded from mainstream political discourse and labor movement leadership. They would object that the constraint forecloses the only path to genuine socialist transformation, but their exclusion is what makes the constraint operable — their presence would shatter the electoral consensus.
% DISAPPEARANCE_RATIONALE: If the democratic gradualist constraint vanished overnight, the left political field would reorganize around either revolutionary rupture (accelerating confrontation with state power) or political resignation (demobilization). The constraint structures the entire spectrum of working-class politics; its disappearance forces a binary choice that the constraint exists to avoid.
% FOUNDING_PROBLEM: How to achieve socialist transformation without triggering civil war, counter-revolutionary dictatorship, or the authoritarian deformation of the workers' movement itself — the problem posed by the Paris Commune's defeat and the Bismarckian state's consolidation.
% FOUNDING_PROBLEM_CORROBORATION: Reformist leaders (Bernstein, Kautsky pre-1914, contemporary social democrats) attest the problem remains live — capitalism's stability requires gradual transformation. Revolutionary theorists (Luxemburg 1900, Lenin 1905, Pannekoek 1930s) attested the problem was dead — reform accumulates into counter-reform. Contemporary historical sociologists (Therborn, Esping-Andersen) find the empirical record mixed: reforms won, transformation not achieved.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.40) is moderate: the constraint delivers real reforms but captures the radical surplus that might push beyond them. Suppression (0.45) is structural: electoral thresholds, party discipline, and state repression of extra-parliamentary action maintain the constraint. Theater (0.35) rises over time as radical rhetoric masks reformist practice. Accessibility collapse (0.55) is partial: revolutionary alternatives exist but are marginalized, not eliminated. Resistance (0.50) comes from both right (anti-socialist laws) and left (revolutionary critiques). The measurement grid uses a shared 0-150 time scale (years since Gotha Program 1875) with all metrics authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (social democratic parties), the constraint is a rope — genuine coordination achieving historic reforms. From the payer seats (revolutionary militants, council communists), it is a snare — their energy is extracted to stabilize capitalism. From the analytical seat, it is a tangled rope — both coordination and extraction are real, neither reducible to the other. The engine computes this divergence from the declared structural data; the claimed_type (tangled_rope) represents the analytical seat's judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties are structural beneficiaries (d ≈ 0.15) — they collect institutional rents and agenda control. Trade unions and reformist politicians are beneficiaries (d ≈ 0.25) with some payer aspects (compromise costs). Working-class voters sit near symmetric (d ≈ 0.45) — genuine gains but constrained horizon. Revolutionary militants are identity-locked targets (d ≈ 0.90) — their self-concept fuses with the rupture the constraint suppresses. Council communists are trapped (d ≈ 0.95) — no institutional exit exists. Bourgeois parties are arbitrage-grade beneficiaries (d ≈ 0.10) — they extract stability from the constraint. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding Commune-style defeat) was live in 1875. By 1914 the war-credits vote revealed the constraint had become a snare for internationalism. Post-1945, the constraint managed capitalism rather than transforming it — mandatrophy unresolved. The constraint persists because no party bears the cost of fixing it: reformists benefit, revolutionaries are too weak to replace it, bourgeois parties prefer it to rupture. This is the classic piton dynamic, but with active enforcement maintaining the electoral road — hence tangled_rope, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_capture,
    'Is the democratic gradualist constraint a genuine coordination mechanism that delivers the maximum achievable socialism under liberal democracy, or a capture mechanism that channels radical energy into reforms that stabilize capitalism?',
    'Counterfactual historical comparison: did social democratic governments in power (Germany 1918, Sweden 1930s, Chile 1970, Mitterrand 1981) attempt structural transformations that were blocked by extra-parliamentary forces, or did they voluntarily halt at the boundary of capitalist property relations?',
    'If capture, the constraint is a snare with a coordination cover story; if genuine coordination, the moderate ε reflects real structural limits, not bad faith. Changes classification from tangled_rope toward snare or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture, empirical, 'Whether the constraint''s coordination function is authentic or a cover for extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of revolutionary alternatives structural (electoral systems, party law, state repression) or internalized (reformist ideology, career incentives, identity fusion with the parliamentary road)?',
    'Post-exit suppression trajectory: track militants who leave reformist parties — do they face continued structural barriers, or does suppression persist primarily through internalized belief that ''there is no alternative''?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. If structural, suppression drops sharply on exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism for revolutionary militants.').

omega_variable(
    founding_problem_liveness,
    'Does the founding problem (avoiding civil war/counter-revolution) remain live given modern state capacity for surveillance and repression, or has it become a cover story for reformist self-preservation?',
    'Compare revolutionary rupture outcomes (Russia 1917, China 1949, Cuba 1959) with reformist outcomes (Nordic model, Allende, Syriza) on metrics: working-class power durability, socialist transformation depth, human cost. If rupture consistently produces worse outcomes on all metrics, the founding problem is live.',
    'If dead, the constraint''s persistence is pure mandatrophy — it solves a problem that no longer exists (or never existed in the form claimed). If live, the constraint retains genuine coordination justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_liveness, preference, 'Whether the historical justification for gradualism remains valid under contemporary conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrm_dgr_tr_t0, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mrm_dgr_tr_t25, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(mrm_dgr_tr_t50, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(mrm_dgr_tr_t75, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 75, 0.33).
narrative_ontology:measurement(mrm_dgr_tr_t100, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(mrm_dgr_tr_t125, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 125, 0.38).
narrative_ontology:measurement(mrm_dgr_tr_t150, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 150, 0.35).

% Extraction over time
narrative_ontology:measurement(mrm_dgr_be_t0, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mrm_dgr_be_t25, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(mrm_dgr_be_t50, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(mrm_dgr_be_t75, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement(mrm_dgr_be_t100, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(mrm_dgr_be_t125, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 125, 0.42).
narrative_ontology:measurement(mrm_dgr_be_t150, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 150, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mrm_dgr_su_t0, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(mrm_dgr_su_t25, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(mrm_dgr_su_t50, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(mrm_dgr_su_t75, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(mrm_dgr_su_t100, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(mrm_dgr_su_t125, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 125, 0.48).
narrative_ontology:measurement(mrm_dgr_su_t150, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 150, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.08).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the single label 'revolutionary method' into three structurally distinct constraints with different ε values (democratic_gradualism: 0.40, vanguard_rupture: 0.65, council_communist: 0.30), different beneficiary/victim structures (parties/unions vs. party cadres vs. workplace assemblies), and different institutional logics (parliamentary, insurrectionary, councilist). The ε-invariance principle requires separate stories because the observable 'revolutionary success' means different things in each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, moderate, 0.45).
constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
