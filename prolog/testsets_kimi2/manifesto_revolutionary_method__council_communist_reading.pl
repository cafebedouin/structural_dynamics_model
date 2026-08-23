% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Revolutionary Method (Workers' Soviets)
 *   domain: political/philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the council communist reading of the
 *   manifesto_revolutionary_method kernel. It treats workers' councils
 *   (soviets) as direct democratic organs that replace both the capitalist
 *   state and the vanguard party, holding power through federated workplace
 *   assemblies. The reading claims low internal extractiveness (epsilon â
 *   0.25) because coordination among workers is meant to be horizontal and
 *   recallable; however, the constraint actively suppresses the old state and
 *   party apparatus, generating victims among state bureaucrats and party
 *   officials. The claim/metric independence is maintained: the reading
 *   claims a decentralized coordination mechanism while the metrics
 *   acknowledge the asymmetric extraction inherent in revolutionary
 *   dispossession of the old administrative classes. External suppression by
 *   rival readings (vanguardism, gradualism) is documented in the omegas
 *   rather than the base suppression metric, which captures the constraint's
 *   own coercive apparatus.
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary agenda-setter and beneficiary (organized/generational/constrained) â holds direct democratic power through federated assemblies.
 *   - state_bureaucrats: Primary payer (institutional/biographical/trapped) â displaced administrative class of the old state apparatus.
 *   - party_officials: Secondary payer (institutional/biographical/trapped) â vanguard party functionaries stripped of separate directive authority.
 *   - gradualist_reformists: Excluded voice (moderate/generational/constrained) â parliamentary socialists rendered irrelevant by the abolition of electoral state forms.
 *   - council_communist_theorists: Analytical observer (analytical/civilizational/analytical) â theorizes the structural distinction from vanguardism and reformism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.72).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Revolutionary Method (Workers' Soviets)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political/philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '332ea910-4ec8-4037-9566-9c70e3c31e4f').
narrative_ontology:cs_kernel_codification('332ea910-4ec8-4037-9566-9c70e3c31e4f', distributed).
narrative_ontology:cs_authority_grounding('332ea910-4ec8-4037-9566-9c70e3c31e4f', practice).
narrative_ontology:cs_interpretation_layer_present('332ea910-4ec8-4037-9566-9c70e3c31e4f').
narrative_ontology:cs_reading_relation('332ea910-4ec8-4037-9566-9c70e3c31e4f', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('332ea910-4ec8-4037-9566-9c70e3c31e4f', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('332ea910-4ec8-4037-9566-9c70e3c31e4f', foundational, workers_council_sovereignty).
narrative_ontology:cs_axiom_status(workers_council_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('332ea910-4ec8-4037-9566-9c70e3c31e4f', workers_council_sovereignty, deontological).
narrative_ontology:cs_axiom('332ea910-4ec8-4037-9566-9c70e3c31e4f', foundational, anti_bureaucratic_recallability).
narrative_ontology:cs_axiom_status(anti_bureaucratic_recallability, holdable).
narrative_ontology:cs_axiom_grounding('332ea910-4ec8-4037-9566-9c70e3c31e4f', anti_bureaucratic_recallability, instrumental).
narrative_ontology:cs_reference_frame('332ea910-4ec8-4037-9566-9c70e3c31e4f', direct_council_democracy).
narrative_ontology:cs_drift_state('332ea910-4ec8-4037-9566-9c70e3c31e4f', post_revolutionary_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('332ea910-4ec8-4037-9566-9c70e3c31e4f', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, party_officials).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, proletarian_self_emancipation).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, abolition_of_the_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold decision-making power through federated workplace assemblies and immediately recallable delegates. They coordinate production and administration directly, replacing both capitalist management and state bureaucracy. Their exit options are constrained by the revolutionary context and the need to defend the councils against internal restoration and external attack.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary).

% Administrative personnel of the displaced capitalist state apparatus. They lose their positions, privileges, and authority as workers' councils assume administrative functions. They are structurally excluded from the new decision-making bodies and face revolutionary suppression of any attempt to restore state power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Functionaries of the vanguard party who see their claimed leadership role abolished by council sovereignty. The council communist reading denies the party any separate executive or directive power, reducing officials to individual delegates recallable by workplaces. Their hierarchical authority and organizational capital are rendered illegitimate.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, party_officials, payer,
    institutional, biographical, trapped, national).

% Advocates of electoral and parliamentary transition to socialism. They are structurally excluded from the council framework because the councils abolish parliamentary institutions and reject gradual reform as a viable path, rendering the reformist strategy and its representatives irrelevant to the new power structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, gradualist_reformists, excluded,
    moderate, generational, constrained, national).

% Analytical theorists who articulate the council communist framework and distinguish it from vanguardism and reformism. They observe the structural relationship between worker assemblies and state abolition without holding executive power, tracing the historical degeneration of councils into party instruments.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, council_communist_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates production and post-revolutionary governance through direct, recallable workplace delegates in federated assemblies, solving the collective-action problem of administration without recreating a separate bureaucratic or party hierarchy.
% TRANSFER_FUNCTION: Transfers administrative, planning, and coercive authority from the capitalist state apparatus and vanguard party central committees to federated workers' councils and their immediately recallable delegates.
% ABSENT_VOICES: Vanguard party theorists who insist on centralized party leadership, and democratic gradualists who advocate parliamentary reform, are structurally absent because the council framework abolishes both the separate party apparatus and parliamentary institutions as loci of power.
% DISAPPEARANCE_RATIONALE: If the councils vanished overnight, the federated worker democracy would collapse; either the capitalist state would reconstitute, a vanguard party would seize the administrative vacuum, or production coordination would fragment. The specific arrangement of direct workplace sovereignty is load-bearing for the post-revolutionary order this reading proposes.
% FOUNDING_PROBLEM: The problem of proletarian revolution: how the working class can exercise political and economic power without reproducing the bureaucratic state or subordinating itself to a separate party apparatus, and how production can be coordinated democratically after the overthrow of private ownership.
% FOUNDING_PROBLEM_CORROBORATION: Council communist theorists (Pannekoek, Gorter) attest the problem is live and solved only by councils. Independent historians of the Russian and German revolutions corroborate that workers' councils emerged spontaneously to solve immediate coordination problems, but neither the Bolshevik party nor the Weimar state accepted council sovereignty as a permanent solution; thus the problem's status is contested by rival readings and by historical outcomes.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is held low (0.25) because the council system is designed as direct worker self-management with recallable delegates, minimizing internal bureaucratic rent. Suppression is high (0.72) because the constraint's persistence requires actively dismantling the capitalist state and preventing party restoration. Resistance is very high (0.88) because both the displaced state bureaucracy and the vanguard party mount active resistance, and rival revolutionary readings contest the councils' legitimacy. Theater ratio is low (0.20) because the coordination function (production governance) is substantive rather than performative, though some ceremonial aspects of worker democracy may exist. Accessibility collapse is moderate (0.35): alternatives (state restoration, party dictatorship) remain legible and actively advocated by rival readings. The temporal series show extraction stable at low levels, suppression peaking during revolutionary consolidation, and theater gradually rising as institutional habits form.
 *
 * PERSPECTIVAL GAP:
 *   From the worker-collective seat, the constraint is genuine self-emancipation and horizontal coordination. From the bureaucrat and party-official seats, the same structure operates as revolutionary expropriation of their institutional positions. The engine computes this divergence from the structural role and exit data: same constraint, diametrically opposed directionality vectors.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives sit near the beneficiary end (low d): they hold sovereignty, set the agenda, and internalize the coordination gains. State bureaucrats and party officials sit near the full-target end (high d): they bear the costs of dispossession, face trapped exit options, and are actively suppressed. The high resistance metric reflects their organized opposition. Gradualist reformists are excluded rather than targeted, sitting outside the directionality calculation. The theorist observer sits at analytical exit with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the council system as pure extraction (snare) or pure coordination (rope). The genuine coordination functionâsolving post-revolutionary production governance without a separate bureaucracyâis real and acknowledged. The asymmetric extractionâthe dispossession of state and party officialsâis also real. Both are necessary for the classification. If the coordination function atrophied and only the coercive shell remained (e.g., councils became rubber stamps for a new bureaucracy), the constraint would drift toward piton or snare; the measurements guard against this by tracking theater_ratio over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_extraction_or_abolition,
    'Does the displacement of state bureaucrats and party officials by workers'' councils constitute extractive coercion or legitimate revolutionary abolition of class enemies?',
    'Historical analysis of whether displaced officials were integrated, expropriated, or merely removed from power; comparison with other revolutionary transitions and the structural fate of administrative cadres.',
    'If the displacement is read as extractive, the constraint remains tangled_rope or tends toward snare. If read as legitimate abolition of a class relationship, the classification may shift toward scaffold or low-extraction coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_extraction_or_abolition, conceptual, 'Ambiguity over whether revolutionary dispossession counts as extraction or legitimate class abolition.').

omega_variable(
    external_suppression_by_rivals,
    'To what extent does the measured suppression belong to the council constraint itself versus suppression of the councils by rival vanguard and gradualist forces?',
    'Distinguish internal enforcement (councils suppressing counter-revolution) from external attacks (councils being suppressed by party/army); temporal measurement of when suppression was outward-facing versus inward-facing.',
    'If most suppression is external, the constraint''s own extractiveness may be lower than base metrics suggest; if internal, it validates higher suppression and enforcement readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_suppression_by_rivals, empirical, 'Disambiguation of suppression source between the constraint and its rivals.').

omega_variable(
    coordination_degradation_under_stress,
    'Do workers'' councils maintain their coordination function under conditions of civil war and economic collapse, or do they degrade into bureaucratic instruments?',
    'Comparative historical study of council regimes under stress (Russia 1917-18, Germany 1918-19, Hungary 1956), measuring the degree to which delegate recall and workplace autonomy survived or were hollowed out.',
    'If councils consistently degrade under pressure, the constraint may be a scaffold or piton rather than a stable tangled_rope; if they persist, the low epsilon reading is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_degradation_under_stress, empirical, 'Empirical question of whether council democracy survives stress or bureaucratizes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(council_communist_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(council_communist_tr_t6, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(council_communist_tr_t12, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(council_communist_tr_t18, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 18, 0.16).
narrative_ontology:measurement(council_communist_tr_t24, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(council_communist_tr_t30, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(council_communist_tr_t36, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement(council_communist_tr_t42, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 42, 0.24).
narrative_ontology:measurement(council_communist_tr_t48, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 48, 0.26).
narrative_ontology:measurement(council_communist_tr_t54, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 54, 0.28).
narrative_ontology:measurement(council_communist_tr_t60, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(council_communist_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(council_communist_be_t6, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(council_communist_be_t12, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(council_communist_be_t18, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 18, 0.22).
narrative_ontology:measurement(council_communist_be_t24, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(council_communist_be_t30, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(council_communist_be_t36, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 36, 0.25).
narrative_ontology:measurement(council_communist_be_t42, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 42, 0.24).
narrative_ontology:measurement(council_communist_be_t48, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 48, 0.23).
narrative_ontology:measurement(council_communist_be_t54, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 54, 0.22).
narrative_ontology:measurement(council_communist_be_t60, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 60, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(council_communist_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(council_communist_su_t6, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(council_communist_su_t12, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(council_communist_su_t18, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 18, 0.8).
narrative_ontology:measurement(council_communist_su_t24, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(council_communist_su_t30, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(council_communist_su_t36, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 36, 0.78).
narrative_ontology:measurement(council_communist_su_t42, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 42, 0.75).
narrative_ontology:measurement(council_communist_su_t48, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement(council_communist_su_t54, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 54, 0.7).
narrative_ontology:measurement(council_communist_su_t60, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% The manifesto_revolutionary_method kernel decomposes into three structurally distinct constraints: council_communist_reading (direct workplace sovereignty), vanguard_rupture_reading (party state seizure), and democratic_gradualism_reading (electoral reform). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
