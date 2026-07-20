% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Vanguard Party Dictatorship as Revolutionary Transitional State
 *   domain: political/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates the vanguard-rupture reading of the
 *   revolutionary-method kernel: the claim that revolutionary transformation
 *   requires organized seizure of state power by a professional revolutionary
 *   party, which then guides the dictatorship of the proletariat as a
 *   transitional state form. Historically operationalized in the Soviet model
 *   and its derivatives, the constraint coordinates industrialization and
 *   defense against counter-revolution while extracting political authority
 *   and economic surplus from autonomous worker organizations, political
 *   pluralists, and the broader proletariat. The coordination story
 *   (transitional defense of the revolution) and the extraction story
 *   (permanent party-state monopoly) are structurally inseparable, producing
 *   high asymmetric extraction sustained by active suppression of alternative
 *   pathways.
 *
 * KEY AGENTS:
 *   - vanguard_party_cadres: Agenda-setter (institutional/constrained) â monopolizes political authority and ideological adjudication
 *   - state_planning_apparatus: Beneficiary (institutional/constrained) â captures bureaucratic rents from centralized allocation
 *   - political_pluralists: Payer (moderate/trapped) â bears suppression as excluded political competitors
 *   - autonomous_worker_organizations: Payer (powerless/trapped) â bear dismantling of independent labor power
 *   - industrial_proletariat: Dual-positioned payer/beneficiary (organized/identity_locked) â nominally the ruling class, structurally subordinated
 *   - revolutionary_theorists: Observer (analytical/analytical) â documents drift and compares readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.72).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.88).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Dictatorship as Revolutionary Transitional State").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, '1ae4892a-ccc2-460f-9107-f30ba6a23012').
narrative_ontology:cs_kernel_codification('1ae4892a-ccc2-460f-9107-f30ba6a23012', formalized).
narrative_ontology:cs_authority_grounding('1ae4892a-ccc2-460f-9107-f30ba6a23012', lineage).
narrative_ontology:cs_interpretation_layer_present('1ae4892a-ccc2-460f-9107-f30ba6a23012').
narrative_ontology:cs_reading_relation('1ae4892a-ccc2-460f-9107-f30ba6a23012', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ae4892a-ccc2-460f-9107-f30ba6a23012', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('1ae4892a-ccc2-460f-9107-f30ba6a23012', foundational, vanguard_party_necessity).
narrative_ontology:cs_axiom_status(vanguard_party_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1ae4892a-ccc2-460f-9107-f30ba6a23012', vanguard_party_necessity, instrumental).
narrative_ontology:cs_axiom('1ae4892a-ccc2-460f-9107-f30ba6a23012', foundational, state_power_seizure_imperative).
narrative_ontology:cs_axiom_status(state_power_seizure_imperative, holdable).
narrative_ontology:cs_axiom_grounding('1ae4892a-ccc2-460f-9107-f30ba6a23012', state_power_seizure_imperative, instrumental).
narrative_ontology:cs_reference_frame('1ae4892a-ccc2-460f-9107-f30ba6a23012', revolutionary_party_supremacy).
narrative_ontology:cs_drift_state('1ae4892a-ccc2-460f-9107-f30ba6a23012', late_socialist_stagnation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ae4892a-ccc2-460f-9107-f30ba6a23012', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, dictatorship_of_proletariat_doctrine).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, historical_materialism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolize political decision-making, control state organs and security apparatus, and justify their monopoly as the historically necessary leadership of the transitional dictatorship of the proletariat. They set the ideological line, purge dissenting factions, and administer the coercive machinery that suppresses alternative political organization.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres, agenda_setter,
    institutional, generational, constrained, global).

% Manages centralized economic planning, labor allocation, and surplus extraction under party guidance. Benefits from expanded bureaucratic authority and resource control that the vanguard monopoly makes possible. Its personnel overlap with party cadres, creating a fused party-state managerial class.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Advocate for multi-party competition, parliamentary socialism, or competitive elections. Are systematically excluded from legal political life, imprisoned, exiled, or denounced as counter-revolutionary. No exit from suppression exists within the territorial state; emigration is the only partial escape and is often prevented.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Independent trade unions, factory committees, or strike committees that operate outside party control. Are dismantled, co-opted into party-controlled unions, or criminalized. Their members face dismissal, arrest, or worse for unauthorized bargaining or solidarity actions.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    powerless, biographical, trapped, national).

% Designated in doctrine as the ruling class exercising dictatorship, but structurally denied independent political voice or autonomous organization. Receives social guarantees, employment security, and ideological recognition in exchange for labor discipline, suppressed wages, and subordination to party-directed production targets. Class identity is fused with party loyalty, making exit psychologically and socially costly.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, industrial_proletariat, beneficiary).

% Analyze the structural gap between the revolutionary promise of proletarian emancipation and the institutional reality of permanent party-state bureaucracy. Document drift from transitional dictatorship toward bureaucratic class rule, and compare the vanguard model with competing revolutionary strategies across historical contexts.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__vanguard_rupture_reading, vanguard_party_cadres).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__vanguard_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes political authority in a disciplined vanguard party to coordinate revolutionary transformation, suppress counter-revolutionary forces, and direct state-led economic reorganization during the transition from capitalism to socialism.
% TRANSFER_FUNCTION: Transfers political authority, economic surplus, and autonomous organizational capacity from independent worker bodies, political competitors, and the general population to the vanguard party cadres and the state-planning bureaucracy.
% ABSENT_VOICES: Anarchist collectives, syndicalists, council communists, and independent social democrats who reject party monopoly are structurally excluded from the theoretical framework and from institutionalized political life; their absence is enforced through banning, co-optation, or theoretical dismissal as reformist or petty-bourgeois.
% DISAPPEARANCE_RATIONALE: Without the vanguard party's monopoly on state power, autonomous worker organizations would reclaim independent bargaining capacity, political pluralism would re-emerge, and the centralized planning apparatus would fragment; the entire post-revolutionary state form would reorganize around competing centers of authority.
% FOUNDING_PROBLEM: How to achieve socialist revolution in a society with a dispersed, semi-conscious proletariat and powerful counter-revolutionary forces, and how to prevent capitalist restoration after the initial revolutionary rupture.
% FOUNDING_PROBLEM_CORROBORATION: Official party historians and state Marxist-Leninist institutes attest the counter-revolutionary threat remains live. Dissident Marxists, anarchist historians, and liberal critics attest the problem was largely resolved or manufactured to justify bureaucratic consolidation; no source outside the benefiting parties corroborates the live-status claim without contest.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the party's monopoly on political organization decouples its authority from the proletariat's actual preferences, creating a persistent transfer of surplus and voice upward. Suppression (0.88) is higher still because the constraint's persistence depends on actively destroying alternative political forms â pluralist parties, independent unions, and rival readings of socialism â rather than on voluntary coordination. Theater ratio (0.50) reflects the substantial ideological performance required to maintain the 'transitional' framing after decades of permanent rule: congresses, constitutional claims, and ritualized worker participation that obscures genuine party supremacy. Accessibility collapse (0.70) is high because once the vanguard logic is institutionalized, alternative revolutionary pathways (councils, gradual reform, syndicalism) become practically unthinkable within the regime's symbolic order. Resistance (0.75) is high due to persistent underground dissent, exile movements, and periodic worker unrest. The temporal series trace the constraint's lifecycle: initial revolutionary mobilization (moderate extraction, low theater), civil-war consolidation (rising extraction and suppression), high Stalinist centralization (peak extraction and suppression), post-war normalization (sustained extraction with lower theater), and late stagnation (falling suppression capacity, rising theater as ideology decouples from practice).
 *
 * PERSPECTIVAL GAP:
 *   The party cadre seat and the autonomous worker seat should compute sharply different types. From the cadre perspective, the constraint is a hard-won coordination mechanism that prevents capitalist restoration and channels working-class energy into state-building. From the worker perspective, the same structure suppresses independent organization and extracts labor discipline through a party-controlled union apparatus. The industrial proletariat's identity-locked exit amplifies this divergence: they experience the constraint as naturalized class rule, while pluralists experience it as transparent coercion. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The vanguard party cadres and state-planning apparatus are the structural beneficiaries of extraction (political monopoly, surplus control, status allocation), placing their directionality near the beneficiary pole. Political pluralists and autonomous worker organizations are the direct targets of suppression and surplus extraction, placing their directionality near the full-target pole. The industrial proletariat sits ambiguously: doctrinally a beneficiary, structurally a payer, with identity-locked exit pushing its effective directionality closer to the target end than the doctrinal framing suggests. The analytical observer seat carries no directional stake. No override is needed because the beneficiary-victim declarations plus exit options already map the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was originally justified as a scaffold: a transitional dictatorship that would wither away once class enemies were defeated and socialist consciousness matured. However, no enforceable sunset clause was ever institutionalized. Instead, the party's self-identification as the vanguard became permanent, and the 'transitional' state form atrophied into a steady-state extraction structure. This is a classic mandatrophy pattern: the founding problem (counter-revolutionary vulnerability) was either solved or manufactured into irrelevance, but the arrangement persisted because the party apparatus had become the primary beneficiary of its own monopoly. The classification as tangled_rope captures this exactly: the coordination function was real at inception (civil-war mobilization, rapid industrialization) but became inseparable from extraction as the mandate expired. A scaffold that cannot sunset becomes a tangled rope, and eventually a snare if the coordination function fully atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_state_permanence,
    'Is the dictatorship of the proletariat under party guidance genuinely transitional toward stateless communism, or has it become a permanent bureaucratic class structure?',
    'Comparative historical analysis of regimes claiming the vanguard model: if no instance has produced the promised withering away of the state despite favorable conditions, the transitional claim is reclassified as permanent extraction.',
    'If permanent, the constraint''s coordination function is largely theatrical and the classification shifts toward snare; if genuinely transitional but blocked by external factors, the high extraction is an unfortunate instrumental cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_state_permanence, empirical, 'Whether the transitional dictatorship is structurally capable of self-termination.').

omega_variable(
    proletarian_beneficiary_ambiguity,
    'Does the industrial proletariat structurally benefit from the vanguard party''s monopoly, or does it bear the primary extraction disguised by nominal beneficiary status?',
    'Measure autonomous worker bargaining power, wage shares, and political voice before and after vanguard consolidation; compare with trajectories in non-vanguard social democratic regimes.',
    'If the proletariat is a net payer despite doctrinal beneficiary status, the directionality derivation underestimates extraction for the majority and overstates the constraint''s coordination legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proletarian_beneficiary_ambiguity, empirical, 'Whether the proletariat is beneficiary or victim of the party-state monopoly.').

omega_variable(
    worker_subordination_mechanism,
    'Is worker compliance with the vanguard party-state primarily structural (coercion, surveillance, material dependence) or internalized (class-party identity fusion, ideological belief in historical necessity)?',
    'Measure post-collapse or post-emigration political behavior: if subordination persists after structural coercion is removed, internalization was a significant independent mechanism.',
    'If internalized, effective suppression exceeds the structural measure because workers carry the constraint with them after exit, raising the true extraction floor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(worker_subordination_mechanism, conceptual, 'Structural versus internalized suppression of worker autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mani_tr_t12, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(mani_tr_t24, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(mani_tr_t36, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement(mani_tr_t48, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 48, 0.55).
narrative_ontology:measurement(mani_tr_t60, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 60, 0.62).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mani_be_t12, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(mani_be_t24, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(mani_be_t36, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 36, 0.78).
narrative_ontology:measurement(mani_be_t48, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 48, 0.74).
narrative_ontology:measurement(mani_be_t60, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 60, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mani_su_t12, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(mani_su_t24, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 24, 0.95).
narrative_ontology:measurement(mani_su_t36, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 36, 0.92).
narrative_ontology:measurement(mani_su_t48, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 48, 0.88).
narrative_ontology:measurement(mani_su_t60, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the manifesto_revolutionary_method kernel. The vanguard-rupture reading should not be conflated with the democratic-gradualist or council-communist readings, which instantiate structurally distinct constraints with different beneficiary-victim asymmetries and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
