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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Revolutionary Rupture — Dictatorship of Proletariat
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   The vanguard rupture reading claims revolutionary transformation REQUIRES
 *   organized party seizure of state power and transitional dictatorship
 *   under party guidance to overcome capitalist state apparatus and organize
 *   post-revolutionary reconstruction. The claim is that this method solves a
 *   genuine coordination problem — concentrating force sufficient to break
 *   state machinery and preventing counter-revolutionary restoration. The
 *   metrics, however, describe substantially extractive, highly suppressive
 *   operation: extractiveness rises from 0.35 (revolutionary period) to 0.68
 *   (stabilized post-revolutionary state) as coordination demands yield to
 *   state apparatus consolidation; suppression rises sharply from 0.42 to
 *   0.82 as alternative pathways are closed and party monopoly enforced;
 *   theater ratio rises gradually from 0.28 to 0.41, indicating increasing
 *   performative framing of revolutionary discipline as historical necessity.
 *   The claim/metric gap is deliberate and diagnostic: the vanguard reading
 *   asserts coordination function; the temporal trajectory shows extraction
 *   accumulation and theater growth, which the engine will measure as either
 *   validating the coordination claim (if extraction genuinely diminishes as
 *   provisional dictatorship supposedly transitions to communism) or as
 *   evidence the constraint has devolved into state power capture by a new
 *   ruling class.
 *
 * KEY AGENTS:
 *   - party_cadres: Organized group claiming scientific revolutionary authority; sets post-seizure agenda; theoretically temporary during dictatorship of proletariat phase but institutionally permanent
 *   - state_planning_apparatus: Inherits state power; benefits from concentration of economic decision authority; cannot exit without dismantling revolutionary state
 *   - autonomous_worker_organizations: Pre-revolutionary independent unions and councils; suppressed under vanguard discipline; trapped without option to reassert autonomy
 *   - working_class_base: Mass on whose behalf vanguard claims to act; receives promise of future communist liberation; bears present suppression and labor discipline
 *   - political_pluralists: Competing revolutionary and democratic factions; institutionally excluded and suppressed as class enemies or historical obstacles
 *   - theoretical_marxist_orthodoxy: Doctrinal framework the constraint vindicates; non-agent entry but structurally beneficiary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.82).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Revolutionary Rupture — Dictatorship of Proletariat").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'e5f2152a-15a8-47fc-a9de-333d6edf2909').
narrative_ontology:cs_kernel_codification('e5f2152a-15a8-47fc-a9de-333d6edf2909', fixed_text).
narrative_ontology:cs_authority_grounding('e5f2152a-15a8-47fc-a9de-333d6edf2909', lineage).
narrative_ontology:cs_interpretation_layer_present('e5f2152a-15a8-47fc-a9de-333d6edf2909').
narrative_ontology:cs_reading_relation('e5f2152a-15a8-47fc-a9de-333d6edf2909', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_reading_relation('e5f2152a-15a8-47fc-a9de-333d6edf2909', manifesto_revolutionary_method__democratic_gradualism_reading, influences).
narrative_ontology:cs_axiom('e5f2152a-15a8-47fc-a9de-333d6edf2909', foundational, vanguard_party_necessary_for_class_consciousness).
narrative_ontology:cs_axiom_status(vanguard_party_necessary_for_class_consciousness, holdable).
narrative_ontology:cs_axiom_grounding('e5f2152a-15a8-47fc-a9de-333d6edf2909', vanguard_party_necessary_for_class_consciousness, empirically_contingent).
narrative_ontology:cs_axiom('e5f2152a-15a8-47fc-a9de-333d6edf2909', foundational, rupture_necessary_capitalist_state_cannot_transform_itself).
narrative_ontology:cs_axiom_status(rupture_necessary_capitalist_state_cannot_transform_itself, holdable).
narrative_ontology:cs_axiom_grounding('e5f2152a-15a8-47fc-a9de-333d6edf2909', rupture_necessary_capitalist_state_cannot_transform_itself, empirically_contingent).
narrative_ontology:cs_axiom('e5f2152a-15a8-47fc-a9de-333d6edf2909', secondary, transitional_dictatorship_dissolves_toward_communism).
narrative_ontology:cs_axiom_status(transitional_dictatorship_dissolves_toward_communism, overridden).
narrative_ontology:cs_axiom_grounding('e5f2152a-15a8-47fc-a9de-333d6edf2909', transitional_dictatorship_dissolves_toward_communism, instrumental).
narrative_ontology:cs_reference_frame('e5f2152a-15a8-47fc-a9de-333d6edf2909', capitalist_state_and_bourgeois_property_relations_require_revolutionary_rupture).
narrative_ontology:cs_drift_state('e5f2152a-15a8-47fc-a9de-333d6edf2909', post_vanguard_state_consolidation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5f2152a-15a8-47fc-a9de-333d6edf2909', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, pre_revolutionary_institutional_class).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).

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
 *   Extractiveness is authored as high (0.68 end-state) because the vanguard party's control over state power, labor allocation, and ideological legitimacy represents a substantial transfer of authority and autonomy from workers and competing factions to party apparatus, even if framed as temporary. The measurement trajectory shows extractiveness accelerating from 0.35 (pre-seizure) to 0.58 (seizure/consolidation phase) to 0.68 (stabilized state), then plateauing — suggesting the constraint's extraction reaches a stable level once the revolutionary rupture is complete and the apparatus is consolidated. Suppression is higher and rises more sharply (0.42 → 0.82) because the vanguard reading explicitly requires suppression of alternative pathways: competing parties, autonomous worker organizations, political pluralism, and counter-revolutionary elements must be suppressed to prevent the revolution's reversal. Theater rises gradually from 0.28 to 0.41, indicating the ratio of performative revolutionary rhetoric to functional state apparatus grows as the state stabilizes — revolutionary discipline that was functionally necessary during active rupture becomes increasingly theatrical as the apparatus perpetuates itself. Accessibility collapse is high (0.79) because once the vanguard reading is institutionalized, alternatives (councils, pluralism, gradualism) become structurally inaccessible: they are labeled counter-revolutionary, theoretically impossible, or organisationally suppressed. Resistance is high (0.73) because the constraint meets substantial resistance from suppressed factions, international counter-revolution, and worker organizations whose autonomy is curtailed. The one-shot example's time-grid discipline is maintained: every metric is authored at every interval point (0, 5, 10, 20, 30, 40, 50) to prevent the measurement-alignment artifact that previously injected end-state values into earlier time points.
 *
 * PERSPECTIVAL GAP:
 *   From the party_cadres and state_planning_apparatus seats, the vanguard rupture reading is genuine coordination — a functional necessity for revolutionary seizure and reconstruction. From these seats, suppression of alternatives and autonomous organizations is regrettable but historically necessary, and extraction is investment in building communism. From the autonomous_worker_organizations and political_pluralists seats, the same structure operates as coercive state power capture by a new ruling class (the party apparatus) using revolutionary rhetoric as cover. The temporal measurements support this perspectival divergence: extractiveness plateaus at 0.68 rather than declining toward zero (as the promised transition to communism would require), and theater ratio stabilizes at 0.41 (suggesting the revolutionary framing becomes increasingly decoupled from functional necessity). The engine computes each seat's classification from the structural data; the authored metrics do not adjudicate the gap, they merely describe its persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   The party_cadres seat gets d ≈ 0.15 (beneficiary pole): they benefit from state power, set the agenda, and have high power and organized exit options (though identity-locked). The state_planning_apparatus sits near d ≈ 0.25 (beneficiary side): institutional power, benefits from centralized authority, but constrained exit (cannot exit without dismantling the state). Autonomous_worker_organizations get d ≈ 0.85 (target pole): they are suppressed, trapped, pay through loss of autonomy and labor discipline, and have no exit except to criminalize themselves. Political_pluralists also sit high on d ≈ 0.80 (target pole): they are victims of the constraint, excluded and suppressed, with trapped/no exit. The working_class_base is complex: structurally identity-locked (revolutionary consciousness becomes identity, defection is apostasy) and theoretically beneficiary (promised future liberation) but currently paying through suppression and labor discipline — their d sits near 0.60 (slightly target-leaning), reflecting the theoretical benefit promise offset by present suppression. Pre-revolutionary_institutional_class sits at d ≈ 0.95 (maximal target): explicitly expropriated, dispossessed, and eliminated. These directionalities flow from the beneficiary/victim declarations: party cadres and state apparatus are beneficiaries; pluralists, autonomous workers, and the pre-revolutionary class are victims. The working-class base is simultaneously beneficiary (promised communism) and payer (suppressed present), hence the dual role and mid-range d. No overrides needed: the derivation from structural data captures the reading's asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The vanguard rupture reading declares its founding problem as live (capitalist state apparatus, bourgeois property relations, need for vanguard party guidance). However, the measurement trajectory and the theoretical structure of the constraint suggest mandatrophy risk: the dictatorship of the proletariat is claimed to be TRANSITIONAL — a temporary phase during which the working class, via party leadership, consolidates power and builds the material conditions for communism, eventually enabling the state to wither away. The temporal measurements show extraction and suppression plateauing rather than declining, and theater ratio rising — both of which violate the mandatrophy-resolution prediction: if the provisional dictatorship were actually transitional, extraction should diminish as material conditions improved and the need for tight control decreased; suppression should decline as counter-revolutionary threats were eliminated; theater should rise (as the constraint became increasingly performative rather than functionally necessary). The stability of these metrics at t=30-50 suggests the constraint is not transitioning but has stabilized into a new steady state — which would reclassify it from tangled rope (temporary coordination + extraction) into piton (atrophied function, theatrical maintenance, institutional inertia) or snare (pure extraction defended as temporary coordination). This divergence between the claimed transitional mandate and the observed plateau trajectory is exactly what the mandatrophy analysis exists to flag: a constraint that declares its own obsolescence but shows no measurement trajectory toward that obsolescence. Classify as tangled rope rather than piton or snare for now because the theoretical function (coordination of revolutionary seizure and reconstruction) was real at t=0-10, and extractiveness genuinely tracks higher in response to the constraint's structural asymmetry; but flag the mandatrophy trajectory divergence as an omega variable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transitional_mandate_vs_extraction_plateau,
    'Does the dictatorship of the proletariat represent a genuinely transitional phase toward communism and withering of the state, or has it stabilized into a permanent new state form (state socialism) that abandoned the transitional mandate?',
    'Temporal analysis of post-revolutionary trajectories: in systems claiming vanguard rupture reading, does suppression decrease as material conditions supposedly improve and class enemies are eliminated? Does state power actually begin to diffuse toward worker councils or diminish? Or does state apparatus consolidate and resist dissolution? Comparative historical examination of whether post-revolutionary states show declining extraction and theater ratios consistent with transitional function.',
    'If the transition is real, extractiveness and suppression should decline over time (t=50 trajectory should be downward from t=30), and the constraint should remain tangled rope. If the mandate has been abandoned and extraction stabilizes, the constraint reclassifies toward piton (theatrical maintenance of defunct transitional claim) or snare (pure extraction defended by revolutionary rhetoric). Measurement plateau at t=30-50 suggests the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_mandate_vs_extraction_plateau, empirical, 'Whether the vanguard dictatorship is actually transitional or has become permanent state form.').

omega_variable(
    party_consciousness_vs_external_imposition,
    'Does vanguard party leadership genuinely educate and organize working-class consciousness, or does it impose consciousness from outside and suppress autonomous working-class political development?',
    'Historical and sociological examination of: (1) whether working-class organizations pre-revolutionary and post-revolutionary show spontaneous political development, or (2) whether they are strictly party-directed. Whether autonomous worker organizations that challenge party leadership survive or are suppressed. Whether suppressed factions (councils, anarchists, independent unions) show evidence of genuine worker preference or are correctly identified as counter-revolutionary.',
    'If vanguard genuinely educates consciousness, we should observe: working-class base with high political sophistication that aligns with party, autonomous organizations that emerge post-seizure are genuinely counter-revolutionary, working-class base identity is not identity-locked but volitional. If vanguard imposes consciousness, we observe: suppression of autonomous organizations despite worker support, identity-locking of working-class base so that deviation is treated as apostasy, contradiction between stated worker interest and state actions. The suppression_requirement trajectory (0.42 → 0.82) and high accessibility_collapse (0.79) suggest the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_consciousness_vs_external_imposition, empirical, 'Whether vanguard party education is real or cover for imposing consciousness externally.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the vanguard rupture reading logically FORECLOSE the council communist and democratic gradualist readings (i.e., they cannot coexist in any single framework), or do they COEXIST as live contested positions held by different parties and traditions?',
    'Examination of whether: (1) the vanguard reading''s core premises (need for vanguard party, necessity of rupture, transition dictatorship) directly contradict the core premises of councils (direct worker democracy, rejection of vanguard mediation) and gradualism (electoral reform, parliamentary socialism) such that no coherent framework could hold both, OR (2) whether historical reality shows competing parties holding these readings simultaneously, arguing for their relative merits, and not foreclosing each other within shared frameworks (e.g., both vanguardists and gradualists participate in some democratic institutions, or both vanguardists and councilists compete for working-class loyalty within revolutionary movements).',
    'If forecloses: the vanguard reading is incompatible at the logical level with the siblings; holding it requires rejecting the others as incoherent. If coexists_with: the readings are different framings of the same problem, held by different actors, without one logically eliminating the other. This affects how the kernel contest is framed and what resolution mechanism could exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether vanguard rupture reading logically forecloses or coexists with council and gradualist readings.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.82) in vanguard dictatorship primarily structural (external coercion, legal barriers, exclusion from institutions) or internalized (revolutionary consciousness so internalized that dissent becomes unthinkable, identity-fusion making deviation apostasy)?',
    'Post-exit trajectory analysis: if suppressed actors (autonomous worker organizations, competing factions) have opportunity to reorganize after the vanguard state collapses or loses control, do they re-emerge, suggesting suppression was structural? Or do they remain suppressed through internalized identity-lock, suggesting internalization? Examination of whether party cadres and working-class base retain vanguard identity and discipline even after institutional structures dissolve.',
    'If structural: the suppression measurement captures external coercive force; removing the constraint removes suppression. If internalized: suppression persists even after institutional removal because consciousness carries the lock forward; the effective suppression is higher than the structural measure suggests, and fixing the constraint requires de-indoctrination, not just institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression in vanguard constraint is structural or internalized identity-lock.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'What epistemic authority grounds the claim that vanguard party seizure of state power is the CORRECT reading of how revolutionary transformation works? Scientific analysis of historical materialism, vanguardist tradition, or commitment to a particular political outcome?',
    'Examination of grounding for the axiom that party seizure is necessary: is it grounded in empirically testable claims about revolutionary dynamics (empirically_contingent), in normative commitments to working-class power (deontological), in the authority of Marxist-Leninist tradition (lineage), or in instrumental efficacy for achieving communism (instrumental)? Different grounding types produce different vulnerabilities to challenge and different pathways to foreclosure.',
    'If empirically_contingent: the axiom is vulnerable to historical evidence showing successful revolutions without vanguards or failed vanguard transitions, which could lead to axiom_overriding. If deontological: the axiom survives empirical failure but can be challenged on rights grounds. If lineage: the axiom''s authority derives from transmission tradition, vulnerable to tradition-breaking challenge. If instrumental: the axiom survives if communism is achieved, but is questioned if the transition stalls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'What kind of epistemic claim grounds the vanguard rupture axiom — empirical, normative, traditional, or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(mani_tr_t50, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(mani_be_t50, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(mani_su_t50, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.18).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'manifesto_revolutionary_method'. The kernel's multiple readings — vanguard rupture, council communist, democratic gradualism — represent fundamentally different answers to the founding problem of working-class liberation. Each reading has its own ε value, beneficiary/victim structure, and extracted profile. They are linked via affects_constraints because they compete directly for legitimacy in the same domain (revolutionary method) and because acceptance of one reading typically involves rejection or subordination of the others. The vanguard reading influences the council reading by institutionally suppressing council organizations; the gradualist reading is foreclosed by the vanguard reading's claim that rupture is necessary. See kernel_context in commentary for full description of the reading's relationship to siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
