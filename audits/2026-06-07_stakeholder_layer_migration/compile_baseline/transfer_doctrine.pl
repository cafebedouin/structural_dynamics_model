% ============================================================================
% CONSTRAINT STORY: transfer_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transfer_doctrine, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: transfer_doctrine
 *   human_readable: Transfer Doctrine: Arab Population Removal as Demographic Solution
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The transfer doctrine — the ideological and practical commitment to Arab
 *   population removal as the solution to the demographic problem — evolved
 *   from proposal to implementation between the 1930s and 1948. Early Zionist
 *   leaders (Ben-Gurion, Weizmann, Katznelson) discussed transfer in private
 *   correspondence and leadership meetings from the 1930s onward, framing it
 *   as a necessary mechanism for achieving a viable Jewish majority state.
 *   The 1937 Peel Commission's proposal for population exchange legitimized
 *   the concept in international discourse. By 1948, Plan Dalet and
 *   subsequent military operations resulted in the expulsion or flight of
 *   approximately 750,000 Palestinian Arabs from the territory that became
 *   Israel. The constraint operates differently depending on the observer's
 *   structural position and interpretive reading of the Zionist project. From
 *   the national liberation reading, transfer is tragic necessity — a
 *   coordination mechanism enabling Jewish sovereignty in the face of
 *   existential threat. From the settler-colonial reading, transfer is the
 *   constitutive mechanism of indigenous displacement, with the coordination
 *   story as cover. From the refugee perspective, it is pure extraction
 *   regardless of framing: total loss of land, community, and political
 *   rights with no exit across generations. The constraint's theater ratio
 *   (0.35) reflects the gap between official denial narratives ('they left
 *   voluntarily') and archival evidence of systematic expulsion. The
 *   measurements show extractiveness and suppression intensifying sharply at
 *   time_point 6 (1948 implementation) and remaining at maximum levels
 *   through the present due to denial of return.
 *
 * KEY AGENTS:
 *   - Palestinian Refugees: Primary victim (powerless/trapped) — bore total extraction through expulsion and denial of return; no exit across generations
 *   - Palestinian Arab Population (Pre-1948): Primary victim (powerless/identity_locked) — identity fused with ancestral land; transfer targeted this fusion
 *   - Zionist Leadership: Primary beneficiary (institutional/arbitrage) — achieved demographic transformation enabling Jewish majority state; had alternative options (partition with different borders, power-sharing, binational frameworks)
 *   - Israeli State Apparatus: Beneficiary (institutional/arbitrage) — maintains denial narrative and legal barriers to return; frames transfer as either temporary wartime measure (scaffold) or maintains 'voluntary exodus' narrative (piton)
 *   - Palestinian Political Leadership: Secondary victim (moderate/constrained) — had limited capacity to prevent implementation but some agency through rejection of partition and armed resistance
 *   - International Community: Mixed position (organized/constrained) — partition framework had genuine coordination function but asymmetric extraction in implementation; could not impose equitable solution
 *   - Analytical Observer: Settler-colonial reading (analytical/analytical) — sees transfer as constitutive extraction mechanism, not incidental war outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transfer_doctrine, 0.88).
domain_priors:suppression_score(transfer_doctrine, 0.92).
domain_priors:theater_ratio(transfer_doctrine, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transfer_doctrine, extractiveness, 0.88).
narrative_ontology:constraint_metric(transfer_doctrine, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(transfer_doctrine, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transfer_doctrine, snare).
narrative_ontology:human_readable(transfer_doctrine, "Transfer Doctrine: Arab Population Removal as Demographic Solution").
narrative_ontology:topic_domain(transfer_doctrine, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(transfer_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transfer_doctrine, 'a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d').
narrative_ontology:cs_kernel_codification('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', distributed).
narrative_ontology:cs_authority_grounding('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', lineage).
narrative_ontology:cs_interpretation_layer_present('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d').
narrative_ontology:cs_reading_relation('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', transfer_doctrine__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', transfer_doctrine__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', colonial_structure_determines_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', foundational, displacement_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', displacement_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', secondary, indigenous_status_requires_continuous_presence).
narrative_ontology:cs_axiom_status(indigenous_status_requires_continuous_presence, holdable).
narrative_ontology:cs_axiom_grounding('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', indigenous_status_requires_continuous_presence, conventional).
narrative_ontology:cs_reference_frame('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', defensive_necessity_framework).
narrative_ontology:cs_drift_state('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', post_new_historians_archival_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7f3c8e1-4d2b-4f9a-8c3d-9e6f5a2b1c0d', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transfer_doctrine, jewish_state_builders).
narrative_ontology:constraint_beneficiary(transfer_doctrine, zionist_leadership).
narrative_ontology:constraint_beneficiary(transfer_doctrine, israeli_state_apparatus).
narrative_ontology:constraint_victim(transfer_doctrine, palestinian_refugees).
narrative_ontology:constraint_victim(transfer_doctrine, palestinian_arab_population).
narrative_ontology:constraint_victim(transfer_doctrine, displaced_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN REFUGEES (SNARE) — Trapped by physical expulsion, legal statelessness, and denial of return. No exit from refugee status across generations. Maximum extraction: loss of land, property, community, and political rights. The constraint operates as pure extraction with no coordination function from this position.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN ARAB POPULATION PRE-1948 (SNARE) — Identity-locked to ancestral land and community structures. Could not exit without abandoning identity as Palestinian Arabs rooted in specific villages and regions. The transfer doctrine targeted precisely this identity-land fusion. Extraction was total: the constraint's function was elimination of this population from the territory.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: ZIONIST LEADERSHIP / NATIONAL LIBERATION READING (ROPE) — From the national liberation reading, transfer is a tragic but necessary coordination mechanism: solving the demographic problem enables Jewish majority and state viability. Beneficiaries with arbitrage exit (could have pursued other territorial solutions, accepted partition with different borders, or negotiated power-sharing). Experiences the constraint as coordination toward the goal of Jewish sovereignty.
constraint_indexing:constraint_classification(transfer_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PALESTINIAN POLITICAL LEADERSHIP (SNARE) — Constrained by limited military capacity, fragmented political structure, and British Mandate withdrawal. Could not prevent implementation but had some agency (rejection of partition, armed resistance). Experiences the constraint as extraction: the doctrine eliminated the population they represented. Not trapped (had some options) but extraction was severe.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ISRAELI STATE APPARATUS / TRANSITIONAL FRAMING (SCAFFOLD) — From the perspective that frames transfer as a temporary wartime necessity (1948 as exceptional moment), the constraint has scaffold logic: a transitional measure justified by existential threat, not a permanent policy. This framing requires the sunset claim that transfer ended with the 1948 war. The state apparatus had arbitrage exit (could have permitted return post-war, as international law required). Low experienced extraction because this perspective sees the measure as temporary coordination.
constraint_indexing:constraint_classification(transfer_doctrine, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ISRAELI STATE APPARATUS / DENIAL MAINTENANCE (PITON) — From the perspective that maintains the 'they left voluntarily' narrative despite archival evidence of expulsion orders, the constraint has degraded into theatrical performance. The original function (demographic transformation) succeeded in 1948; what remains is the ritual denial and the legal-bureaucratic apparatus preventing return. The performance is maintained through institutional inertia and political necessity, not because the denial is credible.
constraint_indexing:constraint_classification(transfer_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SETTLER-COLONIAL READING (SNARE) — From the settler-colonial reading, transfer is the constitutive mechanism of the colonial project: indigenous displacement is not incidental but definitional. The coordination story (demographic necessity for Jewish state) is the cover; the structure is extraction. Analytical position with civilizational time horizon sees the constraint as a snare: systematic extraction with suppressed alternatives (binational state, equal citizenship, federated solution) and identifiable victims. This is the claimed_type.
constraint_indexing:constraint_classification(transfer_doctrine, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: INTERNATIONAL COMMUNITY / PARTITION FRAMEWORK (TANGLED ROPE) — The UN partition plan (1947) and subsequent international legal framework experience the constraint as tangled rope: genuine coordination function (resolving competing claims through territorial division) entangled with asymmetric extraction (partition allocated 56% of land to 33% of population; no mechanism for equitable implementation). Organized actors (UN, regional powers) with constrained exit (could not impose solution, could not ignore the conflict). Mixed coordination and extraction.
constraint_indexing:constraint_classification(transfer_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transfer_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transfer_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transfer_doctrine, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transfer_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transfer_doctrine, TR),
    TR >= 0.70.

:- end_tests(transfer_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The constraint extracted land, property, community, and political rights from 750,000 Palestinians, with no compensation and no return. The extraction was not incidental but constitutive: the demographic transformation was the constraint's primary function. The value reflects that extraction was nearly total for the victim population, though not quite 1.0 because some Palestinian Arabs remained (20% of Israeli population) and some property compensation occurred (inadequate but non-zero). Suppression (0.92): Very high. Alternatives (binational state, equal citizenship, federated solution, negotiated return) were systematically suppressed through military force, legal barriers (Absentee Property Law, citizenship restrictions), and international diplomatic pressure. The suppression persists: right of return remains blocked by Israeli law and policy. Not quite 1.0 because international legal framework (UNGA 194) formally recognizes return right, even if unimplemented. Theater ratio (0.35): Moderate. Significant gap between official narrative ('voluntary exodus,' 'no expulsion orders') and archival evidence (Plan Dalet, village-specific expulsion orders documented by New Historians). The theater has increased over time as the denial narrative became institutionalized, but it is not as high as pure piton constraints because substantial scholarship and international legal discourse recognize the expulsion. The performance is maintained but not universally credible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by the contested kernel (zionist_legitimacy_basis) and its three readings. From the national liberation reading (Perspective 3), transfer is coordination: a tragic but necessary mechanism for Jewish sovereignty, justified by persecution and historical connection. From the settler-colonial reading (Perspective 7, the analytical observer and claimed_type), transfer is extraction: the constitutive mechanism of indigenous displacement, with the coordination story as cover. From the refugee perspective (Perspectives 1-2), it is pure extraction regardless of framing: total loss with no exit. The scaffold perspective (Perspective 5) frames transfer as temporary wartime necessity with an implied sunset (return should have followed the war's end). The piton perspective (Perspective 6) sees the current denial maintenance as degraded performance. The tangled rope perspective (Perspective 8, international community) sees mixed coordination (partition framework) and extraction (asymmetric implementation). The gap is not resolvable within a single framework because the readings rest on incompatible axioms about the nature of the Zionist project itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Palestinian refugees (Perspectives 1-2) are full targets: victims with trapped or identity_locked exit, experiencing maximum extraction (d → 1.0). Zionist leadership and Israeli state apparatus (Perspectives 3, 5, 6) are beneficiaries: institutional actors with arbitrage exit, experiencing the constraint as coordination or degraded performance (d → 0.0-0.2). Palestinian political leadership (Perspective 4) is a victim with constrained exit: had some agency but could not prevent extraction (d → 0.7-0.8). International community (Perspective 8) is mixed: organized actors with constrained exit, experiencing both coordination function and asymmetric extraction (d → 0.4-0.5). The analytical observer (Perspective 7) uses the analytical exit option, which derives d from the structural analysis rather than from personal experience — in this case, the settler-colonial reading identifies the constraint as extraction, so d → 0.8-0.9 (high extraction, though not maximum because the analytical position recognizes the coordination story as a real feature of the beneficiary perspective, not pure fabrication).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the classification depends on which reading of the contested kernel (zionist_legitimacy_basis) the observer holds. From the national liberation reading, transfer is coordination (rope or scaffold): solving the demographic problem enables Jewish sovereignty, justified by persecution and historical connection. From the settler-colonial reading, transfer is extraction (snare): indigenous displacement is the constitutive mechanism, with the coordination story as cover. Both readings are internally coherent given their axioms, but the axioms are incompatible (see omega: reading_foreclosure_empirical_basis). The analytical observer's snare classification (claimed_type) reflects the settler-colonial reading's structural analysis: the constraint systematically extracted land and rights from an identifiable victim population, suppressed alternatives, and persists through denial of return. The coordination function (demographic transformation enabling Jewish majority) is real from the beneficiary perspective, but it does not make the constraint a rope from the victim perspective — the asymmetry is too severe. The mandatrophy is not 'which type is correct?' but 'which reading's axioms do you accept?' The presheaf over the observation site includes all perspectives, and the kernel's contested status is the irreducible fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_circumstance,
    'Was the 1948 Palestinian exodus primarily the result of deliberate expulsion policy (Plan Dalet implementation) or primarily the unintended consequence of war and panic?',
    'Archival analysis of military orders, leadership statements, and operational patterns. Benny Morris, Ilan Pappe, and other New Historians have documented systematic expulsion orders for many villages, but debate continues over the relative weight of policy vs circumstance across all 400+ depopulated villages.',
    'If primarily deliberate: snare classification confirmed (systematic extraction). If primarily circumstantial: could shift toward tangled_rope (coordination failure with asymmetric harm) from some perspectives. Does not change the refugee perspective (trapped regardless of intent) or the structural outcome (demographic transformation achieved).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_circumstance, empirical, 'Whether 1948 exodus was primarily policy-driven or war-circumstance').

omega_variable(
    transfer_advocacy_continuity,
    'Does the ideological continuity from 1930s transfer proposals (Peel Commission response) to 1948 implementation constitute a single constraint, or are these structurally distinct phenomena that should be decomposed?',
    'Historical analysis of the gap between advocacy and implementation: Was 1948 the execution of a long-held plan, or an opportunistic response to war conditions that happened to align with earlier proposals? Archival evidence shows both continuity (same leaders, same demographic logic) and discontinuity (1948 exceeded earlier proposals in scope and method).',
    'If single constraint: the current story is correct. If distinct: should decompose into ''transfer_advocacy_doctrine'' (1930s-1947, primarily ideological) and ''transfer_implementation_1948'' (wartime execution, primarily operational), linked via network.affects_constraints. Current story treats them as one constraint with temporal evolution (measurements show increasing extractiveness), which may collapse structurally distinct phases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_advocacy_continuity, conceptual, 'Whether transfer advocacy and implementation are one constraint or two').

omega_variable(
    alternative_suppression,
    'Were binational state proposals, federated solutions, or equal citizenship frameworks genuinely suppressed alternatives, or were they structurally non-viable given the demographic and political conditions?',
    'Counterfactual analysis of alternative frameworks: Could a binational state have been stable given intercommunal violence patterns? Could equal citizenship have been accepted by either leadership? Historical evidence shows these alternatives were proposed (Judah Magnes, Ihud group, some Arab leaders) but rejected by mainstream Zionist leadership and most Palestinian leadership. The question is whether rejection was due to power asymmetry (suppression) or due to genuine incompatibility (structural non-viability).',
    'If genuinely suppressed: snare classification confirmed (alternatives existed but were blocked). If structurally non-viable: could shift toward mountain from some perspectives (demographic incompatibility as immutable constraint) or tangled_rope (coordination failure rather than extraction). Does not change the refugee perspective (extraction regardless of alternative viability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_suppression, conceptual, 'Whether alternative frameworks were suppressed or structurally non-viable').

omega_variable(
    reading_foreclosure_empirical_basis,
    'Does the empirical record of displacement patterns, land acquisition mechanisms, and demographic engineering foreclose the national liberation reading''s framing of transfer as defensive necessity?',
    'Systematic analysis of land acquisition (purchase vs seizure ratios), displacement timing (pre-war vs during-war vs post-war), and leadership statements (private vs public framing). If the structural pattern matches colonial land clearance more than defensive population exchange, the national liberation reading''s empirical premises are overridden. The New Historians'' archival work provides substantial evidence, but the reading persists in Israeli public discourse.',
    'If empirically overridden: the national liberation reading''s axiom (defensive necessity justifies displacement) has grounding_type empirically_contingent and should be marked as foreclosed by axiom_overriding drift. If empirically contested but not overridden: axiom remains holdable and readings coexist. This omega determines whether the reading_relations include a forecloses edge or only coexists_with edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical_basis, empirical, 'Whether archival evidence forecloses the defensive necessity framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transfer_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transfer_theater_1930s, transfer_doctrine, theater_ratio, 0, 0.15).
narrative_ontology:measurement(transfer_theater_1940s, transfer_doctrine, theater_ratio, 3, 0.2).
narrative_ontology:measurement(transfer_theater_1948, transfer_doctrine, theater_ratio, 6, 0.25).
narrative_ontology:measurement(transfer_theater_present, transfer_doctrine, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(transfer_extract_1930s, transfer_doctrine, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(transfer_extract_1940s, transfer_doctrine, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(transfer_extract_1948, transfer_doctrine, base_extractiveness, 6, 0.88).
narrative_ontology:measurement(transfer_extract_present, transfer_doctrine, base_extractiveness, 10, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(transfer_suppress_1930s, transfer_doctrine, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(transfer_suppress_1940s, transfer_doctrine, suppression_requirement, 3, 0.7).
narrative_ontology:measurement(transfer_suppress_1948, transfer_doctrine, suppression_requirement, 6, 0.92).
narrative_ontology:measurement(transfer_suppress_present, transfer_doctrine, suppression_requirement, 10, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transfer_doctrine, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of 'demographic_engineering_imperative' (the broader structural requirement for Jewish majority) but represents a distinct mechanism: the specific doctrine of Arab population removal. The upstream constraint (demographic_engineering_imperative, claimed_type tangled_rope) has its own extractiveness reflecting the mixed coordination-extraction of demographic management generally. This constraint (transfer_doctrine) has higher extractiveness (0.88 vs upstream's likely lower value) because it represents the most extractive implementation mechanism within the broader demographic imperative. The two constraints should remain separate stories because they have different ε values and different victim sets: demographic engineering affects both Jewish immigrants (coordinated) and Palestinian Arabs (extracted from), while transfer doctrine affects primarily Palestinian Arabs (extracted from) with Jewish state-builders as beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
