% ============================================================================
% CONSTRAINT STORY: syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_syncretic_fusion_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: syncretic_fusion_reading
 *   human_readable: Syncretic Fusion: Kami-Buddha Ontological Unification
 *   domain: religious_studies/japanese_history/ontology
 *
 * SUMMARY:
 *   The syncretic fusion reading of kami-buddha ontology represents one
 *   coherent interpretation of the relationship between indigenous Japanese
 *   kami and imported Buddhist cosmology. This reading claims that kami are
 *   local manifestations (suijaku) of universal buddha-nature (honji) — a
 *   hierarchical ontology in which kami are subordinated to but not
 *   eliminated by Buddhist metaphysics. The constraint operates across
 *   multiple institutional and temporal scales: from the local folk
 *   practitioner maintaining shrine practice within a Buddhist-dominated
 *   framework, to the syncretic priest managing dual religious authority, to
 *   the Buddhist institutional centers that benefit from the expanded
 *   cosmological scope, to the Meiji state that later enforces separation of
 *   Shinto and Buddhism as a modernization policy. The constraint exhibits
 *   all six DR types from different perspectives, making it a diagnostic
 *   exemplar for how contested ontological readings function as structural
 *   constraints. The measurement trajectory shows declining suppression (as
 *   enforcement mechanisms weakened post-Meiji) and rising theater ratio (as
 *   the doctrine became more performative and less functionally binding),
 *   while extractiveness remained relatively stable — indicating a shift from
 *   active institutional extraction to maintenance through scholarly and
 *   cultural convention.
 *
 * KEY AGENTS:
 *   - Folk Practitioners (Village Shrines): Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with local kami practice; experience ontological subordination without consent or participation in the interpretive framework
 *   - Syncretic Priests (Temple-Shrine Complexes): Secondary agents (moderate/constrained) — benefit from the fusion framework's coherence while constrained by Buddhist doctrinal authority; coordinate genuine dual religious practice
 *   - Buddhist Institutional Authority (National Centers): Primary beneficiary (institutional/arbitrage) — benefits from expanded cosmological scope and resource base; experiences the constraint as coordination rather than extraction
 *   - Meiji Modernization State: Organized actor (organized/constrained) — enforces Shinbutsu bunri (separation) as modernization policy, targeting the syncretic fusion constraint as an obstacle
 *   - Contemporary Academic Interpreters: Institutional observers (institutional/arbitrage) — maintain the framework as historical-interpretive category; see it as mostly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contested institutional reading as ontological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(syncretic_fusion_reading, 0.35).
domain_priors:suppression_score(syncretic_fusion_reading, 0.48).
domain_priors:theater_ratio(syncretic_fusion_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(syncretic_fusion_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(syncretic_fusion_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(syncretic_fusion_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(syncretic_fusion_reading, "Syncretic Fusion: Kami-Buddha Ontological Unification").
narrative_ontology:topic_domain(syncretic_fusion_reading, "religious_studies/japanese_history/ontology").

domain_priors:requires_active_enforcement(syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(syncretic_fusion_reading, 'fc1481de-6ff7-42a1-9b1d-05a16e7dea43').
narrative_ontology:cs_kernel_codification('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', formalized).
narrative_ontology:cs_authority_grounding('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', lineage).
narrative_ontology:cs_interpretation_layer_present('fc1481de-6ff7-42a1-9b1d-05a16e7dea43').
narrative_ontology:cs_reading_relation('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', syncretic_fusion_reading__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', syncretic_fusion_reading__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', kami_are_buddha_manifestations, deontological).
narrative_ontology:cs_axiom('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', foundational, buddha_nature_is_ontological_ground).
narrative_ontology:cs_axiom_status(buddha_nature_is_ontological_ground, holdable).
narrative_ontology:cs_axiom_grounding('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', buddha_nature_is_ontological_ground, deontological).
narrative_ontology:cs_reference_frame('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', honji_suijaku_hierarchy).
narrative_ontology:cs_drift_state('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fc1481de-6ff7-42a1-9b1d-05a16e7dea43', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(syncretic_fusion_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, buddhist_institutional_authority).
narrative_ontology:constraint_victim(syncretic_fusion_reading, indigenous_kami_autonomy).
narrative_ontology:constraint_victim(syncretic_fusion_reading, folk_religious_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, syncretic_priests).
narrative_ontology:constraint_victim(syncretic_fusion_reading, folk_shrine_practitioners).
narrative_ontology:constraint_victim(syncretic_fusion_reading, syncretic_priests).
narrative_ontology:constraint_vindicates(syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(syncretic_fusion_reading, universal_buddha_nature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain local kami veneration within a framework that ontologically subordinates their kami to Buddhist buddha-nature. They experience the constraint as reframing their sacred landscape without their consent or participation in the interpretive framework. Their ritual autonomy is absorbed into Buddhist institutional logic. Exit would require abandoning their community identity and ritual role.
narrative_ontology:constraint_stakeholder(syncretic_fusion_reading, folk_shrine_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Manage temple-shrine complexes that serve both Buddhist and kami devotees. They benefit from the fusion framework's coherence (it provides a single cosmological structure for dual practice) while being constrained by Buddhist doctrinal authority. They extract institutional authority and resource control through the honji-suijaku hierarchy while also bearing the cost of maintaining doctrinal consistency.
narrative_ontology:constraint_stakeholder(syncretic_fusion_reading, syncretic_priests, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(syncretic_fusion_reading, syncretic_priests, payer).

% Buddhist institutional centers benefit from the syncretic fusion framework without bearing its costs. The honji-suijaku doctrine allows them to absorb kami worship into their cosmology, expanding their authority and resource base while maintaining doctrinal coherence. They have exit options (could theoretically abandon syncretism) but have no incentive to do so.
narrative_ontology:constraint_stakeholder(syncretic_fusion_reading, buddhist_institutional_authority, beneficiary,
    institutional, generational, arbitrage, national).

% The Meiji government and Shinto revivalists enforce Shinbutsu bunri (separation of Shinto and Buddhism) as a modernization policy. They target the syncretic fusion constraint as an obstacle to constructing a unified national Shinto identity. They enforce separation through state power, forcing Buddhist institutions to divest from shrine properties and reorganizing religious authority along nationalist lines.
narrative_ontology:constraint_stakeholder(syncretic_fusion_reading, meiji_modernization_state, agenda_setter,
    organized, civilizational, constrained, national).

% The abstract principle of kami as autonomous spiritual entities (prior to syncretic subordination) is excluded from the framework that now governs their ontological status. This is not an agent but a non-agent entity kept for narrative completeness — the pre-syncretic kami autonomy that the fusion reading subordinates.
narrative_ontology:constraint_stakeholder(syncretic_fusion_reading, indigenous_kami_autonomy, excluded,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(syncretic_fusion_reading, indigenous_kami_autonomy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrating indigenous kami worship into a coherent Buddhist cosmological framework, allowing dual religious practice to coexist within a single metaphysical system. The honji-suijaku doctrine solves the genuine problem of how to maintain both Buddhist and kami religious practice without doctrinal contradiction.
% TRANSFER_FUNCTION: The constraint transfers ontological authority from kami (as autonomous spiritual entities) to Buddhist institutions (as the interpreters and administrators of the honji-suijaku hierarchy). It also transfers resource control and institutional legitimacy to Buddhist centers, which now mediate the relationship between folk practitioners and their kami.
% ABSENT_VOICES: Pre-Buddhist kami theology and folk practitioners who were not consulted in the development of the syncretic fusion framework. The framework was imposed top-down through Buddhist institutional authority rather than negotiated with folk practitioners. Contemporary Shinto revivalists (who later enforce separation) are also absent from the original syncretic framework's development.
% DISAPPEARANCE_RATIONALE: If the syncretic fusion constraint disappeared, the world would rearrange substantially. Buddhist institutions would lose the cosmological scope and resource base they gained through syncretism. Folk practitioners would regain ontological autonomy for their kami (or would need to reconstruct kami theology without Buddhist subordination). The Meiji-era separation of Shinto and Buddhism would not have occurred in the same way. Contemporary religious practice and institutional arrangements depend on the centuries-long history of syncretic fusion.
% FOUNDING_PROBLEM: The founding problem was the integration of indigenous Japanese kami worship into imported Buddhist cosmology without eliminating either tradition. Buddhist institutions needed a framework that could absorb kami into their metaphysical system while maintaining doctrinal coherence. Folk practitioners needed a way to continue kami veneration within a Buddhist-dominated institutional landscape.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by Buddhist institutional texts (the honji-suijaku doctrine is explicitly formulated in Buddhist philosophical works) and by historical evidence of temple-shrine complexes managing dual religious practice. However, the problem's status as 'dead' is attested by contemporary religious practice: the syncretic fusion is no longer actively enforced as a binding ontological claim. The Meiji separation of Shinto and Buddhism (1868 onward) formally rejected the founding problem's premise, declaring kami and buddhas to be ontologically separate. Contemporary practitioners maintain the framework more as cultural heritage than as a live metaphysical commitment.
narrative_ontology:disappearance_verdict(syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(syncretic_fusion_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOLK PRACTITIONER (SNARE) — Structurally mobile (could theoretically abandon shrine practice) but identity-fused with local kami veneration. The syncretic reading subordinates their kami to Buddhist ontology without their consent or participation in the interpretive framework. They experience the constraint as extraction: their sacred landscape is reframed as manifestation of something else, their ritual autonomy is absorbed into Buddhist institutional logic, and they have no voice in the ontological hierarchy that now governs their practice. Identity lock prevents exit — abandoning kami practice would dissolve their community identity and ritual role.
constraint_indexing:constraint_classification(syncretic_fusion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SYNCRETIC PRIEST (TANGLED ROPE) — Constrained by institutional hierarchy and doctrinal authority but also benefits from the fusion framework. The priest coordinates genuine religious practice (serving both Buddhist and kami devotees) while extracting institutional authority and resource control through the honji-suijaku hierarchy. The constraint both enables their work (provides a coherent framework for dual practice) and subordinates them to Buddhist doctrinal authority. Moderate extraction because they have some agency in local interpretation, but constrained by the need to maintain doctrinal consistency with Buddhist centers.
constraint_indexing:constraint_classification(syncretic_fusion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST INSTITUTIONAL AUTHORITY (ROPE) — Benefits from the syncretic fusion framework without bearing its costs. The honji-suijaku doctrine allows Buddhist institutions to absorb kami worship into their cosmology, expanding their authority and resource base while maintaining doctrinal coherence. They experience the constraint as coordination: it solves the genuine problem of integrating indigenous religious practice into Buddhist institutional structure. Net beneficiary — extraction runs toward this agent. They have exit options (could theoretically abandon syncretism) but have no incentive to do so.
constraint_indexing:constraint_classification(syncretic_fusion_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEIJI MODERNIZATION STATE (TANGLED ROPE) — Organized state actors (Meiji government, Shinto revivalists) see the syncretic fusion as an obstacle to their project of constructing a unified national Shinto identity. The state enforces Shinto-Buddhism separation (Shinbutsu bunri) as a modernization policy, which both coordinates a new national religious framework AND extracts from Buddhist institutions by forcing divestment from shrine properties. The constraint (syncretic fusion) becomes the target of state enforcement, not the enforcer. This perspective shows how the same ontological claim can be experienced as coordination by one institutional actor (Buddhist authority) and as extraction by another (modernizing state).
constraint_indexing:constraint_classification(syncretic_fusion_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTEMPORARY ACADEMIC INTERPRETER (PITON) — Modern scholars and religious institutions maintain the syncretic fusion framework as a historical-interpretive category, but its functional role has atrophied. The honji-suijaku doctrine is now mostly performative — cited as historical fact, taught in religious studies courses, maintained in temple-shrine complexes as cultural heritage — but no longer actively enforces the ontological subordination it once did. The constraint persists through institutional inertia and scholarly convention rather than active enforcement. Theater ratio is high because the framework is maintained as a coherent narrative despite its reduced functional grip on actual religious practice.
constraint_indexing:constraint_classification(syncretic_fusion_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ONTOLOGICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the syncretic fusion reading claims to describe an ontological fact: kami ARE local manifestations of universal buddha-nature. This perspective treats the honji-suijaku doctrine as a natural law of religious ontology — a necessary truth about the structure of reality rather than a contingent institutional arrangement. However, the structural data contradicts this classification: the constraint requires active enforcement, has identifiable beneficiaries (Buddhist institutions), and shows theater ratio indicating performative maintenance. The engine will compute this as a false summit, revealing that the 'ontological necessity' framing naturalizes what is actually a contested institutional reading.
constraint_indexing:constraint_classification(syncretic_fusion_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(syncretic_fusion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(syncretic_fusion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(syncretic_fusion_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(syncretic_fusion_reading, TR),
    TR >= 0.70.

:- end_tests(syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The syncretic fusion reading benefits Buddhist institutions substantially (expanded authority, resource base, cosmological scope) while imposing costs on folk practitioners (ontological subordination, loss of kami autonomy). The extraction is not maximal because the framework also provides genuine coordination benefits — it allows dual religious practice to coexist within a single coherent cosmology, which serves folk practitioners' practical needs even as it subordinates their ontological status. Suppression (0.48): Moderate. The framework requires active enforcement through doctrinal authority and institutional hierarchy, but suppression is not total — folk practitioners retain significant agency in local interpretation and practice. The measurement trajectory shows declining suppression post-Meiji as enforcement mechanisms weakened and the framework became more culturally embedded than institutionally enforced. Theater ratio (0.58): Moderate-high. The syncretic fusion doctrine is increasingly performative — maintained as a coherent historical narrative and scholarly framework but no longer actively enforcing the ontological subordination it once did. The rising trajectory reflects the shift from active institutional enforcement to maintenance through convention and cultural heritage.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single ontological claim. The folk practitioner sees extraction (Snare) — their kami are reframed as manifestations of something else without their consent. The syncretic priest sees mixed coordination and extraction (Tangled Rope) — the framework enables their work while constraining their authority. The Buddhist institution sees pure coordination (Rope) — the framework solves the genuine problem of integrating indigenous practice into Buddhist cosmology. The Meiji state sees an obstacle to modernization (Tangled Rope) — the syncretic fusion must be dismantled to construct a unified national Shinto identity. The contemporary academic interpreter sees a degraded historical framework (Piton) — maintained through scholarly convention rather than active enforcement. The civilizational analytical observer risks seeing an ontological necessity (Mountain) — kami ARE manifestations of buddha-nature — but the structural data reveals this as a false summit: the constraint requires active enforcement, has identifiable beneficiaries, and shows rising theater ratio indicating performative maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the extraction flow. Folk practitioners are victims with identity-locked exit — they experience maximum extraction (d ≈ 1.0). Syncretic priests are constrained agents with mixed beneficiary/victim status — they experience moderate extraction (d ≈ 0.55). Buddhist institutions are beneficiaries with arbitrage options — they experience negative extraction/subsidy (d ≈ 0.15). The Meiji state is an organized actor targeting the constraint as an obstacle — their directionality is complex (they are neither beneficiary nor victim of the syncretic fusion itself, but rather enforce a competing constraint). The academic interpreter is an analytical observer with arbitrage options — they experience the constraint as a scholarly framework rather than an extraction mechanism (d ≈ 0.20). The civilizational analytical observer risks naturalizing the constraint as ontological necessity, which would collapse directionality into a false mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how a single ontological reading can function as both coordination and extraction depending on the observer's structural position. The Buddhist institution genuinely solves a coordination problem (integrating indigenous practice into Buddhist cosmology) while simultaneously extracting authority and resources from folk practitioners. The constraint is not 'really' one type or the other — it is genuinely both, from different perspectives. The mandatrophy is resolved by recognizing that the syncretic fusion reading is a contested kernel reading, not a natural law. The 'ontological necessity' framing (mountain perspective) naturalizes what is actually a contingent institutional arrangement that benefits Buddhist authority and subordinates folk practitioners. The false summit signature is strong: the constraint is presented as a discovery of natural religious law but functions as an institutional extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_pragmatic_status,
    'Is the honji-suijaku doctrine an ontological claim about the nature of kami and buddhas, or a pragmatic institutional framework for managing dual religious practice?',
    'Historical analysis of doctrinal texts and institutional practice; comparison of how the doctrine is invoked in theological contexts vs. administrative contexts; examination of whether practitioners treat it as metaphysically binding or operationally convenient',
    'If ontological: the constraint is a genuine metaphysical claim (mountain candidate). If pragmatic: the constraint is an institutional arrangement (tangled rope confirmed). If mixed: the constraint exhibits the false summit signature — presented as ontological but functioning as institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_pragmatic_status, conceptual, 'Whether honji-suijaku is ontological necessity or pragmatic institutional framework').

omega_variable(
    folk_practitioner_consent_and_awareness,
    'To what extent were folk practitioners aware of and consenting to the ontological subordination of kami under the honji-suijaku framework?',
    'Historical documentation of folk religious practice; examination of shrine records and folk narratives; analysis of whether the framework was imposed top-down or negotiated locally; comparison of pre-syncretic and syncretic-era folk practice',
    'If imposed without consent: snare classification confirmed (extraction without participation). If negotiated: tangled rope classification more appropriate (coordination with asymmetric benefits). If folk practitioners actively resisted: snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_practitioner_consent_and_awareness, empirical, 'Degree of folk practitioner consent to ontological subordination').

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the syncretic fusion reading a discovery of natural religious law (kami ARE manifestations of buddha-nature) or a construction of Buddhist institutional authority (kami WERE MADE manifestations through doctrinal reframing)?',
    'Comparative analysis of pre-Buddhist kami ontology; examination of whether the fusion doctrine emerges from kami theology or is imposed upon it; analysis of alternative readings (domain partition, pragmatic incoherence) and their structural coherence',
    'If natural law: mountain classification appropriate. If constructed: false summit signature confirmed — the constraint naturalizes an institutional arrangement. This omega is the FSM candidate trigger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Natural law discovery vs. institutional construction of syncretic fusion').

omega_variable(
    sibling_reading_foreclosure,
    'Does the syncretic fusion reading logically foreclose the domain partition reading (kami and buddhas are ontologically separate) or merely compete with it?',
    'Logical analysis of the two readings'' core premises; examination of whether a single coherent framework could hold both readings simultaneously; historical analysis of whether practitioners held both readings in tension',
    'If forecloses: the readings are mutually exclusive (rare). If coexists: both readings remain live options for different parties (more likely). If influences: the fusion reading creates pressure on the partition reading without logically eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether syncretic fusion forecloses domain partition reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(syncretic_fusion_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(syncfus_theater_t0, syncretic_fusion_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(syncfus_theater_t3, syncretic_fusion_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(syncfus_theater_t6, syncretic_fusion_reading, theater_ratio, 6, 0.58).
narrative_ontology:measurement(syncfus_theater_t9, syncretic_fusion_reading, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(syncfus_extract_t0, syncretic_fusion_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(syncfus_extract_t3, syncretic_fusion_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(syncfus_extract_t6, syncretic_fusion_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(syncfus_extract_t9, syncretic_fusion_reading, base_extractiveness, 9, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(syncfus_suppress_t0, syncretic_fusion_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(syncfus_suppress_t3, syncretic_fusion_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(syncfus_suppress_t6, syncretic_fusion_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(syncfus_suppress_t9, syncretic_fusion_reading, suppression_requirement, 9, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(syncretic_fusion_reading, pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(syncretic_fusion_reading, shinbutsu_bunri_separation).

% DUAL FORMULATION NOTE:
% The syncretic fusion reading is one of three structurally distinct readings of the kami-buddha ontology kernel. Each reading has its own ε value, beneficiary/victim structure, and classification profile. The domain_partition_reading treats kami and buddhas as ontologically separate (lower extraction, different beneficiaries). The pragmatic_incoherence_reading treats the fusion as useful fiction (higher theater ratio, different suppression profile). These are not the same constraint viewed from different angles — they are genuinely different constraints with different structural properties. The Meiji-era shinbutsu_bunri_separation constraint is downstream of this kernel dispute, enforcing state-mandated separation as a modernization policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(syncretic_fusion_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
