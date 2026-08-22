% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Kami-Buddha Ontological Fusion (Syncretic Reading)
 *   domain: religious/theological/metaphysical
 *
 * SUMMARY:
 *   The syncretic-fusion reading claims that kami and buddhas are
 *   ontologically unified—that honji suijaku ('original essence, manifest
 *   traces') describes metaphysical truth, not institutional arrangement.
 *   Under this reading, the sustained integration of Shinto and Buddhist
 *   practice in temples, shrines, priesthoods, and lay worship is not the
 *   result of historical accident or state policy, but of deep metaphysical
 *   compatibility. Practitioners and priests benefit from a coherent
 *   interpretive frame that permits them to treat both traditions as
 *   accessing a single underlying reality. This is one of three contending
 *   readings of the shinbutsu kernel; the others partition the domains
 *   functionally and deny the coherence of syncretism altogether. The
 *   authored constraint describes THIS reading's structure and operation, not
 *   the contested kernel as a whole.
 *
 * KEY AGENTS:
 *   - Syncretic priesthood: maintains dual-competence authority, teaches integrated theology, administers mixed temples/shrines; identity-locked to the syncretic frame
 *   - Institutional continuity advocates: temples/shrines with shared space and personnel; benefit from avoiding forced divestment or specialization
 *   - Lay practitioners with syncretic orientation: ordinary believers in integrated communities; derive coherence from the unified-cosmos framing
 *   - Domain-partition advocates: excluded reformers arguing for functional separation of kami (this-world) and buddha (afterlife) domains
 *   - Modern institutional rationalists: excluded scholars and activists arguing syncretism is state-imposed policy, not metaphysical truth
 *   - Theological observers: analytical seat studying the constraint from outside any disputant position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.31).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.44).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Kami-Buddha Ontological Fusion (Syncretic Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/theological/metaphysical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '1dbae26b-f83b-4dd9-a81a-013314cba66a').
narrative_ontology:cs_kernel_codification('1dbae26b-f83b-4dd9-a81a-013314cba66a', distributed).
narrative_ontology:cs_authority_grounding('1dbae26b-f83b-4dd9-a81a-013314cba66a', lineage).
narrative_ontology:cs_interpretation_layer_present('1dbae26b-f83b-4dd9-a81a-013314cba66a').
narrative_ontology:cs_reading_relation('1dbae26b-f83b-4dd9-a81a-013314cba66a', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dbae26b-f83b-4dd9-a81a-013314cba66a', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('1dbae26b-f83b-4dd9-a81a-013314cba66a', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('1dbae26b-f83b-4dd9-a81a-013314cba66a', kami_buddha_ontological_unity, deontological).
narrative_ontology:cs_axiom('1dbae26b-f83b-4dd9-a81a-013314cba66a', foundational, honji_suijaku_metaphysical_necessity).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1dbae26b-f83b-4dd9-a81a-013314cba66a', honji_suijaku_metaphysical_necessity, deontological).
narrative_ontology:cs_reference_frame('1dbae26b-f83b-4dd9-a81a-013314cba66a', unified_cosmology_framework).
narrative_ontology:cs_drift_state('1dbae26b-f83b-4dd9-a81a-013314cba66a', contemporary_institutional_pressure, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('1dbae26b-f83b-4dd9-a81a-013314cba66a', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, institutional_continuity_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_syncretic_orientation).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, metaphysical_complementarity_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains both Shinto and Buddhist ritual competence and theological authority. Their institutional legitimacy rests on the claim that kami and buddhas occupy a unified metaphysical substrate accessible through both traditions' practices. Develops interpretive literature, trains new priests in both systems, and adjudicates which rituals apply to which situations. Exit from this framing would dissolve their professional identity and the institutional structures they oversee.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).

% Buddhist temples and Shinto shrines that share ritual space, personnel, and theological interpretation. They benefit from the syncretic framing because it justifies maintaining integrated compounds and dual-competence priesthoods without requiring a choice between Buddhist or Shinto identity. Separation would force divestment, retraining, or merger.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, institutional_continuity_advocates, beneficiary,
    organized, generational, constrained, national).

% Ordinary believers in regions where kami and buddha worship are integrated—they make offerings, seek blessings, and interpret their own spiritual experience through a unified framework. They derive comfort from the sense that they are navigating a coherent cosmos, not managing two separate supernatural jurisdictions. Exit would require adopting a partition theology or renouncing integrated practice entirely.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_syncretic_orientation, beneficiary,
    powerless, biographical, constrained, local).

% Reformers and scholars who argue that kami govern this-world affairs (health, agriculture, weather) while buddhas govern the afterlife and karmic liberation. They would redesign institutional boundaries to match this partition and are structurally excluded from the syncretic priesthood's authority structure—their reading of the kernel is ruled incoherent or empirically false within the syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, domain_partition_advocates, excluded,
    organized, generational, constrained, national).

% Contemporary reformers (both scholarly and activist) who argue that syncretism is institutional inertia, not metaphysical truth. They point to the Meiji-era separation mandates (shinbutsu bunri) as evidence that syncretism was state policy, not doctrinal necessity. They are excluded from the legitimating conversation because the syncretic reading treats its commitment as pre-institutional—not a state-imposed or institutional choice, but an ontological given.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_institutional_rationalists, excluded,
    organized, biographical, mobile, national).

% Religious scholars and historians who study the constraint from outside any of the disputant positions. They can examine historical documents, interview practitioners, and assess whether the syncretic reading was ever genuinely held as metaphysical truth versus institutional convenience. Their analytical seat provides no stake in the outcome.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, theological_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of managing two separate supernatural traditions (Shinto and Buddhism) without requiring practitioners or priests to choose between them. Permits integrated worship, shared sacred space, unified ritual interpretation, and personnel trained in both systems. The coordination function is: manage multiple religious languages and practices as if they reference a single underlying ontology.
% TRANSFER_FUNCTION: Moves authority and legitimacy from individual choice toward institutional continuity. Practitioners and priests give up the right to treat kami-worship and buddha-worship as genuinely separate domains in exchange for receiving a coherent interpretive framework that permits simultaneity without cognitive dissonance. The constraint extracts time, allegiance, and intellectual commitment to maintain the unified narrative.
% ABSENT_VOICES: Domain-partition advocates and institutional rationalists are present in the contemporary scholarly and reform literature but structurally excluded from the syncretic priesthood's interpretive authority. They would argue that syncretism is institutional arrangement, not metaphysical truth, and that separation is both coherent and historically precedented (Meiji shinbutsu bunri mandates). Their exclusion is maintained by the syncretic reading's assertion that the partition question is not open—that kami and buddhas are already unified at the metaphysical level, making the institutional question settled.
% DISAPPEARANCE_RATIONALE: If the syncretic-fusion reading vanished and practitioners adopted the domain-partition reading instead, the institutional landscape would reorganize: temples and shrines would separate or specialize, dual-competence priesthoods would become anachronistic, ritual spaces would be redesignated, and theological literature would shift from unity-seeking to boundary-maintenance. Believers would reinterpret their spiritual experience through a partition lens. The material and social infrastructure built on syncretism would require wholesale redesign.
% FOUNDING_PROBLEM: Early Japanese religious history presented a theological puzzle: Buddhism arrived in Japan with mature metaphysical systems and institutional forms, yet kami-worship persisted and interacted with Buddhist practice rather than being replaced. The syncretic-fusion reading solves this by asserting that kami and buddhas occupy a unified metaphysical level, making coexistence metaphysically necessary, not institutionally accidental.
% FOUNDING_PROBLEM_CORROBORATION: The syncretic priesthood attests the founding problem is solved and ongoing: kami and buddhas remain metaphysically unified and practitioners continue to need integrated frameworks. Scholars of Japanese religion from outside the priesthood (e.g., historians of the Edo period, anthropologists of contemporary practice) attest that syncretism was indeed institutional practice and widely held, but contest whether it was ever a genuine metaphysical commitment or a pragmatic institutional arrangement that acquired theological justification post-hoc. The Meiji Government's shinbutsu bunri mandates and modern Shinto reformism attest that separation is coherent and institutionally viable, contradicting the claim that unity is metaphysical necessity.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31) because the constraint's primary function is coordination—providing a unified interpretive framework—not rent collection. The syncretic priesthood and institutional continuity advocates benefit materially from avoiding separation, but the extraction is modest because the benefit flows to maintaining status quo, not from active coercive collection. Suppression is moderate (0.44) because the constraint persists partly through the institutional authority of the priesthood (which excludes domain-partition voices) and partly through practical lock-in (priests are trained only in syncretism; believers have invested decades in integrated practice). Theater is low (0.22) because the ritual and theological work is genuinely performed—syncretism is not mere pretense. The temporal series is flat after the early period: once the syncretic reading stabilized institutionally (by ~t=42, Edo consolidation), extractiveness and theater remained stable; no new rent-seeking or performative maintenance emerged at scale. The slight rise in extractiveness from t=28 to t=42 reflects institutional hardening as the priesthood formalized syncretic theology into textual doctrine; after t=42 stabilization occurs.
 *
 * PERSPECTIVAL GAP:
 *   The key asymmetry is between the priesthood/institutional advocates (d low, experience it as coordination) and the partition advocates (excluded, experience it as suppression). The engine should compute priesthood as rope-seat (low extraction, coordination function, low enforcement requirement) and partition advocates as snare-seat (high suppression, no coordination benefit, excluded). The same constraint produces different per-seat types—that is the perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The syncretic priesthood is the beneficiary and agenda-setter (d ~ 0.1): they set the interpretive rules, control training, adjudicate what counts as valid theology, and maintain institutional continuity. Their power is institutional; their exit is identity-locked (priesthood identity is fused with syncretic theology). Lay practitioners occupy a mixed position (d ~ 0.45): they receive genuine coordination benefit (a coherent interpretive frame) but also bear costs (constrained alternatives, identity-lock into the framework). They are powerful only if organized collectively (which they are not). Domain-partition advocates and modern rationalists are excluded (d undefined in traditional sense; they experience suppression without negotiating power). The priesthood's low d is derived from their beneficiary role, institutional power, and arbitrage options (they could adopt partition theology, but doing so would destroy their institutional base—the arbitrage is theoretical, not real; I have not overridden the derivation). The lay practitioners' moderate d reflects the genuine coordination function and modest identity lock-in. No directionality overrides are needed; the structural data produces the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic-fusion reading avoids mandatrophy by maintaining that its founding problem (how to coexist with two religious traditions) remains live. The priesthood attests this: practitioners still need integrated frameworks, kami and buddha worship continue to interact, and the metaphysical commitment remains active. However, the modern rationalists' reading of the founding problem as dead (syncretism was institutional convenience, now superseded by clear separation mandates and voluntary disaffiliation) suggests a potential mandatrophy trajectory: if the domain-partition reading gains institutional adoption, the syncretic-fusion reading would persist as zombie constraint—enforced by institutional inertia and path-dependence rather than living commitment. The theater_ratio's flatness after t=42 could indicate incipient theater (the constraints performs commitment without living change), but the measurement series is too short and the contemporary attestations from practitioners too robust to declare mandatrophy present. An omega addresses this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_vs_institutional_indistinguishability,
    'Can we distinguish between a genuine metaphysical commitment (kami and buddhas are ontologically unified) and an institutional arrangement that acquired theological justification post-hoc?',
    'Historical textual analysis: examine whether syncretic theology preceded institutional integration or followed it. Interview contemporary priests about whether they hold syncretism as metaphysical conviction or institutional doctrine they manage professionally. Ethnographic study of lay practitioners'' actual reasoning about when and why they invoke kami versus buddhas.',
    'If institutional integration clearly preceded theological synthesis, the constraint''s claimed type should shift from rope (coordination around a real metaphysical truth) toward piton (institutional inertia justified by acquired mythology). If contemporary practitioners hold it as genuine metaphysical conviction, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_vs_institutional_indistinguishability, empirical, 'Whether syncretic fusion is metaphysical conviction or post-hoc institutional justification').

omega_variable(
    mandatrophy_trajectory_under_partition_pressure,
    'Is the syncretic reading''s founding problem truly live, or does it persist as zombie constraint due to institutional lock-in and priesthood identity-fusion?',
    'Monitor priesthood and lay-practitioner attestations over 20–30 years; measure whether new theological works defend syncretism as metaphysical necessity or as institutional pragmatism. Track whether Shinto shrines and Buddhist temples disengage from integration when legal and institutional barriers are removed (as in post-Meiji separation movements). Measure whether younger priests in integrated compounds express identity-lock or genuine commitment to the syncretic theology.',
    'If the founding problem is dead and recognized as such within the priesthood, reclassify as piton (performance-maintained constraint). If the problem is genuinely live, rope classification holds. If contestation is increasing without resolution, it may indicate a transition from rope to tangled-rope (coordination + extraction coexist).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_trajectory_under_partition_pressure, empirical, 'Whether the syncretic reading''s founding problem remains live or persists as inertia').

omega_variable(
    alternative_reading_coherence,
    'Is the domain-partition reading structurally coherent, or does it require denying the kami-buddha interactions that practitioners report?',
    'Theological analysis: construct the strongest possible domain-partition theology and test whether it accommodates the full range of kami-buddha interactions documented in historical and contemporary practice (e.g., buddhas with kami attributes, kami appearing in buddha dreams, shared ritual objects). If the partition reading requires denying or reinterpreting documented interactions, it is inherently weaker than the syncretic reading.',
    'If partition is incoherent, the syncretic reading''s claim to metaphysical necessity is strengthened. If partition is equally coherent but requires historical rewriting, the constraint shifts toward tangled-rope (both readings have coordination functions; one requires suppression to maintain institutional advantage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_coherence, conceptual, 'Whether the domain-partition alternative is structurally coherent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t14, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 14, 0.19).
narrative_ontology:measurement_basis(shin_tr_t14, projected).
narrative_ontology:measurement(shin_tr_t28, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 28, 0.21).
narrative_ontology:measurement_basis(shin_tr_t28, observed).
narrative_ontology:measurement(shin_tr_t42, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 42, 0.24).
narrative_ontology:measurement_basis(shin_tr_t42, observed).
narrative_ontology:measurement(shin_tr_t71, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 71, 0.22).
narrative_ontology:measurement_basis(shin_tr_t71, observed).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(shin_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t14, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 14, 0.29).
narrative_ontology:measurement_basis(shin_be_t14, projected).
narrative_ontology:measurement(shin_be_t28, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 28, 0.31).
narrative_ontology:measurement_basis(shin_be_t28, observed).
narrative_ontology:measurement(shin_be_t42, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 42, 0.33).
narrative_ontology:measurement_basis(shin_be_t42, observed).
narrative_ontology:measurement(shin_be_t71, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 71, 0.31).
narrative_ontology:measurement_basis(shin_be_t71, observed).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(shin_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_substrate__syncretic_fusion_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Part of the shinbutsu kernel constraint family (three readings). Syncretic-fusion reading claims kami-buddha ontological unity and honji suijaku as metaphysical truth. Domain-partition reading claims functional separation (kami = this-world, buddhas = afterlife). Incoherent-bundle reading claims syncretism is institutional drift without coherent metaphysical ground. All three readings reference the same underlying historical phenomenon (coexistence of Shinto and Buddhism in Japanese religious practice) but define the constraint differently based on how they answer the unification question. Each story carries its own ε, beneficiary/victim structure, and founding problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
