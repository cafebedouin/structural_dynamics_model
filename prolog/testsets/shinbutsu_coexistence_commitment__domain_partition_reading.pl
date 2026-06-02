% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: Functional Coexistence Without Theological Unification
 *   domain: religious_philosophy/japanese_syncretism/commitment_systems
 *
 * SUMMARY:
 *   For approximately 1,200 years, Japanese religious practice maintained a
 *   functional coexistence between kami-based Shinto and Buddhist metaphysics
 *   without requiring theological unification. This reading instantiates the
 *   domain-partition interpretation: the constraint is a coordinated
 *   assignment of existential domains — kami govern life, fertility, purity,
 *   seasonal cycles; Buddhas govern death, rebirth, karmic salvation,
 *   ancestor veneration. The partition requires boundary maintenance but not
 *   doctrinal synthesis. This reading interprets the observed separation as a
 *   stable, legitimate, low-extraction coordination mechanism rather than as
 *   incoherent syncretism (syncretic_fusion_reading) or as unstable pragmatic
 *   bundling (incoherent_bundle_reading). The constraint exhibits genuine
 *   Rope properties: it solves real coordination problems (how to manage life
 *   and death transitions without a unified metaphysical framework), involves
 *   minimal coercion, and benefits multiple institutional actors (Buddhist
 *   temples, Shinto shrines, popular practitioners) without severe asymmetric
 *   extraction. Theater ratio remains low across the interval (0.32–0.38)
 *   because the partition is functional — boundary markers are clear, ritual
 *   specialists are distinct, and practitioners experience the separation as
 *   legitimate rather than performative.
 *
 * KEY AGENTS:
 *   - Village Practitioners: Primary beneficiary (moderate/constrained) — the domain partition enables coordination of life and death management without requiring intellectual unification; experience low extraction.
 *   - Buddhist Institutional Authority: Primary beneficiary (institutional/arbitrage) — exclusive domain over death, funerary rites, ancestor veneration grants high institutional legitimacy and necessity; experience pure coordination benefit.
 *   - Shinto Sanctuary Authority: Primary beneficiary (institutional/arbitrage) — exclusive domain over life, agricultural blessing, purification grants high institutional legitimacy; symmetrically positioned to Buddhist authority.
 *   - Reformist Intellectuals / State Actors: Secondary critic (powerful/mobile) — Neo-Confucian scholars and Meiji modernizers viewed partition as incoherent constraint preventing unified Shinto nationalism; sought unification through state power (Kami-only or Buddhist-only synthesis).
 *   - Analytical Observer: Comparative perspective (analytical/analytical) — civilizational analysis recognizes domain partition as coherent solution to alignment problem between incompatible soteriologies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.18).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Kami-Buddha Domain Partition: Functional Coexistence Without Theological Unification").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_philosophy/japanese_syncretism/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, '5f09bc55-0b8d-413e-848a-a1c3e0484a9e').
narrative_ontology:cs_kernel_codification('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', distributed).
narrative_ontology:cs_authority_grounding('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', practice).
narrative_ontology:cs_interpretation_layer_present('5f09bc55-0b8d-413e-848a-a1c3e0484a9e').
narrative_ontology:cs_reading_relation('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', foundational, domain_partition_solves_genuine_alignment_problem).
narrative_ontology:cs_axiom_status(domain_partition_solves_genuine_alignment_problem, holdable).
narrative_ontology:cs_axiom_grounding('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', domain_partition_solves_genuine_alignment_problem, instrumental).
narrative_ontology:cs_axiom('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', foundational, boundary_maintenance_legitimate_without_ontological_unification).
narrative_ontology:cs_axiom_status(boundary_maintenance_legitimate_without_ontological_unification, holdable).
narrative_ontology:cs_axiom_grounding('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', boundary_maintenance_legitimate_without_ontological_unification, conventional).
narrative_ontology:cs_reference_frame('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', functional_coexistence_as_legitimate_coordination).
narrative_ontology:cs_drift_state('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', contemporary_post_separation_state_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5f09bc55-0b8d-413e-848a-a1c3e0484a9e', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, popular_religious_practice).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shinto_sanctuary_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (ROPE) — The domain partition enables genuine coordination of life and death management without requiring intellectual unification. A household performs kami rituals for harvest and purification, Buddhist rituals for ancestor veneration and death ceremonies. The constraint is experienced as low-extraction coordination: separate specialists, separate calendars, separate ontologies. Exit costs are moderate (violating village norms) but the system solves real coordination problems (agricultural cycles, life transitions) with minimal extraction overhead.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: BUDDHIST INSTITUTIONAL AUTHORITY (ROPE) — The partition grants Buddhist institutions exclusive domain over death and afterlife — funerary rites, ancestor veneration, mortuary liturgy. This is genuine coordination benefit: Buddhist temples become essential institutional mediators for existentially significant life transitions. No extraction overhead; the authority is experienced as legitimate because the domain boundary is clear and the service is necessary. Arbitrage exit is available — temples can shift resources between death-rites and other Buddhist functions — but the partition makes death-rites maximally valuable.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SHINTO SANCTUARY AUTHORITY (ROPE) — Symmetrically, the partition grants Shinto shrines exclusive domain over life: agricultural cycles, birth purification, harvest blessing, pollution cleansing. The authority is experienced as legitimate and extractive-free — the services fill genuine functional needs and no theological unification is required. Shinto practitioners experience low suppression because the constraint does not demand doctrinal coherence; it demands only functional separation.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORMIST INTELLECTUAL / STATE ACTOR (TANGLED ROPE) — Neo-Confucian scholars and Meiji modernizers viewed the partition as incoherent and sought unification — 'true' Shinto purified of Buddhist corruption. This perspective sees extraction: the partition prevents intellectual coherence and requires maintenance of two parallel institutional systems. But reformists also benefited from the partition's clarity in their critique — they could envision a unified (and nationalist) Shinto system precisely because the current partition was so stark. This creates tension: the reformist movement used the partition as a scaffold for its own synthesis project while treating the partition itself as a constraint to be overcome.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__domain_partition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational comparative perspective, the domain partition is a genuine coordination mechanism: it solves the alignment problem between two incompatible soteriological systems (kami-based life continuity vs Buddha-based karmic rebirth) by assigning them to separate jurisdictions. The constraint enforces boundary maintenance, not doctrinal unity. This is low-extraction coordination: it minimizes theological contradiction and maximizes institutional functionality. The analytical observer sees the partition as a coherent solution to a hard problem — how to maintain two incompatible metaphysical systems without requiring agents to adopt one coherent worldview.
constraint_indexing:constraint_classification(shinbutsu_coexistence_commitment__domain_partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The domain partition creates institutional niches and legitimate authority domains without significant asymmetric extraction. Buddhist temples provide genuine services (funeral rites, ancestor veneration) that fill existentially important functions. Shinto shrines provide genuine services (agricultural blessing, purification) that fill complementary functions. Neither institutional sector extracts from the other through the partition; instead, the partition clarifies their respective domains and makes both more legitimate. The modest (0.22) rather than minimal (0.05) value reflects slight efficiency costs — maintaining two separate specialist systems requires more institutional infrastructure than a unified system would. But this is coordination cost, not extraction. Suppression (0.18): Low. The partition does not suppress alternatives through coercion or epistemically closed reasoning. Practitioners are not forbidden from seeking Buddhist teaching about life or kami blessing for the dead — the partition is a default coordination mechanism, not an enforcement rule. The modest value reflects social expectation (crossing domains carries minor social cost) but no doctrinal prohibition or institutional punishment. Theater ratio (0.35): Low. The boundary markers are functionally clear — temples handle death, shrines handle life — and practitioners experience the separation as substantive rather than performative. Theater increases modestly over the interval (to 0.38) as Edo-period reformist critique of 'incoherent syncretism' enters popular discourse, introducing some performative boundary-maintenance language alongside the functional separation. But even at 0.38, the ratio indicates genuine coordination function dominates over performative theater.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces coherent perspectival structure. Beneficiaries (village practitioners, Buddhist authorities, Shinto authorities) classify the partition as Rope — they experience genuine coordination benefit with low extraction. The reformist perspective produces Tangled Rope because they experience the partition simultaneously as (a) a constraint preventing intellectual unification they seek, and (b) a scaffold enabling their critique and vision of synthesis. The analytical observer sees Rope with full awareness of its contingency — the partition is a genuine coordination solution but not a natural law or inevitable structure. The perspectival gap is between beneficiaries (who experience the partition as legitimate and functional) and reformists (who experience it as a problem to be overcome). This gap reflects real structural difference: beneficiaries occupy niches within the partition and have low incentive to unify; reformists occupy a meta-institutional position (intellectual authority, later state authority) that seeks unification as a modernization and purification project.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by its structural position within the partition. Village practitioners are neither pure beneficiaries nor pure victims — they benefit from the clarity of the domain partition but bear modest costs of maintaining two specialist systems. Buddhist authorities are net beneficiaries (institutional/arbitrage position): exclusive domain over death makes them institutionally essential and gives them high autonomy over their functional area. Shinto authorities are symmetrically positioned — their exclusive domain over life makes them essential and autonomous. The reformist intellectual perspective occupies a unique position: powerful/mobile with exit options, they can choose to critique or endorse the partition. Directionality for reformists is moderate (d ≈ 0.50) — they benefit intellectually from the partition's clarity (giving them something clear to critique and unified to build from) while experiencing extraction from the partition's resistance to their unification agenda. The analytical observer is at d ≈ 0.73 (far from beneficiary position) — they perceive the full structural complexity, including both the coordination function AND the contingency of the partition, and thus cannot fully inhabit the beneficiary perspective that the partition seems natural.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves potential mandatrophy ('Is shinbutsu coexistence coordination or incoherent compromise?') by demonstrating that the domain partition IS a genuine coordination mechanism — it solves the real alignment problem of how to maintain two incompatible soteriological systems in shared society without requiring unified metaphysics. The partition does not represent failed synthesis (incoherent_bundle_reading) or unstable pragmatism awaiting fusion (syncretic_fusion_reading). Rather, it represents a stable, low-extraction solution that respects both the coherence of each system internally AND the pragmatic requirement that practitioners navigate both domains across life and death. The mandatrophy is resolved by recognizing domain-partition as a valid coordination innovation, analogous to how technical standards coordinate across incompatible platforms without requiring unified architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_ontological_depth,
    'Is the domain partition a shallow pragmatic boundary (separate practice domains) or a deep ontological commitment (two genuinely incompatible metaphysical systems)?',
    'Analysis of theological texts, doctrinal statements, and practitioner interviews across periods. Markers: If practitioners report actual metaphysical confusion or cognitive strain when crossing domains, partition is shallow. If practitioners report clear conceptual separation and no experienced contradiction, partition is ontologically substantive.',
    'If shallow: the constraint is closer to Piton (performative boundary maintenance). If deep: the constraint is genuinely Rope (solves a hard coordination problem). Extraction vs coordination distinction turns on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_ontological_depth, conceptual, 'Whether domain partition is pragmatic boundary or deep ontological commitment').

omega_variable(
    syncretic_fusion_alternative_viability,
    'Could the syncretic_fusion_reading (unified kami-Buddha theology) have developed as a live alternative to domain_partition_reading within Japanese religious history?',
    'Historical counterfactual analysis. Markers: Chinese precedent of fajiao (dharma teaching) absorption into folk religion; Tibetan Buddhist integration of local deities; Christian syncretism models. If comparable contexts produce fusion, partition was contingent and could have gone otherwise. If fusion consistently fails for kami-Buddha specifically, partition may reflect structural necessity.',
    'If partition was contingent: syncretic_fusion_reading forecloses this reading (both cannot coexist in unified framework). If partition was necessary: syncretic_fusion_reading coexists but faces structural pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_fusion_alternative_viability, empirical, 'Historical viability of alternative syncretic fusion development').

omega_variable(
    reformation_movement_causality,
    'Did Neo-Confucian and Meiji-era reformist movements (seeking to unify or purify Shinto) arise because the domain partition was experienced as an extractive constraint, or did they weaponize the partition''s clarity for other political/intellectual agendas?',
    'Textual analysis of reform rhetoric. Markers: If reformers consistently framed partition as ''incoherent burden,'' extraction narrative. If reformers framed partition as ''opportunity for purification/unification,'' agenda narrative. If mixed: distinguishable subgroups with different rationales.',
    'If extraction narrative valid: partition was experienced as Tangled Rope burden that reformists sought to shed. If agenda narrative valid: reformists opportunistically weaponized partition without experiencing it as constraint. Affects interpretation of whether Tangled Rope perspective represents genuine structural experience or external critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_movement_causality, empirical, 'Whether reformation movements experienced partition as extractive or weaponized it').

omega_variable(
    reading_framework_identity,
    'What is the kernel reading that THIS constraint instantiates? Does ''domain partition'' refer to (A) the observed functional separation between kami and Buddha practices, or (B) a specific historical reading/commitment that INTERPRETS the observed separation as ontologically justified?',
    'Clarification of whether this story describes the constraint AS PRACTICED (observational domain) or the constraint AS THEORIZED in a specific tradition (reading domain). If (A), the constraint is about functional boundaries. If (B), the constraint is about how practitioners and intellectuals have legitimated those boundaries.',
    'If (A): the domain_partition_reading is descriptive; sibling readings describe alternative practices. If (B): the domain_partition_reading is normative; sibling readings describe alternative INTERPRETATIONS of the same practices. Affects entire framing of reading_relations and axioms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framework_identity, conceptual, 'Whether constraint describes observed practice or theoretical reading of practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_dp_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(shinbutsu_dp_tr_t400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 400, 0.35).
narrative_ontology:measurement(shinbutsu_dp_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.38).

% Extraction over time
narrative_ontology:measurement(shinbutsu_dp_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(shinbutsu_dp_be_t400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 400, 0.2).
narrative_ontology:measurement(shinbutsu_dp_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The kernel shinbutsu_coexistence_commitment admits three structurally distinct readings, each with different ε values and claim types. domain_partition_reading (this constraint, ε≈0.22, Rope) interprets coexistence as legitimate domain separation; syncretic_fusion_reading (downstream, ε≈0.50+, Tangled Rope or Snare) interprets coexistence as unstable syncretism requiring synthesis; incoherent_bundle_reading (downstream, ε≈0.35, Piton) interprets coexistence as pragmatic bundling with embedded incoherence. Each reading generates a different constraint story with a different base_extractiveness, different beneficiary/victim structure, and different classification. The readings are not observational variants of one constraint — they are distinct readings of a contested kernel that produce measurably different structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__domain_partition_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
