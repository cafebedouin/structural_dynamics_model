% ============================================================================
% CONSTRAINT STORY: learning_capacity_decline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_learning_capacity_decline, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: learning_capacity_decline
 *   human_readable: Learning Capacity Decline in Aging and Institutional Systems
 *   domain: cognitive_science/institutional_performance
 *
 * SUMMARY:
 *   Learning capacity decline operates at the intersection of biological
 *   aging, institutional rigidity, and knowledge evolution speed. This
 *   constraint demonstrates how a natural process (neurological plasticity
 *   reduction with age) becomes extractive when embedded in institutional
 *   systems that treat knowledge as static, credentials as irreplaceable, and
 *   old learning modalities as permanently valid. The measured extractiveness
 *   (0.58) reflects not pure aging but the *interaction* between aging and
 *   institutional suppression of adaptive learning pathways. The constraint
 *   exhibits all six DR types because it conflates two structurally distinct
 *   mechanisms: the biological limit of aging (mountain) and the
 *   institutional extraction of credential gatekeeping and paradigm
 *   suppression (snare/tangled_rope). Decomposition per ε-invariance
 *   principle reveals that the same natural-language concept ('learning
 *   capacity decline') contains two constraints with radically different ε
 *   values.
 *
 * KEY AGENTS:
 *   - Young Learners: Primary victims (powerless/trapped) — must master outdated frameworks to gain credentials; blocked from challenging established paradigms without career cost
 *   - Mid-Career Practitioners: Secondary victims (moderate/constrained) — face credential inflation and retraining costs; cannot easily exit existing knowledge regime
 *   - Established Knowledge Holders: Primary beneficiaries (institutional/arbitrage) — their accumulated credentials remain valuable longer; can arbitrage between old and new knowledge
 *   - Credentialing Institutions: Beneficiary-administrator (institutional/arbitrage) — control credential design; capture rents from credential inflation; orchestrate knowledge gatekeeping
 *   - Field Communities: Organized secondary actors (organized/constrained) — see genuine coordination from shared epistemology alongside suppression of paradigm challenges
 *   - Aging Individuals in Rigid Systems: Secondary victims (powerless/trapped) — bear full cost of knowledge obsolescence without institutional support for retraining
 *   - Analytical Observer: Systemwide view (analytical/analytical) — risks naturalizing institutional extraction as inherent aging
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(learning_capacity_decline, 0.58).
domain_priors:suppression_score(learning_capacity_decline, 0.68).
domain_priors:theater_ratio(learning_capacity_decline, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(learning_capacity_decline, extractiveness, 0.58).
narrative_ontology:constraint_metric(learning_capacity_decline, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(learning_capacity_decline, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(learning_capacity_decline, tangled_rope).
narrative_ontology:human_readable(learning_capacity_decline, "Learning Capacity Decline in Aging and Institutional Systems").
narrative_ontology:topic_domain(learning_capacity_decline, "cognitive_science/institutional_performance").

domain_priors:requires_active_enforcement(learning_capacity_decline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(learning_capacity_decline, established_knowledge_holders).
narrative_ontology:constraint_beneficiary(learning_capacity_decline, credentialing_institutions).
narrative_ontology:constraint_beneficiary(learning_capacity_decline, incumbent_power_structures).
narrative_ontology:constraint_victim(learning_capacity_decline, younger_learners).
narrative_ontology:constraint_victim(learning_capacity_decline, cognitive_flexibility).
narrative_ontology:constraint_victim(learning_capacity_decline, adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG LEARNER (SNARE) — Faces declining institutional capacity to accommodate novel learning modalities or challenge established paradigms. Career advancement requires mastering obsolete frameworks; challenging them risks exclusion. Trapped by credential dependency and cognitive scaffolding built on outdated foundations. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(learning_capacity_decline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PRACTITIONER (TANGLED ROPE) — Constrained by sunk costs in outdated expertise; retraining is costly but necessary. Benefits from institutional scaffolding that structures learning pathways (coordination function) while being extracted from by credential inflation and the requirement to justify prior learning as still-relevant. Mixed extraction and coordination with significant suppression of exit options.
constraint_indexing:constraint_classification(learning_capacity_decline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Experiences the constraint as a coordination mechanism: credential systems structure social division of labor and signal competence. Benefits from first-mover advantage in credential design and can arbitrage between old and new knowledge regimes. Minimal extraction because they control the boundary conditions of what counts as learning.
constraint_indexing:constraint_classification(learning_capacity_decline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FIELD COMMUNITY (TANGLED ROPE) — Organized agents within a discipline see genuine coordination benefits from shared epistemology and methodological standards (rope function) overlaid with extractive suppression of alternative approaches and slower paradigm shifts. Can organize to challenge stale knowledge but face suppression from vested institutional interests. Coordinating and extracting simultaneously.
constraint_indexing:constraint_classification(learning_capacity_decline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CREDENTIAL THEATER (PITON) — The administrative apparatus of certification, accreditation, and degree-granting is largely performative: it signals completion of a curriculum rather than actual learning capacity. The theater persists through institutional inertia — replaced by portfolios and demonstrated competence in practice, yet maintained because alternatives haven't fully displaced it. Theater ratio indicates degradation.
constraint_indexing:constraint_classification(learning_capacity_decline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational view, aging inevitably reduces neural plasticity and learning speed — this is a neuroscientific law. Institutional learning systems necessarily accumulate inertia over time. Both are structural facts of cognitive systems and organizations. The false summit detector will flag this: the constraint is not inherent to aging or institutions, but to the *interaction* between cognitive capacity and rigid institutional structures. Decompose into separate constraints to reveal the contingency.
constraint_indexing:constraint_classification(learning_capacity_decline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(learning_capacity_decline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(learning_capacity_decline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(learning_capacity_decline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(learning_capacity_decline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(learning_capacity_decline, TR),
    TR >= 0.70.

:- end_tests(learning_capacity_decline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Initial extractiveness (0.35) reflects a coordination function: institutions structure knowledge and credential distribution to enable specialization. As knowledge obsolescence accelerates and credential inflation accumulates, extractiveness rises to 0.58 by period 10. The rise is not primarily biological (aging happens at constant rate) but institutional (institutions have not adjusted knowledge-update cycles to match knowledge-evolution speed). Suppression (0.68): High. Multiple mechanisms suppress adaptive learning: (1) credential sunk costs that make retraining expensive, (2) publication bias in academia that suppresses paradigm challenges, (3) institutional inertia in curriculum design, (4) career risk of admitting knowledge obsolescence. Younger learners face suppression through credential requirements; mid-career workers face suppression through credential inflation; field communities face suppression through paradigm gatekeeping. Theater ratio (0.65): Moderately high and rising. Credentialing systems increasingly perform their function without delivering learning capacity verification — degree completion signals persistence and conformity more than competence. As knowledge accelerates, the credential's signaling value decouples from actual learning capacity. Theater has risen from 0.52 to 0.65, indicating degradation of the credential's functional meaning relative to its administrative/performative form.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between powerless and institutional is maximal. Young learners see a Snare (extraction without coordination benefit from their position). Credentialing institutions see a Rope (pure coordination, no extraction from their position). The analytical observer must recognize that both are true perspectives on the same structural data — the institution's coordination function IS genuine, but it is overlaid with extraction of those trapped in the system. The gap reveals that Tangled Rope is the constraint's true classification: genuine coordination (knowledge structuring) + asymmetric extraction (credential gatekeeping, paradigm suppression) + active enforcement (institutional gatekeeping of what counts as valid knowledge). The young learner's Snare perspective reveals the extraction; the institution's Rope perspective reveals the coordination; the analytical truth is both simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) produces different experienced extractiveness for each agent. Young learner: χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (maximum experienced extraction). Credentialing institution: χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative — they experience the constraint as subsidy, not extraction). Mid-career practitioner: χ ≈ 0.58 × 1.00 × 1.0 ≈ 0.58 (moderate experienced extraction, aligned with base). The directionality derivation chains through: beneficiary status + arbitrage exit → low d → negative f(d) for institution; victim status + trapped exit → high d → high f(d) for young learner. These are not overridden — the structural relationships are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by decomposition. The natural-language concept 'learning capacity decline' contains TWO structurally distinct constraints: (1) aging-driven neurological plasticity decline (biological aging limit, ε ≈ 0.08, Mountain), and (2) institutional-extraction-via-credential-gatekeeping (extractive institutional rigidity, ε ≈ 0.58, Tangled Rope). The measured extractiveness of 0.58 is the COMBINED effect of aging AND institutional rigidity. If we measure the constraint under conditions of optimal institutional support (frequent retraining, credential modularity, paradigm openness), extractiveness drops toward 0.15-0.25 (biological aging plus normal learning costs). If we measure under conditions of institutional rigidity (fixed credentials, high retraining costs, paradigm gatekeeping), extractiveness rises to 0.65+. The constraint is ε-variant across observables: different institutional contexts produce different ε values for the same nominal constraint. Therefore, two stories must be written: learning_capacity_biological_aging (ε ≈ 0.08, Mountain) and learning_capacity_institutional_extraction (ε ≈ 0.58, Tangled Rope), linked via network.affects_constraints. The mandatrophy is resolved by refusing the single-story frame and decomposing per ε-invariance principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_institutional_learning_decline,
    'Is the measured learning capacity decline primarily biological (aging-driven neurological changes) or institutional (rigidity of knowledge systems and credentialing structures)?',
    'Compare learning capacity trajectories: (a) in individuals transferred to more adaptive institutional contexts (less rigid curricula, shorter feedback loops), (b) in cohorts before/after institutional reforms that lower credential inflation. If institutional context significantly modulates decline, the constraint is institutional, not biological.',
    'If primarily biological: constraint is mountain (immutable aging process). If primarily institutional: constraint is snare/tangled_rope (contingent extraction overlaid on normal aging). If both-and: need separate constraint stories for biological aging limit vs institutional rigidity, linked via network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_vs_institutional_learning_decline, empirical, 'Whether learning decline is driven by biology or institutional rigidity').

omega_variable(
    credential_inflation_extraction_mechanism,
    'Does the constraint extract through credential inflation (requiring ever-higher qualification for the same work) or through epistemological gatekeeping (controlling what counts as valid knowledge)?',
    'Historical analysis of job requirement creep vs actual competency requirements; comparison of credential requirements with job performance data; tracking of paradigm-challenge suppression across disciplines.',
    'If credential inflation dominates: extractiveness is driven by institutional competition, not learning decline per se. If epistemological gatekeeping dominates: the constraint is about power over knowledge, not capacity. Different mechanisms → different omega variables and different measurement trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_extraction_mechanism, empirical, 'Whether extraction is via credential inflation or epistemological gatekeeping').

omega_variable(
    plasticity_retraining_sufficiency,
    'Given adequate motivated retraining time and resources, can mid-career and older learners achieve functional parity with younger learners on novel material?',
    'Longitudinal studies of adult learners given intensive retraining in new domains (coding bootcamps, career switches, skill retraining programs). Track both speed and ultimate competence. Compare cohorts with different institutional support levels.',
    'If yes: learning decline is partly institutional (systems not providing adequate retraining time/resources), and suppression metric should be revised downward. If no: decline is biological, supporting mountain classification for that component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plasticity_retraining_sufficiency, empirical, 'Whether older learners can achieve parity with adequate retraining').

omega_variable(
    knowledge_obsolescence_rhythm_mismatch,
    'Is the primary constraint the speed of knowledge change relative to institutional knowledge-update cycles, or the speed of knowledge change relative to biological learning speed?',
    'Measure institutional update lag (time from knowledge emergence to curriculum/credentialing change) and compare to both biological learning speed and domain knowledge-halflife. Identify which lag dominates the suppression metric.',
    'If institutional lag dominates: snare/tangled_rope (institutional extraction). If biological lag dominates: mountain (limit of aging biology). If mismatch is the interaction: separate stories for biological aging + institutional rigidity, linked via network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_obsolescence_rhythm_mismatch, empirical, 'Whether constraint is knowledge-speed vs biological-speed or their mismatch').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(learning_capacity_decline, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lcd_tr_t0, learning_capacity_decline, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lcd_tr_t5, learning_capacity_decline, theater_ratio, 5, 0.59).
narrative_ontology:measurement(lcd_tr_t10, learning_capacity_decline, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(lcd_be_t0, learning_capacity_decline, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lcd_be_t5, learning_capacity_decline, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lcd_be_t10, learning_capacity_decline, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(learning_capacity_decline, identity_coordination).
narrative_ontology:affects_constraint(learning_capacity_decline, credential_inflation).
narrative_ontology:affects_constraint(learning_capacity_decline, paradigm_entrenchment).
narrative_ontology:affects_constraint(learning_capacity_decline, knowledge_obsolescence_rhythm).

% DUAL FORMULATION NOTE:
% Learning capacity decline decomposes into three independent constraints per ε-invariance principle: (1) learning_capacity_biological_aging (ε ≈ 0.08, Mountain) — neurological plasticity limit with age, emerges naturally, no suppression; (2) learning_capacity_institutional_extraction (ε ≈ 0.58, Tangled Rope) — credential gatekeeping and paradigm suppression overlaid on normal aging; (3) knowledge_obsolescence_rhythm (ε ≈ 0.42, Tangled Rope) — mismatch between knowledge evolution speed and institutional update cycles. These three are upstream of credential_inflation (a snare where learning decline is weaponized to justify higher credentials) and paradigm_entrenchment (a snare where stale paradigms suppress challenge). The present story models the interaction; sibling stories model the components separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
