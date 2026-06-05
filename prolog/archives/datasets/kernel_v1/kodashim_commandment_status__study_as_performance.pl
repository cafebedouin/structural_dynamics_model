% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Commandment Status: Study as Performance Reading
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates a specific reading of the kodashim
 *   (sacrificial laws) kernel in rabbinic Judaism: the claim that studying
 *   sacrifice laws constitutes genuine performance of the commandment to
 *   offer sacrifices, even after Temple destruction made physical enactment
 *   impossible. This reading, found in classical Talmudic sources and
 *   medieval interpretive traditions, resolves the problem of commandment
 *   obligation after 70 CE by collapsing the distinction between intellectual
 *   engagement with law and practical performance. The reading maintains that
 *   the commandment's force remains undiminished because it derives from
 *   understanding the purpose and meaning of sacrifice, not from physical
 *   altar activity. This is ONE of three structurally distinct readings of
 *   the same kernel: performance_only (commandment is suspended without
 *   Temple), messianic_deferral (commandment deferred but not obsolete, study
 *   maintains readiness), and study_as_performance (study constitutes full
 *   performance). Each reading has different implications for extractiveness,
 *   obligation status, and the victim set.
 *
 * KEY AGENTS:
 *   - Interpretive Community: Primary beneficiary (organized/mobile) — scholars and students who maintain commandment through collective intellectual work. Benefit from meaningful engagement and tradition continuity. Zero extraction.
 *   - Halakhic Tradition: Secondary beneficiary (institutional/arbitrage) — lineage authority sustaining commandment obligation across generations. Preserves coherence of legal system. Zero extraction.
 *   - Individual Practitioner: Constrained agent (powerful/constrained) — bound by obligation to study; experiences study-as-performance as temporary substitute pending messianic restoration. Low but nonzero constraint cost.
 *   - Commandment Obligation Itself: Abstract but structurally real — the reading maintains its operative force; no victim because obligation is preserved rather than suspended.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Commandment Status: Study as Performance Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious_studies/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '541897eb-910e-4990-ae2e-a778f596d0a0').
narrative_ontology:cs_kernel_codification('541897eb-910e-4990-ae2e-a778f596d0a0', fixed_text).
narrative_ontology:cs_authority_grounding('541897eb-910e-4990-ae2e-a778f596d0a0', lineage).
narrative_ontology:cs_interpretation_layer_present('541897eb-910e-4990-ae2e-a778f596d0a0').
narrative_ontology:cs_reading_relation('541897eb-910e-4990-ae2e-a778f596d0a0', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('541897eb-910e-4990-ae2e-a778f596d0a0', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('541897eb-910e-4990-ae2e-a778f596d0a0', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('541897eb-910e-4990-ae2e-a778f596d0a0', study_constitutes_performance, deontological).
narrative_ontology:cs_axiom('541897eb-910e-4990-ae2e-a778f596d0a0', foundational, commandment_obligation_persists).
narrative_ontology:cs_axiom_status(commandment_obligation_persists, holdable).
narrative_ontology:cs_axiom_grounding('541897eb-910e-4990-ae2e-a778f596d0a0', commandment_obligation_persists, deontological).
narrative_ontology:cs_reference_frame('541897eb-910e-4990-ae2e-a778f596d0a0', study_as_commandment_enactment).
narrative_ontology:cs_drift_state('541897eb-910e-4990-ae2e-a778f596d0a0', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('541897eb-910e-4990-ae2e-a778f596d0a0', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, talmudic_interpretive_community).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, commandment_sustenance_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANIZED INTERPRETIVE COMMUNITY (ROPE) — Scholars and students coordinating through textual engagement maintain the commandment's force through collective intellectual work. Pure coordination: the constraint (that studying kodashim laws constitutes performance of the commandment) solves the collective action problem of preserving commandment obligation in post-Temple conditions. No extraction; the community benefits from meaningful engagement with sacred text and from maintaining continuity with prior tradition.
constraint_indexing:constraint_classification(kodashim_commandment_status__study_as_performance, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: HALAKHIC TRADITION / LINEAGE AUTHORITY (ROPE) — The rabbinic tradition sustains the commandment's operative force across generations through the study mechanism. This is pure coordination: the tradition solves the legitimacy problem (how can a commandment persist when its physical enactment is impossible?) through interpretive innovation. The tradition benefits from having a coherent answer to the suspension of Temple sacrifice; communities benefit from the coordination mechanism itself.
constraint_indexing:constraint_classification(kodashim_commandment_status__study_as_performance, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: INDIVIDUAL PRACTITIONER / MESSIANIC ANTICIPATION (SCAFFOLD) — From the perspective of a contemporary observer aware of the messianic expectation, studying kodashim laws is a temporary substitute for actual sacrifice performance. This is scaffold-type coordination: the study mechanism has a sunset clause embedded in its own theological premises — when the Temple is rebuilt and sacrifices resume, study reverts to supplementary status. The practitioner experiences this as temporary, bounded, renewable-if-messianic.
constraint_indexing:constraint_classification(kodashim_commandment_status__study_as_performance, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From the analytical perspective, the identification of study with commandment performance is logically necessary given the premises: if a commandment must be maintained but its physical enactment is impossible, and if the commandment's force derives from intellectual understanding of its purpose (a rabbinic premise), then study that engages that understanding logically sustains the commandment. This appears as a structural inevitability rather than a contingent choice.
constraint_indexing:constraint_classification(kodashim_commandment_status__study_as_performance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Minimal. This reading has essentially zero extractiveness from the performance gap because it claims that study CLOSES the gap entirely — intellectual engagement is not compensation for missed performance, but genuine performance itself. The minimal residual 0.05 accounts for the possibility that this reading is aspirational theater (covered by the 'knowledge_sufficiency_for_performance' omega). Suppression (0.08): Very low. The reading faces no significant barriers to adoption or practice. Scholars can study; commandment obligation persists; no enforcement against the reading itself. The minimal residual reflects only the general epistemic difficulty of evaluating counterfactuals about biblical intent (is study really what the sacrificial commandment was 'for'?). Theater ratio (0.35): Low. Study of sacrifice laws is functionally substantive — it involves detailed textual analysis, legal reasoning, historical reconstruction — not merely performative gesture. The 0.35 reflects the component of theater in the reading itself (the interpretive claim that this constitutes performance may be creative reinterpretation rather than literal truth), but study-as-practice is not theater.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces a narrow perspectival gap because the claim is that the performance gap itself closes. The organized community and institutional tradition both see pure coordination (Rope) because they benefit from the mechanism without extraction. The individual practitioner sees this as temporary (Scaffold) because they anticipate future Temple restoration. The analytical observer at civilizational scale sees logical necessity (Mountain) — given the premises, study-as-performance is structurally inevitable. The gap is not between victims and beneficiaries, but between those who view study-as-performance as permanent solution (community, tradition perspectives) vs. those who embed it in messianic temporality (practitioner) or necessity (analytical). Notably, this reading produces an empty victim set — the whole point is that no one is harmed by the suspension of physical sacrifice because study maintains the commandment's force.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has minimal directionality variation because extraction is collapsed. The beneficiaries (interpretive community, tradition) experience zero chi because they benefit from the coordination mechanism with no cost. The constrained agent (individual practitioner) experiences low chi because they are bound by obligation but the obligation is meaningful and fulfillable. There is no trapped or powerless perspective — the reading's entire structure is that all parties can engage fully in study, maintaining their obligations. If the reading is correct, the constraint is pure coordination (Rope) from all perspectives. If the reading is aspirational theater, some perspectives would show higher chi, but that is captured by the omega variables, not by directionality shifts.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy entirely by refusing the premise that suspends the commandment. The performance_only and messianic_deferral readings face mandatrophy: if the commandment is truly suspended or deferred, what obligates study? study becomes coordination theater without obligation — victims exist (those bound to study by residual obligation without real commandment force). The study_as_performance reading blocks this by claiming that study IS the commandment's enactment in this era. If true, obligation and enactment align; no gap produces extraction. If false (captured by the knowledge_sufficiency omega), the reading collapses into messianic_deferral, and mandatrophy surfaces. The reading is structurally coherent precisely because it claims to close the performance gap rather than defer it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commandment_suspension_scope,
    'Does the commandment status change when Temple sacrifice becomes impossible, or does only the enactment modality change while obligation persists?',
    'Analysis of halakhic texts on obligation vs. enactment; comparison with other suspended commandments (e.g., harvest laws outside Israel); determination of whether obligation is temporal or conditional on physical possibility',
    'If obligation persists: study-as-performance reading is coherent and maintains zero extractiveness. If obligation suspends: performance_only reading becomes structurally mandatory, elevating extractiveness for those bound by obligation without enactment option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commandment_suspension_scope, conceptual, 'Whether commandment obligation persists when physical enactment is impossible').

omega_variable(
    knowledge_sufficiency_for_performance,
    'Does intellectual engagement with sacrifice law constitute genuine performance of the commandment, or is it a proxy that substitutes for genuine performance?',
    'Textual analysis of sources claiming study-as-performance; comparison with parallel structures in other religious systems; determination of whether ''performance'' refers to external enactment or internal intellectual state',
    'If knowledge is sufficient: study-as-performance reading has zero extractiveness and maintains commandment continuity. If knowledge is insufficient: the reading is aspirational theater, elevating extractiveness and supporting performance_only or messianic_deferral readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_sufficiency_for_performance, conceptual, 'Whether intellectual engagement constitutes genuine commandment performance').

omega_variable(
    messianic_temporality_embedding,
    'Is the study-as-performance reading coherent only with an embedded messianic cosmology, or is it stable as a permanent substitute?',
    'Historical analysis of when this reading emerged; determination of whether it is framed as provisional (pending messianic restoration) or permanent; identification of what conditions would trigger reversion or transformation',
    'If provisional: the scaffold perspective is structurally accurate, and reading relations should emphasize coexistence-with-sunset. If permanent: the reading claims genuine replacement of performance by study, foreclosing the performance_only reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_temporality_embedding, conceptual, 'Whether study-as-performance is permanent or embedded in messianic temporality').

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the kodashim_commandment_status kernel, or a distinct constraint about the epistemology of religious obligation?',
    'Verification that the study-as-performance claim is grounded in dispute over the same kernel (commandment status post-Temple) rather than a separate claim about epistemology; mapping of which textual authorities generate this reading vs. alternatives',
    'If same kernel: network relations and omega variables correctly model the constraint as a reading with siblings. If distinct constraint: the constraint should be decomposed and network-linked rather than embedded in committer frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this is a kernel reading or a distinct constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.3).
narrative_ontology:measurement(kodashim_study_tr_t5, kodashim_commandment_status__study_as_performance, theater_ratio, 5, 0.35).
narrative_ontology:measurement(kodashim_study_tr_t10, kodashim_commandment_status__study_as_performance, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(kodashim_study_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(kodashim_study_be_t5, kodashim_commandment_status__study_as_performance, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(kodashim_study_be_t10, kodashim_commandment_status__study_as_performance, base_extractiveness, 10, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__study_as_performance, 0.05).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, temple_reconstruction_eschatology).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, post_destruction_commandment_obligation).

% DUAL FORMULATION NOTE:
% The kodashim_commandment_status kernel admits three structurally distinct readings with different extractiveness values and victim sets. This constraint (study_as_performance, ε=0.05, zero victims) is upstream to performance_only (ε≥0.46, significant victims) and messianic_deferral (ε≈0.25, deferred victims). Each reading should be authored as a separate constraint story. The network links them as siblings of the same kernel dispute, not as causal dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
