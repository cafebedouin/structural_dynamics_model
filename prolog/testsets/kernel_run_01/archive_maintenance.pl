% ============================================================================
% CONSTRAINT STORY: archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_archive_maintenance, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: archive_maintenance
 *   human_readable: Archive Maintenance as Sacrificial Preparation
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   The archive maintenance constraint emerges from a specific halakhic
 *   problem: the commandment to offer sacrifices is suspended because the
 *   Temple does not exist and cannot be ritually constructed in the present
 *   era. Yet the detailed technical knowledge required to execute sacrifices
 *   (architectural specifications, ritual procedures, material requirements)
 *   must be preserved for the (disputed) future state when restoration
 *   becomes possible. This reading frames archive maintenance as a legitimate
 *   reinterpretation of the sacrifice commandment itself: the duty to
 *   preserve the commandment's knowledge is continuous with the original
 *   obligation, deferred into a knowledge-preservation mode. The constraint
 *   exhibits the structural signature of Tangled Rope from the analytical
 *   center: genuine coordination function (future generations benefit from
 *   preserved knowledge without rediscovery cost) mixed with significant
 *   extraction (present generations bear substantial study and contemplative
 *   labor without experiencing present sacrificial function). The theater
 *   ratio (0.58) reflects that much of the embodied practice is performative
 *   — formalized repetition of texts now archived in written form,
 *   maintaining ritual structure as the primary function rather than ensuring
 *   knowledge decay is prevented. The extractiveness has increased over the
 *   measurement interval (0.35 → 0.48) as written documentation has improved:
 *   the case for embodied practice as necessary to knowledge preservation has
 *   weakened, yet the obligation persists, shifting the constraint toward
 *   pure extraction.
 *
 * KEY AGENTS:
 *   - Present Worshipping Community: Primary victim (powerless/trapped) — obligated to maintain knowledge they cannot deploy in living practice; bears full cost of deferred commandment.
 *   - Future Restoration Cohort: Primary beneficiary (institutional/arbitrage) — receives freely available technical knowledge, enabling restoration without rediscovery; experiences pure coordination benefit.
 *   - Interpretive Establishment: Secondary beneficiary (moderate/constrained) — maintains institutional authority as knowledge-steward; constrained by obligation to preserve but also benefits from the role.
 *   - Reform Coalition: Organized analyst (organized/mobile) — proposes that archive maintenance be treated as temporary coordination problem solvable through written documentation and education, with clear sunset as archives mature.
 *   - Inherited Obligation Structure: Institutional inertia (institutional/constrained) — the regime persists through normative force and role identity, even as its functional necessity has declined.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent obligation (knowledge preservation for a disputed future state) as eternal law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(archive_maintenance, 0.48).
domain_priors:suppression_score(archive_maintenance, 0.52).
domain_priors:theater_ratio(archive_maintenance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(archive_maintenance, extractiveness, 0.48).
narrative_ontology:constraint_metric(archive_maintenance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(archive_maintenance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(archive_maintenance, tangled_rope).
narrative_ontology:human_readable(archive_maintenance, "Archive Maintenance as Sacrificial Preparation").
narrative_ontology:topic_domain(archive_maintenance, "religious_studies/halakhic_theory/commitment_system").

domain_priors:requires_active_enforcement(archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(archive_maintenance, 'a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451').
narrative_ontology:cs_created_at('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', '').
narrative_ontology:cs_kernel_codification('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', formalized).
narrative_ontology:cs_authority_grounding('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', lineage).
narrative_ontology:cs_interpretation_layer_present('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451').
narrative_ontology:cs_kernel_id(archive_maintenance, sacrifice_commandment).
narrative_ontology:cs_reading_relation('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', study_as_performance, influences).
narrative_ontology:cs_reading_relation('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', performance_only, influences).
narrative_ontology:cs_axiom('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', foundational, knowledge_preservation_instrumentally_justified).
narrative_ontology:cs_axiom_status(knowledge_preservation_instrumentally_justified, holdable).
narrative_ontology:cs_axiom('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', foundational, commandment_reframes_into_archive_maintenance).
narrative_ontology:cs_axiom_status(commandment_reframes_into_archive_maintenance, holdable).
narrative_ontology:cs_reference_frame('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', commandment_deferred_mode).
narrative_ontology:cs_drift_state('a7e743c1-bcfc-4f5e-8c7c-6507ebd5e451', contemporary_archival_maturation, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(archive_maintenance, future_restoration_cohort).
narrative_ontology:constraint_victim(archive_maintenance, present_worshipping_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESENT WORSHIPPING COMMUNITY (SNARE) — Confined to a commandment whose present sacrificial value is suspended; resources (study labor, attention, ritual hours) extracted toward maintenance of knowledge they cannot deploy in living practice. No exit from the obligation; cannot reclaim the time. Pure extraction from the perspective of those whose worship is constrained by preparation that yields no immediate liturgical function.
constraint_indexing:constraint_classification(archive_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERPRETIVE ESTABLISHMENT (TANGLED ROPE) — Constrained by obligation to maintain the archive; also benefits from the institutional role of knowledge-steward and the authority that comes from being sole custodian of esoteric technical knowledge. Extraction (present effort for uncertain future return) mixed with coordination (preservation of a shared tradition). Constrained exit because abandoning the obligation would dissolve the institutional identity built around stewardship.
constraint_indexing:constraint_classification(archive_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FUTURE RESTORATION COHORT (ROPE) — Receives pure coordination benefit: the archive is freely available technical knowledge, enabling restoration without rediscovery cost. No extraction experienced because the future beneficiary bears no cost. From the perspective of those who will use the maintained archive, this is a pure coordination mechanism — others have solved the knowledge-preservation problem, leaving restoration available as a live option.
constraint_indexing:constraint_classification(archive_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents proposing that archive maintenance be treated as temporary infrastructure (knowledge backed up in multiple formats, teaching delegated to archives rather than ritual obligation) view the constraint as a coordination problem with a clear sunset. As institutional memory and written documentation improve, the live obligation to maintain technical knowledge through embodied practice diminishes. The constraint is architecture for a transitional period; new technology (printed books, digital archives) makes embodied maintenance obsolete.
constraint_indexing:constraint_classification(archive_maintenance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: INHERITED OBLIGATION STRUCTURE (PITON) — The technical maintenance regime persists through institutional inertia. Practitioners report that much of the elaborate study regimen is performative: formalized repetition of texts known to be written down elsewhere, ritual recitation rather than engaged knowledge transmission. Theater ratio (0.58) reflects that the obligation is maintained not primarily because it serves restoration function (that could be done by careful archival study) but because the obligation itself has become the norm. The mechanism has atrophied but persists.
constraint_indexing:constraint_classification(archive_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/eternal perspective, the obligation to preserve sacrificial knowledge is presented as an immutable commandment: the duty to maintain what has been commanded does not depend on whether the command is currently executable. This perspective naturalizes the constraint as part of the fixed order (mitzvot lo titgodedu - the commandments do not contradict each other; therefore knowledge preservation is required eternally). However, the structural data reveals this as a false summit: the beneficiary class (future restoration cohort) is identifiable and finite; the constraint serves contingent future states rather than eternal law.
constraint_indexing:constraint_classification(archive_maintenance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(archive_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(archive_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(archive_maintenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(archive_maintenance, TR),
    TR >= 0.70.

:- end_tests(archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The constraint imposes genuine resource costs (study time, cognitive labor, formalized practice regimen) on the present community without providing present sacrificial function. However, the extraction is not maximal (0.70+) because the future beneficiary is real and identifiable, the knowledge preservation function is genuine, and the present community's motivation is internally justified (service to commandment, preparation for restoration). The uncertainty about whether restoration will actually occur limits the constraint to moderate rather than high extraction. Suppression (0.52): Moderate-high. The present community cannot exit the obligation (it is binding halakhic law), cannot reallocate resources (the obligation is specific), and cannot reclaim the time. However, suppression is not severe (0.70+) because the obligation is transparent and internally justified — the constraint operates through understood duty rather than deception or coercion. Theater ratio (0.58): Moderate-high. The empirical observation is that much study is formalized repetition of texts preserved in written archives. The purpose of embodied practice (ensuring knowledge decay is prevented) could be served by archival study, yet the full ritual regimen persists. The performance element is in maintaining the obligation's form after its functional necessity has declined. The theater has increased over time as written documentation has improved, yet the obligation structure remains constant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp divergence between the beneficiary's experience (Rope — pure coordination benefit) and the victim's experience (Snare — pure extraction). The beneficiary (future restoration cohort) experiences zero extraction because they bear zero cost — the present community has solved their knowledge-preservation problem. The victim (present community) experiences extraction because they bear costs (study labor, attention, time) without present compensating benefit. The interpretive establishment experiences a mixed position (Tangled Rope) — they benefit from institutional authority (stewardship role) but are constrained by the obligation and face potential loss of authority if archival methods improve. The reform coalition sees a temporary problem with solvable architecture (Scaffold) — written archives + periodic study can maintain knowledge without embodied ritual practice; as archives mature, the constraint dissolves. The inherited obligation structure (Piton) is performative — the constraint persists through role identity and normative force rather than functional necessity. The eternal commandment view (Mountain) risks naturalizing what is actually a contingent obligation dependent on whether restoration will occur.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the structural relationship between each agent and the extraction flow. The present worshipping community is a victim (high d → high f(d) → high experienced extractiveness) because they bear costs without present benefit; they are also trapped (no exit options), maximizing their experienced load. The future restoration cohort is a beneficiary (low d → low/negative f(d) → low or negative experienced extractiveness) because they receive pure benefit without cost; they have arbitrage options (they can choose to use or not use the restored Temple). The interpretive establishment is a mixed beneficiary-victim (moderate d → moderate f(d)): they benefit from institutional authority but are constrained by the obligation and face declining relative authority as archival methods improve. The reform coalition has mobile exit options and organized power, so their experienced extraction is dampened by agency — they can envision alternative solutions. The eternal commandment perspective (Mountain) would impose d ≈ 0.5 (symmetric burden and benefit) if the constraint is actually immutable law, but the structural data (identifiable beneficiary, contingent future state, increasing theater ratio) indicates this is a false summit — the beneficiary is not symmetric with the community, the benefit is contingent, and the obligation is maintained partly through inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that archive maintenance has genuine tangled-rope structure: real coordination function (future knowledge preservation) mixed with real asymmetric extraction (present community bears costs for future uncertain benefit). The constraint is not pure extraction (snare) because the coordination value is genuine; it is not pure coordination (rope) because the extraction is real and asymmetric; it is not a temporary scaffold because the knowledge-preservation obligation is treated as indefinite rather than sunset-bound. The false summit (eternal commandment reading) is revealed by the increasing theater ratio and the identifiable beneficiary class — the constraint naturalizes what is actually a contingent obligation dependent on future restoration. The mandatrophy is resolved by accepting that archive maintenance is legitimate Tangled Rope: the present community's burden is justified by genuine future utility, but that justification depends on whether restoration will occur (omega variable: future_restoration_probability) and whether present sacrifice is ethically required for uncertain future benefit (omega variable: ethical_status_of_obligation_to_future).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_restoration_probability,
    'Will the Temple actually be restored, making the preserved knowledge functionally necessary? Or is archive maintenance proceeding under radical uncertainty about whether future restoration is possible or desirable?',
    'Historical analysis of explicit statements in halakhic sources about whether restoration is expected within a bounded timeframe vs. indefinite future. Theological surveys of whether mainstream interpretive authority treats restoration as metaphorical, conditional, or structurally inevitable.',
    'If restoration is expected (bounded timeframe < 200 years): archive maintenance is forward-looking coordination with clear beneficiary. If restoration is uncertain or indefinite: archive maintenance becomes pure obligation divorced from utility, shifting toward snare classification. If restoration is metaphorical/spiritual: knowledge preservation serves present symbolic function, not future practical function, fundamentally reframing the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_restoration_probability, conceptual, 'Whether Temple restoration is contingent or metaphysically necessary').

omega_variable(
    knowledge_decay_rate,
    'What is the actual empirical rate at which technical knowledge (stonework, sacrifice protocols, architectural detail) decays when not actively maintained through embodied practice vs. through archival preservation?',
    'Comparison of knowledge loss rates: traditions maintained through ritual embodied practice vs. written documentation + periodic study revival. Case studies from interrupted traditions (Jewish practice after Second Temple destruction, other traditions with interrupted technical knowledge).',
    'If embodied practice substantially preserves accuracy better than archival study (decay rate difference > 30%): extraction justified by genuine coordination value — the present community''s burden yields future accuracy. If archival study preserves equally well: present obligation is purely extractive — future cohort would not benefit from present sacrifice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_decay_rate, empirical, 'Comparative knowledge decay rates: embodied vs. archival preservation').

omega_variable(
    ethical_status_of_obligation_to_future,
    'Do present-generation communities have standing to impose resource burdens on themselves for future communities that have not consented to, and may reject, the framework imposing the obligation?',
    'Philosophical analysis of intergenerational obligation structures; comparison with other religious/cultural frameworks that impose present costs for future use. Analysis of whether future beneficiaries can opt out of the constraint framework (e.g., can a future generation reject Temple restoration, thereby invalidating the justification for present sacrifice?).',
    'If obligation is binding regardless of future consent: present sacrifice is justified. If obligation is contingent on future affirmation: archive maintenance is extractive from the present community because it imposes costs for a contingent future. If future generations have already implicitly consented through accepting the framework: obligation is legitimate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_status_of_obligation_to_future, preference, 'Ethical status of imposing present costs for contested future utility').

omega_variable(
    reading_identity_ambiguity,
    'Is archive maintenance a reading of the sacrifice commandment (mitzvah interpretively reframed as knowledge preservation), or is it a separate obligation whose identity is contested across the kernel''s sibling readings?',
    'Textual analysis of whether halakhic authority treats archive maintenance as continuous with sacrificial obligation or as a distinct but related duty. Whether the constraint is legitimated by extending the original commandment or by deriving a new one from its preconditions.',
    'If reading is continuous with the original commandment: archive maintenance inherits the commandment''s absolute justificatory force; present costs are subordinate to eternal obligation. If archive maintenance is derived obligation: its justification is contingent on the future state (restoration possibility); present costs require present benefit or clear future benefit to justify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether archive maintenance is a reading of sacrifice or a derived obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(archive_maintenance, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arch_tr_t0, archive_maintenance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(arch_tr_t20, archive_maintenance, theater_ratio, 20, 0.5).
narrative_ontology:measurement(arch_tr_t40, archive_maintenance, theater_ratio, 40, 0.58).
narrative_ontology:measurement(arch_tr_t60, archive_maintenance, theater_ratio, 60, 0.63).

% Extraction over time
narrative_ontology:measurement(arch_be_t0, archive_maintenance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arch_be_t20, archive_maintenance, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(arch_be_t40, archive_maintenance, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(arch_be_t60, archive_maintenance, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(archive_maintenance, enforcement_mechanism).
narrative_ontology:affects_constraint(archive_maintenance, study_as_performance).
narrative_ontology:affects_constraint(archive_maintenance, performance_only).

% DUAL FORMULATION NOTE:
% Archive maintenance is one reading of the sacrifice commandment kernel. The sibling reading study_as_performance treats knowledge preservation as inherently valuable (present-valued sacrifice) rather than instrumentally valuable (future-valued archive). The performance_only reading treats knowledge preservation as a separate obligation not grounded in the original commandment. Each reading has distinct ε values reflecting different structural claims about whether present communities bear costs (archive_maintenance: ε=0.48) or experience present benefit (study_as_performance) or are released from obligation (performance_only). The three stories form a constraint family linked by the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
