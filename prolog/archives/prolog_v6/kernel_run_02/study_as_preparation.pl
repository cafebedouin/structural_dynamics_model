% ============================================================================
% CONSTRAINT STORY: study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_preparation, []).

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
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_preparation
 *   human_readable: Sacrificial Law Study as Messianic Preparation (Kodashim Obligation)
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   In rabbinic Judaism after the destruction of the Second Temple (70 CE),
 *   the obligation to study and teach the laws of Temple sacrifice (Kodashim
 *   — the Mishnaic order dealing with sanctified offerings) persisted despite
 *   the commandment's non-performability. The constraint structures an
 *   obligation whose practical performance is structurally suspended, but
 *   whose study is mandatory. This creates a unique structural situation: the
 *   law remains binding, but the condition for its performance cannot be
 *   satisfied in the present. The constraint is resolved by treating study as
 *   instrumental preparation for a future messianic restoration when the
 *   Temple will be rebuilt and sacrifice can resume. This reading decomposes
 *   'the Kodashim obligation' into a specific interpretation: study is
 *   preparation, not intrinsic value or mere archive. The burden falls on the
 *   current generation (who maintain the knowledge without ever performing
 *   it), with the benefit deferred to the future priestly generation (who
 *   will perform the sacrifices when restoration occurs). The constraint
 *   exhibits characteristics of a temporary coordination mechanism
 *   (scaffold): it has a sunset clause (when the Temple is rebuilt, study
 *   becomes practice), it serves coordination by preserving technical
 *   knowledge, and suppression is moderate (significant barriers, but
 *   organized communities can sustain study).
 *
 * KEY AGENTS:
 *   - Studying Community: Moderate power agents (primary victims) — bear the burden of maintaining Kodashim knowledge for a non-performable commandment; resources diverted from performable obligations
 *   - Textual Preservation Authority (Rabbinic Academy): Institutional beneficiaries (arbitrage) — maintain manuscripts, coordinate study programs, legitimacy depends on knowledge continuity
 *   - Obligated Individual (Student): Powerless agents (trapped/identity_locked) — constitutively bound by the obligation; bear biographical extraction through deferred labor
 *   - Performative Ritual Complex: Institutional executor (arbitrage) — maintains study academies through institutional momentum; primary function (actual sacrifice) is structurally absent
 *   - Messianic Future: Beneficiary (non-agent) — the future priestly generation when Temple is restored; receives the benefit of preserved technical knowledge
 *   - Current Deferred Repair: Victim (non-agent) — the present generation bears the cost of study labor in service to a future they may not experience; the cosmic repair they defer is a structural debt
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_preparation, 0.28).
domain_priors:suppression_score(study_as_preparation, 0.45).
domain_priors:theater_ratio(study_as_preparation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_preparation, extractiveness, 0.28).
narrative_ontology:constraint_metric(study_as_preparation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(study_as_preparation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_preparation, scaffold).
narrative_ontology:human_readable(study_as_preparation, "Sacrificial Law Study as Messianic Preparation (Kodashim Obligation)").
narrative_ontology:topic_domain(study_as_preparation, "religious_studies/jewish_law/textual_preservation").

domain_priors:requires_active_enforcement(study_as_preparation).
narrative_ontology:has_sunset_clause(study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_preparation, '20c85c7f-7ae7-4b8d-8802-54404f38d3b8').
narrative_ontology:cs_created_at('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', '').
narrative_ontology:cs_kernel_codification('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', fixed_text).
narrative_ontology:cs_authority_grounding('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', lineage).
narrative_ontology:cs_interpretation_layer_present('20c85c7f-7ae7-4b8d-8802-54404f38d3b8').
narrative_ontology:cs_kernel_id(study_as_preparation, kodashim_obligation).
narrative_ontology:cs_reading_relation('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', study_as_archive, coexists_with).
narrative_ontology:cs_axiom('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', foundational, messianic_restoration_required).
narrative_ontology:cs_axiom_status(messianic_restoration_required, holdable).
narrative_ontology:cs_axiom_grounding('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', messianic_restoration_required, deontological).
narrative_ontology:cs_axiom('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', foundational, preparation_instrumentality).
narrative_ontology:cs_axiom_status(preparation_instrumentality, holdable).
narrative_ontology:cs_axiom_grounding('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', preparation_instrumentality, theological).
narrative_ontology:cs_reference_frame('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', temple_restoration_framework).
narrative_ontology:cs_drift_state('20c85c7f-7ae7-4b8d-8802-54404f38d3b8', contemporary_exile_indefinite, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_preparation, messianic_redemption_process).
narrative_ontology:constraint_beneficiary(study_as_preparation, future_priestly_generation).
narrative_ontology:constraint_victim(study_as_preparation, current_generation_deferred_repair).
narrative_ontology:constraint_victim(study_as_preparation, textual_knowledge_maintenance_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDYING COMMUNITY (SCAFFOLD) — Communities engaged in Kodashim study bear the burden of maintaining technical knowledge for a commandment that cannot be performed. The constraint is temporary (sunset): study is instrumental preparation for messianic restoration when Temple sacrifice resumes. Moderate power, generational horizon — agents have agency (can organize study) but face material constraints (resources diverted from performable commandments). Extraction is present but declining as open-source textual infrastructure (digital editions, online learning) reduces transaction costs.
constraint_indexing:constraint_classification(study_as_preparation, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: TEXTUAL PRESERVATION AUTHORITY (ROPE) — Rabbinic institutions that maintain Kodashim manuscripts and coordinate study programs experience the constraint as pure coordination with minimal extraction. The beneficiary is the distributed knowledge base itself. Study serves coordination function: preventing textual corruption, standardizing interpretive methods, training scholarly lineages. Authority has arbitrage options (can redirect study resources to other commandments). Low extraction because the coordination benefit to the academy's legitimacy equals the extraction cost.
constraint_indexing:constraint_classification(study_as_preparation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: OBLIGATED INDIVIDUAL (TANGLED ROPE) — A student or scholar bound by the obligation to study non-performable Kodashim law experiences mixed coordination and extraction. Coordination function: learning the law is inherently valuable, preserves Jewish identity and textual lineage. Extraction: resources (time, intellectual effort, opportunity cost) are directed to preparing for a future event that may never materialize. Trapped in biographical horizon — cannot exit the obligation without abandoning religious identity. Suppression is significant (no alternative interpretations permit full exemption) but not total (heterodox communities or modernist readings offer partial releases).
constraint_indexing:constraint_classification(study_as_preparation, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PERFORMATIVE RITUAL COMPLEX (PITON) — Study of Kodashim functions as a substitute performance, maintaining ritual theater in the absence of actual sacrifice. The functional content (learning technical law) persists, but the constraint increasingly operates through institutional inertia: study academies continue because predecessors founded them, not because the immediate function is critical. Theater ratio elevated (0.58) because study stands in for sacrifice, creating symbolic performance rather than material coordination. Piton classification reflects degraded primary function (cannot actually restore Temple sacrifice through study) maintained by institutional momentum.
constraint_indexing:constraint_classification(study_as_preparation, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: IDENTITY-LOCKED PRACTITIONER (SNARE) — A scholar whose professional identity and self-concept are constituted entirely through Kodashim expertise experiences the constraint as pure extraction. Structurally mobile (could exit the obligation by leaving the community or reinterpreting the law), but identity-fused with the role of keeper of sacrificial knowledge. Perceives study as endless deferred labor: preparing for messianic restoration that never arrives, preventing the knowledge from becoming obsolete in the contingent present. The binding mechanism is cognitive (their identity is 'the one who preserves this knowledge'), not material. If identity frame broke, the agent could perceive mobility and reclassify to tangled_rope or rope.
constraint_indexing:constraint_classification(study_as_preparation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DIVINE COMMAND VIEW (MOUNTAIN) — From a civilizational perspective, the obligation to study Kodashim is a fixed divine command that remains in force regardless of material performance possibility. This perspective treats the constraint as unchangeable natural law: God commanded the study of sacrificial law, and the obligation persists immutably, immune to historical contingency. However, the analytical observer risks false summit classification: the claim that study obligation is 'natural law' naturalizes what is actually a contested rabbinic interpretation (see cs_structure.reading_relations: this reading coexists with hermeneutic alternatives). The engine may reclassify this perspective via FSM.
constraint_indexing:constraint_classification(study_as_preparation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_preparation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(study_as_preparation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(study_as_preparation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(study_as_preparation, TR),
    TR >= 0.70.

:- end_tests(study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low. The constraint operates as instrumental preparation, not extraction per se. The current generation studies to preserve knowledge for future use. However, extractiveness is non-zero because: (1) study resources are diverted from performable commandments that offer present benefit; (2) the future benefit is contingent on messianic restoration, which is temporally uncertain; (3) the studying generation may never experience the benefit. The extractiveness rises over time (0.18→0.28) as historical experience lengthens without Temple restoration, converting 'preparation' into increasingly speculative maintenance. Suppression (0.45): Moderate. Barriers to exit include religious obligation, community identity, institutional expectations, and absence of competing interpretations within Orthodox framework. But suppression is not total: heterodox movements and modern reinterpretations permit partial or full exemption. Theater ratio (0.58): Moderate-high. Study of non-performable law increasingly performs a ritual function (maintaining connection to ancestral practice, validating theological commitment to restoration) rather than practical preparation. The theater has risen over the interval as the performability timeline receded; study must justify itself symbolically rather than instrumentally. Claimed type (scaffold): Fits because (a) has sunset clause (when Temple rebuilds, study becomes practice); (b) coordination function (preserves technical knowledge); (c) suppression declines over interval as digital infrastructure reduces transaction costs; (d) beneficiaries exist (future priestly generation, messianic order).
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal orthogonal evaluations of the same obligation. The textual authority sees pure coordination (Rope) — study maintains knowledge, prevents corruption, legitimates the academy. The studying community sees temporary coordination-extraction (Scaffold) — burdensome but instrumental, with a sunset when restoration arrives. The obligated individual sees mixed coordination-extraction (Tangled Rope) — the law is meaningful but deferred labor; they cannot exit. The identity-locked scholar sees pure extraction (Snare) — endless labor for a future that never arrives, with identity preventing exit. The institutional ritual complex sees degraded function (Piton) — study persists through momentum, not present utility; theater has risen. The analytical observer risks seeing immutable law (Mountain) — 'God commanded it, therefore it persists' — but this naturalizes a contested reading choice. The perspectival gaps are not measurement disagreements; they are structural differences in how agents experience the same constraint. A study coordinator experiences it as manageable scaffold; an identity-locked specialist experiences it as snare; an analytical observer risks seeing mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is determined by structural position relative to the obligation. Beneficiaries (future priestly generation, messianic order) have d≈0.1 (full beneficiary, offset by temporal uncertainty). The studying community has d≈0.55 (victim + constrained exit, but with moderate agency through community organization). The obligated individual has d≈0.75 (victim + trapped or identity_locked exit). The analytical observer has d≈0.70 (observer with risk of false summit). No directionality overrides are required; the derivation chain from beneficiary/victim declarations produces accurate d values. The studying community's perspective shows the chi formula's operation: ε=0.28, σ(global)=1.2, f(d)≈0.72 (constrained victim), yielding χ≈0.24 — moderate effective extraction despite low base extraction. Identity-locked perspective produces f(d)≈1.28 (high), yielding χ≈0.36 even with low ε — cognitive capture amplifies experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that the six types reflect legitimate structural differences in how agents experience the obligation, not measurement failures. The mandate is 'study Kodashim' — a mitzvah (commandment). The potential threat: this could classify as snare (pure extraction disguised as obligation), or rope (pure coordination), or mountain (unchangeable divine law). The resolution is perspectival: beneficiaries (future priestly generation) see rope (coordination); the studying community sees scaffold (temporary coordination with sunset); trapped individuals see snare (extraction disguised as obligation); the institution sees piton (degraded function maintained by momentum). No single type is 'correct' — the presheaf over the observation sites captures the full structure. The analytical observer risks false summit (seeing immutable natural law where rabbinic interpretation choices exist), which the engine's FSM signature should detect via the beneficiary declarations and the mandate-independence question ('Is this obligation grounded in divine command or in contingent institutional choice?').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_contingency,
    'When does instrumental preparation become indefinite theatrical maintenance? At what point does deferred performance become structurally impossible rather than temporarily suspended?',
    'Historical analysis of rabbinic discourse on messianic expectations; tracking of references to ''when the Temple is rebuilt'' vs recognition of indefinite exile; identification of inflection points where study obligation reframes from ''preparation'' to ''preservation''',
    'If timeline is bounded (e.g., ''preparation lasts 2000 years, then the commitment is exhausted''): constraint transitions from scaffold to piton or snare at the boundary. If timeline is unbounded: scaffold classification is aspirational rather than structural; constraint is functionally snare or piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timeline_contingency, conceptual, 'Whether instrumental preparation has a structural endpoint or becomes indefinite theater').

omega_variable(
    textual_knowledge_transfer_degradation,
    'Does knowledge transmission for non-performable law experience degradation or corruption over generations compared to knowledge transmission for performable commandments?',
    'Comparative textual analysis of manuscript variants, halakhic disputes, and pedagogical methods for Kodashim vs other rabbinic disciplines; identification of corruption rates, dropout points, or loss of technical precision',
    'If degradation is severe: study fails at coordination function, constraint becomes pure extraction (snare) masquerading as preservation. If degradation is minimal: study succeeds at coordination function, constraint is genuine rope/scaffold. Moderate degradation confirms tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_knowledge_transfer_degradation, empirical, 'Whether knowledge transmission for non-performable law degrades faster than performable law').

omega_variable(
    identity_fusion_vs_structural_choice,
    'Is the identity_locked exit option a genuine cognitive trap or a rhetorical frame the practitioner uses to justify continued study? Can identity-locked agents actually exit if reframing support is provided?',
    'Ethnographic study of practitioners who have exited Kodashim study; interview analysis of disidentification narratives; identification of whether exit required identity rupture or reframing only',
    'If identity fusion is genuine: identity_locked perspective''s snare classification is correct; constraint extracts through cognitive capture. If fusion is rhetorical: agents should be reclassified to constrained/mobile; constraint is weaker snare or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_vs_structural_choice, empirical, 'Whether identity fusion in Kodashim scholars is genuine cognitive trap or rhetorical justification').

omega_variable(
    reading_kernel_ambiguity,
    'Is this the reading that instantiates ''study as preparation for messianic restoration,'' or is it actually ''study as preservation of knowledge regardless of performability''? The axiom ''messianic_restoration_required'' presupposes the constraint is instrumental; but what if study is intrinsically valuable independent of Temple rebuilding?',
    'Textual analysis of rabbinic sources for explicit statements about study instrumentality vs intrinsic value; identification of which reading the Talmud and major rishonim actually endorse; determination of whether ''preparation'' is primary or secondary framing',
    'If study is intrinsically valuable: this reading''s scaffold classification is incorrect; should reclassify as rope. If study is genuinely instrumental but to a future state that is materially impossible: constraint is snare or piton masquerading as scaffold. The reading_relations field presupposes ''study_as_preparation'' and ''study_as_archive'' are distinct readings; if the boundary is actual, this omega confirms the decomposition; if the boundary is rhetorical, the omega identifies reading misalignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether this reading correctly identifies study as instrumental preparation vs misclassifying intrinsic knowledge preservation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_preparation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stud_tr_t0, study_as_preparation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stud_tr_t10, study_as_preparation, theater_ratio, 10, 0.45).
narrative_ontology:measurement(stud_tr_t20, study_as_preparation, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(stud_be_t0, study_as_preparation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(stud_be_t10, study_as_preparation, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(stud_be_t20, study_as_preparation, base_extractiveness, 20, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_preparation, identity_coordination).
narrative_ontology:affects_constraint(study_as_preparation, study_as_performance).
narrative_ontology:affects_constraint(study_as_preparation, study_as_archive).
narrative_ontology:affects_constraint(study_as_preparation, temple_restoration_imperative).

% DUAL FORMULATION NOTE:
% The Kodashim obligation decomposes into three structurally distinct constraints with different ε values: study_as_preparation (this file, ε=0.28, scaffold with sunset), study_as_performance (ε=0.15, rope — study is intrinsically commandment-fulfilling), study_as_archive (ε=0.35, tangled_rope — study maintains knowledge but divorced from performability). Each reading instantiates a different beneficiary/victim structure and temporal horizon. They are not three measurements of one constraint; they are three constraints sharing the same rabbinic kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
