% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory Function — Mourning Practice Reading (Tisha B'Av Ritual Cycle)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   Tisha B'Av and the Jewish commemorative calendar impose ritual obligation
 *   on individual mourners to perform synchronized remembrance of catastrophe
 *   according to canonical halakhic form. This reading instantiates the
 *   constraint as pure mourning-practice and boundary-norm maintenance
 *   (D1/D4): ritual enactment IS group identity; the constraint's function is
 *   to maintain the boundary between canonical commemorative obligation and
 *   fragmented individual or secular alternative memory forms. This reading
 *   does NOT claim that the ritual transmits survival-competence, adaptive
 *   mechanisms, or historical understanding (that is the
 *   survival_competence_reading). It claims instead that the ritual's primary
 *   function is to fuse group-persistence with mandatory memorial performance
 *   — to make forgetting an act of group dissolution. The author's seat is
 *   outside the reading's authority structure (analytical observer),
 *   permitting clear declaration of both the coordination function and its
 *   extractive overlay.
 *
 * KEY AGENTS:
 *   - ritual_authority_clergy: Institutional agenda-setter; administers the memorial calendar and enforces canonical participation forms.
 *   - individual_griever_agency: Powerless target; fused with group identity such that personal grief tempo and alternative commemoration are suppressed.
 *   - alternative_commemoration_voices: Moderate-power excluded seats; secular, artistic, and political memory forms are delegitimized.
 *   - diaspora_participants: Organized seat; participate in costly ritual while gaining distributed group identity.
 *   - post_catastrophe_institutional_actors: Analytical observer seat; measure whether the constraint preserves or obscures historical understanding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory Function — Mourning Practice Reading (Tisha B'Av Ritual Cycle)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__mourning_practice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168').
narrative_ontology:cs_kernel_codification('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', fixed_text).
narrative_ontology:cs_authority_grounding('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', lineage).
narrative_ontology:cs_interpretation_layer_present('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168').
narrative_ontology:cs_reading_relation('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', catastrophe_memory_function__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', foundational, ritual_obligation_constitutive_of_group_persistence).
narrative_ontology:cs_axiom_status(ritual_obligation_constitutive_of_group_persistence, holdable).
narrative_ontology:cs_axiom_grounding('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', ritual_obligation_constitutive_of_group_persistence, deontological).
narrative_ontology:cs_axiom('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', secondary, commemorative_form_canonical_and_non_negotiable).
narrative_ontology:cs_axiom_status(commemorative_form_canonical_and_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', commemorative_form_canonical_and_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', talmudic_obligation_framework).
narrative_ontology:cs_drift_state('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', contemporary_post_enlightenment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a4173ccb-14f9-4ecf-8ec0-ed99ebc4d168', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, group_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, ritual_authority_clergy).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, individual_griever_agency).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, alternative_commemoration_voices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, diaspora_participants).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, diaspora_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, commemorative_obligation_essential_to_group_persistence).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, ritual_boundary_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the memorial calendar (Tisha B'Av fast, kinot recitation, textual liturgy). Administers the boundary between commemorative obligation and alternative memory forms. Justifies the constraint as essential to group survival through memory preservation. Career identity and institutional authority depend on the constraint's persistence and participation rates.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, ritual_authority_clergy, agenda_setter,
    institutional, generational, identity_locked, global).

% The abstract collective (Jewish people/diaspora Jewish identity) is sustained through repeated enactment of shared commemorative obligation. The constraint's operation vindicates the claim that ritual participation IS group persistence — that forgetting or abandoning the fast constitutes group fragmentation. This is a proposition the constraint vindicates, not an agent collecting rents.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, group_identity_continuity, beneficiary,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__mourning_practice_reading, group_identity_continuity).

% Individual mourners are obligated to perform the ritual cycle at prescribed times regardless of personal grief tempo or processing needs. Exit from the ritual means exit from group identity — the constraint fuses ritual performance with belongingness. Personal mourning practices that deviate from the canonical form (secular commemoration, private memory, individual rhythm) are suppressed or delegitimized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, individual_griever_agency, payer,
    powerless, biographical, identity_locked, local).

% Secular historians, artistic interpreters, political movements, and heterodox communities who would commemorate catastrophe through alternative forms (memorial scholarship, artistic reinterpretation, political resistance narratives) are structurally excluded from the canonical memorial frame. Their commemorative voice is treated as incoherent to group memory itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, alternative_commemoration_voices, excluded,
    moderate, biographical, constrained, regional).

% Diaspora communities participate in the memorial cycle across geographic fragmentation, which both creates a distributed group identity (the constraint coordinates memory at scale) and obligates costly ritual participation — fasting, textual study, gathering — regardless of individual location or assimilative pressure. The ritual's cost is precisely what makes it a boundary-marker.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, diaspora_participants, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__mourning_practice_reading, diaspora_participants, beneficiary).

% Academic historians, memory-studies scholars, and institutional remembrance bodies analyze whether Tisha B'Av ritual preserves historically accurate catastrophe-understanding or whether the constraint's ritual-closure mechanism prevents integration of post-catastrophe transformation, innovation, and survival mechanisms into the memorial frame.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, post_catastrophe_institutional_actors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__mourning_practice_reading, ritual_authority_clergy).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__mourning_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains distributed group identity across diaspora and generations through synchronized commemorative obligation. The constraint creates a shared temporal structure (the annual fast, the kinot cycle, the liturgical calendar) that binds individuals into a collective that reproduces itself through remembrance. The coordination solves the fragmentation problem: without the constraint, the diaspora has no synchronization point for identity-persistence.
% TRANSFER_FUNCTION: Transfers individual griever agency and alternative commemorative voice (silenced, delegitimized, suppressed) to the ritual authority's control of the memorial canon and the group identity's claim on all participants. The cost is the obligation to grieve according to prescribed form; the benefit is access to group-membership itself.
% ABSENT_VOICES: Secular commemoration traditions, artistic reinterpretations unbounded by halakha, political memory movements that read catastrophe as historical rupture rather than eternal return, and dissenting griever practices (expedited mourning, assimilative adaptation, individual-centered remembrance) are structurally excluded. They would argue for plural memorial forms, historical contingency, and griever autonomy — but the constraint's logic treats these as incoherent to 'real' group memory.
% DISAPPEARANCE_RATIONALE: If the mourning-obligation constraint vanished, diaspora group identity would fragment into plural commemoration practices within a month. The constraint is structurally necessary for group persistence — without synchronized ritual, the dispersed population loses the coordination mechanism that constitutes its collective. Individuals could still grieve, commemorate, and transmit history, but they would do so in multiple incompatible registers, none hegemonic.
% FOUNDING_PROBLEM: After catastrophic loss (Temple destruction, diaspora expulsion, historical rupture), the surviving community faces fragmentation: memory would scatter into individual narratives, grief would privatize, and the group would dissolve. Ritual obligation solves this by making group-persistence AND memory-preservation identical — one cannot belong without commemorating; one cannot commemorate outside the canonical form.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and contemporary institutional Judaism attest the founding problem is live — that without mandatory memorial obligation, group identity cannot persist across diaspora and generations. Academic historians and secular Jewish communities attest the founding problem is misdiagnosed — that group identity persists through many mechanisms beyond ritual, and that the constraint's framing of commemoration as obligatory reflects institutional power rather than empirical necessity for survival.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint does solve a genuine coordination problem — diaspora group identity without synchronized ritual would fragment — but the solution is asymmetric: the ritual authority controls the form and the individual pays the cost of fused identity-obligation. Suppression is substantial (0.62) because griever agency is suppressed (exit from ritual = exit from group) and alternative memory forms are delegitimized as incoherent to 'real' memory. Theater ratio is very high (0.71) because the ritual's function (boundary-maintenance through synchronized obligation) diverges increasingly from any empirical survival-competence claim as secular Jewish communities persist without ritual participation. The measurement series shows suppression stabilizing and theater ratio rising and plateauing — the constraint's coordination function remains constant while its performative component (the ratio of symbolic to functional activity) grows as modernity fragments belief in the founding problem. The time grid is aligned: every metric authored at every time point (0, 5, 10, 15, 20, 25, 30).
 *
 * PERSPECTIVAL GAP:
 *   From the ritual-authority seat, the constraint is essential coordination: without the obligation, group identity dissolves. From the individual-griever seat, the constraint is identity fusion enforced through suppression — grief becomes obligatory, individual processing tempo becomes incoherent, and exit is prohibited. From the post-catastrophe institutional seat, the constraint is increasingly theatrical: the founding problem (group fragmentation without ritual) is empirically false (diaspora communities persist through law, language, culture, shared history, institutional structure, not ritual), yet the constraint persists and intensifies. The engine computes per-seat divergence from this structural data; the interpretive readings do not converge.
 *
 * DIRECTIONALITY LOGIC:
 *   The ritual-authority seat benefits (controls the memorial canon, administers group membership, collects legitimacy) — directionality near beneficiary (d ≈ 0.15). The individual-griever seat is targeted (fused identity, suppressed agency, constrained exit) — directionality near target (d ≈ 0.85). The diaspora-participant seat is symmetric (genuine coordination benefit, real cost of ritual participation, constrained but not identity-locked exit) — directionality near middle (d ≈ 0.5). Alternative-commemoration voices are excluded, not parties — their exclusion is the enforcement object itself. This structure is encoded in the stakeholder situation descriptions; directionality derivation follows from beneficiary/victim data and exit options without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope, not snare, because the coordination function is genuine and non-empty: diaspora group identity does depend on synchronized ritual, and that solves a real coordination problem at scale. But it is tangled (not rope) because the solution is asymmetric extraction — individual griever agency is suppressed to maintain the boundary, alternative commemoration is delegitimized, and the ritual authority controls the form. The theater-ratio trajectory (rising to 0.71) and the stability of the founding-problem-status dispute signal a potential mandatrophy condition: the founding problem (group-fragmentation-without-ritual) is contested; modern diaspora communities persist through plural mechanisms; yet the constraint's enforcement and suppression do not diminish. A pure tangled_rope persists because both coordination and extraction are live — here, the extraction component persists even as the coordination justification weakens. This is consistent with tangled_rope; the engine will measure whether the ratio tips toward snare (if the coordination function collapses entirely).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    griever_agency_suppression_mechanism,
    'Is the suppression of individual griever agency structural (institutional enforcement of the memorial form) or internalized (the individual has fused their identity with the obligation such that deviation feels like group dissolution)?',
    'Post-exit trajectory: do individuals who leave the ritual-obligation community experience lasting suppression (internalized) or do they rapidly develop alternative commemoration practices (structural suppression, removed)? Longitudinal study of non-observant Jewish identity-maintenance.',
    'If structural, the constraint''s suppression is contingent on ritual authority enforcement and could be remedied by removing enforcement. If internalized, the suppression persists after exit — the individual carries the obligation with them — and the constraint''s persistence depends on identity fusion, not institutional power alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(griever_agency_suppression_mechanism, empirical, 'Whether suppression of griever agency is enforcement-dependent or identity-fused.').

omega_variable(
    coordination_necessity_empirical,
    'Is synchronized ritual obligation empirically necessary for diaspora group identity persistence, or do modern diaspora communities sustain identity through law, language, culture, shared history, and institutional structure independent of ritual?',
    'Comparative study of Jewish identity-retention rates across observance levels; analysis of secular Jewish institutional persistence (organizations, advocacy, memory projects, cultural transmission) independent of ritual participation.',
    'If ritual obligation is necessary for identity-persistence, the founding-problem justification is live and the constraint''s coordination function is real. If identity persists without ritual, the constraint''s founding-problem framing is contested — the ritual persists as boundary-enforcement rather than coordination necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_empirical, empirical, 'Whether diaspora group identity depends structurally on synchronized ritual obligation.').

omega_variable(
    alternative_commemoration_delegitimation,
    'Are alternative commemorative forms (secular, artistic, political memory) treated as genuine alternative readings of the founding problem, or are they delegitimized as incoherent to group identity itself?',
    'Institutional analysis: do rabbinic authorities engage alternative forms as coherent alternatives, or do they treat them as group-fragmentation / apostasy? Do secular Jewish communities maintain explicit identity despite rejecting the constraint, or do they experience identity-erosion pressure?',
    'If delegitimized, the suppression extends to the boundary between canonical and alternative commemoration — the constraint''s function includes delegitimizing plural memory forms. If engaged as alternatives, the constraint''s function is narrower — coordination around a specific form rather than suppression of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_commemoration_delegitimation, conceptual, 'Whether alternative commemoration is suppressed as incoherent or engaged as legitimate difference.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the mourning_practice_reading foreclose the survival_competence_reading (both cannot coexist in the same framework), or do they coexist as held by different institutional authorities?',
    'Textual and institutional analysis: are survival_competence elements (adaptive mechanisms, decentralized continuity, historical transformation) present in the mourning-practice reading''s canonical sources, or has that reading systematically excluded them? Do contemporary rabbinic sources acknowledge survival-competence function or treat it as outside the ritual''s scope?',
    'True foreclosure would mean one reading''s core premise logically contradicts the other. Coexistence would mean different authorities (rabbinic vs. historical vs. secular) hold different readings as live positions without logical incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'The structural relationship between mourning_practice_reading and survival_competence_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 10, 0.66).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 15, 0.69).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 25, 0.71).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 30, 0.71).
narrative_ontology:measurement_basis(cata_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(cata_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(cata_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__mourning_practice_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel has three distinct readings, each instantiating different structural claims about what the ritual preserves. mourning_practice_reading (this constraint) claims the ritual preserves group-identity through mandatory commemorative obligation (D1/D4 only). survival_competence_reading claims the ritual transmits adaptive capacity for institutional continuity (D5 only). hybrid_transformation_reading claims both functions coexist. These are three constraints with different epsilon values, different victim/beneficiary sets, and different suppression mechanisms — they share a kernel (the contested role of ritual in catastrophe memory) but diverge in their reading of what that ritual does. Each reading instantiates a different constraint structure; the network relationships model the logical and institutional dependencies between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__mourning_practice_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
