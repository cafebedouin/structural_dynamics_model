% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinbutsu Partition: Ontological Separation of Shinto and Buddhism
 *   domain: religious/philosophical/social
 *
 * SUMMARY:
 *   Under the partition reading, Shinto and Buddhism are understood as
 *   occupying separate ontological and functional domains without requiring
 *   integration. Shinto addresses life-cycle events, community well-being,
 *   indigenous identity, and the kami (divine beings immanent in natural and
 *   social orders). Buddhism addresses death, ancestors, metaphysical
 *   explanation, salvation, and ultimate reality. Japanese practitioners and
 *   institutions maintained this functional separation for over 1,400 years
 *   without resolving doctrinal contradictions—not because the contradictions
 *   don't exist, but because the partition framework declares them
 *   irrelevant. Each tradition remains autonomous in its domain;
 *   practitioners move between them based on context, not unified belief.
 *
 * KEY AGENTS:
 *   - Buddhist institutional clergy: maintain doctrinal and monastic autonomy, benefit from the partition by avoiding forced integration with Shinto cosmology
 *   - Shinto shrine operators: preserve shrine independence and indigenous ritual authority over life-cycle events
 *   - Lay practitioners: achieve comprehensive ritual coverage by attending both traditions without doctrinal burden
 *   - Honji-suijaku theorists (excluded): argue for explicit metaphysical integration, incompatible with the partition reading
 *   - State coherence-seekers (excluded): periodically demand doctrinal harmony or clear hierarchy, resisted by the partition framework
 *   - Philosophical observers: external analysts measuring whether the partition is stable or a cover for incoherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.28).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.19).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinbutsu Partition: Ontological Separation of Shinto and Buddhism").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious/philosophical/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '3207843e-2aa9-4414-8c4c-94817292b1b0').
narrative_ontology:cs_kernel_codification('3207843e-2aa9-4414-8c4c-94817292b1b0', distributed).
narrative_ontology:cs_authority_grounding('3207843e-2aa9-4414-8c4c-94817292b1b0', distributed).
narrative_ontology:cs_reading_relation('3207843e-2aa9-4414-8c4c-94817292b1b0', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('3207843e-2aa9-4414-8c4c-94817292b1b0', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('3207843e-2aa9-4414-8c4c-94817292b1b0', foundational, domain_separation_principle).
narrative_ontology:cs_axiom_status(domain_separation_principle, holdable).
narrative_ontology:cs_axiom_grounding('3207843e-2aa9-4414-8c4c-94817292b1b0', domain_separation_principle, conventional).
narrative_ontology:cs_axiom('3207843e-2aa9-4414-8c4c-94817292b1b0', foundational, functional_pluralism_doctrine).
narrative_ontology:cs_axiom_status(functional_pluralism_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('3207843e-2aa9-4414-8c4c-94817292b1b0', functional_pluralism_doctrine, instrumental).
narrative_ontology:cs_reference_frame('3207843e-2aa9-4414-8c4c-94817292b1b0', separate_institutional_autonomy).
narrative_ontology:cs_drift_state('3207843e-2aa9-4414-8c4c-94817292b1b0', contemporary_scholarly_reassessment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3207843e-2aa9-4414-8c4c-94817292b1b0', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_institutional_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, functional_pluralism_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, domain_separation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains a coherent doctrinal and institutional identity separate from Shinto practice. They benefit from the partition by avoiding the ontological burden of reconciling Buddhist metaphysics with Shinto deities; practitioners can attend both Buddhist temples and Shinto shrines without forcing integration of the two systems. This permits Buddhism to remain organizationally autonomous while Japanese practitioners maintain their entire ritual repertoire.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_institutional_clergy, beneficiary,
    institutional, generational, arbitrage, national).

% Conduct life-cycle rituals (births, coming-of-age, marriages, ancestor rites) without theological subordination to Buddhist doctrine. The partition preserves shrine autonomy and permits practitioners to treat Shinto as the indigenous, practical framework for community and family matters. Shrines avoid the expense and doctrinal labor of syncretism while retaining social centrality.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_shrine_operators, beneficiary,
    organized, generational, constrained, national).

% Can practice both Buddhism and Shinto without resolving them into a single framework. Birth through Shinto, death through Buddhism, and daily devotions distributed across both systems remain coherent in practice, even if unexplained in doctrine. The partition permits functional autonomy: practitioners choose which tradition's framework fits each context.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Would argue for explicit metaphysical integration: that kami are manifestations (suijaku) of buddhist principles (honji) or vice versa. They are excluded from the partition's framing — their integrationist project is incompatible with the reading's core claim that Shinto and Buddhism operate in separate domains without ontological connection. Yet they persist as a real alternative reading, especially in elite scholarly circles.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, honji_suijaku_theorists, excluded,
    moderate, biographical, constrained, national).

% State authorities, imperial courts, and institutional reformers who periodically demand that contradictions be resolved: coherent state religion, unified doctrine, or explicit doctrinal hierarchy. They are excluded because the partition explicitly denies the need for such integration and treats incoherence as tolerable or even functional.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, institutional_coherence_advocates, excluded,
    powerful, generational, constrained, national).

% Religious scholars, historians, and philosophers who examine whether the partition claim is historically accurate, whether it masks deeper integration, or whether it represents institutionalized incoherence. They produce the evidence for or against the reading's coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, philosophical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits dual religious participation (Buddhist ritual specialists, Shinto shrine communities, lay practitioners) without forcing doctrinal reconciliation. Each tradition maintains its institutional and liturgical autonomy while Japanese society achieves comprehensive ritual coverage: Buddhism handles death, ancestors, and universal salvation; Shinto handles birth, communal well-being, and indigenous identity. Practitioners achieve a complete religious life by combining both rather than choosing between them.
% TRANSFER_FUNCTION: Transfers institutional autonomy and doctrinal freedom from practitioners to the priesthoods. Practitioners accept the unresolved contradiction between the two systems (or remain unconcerned with doctrinal coherence) and in exchange receive complete ritual services without forcing a single priesthood to cover all cases. Shrine priests and Buddhist clergy each maintain their specialist roles and authority within their domains.
% ABSENT_VOICES: Honji-suijaku metaphysicians and syncretist philosophers who would argue for explicit integration are structurally excluded from the partition's framing. State coherence-seekers and theological rationalists who demand doctrinal harmony are also excluded — the reading actively rejects their demand as unnecessary. Practitioners who would prefer explicit doctrinal guidance rather than functional ambiguity are not consulted; the arrangement privileges institutional and clerical autonomy over lay philosophical satisfaction.
% DISAPPEARANCE_RATIONALE: If the partition commitment vanished—if the assumption that Shinto and Buddhism occupy separate domains collapsed—Japanese practitioners and institutions would face urgent pressure to resolve the integration question. Either doctrinal integration would be forced (honji-suijaku reinstated as mandatory framework), or one tradition would be subordinated to the other, or the contradictions would surface as institutional chaos requiring state intervention. The entire lifecycle-division-of-labor system would require renegotiation. Practitioners' ability to maintain dual practice without doctrinal friction depends on the partition being held in place.
% FOUNDING_PROBLEM: Early Japanese religious practice combined indigenous shamanic and agricultural rites (Shinto) with imported Buddhist doctrine and monastic institutions (6th century onward). The two systems approached death, ritual authority, cosmology, and practice-goals differently. Rather than force integration (which would require subordinating one system or inventing metaphysical bridges), Japanese institutional practice evolved a functional separation: Shinto handles community, lifecycle, and indigenous identity; Buddhism handles metaphysical explanation, salvation, and ancestor intercession. The partition permitted both to coexist by treating them as addressing non-overlapping domains.
% FOUNDING_PROBLEM_CORROBORATION: The Buddhist institutional hierarchy and Shinto shrine operators attest the partition works and is worth preserving. Practitioners' behavior (dual attendance without reported confusion) provides evidence for functional coexistence. However, historical scholars outside both institutions (Grapard, Kuroda, Rambelli) and contemporary observers dispute whether the partition is a real ontological commitment or a convenient fiction masking deeper integration or systematic incoherence. Syncretist philosophers and honji-suijaku theorists (ancient and modern) argue the partition is not stable and that genuine unity or explicitly-acknowledged incoherence is the truer description.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28) because the partition does involve asymmetric benefits: Buddhist clergy and Shinto shrine operators gain institutional autonomy and protected specialist roles, while lay practitioners gain convenience but lose doctrinal clarity and must accept unresolved contradictions. However, extraction is constrained by the fact that no single party captures all the gains—both priesthoods benefit, practitioners retain mobility, and the system provides genuine value (complete ritual coverage). Suppression is low (0.19) because the partition requires minimal enforcement; practitioners naturally prefer attending both traditions over choosing, and institutional clergy are willing to accept the arrangement. Theater is moderate (0.42) because while the partition solves a real coordination problem, significant performative effort goes into NOT discussing the underlying contradictions—scholars and theologians expend energy explaining how the partition 'works' despite being logically unstable. The measurement series show theater-ratio rising through the medieval period (0.25 → 0.43) as institutional consolidation increased rhetorical defense of the partition, then stabilizing as the framework became naturalized. Extractiveness and suppression also rise slightly through the early medieval period as institutional extraction solidified, then plateau as the equilibrium stabilized.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-clergy position (Buddhist and Shinto authorities), the partition is genuine coordination: it permits each tradition to maintain doctrinal and organizational integrity while reaching the entire population. From the lay-practitioner position, it is a functional convenience that works in practice without needing theoretical justification. From the position of honji-suijaku theorists and philosophical coherence-seekers, the partition is either an inadequate mask over deeper integration (the syncretic reading) or an institutionalized evasion of genuine incoherence (the incoherence reading). The engine computes per-seat classifications from the structural data; the divergence in how different seats experience this constraint is the central analytical result.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist clergy and Shinto shrine operators occupy the structural-beneficiary position (d ≈ 0.25–0.35): they gain autonomy, institutional protection, specialized roles, and (for major temples and shrines) economic and political power from the partition framework. Lay practitioners sit near symmetric (d ≈ 0.50): they gain genuine coordination benefit (complete ritual services without forced choice) but also bear a subtle cost (doctrinal incoherence, inability to appeal to a unified framework for resolving conflicts between traditions). Honji-suijaku theorists and state coherence-seekers are structurally trapped targets (d ≈ 0.75–0.90) insofar as the partition actively forecloses their preferred resolutions and requires them to accept an arrangement they regard as intellectually unstable. The partition's stability depends on practitioners not asking questions that would force integration or explicit acknowledgment of incoherence—a form of low-intensity suppression of alternative framings.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading avoids misclassifying the constraint. If it were coded as a pure rope (genuine coordination), the high theater-ratio (0.42) and the presence of significant excluded voices would be unexplained. If it were coded as a snare (pure extraction), the real benefits to practitioners (complete ritual coverage, individual autonomy) and the lack of institutional coercion would make the classification false. Coding it as rope with acknowledged theater accounts for both: the arrangement does solve a genuine coordination problem (ritual completeness without forced choice), but a substantial portion of the system's operation goes into rhetorical defense of the partition rather than functional necessity, and the partition actively suppresses alternative readings (syncretic and incoherence) that some informed observers consider more accurate. The mandatrophy question is whether the partition's founding problem (managing two incompatible systems without forced integration) is still live or has been superseded. The reading's answer is 'contested'—practitioners live the partition daily, but scholars increasingly view it as historically contingent or intellectually unstable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_syncretic_depth,
    'Is the partition a genuine ontological commitment, or does it mask deeper honji-suijaku integration that practitioners implicitly rely on without articulating?',
    'Ethnographic and textual analysis of lay understanding: do practitioners experience Shinto and Buddhism as separate systems, or as aspects of a unified cosmology they cannot fully articulate? Analysis of ritual correlations and overlaps that might imply hidden integration. Theological examination of whether practitioners'' actual doctrinal presuppositions (even if unstated) cohere with syncretist metaphysics.',
    'If the partition is real (practitioners genuinely experience separation), the constraint is a rope. If it masks implicit syncretic integration, the constraint is closer to the syncretic reading (different ε, different beneficiary structure). If it masks systematic incoherence that practitioners evade without resolving, the constraint shifts toward the incoherence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_syncretic_depth, empirical, 'Whether the partition represents genuine ontological separation or masked deeper integration/incoherence.').

omega_variable(
    institutional_extraction_vs_genuine_benefit,
    'To what degree does the partition benefit lay practitioners (complete ritual coverage without forced choice) versus extracting autonomy/authority from lay practitioners to benefit priesthoods?',
    'Historical and ethnographic evidence of lay agency: do practitioners request alternatives to the partition (unified doctrine, single priesthood, explicit incoherence acknowledged)? Do they experience the arrangement as constraining or liberating? Analysis of whether practitioners could achieve the same ritual coverage through other means (e.g., heterodox sects, folk practice, voluntary integration) if the partition were not enforced.',
    'High practitioner agency and preference for the partition would support the rope classification. Evidence that practitioners are coerced or manipulated into accepting unresolved contradictions would shift toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_vs_genuine_benefit, empirical, 'Whether the partition genuinely serves practitioners or extracts from them to benefit priesthoods.').

omega_variable(
    theater_mechanism_identity,
    'What explains the high theater ratio (0.42)? Is it performative maintenance of an unstable fiction, or legitimate pedagogical work necessary to transmit a genuinely-held but conceptually-complex partition framework?',
    'Textual analysis of clerical literature defending the partition: does it argue that the partition is functional and appropriate, or that contradictions must be overlooked/ignored? Comparison with theological literature from syncretic periods that did attempt integration: does the rhetoric of integration-seekers differ in kind from the rhetoric of partition defenders, or only in direction? Ethnographic observation of whether clerical training involves genuine philosophical argument for partition or routine socialization into not asking integration questions.',
    'If theater represents performative defense of an untenable position, the constraint approaches piton or snare territory. If theater represents legitimate pedagogical transmission of a held position, the constraint remains rope. The distinction affects whether the constraint''s long-term stability is structural or depends on active institutional suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_mechanism_identity, conceptual, 'Whether theater represents legitimate pedagogy or performative defense of an indefensible position.').

omega_variable(
    committer_frame_reading_stability,
    'Is the partition reading self-standing as an ontological claim, or does it depend on the incoherence reading as a background assumption (i.e., ''the partition works because we don''t ask whether it''s coherent'')?',
    'Examination of whether the partition can be stated as a positive ontological claim (''Shinto addresses X domain, Buddhism addresses Y domain, and the separation is principled'') or only as a negative claim (''we don''t require integration''). Historical analysis of whether the partition was ever asserted as positively true versus merely accepted as pragmatically useful. Comparison with parallel religious arrangements elsewhere that maintain genuine non-integration versus those that maintain masked integration.',
    'If the partition is genuinely self-standing, it is stable as a distinct reading with its own ε and beneficiary structure. If it depends on the incoherence reading (acknowledged contradiction) as background, the reading may be less independent than the schema represents. This is an Ω_C (conceptual) uncertainty about whether the reading''s claimed axioms are genuinely held or instrumentally invoked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_stability, conceptual, 'Whether the partition is a positive ontological claim or a pragmatic evasion of incoherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement_basis(shin_tr_t200, observed).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 600, 0.41).
narrative_ontology:measurement_basis(shin_tr_t600, observed).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1000, 0.43).
narrative_ontology:measurement_basis(shin_tr_t1000, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1200, 0.44).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1400, 0.42).
narrative_ontology:measurement_basis(shin_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement_basis(shin_be_t200, observed).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 600, 0.28).
narrative_ontology:measurement_basis(shin_be_t600, observed).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1000, 0.31).
narrative_ontology:measurement_basis(shin_be_t1000, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1200, 0.27).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1400, 0.28).
narrative_ontology:measurement_basis(shin_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 200, 0.12).
narrative_ontology:measurement_basis(shin_su_t200, observed).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 600, 0.18).
narrative_ontology:measurement_basis(shin_su_t600, observed).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1000, 0.21).
narrative_ontology:measurement_basis(shin_su_t1000, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1200, 0.19).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1400, 0.19).
narrative_ontology:measurement_basis(shin_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu_ontological_commitment kernel has three distinct constraint readings: partition_reading (Shinto and Buddhism occupy separate ontological domains), syncretic_reading (unified under honji-suijaku), and incoherence_reading (institutionalized contradiction without integration). Each reading instantiates a different ε, beneficiary/victim structure, and suppression profile. The three stories are linked by network.affects_constraints so that contamination propagation and coherence analysis can track how a shift in one reading (e.g., historical evidence for or against syncretic integration) would structurally pressure the other readings. The partition reading claims to be ontologically stable; the omegas document irreducible uncertainties about whether it masks syncretic or incoherent substrates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
