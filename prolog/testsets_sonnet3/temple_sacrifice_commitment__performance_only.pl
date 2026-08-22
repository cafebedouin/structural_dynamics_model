% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Sacrifice Commitment as Dormant Husk — Performance-Only Reading
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This story instantiates the performance_only reading of the
 *   temple_sacrifice_commitment kernel: sacrificial law is a legal system
 *   whose obligations require material instantiation (an altar, an animal, a
 *   functioning priesthood, a specific site), and absent those conditions,
 *   study of the law is archival preservation of a defunct practice rather
 *   than occupation of the commitment it describes. This is distinct from the
 *   study_as_exercise reading (where study itself discharges the commitment)
 *   and the symbolic_transformation reading (where prayer/study are an
 *   authorized substitute instantiation) and the hybrid_preparatory reading
 *   (a suspended middle state). Under this reading the corpus is currently a
 *   dormant husk: real, coordinated, low-cost to maintain, but not doing what
 *   it claims to do. ε stays low because nothing is presently extracted by
 *   anyone from anyone under this reading's own terms — no one performs
 *   sacrifice, so there is no live extraction mechanism, only a maintained
 *   claim.
 *
 * KEY AGENTS:
 *   - study_house_scholars: coordinate and benefit from corpus preservation without occupying the commitment
 *   - denominational_authorities_preserving_distinction: agenda-setters maintaining the doctrinal boundary between archiving and occupying
 *   - future_restoration_claimants: currently absent, structurally implicated if restoration is ever attempted
 *   - sibling_reading_communities: excluded by this reading's core premise from claiming their own practice occupies the commitment
 *   - comparative_halakhic_analysts: analytical observers across the whole kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.08).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.12).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Sacrifice Commitment as Dormant Husk — Performance-Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, 'c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d').
narrative_ontology:cs_kernel_codification('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', fixed_text).
narrative_ontology:cs_authority_grounding('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', lineage).
narrative_ontology:cs_interpretation_layer_present('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d').
narrative_ontology:cs_reading_relation('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', foundational, material_instantiation_is_necessary_condition).
narrative_ontology:cs_axiom_status(material_instantiation_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', material_instantiation_is_necessary_condition, conventional).
narrative_ontology:cs_axiom('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', foundational, study_without_performance_is_archival_not_occupational).
narrative_ontology:cs_axiom_status(study_without_performance_is_archival_not_occupational, holdable).
narrative_ontology:cs_axiom_grounding('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', study_without_performance_is_archival_not_occupational, conventional).
narrative_ontology:cs_reference_frame('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', temple_era_literal_performance_standard).
narrative_ontology:cs_drift_state('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', contemporary_post_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c8a8ddd8-e436-4bbe-a9cf-281c7972cc3d', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, study_house_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, denominational_authorities_preserving_distinction).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, material_instantiation_requirement).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, commitment_dormancy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote substantial intellectual labor to reconstructing sacrificial law in exhaustive procedural detail, gaining scholarly prestige and continuity of a canonical curriculum, while the performance-only reading tells them plainly that this labor is preservation, not occupation of the commitment. They benefit from the intellectual status the study confers, and from the coordination function of keeping the corpus alive for a hypothetical future, without bearing any material cost of performance since none is required or permitted.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, study_house_scholars, beneficiary,
    moderate, civilizational, constrained, global).

% Maintain the theological line that study is archival rather than occupational, which forecloses claims that lay study or informal prayer circles have already fulfilled or substituted for the commitment. This preserves their gatekeeping authority over what counts as legitimate restoration and prevents rival readings (study-as-exercise, symbolic-transformation) from declaring the matter settled and the authorities' continued vigilance unnecessary.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, denominational_authorities_preserving_distinction, agenda_setter,
    institutional, generational, constrained, global).

% Do not yet exist as a concrete population but are structurally implicated: if a future political or religious movement attempted literal restoration under the performance-only reading, the animals used in sacrifice and any populations disrupted by contested control of the physical site would bear costs. They are absent from the current conversation because the reading currently produces no material practice, only a standing claim that would activate victims if triggered.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, future_restoration_claimants, excluded,
    powerless, civilizational, trapped, national).

% Communities holding the study-as-exercise or symbolic-transformation readings experience their own study and prayer as full occupation of the commitment; the performance-only reading structurally denies them this standing, telling them their practice — however devout — does not discharge the obligation. They are excluded from this constraint's own framework by definition, not merely absent from a room.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, sibling_reading_communities, excluded,
    organized, generational, mobile, global).

% Study the kernel contest itself across all four readings, documenting how each reading's authority-grounding and drift response differs, without holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, comparative_halakhic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__performance_only, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__performance_only, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves an exhaustive, standardized procedural corpus (a coordination good: shared terminology, sequencing, disqualifying-defect taxonomy) so that if material conditions for performance ever recur, execution would not have to be reinvented from fragments.
% TRANSFER_FUNCTION: Moves scholarly attention, curricular time, and institutional prestige toward the custodianship of a currently unexercisable body of law; moves nothing material since no sacrifice occurs. No resource flows from a payer class under this reading — the corpus is inert with respect to victims until and unless performance is attempted.
% ABSENT_VOICES: Communities holding the study-as-exercise and symbolic-transformation readings would strenuously object that their devotional practice already occupies the commitment; they are excluded from this constraint's frame by the reading's own core premise, not by procedural oversight. Future restoration claimants and any population that would be materially affected by literal restoration are also absent, since under this reading no restoration is presently occurring.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished overnight, the scholarly corpus itself would not disappear, but its theological status would shift: study could then be reclassified as occupation (collapsing into the study-as-exercise reading) or as authorized transformation (collapsing into symbolic-transformation), each of which would relieve pressure toward literal restoration but also remove the doctrinal firewall the current authorities use to distinguish sanctioned scholarship from premature or illegitimate practice. Whether this counts as the world rearranging or staying the same is itself contested between the sibling communities.
% FOUNDING_PROBLEM: Following the destruction of the physical site, the tradition needed a way to keep sacrificial law alive as a formal body of commitments — pending eventual restoration — without either declaring the commitment abandoned or permitting ad hoc, uncontrolled attempts at partial performance.
% FOUNDING_PROBLEM_CORROBORATION: The custodial authorities themselves attest the founding problem is still live (restoration remains theologically pending). Comparative historians of religion and scholars from the sibling reading communities (study-as-exercise, symbolic-transformation) attest from outside this reading's beneficiary set that the material conditions triggering literal performance have been absent for millennia and that the performance-only reading now functions primarily to preserve institutional authority over doctrinal boundaries rather than to track a genuinely pending restoration.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08) because under this reading's own terms nothing material currently changes hands — there is no sacrifice, no victim, no payer. Suppression is low-moderate (0.12) because no one is coercively prevented from studying; the constraint's force is doctrinal classification, not physical barrier. Theater ratio is authored moderately high and rising (0.4 to 0.65) because as generations pass with no restoration, an increasing share of the corpus's institutional activity (conferences, publication, curricular defense of the distinction) functions to perform continuity of relevance rather than to prepare for an imminent, concretely anticipated event — this is the piton signature: a former functional legal system whose primary activity has become inertial maintenance of its own classificatory boundary. Accessibility collapse is moderate (0.35): scholars could shift to a rival reading (study-as-exercise) without material barrier, which is precisely why the doctrinal boundary requires active theological defense rather than physical enforcement. Resistance is moderate (0.4): sibling reading communities actively contest the performance_only framing.
 *
 * PERSPECTIVAL GAP:
 *   From the custodial authority seat, the arrangement is a necessary, principled holding pattern preserving doctrinal integrity against premature or heterodox practice. From the sibling reading communities' seat, the same arrangement is an act of theological gatekeeping that denies their devotional practice the standing it claims for itself. The engine computes these as different seat-level readings of the same structural data; this story does not adjudicate between them, only authors the performance_only seat's own metrics honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   Study house scholars and denominational authorities sit near the beneficiary end: they retain prestige, curricular continuity, and gatekeeping authority from maintaining the dormancy classification, at negligible cost since no performance is required of them. Future restoration claimants are declared with trapped exit and powerless standing because, should the reading's own logic ever be acted upon, they would bear costs they cannot presently negotiate or exit from — this is a directionality claim about a currently latent but structurally real future target class, not a claim about anyone bearing costs today.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (keeping the law alive pending restoration) is authored as contested rather than flatly dead or live: the custodial authorities insist the problem remains genuinely live (restoration theologically pending), while outside observers note the material trigger conditions have been absent for millennia, suggesting the arrangement's actual present function is boundary-maintenance rather than restoration-preparation. This is exactly the mismatch the R5 apparatus is built to surface: founding_problem_status=contested plus disappearance_verdict=contested should not be read as either resolved coordination or confirmed capture, but flagged for the piton/theater cross-check the engine performs — a dormant husk with rising theater ratio is a classic pre-piton signature, distinct from either the low-epsilon rope the study_as_exercise reading would compute, or the potential tangled_rope this reading itself becomes if restoration is ever attempted without addressing the future-victims omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_vs_active_suppression,
    'Is the commitment genuinely dormant (no material trigger conditions exist or are sought), or is the ''performance_only'' classification itself an active suppression mechanism preventing rival readings (study_as_exercise, symbolic_transformation) from being recognized as valid occupation, thereby preserving custodial authority indefinitely?',
    'Track whether denominational authorities invest resources in actively contesting sibling readings (doctrinal rulings, excommunication risk, curricular exclusion) versus merely holding the position passively; active, resourced contestation of sibling readings would indicate the classification functions as suppression rather than neutral description.',
    'If suppression-driven, the true structural type shifts toward a piton with an active enforcement layer (closer to a snare on the authority seat) rather than a genuinely inert husk; if passive, the piton classification with low suppression stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_vs_active_suppression, conceptual, 'Whether the dormancy claim is neutral description or active boundary enforcement against sibling readings.').

omega_variable(
    future_restoration_trigger_ambiguity,
    'What would count, under this reading''s own terms, as the material conditions sufficient to convert the dormant husk into an active occupied commitment — and who decides when those conditions are met?',
    'Examine historical and contemporary doctrinal rulings on what specifically constitutes sufficient material instantiation (site control, priesthood genealogy verification, ritual purity apparatus) and who holds recognized authority to declare the threshold crossed.',
    'If the threshold is deliberately left maximally vague or is controlled entirely by the current custodial authorities, the reading functions partly to preserve those authorities'' future veto power over restoration attempts, which would support authoring a directionality override raising their effective d toward extraction rather than pure beneficiary status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_restoration_trigger_ambiguity, empirical, 'Ambiguity in what triggers activation from dormant husk to occupied commitment, and who controls that determination.').

omega_variable(
    cs_framing_kernel_vs_authority_layer,
    'Is the more defensible framing of this constraint the kernel itself (the sacrificial law corpus and its material-instantiation requirement), or the layered legitimacy claim above it (the custodial authorities'' standing to declare which readings are permissible)?',
    'Compare classification outcomes: framed as the kernel alone, this reads as a formalized, lineage-grounded commitment system with low extraction; framed as the authority layer, the same facts foreground an institutional actor whose authority is partly constituted by successfully policing the boundary between archiving and occupying, which raises the salience of the extraction question even though ε itself does not change.',
    'The kernel-framing supports the piton/dormant-husk classification authored here. The authority-layer framing would emphasize institutional self-preservation more heavily and might support authoring denominational_authorities_preserving_distinction with a directionality override rather than derived beneficiary status, since the derivation from beneficiary declaration alone may understate how much the authorities'' institutional standing depends on the boundary holding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_authority_layer, conceptual, 'Alternative framing of the constraint as the kernel corpus versus the authority structure that adjudicates it, and how that choice shapes emphasis without changing the authored epsilon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__performance_only, theater_ratio, 20, 0.48).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__performance_only, theater_ratio, 40, 0.55).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__performance_only, theater_ratio, 60, 0.58).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__performance_only, theater_ratio, 80, 0.62).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__performance_only, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__performance_only, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__performance_only, base_extractiveness, 40, 0.06).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__performance_only, base_extractiveness, 60, 0.07).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__performance_only, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__performance_only, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This is one of four sibling stories decomposing the natural-language label 'the sacrifice commitment' per the ε-invariance principle: performance_only (this story, piton-shaped dormant husk), study_as_exercise (rope-shaped, study itself as occupation), symbolic_transformation (rope-shaped, authorized substitute instantiation), and hybrid_preparatory (scaffold-shaped, explicit suspended/preparatory framing with an implicit sunset at restoration). Each reading authors its own ε and claimed_type against the same kernel text; none averages or hedges across the others. All four are linked bidirectionally via affects_constraints since they are direct competitors for doctrinal authority over the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
