% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition Reading (Kami/Buddha Separation)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   The domain partition reading instantiates the shinbutsu-shugo
 *   (kami-Buddha coexistence) commitment by positing that kami and Buddhist
 *   deities operate in structurally separate soteriological and functional
 *   domains without requiring ontological unification. Kami govern life-world
 *   concerns—purification, fertility, harvest, daily fortune, ancestral
 *   veneration at the local level—while Buddhas govern eschatological
 *   concerns—death, karma, rebirth, ultimate salvation. This reading
 *   preserves the actual institutional and practical organization of medieval
 *   and early-modern Japanese religious life: lay practitioners visited
 *   shrines for life-cycle blessings and temples for death rituals without
 *   experiencing doctrinal incoherence. The constraint is CLAIMED as rope
 *   (genuine coordination solving a real problem: coexistence of two powerful
 *   religious systems without forcing theological reduction) while the
 *   metrics show moderate extractiveness and low suppression (the partition
 *   is maintained through institutional practice and popular legitimacy, not
 *   coercion). The measurement series on a shared time grid tracks the slow
 *   rise in extractiveness and theater ratio as the institutional burden of
 *   maintaining the partition increased—scholastic theologians paid
 *   intellectual costs as honji suijaku metaphysics became increasingly inert
 *   commentary on a partition that institutional practice actually enforced.
 *
 * KEY AGENTS:
 *   - Buddhist institutional authority: sets and administers the partition through ritual codification and doctrinal boundary maintenance; benefits by avoiding unification burden while maintaining reach into both domains
 *   - Kami shrine practitioners: preserve kami worship autonomy; benefit from institutional legitimacy in their domain without requiring Buddhist philosophical integration
 *   - Lay practitioners: benefit from low cognitive load; navigate both systems seamlessly at the lived level without experiencing contradiction
 *   - Scholastic theologians: bear intellectual burden of maintaining theoretical honji suijaku while practical operation diverges; their positions become increasingly performative
 *   - Meiji state reformers: excluded from the partition consensus; later able to overturn it by claiming fundamental incoherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.29).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition Reading (Kami/Buddha Separation)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'f41648f0-5b17-4eba-ae0a-9e063ec3463d').
narrative_ontology:cs_kernel_codification('f41648f0-5b17-4eba-ae0a-9e063ec3463d', distributed).
narrative_ontology:cs_authority_grounding('f41648f0-5b17-4eba-ae0a-9e063ec3463d', practice).
narrative_ontology:cs_interpretation_layer_present('f41648f0-5b17-4eba-ae0a-9e063ec3463d').
narrative_ontology:cs_reading_relation('f41648f0-5b17-4eba-ae0a-9e063ec3463d', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('f41648f0-5b17-4eba-ae0a-9e063ec3463d', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('f41648f0-5b17-4eba-ae0a-9e063ec3463d', foundational, soteriological_domains_are_structurally_distinct).
narrative_ontology:cs_axiom_status(soteriological_domains_are_structurally_distinct, holdable).
narrative_ontology:cs_axiom_grounding('f41648f0-5b17-4eba-ae0a-9e063ec3463d', soteriological_domains_are_structurally_distinct, deontological).
narrative_ontology:cs_axiom('f41648f0-5b17-4eba-ae0a-9e063ec3463d', foundational, boundary_maintenance_without_unification_is_legitimate).
narrative_ontology:cs_axiom_status(boundary_maintenance_without_unification_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f41648f0-5b17-4eba-ae0a-9e063ec3463d', boundary_maintenance_without_unification_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('f41648f0-5b17-4eba-ae0a-9e063ec3463d', functional_coexistence_without_ontological_unification).
narrative_ontology:cs_drift_state('f41648f0-5b17-4eba-ae0a-9e063ec3463d', meiji_modernization_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f41648f0-5b17-4eba-ae0a-9e063ec3463d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, kami_shrine_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__domain_partition_reading, scholastic_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples and their institutional hierarchy maintained the partition reading by codifying practices that respected kami domains (kami invoked for harvest, daily life; Buddhas invoked for death, afterlife salvation). They administered the boundary through ritual prescription and doctrinal teaching without requiring theological unification. They benefited from this reading by avoiding the doctrinal burden of reconciling fundamentally distinct soteriologies while maintaining institutional reach into both domains.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_authority, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_authority, beneficiary).

% Shrine priests and local communities preserved kami worship as a distinct practice domain: purification rituals, harvest ceremonies, fertility rites, ancestral veneration at the local level. The partition reading allowed them to maintain kami practice without requiring integration into Buddhist philosophical frameworks, preserving indigenous practice autonomy. They retained institutional autonomy and popular legitimacy in their domain.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, kami_shrine_practitioners, beneficiary,
    moderate, generational, mobile, local).

% Ordinary people navigated both systems seamlessly: visiting shrines for life-cycle blessings (birth, marriage, harvest), visiting temples for death rituals and afterlife concerns. The partition reading solved the cognitive load problem — no theological reconciliation required, no internal contradiction felt at the lived level. They could pray to Amaterasu for daily fortune and to Amida Buddha for salvation without experiencing doctrinal incoherence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, lay_practitioners, beneficiary,
    powerless, biographical, mobile, local).

% Buddhist scholars and philosophers who inherited the honji suijaku tradition bore the burden of maintaining theoretical silence on the partition: they could not fully articulate the domain separation without contradicting syncretism (which remained institutionally valorized), yet the partition reading's practical force left their unification metaphysics increasingly inert. They paid in intellectual resources managing the contradiction between what institutional practice actually did (partition) and what doctrine was supposed to claim (fusion).
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, scholastic_theologians, payer,
    moderate, biographical, constrained, national).

% Meiji administrators were excluded from the domain-partition consensus; they would later argue that the constraint was neither genuinely kami-affirming nor genuinely Buddhist, but rather institutional compromise masking religious incoherence. Their exclusion from the negotiating table meant they had no standing in the traditional shared framework, which positioned them to later overturn it via state power rather than intellectual persuasion.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_reformers, excluded,
    institutional, biographical, trapped, national).

% Modern scholars study the partition reading as evidence that functional coexistence of distinct soteriologies is possible without ontological unification. They observe the constraint without participation in its authority structures and provide external analysis of how long the partition held and what conditions eventually destabilized it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, comparative_religionists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_institutional_authority).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables coexistence of two distinct soteriological and ceremonial systems addressing irreducibly different existential problems: kami practices address life-world concerns (purification, fertility, harvest, daily fortune, family ancestral veneration); Buddhist practices address eschatological concerns (death, karma, rebirth, ultimate salvation). The partition solves the institutional coordination problem: how to permit both systems institutional legitimacy and lay participation without forcing either to retreat or to submit to theological reduction.
% TRANSFER_FUNCTION: Moves authority to adjudicate and practice different domains between kami shrines and Buddhist temples. Shrine authority over life-cycle and seasonal rituals; temple authority over death rites and afterlife concerns. Transfers doctrinal autonomy to lay practitioners: they conduct kami rituals without requiring Buddhist philosophical framework and conduct Buddhist practice without requiring kami integration into Buddhist metaphysics. Transfers institutional resources: maintenance of separate hierarchies, ritual specialists, textual traditions.
% ABSENT_VOICES: Theological systematizers seeking genuine ontological unification (honji suijaku advocates who want the partition dissolved into fusion) are present but systematically downweighted in the partition-reading consensus. Kami-exclusivists seeking to eliminate Buddhist supplementation entirely are outside the working coalition. Later, Meiji state reformers who would declare the partition fundamentally incoherent were excluded from the pre-Meiji negotiated framework—their exclusion positioned them to later overturn it.
% DISAPPEARANCE_RATIONALE: If the partition reading disappeared and were replaced by syncretic fusion (forcing kami into Buddhist metaphysics), Japanese religious practice would reorganize around unified Buddhist soteriology with kami as manifestations—kami worship would lose institutional autonomy and be subordinated to Buddhist eschatological logic. If replaced by incoherence reading (declaring the partition was never coherent), lay practice would face explicit contradiction requiring individual resolution or institutional schism—religious life would lose the low-friction coexistence. The partition enables an arrangement that would not survive its removal.
% FOUNDING_PROBLEM: Early medieval Japan faced institutional coordination problem: two powerful religious systems with distinct institutional bases and lay constituencies. Buddhism was powerful in court, aristocratic discourse, and philosophical speculation; kami worship was embedded in local agriculture, family, community practice, and indigenous religious sensibility. Neither could exclude the other; both had too much institutional and social investment. The founding problem was: how to permit both systems to operate and expand without forcing theological unification (which would require kami to be reduced to Buddhist doctrine) or institutional separation (which would require kami worship to retreat to pre-Buddhist localism or Buddhism to retreat to elite discourse).
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese Buddhism (Richard Bowring, James Heisig) and Shinto scholars (Inoue Nobutaka) attest from outside the Buddhist institutional beneficiary set that the partition reading solved a genuinely live coordination problem in early and medieval Japan—two powerful religious traditions with incompatible metaphysical foundations cannot coexist institutionally without either unification or separation; the partition models a third path through institutional boundary maintenance. Contemporary scholars (Diana Eck on religious pluralism) observe that the partition reading remains relevant to how distinct traditions coexist with autonomy in pluralist contexts. Meiji-era reformers who declared shinbutsu-bunri necessary (shinbutsu-separation) implicitly attested that the partition had been functional—it was only the state modernization project, not internal doctrinal refutation, that made separation seem necessary.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness at 0.38 reflects that the constraint does extract institutional authority and interpretive control from both lay practitioners and scholastic theologians—lay practitioners lose the right to theologically reconcile the two systems on their own terms; theologians must maintain syncretist metaphysics even as institutional practice partitions. Suppression at 0.29 is moderate because the constraint is maintained through institutional practice and popular legitimacy rather than coercive denial of alternatives—practitioners could (and some did) adopt fusion readings, but the partition reading was pragmatically overwhelmingly advantageous. Theater at 0.22 captures the increasing performative character of honji suijaku commentary—institutional Buddhism maintained the fusion metaphysics as valorized doctrine while institutional practice and lay experience operated on a partition logic. Accessibility collapse at 0.45 reflects that alternatives (pure syncretic fusion requiring theological reduction; pure separation requiring institutional schism) are available but less functional than the partition. Resistance at 0.52 indicates the constraint meets real pushback from theologians seeking genuine unification and from kami-exclusivists seeking independence, though that resistance operates within elite discourse rather than threatening institutional stability. The measurement series shows steady rise in extractiveness and stakes-inflation particularly at the organizational level (1868): institutional Buddhism increasingly required maintenance of the partition boundary even as Meiji modernization challenged its legitimacy premises. Stakes-inflation at the structural level rises as state reformers externally declare the partition incoherent, raising the cost of maintaining it against state pressure.
 *
 * PERSPECTIVAL GAP:
 *   Payers and beneficiaries should experience dramatically different classifications. From the institutional authority seat: rope, genuine coordination function, moderate theater as maintenance overhead. From the theologian seat: tangled_rope or snare trending, asymmetric extraction (institutional hierarchy forces partition maintenance on them), increasing theater as institutional inertia overtakes functional purpose. The engine derives these from the declared power atoms, exit options, and beneficiary/victim structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (buddhist_institutional_authority, lay_practitioners) derive low d from beneficiary role declarations: institutional authority gets arbitrage-grade exit (powerful, institutional position, could walk away); lay practitioners get mobile exit (could theoretically adopt pure fusion or pure separation, but functional cost of doing so is high). Payers (scholastic_theologians) derive higher d from payer role: they have constrained exit (identity_locked into their scholastic role, institutional hierarchy, disciplinary authority structures—their career depends on sophisticated Buddhist philosophy, so rejecting the partition would require institutional apostasy). The partition reading binds theologians more tightly than it binds lay practitioners precisely because theologian identity is constituted through the philosophical tradition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling coexistence of two powerful religious systems without forcing one to retreat) remains live at 1868 but the institutional commitment (the partition reading itself) has begun to atrophy under Meiji pressure. The mismatch between disappearance_verdict (world_rearranges—arrangements depend on the partition) and founding_problem_status (live—the original coordination problem persists) is NOT a mandatrophy marker because the constraint is not yet a theater of pure inertia. Instead, the rising theater_ratio and stakes_inflation in the measurement series indicate PREPARATION for mandatrophy: institutional Buddhism is maintaining the partition increasingly through administrative/doctrinal performance rather than through actual institutional vigor, setting up the condition where state power (Meiji) can overturn it rapidly because the constraint's legitimacy has already hollowed into theater. At the moment of snapshot (1868), mandatrophy is not yet present, but the trajectory is unmistakable—the constraint is transitioning from functional coordination maintained by institutional practice and popular legitimacy toward a theater of inert metaphysics maintained by administrative hierarchy alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fusion_vs_partition_empirical_status,
    'Did pre-modern Japanese practitioners actually experience the two systems as fused (honji suijaku) or as partitioned (separate domains)? Does doctrinal endorsement of fusion reflect genuine lived experience or theologians'' aspirational unification?',
    'Textual analysis of devotional literature, prayer records, and ritual prescriptions: if honji suijaku is invoked in actual devotional practice and prayer, fusion is phenomenologically operative; if it appears only in scholastic commentary while actual prayers address kami and Buddha separately, the partition is the operative framework.',
    'If practitioners actually experienced fusion, the partition reading mis-characterizes the constraint and both reading-types may be computing the same underlying practice differently. If fusion appears only in elite commentary while lay practice partitions, the partition reading is descriptively accurate and fusion reading is an elite theoretical aspiration disconnected from the actual constraint the engine should measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_vs_partition_empirical_status, empirical, 'Whether fusion (honji suijaku) was a lived experience or scholastic ideal disconnected from actual practice').

omega_variable(
    institutional_enforcement_of_partition_boundary,
    'To what extent was the partition actively enforced by institutional authority vs. maintained through popular preference? Did shrine and temple administrators suppress fusion advocates or did they simply permit non-enforcement?',
    'Historical records of doctrinal disputes, institutional sanctions against fusion preachers, and institutional prescriptions for boundary maintenance. If institutions actively suppressed fusion advocates, enforcement was real and suppression >0.4. If institutions were permissive and the partition persisted through lay convenience, suppression <0.2.',
    'Higher institutional enforcement would raise suppression measurement and push the constraint from rope toward tangled_rope. Lower enforcement suggests the partition is genuine coordination (low suppression, genuine rope) rather than institutional extraction requiring active boundary maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_of_partition_boundary, empirical, 'Whether institutional authority actively enforced boundary separation or merely permitted it through non-interference').

omega_variable(
    performance_ratio_temporal_validity,
    'Does the rising theater_ratio in the measurement series reflect actual institutional decline in functional purpose, or does it reflect changing external scrutiny (Meiji modernization''s critical gaze) that makes the partition appear theatrical to observers while practitioners experience it as functional?',
    'Comparative analysis: if internal institutional documents from 1650-1868 show declining functional rhetoric and rising boundary-maintenance-as-duty language, the rise is real institutional drift; if external critics (Meiji reformers, Western scholars) impose the theatrical reading but internal documents show sustained functional purpose, the rise reflects observer effect.',
    'If the rise is real institutional drift toward performance, mandatrophy preparation is genuine and the constraint is transitioning to piton. If the rise is observer-driven projection, the constraint may remain functional-rope despite external critique.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performance_ratio_temporal_validity, conceptual, 'Whether rising theater ratio reflects institutional purpose-drift or external observer critique imposing theatrical frame').

omega_variable(
    kernel_contest_resolution_path,
    'Which reading (partition, fusion, incoherence) would win if the contest were resolved by internal theological persuasion vs. external state power? Did Meiji victory in favor of shinbutsu-bunri reflect genuine intellectual refutation of the partition, or did state force overturn a still-functional arrangement?',
    'Counterfactual: if Meiji had not imposed state force, would Buddhist institutions have voluntarily abandoned the partition for fusion or incoherence? If institutional Buddhism shows no sustained intellectual movement toward either alternative before 1868, state power was the decisive force, not internal resolution.',
    'If state power was decisive, the constraint was genuine coordination (rope) that was forcibly overthrown—the partition reading remains valid as a descriptor of pre-Meiji institutional logic. If Buddhist institutions were already internally moving toward incoherence reading, Meiji merely accelerated an already-underway transition—the partition was already atrophying (piton preparation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_resolution_path, conceptual, 'Whether Meiji shinbutsu-bunri reflected intellectual refutation of partition or external force overthrow of a still-functional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1000, 0.11).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.14).
narrative_ontology:measurement(shin_tr_t1450, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1450, 0.18).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1650, 0.21).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.22).

% Extraction over time
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.18).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1000, 0.22).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement(shin_be_t1450, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1450, 0.35).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1650, 0.38).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 800, 0.12).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1000, 0.16).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1200, 0.19).
narrative_ontology:measurement(shin_su_t1450, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1450, 0.24).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1650, 0.28).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1868, 0.29).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=800, tn=1868
narrative_ontology:measurement(shin_grid_01, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(class), 800, 0.38).
narrative_ontology:measurement(shin_grid_02, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(class), 1868, 0.48).
narrative_ontology:measurement(shin_grid_03, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(individual), 800, 0.28).
narrative_ontology:measurement(shin_grid_04, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(individual), 1868, 0.35).
narrative_ontology:measurement(shin_grid_05, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(organizational), 800, 0.42).
narrative_ontology:measurement(shin_grid_06, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(organizational), 1868, 0.58).
narrative_ontology:measurement(shin_grid_07, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(structural), 800, 0.35).
narrative_ontology:measurement(shin_grid_08, shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse(structural), 1868, 0.52).
narrative_ontology:measurement(shin_grid_09, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(class), 800, 0.35).
narrative_ontology:measurement(shin_grid_10, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(class), 1868, 0.62).
narrative_ontology:measurement(shin_grid_11, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(individual), 800, 0.28).
narrative_ontology:measurement(shin_grid_12, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(individual), 1868, 0.42).
narrative_ontology:measurement(shin_grid_13, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(organizational), 800, 0.38).
narrative_ontology:measurement(shin_grid_14, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(organizational), 1868, 0.65).
narrative_ontology:measurement(shin_grid_15, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(structural), 800, 0.32).
narrative_ontology:measurement(shin_grid_16, shinbutsu_coexistence_commitment__domain_partition_reading, resistance(structural), 1868, 0.58).
narrative_ontology:measurement(shin_grid_17, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(class), 800, 0.18).
narrative_ontology:measurement(shin_grid_18, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(class), 1868, 0.38).
narrative_ontology:measurement(shin_grid_19, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(individual), 800, 0.08).
narrative_ontology:measurement(shin_grid_20, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(individual), 1868, 0.12).
narrative_ontology:measurement(shin_grid_21, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(organizational), 800, 0.22).
narrative_ontology:measurement(shin_grid_22, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(organizational), 1868, 0.55).
narrative_ontology:measurement(shin_grid_23, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(structural), 800, 0.15).
narrative_ontology:measurement(shin_grid_24, shinbutsu_coexistence_commitment__domain_partition_reading, stakes_inflation(structural), 1868, 0.42).
narrative_ontology:measurement(shin_grid_25, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(class), 800, 0.12).
narrative_ontology:measurement(shin_grid_26, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(class), 1868, 0.28).
narrative_ontology:measurement(shin_grid_27, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(individual), 800, 0.06).
narrative_ontology:measurement(shin_grid_28, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(individual), 1868, 0.15).
narrative_ontology:measurement(shin_grid_29, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(organizational), 800, 0.14).
narrative_ontology:measurement(shin_grid_30, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(organizational), 1868, 0.38).
narrative_ontology:measurement(shin_grid_31, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(structural), 800, 0.08).
narrative_ontology:measurement(shin_grid_32, shinbutsu_coexistence_commitment__domain_partition_reading, suppression(structural), 1868, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu-shugo commitment kernel admits three structurally distinct constraint readings with different ε values and beneficiary structures. (1) The domain_partition_reading (this file) posits separate soteriological domains without unification—low-moderate extractiveness, institutional Buddhism benefits from avoiding unification burden. (2) The syncretic_fusion_reading posits honji suijaku (kami as Buddhist manifestations)—requires theological reduction of kami to Buddhism, different beneficiary structure. (3) The incoherent_bundle_reading claims the partition was never coherent but maintained through institutional power—higher extractiveness, theater-dominant constraint. These are not three perspectives on one constraint but three different constraints instantiated from the same kernel. The partition reading's ε (0.38) is structurally distinct from fusion reading ε and incoherence reading ε—different observables. Decomposition follows DP-001 (ε-invariance): a reading that requires theological fusion changes which agents benefit and how extraction is distributed; a reading that declares the partition incoherent changes the referent of the entire constraint. All three readings share the kernel (shinbutsu-coexistence) but emit different constraints. Links established via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
