% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Sacrificial Commitment as Authorized Symbolic Transformation (Prayer/Study Instantiation)
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates the symbolic_transformation reading of the
 *   sacrifice-commitment kernel: prayer and study are held to be an
 *   AUTHORIZED NEW INSTANTIATION of the biblical sacrificial commandment, not
 *   a temporary substitute standing in for a suspended practice pending
 *   restoration. This is structurally distinct from study_as_exercise (which
 *   treats study itself as direct performance, without claiming
 *   transformation), performance_only (which denies any non-material
 *   fulfillment is possible), and hybrid_preparatory (which treats the
 *   current period as suspended-but-preparatory rather than
 *   transformed-and-complete). The distinguishing move of this reading is the
 *   claim of AUTHORITY TO TRANSFORM: the rabbinic tradition asserts it has
 *   standing to redefine the mode of fulfillment of a divine command, not
 *   merely to manage an interim absence. That authority claim is exactly what
 *   generates elevated extractiveness in this reading relative to a purely
 *   preparatory or exercise-based account — if the transformation is
 *   unauthorized drift dressed as continuity, the institutional structure
 *   that benefits from the settled answer (liturgical establishment, rabbinic
 *   centrality) is extracting legitimacy it has not earned, at the expense of
 *   those who hold the original material commandment as non-negotiable and
 *   unmet.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.62).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.58).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Sacrificial Commitment as Authorized Symbolic Transformation (Prayer/Study Instantiation)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '842b1b68-33eb-4d1a-892b-3df3eb793101').
narrative_ontology:cs_kernel_codification('842b1b68-33eb-4d1a-892b-3df3eb793101', fixed_text).
narrative_ontology:cs_authority_grounding('842b1b68-33eb-4d1a-892b-3df3eb793101', lineage).
narrative_ontology:cs_interpretation_layer_present('842b1b68-33eb-4d1a-892b-3df3eb793101').
narrative_ontology:cs_reading_relation('842b1b68-33eb-4d1a-892b-3df3eb793101', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('842b1b68-33eb-4d1a-892b-3df3eb793101', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('842b1b68-33eb-4d1a-892b-3df3eb793101', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('842b1b68-33eb-4d1a-892b-3df3eb793101', foundational, rabbinic_authority_may_reconstitute_command_mode).
narrative_ontology:cs_axiom_status(rabbinic_authority_may_reconstitute_command_mode, holdable).
narrative_ontology:cs_axiom_grounding('842b1b68-33eb-4d1a-892b-3df3eb793101', rabbinic_authority_may_reconstitute_command_mode, conventional).
narrative_ontology:cs_axiom('842b1b68-33eb-4d1a-892b-3df3eb793101', foundational, prayer_constitutes_completed_fulfillment_not_interim_measure).
narrative_ontology:cs_axiom_status(prayer_constitutes_completed_fulfillment_not_interim_measure, holdable).
narrative_ontology:cs_axiom_grounding('842b1b68-33eb-4d1a-892b-3df3eb793101', prayer_constitutes_completed_fulfillment_not_interim_measure, deontological).
narrative_ontology:cs_reference_frame('842b1b68-33eb-4d1a-892b-3df3eb793101', second_temple_priestly_cultic_order).
narrative_ontology:cs_drift_state('842b1b68-33eb-4d1a-892b-3df3eb793101', contemporary_diaspora_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('842b1b68-33eb-4d1a-892b-3df3eb793101', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, post_temple_liturgical_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, material_restorationist_communities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, temple_mount_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_observant_practitioners).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, diaspora_observant_practitioners).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_to_reconstitute_divine_command).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, prayer_as_valid_sacrificial_substitute_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares, through accumulated halakhic ruling (particularly the Talmudic principle that prayer stands 'in place of' the sacrifices, and codified across the liturgy), that the obligation attached to sacrifice has been transformed rather than suspended or archived. This ruling is the mechanism by which the commitment continues to be experienced as occupied — Jews praying three times daily are told they are fulfilling, not deferring, the underlying command. The authority structure that issues this ruling also administers and depends on the resulting liturgical institution: synagogues, prayer books, and the rabbinate's own interpretive centrality all rest on the transformation claim being accepted as authoritative rather than as an emergency workaround.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, beneficiary).

% Synagogue structures, siddur publishing, cantorial and rabbinic professional classes, and communal prayer infrastructure all derive their centrality from the claim that prayer is the authorized new instantiation of the commitment, not a stopgap. If the commitment were instead read as merely suspended-pending-restoration (hybrid_preparatory) or as requiring literal performance (performance_only), much of this institutional apparatus would be reframed as provisional rather than as the fulfillment itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, post_temple_liturgical_institutions, beneficiary,
    institutional, civilizational, mobile, global).

% The vast majority of contemporary observant Jews receive, through this reading, a way to consider the commandment fully occupied through prayer and study without material access to a functioning Temple or altar — this resolves what would otherwise be a permanent, unfulfillable obligation. The cost they bear is subtle: they inherit a settled answer to a live theological question, foreclosing (for most, practically) engagement with whether the commitment is actually being met or merely narratively closed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_observant_practitioners, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, diaspora_observant_practitioners, payer).

% Communities and individuals who hold that only literal reinstatement of sacrificial performance can occupy the commitment experience the symbolic_transformation ruling as authority overreach — a claim that rabbinic institutions possess the standing to redefine what God commanded, rather than merely to manage its temporary absence. They bear the cost of being told their preferred reading is settled rather than live; their theological position is treated by mainstream institutions as fringe or premature, and they have no comparable institutional platform from which to contest the ruling's authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, material_restorationist_communities, payer,
    powerless, generational, trapped, national).

% Organizations advocating for practical steps toward Temple rebuilding and resumed sacrificial practice are marginalized in part because the dominant liturgical establishment has already declared the commitment satisfied through prayer — removing the sense of urgency and theological necessity that would otherwise support their project. Political and religious authorities that could enable their aims have little incentive to disturb an arrangement the majority institution treats as settled and complete.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, temple_mount_activists, payer,
    powerless, generational, trapped, regional).

% Study the textual and historical record of how the sacrifice-to-prayer transformation was formulated and authorized, comparing rabbinic self-descriptions of the change (as continuation, not substitution) against the historical circumstances (Temple destruction, Roman suppression) that necessitated it, without being party to either the authority claim or its contestation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, halakhic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides religious continuity for a dispersed population that lost material access to the required sacrificial site and cannot restore it unilaterally — without some authorized reconstitution of the commandment's mode of fulfillment, observant practice would face an unfulfillable, indefinitely suspended obligation at the center of communal life.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority from a site-bound priestly/Temple apparatus to a text-and-prayer-bound rabbinic apparatus; moves theological legitimacy away from restorationist and literalist readings and toward the liturgical establishment; moves practical urgency away from Temple-rebuilding projects.
% ABSENT_VOICES: Material restorationist communities and Temple Mount activists hold that the transformation claim was never theirs to accept or reject — it was declared by an authority structure that also stood to gain administrative and interpretive centrality from the declaration. Their objection (this is unauthorized redefinition of a fixed divine command, not authorized transformation) is treated by the dominant reading as already adjudicated rather than as live.
% DISAPPEARANCE_RATIONALE: If the symbolic_transformation ruling were withdrawn, the liturgical establishment's claim to have fully occupied the commandment would collapse into an admission of ongoing suspension — restorationist and hybrid_preparatory readings would gain immediate legitimacy, potentially reorganizing observant practice around anticipation of restoration rather than settled fulfillment. Mainstream institutions dispute that anything would change, since prayer's efficacy is treated by them as already fully vindicated rather than contingent on the ruling's continued authority.
% FOUNDING_PROBLEM: The Temple's destruction (70 CE) removed the physical site and priestly apparatus required for sacrificial performance, leaving a core biblical commandment with no material means of fulfillment and no clear timeline for restoration.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic tradition itself (via Hosea 14:3, 'let our lips substitute for bullocks,' and Talmudic elaboration) attests that the problem was real and the transformation intentional. Historians of religion outside the tradition's own authority structure corroborate the historical circumstance (forced discontinuation, no chosen substitution) but frequently characterize the 'authorized transformation' framing itself as a retrospective theological narrative constructed by the surviving rabbinic class to consolidate authority after the priestly class lost its institutional base — a reading restorationist communities cite directly against the transformation claim's neutrality.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that a specific interpretive authority converts a genuinely unresolved theological/practical problem (no Temple, no altar) into a settled claim of full occupation, and that settlement redounds heavily to the benefit of the institutions issuing it. Suppression (0.58) is moderate-high: dissenting restorationist readings are not violently suppressed but are structurally marginalized — denied liturgical centrality, treated as fringe within mainstream educational and communal institutions, and given no comparable platform. Theater ratio (0.44) captures that a substantial share of the transformation's maintenance is now performative continuity (millennia of unbroken prayer practice functioning as evidence of validity) rather than fresh argument for the authority claim itself. Accessibility collapse (0.5) and resistance (0.55) sit at moderate levels because material-restorationist and hybrid_preparatory readings remain genuinely live within observant communities — the alternative is not eliminated, only outcompeted institutionally.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority structure's seat, this is not extraction but faithful continuity — the same interpretive tradition that always adapted commandments to circumstance (as with agricultural laws outside the land, or Sabbath laws under duress). From the material restorationist seat, the identical structure is an unelected body claiming a power (redefinition of divine command) that was never delegated to it, entrenched precisely because the entrenchment benefits the body making the claim. The engine computes these as different seat-level types from the same structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority structure and the liturgical institutions built on its ruling are the structural beneficiaries: they gain interpretive centrality, communal infrastructure, and a settled theological answer that removes pressure toward material restoration (which would relocate authority to a priestly/Temple apparatus they do not control). Diaspora practitioners are near-symmetric beneficiary/payer: they receive resolution of an otherwise permanently unfulfillable obligation, but at the cost of inheriting a foreclosed theological question. Material restorationists and Temple Mount activists are near-full targets: their preferred reading is treated as already settled against them by an authority they had no voice in constituting, and their political/religious marginalization is a direct function of the transformation ruling's success.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification carries genuine mandatrophy risk in both directions. If this reading's authority claim is correct — the rabbinic tradition genuinely possesses standing to reconstitute the mode of commandment fulfillment — then treating the arrangement as extraction mislabels legitimate, coordinated religious continuity as capture. If the authority claim is unauthorized drift, treating it as pure coordination (rope) would launder an institutional power-grab as settled theology. Tangled_rope is the honest middle: genuine coordination function (solving an otherwise unfulfillable obligation for a dispersed population) coexists with asymmetric extraction (institutional beneficiaries who also control the narrative that forecloses the losing readings) — both must be true simultaneously and the classification should not resolve toward either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorized_vs_unauthorized_transformation,
    'Did the rabbinic authority structure possess legitimate standing (within its own tradition''s rules for legal/theological change) to declare the sacrificial commandment transformed, or is the transformation claim itself an unauthorized exercise of interpretive power dressed in the language of continuity?',
    'Internal halakhic argument about the scope of rabbinic authority to reconstitute (not merely apply or suspend) a biblical commandment''s mode of fulfillment; comparison with other claimed transformations in the tradition''s history (e.g., post-Temple purity law adaptations) for consistency of method; assessment of whether the transformation claim was contested at the time of its formulation or only retrospectively naturalized.',
    'If authorized, the coordination function dominates and this reading is closer to rope/scaffold in practice even though tangled_rope is authored here; if unauthorized drift, the extraction component is understated at 0.62 and the constraint functions closer to snare for restorationist stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorized_vs_unauthorized_transformation, conceptual, 'Whether the transformation claim was a legitimate exercise of interpretive authority or unauthorized theological drift.').

omega_variable(
    committer_framing_disagreement_location,
    'Where exactly does this reading''s disagreement with the sibling readings live — is it primarily about WHETHER transformation occurred (vs. mere suspension or exercise), or about WHO has standing to declare it?',
    'Textual analysis distinguishing hybrid_preparatory''s ''held open, not closed'' claim, study_as_exercise''s ''no transformation needed, study already performs'' claim, and performance_only''s ''no non-material account is valid'' claim from this reading''s ''transformation occurred and was authorized'' claim — mapping each to distinct premises about the divine command''s nature (fixed material requirement vs. adaptable mode) versus about institutional authority (who may declare adaptation).',
    'If the disagreement is primarily about authority (who may declare), the sibling relations should weight toward coexists_with (parties dispute the legitimacy of a claim, not the claim''s coherence). If primarily about the nature of the command itself (fixed vs. adaptable), some sibling relations shift toward forecloses (a fixed-material premise and an adaptable-mode premise cannot both hold in one framework).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_disagreement_location, conceptual, 'Locating whether the reading disagreement is about authority-to-declare or about the command''s own adaptability.').

omega_variable(
    false_summit_transformation_as_natural_continuity,
    'Is ''prayer replaces sacrifice'' presented by the tradition as an obvious, near-natural continuation of biblical religion (minimizing the visibility of the authority claim it actually rests on), and if so, does that framing itself constitute a false-summit dynamic — a constructed institutional arrangement dressed as an inevitable development?',
    'Historical-critical examination of how quickly and how contested the transformation claim became normalized within rabbinic literature versus how it is taught today (as settled fact rather than as a historically contingent, once-debated ruling); comparison with how other traditions in the same period handled the loss of central cultic sites.',
    'If the transformation is now taught and experienced as simply how the commandment always worked (rather than as a specific interpretive act with identifiable beneficiaries), this constraint''s kinship to a Mountain-style false summit (naturalized construction with real beneficiaries) increases, independent of its tangled_rope classification here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_transformation_as_natural_continuity, empirical, 'Whether the transformation''s naturalized presentation obscures its status as a specific, contestable, beneficiary-laden interpretive act.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(temp_tr_t100, projected).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.35).
narrative_ontology:measurement_basis(temp_tr_t500, projected).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.4).
narrative_ontology:measurement_basis(temp_tr_t1000, projected).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.42).
narrative_ontology:measurement_basis(temp_tr_t1500, projected).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1900, 0.44).
narrative_ontology:measurement_basis(temp_tr_t1900, projected).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(temp_be_t100, projected).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.5).
narrative_ontology:measurement_basis(temp_be_t500, projected).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.56).
narrative_ontology:measurement_basis(temp_be_t1000, projected).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.6).
narrative_ontology:measurement_basis(temp_be_t1500, projected).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement_basis(temp_be_t1900, projected).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(temp_su_t0, projected).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 100, 0.38).
narrative_ontology:measurement_basis(temp_su_t100, projected).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.46).
narrative_ontology:measurement_basis(temp_su_t500, projected).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.52).
narrative_ontology:measurement_basis(temp_su_t1000, projected).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement_basis(temp_su_t1500, projected).
narrative_ontology:measurement(temp_su_t1900, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1900, 0.58).
narrative_ontology:measurement_basis(temp_su_t1900, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This story is one of four constraints in the temple_sacrifice_commitment kernel family, each authoring the same underlying commitment (the biblical sacrificial commandment after Temple destruction) as read differently by different halakhic/theological positions. symbolic_transformation (this story) claims the commandment's mode of fulfillment has been authoritatively changed, closing the question. study_as_exercise claims study itself directly performs the command without any transformation claim. performance_only claims only material sacrifice ever occupies the command and all else is archival. hybrid_preparatory claims the current period is suspended-but-preparatory, neither transformed nor archived, oriented toward restoration. Each reading carries a distinct epsilon: this reading's epsilon (0.62) is comparatively elevated because it alone claims institutional authority to redefine the command's mode, which is the specific structural feature that generates asymmetric extraction against restorationist stakeholders. All four are linked bidirectionally via affects_constraints as members of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
