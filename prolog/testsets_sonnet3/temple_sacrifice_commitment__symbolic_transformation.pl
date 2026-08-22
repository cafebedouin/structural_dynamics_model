% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Sacrifice Commitment Reading: Authorized Symbolic Transformation (Prayer/Study as New Instantiation)
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This story instantiates the symbolic_transformation reading of the
 *   temple_sacrifice_commitment kernel: the claim that the destruction of the
 *   Temple did not suspend the sacrificial commandment but transformed its
 *   instantiation, so that fixed prayer and continued study of sacrificial
 *   law now fully occupy the commitment. This is structurally distinct from
 *   the sibling readings — study_as_exercise treats study itself as the
 *   performance (a narrower epistemic claim), performance_only denies any
 *   substitute is possible and treats current practice as archival, and
 *   hybrid_preparatory treats study as a holding pattern rather than
 *   completion. The transformation reading is the one that claims the most:
 *   not preservation, not exercise, not waiting, but authorized replacement.
 *   That is exactly why its extraction profile is highest among the four —
 *   the claim that an authority structure can redefine the content of a
 *   divine command, and that doing so is not innovation but continuity,
 *   concentrates interpretive power in the rabbinic class in a way the other
 *   readings do not attempt.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure: agenda_setter (institutional/arbitrage) — declares and administers the transformation doctrine
 *   - diaspora_congregational_leadership: beneficiary (organized/constrained) — institutional life built on the doctrine's correctness
 *   - ordinary_observant_practitioners: beneficiary/payer (moderate/constrained) — receive a completable obligation, absorb foreclosed inquiry
 *   - restorationist_minorities: payer (powerless/trapped) — hold performance as non-negotiable, marginalized for it
 *   - temple_mount_activists: payer (powerless/trapped) — bear direct social and legal costs for pursuing restoration
 *   - halakhic_historians: observer (analytical) — document the doctrine's post-destruction origin
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.58).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.42).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Sacrifice Commitment Reading: Authorized Symbolic Transformation (Prayer/Study as New Instantiation)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'adf27bf6-4608-4ee9-bd10-22f0e286a828').
narrative_ontology:cs_kernel_codification('adf27bf6-4608-4ee9-bd10-22f0e286a828', fixed_text).
narrative_ontology:cs_authority_grounding('adf27bf6-4608-4ee9-bd10-22f0e286a828', lineage).
narrative_ontology:cs_interpretation_layer_present('adf27bf6-4608-4ee9-bd10-22f0e286a828').
narrative_ontology:cs_reading_relation('adf27bf6-4608-4ee9-bd10-22f0e286a828', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('adf27bf6-4608-4ee9-bd10-22f0e286a828', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('adf27bf6-4608-4ee9-bd10-22f0e286a828', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('adf27bf6-4608-4ee9-bd10-22f0e286a828', foundational, rabbinic_authority_can_reconstitute_mode_of_commandment).
narrative_ontology:cs_axiom_status(rabbinic_authority_can_reconstitute_mode_of_commandment, holdable).
narrative_ontology:cs_axiom_grounding('adf27bf6-4608-4ee9-bd10-22f0e286a828', rabbinic_authority_can_reconstitute_mode_of_commandment, conventional).
narrative_ontology:cs_axiom('adf27bf6-4608-4ee9-bd10-22f0e286a828', foundational, prayer_and_study_fully_discharge_sacrificial_obligation).
narrative_ontology:cs_axiom_status(prayer_and_study_fully_discharge_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('adf27bf6-4608-4ee9-bd10-22f0e286a828', prayer_and_study_fully_discharge_sacrificial_obligation, conventional).
narrative_ontology:cs_axiom('adf27bf6-4608-4ee9-bd10-22f0e286a828', secondary, material_performance_remains_strictly_required).
narrative_ontology:cs_axiom_status(material_performance_remains_strictly_required, overridden).
narrative_ontology:cs_axiom_grounding('adf27bf6-4608-4ee9-bd10-22f0e286a828', material_performance_remains_strictly_required, deontological).
narrative_ontology:cs_reference_frame('adf27bf6-4608-4ee9-bd10-22f0e286a828', sacrificial_cult_as_literal_ongoing_obligation).
narrative_ontology:cs_drift_state('adf27bf6-4608-4ee9-bd10-22f0e286a828', post_destruction_rabbinic_consolidation, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('adf27bf6-4608-4ee9-bd10-22f0e286a828', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_congregational_leadership).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, liturgical_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, restorationist_minorities).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, temple_mount_activists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, ordinary_observant_practitioners).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, ordinary_observant_practitioners).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_to_reconstitute_divine_command).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, prayer_as_valid_successor_instantiation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares that the destruction of the Temple did not suspend the sacrificial commitment but transformed it: the fixed liturgy of prayer (tefillah) and continued study of sacrificial law now constitute the authorized instantiation of the command. This declaration is the load-bearing act that keeps rabbinic Judaism functioning as a continuous legal system rather than one whose central commitment went dark. The authority that makes this declaration is the same authority that benefits from being recognized as competent to make it — its own jurisdiction over the tradition is what the transformation claim vindicates.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority_structure, beneficiary).

% Runs synagogue life organized entirely around the prayer-as-substitute framework: three daily services corresponding to the historical sacrificial offerings, liturgical calendars keyed to Temple ritual. Their institutional viability depends on the transformation reading being correct; if performance were held mandatory and unperformed, their entire operational structure would be an admission of unresolved defect rather than a completed observance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_congregational_leadership, beneficiary,
    organized, generational, constrained, global).

% Fulfill what they are told is a complete religious obligation through prayer and study, receiving spiritual and communal benefit and freedom from a genuinely impossible material demand (there is no standing Temple, no priesthood in active service, no altar). They also bear a cost: they are formed within a framework that forecloses examining whether their observance is actually incomplete, and they inherit no live pressure to seek restoration.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, ordinary_observant_practitioners, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, ordinary_observant_practitioners, payer).

% Hold that sacrifice is a material commandment that cannot be transformed by rabbinic decree, only suspended by circumstance and reactivated by restoration of the Temple and priesthood. They are treated within mainstream institutions as fringe, sometimes destabilizing, and are structurally marginalized from liturgical and educational authority precisely because their position denies the legitimacy of the transformation claim that those institutions depend on.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, restorationist_minorities, payer,
    powerless, generational, trapped, regional).

% Actively work toward conditions for renewed sacrificial practice (priestly genealogy preservation, ritual object reconstruction, political advocacy for access). They bear direct costs — social ostracism from mainstream institutions, legal and political risk, arrest in some jurisdictions — precisely because their activity is read by the authority structure as an implicit rejection of the transformation doctrine's completeness.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, temple_mount_activists, payer,
    powerless, biographical, trapped, regional).

% Study the textual and historical record of how the transformation doctrine was formulated (post-70 CE rabbinic sources, Yohanan ben Zakkai's reforms, the liturgical fixing under later authorities) without a stake in which reading is theologically correct. They document that the doctrine was itself a rabbinic innovation responding to catastrophe, not a pre-existing provision within the original commandment.
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
% COORDINATION_FUNCTION: Provides religious continuity for a dispersed population that lost the physical infrastructure (Temple, priesthood, altar) required for its central commanded practice — without this reading, communal religious life would have to either treat the core commandment as permanently unfulfillable or fracture around competing claims of what now counts as observance.
% TRANSFER_FUNCTION: Moves interpretive authority from a now-defunct priestly/Temple apparatus to the rabbinic scholarly class, and moves the locus of religious legitimacy from material performance to textual/liturgical practice the rabbinic class controls and teaches.
% ABSENT_VOICES: Restorationist minorities and Temple Mount activists hold that the transformation was never authorized by the original command and dispute the rabbinic authority's competence to make that substitution; they are present in the tradition's margins but excluded from setting mainstream liturgical or educational policy, and their objection is treated as fringe rather than adjudicated.
% DISAPPEARANCE_RATIONALE: If the symbolic-transformation doctrine were withdrawn, mainstream diaspora religious practice would lose its claimed completeness — communities would either have to reconstruct a performance-only framework (declaring centuries of observance materially deficient) or shift toward the study-as-exercise or hybrid-preparatory readings; institutional liturgy, rabbinic authority claims, and lay practice would all require re-justification.
% FOUNDING_PROBLEM: The Second Temple's destruction in 70 CE eliminated the physical site and priestly apparatus required for the sacrificial commandments, threatening to leave the central commanded practice of the tradition permanently unfulfillable and the legal system built around it structurally broken.
% FOUNDING_PROBLEM_CORROBORATION: Halakhic historians, working from primary rabbinic sources outside any stake in doctrinal correctness, corroborate that the transformation doctrine was a post-destruction innovation attributed to figures like Yohanan ben Zakkai and later liturgical authorities, not a provision transmitted as part of the original commandment; restorationist minorities and Temple Mount activists — also outside the rabbinic authority's own benefiting circle — corroborate that the founding problem (absence of a functioning Temple) remains live and unsolved, disputing that it was ever authorized to be resolved by substitution rather than by eventual restoration.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.58 — substantial but not extreme — because the coordination function is genuine (a dispersed population needed a way to continue religious life without a Temple) even though it is bundled with a strong extraction of interpretive authority. Suppression sits at 0.42, moderate: this is not enforced by coercive apparatus but by institutional exclusion of dissenting readings from mainstream liturgical and educational authority, and by the social costs borne by those who insist on the performance-only or restorationist positions. Theater ratio is authored at 0.40 and rising slightly over the interval, because a growing share of the doctrine's maintenance is liturgical performance (fixed prayer times keyed to sacrificial hours) whose function is increasingly commemorative/identity-sustaining rather than doing the work the original coordination problem required. Accessibility collapse is moderate (0.5): once inside the mainstream tradition, alternatives to the transformation reading are practically difficult to access, but they are not eliminated — restorationist and activist positions persist at the margins. Resistance is moderate (0.45), tracking the persistent, if marginalized, restorationist current across nearly two millennia.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic authority structure is the clearest beneficiary: the transformation claim is simultaneously the vindication of its own jurisdiction and the basis of its continued relevance — it is both agenda_setter and beneficiary, close to the full-beneficiary end of directionality. Diaspora congregational leadership and ordinary practitioners are secondary beneficiaries: they receive a workable, complete religious life, though practitioners also carry a diffuse cost in foreclosed inquiry. Restorationist minorities and Temple Mount activists are the clearest targets: their trapped exit options (there is no viable alternative religious-legal home that both preserves the sacrificial commandment as material and offers institutional standing) and marginalization push their directionality toward the full-target end, higher than their raw powerlessness alone would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric mislabelings. First, against reading this purely as extraction (a snare): the transformation doctrine did solve a genuine and civilizationally serious coordination problem — without it, a legal system built around an now-impossible-to-perform central commandment would have faced fracture or dissolution, and the doctrine's coordination function for diaspora communal continuity is real, not merely claimed. Second, against reading this purely as coordination (a rope): the doctrine also transfers real interpretive power to the rabbinic class and imposes real costs — social marginalization, foreclosure of restoration advocacy, exclusion from mainstream authority — on those who hold the original material commandment as non-negotiable. Tangled Rope names both halves without collapsing them into each other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorized_transformation_vs_unauthorized_drift,
    'Did the rabbinic authority structure possess legitimate authority to transform the sacrificial commandment''s mode of fulfillment, or did it unilaterally redefine a divine command it had no standing to redefine?',
    'This is not resolvable by external empirical evidence — it depends on prior commitments about the source and scope of rabbinic authority (Oral Torah transmission claims) that are themselves contested by the sibling readings. Resolution would require settling a prior theological question about the boundaries of interpretive authority, which the tradition itself has never definitively closed.',
    'If unauthorized, this reading is a Snare wearing coordination-function cover: the ''transformation'' is rent-seeking by an interpretive class that expanded its own jurisdiction under cover of necessity. If authorized, the Tangled Rope classification holds as authored, with real coordination alongside real but legitimate concentration of authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authorized_transformation_vs_unauthorized_drift, conceptual, 'Whether the doctrinal transformation was a legitimate exercise of transmitted authority or an unauthorized expansion of rabbinic jurisdiction.').

omega_variable(
    kernel_sibling_disagreement_locus,
    'Where exactly do the four sibling readings of temple_sacrifice_commitment disagree — is it about WHETHER substitution occurred, WHAT COUNTS as occupying the commandment, or WHO has standing to decide?',
    'Textual-historical analysis of each reading''s foundational sources (post-70 CE rabbinic literature, liturgical fixing texts, restorationist halakhic writings) to isolate whether the disagreement is empirical (what the sources actually say), interpretive (how to read what they say), or jurisdictional (who gets to decide the reading).',
    'If the disagreement is purely jurisdictional, all four readings could be internally coherent and the dispute reduces to a contest of authority rather than of fact — supporting coexists_with relations across the whole kernel. If the disagreement is substantive (the sources genuinely support only one reading), the other readings are not equally live and the classification asymmetry (this reading''s higher ε) reflects a real interpretive overreach rather than a mere framing choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_disagreement_locus, conceptual, 'Locating whether the kernel dispute is empirical, interpretive, or jurisdictional in nature.').

omega_variable(
    restorationist_coalition_potential,
    'Could restorationist minorities and Temple Mount activists, both powerless and trapped individually, achieve coalition-level influence sufficient to force the mainstream authority structure to acknowledge their reading as live rather than fringe?',
    'Track political and organizational developments (priestly genealogy registries, political movements advocating Temple access, cross-denominational restorationist alliances) for evidence of coalition formation and its effect on mainstream institutional treatment of the transformation doctrine''s contestedness.',
    'Coalition success would shift restorationist directionality away from the pure-target end and could pressure the mainstream reading toward acknowledging hybrid_preparatory-style hedging rather than full transformation completeness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restorationist_coalition_potential, empirical, 'Whether powerless individual restorationist actors can achieve coalition-level structural leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 200, 0.28).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.32).
narrative_ontology:measurement(temp_tr_t900, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 900, 0.35).
narrative_ontology:measurement(temp_tr_t1300, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1300, 0.37).
narrative_ontology:measurement(temp_tr_t1700, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1700, 0.39).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1950, 0.4).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.46).
narrative_ontology:measurement(temp_be_t900, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 900, 0.5).
narrative_ontology:measurement(temp_be_t1300, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1300, 0.54).
narrative_ontology:measurement(temp_be_t1700, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1700, 0.56).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1950, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 200, 0.46).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.44).
narrative_ontology:measurement(temp_su_t900, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 900, 0.43).
narrative_ontology:measurement(temp_su_t1300, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1300, 0.42).
narrative_ontology:measurement(temp_su_t1700, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1700, 0.42).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1950, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the temple_sacrifice_commitment kernel, each authored as its own ε-invariant constraint per the ε-invariance principle. symbolic_transformation claims the strongest form of substitution (full authorized replacement) and is authored with the highest extractiveness among the four because it concentrates the most interpretive power in the deciding authority. study_as_exercise makes a narrower claim (study alone performs the command); performance_only denies substitution is possible at all and treats current practice as archival preservation of a suspended commandment; hybrid_preparatory treats the interim as neither complete occupation nor mere archiving. All four should be read together as a constraint family; none is the 'correct' background reading against which the others are deviations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
