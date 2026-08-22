% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Catastrophe-Memory Ritual as Symbolic Continuity and Identity Marker
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the symbol-continuity reading of the
 *   catastrophe-memory kernel: the mourning-practice is read as an
 *   identity-marker whose function is to keep symbolic reference to a
 *   founding catastrophe alive across dispersed generations, independent of
 *   any operational survival benefit. The beneficiary of this reading is not
 *   a person but tradition-continuity itself, administered by communal
 *   custodians; the victim is adaptive modification — anyone who would update
 *   the form to reduce hardship or fit changed circumstances pays a social
 *   cost for proposing change, and mixed-status descendants pay for not
 *   fitting the fixed form at all. Extraction is low: the practice does not
 *   primarily move resources or labor to an extracting party, it moves
 *   flexibility away from individuals toward a fixed symbolic standard
 *   maintained by custodial authority. This is deliberately narrower than the
 *   sibling readings — it says nothing about survival competence, trauma
 *   encoding, or boundary enforcement against outsiders, all of which are
 *   separate constraints over the same underlying ritual practice.
 *
 * KEY AGENTS:
 *   - communal_identity_custodians: agenda_setter/beneficiary (institutional/identity_locked) — administers the fixed symbolic form and collects the authority of custodianship
 *   - tradition_continuity_itself: non-agent beneficiary — the abstract good the practice is performed to sustain
 *   - observant_community_members: beneficiary/payer (organized/identity_locked) — gain belonging, bear recurring ritual cost
 *   - adaptive_modification_advocates: payer (moderate/constrained) — pay in social friction for proposing updates to the form
 *   - intermarried_and_mixed_status_descendants: payer (powerless/trapped) — pay for not fitting the rigid symbolic form
 *   - descendant_children_being_initiated: excluded (powerless/trapped) — initiated without consent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Catastrophe-Memory Ritual as Symbolic Continuity and Identity Marker").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '59bf6115-fb96-46c9-9a22-3f7ef5918118').
narrative_ontology:cs_kernel_codification('59bf6115-fb96-46c9-9a22-3f7ef5918118', distributed).
narrative_ontology:cs_authority_grounding('59bf6115-fb96-46c9-9a22-3f7ef5918118', practice).
narrative_ontology:cs_interpretation_layer_present('59bf6115-fb96-46c9-9a22-3f7ef5918118').
narrative_ontology:cs_reading_relation('59bf6115-fb96-46c9-9a22-3f7ef5918118', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('59bf6115-fb96-46c9-9a22-3f7ef5918118', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('59bf6115-fb96-46c9-9a22-3f7ef5918118', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('59bf6115-fb96-46c9-9a22-3f7ef5918118', foundational, symbolic_form_is_the_identity_not_merely_its_expression).
narrative_ontology:cs_axiom_status(symbolic_form_is_the_identity_not_merely_its_expression, holdable).
narrative_ontology:cs_axiom_grounding('59bf6115-fb96-46c9-9a22-3f7ef5918118', symbolic_form_is_the_identity_not_merely_its_expression, conventional).
narrative_ontology:cs_axiom('59bf6115-fb96-46c9-9a22-3f7ef5918118', secondary, unbroken_performance_constitutes_continuity_regardless_of_documented_variation).
narrative_ontology:cs_axiom_status(unbroken_performance_constitutes_continuity_regardless_of_documented_variation, holdable).
narrative_ontology:cs_axiom_grounding('59bf6115-fb96-46c9-9a22-3f7ef5918118', unbroken_performance_constitutes_continuity_regardless_of_documented_variation, conventional).
narrative_ontology:cs_reference_frame('59bf6115-fb96-46c9-9a22-3f7ef5918118', premodern_diaspora_transmission_norm).
narrative_ontology:cs_drift_state('59bf6115-fb96-46c9-9a22-3f7ef5918118', contemporary_assimilation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59bf6115-fb96-46c9-9a22-3f7ef5918118', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_itself).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, communal_identity_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, intermarried_and_mixed_status_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, observant_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, observant_community_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, unbroken_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic, communal, and lay authorities set the calendar and liturgical form of the mourning-practice, determine what counts as faithful observance, and correct deviations. Their standing derives from being seen to carry the practice forward unbroken; they do not extract money or labor so much as collect the social and religious authority that comes from custodianship of continuity itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, communal_identity_custodians, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, communal_identity_custodians, beneficiary).

% Not an actor but the thing the practice serves: an unbroken chain of symbolic performance linking present participants to a distant catastrophe and to each other across dispersed communities. It 'benefits' in the sense that every faithful performance of the ritual is itself the continuity — the practice is both the means and the good it produces, with no further payoff required to justify it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_itself, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_itself).

% Perform the mourning-practice each cycle, receiving a felt sense of belonging, historical rootedness, and legible group membership. They also bear the recurring cost in time, emotional labor, and constrained behavior (fasting, restricted activity, mandated posture of grief) that the practice requires, and cannot easily opt out without visible social cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, observant_community_members, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, observant_community_members, payer).

% Members and sub-communities who propose updating the ritual's form to fit changed circumstances — shorter observance, altered symbolism, integration with contemporary mourning idioms. Their proposals are treated as threats to continuity rather than adaptations; they pay in social friction, accusations of dilution, and exclusion from interpretive authority, even when their changes would reduce hardship without abandoning the symbolic core.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates, payer,
    moderate, biographical, constrained, regional).

% Descendants whose family lines or affiliations sit ambiguously relative to the community's continuity claim. The rigidity of the symbolic form gives them no flexible entry point — they either perform an inherited identity that does not fully fit them or are read as having broken the chain, with no practice-sanctioned way to hold a modified or partial relationship to it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, intermarried_and_mixed_status_descendants, payer,
    powerless, generational, trapped, regional).

% Children raised into the mourning-practice before they can consent to or evaluate its meaning for them. They would have views about the form and weight of the observance if consulted, but the transmission logic of the ritual treats early, non-consensual initiation as the mechanism of continuity itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, descendant_children_being_initiated, excluded,
    powerless, biographical, trapped, local).

% Study the mourning-practice comparatively across diaspora communities, tracking how symbolic form has and has not changed, and assessing claims of unbroken transmission against the historical record of documented liturgical variation and reform.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, comparative_ritual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, communal_identity_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves a genuine coordination problem: how a dispersed community, without a shared territory or centralized enforcement, keeps a shared symbolic reference to a foundational catastrophe alive across generations so that members recognize each other as belonging to the same continuous group.
% TRANSFER_FUNCTION: The arrangement moves flexibility and adaptive discretion away from individual members and toward the fixed symbolic form: those who would modify the practice to reduce hardship or better fit changed circumstances absorb the cost of rigidity, while the abstract good of 'unbroken continuity' and the custodial authorities who administer it collect the social credit of having preserved it.
% ABSENT_VOICES: Children initiated before consent, and descendants of mixed or ambiguous status, would likely argue for a more graduated or optional relationship to the practice; they are structurally present in the ritual (it is performed on and around them) but absent from the interpretive body that decides its form.
% DISAPPEARANCE_RATIONALE: If the mourning-practice vanished overnight, the dispersed community would lose its main recurring occasion for collective self-recognition across distance and generation; custodial authorities would lose their principal source of standing; adaptive-modification advocates would gain freedom to propose alternative forms without being cast as threats to continuity; and mixed-status descendants would face a different, possibly easier, question of what belonging requires.
% FOUNDING_PROBLEM: A dispersed, historically catastrophized community needed a way to keep its shared identity and its memory of the catastrophe alive without a state, a shared territory, or continuous face-to-face contact to enforce cohesion.
% FOUNDING_PROBLEM_CORROBORATION: Communal identity custodians and observant members attest the founding problem remains live — dispersion and assimilation pressure persist. Comparative ritual scholars, working from documented liturgical variation across diaspora communities, attest that the specific symbolic form has in fact changed substantially over centuries even while being described internally as unbroken, and that the 'unbroken transmission' claim is itself a retrospectively stabilized narrative rather than a literal continuity — this corroboration comes from outside the custodial and beneficiary seats.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 by interval end) because, under this reading, the practice does not move material resources to a concentrated extracting party — its 'payoff' is the symbolic good of continuity itself, collected diffusely by custodians and the tradition as such. Suppression and theater ratio are both moderate and rising: as the practice ages, an increasing share of its maintenance is performative assertion of unbrokenness (theater_ratio climbing from 0.25 to 0.42) rather than functional transmission, and mild active correction of deviation (suppression_requirement climbing from 0.22 to 0.35) is required to hold the fixed form against drift. Accessibility_collapse (0.5) and resistance (0.4) are mid-range, consistent with a constraint that is neither a natural law nor fully coercive: alternatives to the fixed ritual form exist and are proposed (by adaptive_modification_advocates) but are socially costly to adopt, not physically barred.
 *
 * PERSPECTIVAL GAP:
 *   From the custodial seat, the fixed form's persistence is itself proof of successful transmission and worth defending. From the adaptive_modification_advocate seat, the same rigidity is an arbitrary cost imposed by people who benefit from being seen as guardians of continuity. The engine should compute these as structurally different experiences of the same authored data, not as a dispute to be adjudicated in the commentary.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity custodians sit near the beneficiary end: they administer the form and collect the standing that comes from being seen as its faithful keepers, with identity-locked exit (their authority IS their custodianship). Adaptive_modification_advocates and mixed-status descendants sit toward the target end: the former pay social costs for proposing change, the latter pay for structurally not fitting a form they had no say in designing. Observant members are genuinely mixed — real belonging benefit, real recurring cost — which is why they carry both beneficiary and payer roles rather than being forced into one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining shared identity across a dispersed community without central enforcement) is contested rather than cleanly dead or live: dispersion pressure is real and ongoing, which argues the problem persists, but comparative-ritual scholarship shows the specific symbolic form claimed as 'unbroken' has in fact varied substantially over time, which argues the unbroken-transmission story is itself a retrospective stabilization rather than a literal continuity. This is exactly the kind of divergence the classification should surface rather than resolve by fiat: the practice is not mislabeled as pure extraction (there is a real coordination function — shared identity across distance) nor is it mislabeled as costless coordination (adaptive_modification_advocates and mixed-status descendants bear real, identifiable costs from the form's rigidity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_claim_vs_documented_variation,
    'Is the ''unbroken transmission'' the symbol-continuity reading rests on a structurally accurate description, or a retrospectively constructed narrative papering over documented liturgical change?',
    'Comparative historical-liturgical analysis across diaspora communities, tracing documented changes in ritual form, timing, and symbolism against the community''s own claims of unbrokenness.',
    'If the continuity claim is substantially constructed, the beneficiary ''tradition_continuity_itself'' is closer to a legitimating fiction that custodial authorities use to justify present-day control over ritual form, which would push this reading toward a higher-extraction classification than currently authored (0.28).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_vs_documented_variation, empirical, 'Whether claimed unbroken transmission survives comparative historical scrutiny.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the symbol_continuity_reading''s boundary sit relative to the trauma_encoding_reading and survival_competence_reading — is symbolic identity-marking genuinely separable from trauma-transmission and adaptive-competence functions, or are these three readings describing entangled aspects of one indivisible practice that has been split for analytical convenience?',
    'Ethnographic and psychological research distinguishing what participants report the practice is FOR (identity vs. warning vs. skill-transmission) across different sub-communities and generations, checked against whether removing one claimed function (e.g. trauma-warning) changes participation in ways the other claims would not predict.',
    'If the functions are genuinely entangled rather than separable, the four sibling readings may need to be understood as co-occurring facets of a single higher-extraction constraint rather than four independently ε-stable readings — this would not change THIS story''s authored ε, but would bear on how the kernel family''s network edges should be interpreted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the symbol-continuity function is structurally separable from sibling readings or an analytical decomposition of one entangled practice.').

omega_variable(
    custodial_capture_of_continuity_good,
    'Does the abstract good ''tradition_continuity_itself'' actually accrue diffusely to the whole community, or is it substantially captured by communal_identity_custodians as concentrated social authority, making this closer to a tangled_rope with a hidden concentrated beneficiary than the low-extraction rope-like reading suggests?',
    'Track how custodial authority (appointment to ritual leadership roles, deference in communal disputes, resource allocation within the community) correlates with demonstrated fidelity to the fixed ritual form over time.',
    'If custodial capture is substantial, the tangled_rope classification is well-grounded and the beneficiary structure should be read as concentrated (custodians) rather than diffuse (tradition itself); if capture is minimal, the arrangement is closer to a genuine low-extraction rope with incidental custodial status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_capture_of_continuity_good, empirical, 'Whether custodial authorities meaningfully capture the social value of the continuity claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.31).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 80, 0.33).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the catastrophe_memory_kernel, each authored as an independently ε-stable constraint per the ε-invariance principle. symbol_continuity_reading isolates the identity-marking function (low extraction, custodial-authority beneficiary, adaptive-modification victim). survival_competence_reading would isolate a claimed operational survival-skill transfer function; trauma_encoding_reading would isolate a warning-system/psychological-transmission function; boundary_maintenance_reading would isolate an exclusionary boundary-enforcement function against out-group members. Each sibling should carry its own beneficiary/victim structure and its own ε rather than sharing this story's values — do not average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
