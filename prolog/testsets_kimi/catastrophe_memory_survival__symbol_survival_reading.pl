% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Form Preservation as Symbolic Survival (Symbol Survival Reading)
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint instantiates the symbol_survival_reading of the
 *   catastrophe_memory_survival kernel: rabbinic authority maintains that
 *   Jewish survival through historical catastrophe is constituted by the
 *   continuity of rabbinically governed ritual practice itself. The
 *   constraint coordinates a practicing community around identity-preserving
 *   ritual while extracting interpretive control and legitimacy costs from
 *   secularized Jews who lose transmission. It is claimed as tangled_rope
 *   because the coordination function (identity preservation) is real but
 *   asymmetrically coupled to rabbinic authority's extraction of interpretive
 *   control.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda-setter and primary beneficiary (institutional/global) â maintains interpretive control over ritual form
 *   - practicing_community: Coordinated beneficiary (organized/national) â receives identity continuity through enacted ritual
 *   - secularized_jews: Primary payer/victim (moderate/national) â bears cost of transmission loss and delegitimation
 *   - alternative_memory_keepers: Excluded voices (moderate/national) â argue for non-rabbinic transmission channels
 *   - memory_studies_scholars: Analytical observer (analytical/global) â documents the structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.65).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Form Preservation as Symbolic Survival (Symbol Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '873904da-b739-459e-a92c-29216362caad').
narrative_ontology:cs_kernel_codification('873904da-b739-459e-a92c-29216362caad', fixed_text).
narrative_ontology:cs_authority_grounding('873904da-b739-459e-a92c-29216362caad', lineage).
narrative_ontology:cs_interpretation_layer_present('873904da-b739-459e-a92c-29216362caad').
narrative_ontology:cs_reading_relation('873904da-b739-459e-a92c-29216362caad', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('873904da-b739-459e-a92c-29216362caad', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('873904da-b739-459e-a92c-29216362caad', foundational, ritual_continuity_constitutes_collective_survival).
narrative_ontology:cs_axiom_status(ritual_continuity_constitutes_collective_survival, holdable).
narrative_ontology:cs_axiom_grounding('873904da-b739-459e-a92c-29216362caad', ritual_continuity_constitutes_collective_survival, conventional).
narrative_ontology:cs_axiom('873904da-b739-459e-a92c-29216362caad', foundational, rabbinic_interpretive_supremacy_over_practice).
narrative_ontology:cs_axiom_status(rabbinic_interpretive_supremacy_over_practice, holdable).
narrative_ontology:cs_axiom_grounding('873904da-b739-459e-a92c-29216362caad', rabbinic_interpretive_supremacy_over_practice, conventional).
narrative_ontology:cs_reference_frame('873904da-b739-459e-a92c-29216362caad', rabbinic_ritual_continuity_norm).
narrative_ontology:cs_drift_state('873904da-b739-459e-a92c-29216362caad', contemporary_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('873904da-b739-459e-a92c-29216362caad', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, practicing_community).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, ritual_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__symbol_survival_reading, symbolic_survival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines correct ritual form, adjudicates boundary norms, and claims that Jewish survival through catastrophe is constituted by continuity of rabbinically governed practice. Derives institutional legitimacy from being the guardian of this continuity and enforces conformity through interpretive gatekeeping.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Participates in ritual as the embodied enactment of collective identity. Receives group cohesion, intergenerational continuity, and belonging, but their Jewishness is legible primarily through rabbinically validated practice. Exit means severing a core identity anchor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, practicing_community, beneficiary,
    organized, generational, identity_locked, national).

% Do not practice rabbinically prescribed ritual or have abandoned it. Bear the cost of transmission failure: their Jewish identity is not recognized as fully legitimate by rabbinic gatekeepers, and they cannot transmit a rabbinically validated continuity to descendants. Remain identity-locked to a community that structurally devalues their mode of existence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, identity_locked, national).

% Secular Jewish educators, cultural organizers, and feminist liturgists who argue for non-rabbinic transmission channels. They are structurally excluded from the ritual-authority conversation and their alternatives are delegitimized as inauthentic.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, alternative_memory_keepers, excluded,
    moderate, biographical, mobile, national).

% Study ritual and memory from outside the rabbinic authority framework. They observe the structural asymmetry between rabbinic control and secularized Jews' transmission losses without being bound by the constraint's identity demands.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, memory_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves collective identity and boundary-norms through symbolic ritual experience, enabling group continuity across generations by embedding membership in enacted practice.
% TRANSFER_FUNCTION: Moves interpretive control and legitimacy of identity-definition from the diffuse community to rabbinic authority; moves the costs of non-practice â exclusion, loss of transmission, and identity erasure â onto secularized or non-conforming Jews.
% ABSENT_VOICES: Secular Jewish educators, feminist liturgists, and alternative memory-keepers who would argue for non-rabbinic transmission channels are excluded from the ritual-authority conversation.
% DISAPPEARANCE_RATIONALE: If rabbinic-controlled ritual form vanished as the gate of Jewish continuity, communal boundaries would destabilize, rabbinic authority would lose its primary legitimacy mechanism, and secularized Jews would no longer bear the specific cost of transmission failure tied to ritual non-observance.
% FOUNDING_PROBLEM: Catastrophic dispersal and memory-loss threatened collective identity; ritual practice was constructed as a portable, embodied memory-system that could survive without state or territory.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and trauma studies scholars attest the dispersal and catastrophe problem from outside the beneficiary set; secular Jewish intellectuals and demographers contest that ritual form preservation is the currently necessary response, arguing that alternative memory systems exist.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because ritual form preservation is actively enforced by rabbinic gatekeeping that denies legitimacy to non-conforming practice; suppression (0.65) reflects both communal sanction and internalized identity lock; theater_ratio (0.45) captures the growing performative component of ritual when its practical survival content is thin; accessibility_collapse (0.60) indicates that while secular alternatives exist, they are structurally devalued within the rabbinic framework; resistance (0.55) measures active contestation by secularized Jews and alternative memory-keepers. The temporal series show extraction and theater rising together as secularization pressures the system to enforce form more strictly.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is a rope: genuine coordination preserving a people against annihilation. From the secularized Jew seat, it is a snare: an enforced interpretive monopoly that extracts legitimacy and transmits costs to those outside the ritual boundary. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits near the beneficiary pole (low d): the constraint subsidizes their interpretive monopoly. Practicing community sits slightly beneficiary-ward of center: they receive coordination but pay in autonomy. Secularized Jews sit near the full-target end (high d): the constraint extracts from them by defining their identity as failed or incomplete. Alternative memory keepers are excluded, receiving no directionality calculation because they are outside the constraint's operative scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â catastrophic dispersal threatening memory â is contested in status. If the problem is dead (contemporary Jews have multiple memory technologies), the constraint risks mandatrophy. However, because a practicing community still genuinely coordinates through ritual, the constraint is not yet a piton; it is a tangled rope where the coordination strand sustains the extraction strand.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbol_survival_vs_practical_encoding,
    'Does ritual primarily encode practical survival competence, or is its symbolic/identity function independent of practical content?',
    'Archaeological and ethnographic analysis of ritual content for practical versus purely symbolic encoding, plus comparative survival outcomes across communities with different ritual emphases.',
    'If practical encoding is dominant, this reading''s high epsilon is misattributed and the constraint might compute closer to rope or scaffold; if purely symbolic, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_survival_vs_practical_encoding, conceptual, 'Competing reading about whether ritual is symbolic or practical survival encoding.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement of ritual form structural (communal sanctions, exclusion) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory observation: if secularized Jews who leave the ritual system continue to enforce rabbinic norms on themselves or experience self-delegitimation, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, amplifying extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in communal ritual enforcement.').

omega_variable(
    rabbinic_authority_benefit_concentration,
    'Does the coordination benefit of ritual continuity accrue primarily to the practicing community, or is the benefit asymmetrically captured by rabbinic authority?',
    'Comparative analysis of power, resource, and legitimacy flows within the community, measuring whether interpretive control translates into material or status extraction.',
    'Would shift the balance between rope and tangled rope: diffuse community benefit pulls toward rope, concentrated rabbinic capture pulls toward tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_benefit_concentration, empirical, 'Whether rabbinic authority extracts disproportionate benefit from ritual continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_survival kernel, decomposed per the epsilon-invariance principle because the sibling readings instantiate structurally distinct claims with different epsilon profiles and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
