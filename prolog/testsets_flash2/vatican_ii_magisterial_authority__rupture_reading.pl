% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models the 'rupture reading' of Vatican II, which
 *   asserts a fundamental break with pre-conciliar Catholic teaching. In this
 *   interpretation, the Council's texts encode a new ecclesiology
 *   incompatible with prior doctrines, authorizing radical implementation and
 *   superseding previous positions (e.g., on religious freedom or the rights
 *   of error). This reading is actively enforced by progressive elements
 *   within the Church, leading to significant extraction from traditionalist
 *   and conservative factions who experience marginalization and suppression.
 *   The claimed type is 'tangled_rope' because it offers a coordination
 *   function (adapting the Church to modernity) but with clear asymmetric
 *   extraction and active enforcement against dissenters.
 *
 * KEY AGENTS:
 *   - progressive_theologians: Primary beneficiary (organized/mobile) — legitimizes their work
 *   - liberal_clergy: Agenda-setter (institutional/constrained) — actively implements the reading
 *   - traditionalist_catholics: Primary target (powerless/identity_locked) — bears significant extraction
 *   - conservative_clergy: Payer (moderate/constrained) — forced to comply despite disagreement
 *   - magisterial_authority: Agenda-setter (institutional/constrained) — enforces the new ecclesiology
 *   - historical_theologians: Observer (analytical/analytical) — analyzes the contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.65).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'd78f5a7d-b12e-4049-9a88-ee75ae2ace13').
narrative_ontology:cs_kernel_codification('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', fixed_text).
narrative_ontology:cs_authority_grounding('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', lineage).
narrative_ontology:cs_interpretation_layer_present('d78f5a7d-b12e-4049-9a88-ee75ae2ace13').
narrative_ontology:cs_reading_relation('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', foundational, doctrinal_progress_through_supersession).
narrative_ontology:cs_axiom_status(doctrinal_progress_through_supersession, holdable).
narrative_ontology:cs_axiom_grounding('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', doctrinal_progress_through_supersession, deontological).
narrative_ontology:cs_axiom('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', secondary, religious_freedom_contradicts_prior_teaching).
narrative_ontology:cs_axiom_status(religious_freedom_contradicts_prior_teaching, holdable).
narrative_ontology:cs_axiom_grounding('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', religious_freedom_contradicts_prior_teaching, empirically_contingent).
narrative_ontology:cs_reference_frame('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', post_conciliar_adaptive_church).
narrative_ontology:cs_drift_state('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d78f5a7d-b12e-4049-9a88-ee75ae2ace13', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liberal_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, conservative_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the rupture reading as it legitimizes their theological innovations and pastoral practices, allowing for greater adaptation to modern thought and culture. They see the Council as a liberation from rigid pre-conciliar structures.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_theologians, beneficiary,
    organized, biographical, mobile, global).

% Actively implement and enforce the rupture reading in their dioceses and parishes, promoting liturgical changes, ecumenical initiatives, and a more inclusive ecclesiology. They view themselves as fulfilling the true spirit of the Council.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liberal_clergy, agenda_setter,
    institutional, biographical, constrained, global).

% Bear the costs of the rupture reading, experiencing a loss of traditional liturgy, doctrine, and spiritual practices. They feel alienated and marginalized, often facing ecclesiastical sanctions for resisting changes they perceive as illegitimate. Their identity is deeply tied to pre-conciliar forms.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_catholics, payer,
    powerless, generational, identity_locked, global).

% Are forced to implement policies and teachings derived from the rupture reading, even if they disagree with its interpretation. Their careers and positions depend on obedience to hierarchical authority, limiting their ability to openly resist without risking their livelihood.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, conservative_clergy, payer,
    moderate, biographical, constrained, global).

% The formal teaching authority of the Church, which, in this reading, has implicitly or explicitly endorsed the rupture interpretation through subsequent documents and appointments. It enforces the new ecclesiology, often by suppressing dissent from traditionalist quarters.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, magisterial_authority, agenda_setter,
    institutional, civilizational, constrained, universal).

% Analyze the historical development of doctrine and the Council's texts, often from an academic perspective. They observe the contestation and the various interpretations without necessarily endorsing one, though their scholarship can influence the debate.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, historical_theologians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Church's adaptation to the modern world, fostering ecumenical dialogue, religious freedom, and a more collegial understanding of authority, thereby preventing further alienation from contemporary society.
% TRANSFER_FUNCTION: Transfers doctrinal and liturgical authority from a rigid, pre-conciliar framework to a more adaptable, 'living tradition' interpretation, empowering progressive elements within the Church while marginalizing traditionalists.
% ABSENT_VOICES: The voices of pre-conciliar theologians and popes, whose teachings are deemed superseded by the rupture reading, are absent from the contemporary magisterial discourse, except as historical counterpoints to be overcome. Their arguments for the immutability of certain doctrines are effectively silenced.
% DISAPPEARANCE_RATIONALE: If the rupture reading of Vatican II vanished, the Church would face an immediate crisis of identity and authority. Liturgical practices would revert, ecumenical efforts would cease, and the entire post-conciliar theological landscape would collapse, forcing a radical re-evaluation of the Church's relationship with modernity.
% FOUNDING_PROBLEM: The Church faced increasing irrelevance and alienation from a rapidly modernizing world, perceived as rigid, triumphalist, and out of touch, leading to a crisis of faith and engagement among many Catholics.
% FOUNDING_PROBLEM_CORROBORATION: Progressive theologians and many lay Catholics attest that the problem of the Church's relevance to the modern world remains live, justifying ongoing adaptation. Traditionalists and some conservative clergy, however, argue that the 'rupture' itself exacerbated the crisis, creating new problems rather than solving the original one; independent sociological studies of religious practice offer mixed corroboration, showing both adaptation and decline.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because traditionalist elements are forced to abandon deeply held beliefs and practices, or face marginalization. Suppression (0.70) is also high, as the rupture reading is actively enforced through ecclesiastical appointments, liturgical regulations, and disciplinary actions against dissenters. The theater ratio (0.20) is relatively low, as the implementation of the rupture reading is a genuine, active process, not merely performative. Accessibility collapse (0.40) is moderate, as traditionalists have limited, often illicit, alternatives (e.g., independent chapels). Resistance (0.75) is high, reflecting the ongoing, vocal opposition from traditionalist and conservative groups.
 *
 * PERSPECTIVAL GAP:
 *   Progressive theologians and liberal clergy experience this as a necessary and beneficial adaptation (low extraction, high coordination). Traditionalist Catholics and conservative clergy experience it as a profound loss and enforced rupture (high extraction, high suppression). The magisterial authority, in this reading, acts as an agenda-setter, enforcing the rupture interpretation, which benefits some while extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive theologians and liberal clergy are beneficiaries, as the rupture reading legitimizes their theological and pastoral approaches. Traditionalist Catholics and conservative clergy are victims, as their adherence to pre-conciliar forms is suppressed. The magisterial authority, by enforcing this reading, acts as an agenda-setter, facilitating the benefits for some while imposing costs on others. Traditionalist Catholics are identity-locked, as their faith is deeply intertwined with the pre-conciliar tradition, making exit from the Church itself a profound crisis.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the rupture reading as pure extraction by acknowledging its genuine coordination function: adapting the Church to modernity. However, it highlights that this adaptation comes at a significant cost to those who adhere to prior teachings, making it a tangled rope rather than a pure rope. The mandate to adapt has been interpreted in a way that creates clear winners and losers, requiring active enforcement to maintain the new equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_continuity_ambiguity,
    'To what extent do the conciliar texts, particularly on religious freedom (Dignitatis Humanae), genuinely contradict prior magisterial teaching, or can they be reconciled through a hermeneutic of continuity?',
    'Comprehensive historical-theological analysis by a neutral, interdisciplinary commission, assessing the logical coherence and historical development of doctrine across the pre- and post-conciliar periods.',
    'If a genuine contradiction is established, the rupture reading''s claim of doctrinal progress through supersession is strengthened, further legitimizing the extraction from traditionalists. If continuity is demonstrated, the rupture reading''s foundation is weakened, potentially reclassifying it towards a piton or even a rope, as the justification for suppression erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_continuity_ambiguity, conceptual, 'Ambiguity regarding the actual doctrinal relationship between Vatican II and prior teaching.').

omega_variable(
    enforcement_legitimacy_ambiguity,
    'Is the active enforcement of the rupture reading by liberal clergy and magisterial authority a legitimate exercise of pastoral governance, or an abuse of power to suppress legitimate dissent?',
    'Independent review of disciplinary actions and liturgical restrictions against traditionalists, assessed against established canonical norms and principles of justice, by a body outside the immediate ecclesiastical hierarchy.',
    'If enforcement is deemed illegitimate, the suppression metric would be re-evaluated as higher and more purely extractive, pushing the classification closer to a snare. If deemed legitimate, the tangled_rope classification is reinforced, acknowledging the coordination function of maintaining a unified (albeit contested) ecclesial vision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_ambiguity, preference, 'Ambiguity regarding the legitimacy of enforcing the rupture reading.').

omega_variable(
    identity_lock_mechanism,
    'Is the ''identity_locked'' exit option for traditionalist Catholics primarily due to their deep theological convictions, or is it reinforced by social isolation and lack of viable alternative communities?',
    'Sociological studies of traditionalist communities, examining the role of social networks, access to information, and the availability of alternative Catholic expressions (e.g., Eastern Rites, independent chapels) in shaping their exit calculus.',
    'If social isolation is a primary factor, the effective suppression is higher than structural measures suggest, as the constraint leverages social mechanisms to bind individuals. If theological conviction is dominant, the identity lock is more internal, making external ''fixes'' less effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized identity lock for traditionalist Catholics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Vatican II magisterial authority' kernel. This 'rupture reading' asserts a fundamental break with pre-conciliar teaching, encoding a new ecclesiology incompatible with prior doctrine. It is linked to the 'continuity reading' and 'composite overdetermination reading' as competing interpretations of the same conciliar texts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
