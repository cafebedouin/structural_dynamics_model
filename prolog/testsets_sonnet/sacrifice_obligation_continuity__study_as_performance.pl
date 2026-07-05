% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of the Commandment
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This story instantiates one reading of the
 *   sacrifice_obligation_continuity kernel: the claim that studying the laws
 *   of sacrifice (Seder Kodashim and related Talmudic material) itself
 *   constitutes fulfillment of the underlying commandment, independent of
 *   physical Temple performance. This is the study_as_performance reading. It
 *   is distinct from three sibling readings covered in separate constraint
 *   stories: performance_only (study is preparation, not fulfillment —
 *   obligation remains substantively unfulfilled), messianic_suspension
 *   (obligation is neither fulfilled nor violated, simply suspended pending
 *   restoration), and archival_preservation (study has no normative force at
 *   all; sacrifice law is dead law preserved as cultural memory). Each
 *   reading has a different beneficiary/victim structure and a different ε:
 *   this reading has essentially no victims (the obligation is satisfied, not
 *   deferred or burdened) and low extractiveness (study is broadly
 *   accessible, requiring no scarce resource or gatekept infrastructure),
 *   whereas performance_only would show unresolved obligation-pressure and
 *   messianic_suspension would show indefinite deferral costs. These are not
 *   the same constraint measured differently — they are structurally distinct
 *   claims about what the sacrificial commandments currently require, and
 *   each gets its own file per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - study_practitioners: Primary beneficiary (moderate/mobile) — gains full discharge of obligation through study
 *   - textual_tradition_scholars: Coordinating beneficiary and agenda-setter (institutional/mobile) — articulates and transmits the doctrine
 *   - temple_restorationists: Excluded competing claimant (moderate/constrained) — holds a rival reading sidelined by this one's dominance
 *   - comparative_religion_observers: Analytical observer (analytical/analytical) — traces the doctrine's historical function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.08).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study of Sacrifice Law as Fulfillment of the Commandment").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '680582ef-3f48-45e3-b62a-8b122071b515').
narrative_ontology:cs_kernel_codification('680582ef-3f48-45e3-b62a-8b122071b515', fixed_text).
narrative_ontology:cs_authority_grounding('680582ef-3f48-45e3-b62a-8b122071b515', lineage).
narrative_ontology:cs_interpretation_layer_present('680582ef-3f48-45e3-b62a-8b122071b515').
narrative_ontology:cs_reading_relation('680582ef-3f48-45e3-b62a-8b122071b515', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('680582ef-3f48-45e3-b62a-8b122071b515', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('680582ef-3f48-45e3-b62a-8b122071b515', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('680582ef-3f48-45e3-b62a-8b122071b515', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('680582ef-3f48-45e3-b62a-8b122071b515', study_constitutes_performance, conventional).
narrative_ontology:cs_axiom('680582ef-3f48-45e3-b62a-8b122071b515', secondary, obligation_currently_dischargeable).
narrative_ontology:cs_axiom_status(obligation_currently_dischargeable, holdable).
narrative_ontology:cs_axiom_grounding('680582ef-3f48-45e3-b62a-8b122071b515', obligation_currently_dischargeable, conventional).
narrative_ontology:cs_reference_frame('680582ef-3f48-45e3-b62a-8b122071b515', temple_era_cultic_performance).
narrative_ontology:cs_drift_state('680582ef-3f48-45e3-b62a-8b122071b515', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('680582ef-3f48-45e3-b62a-8b122071b515', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, study_practitioners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, textual_tradition_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, yeshiva_institutions).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, torah_study_equivalent_to_practice).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, obligation_persists_absent_temple).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in daily or periodic study of the sacrificial order (Seder Kodashim, laws of korbanot) as a devotional and legal practice. Under this reading, the study itself discharges the underlying commandment — no altar, no animal, no Temple is required. They gain full religious standing on the obligation through textual engagement alone, at the cost only of the time and access needed to study; the practice is open to essentially anyone literate in the relevant texts.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, study_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Rabbinic authorities and legal scholars articulate and transmit the doctrine that study equals fulfillment, citing prophetic and Talmudic sources (e.g., Hosea's 'let us render as bulls the offering of our lips,' and Talmudic statements that one who studies the laws of a sacrifice is credited as though it were offered). They administer the interpretive tradition that keeps this reading authoritative and teachable, and they benefit from the doctrine's continuity as it validates their own scholarly vocation as religiously complete rather than merely preparatory.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, textual_tradition_scholars, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__study_as_performance, textual_tradition_scholars, agenda_setter).

% Hold that actual sacrificial performance remains the object of the commandment and regard study-as-fulfillment as, at most, a partial or provisional substitute pending restoration of the Temple. They are not victims of this constraint but their competing normative claim is structurally sidelined wherever the study-as-performance reading is treated as settled rather than as one of several live positions.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, temple_restorationists, excluded,
    moderate, civilizational, constrained, global).

% Study how post-Temple Judaism reorganized normative obligation around text rather than cultic act, tracing continuity and rupture across the destruction of the Second Temple. They take no side but document how this reading functioned to preserve the coherence and livability of halakhic obligation without physical infrastructure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, comparative_religion_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which a commandment whose physical object (the Temple altar) no longer exists can still be fulfilled, coordinating an entire community's sense of ongoing legal completeness and religious standing around a text-based practice that requires no scarce or destroyed infrastructure.
% TRANSFER_FUNCTION: Moves normative weight from cultic performance to textual engagement: no material transfer occurs between parties; what is 'transferred' is legal-religious credit, from the domain of impossible physical acts to the domain of accessible study, without extracting resources from any party.
% ABSENT_VOICES: Those who hold the performance_only or messianic_suspension readings would object that treating study as actual fulfillment overstates what text can accomplish and risks permanently deferring the restorationist project by removing its urgency; they are represented in the broader legal literature but are not privileged within this reading's own internal presentation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, communities without access to a functioning Temple would lose a doctrinally secure basis for claiming the sacrificial commandments as currently fulfillable, and the daily/curricular practice of studying Seder Kodashim would lose its status as the commandment's fulfillment rather than mere preparation — study would continue but under a different normative description (as under performance_only or archival_preservation), reshaping curricula, liturgical emphasis, and the felt completeness of religious observance.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the physical performance of korbanot became impossible, threatening to leave a substantial body of commandments permanently unfulfillable and creating a gap in the sense of complete religious observance.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources themselves (Talmud, Menachot 110a; Hosea 14:3) attest that this reformulation was needed and effective. Comparative religion scholars, writing from outside the tradition's own legal authority, corroborate that the study-as-fulfillment doctrine functioned historically to stabilize halakhic practice after the Temple's destruction, though they note the doctrine also serves the institutional interest of scholars whose vocation it validates — corroboration exists, but it is not disinterested on the tradition-internal side.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because the study-as-performance reading imposes no material cost on any party distinguishable from ordinary religious observance; nothing is extracted, and the practice is a net psychological/religious benefit to those who engage in it. Suppression is low (0.05) — no one is coerced into accepting this reading, and rival readings (performance_only, messianic_suspension, archival_preservation) circulate openly in the same textual tradition. Accessibility collapse is low (0.15): study is highly accessible, requiring literacy and access to texts rather than land, priesthood, or capital. Resistance is low (0.1), consistent with a genuine coordination solution to a real problem (unfulfillable commandments after Temple destruction) rather than an imposed extraction. Theater ratio is modest and stable across the interval (0.10–0.12): the practice is not merely performative dressing over an absent function — the study itself is asserted to BE the function.
 *
 * DIRECTIONALITY LOGIC:
 *   Study practitioners and textual tradition scholars are both structural beneficiaries: the former gain religious completeness, the latter gain doctrinal validation of their own vocation. No victim group is declared for this reading — the doctrine's entire structural point is that the obligation is SATISFIED, not that some party bears its cost. Temple restorationists are marked excluded rather than victim: they are not harmed by this reading operating, but their alternative framing is structurally deprioritized wherever study_as_performance holds field.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction making sacrificial commandments literally unperformable) is genuinely dead in the narrow sense — no Temple has existed for nearly two millennia — yet the doctrine remains vigorously practiced and taught, which could look like classic mandatrophy (a mandate persisting past its function). But the founding_problem_status is authored as contested rather than dead: study_as_performance does not claim the ORIGINAL problem persists, it claims a REFRAMED problem (how to fulfill obligations under permanently altered material conditions) that remains genuinely live. This is precisely the case the classification exists to protect against mislabeling: a coordination mechanism that adapted its object rather than merely persisting past a dead function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_fulfillment_ambiguity,
    'Does the Talmudic and prophetic textual basis (e.g., Menachot 110a, Hosea 14:3) actually establish study as literal fulfillment, or is this reading itself a later homiletical extension read back into earlier sources to make an intolerable situation livable?',
    'Close philological and historical analysis of the earliest attestations of the study-as-fulfillment claim, dated against the destruction of the Second Temple, to determine whether the doctrine is contemporaneous with the crisis or a later theological development.',
    'If the doctrine is a later retrofit rather than an original halakhic principle, its authority is weaker relative to performance_only, and the beneficiary structure here would look more like institutional self-justification by the scholarly class that transmits it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfillment_ambiguity, empirical, 'Whether study-as-fulfillment is an original or retrofitted doctrine.').

omega_variable(
    kernel_reading_dominance,
    'Among the four readings of the sacrifice_obligation_continuity kernel, which is treated as authoritative in which communities, and does the apparent low extraction/low suppression of study_as_performance depend on it NOT being coercively imposed over the sibling readings?',
    'Survey normative literature and communal practice across denominations (e.g., differing emphases in Orthodox yeshiva curricula vs. Reform theology) to map which reading holds practical authority where, and whether any community suppresses competing readings.',
    'If a community coercively privileges study_as_performance over performance_only or messianic_suspension (e.g., by denying restorationist advocacy legitimacy), this reading''s low suppression score would need revision for that community''s instantiation of it, even though the doctrine''s own content is not exclusionary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Whether this reading''s benign metrics depend on the kernel''s readings coexisting rather than one being coercively imposed.').

omega_variable(
    scholarly_class_interest,
    'Is the study-as-performance doctrine''s persistence explained by its genuine coordination value, or partly by the interest of the rabbinic/scholarly class in a doctrine that makes their vocation (rather than priesthood or land) the site of religious completeness?',
    'Compare doctrinal emphasis and institutional resource allocation (yeshiva funding, curricular hours devoted to Seder Kodashim) against communities with weaker scholarly-class institutional stakes, to see if the doctrine''s prominence tracks institutional interest.',
    'If tracking institutional interest strongly, this reading may carry more concentrated benefit to textual_tradition_scholars specifically than to the broader community of study_practitioners, suggesting an FSM-adjacent structure worth flagging even though beneficiaries are not currently modeled as extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_class_interest, conceptual, 'Whether scholarly institutional interest, not just coordination need, explains the doctrine''s prominence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 400, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t400, observed).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 800, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t800, observed).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1200, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t1200, observed).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t1600, observed).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t400, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 400, 0.06).
narrative_ontology:measurement_basis(sacr_be_t400, observed).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 800, 0.06).
narrative_ontology:measurement_basis(sacr_be_t800, observed).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1200, 0.07).
narrative_ontology:measurement_basis(sacr_be_t1200, observed).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1600, 0.08).
narrative_ontology:measurement_basis(sacr_be_t1600, observed).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 1950, 0.08).
narrative_ontology:measurement_basis(sacr_be_t1950, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This story is one of four constraint files decomposing the natural-language concept 'is the sacrificial obligation still binding, and if so how is it discharged.' Each sibling reading (performance_only, messianic_suspension, archival_preservation) has its own ε, its own beneficiary/victim structure, and its own claimed type — study_as_performance is authored here as the lowest-extraction, no-victim reading because its entire content is that the obligation has already been satisfied through an accessible practice, unlike performance_only (unresolved obligation-pressure) or messianic_suspension (indefinite deferral cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
