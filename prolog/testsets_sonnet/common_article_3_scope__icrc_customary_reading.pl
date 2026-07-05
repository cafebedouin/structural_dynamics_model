% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope via ICRC Customary-Law Tracking
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions extends minimum
 *   humanitarian protections to persons in 'armed conflict not of an
 *   international character,' but the treaty text never defines that
 *   threshold. This story instantiates the ICRC customary-law reading: rather
 *   than treating CA3's scope as a fixed intensity/organization threshold
 *   (the state-centric reading) or as an automatic floor triggered by any
 *   organized violence (the expansive human-rights reading), this reading
 *   treats scope as PROCEDURALLY determined — continuously derived from an
 *   accumulating, cross-referenced record of state practice and opinio juris
 *   that the ICRC compiles and tribunals cite. The structural delta from its
 *   siblings is that this reading is a coordination MECHANISM for
 *   interpretation, not a substantive scope rule; it allows the boundary to
 *   move gradually with conflict evolution without formal amendment, at the
 *   cost of leaving the boundary perpetually somewhat indeterminate at any
 *   given moment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.31).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.28).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope via ICRC Customary-Law Tracking").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '3d1e0630-6500-4734-8639-32cc0f35574b').
narrative_ontology:cs_kernel_codification('3d1e0630-6500-4734-8639-32cc0f35574b', distributed).
narrative_ontology:cs_authority_grounding('3d1e0630-6500-4734-8639-32cc0f35574b', practice).
narrative_ontology:cs_interpretation_layer_present('3d1e0630-6500-4734-8639-32cc0f35574b').
narrative_ontology:cs_reading_relation('3d1e0630-6500-4734-8639-32cc0f35574b', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('3d1e0630-6500-4734-8639-32cc0f35574b', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('3d1e0630-6500-4734-8639-32cc0f35574b', foundational, scope_is_procedurally_derived_not_fixed).
narrative_ontology:cs_axiom_status(scope_is_procedurally_derived_not_fixed, holdable).
narrative_ontology:cs_axiom_grounding('3d1e0630-6500-4734-8639-32cc0f35574b', scope_is_procedurally_derived_not_fixed, conventional).
narrative_ontology:cs_axiom('3d1e0630-6500-4734-8639-32cc0f35574b', foundational, accumulated_state_practice_constitutes_binding_evidence).
narrative_ontology:cs_axiom_status(accumulated_state_practice_constitutes_binding_evidence, holdable).
narrative_ontology:cs_axiom_grounding('3d1e0630-6500-4734-8639-32cc0f35574b', accumulated_state_practice_constitutes_binding_evidence, conventional).
narrative_ontology:cs_reference_frame('3d1e0630-6500-4734-8639-32cc0f35574b', post_1949_treaty_indeterminacy).
narrative_ontology:cs_drift_state('3d1e0630-6500-4734-8639-32cc0f35574b', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3d1e0630-6500-4734-8639-32cc0f35574b', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_interpretive_authority).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, victims_of_non_international_armed_conflict).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, customary_law_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, states_party_to_geneva_conventions).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_party_to_geneva_conventions).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, military_commanders_and_forces).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, gradualist_treaty_evolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compiles state practice, tribunal decisions, and military manuals into customary-law studies (notably the 2005 ICRC Customary IHL Study) that states and courts cite when determining whether CA3 applies to a given situation. Does not itself adjudicate disputes but shapes the evidentiary record that others use to do so. Its authority rests on being seen as a neutral compiler rather than an interested party, which it must continuously protect.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_interpretive_authority, agenda_setter,
    institutional, generational, analytical, global).

% Retain the ability to shape CA3's practical scope through their own conduct and public legal justifications (opinio juris) without needing unanimous treaty renegotiation. Benefit from flexibility to argue scope narrowly in conflicts implicating their own forces, but are also bound over time by an accumulating practice record they cannot unilaterally disown once contributed.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_party_to_geneva_conventions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, states_party_to_geneva_conventions, payer).

% Civilians and detainees in internal conflicts whose protection under CA3 depends on whether the situation is recognized as falling within its evolving customary scope. Have no voice in the state-practice-and-opinio-juris record that determines their protection; benefit when the customary reading expands coverage, but the mechanism that decides this operates entirely above them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, victims_of_non_international_armed_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Must operate under rules of engagement calibrated to an uncertain and shifting scope determination, since the customary boundary of CA3 is not fixed at any single moment but tracked and re-derived from an accumulating record. Bear compliance costs and legal risk from ambiguity about which situations trigger CA3 obligations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, military_commanders_and_forces, payer,
    organized, immediate, constrained, national).

% Draw on ICRC customary-law compilations when ruling on individual cases (e.g., ICTY jurisprudence on the Tadic threshold), and their rulings in turn feed back into the practice record the ICRC tracks. Sit both inside and outside the mechanism: they consume the customary record and also generate new practice through their own holdings.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, agenda_setter).

% Bound by CA3 obligations once a conflict is classified as falling within its scope, yet have no standing to contribute opinio juris or be counted as a state-practice source — the customary mechanism that defines their obligations is built entirely from state and quasi-state conduct they cannot participate in shaping.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, excluded,
    moderate, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, evidence-based method for determining when internal armed violence has crossed into CA3's protective scope, allowing the boundary to adapt to new conflict forms (cyber-enabled insurgency, transnational non-state actors, prolonged low-intensity violence) without requiring states to renegotiate treaty text each time.
% TRANSFER_FUNCTION: Moves interpretive authority over CA3's boundary from a fixed textual threshold toward an accumulating record of state conduct and legal justification; procedurally, it moves the burden of establishing scope from treaty drafters onto whoever can marshal the strongest showing of consistent practice and opinio juris.
% ABSENT_VOICES: Victims of internal conflict and non-state armed groups have no standing to contribute to the state-practice-and-opinio-juris record, despite being the parties whose protection or obligation the record ultimately determines. Their situations are the raw material the mechanism interprets, but they are not sources the mechanism recognizes.
% DISAPPEARANCE_RATIONALE: States would likely fall back to a state-centric fixed-threshold reading (more predictable but slower to adapt) or a human-rights-floor reading (more protective but less predictable for military planners) — which one prevails is itself a live dispute among the parties, so the rearrangement is real but its direction is not settled.
% FOUNDING_PROBLEM: CA3's 1949 text uses undefined terms ('armed conflict not of an international character') and provides no institutional mechanism for updating what counts as such a conflict as warfare's character changed; customary-law tracking was adopted as the flexible interpretive method to keep the provision applicable without perpetual treaty renegotiation.
% FOUNDING_PROBLEM_CORROBORATION: International tribunals (ICTY in Tadic, ICJ in Nicaragua) independently corroborate that the textual threshold is genuinely indeterminate and that customary practice has been used as the operative interpretive method outside ICRC's own publications; this is not solely the ICRC's self-assessment of its own necessity.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, contested).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.31 at present) because the mechanism does not visibly transfer resources to a rent-collecting party — its cost is diffuse uncertainty borne by commanders and unprotected victims rather than concentrated extraction. Suppression is moderate (0.28): the mechanism does not coerce compliance so much as it structurally excludes non-state and victim voices from contributing to the record that defines their own protection. Theater ratio is low but rising slightly (0.10 to 0.22) reflecting increasing citation of customary-law compilations in venues where the underlying practice record is thin or contested, a mild Goodhart-style drift where compilation activity begins to substitute for genuinely settled practice.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC's seat, this is coordination: a principled, evidence-driven method to keep IHL applicable to modern conflict forms. From a military commander's seat under the same structure, the identical mechanism appears as an unpredictable, moving target that cannot be resolved in advance of an operation. From a non-state armed group's seat, it is a rule-making process that binds them without ever consulting them. The engine should register this constraint as tangled toward rope for state/ICRC seats and considerably more extractive/excluding for the excluded and powerless seats, even though no single party is straightforwardly 'extracting rents.'
 *
 * DIRECTIONALITY LOGIC:
 *   States are the primary source of the customary record (opinio juris derives from their statements and conduct) and thus retain the most control — they are near-beneficiaries who can shape scope through their own conduct, though once practice accumulates they are also bound by contributions they cannot unilaterally retract. Victims of internal conflict benefit from expansive interpretation when it occurs but have zero input into the mechanism that produces it, making them structurally powerless beneficiaries. Non-state armed groups are excluded entirely from contributing evidence while remaining fully bound by whatever scope determination results — this asymmetry is the clearest directional tension in the story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an undefined 1949 threshold term needing a living interpretive method) remains live per tribunal corroboration, distinguishing this from a mandatrophic constraint where the original problem has vanished but the interpretive apparatus persists. The customary-tracking mechanism is not dead ritual; it continues to be actively invoked and contested in real adjudication (ICTY, ICJ), which is why founding_problem_status is authored as live rather than dead or contested-toward-dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_as_coordination_or_capture,
    'Does the customary-law tracking mechanism function as genuine adaptive coordination (allowing IHL to keep pace with evolving conflict forms) or as a vehicle by which powerful states shape scope determinations to favor their own operational interests, since they are disproportionately the sources of ''state practice''?',
    'Comparative analysis of whose military manuals, statements, and conduct are weighted most heavily in ICRC customary-law compilations, and whether smaller/less powerful states'' contrary practice is treated as equally probative.',
    'If the mechanism systematically privileges powerful-state practice, the coordination framing understates a directional extraction from less-powerful states and non-state actors toward the states whose conduct sets the customary baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_as_coordination_or_capture, empirical, 'Whether customary-law tracking is neutral coordination or state-power-weighted extraction.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the choice among the three CA3 readings (icrc_customary, state_centric, expansive_human_rights) itself neutral, or does the availability of the customary-law procedural reading function to defer resolution of the state-centric vs. expansive substantive dispute indefinitely?',
    'Track whether tribunals invoking the customary reading systematically avoid ruling on the substantive scope question, or whether the customary record itself gradually converges toward one of the two substantive readings over time.',
    'If the procedural reading is used to avoid rather than resolve the substantive dispute, its coordination benefit (flexibility) doubles as a mechanism preserving indeterminacy that primarily costs victims awaiting a scope determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the procedural reading resolves or perpetually defers the underlying substantive kernel dispute.').

omega_variable(
    icrc_neutrality_as_natural_fact,
    'Is the ICRC''s compiling role a naturally neutral technical function, or a constructed institutional position that benefits the ICRC''s own authority and relevance as the recognized compiler of customary IHL?',
    'Examine whether alternative compiling bodies (national Red Cross societies, academic consortia, UN bodies) could perform the same function with comparable legitimacy, and whether the ICRC''s compilation choices have been contested as favoring particular outcomes.',
    'If the ICRC''s compiling authority is substantially self-reinforcing rather than purely functional, the beneficiary declaration for icrc_interpretive_authority understates a directional benefit to the institution''s own standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icrc_neutrality_as_natural_fact, conceptual, 'Whether ICRC''s compiling authority is a natural technical role or a self-reinforcing institutional position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1965, common_article_3_scope__icrc_customary_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(comm_tr_t1980, common_article_3_scope__icrc_customary_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__icrc_customary_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__icrc_customary_reading, theater_ratio, 2015, 0.21).
narrative_ontology:measurement(comm_tr_t2025, common_article_3_scope__icrc_customary_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.18).
narrative_ontology:measurement(comm_be_t1965, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(comm_be_t1980, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1980, 0.23).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1995, 0.26).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(comm_be_t2025, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2025, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_article_3_scope__icrc_customary_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the common_article_3_scope kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. icrc_customary_reading treats scope as procedurally determined via evolving practice (this file); state_centric_reading treats scope as a fixed intensity/organization threshold; expansive_human_rights_reading treats scope as an unconditional floor for any organized armed violence. The three are linked via affects_constraints rather than merged, since each has a distinct beneficiary/victim structure and a distinct ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
