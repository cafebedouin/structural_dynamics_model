% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Equality Clause Read as Universal Principle Requiring Iterative Expansion
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the universalist reading of the 'all men are created
 *   equal' kernel: the equality clause's universal grammar is treated as the
 *   operative commitment, binding on later generations to iteratively extend
 *   recognized equal status to groups the founders excluded, regardless of
 *   founder intent. This is one of three readings of the same kernel text.
 *   The originalist reading (a separate constraint) treats founder intent as
 *   scope-determining. The textualist paradox reading (a separate constraint)
 *   treats the gap between universal language and restricted application as
 *   an unresolved performative contradiction rather than a mandate for
 *   progressive correction. This story's ε is authored for the universalist
 *   reading's own arrangement — the iterative expansion project as it
 *   actually operates, including its coordination costs and its
 *   incompleteness — not for a hypothetical fully-realized universal-equality
 *   end state.
 *
 * KEY AGENTS:
 *   - constitutional_reform_coalitions: agenda_setter (organized/mobile) — press and administer the expansion
 *   - previously_excluded_groups_seeking_inclusion: beneficiary (organized/constrained) — gain standing once recognized
 *   - groups_denied_equal_status_pending_expansion: payer (powerless/trapped) — bear the cost of not-yet-recognized exclusion
 *   - incumbent_beneficiaries_of_narrower_readings: payer/beneficiary (powerful/constrained) — lose relative advantage as expansion proceeds
 *   - federal_and_state_courts: agenda_setter/observer (institutional/analytical) — operationalize the expanding boundary
 *   - originalist_jurists_and_scholars: excluded (organized/constrained) — object but do not control the reading once adopted
 *   - future_unrecognized_claimant_groups: excluded (powerless/trapped) — exist under the logic but have no seat yet
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.42).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Equality Clause Read as Universal Principle Requiring Iterative Expansion").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '57878ca6-5ca9-4c5b-ab4d-011a769846f9').
narrative_ontology:cs_kernel_codification('57878ca6-5ca9-4c5b-ab4d-011a769846f9', fixed_text).
narrative_ontology:cs_authority_grounding('57878ca6-5ca9-4c5b-ab4d-011a769846f9', practice).
narrative_ontology:cs_interpretation_layer_present('57878ca6-5ca9-4c5b-ab4d-011a769846f9').
narrative_ontology:cs_reading_relation('57878ca6-5ca9-4c5b-ab4d-011a769846f9', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('57878ca6-5ca9-4c5b-ab4d-011a769846f9', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('57878ca6-5ca9-4c5b-ab4d-011a769846f9', foundational, textual_universality_binds_beyond_founder_application).
narrative_ontology:cs_axiom_status(textual_universality_binds_beyond_founder_application, holdable).
narrative_ontology:cs_axiom_grounding('57878ca6-5ca9-4c5b-ab4d-011a769846f9', textual_universality_binds_beyond_founder_application, conventional).
narrative_ontology:cs_axiom('57878ca6-5ca9-4c5b-ab4d-011a769846f9', secondary, moral_understanding_progresses_and_reinterpretation_tracks_it).
narrative_ontology:cs_axiom_status(moral_understanding_progresses_and_reinterpretation_tracks_it, holdable).
narrative_ontology:cs_axiom_grounding('57878ca6-5ca9-4c5b-ab4d-011a769846f9', moral_understanding_progresses_and_reinterpretation_tracks_it, empirically_contingent).
narrative_ontology:cs_reference_frame('57878ca6-5ca9-4c5b-ab4d-011a769846f9', founding_era_universal_declaration).
narrative_ontology:cs_drift_state('57878ca6-5ca9-4c5b-ab4d-011a769846f9', contemporary_equal_protection_doctrine, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('57878ca6-5ca9-4c5b-ab4d-011a769846f9', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, previously_excluded_groups_seeking_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_movements).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, constitutional_reform_coalitions).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_denied_equal_status_pending_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, moral_progress_thesis).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, constitutional_perfectibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigate, legislate, and organize to press the equality clause's language outward to cover groups not covered by the founding-era application. They administer the reading itself — deciding, case by case, which exclusion is next to fall — and their institutional survival depends on the expansion project remaining live.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_reform_coalitions, agenda_setter,
    organized, generational, mobile, national).

% Currently outside the clause's practical protection but organizing to be brought inside it. They gain formal legal standing and material protections once the reading is extended to them, at the cost of protracted legal and political struggle with no fixed endpoint or guaranteed timeline.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, previously_excluded_groups_seeking_inclusion, beneficiary,
    organized, generational, constrained, national).

% Are the next-in-line claimants whose inclusion has not yet been recognized by courts or legislatures. They bear the cost of the iterative model's incompleteness: because the principle is 'always further expanding,' their exclusion today is framed as a temporary, soon-to-be-corrected oversight rather than a present injury requiring immediate remedy, which can defer redress indefinitely.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_denied_equal_status_pending_expansion, payer,
    powerless, biographical, trapped, national).

% Hold advantages tied to the historically narrower application of the clause (e.g., property, franchise, or standing advantages accrued under earlier, more restrictive readings). Each act of expansion redistributes some relative advantage away from them; they experience the universalist reading as an open-ended claim on their position with no principled stopping point.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings, beneficiary).

% Adjudicate which groups the clause now covers, drawing on precedent, changed social understanding, and doctrine rather than founding-era taxonomy alone. Their rulings both enforce the current boundary and authorize the next round of claims, making them the mechanism through which the iterative expansion is operationalized.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, federal_and_state_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, federal_and_state_courts, observer).

% Object that the universalist reading detaches the clause from any determinate meaning and effectively lets each generation rewrite the Constitution under the guise of interpretation. Their objection is heard in dissents and legal scholarship but does not control the reading's operation once a majority coalition of courts or legislatures adopts the expansive frame.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_jurists_and_scholars, excluded,
    organized, generational, constrained, national).

% Groups whose claim to equal status has not yet been articulated or recognized as such — the reading's logic implies they exist, but the iterative model gives them no seat, no timeline, and no assurance the expansion project reaches them at all.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, future_unrecognized_claimant_groups, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, revisable standard for expanding the polity's circle of legally recognized equals without requiring a new constitutional text each time — courts and legislatures can extend recognized status to previously excluded groups by reinterpretation rather than amendment, coordinating moral and legal change around a stable textual anchor.
% TRANSFER_FUNCTION: Moves formal legal standing, and the material protections that follow from it, from groups currently holding narrower relative advantage to groups newly recognized as covered by the equality principle — the direction of transfer runs toward whichever group's claim the current coalition of courts, legislatures, and social movements next validates.
% ABSENT_VOICES: Groups whose claim to equal status has not yet been framed as such by any organized movement are structurally absent from the record — the iterative model can only expand to claims that have been articulated and pressed; unarticulated exclusions have no seat at all. Originalist jurists are present but structurally overruled once expansive coalitions control courts and legislatures.
% DISAPPEARANCE_RATIONALE: If the universalist reading were abandoned in favor of a fixed, founder-bound scope, the last two centuries of expansion (racial, gender, and other status-based inclusions) would lose their doctrinal foundation; ongoing claims by currently excluded groups would have no textual anchor to press against, and legal doctrine would revert to treating exclusion as constitutionally settled rather than as pending correction.
% FOUNDING_PROBLEM: The founding text used universal language ('all men are created equal') while the founders' own social order excluded most of the population it apparently described; the universalist reading was built to resolve this by treating the text's universal grammar, not the founders' restricted application, as the operative commitment — allowing later generations to correct the gap between stated principle and lived practice.
% FOUNDING_PROBLEM_CORROBORATION: Courts adopting expansive equal protection doctrine (citing changed understanding rather than founder intent) corroborate the reading from the bench; historians of the abolition, suffrage, and civil rights movements corroborate it from outside any current beneficiary group by documenting that each expansion was contested and resisted by incumbents at the time, indicating the gap between universal language and restricted application has been a live, unresolved site of struggle rather than a settled matter — this corroboration comes from scholarship and judicial record outside the currently-organized beneficiary coalitions.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) reflecting genuine coordination costs: expansion requires litigation, legislative coalition-building, and social mobilization, and each successful expansion redistributes some relative advantage from incumbents to newly-recognized groups — a real transfer, not merely rhetorical. Suppression starts moderate-high (0.55) reflecting early resistance to any expansion beyond the founding taxonomy, and falls over the interval (to 0.38) as expanded readings become doctrinally entrenched and resistance to incremental extension weakens. Theater ratio starts higher (0.40) — early expansions were often symbolic/rhetorical before being backed by enforceable remedy — and falls (to 0.22) as doctrine matured into enforceable standards. Accessibility collapse is low-moderate (0.30): the reading does not foreclose the originalist or textualist-paradox alternatives, which remain live in scholarship and dissent. Resistance is substantial (0.62), consistent with a reading that must continually argue against a rival, textually-grounded alternative rather than resting on settled consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of constitutional reform coalitions and recognized beneficiaries, this reading looks like principled moral and legal progress — a rope extending genuine coordination benefits outward. From the seat of groups still pending recognition, the same structure looks like a tangled rope: coordination is real (the doctrine exists and sometimes delivers), but the promise of eventual inclusion can substitute for present remedy, and the timeline is set by others. From incumbent beneficiaries of narrower readings, the structure looks like an open-ended claim on their position with no textually fixed stopping point — which is precisely the objection pressed by the originalist and textualist-paradox siblings.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional reform coalitions and courts sit near the agenda-setting end: they administer which claims get recognized next and their institutional purpose is the expansion project itself. Previously excluded groups who achieve recognition are beneficiaries — the constraint's operation, once it reaches them, subsidizes their standing. Groups still pending recognition are victims of the model's incompleteness: their exclusion is framed as temporary and correctable, which can function to defer rather than deliver redress, giving them high effective extraction despite the reading's benevolent self-description. Incumbent beneficiaries of narrower readings are dual-positioned: they retain much of their prior advantage (beneficiary) while losing ground incrementally (payer) as expansion proceeds — their trapped/constrained exit reflects that they cannot simply opt out of a shifting constitutional boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal language exceeding founding-era application) remains live by this reading's own lights — new claimant groups continue to emerge and press for recognition, so the arrangement has not become a shell defending a completed task. However, the model's structural feature of perpetual incompleteness is also what allows incumbents and courts to indefinitely defer specific claims under the promise of eventual correction; the corpus should watch for cases where 'eventually' becomes the mechanism of extraction rather than good-faith progress. This is why founding_problem_status is authored 'live' rather than 'dead': the expansion project continues to do real work, but its perpetual-motion structure is also its vulnerability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansion_endpoint_indeterminacy,
    'Does the universalist reading have any principled stopping point, or is ''iterative expansion'' open-ended by construction — and if open-ended, does that indeterminacy itself function as a deferral mechanism against currently pending claimants?',
    'Track historical claimant groups from first articulation of their claim to formal recognition; measure whether time-to-recognition is shrinking (consistent with genuine progress) or driven primarily by external political shifts unrelated to the doctrine''s own logic (consistent with deferral).',
    'If time-to-recognition tracks doctrinal maturation, the universalist reading functions closer to a rope with real, if slow, coordination benefit. If it tracks external political contingency with no doctrinal momentum, the ''iterative expansion'' framing risks being a legitimating cover for indefinite deferral, pushing the classification toward snare for currently-pending groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_endpoint_indeterminacy, conceptual, 'Whether iterative expansion has a real endpoint or functions as open-ended deferral.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among originalist, textualist-paradox, and universalist readings itself determined by anything internal to the constitutional text, or is it externally selected by which reading currently serves the interpreter''s institutional or political position?',
    'Compare judicial and scholarly adoption patterns of each reading against the interpreter''s institutional incentives and the historical moment; look for cases where a single actor switches readings opportunistically across different equality claims.',
    'If reading-selection tracks interpreter interest rather than principled textual analysis, all three readings (including this one) are partly post-hoc justifications for outcomes reached on other grounds, which would elevate suppression and reduce the coordination credit given to any single reading, including universalist_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading choice is textually principled or outcome-driven, across all three sibling readings.').

omega_variable(
    incumbent_loss_measurement,
    'How much of the measured extractiveness reflects genuine transfer of unjust relative advantage away from incumbents (a legitimate correction) versus genuine new costs imposed on incumbents that were not previously benefiting from anyone''s exclusion?',
    'Case-by-case historical analysis of specific expansions (e.g., extension of suffrage, extension of equal protection to new classes) distinguishing loss of exclusionary privilege from loss of independently-earned position.',
    'If losses are overwhelmingly the former, the extraction from incumbents is better described as removal of an unearned subsidy rather than a new cost — this would lower the effective extraction attributed to incumbent_beneficiaries_of_narrower_readings as victims. If substantial losses are of the latter kind, the tangled_rope classification''s victim declaration for incumbents is more solidly grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_loss_measurement, empirical, 'Whether incumbent losses under expansion are corrections of unjust privilege or independent new costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__universalist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(all__tr_t50, all_men_created_equal__universalist_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(all__tr_t100, all_men_created_equal__universalist_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(all__tr_t150, all_men_created_equal__universalist_reading, theater_ratio, 150, 0.27).
narrative_ontology:measurement(all__tr_t200, all_men_created_equal__universalist_reading, theater_ratio, 200, 0.24).
narrative_ontology:measurement(all__tr_t250, all_men_created_equal__universalist_reading, theater_ratio, 250, 0.22).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__universalist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(all__be_t50, all_men_created_equal__universalist_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(all__be_t100, all_men_created_equal__universalist_reading, base_extractiveness, 100, 0.3).
narrative_ontology:measurement(all__be_t150, all_men_created_equal__universalist_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(all__be_t200, all_men_created_equal__universalist_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(all__be_t250, all_men_created_equal__universalist_reading, base_extractiveness, 250, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__universalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(all__su_t50, all_men_created_equal__universalist_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(all__su_t100, all_men_created_equal__universalist_reading, suppression_requirement, 100, 0.46).
narrative_ontology:measurement(all__su_t150, all_men_created_equal__universalist_reading, suppression_requirement, 150, 0.42).
narrative_ontology:measurement(all__su_t200, all_men_created_equal__universalist_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(all__su_t250, all_men_created_equal__universalist_reading, suppression_requirement, 250, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.1).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint, originalist_reading, and textualist_paradox_reading form a three-member reading family over the single all_men_created_equal kernel. Each reading is authored as a separate, ε-invariant constraint per the decomposition principle: the kernel text is held fixed while the authority-grounding mechanism (founder intent vs. iterative practice vs. irreducible contradiction) differs. This story (universalist_reading) authors moderate extractiveness reflecting real coordination costs of expansion and a growing/receding victim set; originalist_reading is expected to author a narrower, more mountain-like or rope-like profile bounded to founding-era taxonomy; textualist_paradox_reading is expected to author the contradiction itself as the primary structural fact, likely with distinct extraction dynamics tied to the unresolved gap rather than to expansion machinery. influences (not forecloses) is declared toward textualist_paradox_reading because the universalist reading's expansion practice changes the stakes and resource availability for parties pressing the contradiction reading, without logically ruling it out.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
