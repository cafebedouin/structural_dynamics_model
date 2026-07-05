% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism Reading of Basic Law Interpretive Authority
 *   domain: constitutional_law_political_theory
 *
 * SUMMARY:
 *   This story instantiates the popular constitutionalism reading of the
 *   basic_law_interpretive_authority kernel: the claim that constitutional
 *   meaning is properly determined through ongoing democratic contestation
 *   across courts, legislatures, and social movements, with no single
 *   institution holding terminal interpretive authority. This is a distinct
 *   constraint from the judicial_supremacy_reading (courts as final
 *   interpreter) and the parliamentary_sovereignty_reading (legislature as
 *   final interpreter) — those are separate stories with their own ε values
 *   and stakeholder structures, linked here only through
 *   network.affects_constraints. The coordination function is real:
 *   distributed interpretive authority prevents any single institution from
 *   permanently ossifying meaning against later democratic correction. The
 *   extraction is also real: perpetual contestability transfers the cost of
 *   instability onto parties who need settlement — minority rights claimants
 *   most acutely, since a right they win today is never secure against
 *   tomorrow's mobilizational cycle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.28).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism Reading of Basic Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law_political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'd9004284-6da5-4937-8df5-087335b3407f').
narrative_ontology:cs_kernel_codification('d9004284-6da5-4937-8df5-087335b3407f', distributed).
narrative_ontology:cs_authority_grounding('d9004284-6da5-4937-8df5-087335b3407f', distributed).
narrative_ontology:cs_reading_relation('d9004284-6da5-4937-8df5-087335b3407f', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d9004284-6da5-4937-8df5-087335b3407f', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('d9004284-6da5-4937-8df5-087335b3407f', foundational, no_institution_holds_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(no_institution_holds_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('d9004284-6da5-4937-8df5-087335b3407f', no_institution_holds_terminal_interpretive_authority, conventional).
narrative_ontology:cs_axiom('d9004284-6da5-4937-8df5-087335b3407f', foundational, constitutional_meaning_is_perpetually_revisable_through_democratic_process).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_perpetually_revisable_through_democratic_process, holdable).
narrative_ontology:cs_axiom_grounding('d9004284-6da5-4937-8df5-087335b3407f', constitutional_meaning_is_perpetually_revisable_through_democratic_process, instrumental).
narrative_ontology:cs_reference_frame('d9004284-6da5-4937-8df5-087335b3407f', distributed_interpretive_dialogue).
narrative_ontology:cs_drift_state('d9004284-6da5-4937-8df5-087335b3407f', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9004284-6da5-4937-8df5-087335b3407f', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_social_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, electorally_ascendant_political_coalitions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars_of_contestation).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, minority_rights_claimants_between_cycles).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, regulated_parties_facing_persistent_legal_uncertainty).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, lower_courts_lacking_settled_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize sustained campaigns — litigation, protest, electoral pressure, legislative lobbying — to shift what the constitution is understood to mean. This reading grants them a permanent seat at the interpretive table rather than requiring them to win once before a court and then stop. They benefit precisely because no single institution can foreclose the argument against them permanently; every adverse ruling or statute is reopenable.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_social_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, mobilized_social_movements, agenda_setter).

% Use electoral majorities to press constitutional reinterpretation through legislation, appointments, and public argument, rather than treating prior judicial rulings as settled. They gain because the constitution remains an object of ongoing political contest they can win rather than a fixed text administered by a court insulated from them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, electorally_ascendant_political_coalitions, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, electorally_ascendant_political_coalitions, agenda_setter).

% Depend on a stable, judicially secured reading of a right to protect them against a hostile majority. Under this reading, no ruling in their favor is ever fully terminal — it can be relitigated, legislated around, or reversed in the next electoral or mobilizational cycle. Between favorable and unfavorable cycles they live with a right whose content depends on which coalition currently has momentum, with no exit from the jurisdiction whose politics determines their status.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, minority_rights_claimants_between_cycles, payer,
    powerless, biographical, trapped, national).

% Businesses, administrative agencies, and individuals who must plan conduct against a constitutional baseline that never fully settles because interpretive authority is distributed and perpetually contestable. They bear compliance costs, litigation exposure, and planning uncertainty that a terminal adjudicative rule would foreclose. Exit is possible only by relocating activity outside the jurisdiction, at real cost.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, regulated_parties_facing_persistent_legal_uncertainty, payer,
    moderate, biographical, constrained, national).

% Must adjudicate concrete disputes without a stable apex ruling to anchor doctrine, because under this reading no single institutional pronouncement is treated as final. They absorb the administrative and reputational cost of inconsistent rulings across jurisdictions while the contestation plays out above and around them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, lower_courts_lacking_settled_precedent, payer,
    moderate, immediate, constrained, national).

% Academic and public intellectual communities whose professional and reputational capital is built on theorizing, narrating, and adjudicating rounds of ongoing constitutional contestation. A world of terminal judicial or legislative settlement would substantially reduce the demand for their interpretive labor.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars_of_contestation, beneficiary,
    moderate, generational, mobile, national).

% The apex court retains a voice but not a terminal one under this reading — its rulings are treated as one contribution to an ongoing dialogue rather than the final word, and it can be effectively overridden through sustained legislative or popular pressure over time. It would object that this diminishes the rule-of-law function courts are meant to secure, but that objection is structurally discounted by the reading itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, apex_judiciary, excluded,
    institutional, generational, constrained, national).

% Comparative constitutional scholars and historians who study how interpretive authority is distributed across institutions over time, without a stake in any single round of contestation's outcome.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_theorists_analytical, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__popular_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__popular_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes interpretive authority so that constitutional meaning tracks ongoing democratic will and can adapt to social change without requiring formal amendment, which coordinates around a genuine problem: a single terminal interpreter (court or legislature) can ossify meaning against later, better-informed majorities or against injustices the founding generation did not anticipate.
% TRANSFER_FUNCTION: Moves the cost of interpretive instability from the political and academic actors who thrive on contestation to parties who need settled expectations — minority rights claimants who need a floor beneath them, regulated parties who need a stable baseline, and lower courts who need doctrinal anchors.
% ABSENT_VOICES: Minority groups protected by a specific ruling but lacking sustained mobilization capacity are structurally underrepresented in the next round of contestation; their earlier victory does not accumulate into settled protection the way it would under judicial supremacy. Lower courts have no forum to demand doctrinal closure.
% DISAPPEARANCE_RATIONALE: Proponents argue that if popular constitutionalism disappeared and a terminal interpreter (court or legislature) took over, the world would rearrange substantially: rights currently open to renegotiation would either freeze protectively or freeze adversely depending on which institution won the terminal role, and social movements would lose their standing to relitigate settled doctrine. Critics of the reading argue the underlying contestation dynamic would continue informally regardless of which institution is nominally terminal, so the world would not fully rearrange — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The problem of judicial or legislative capture: an institution granted terminal interpretive authority can ossify constitutional meaning in ways that entrench a particular era's power arrangements against subsequent democratic correction, and doing so without any mechanism for realignment other than formal amendment (a very high bar) risks a permanently unresponsive constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists studying democratic backsliding and comparative constitutionalism (outside the movements and coalitions that benefit from perpetual contestability) corroborate that terminal adjudicative authority can produce genuine ossification. But the same outside scholarship also documents that perpetual contestability produces its own harm — protection instability for minorities — so the corroboration supports the founding problem's continued relevance without validating that this reading's proposed remedy is net beneficial rather than merely redistributive of institutional risk.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and suppression (0.28) are both moderate rather than extreme: this reading does not coerce compliance through a dominant enforcement apparatus (no court order, no statute forecloses the debate), so suppression is comparatively low — the mechanism of harm is instability, not coercion. Theater ratio (0.38) reflects that a meaningful share of 'ongoing contestation' in mature democracies is performative re-litigation of settled points for electoral or academic benefit rather than genuine doctrinal movement, and this share appears to be rising over the measured interval. Accessibility collapse (0.35) is comparatively low because the entire premise of this reading is that alternatives (a different reading of a given right) remain genuinely live rather than foreclosed — that openness is the reading's defining feature, not a defect, though it is also precisely what produces cost for minority claimants. Resistance (0.55) is moderately high: apex courts and settled-doctrine constituencies actively resist the framing that their rulings are non-terminal.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobilized social movements and electorally ascendant coalitions are declared beneficiaries because the reading directly grants them a permanent, non-foreclosed seat in determining constitutional meaning — this is close to the beneficiary end of directionality. Minority rights claimants between cycles, regulated parties, and lower courts are declared victims/payers because they bear the direct cost of the absence of terminal settlement: protection instability, planning uncertainty, and doctrinal incoherence, respectively. Their exit options are trapped or constrained, which the derivation chain should push toward higher effective extraction. The apex judiciary is excluded rather than victim/beneficiary because its objection (that this diminishes rule-of-law function) is structurally present in the discourse but is discounted by the reading's own logic — it retains institutional power (institutional atom) but is not treated as a party whose costs the reading is designed to address.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing terminal institutional ossification against later democratic correction) remains genuinely live in general, which argues against treating this reading as mandatrophic. But the founding_problem_status is authored as contested rather than live because the specific remedy — perpetual, undifferentiated contestability across ALL constitutional questions rather than a more targeted anti-ossification mechanism (e.g., supermajority override, sunset review) — may have outlived the narrower problem it was meant to solve, becoming a general-purpose justification for relitigating settled minority protections whenever a hostile coalition gains power. This is exactly the divergence the framework is built to surface: the coordination story (preventing ossification) is real, but the specific institutional form this reading takes may extract well beyond what that coordination function requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_basic_law,
    'This constraint is one of three declared readings of the basic_law_interpretive_authority kernel (judicial_supremacy_reading, parliamentary_sovereignty_reading, popular_constitutionalism_reading). Which reading a given constitutional order actually operates under is itself a contested empirical and normative question, not something this story can resolve — the popular_constitutionalism_reading may be the accurate description of some systems (or of no system, functioning instead as an aspirational academic frame layered over a system that in practice defers to courts or legislatures).',
    'Comparative institutional analysis: track whether apex court rulings on contested constitutional questions are in practice treated as terminal (supporting judicial_supremacy_reading), routinely overridden by ordinary legislation without constitutional amendment (supporting parliamentary_sovereignty_reading), or genuinely remain live across sustained multi-institutional contestation over decades (supporting popular_constitutionalism_reading).',
    'If the descriptive reality tracks judicial_supremacy_reading or parliamentary_sovereignty_reading more closely, this story describes an aspirational/academic frame rather than the operative constraint, which would substantially lower its real-world extractiveness (fewer parties actually bear the instability cost described) while raising the theater_ratio further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basic_law, conceptual, 'Which of the three kernel readings actually describes a given constitutional order''s operative practice is unresolved and may vary by jurisdiction and era.').

omega_variable(
    instability_cost_vs_ossification_benefit,
    'Is the aggregate harm from perpetual interpretive instability (borne concentratedly by minority rights claimants and diffusely by regulated parties) greater or less than the aggregate harm averted by preventing terminal institutional ossification?',
    'Long-run comparative study of rights trajectories and regulatory certainty in jurisdictions operating closer to each reading, controlling for other institutional variables (federalism, party system, judicial independence norms).',
    'If instability costs dominate, the popular_constitutionalism_reading''s coordination story is substantially overstated relative to its extraction, pushing the computed classification toward tangled_rope or snare depending on suppression trends. If ossification-avoidance benefits dominate, the reading is closer to a genuine rope with distributed but justified costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instability_cost_vs_ossification_benefit, empirical, 'Whether perpetual contestability''s costs to minority claimants and regulated parties outweigh its benefit of preventing institutional ossification is an unresolved empirical question.').

omega_variable(
    movement_capture_of_contestation_process,
    'Does the popular constitutionalism process genuinely distribute interpretive influence across the broader democratic public, or does it concentrate influence in well-resourced, sustained-mobilization-capable movements and coalitions, effectively substituting one narrow interpretive elite (courts) for another (professional advocacy organizations and durable electoral coalitions)?',
    'Track which social groups actually succeed in shifting constitutional meaning through contestation versus which groups have equally strong normative claims but lack sustained mobilization capacity (resource-poor, geographically dispersed, or episodically salient constituencies).',
    'If influence concentrates among well-resourced movements, the ''democratic contestation'' framing functions partly as cover for a different, less visible form of capture — supporting reclassification toward tangled_rope with a narrower beneficiary set than the coordination story suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(movement_capture_of_contestation_process, empirical, 'Whether the contestation process genuinely democratizes interpretive influence or merely relocates elite capture from courts to sustained-mobilization actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 32, 0.26).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the basic_law_interpretive_authority kernel. judicial_supremacy_reading and parliamentary_sovereignty_reading are separate constraint stories with independently authored ε and stakeholder structures; this story does not average or blend across them per the ε-invariance principle. All three should link to each other in their respective network.affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
