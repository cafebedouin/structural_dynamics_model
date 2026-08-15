% ============================================================================
% CONSTRAINT STORY: menu_curation_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_menu_curation_capture, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: menu_curation_capture
 *   human_readable: Self-Selected Kill-Condition Menu Capture
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   Cheap LLM-assisted generation of candidate falsifiers was supposed to
 *   democratize falsifiability discipline — anyone can now produce a menu of
 *   kill conditions for a claim they hold. But generating the menu is only
 *   half the act; selecting from it is a second, discretionary move that the
 *   same person who benefits from appearing falsifiable also controls. The
 *   declarant systematically picks the least-threatening item, producing a
 *   documented commitment (a genuine artifact: 'here is my kill condition')
 *   that carries none of the exposure a real precommitment would. This is
 *   downstream of the omega_production_cost_asymmetry mountain: that upstream
 *   constraint is a genuine structural fact about inference cost collapsing;
 *   THIS constraint is what a self-interested agent does with the slack that
 *   collapse creates. The mountain is not itself extractive; the curation
 *   layered on top of it is.
 *
 * KEY AGENTS:
 *   - the_declarant_with_slack: agenda_setter/beneficiary (moderate/arbitrage) — generates and curates the menu, collects the credibility benefit
 *   - the_excluded_stakeholder_e_g_the_parent: payer (powerless/trapped) — bears the cost of untested conditions, absent from the selection step
 *   - llm_menu_generator: non-agent observer — produces the menu without a stake in the pick
 *   - adversarial_assigner: excluded (moderate/constrained) — the role that would fix the problem if invited in, and structurally is not
 *   - downstream_audience: beneficiary (organized/constrained) — extends trust on the strength of the declared-but-uncurated-for-real-exposure condition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(menu_curation_capture, 0.71).
domain_priors:suppression_score(menu_curation_capture, 0.58).
domain_priors:theater_ratio(menu_curation_capture, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(menu_curation_capture, extractiveness, 0.71).
narrative_ontology:constraint_metric(menu_curation_capture, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(menu_curation_capture, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(menu_curation_capture, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(menu_curation_capture, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(menu_curation_capture, snare).
narrative_ontology:human_readable(menu_curation_capture, "Self-Selected Kill-Condition Menu Capture").
narrative_ontology:topic_domain(menu_curation_capture, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(menu_curation_capture, 'd27fa8bc-f186-4943-886e-1b6f73fdd148').
narrative_ontology:cs_kernel_codification('d27fa8bc-f186-4943-886e-1b6f73fdd148', distributed).
narrative_ontology:cs_authority_grounding('d27fa8bc-f186-4943-886e-1b6f73fdd148', distributed).
narrative_ontology:cs_reading_relation('d27fa8bc-f186-4943-886e-1b6f73fdd148', menu_curation_capture__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('d27fa8bc-f186-4943-886e-1b6f73fdd148', menu_curation_capture__pragmatist_reading, influences).
narrative_ontology:cs_reading_relation('d27fa8bc-f186-4943-886e-1b6f73fdd148', menu_curation_capture__proceduralist_reading, forecloses).
narrative_ontology:cs_axiom('d27fa8bc-f186-4943-886e-1b6f73fdd148', foundational, slack_determines_curation_capture_not_standpoint).
narrative_ontology:cs_axiom_status(slack_determines_curation_capture_not_standpoint, holdable).
narrative_ontology:cs_axiom_grounding('d27fa8bc-f186-4943-886e-1b6f73fdd148', slack_determines_curation_capture_not_standpoint, empirically_contingent).
narrative_ontology:cs_axiom('d27fa8bc-f186-4943-886e-1b6f73fdd148', foundational, cheap_production_does_not_imply_cheap_selection).
narrative_ontology:cs_axiom_status(cheap_production_does_not_imply_cheap_selection, holdable).
narrative_ontology:cs_axiom_grounding('d27fa8bc-f186-4943-886e-1b6f73fdd148', cheap_production_does_not_imply_cheap_selection, conventional).
narrative_ontology:cs_reference_frame('d27fa8bc-f186-4943-886e-1b6f73fdd148', declaration_as_self_certifying_commitment).
narrative_ontology:cs_drift_state('d27fa8bc-f186-4943-886e-1b6f73fdd148', post_llm_generation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d27fa8bc-f186-4943-886e-1b6f73fdd148', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(menu_curation_capture, the_declarant_with_slack).
narrative_ontology:constraint_victim(menu_curation_capture, the_excluded_stakeholder_e_g_the_parent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(menu_curation_capture, downstream_audience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Has the tooling, time, and survivable public-error margin to generate a menu of candidate kill conditions from an LLM and then pick from that menu. Presents the selection as good-faith falsifiability discipline — a declared, checkable commitment. In practice selects the item on the menu least likely to actually fire, because generating and reviewing the menu is cheap while being publicly wrong is costly to reputation. Collects the credibility benefit of 'having declared a kill condition' without absorbing the exposure a real kill condition would carry.
narrative_ontology:constraint_stakeholder(menu_curation_capture, the_declarant_with_slack, agenda_setter,
    moderate, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(menu_curation_capture, the_declarant_with_slack, beneficiary).

% Structurally positioned to see the arrangement's actual failure modes (the domain expertise of lived proximity — the parent, the frontline worker, the person the declaration is about) but has no access to the menu-generation step and no say in which candidate condition gets selected. Bears the cost when the arrangement's real defect goes untested because the declarant curated around it. Cannot generate a competing menu at the same cost and cannot force the declarant's choice onto the record.
narrative_ontology:constraint_stakeholder(menu_curation_capture, the_excluded_stakeholder_e_g_the_parent, payer,
    powerless, biographical, trapped, local).

% Produces the candidate menu of falsifiers cheaply and near-instantly on request. Has no stake in which item is chosen and no mechanism to force adversarial selection; its agreeableness (tendency to generate plausible-sounding but non-threatening candidates when not explicitly pushed toward adversarial framing) is part of what makes the least-threatening choice easy to justify as 'a model-suggested condition,' not a self-serving pick.
narrative_ontology:constraint_stakeholder(menu_curation_capture, llm_menu_generator, observer,
    institutional, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(menu_curation_capture, llm_menu_generator).

% A third party (a genuine adversarial collaborator, a red-teamer, an assigned skeptic) who would pick a kill condition FOR the declarant rather than letting the declarant pick from a self-curated menu. Structurally absent from the ordinary declaration process — the comparison this constraint exists to make (self-selected vs. adversarially-assigned firing rates) can only happen when someone in this role is deliberately brought in, which rarely occurs by default.
narrative_ontology:constraint_stakeholder(menu_curation_capture, adversarial_assigner, excluded,
    moderate, immediate, constrained, local).

% Reads the declared kill condition as evidence of epistemic honesty and updates trust in the declarant's claim accordingly. Benefits from the appearance of falsifiability discipline without the tools to distinguish a self-curated menu pick from a genuinely adversarial one; their trust is the resource the curation captures.
narrative_ontology:constraint_stakeholder(menu_curation_capture, downstream_audience, beneficiary,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(menu_curation_capture, the_declarant_with_slack).
narrative_ontology:fixing_cost_class(menu_curation_capture, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Declaring a kill condition in advance genuinely solves a real problem: it lets others check, after the fact, whether a claim was actually falsifiable rather than infinitely elastic. When done adversarially it is real epistemic discipline.
% TRANSFER_FUNCTION: Moves credibility from the audience and the excluded stakeholder to the declarant: the declarant receives the reputational benefit of 'having a falsifiable, self-critical position' while the actual exposure that would justify that credit is minimized by curating the menu toward conditions unlikely to fire.
% ABSENT_VOICES: The excluded stakeholder (e.g. the parent, the frontline observer, the person with the most at stake in the arrangement actually being tested) is not present at the menu-generation or selection step. The adversarial assigner who would supply a real kill condition is likewise absent unless deliberately invited in.
% DISAPPEARANCE_RATIONALE: If self-curated menus disappeared and every declared kill condition had to be adversarially assigned or drawn from an unassisted, unreviewed self-generation step, the empirical firing rate of declared conditions would rise sharply, declarants would bear real reputational exposure more often, and the excluded stakeholders' structural knowledge would become load-bearing rather than decorative.
% FOUNDING_PROBLEM: The falling cost of generating candidate falsifiers (per the upstream omega_production_cost_asymmetry mountain) was supposed to solve the problem that declaring real, checkable kill conditions used to be too expensive and rare — cheap generation makes falsifiability discipline affordable at scale.
% FOUNDING_PROBLEM_CORROBORATION: The declarant and downstream audience attest the practice solves the founding problem: menus make falsifiability affordable and visible. The excluded stakeholder and adversarial-collaboration methodologists (outside the benefiting parties) attest that the founding problem has mutated: cheap generation solved production cost but created a new, unaddressed selection-cost problem — curation is now the site of the original evasion, just moved one step downstream. No party inside the declarant's own practice independently audits which menu item was chosen or why.
narrative_ontology:disappearance_verdict(menu_curation_capture, world_rearranges).
narrative_ontology:founding_problem_status(menu_curation_capture, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(menu_curation_capture, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(menu_curation_capture, 'none', 1).
narrative_ontology:epsilon_provenance(menu_curation_capture, 0.71, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(menu_curation_capture_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(menu_curation_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(menu_curation_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and theater ratio (0.62) both climb over the interval as the practice scales: early adopters of menu-based kill conditions were plausibly sincere (low theater), but as the pattern becomes recognized and rewarded, more declarants adopt it instrumentally — the ratio of performative declaration to genuinely exposed commitment rises. Suppression (0.58) is moderate: nothing legally or physically bars an adversarial assignment, but the social cost of inviting a real adversary (rather than a cooperative model) to assign your kill condition is real and rising. Accessibility collapse is only moderate (0.48) — the alternative (adversarial assignment, unassisted self-generation) remains cheaply available in principle, which is exactly why this is a snare of discretion rather than a mountain of necessity: the fix is available, just not incentive-compatible for the declarant. Resistance is low (0.35) because the victims (excluded stakeholders) rarely have visibility into the curation step at all — they cannot resist what they cannot see.
 *
 * PERSPECTIVAL GAP:
 *   From the declarant's seat this looks like genuine epistemic virtue — a real commitment was made, checkable in principle. From the excluded stakeholder's seat, the same act is theater: the one condition that would have actually tested the arrangement's known weak point was never on the menu, because the declarant (consciously or not) never asked the LLM to generate it, or asked and discarded it. The engine should register this as a seat divergence rather than a factual dispute about whether a kill condition exists — it does exist; its selection is the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarant sits at the beneficiary end: they set the menu-generation prompt, review the candidates, and pick — full discretion, full credibility upside, minimal downside. The excluded stakeholder sits at the target end: trapped in the local arrangement the declaration is about, with no menu-generation access and no channel to insert a real kill condition. The downstream audience is a secondary beneficiary — they get the comfort of 'falsifiability was performed' without bearing the cost of checking whether it was performed adversarially.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (production cost of candidate falsifiers) is genuinely solved by the upstream mountain — that function has not atrophied, it has been fully achieved. What persists past its usefulness is the inference that DECLARING a self-curated condition is equivalent to being genuinely exposed. The mandate that has outlived its function is not 'generate falsifiers' but 'trust a self-selected menu pick as evidence of falsifiability' — that inference should have been retired once curation discretion became visible as a distinct move from generation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    curation_vs_generation_separability,
    'Can the selection step be structurally separated from the declarant so that curation discretion is removed — e.g., by having a third party or a second, independent model instance pick from the menu blind to the declarant''s preferences?',
    'Run the comparison the primary observable specifies: empirical firing rates of self-selected menu picks vs. unassisted self-generated conditions vs. adversarially-assigned conditions, across a sample of declarants and domains.',
    'If firing rates are statistically indistinguishable across the three sets, curation is not systematically capturing anything and this constraint dissolves toward a rope (cheap generation genuinely democratizing falsifiability). If self-selected menu picks fire at a substantially lower rate than adversarially-assigned ones, the snare reading is empirically corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curation_vs_generation_separability, empirical, 'Whether curation discretion produces a measurable firing-rate gap versus adversarial assignment.').

omega_variable(
    model_agreeableness_contribution,
    'How much of the least-threatening bias comes from the declarant''s selection versus the LLM''s own tendency to generate agreeable, non-threatening candidates in the first place — i.e., is the menu itself pre-curated by model sycophancy before the human ever chooses?',
    'Compare menus generated under neutral prompting versus explicitly adversarial prompting (''generate the condition most likely to falsify this, even if uncomfortable'') from the same model, same declarant, same claim.',
    'If the model''s default menu is already skewed toward non-threatening candidates, part of the extraction is upstream of the declarant''s choice — implicating the tooling itself, not just the selector, and potentially requiring a separate constraint story for model-level agreeableness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_agreeableness_contribution, empirical, 'Whether menu pre-curation by the model itself contributes to the capture independent of human selection.').

omega_variable(
    kernel_reading_selection_basis,
    'This story adopts the instrumentalist reading of the positional_disagreement_as_evidence kernel — locating the extraction in slack-based access to curation tooling rather than in standpoint-based structural marginalization or procedural compliance. Is this the correct reading for THIS specific structural delta, or does the excluded stakeholder''s exclusion better fit the standpoint reading?',
    'Trace whether the excluded stakeholder''s disadvantage tracks slack (time, tooling, survivable error — instrumentalist) or tracks a priori structural credibility asymmetry independent of tooling access (standpoint). Check whether providing the excluded stakeholder equal tooling access eliminates the gap (instrumentalist prediction) or leaves it intact (standpoint prediction).',
    'If tooling access alone closes the gap, the instrumentalist reading (adopted here) is vindicated and this constraint is correctly typed as snare-via-slack. If the gap persists after equalizing tooling, the standpoint reading is the better fit and beneficiary/victim sets should be re-derived from structural credibility position rather than slack, which could shift stakeholders and possibly the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Which kernel reading of positional_disagreement_as_evidence correctly explains this constraint''s beneficiary/victim structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(menu_curation_capture, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(menu_tr_t0, menu_curation_capture, theater_ratio, 0, 0.28).
narrative_ontology:measurement(menu_tr_t4, menu_curation_capture, theater_ratio, 4, 0.36).
narrative_ontology:measurement(menu_tr_t8, menu_curation_capture, theater_ratio, 8, 0.44).
narrative_ontology:measurement(menu_tr_t12, menu_curation_capture, theater_ratio, 12, 0.51).
narrative_ontology:measurement(menu_tr_t16, menu_curation_capture, theater_ratio, 16, 0.56).
narrative_ontology:measurement(menu_tr_t20, menu_curation_capture, theater_ratio, 20, 0.6).
narrative_ontology:measurement(menu_tr_t24, menu_curation_capture, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(menu_be_t0, menu_curation_capture, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(menu_be_t4, menu_curation_capture, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(menu_be_t8, menu_curation_capture, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(menu_be_t12, menu_curation_capture, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(menu_be_t16, menu_curation_capture, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(menu_be_t20, menu_curation_capture, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(menu_be_t24, menu_curation_capture, base_extractiveness, 24, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(menu_curation_capture, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(menu_curation_capture, information_standard).
narrative_ontology:boltzmann_floor_override(menu_curation_capture, 0.05).
narrative_ontology:affects_constraint(menu_curation_capture, omega_production_cost_asymmetry).

% DUAL FORMULATION NOTE:
% omega_production_cost_asymmetry (upstream, mountain) is the genuine structural fact that generating candidate falsifiers has become cheap via LLM assistance — a mountain because no party's enforcement determines whether inference costs have fallen, and no one collects rent from the cost curve itself. menu_curation_capture (this story) is the DOWNSTREAM discretionary act layered on top: given cheap generation, WHO SELECTS from the resulting menu and WHY is a separate structural question with its own beneficiary/victim structure, its own ε, and its own classification (snare). The two are linked because the upstream mountain is the precondition that makes the downstream capture possible — without cheap generation there would be no menu to curate — but the mountain itself has negligible extraction while the curation layered on it is substantially extractive. This is the ε-invariance decomposition: measuring 'the cost of generating falsifiers' gives a different, much lower ε than measuring 'who gets to pick from what was generated.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
