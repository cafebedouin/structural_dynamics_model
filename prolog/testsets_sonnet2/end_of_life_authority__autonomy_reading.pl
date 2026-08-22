% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Grounded Right to Medically Assisted Death
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the autonomy_reading of the end_of_life_authority
 *   kernel: the claim that individual autonomy grounds a right to control the
 *   circumstances and timing of one's own death when facing unbearable,
 *   irremediable suffering. Under this reading, the standing arrangement
 *   under contest is the current legal default — variously restrictive
 *   statutes and eligibility gates governing assisted dying — assessed by the
 *   autonomy reading's own lights: it sees that default as too narrow,
 *   leaving suffering patients without recourse, and sees paternalistic
 *   restriction as the extractive force to be measured. This is NOT a story
 *   about the sanctity reading (which holds the opposite premise) or the
 *   slippery_slope_mechanism (which is an empirical claim about downstream
 *   eligibility drift, authored as its own sibling constraint). Those are
 *   separate files linked via network.affects_constraints; this file's ε,
 *   beneficiaries, and victims are authored solely from the autonomy premise.
 *
 * KEY AGENTS:
 *   - competent_terminal_patients_seeking_control: primary beneficiary of the recognized right (powerless/trapped) — gains legal authority over own death
 *   - suffering_prolonged_patients_denied_access: primary victim under this reading (powerless/trapped) — suffering continues because eligibility criteria or jurisdictional absence deny the autonomy right
 *   - assisted_dying_practitioners: agenda-setting clinical body administering the criteria (organized/constrained)
 *   - disabled_persons_pressured_by_cost_framing: secondary victim class — bear indirect coercive pressure from the coexistence of the right with underfunded care
 *   - palliative_care_underfunded_by_policy_substitution: institutional payer — competes for policy attention and resources with the assisted-dying apparatus
 *   - judicial_and_legislative_review_bodies: analytical observer seat adjudicating eligibility scope over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.42).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.68).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Grounded Right to Medically Assisted Death").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '4919987e-0813-4358-919d-631cb33d829a').
narrative_ontology:cs_kernel_codification('4919987e-0813-4358-919d-631cb33d829a', distributed).
narrative_ontology:cs_authority_grounding('4919987e-0813-4358-919d-631cb33d829a', distributed).
narrative_ontology:cs_reading_relation('4919987e-0813-4358-919d-631cb33d829a', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('4919987e-0813-4358-919d-631cb33d829a', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('4919987e-0813-4358-919d-631cb33d829a', foundational, self_determination_grounds_death_timing_right).
narrative_ontology:cs_axiom_status(self_determination_grounds_death_timing_right, holdable).
narrative_ontology:cs_axiom_grounding('4919987e-0813-4358-919d-631cb33d829a', self_determination_grounds_death_timing_right, deontological).
narrative_ontology:cs_axiom('4919987e-0813-4358-919d-631cb33d829a', secondary, unbearable_irremediable_suffering_is_sufficient_criterion).
narrative_ontology:cs_axiom_status(unbearable_irremediable_suffering_is_sufficient_criterion, holdable).
narrative_ontology:cs_axiom_grounding('4919987e-0813-4358-919d-631cb33d829a', unbearable_irremediable_suffering_is_sufficient_criterion, empirically_contingent).
narrative_ontology:cs_reference_frame('4919987e-0813-4358-919d-631cb33d829a', blanket_prohibition_baseline).
narrative_ontology:cs_drift_state('4919987e-0813-4358-919d-631cb33d829a', contemporary_multijurisdictional_statute_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4919987e-0813-4358-919d-631cb33d829a', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, competent_terminal_patients_seeking_control).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, assisted_dying_practitioners).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, autonomy_advocacy_organizations).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, suffering_prolonged_patients_denied_access).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, disabled_persons_pressured_by_cost_framing).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, palliative_care_underfunded_by_policy_substitution).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, self_determination_over_bodily_fate).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, suffering_as_sufficient_medical_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Facing a diagnosed terminal illness with a defined suffering trajectory, they seek legal authority to determine the timing and manner of their death rather than have it dictated by disease progression or institutional default. Their only alternative absent this constraint is enduring the full course of the illness or seeking unregulated, riskier means of ending it themselves.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, competent_terminal_patients_seeking_control, beneficiary,
    powerless, immediate, trapped, national).

% Patients who meet the moral case for relief from unbearable suffering but fall outside the enacted eligibility criteria — non-terminal chronic conditions, contested prognosis timelines, or jurisdictions without any statute — continue to suffer under the standing legal default that requires continuation of life absent narrow qualifying conditions. They bear the cost of a right that exists in principle but is gated in practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, suffering_prolonged_patients_denied_access, payer,
    powerless, immediate, trapped, national).

% Physicians and associated clinical bodies administer eligibility assessments, prescribe or administer lethal medication under statute, and shape the professional guidelines that operationalize the autonomy principle into clinical practice. They set the working boundaries of who qualifies, subject to legislative and regulatory oversight, and carry professional and legal risk for each determination.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, assisted_dying_practitioners, agenda_setter,
    organized, biographical, constrained, national).

% Disabled and chronically ill people, particularly those dependent on underfunded care systems, report structural pressure to view assisted death as a rational response to being a burden or a cost, especially where a functioning right to die coexists with inadequate long-term care funding. Their exit from this pressure is limited by the same resource scarcity that generates it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disabled_persons_pressured_by_cost_framing, payer,
    powerless, biographical, constrained, national).

% Advocacy groups campaign for statutory recognition of the autonomy principle, litigate test cases, and lobby for expanded eligibility criteria over successive legislative cycles. They derive institutional purpose and continued relevance from both the existence of the right and its ongoing expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, autonomy_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, autonomy_advocacy_organizations, agenda_setter).

% Palliative and hospice care providers argue that legislative and public attention devoted to assisted-dying statutes substitutes for, rather than complements, investment in pain management and end-of-life support infrastructure. They compete for the same policy attention and budget lines and report that access to good palliative care is often worse in jurisdictions that adopted assisted dying earliest.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_underfunded_by_policy_substitution, payer,
    moderate, generational, constrained, national).

% Coalitions representing disability rights and religious communities argue the autonomy framing obscures coercive social and economic pressures on vulnerable populations and object to statutory expansion; their testimony is frequently entered into legislative record but has limited effect once the autonomy-based statute is enacted and normalized.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_and_disability_rights_coalitions, excluded,
    organized, generational, constrained, national).

% Courts and legislatures periodically review eligibility criteria, hear challenges from both expansion advocates and restriction advocates, and can broaden or narrow the statute's scope. Their rulings determine whether the autonomy principle is read narrowly (terminal, competent adults) or expansively (chronic, psychiatric, or advance-directive cases).
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, judicial_and_legislative_review_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally regulated, clinically supervised pathway for competent adults facing unbearable and otherwise irremediable suffering to end their life on their own terms, replacing unregulated self-harm or unsanctioned covert practice with an accountable, criteria-based process.
% TRANSFER_FUNCTION: Moves decisional authority over the timing and manner of death from the default presumption of continued treatment/state interest in preserving life toward the individual patient; simultaneously moves social and fiscal attention from long-term palliative infrastructure toward eligibility administration for the dying pathway.
% ABSENT_VOICES: Disability rights and religious coalitions object in legislative hearings but hold no veto once statutes pass; suffering patients outside the eligibility line (non-terminal, contested prognosis, psychiatric suffering) have no seat at all — they are neither party to the statute's protections nor represented as its cost-bearers in most public debate.
% DISAPPEARANCE_RATIONALE: If the autonomy-grounded right vanished overnight, patients currently eligible would lose legal access to a supervised death and would face the full course of terminal illness or seek unregulated means; advocacy organizations would lose their central campaign object; practitioners would face renewed legal exposure for any assistance provided; the entire regulatory and eligibility-assessment apparatus built around this right would become inoperative.
% FOUNDING_PROBLEM: Terminally ill, competent patients facing unbearable and irremediable suffering had no legal, supervised way to control the timing of their death and were forced either to endure suffering to its natural conclusion or to seek unregulated, risky, and often traumatic self-directed or covertly assisted deaths.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care researchers and some bioethicists outside the advocacy movement corroborate that the founding problem (uncontrolled suffering at end of life) was real and remains partly live, but argue the autonomy-statute response has, in several jurisdictions, outpaced and substituted for investment in palliative care that would address the same suffering without requiring death as the remedy — a claim contested by advocacy organizations who assert the two are complementary, not substitutive.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because under the autonomy reading itself, the primary harm is not extraction of resources from a captured population but denial of a claimed right to those excluded by eligibility gates — the 'extraction' here is measured as the cost borne by suffering_prolonged_patients_denied_access and the pressured disabled population, not as rent captured by a concentrated beneficiary. Suppression starts high (0.75) reflecting the paternalistic restrictions the autonomy reading identifies as the coercive baseline (blanket prohibition, criminal liability for assistance) and falls modestly over the interval (to 0.68) as statutes are enacted and normalized — suppression here tracks the residual restrictive apparatus still gating access for excluded populations, not the enforcement of the right itself. Theater ratio is low and rising slowly (0.10 to 0.20), reflecting genuine clinical/legal function with a small but growing performative compliance layer (documentation rituals, review boards) as the apparatus matures.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent terminal patients who qualify are the clearest beneficiaries — the right subsidizes their control over dying, d near the beneficiary end. Suffering-prolonged patients denied access sit at the target end: the standing (restrictive) arrangement extracts from them by withholding the remedy the autonomy principle claims they are entitled to; their exit is trapped because there is no legal alternative pathway. Disabled persons under cost-framing pressure are targets by a different mechanism — not denial of the right but structural pressure to exercise it under conditions of scarcity, which the autonomy reading treats as a policy-context harm rather than a flaw in the autonomy principle itself. Practitioners and advocacy organizations are agenda-setters whose position combines administering and campaigning for the right's scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — uncontrolled, unregulated suffering at the end of life — remains partly live: hence founding_problem_status is authored as contested rather than dead. This prevents two mislabeling errors: treating the autonomy right as pure extraction (it is not; it responds to a real and continuing coordination/relief problem) and treating it as a fully resolved, static achievement (it is not; the corroboration record shows the remedy has, per some outside observers, become a substitute for palliative investment rather than a complement, which is itself a form of drift the framework should track via the sibling slippery_slope_mechanism story rather than folding into this one).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_versus_sanctity_framework_incompatibility,
    'Can a single legal framework hold both the autonomy_reading''s premise (individual self-determination grounds the right to control timing of death) and the sanctity_reading''s premise (intrinsic value of life prohibits intentional life-ending) without contradiction?',
    'Comparative jurisprudence analysis: examine whether any enacted statute successfully incorporates both premises as co-equal grounds, or whether every enacted framework resolves the tension by subordinating one premise to the other (e.g. sanctity-as-default with narrow autonomy carve-outs, or autonomy-as-default with sanctity relegated to conscience objection).',
    'If no framework can hold both without one subordinating the other, this confirms the forecloses relation authored in cs_structure.reading_relations between autonomy_reading and sanctity_reading is structurally correct rather than a modeling artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_versus_sanctity_framework_incompatibility, conceptual, 'Whether autonomy and sanctity readings are logically co-holdable in one framework.').

omega_variable(
    eligibility_criteria_as_natural_or_constructed_boundary,
    'Is the current eligibility boundary (competent, terminal, unbearable suffering) a principled application of the autonomy premise, or a politically contingent compromise that the autonomy premise itself provides no principled basis for holding fixed?',
    'Textual and legislative-history analysis of enacted statutes: determine whether the terminal-illness and competence requirements are derived from the autonomy principle itself or are political concessions layered onto it to secure passage.',
    'If the boundary is a political concession rather than a principled derivation, the autonomy premise provides no internal resistance to the eligibility expansion the slippery_slope_mechanism reading documents — meaning that reading''s empirical pattern would be a structurally predictable consequence of this reading''s own premise, not an external distortion of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_criteria_as_natural_or_constructed_boundary, conceptual, 'Whether eligibility limits are principled or contingent under the autonomy premise.').

omega_variable(
    palliative_substitution_causal_mechanism,
    'Does the enactment of an autonomy-grounded assisted-dying statute causally reduce palliative care investment (substitution effect), or is underfunded palliative care an independent, pre-existing condition that the statute merely operates alongside?',
    'Longitudinal comparison of palliative care funding trajectories in jurisdictions before and after statute enactment, against matched jurisdictions without such statutes, controlling for baseline healthcare spending trends.',
    'If a genuine substitution effect is found, the extractiveness borne by palliative_care_underfunded_by_policy_substitution is a direct consequence of this constraint''s operation and should be weighted more heavily in future ε revisions; if independent, that victim class should be attenuated or removed in a future version of this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(palliative_substitution_causal_mechanism, empirical, 'Whether assisted-dying statutes causally displace palliative care investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__autonomy_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__autonomy_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__autonomy_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__autonomy_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__autonomy_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__autonomy_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__autonomy_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__autonomy_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__autonomy_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__autonomy_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__autonomy_reading, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__autonomy_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__autonomy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__autonomy_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__autonomy_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.1).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'the right to die' / 'end-of-life authority' per the ε-invariance principle. autonomy_reading (this file) authors ε=0.42 against the standing restrictive/gated arrangement, with suffering-prolonged patients as victims of denial. sanctity_reading authors a structurally opposite premise and would author its own ε against the standing permissive arrangement, with different victims (those whose deaths are hastened or normalized). slippery_slope_mechanism is an empirical-pattern reading tracking eligibility expansion over time as its own constraint, downstream of autonomy_reading's normalization effect. All three share the kernel_id end_of_life_authority but are NOT merged into one story — each has its own ε, beneficiaries, victims, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
