% ============================================================================
% CONSTRAINT STORY: negotiated_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_negotiated_agency_reading, []).

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
 *   constraint_id: negotiated_agency_reading
 *   human_readable: Negotiated Agency Reading of the Freedom Locus (Bounded Control Within Real Constraint)
 *   domain: applied_philosophy/self_help_ideology
 *
 * SUMMARY:
 *   This story instantiates the 'negotiated agency' reading of the
 *   freedom-locus kernel: the claim that individual control is real and often
 *   underused (Browne's genuine insight about locus of control) but operates
 *   within, not instead of, structural and relational limits. This reading
 *   deliberately positions itself as the synthesis between the
 *   sovereign_agency_reading (control is near-total; structural claims are
 *   excuse-making) and the structural_conditions_reading (structural
 *   conditions dominate; agency-talk is blame-shifting), while also standing
 *   apart from the relational_obligation_reading (freedom is constituted
 *   through obligations to others, not merely bounded by them). Because this
 *   reading concedes ground to both poles without fully adopting either, it
 *   produces the lowest ε among the four siblings — it denies no pole
 *   outright, but its very moderateness is what lets it be sold as an
 *   evidence-based synthesis while still functioning, for a meaningful subset
 *   of payers, as a milder version of the same individualizing extraction the
 *   structural reading names directly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(negotiated_agency_reading, 0.32).
domain_priors:suppression_score(negotiated_agency_reading, 0.38).
domain_priors:theater_ratio(negotiated_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(negotiated_agency_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(negotiated_agency_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(negotiated_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(negotiated_agency_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(negotiated_agency_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(negotiated_agency_reading, tangled_rope).
narrative_ontology:human_readable(negotiated_agency_reading, "Negotiated Agency Reading of the Freedom Locus (Bounded Control Within Real Constraint)").
narrative_ontology:topic_domain(negotiated_agency_reading, "applied_philosophy/self_help_ideology").

domain_priors:requires_active_enforcement(negotiated_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(negotiated_agency_reading, '89220193-8268-4531-a888-0005e27fce45').
narrative_ontology:cs_kernel_codification('89220193-8268-4531-a888-0005e27fce45', distributed).
narrative_ontology:cs_authority_grounding('89220193-8268-4531-a888-0005e27fce45', practice).
narrative_ontology:cs_interpretation_layer_present('89220193-8268-4531-a888-0005e27fce45').
narrative_ontology:cs_reading_relation('89220193-8268-4531-a888-0005e27fce45', negotiated_agency_reading__sovereign_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('89220193-8268-4531-a888-0005e27fce45', negotiated_agency_reading__structural_conditions_reading, coexists_with).
narrative_ontology:cs_reading_relation('89220193-8268-4531-a888-0005e27fce45', negotiated_agency_reading__relational_obligation_reading, influences).
narrative_ontology:cs_axiom('89220193-8268-4531-a888-0005e27fce45', foundational, control_is_real_but_domain_bounded).
narrative_ontology:cs_axiom_status(control_is_real_but_domain_bounded, holdable).
narrative_ontology:cs_axiom_grounding('89220193-8268-4531-a888-0005e27fce45', control_is_real_but_domain_bounded, empirically_contingent).
narrative_ontology:cs_axiom('89220193-8268-4531-a888-0005e27fce45', secondary, boundary_of_agency_must_be_evidence_drawn_not_market_drawn).
narrative_ontology:cs_axiom_status(boundary_of_agency_must_be_evidence_drawn_not_market_drawn, holdable).
narrative_ontology:cs_axiom_grounding('89220193-8268-4531-a888-0005e27fce45', boundary_of_agency_must_be_evidence_drawn_not_market_drawn, instrumental).
narrative_ontology:cs_reference_frame('89220193-8268-4531-a888-0005e27fce45', moderate_locus_of_control_synthesis).
narrative_ontology:cs_drift_state('89220193-8268-4531-a888-0005e27fce45', contemporary_wellness_industry_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89220193-8268-4531-a888-0005e27fce45', '').
narrative_ontology:cs_kernel_id(negotiated_agency_reading, freedom_locus_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(negotiated_agency_reading, self_help_industry_moderates).
narrative_ontology:constraint_beneficiary(negotiated_agency_reading, resilience_coaching_practitioners).
narrative_ontology:constraint_beneficiary(negotiated_agency_reading, individuals_with_marginal_agency_gains).
narrative_ontology:constraint_victim(negotiated_agency_reading, structurally_trapped_low_income_workers).
narrative_ontology:constraint_victim(negotiated_agency_reading, chronically_ill_individuals_told_to_optimize_mindset).
narrative_ontology:constraint_vindicates(negotiated_agency_reading, compatibilist_freedom_thesis).
narrative_ontology:constraint_vindicates(negotiated_agency_reading, bounded_locus_of_control_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and popularize the 'bounded agency' framing — books, seminars, coaching certifications that explicitly acknowledge structural limits while still selling personal-responsibility techniques. They benefit from occupying the reasonable middle ground: neither the harsh 'you're the sole author of your fate' pitch nor the fatalist 'systems determine everything' critique, which lets them retain market share against both poles while deflecting the harder critique that their techniques still individualize what is often a structural problem.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, self_help_industry_moderates, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(negotiated_agency_reading, self_help_industry_moderates, beneficiary).

% Deliver mindset and habit-change coaching under the bounded-agency framing, charging fees for helping clients identify the real (if narrower than advertised) margin of control they have. Their livelihood depends on the reading being true enough to produce results some of the time, which it does for clients whose structural position leaves genuine slack.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, resilience_coaching_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% People whose situation genuinely contains an underused margin of control — habits, framing, negotiation postures, small decisions — and who benefit from having that margin named and worked on rather than either denied (fatalism) or inflated into total responsibility (bootstrap ideology). For them the reading matches lived reality reasonably well.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, individuals_with_marginal_agency_gains, beneficiary,
    moderate, biographical, constrained, local).

% Face wage, scheduling, and housing constraints where the 'bounded' margin the reading concedes is real but so thin that coaching interventions aimed at their mindset or negotiation posture cannot move the outcome that matters (rent, hours, food security). The reading's acknowledgment of structural limits is honest in principle but in practice still directs some of their attention and money toward agency-work rather than toward the structural fight, because the industry built on this reading still needs a product to sell them.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, structurally_trapped_low_income_workers, payer,
    powerless, immediate, trapped, national).

% Live with conditions where the honestly bounded reading still gets applied past its own stated limit — coaches and literature built on 'you have more control than you think' bleed into implying the remaining suffering is a failure to locate that control, even when the reading's own theory says some walls are real. They pay in time, money, and self-blame for a margin of control that, for their specific condition, may be near zero.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, chronically_ill_individuals_told_to_optimize_mindset, payer,
    powerless, biographical, trapped, national).

% Hold the stronger reading that control is close to total and structural claims are largely excuse-making. They are not in this constraint's stakeholder set by construction — they occupy the sibling sovereign_agency_reading — but they contest this reading's concession of any trap as a capitulation that undersells what deliberate practice can overcome.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, sovereign_agency_advocates, excluded,
    organized, generational, analytical, national).

% Hold the sibling structural_conditions_reading — that systemic conditions are the dominant determinant and individual-agency framing is itself a mechanism that shifts blame downward. They are not part of this reading's stakeholder set; they contest this reading's claim that the bounded frame is genuinely balanced rather than a soft version of the same individualizing move.
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, structural_critics, excluded,
    organized, generational, analytical, national).

% Study compatibilist and libertarian free-will positions, locus-of-control psychology, and the empirical literature on self-efficacy interventions. They can assess where the bounded-agency claim tracks evidence (some domains) and where it is stretched to cover domains it does not fit (chronic illness, poverty traps).
narrative_ontology:constraint_stakeholder(negotiated_agency_reading, philosophy_of_action_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(negotiated_agency_reading, self_help_industry_moderates).
narrative_ontology:fixing_cost_class(negotiated_agency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a middle framework that lets individuals identify and act on the real margin of control available to them without either denying agency entirely (fatalism, which is demotivating and often false) or claiming total agency (which is cruel and often false). This genuinely helps people whose situation contains meaningful unused slack.
% TRANSFER_FUNCTION: Moves attention, coaching fees, and self-attribution of outcomes toward individuals who occupy a moderate structural position, while providing rhetorical cover that lets the same framework be sold — at real cost in money, time, and self-blame — to individuals whose structural position leaves little or no exploitable margin.
% ABSENT_VOICES: Sovereign-agency advocates and structural critics are both excluded from this reading's own frame by construction: the bounded reading positions itself as the reasonable synthesis, which lets it avoid direct confrontation with either pole's strongest objections. Structurally trapped payers rarely get to define where the boundary of 'bounded' actually sits — the industry and the moderate coaches set that boundary.
% DISAPPEARANCE_RATIONALE: Practitioners and moderate self-help authors would say the coaching and framing infrastructure would need to be rebuilt in some other form because the underlying psychological reality (some real margin of control exists) would still need addressing. Structural critics would say little would change for the structurally trapped, because the interventions built on this reading rarely move the variables that matter for them; the dispute over which is true is itself part of what the kernel leaves open.
% FOUNDING_PROBLEM: Neither 'you control everything' (which produces cruelty toward the genuinely constrained) nor 'you control nothing' (which produces learned helplessness even among those with real slack) matched observed variation in how much control people actually had over their outcomes. The bounded reading was built to locate the empirically defensible middle.
% FOUNDING_PROBLEM_CORROBORATION: Locus-of-control and self-efficacy researchers outside the self-help industry corroborate that measured control varies substantially by domain and structural position, supporting the reading's core claim in the abstract. However, labor economists and disability advocates — also outside the benefiting parties — attest that in practice the reading's boundary between 'genuine margin' and 'real wall' gets drawn generously in favor of sellable interventions and stingily in favor of naming intractable structural conditions, which is exactly the coordination/extraction seam this reading's classification is meant to surface.
narrative_ontology:disappearance_verdict(negotiated_agency_reading, contested).
narrative_ontology:founding_problem_status(negotiated_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(negotiated_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-10',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(negotiated_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(negotiated_agency_reading, 0.32, 'claude-sonnet-5', 'harry_browne_freedom_kernel_2026_20260810_020156', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(negotiated_agency_reading_tests).
:- end_tests(negotiated_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) sits low relative to what a pure bootstrap ideology or a pure fatalist ideology would score, because this reading's core claim — that some real margin of control exists in most human situations — is genuinely true across a wide range of cases and is not merely cover. Suppression (0.38) reflects the reading's tendency to discourage full engagement with either the strongest sovereign-agency claims or the strongest structural claims, since occupying the middle ground requires not fully validating either flank. Theater ratio (0.28) captures a moderate but real gap between the coaching/self-help apparatus's stated purpose (locating genuine agency margin) and its actual function (retaining market share by sounding reasonable), which grows slowly across the interval as the industry professionalizes its moderate-positioning language.
 *
 * DIRECTIONALITY LOGIC:
 *   Self-help industry moderates and coaching practitioners sit near the beneficiary end: they set the boundary of what counts as 'bounded' and collect fees for helping people work that boundary. Individuals with genuine marginal agency gains are near-symmetric beneficiaries — the reading matches their situation reasonably well, so directionality here is close to neutral. Structurally trapped low-income workers and chronically ill individuals sit toward the target end: trapped exit options and a boundary drawn generously toward 'buy the intervention' rather than 'name the wall' push their effective extraction upward even though the reading, unlike the sovereign_agency_reading, formally concedes their trap is real.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — locating the empirically real margin of control without falling into either cruelty or fatalism — remains partially live: locus-of-control variation is a real, well-corroborated phenomenon. What has drifted is not the problem's existence but the boundary-drawing practice: the industry built on this reading has an incentive to draw the 'genuine margin' line generously enough to keep selling interventions, even into domains (chronic illness, poverty traps) where independent corroborators (labor economists, disability advocates) say the wall is closer to total. This is not full mandatrophy — the founding problem is still partly live and the reading's core claim is not false — but it is a live site of drift the classification is built to surface rather than average away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_drawing_generosity,
    'Is the line this reading draws between ''genuine exploitable margin of control'' and ''real structural wall'' drawn by evidence, or by what is commercially sellable as an intervention?',
    'Compare, across matched structural positions (income, health status), the industry''s claimed agency-margin against independent empirical measures (randomized intervention effect sizes, labor-market mobility studies) to see whether claimed margin tracks measured margin or tracks what generates coaching revenue.',
    'If the boundary tracks revenue more than evidence, the reading functions as a milder version of the sovereign_agency_reading''s extraction dressed in structural-acknowledgment language, raising its effective ε closer to that sibling. If it tracks evidence, the reading''s low-ε claim is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_drawing_generosity, empirical, 'Whether the reading''s agency/wall boundary is evidence-driven or revenue-driven.').

omega_variable(
    synthesis_vs_soft_extraction,
    'Is the negotiated-agency reading a genuine philosophical synthesis, or a rhetorically stabilized middle position that survives specifically because it is harder to attack than either pole?',
    'Track argumentative survival: does the reading update its boundary when confronted with strong structural counter-evidence (e.g., in chronic illness or poverty-trap domains), or does it retreat to vague language (''everyone has some control'') that is unfalsifiable in practice?',
    'If the reading updates, it behaves as genuine philosophy under evidentiary pressure — closer to a rope than a tangled_rope. If it retreats into unfalsifiable vagueness under pressure specifically in the domains where it is most contested, that vagueness is itself doing extractive work by insulating the coaching product from disconfirmation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_vs_soft_extraction, conceptual, 'Whether the reading''s moderateness is genuine synthesis or a self-protecting rhetorical position.').

omega_variable(
    kernel_framing_underdetermination,
    'Should this reading be evaluated primarily against the sovereign_agency_reading (as a corrective to overclaiming) or against the structural_conditions_reading (as a milder version of the same individualizing move)?',
    'This is a framing choice, not an empirical fact: the reading was authored here treating the structural_conditions_reading comparison as primary (hence relatively low ε, since it does formally concede real traps), but an author foregrounding the sovereign_agency_reading comparison might assign it higher ε as ''individualizing extraction wearing a moderate hat.''',
    'Under the structural-conditions-primary framing (adopted here), this reading classifies as tangled_rope with moderate-low ε. Under a sovereign-agency-primary framing, the same structural facts could support a higher ε and a reading closer to snare, since the concession of ''some real walls'' would be read as a legitimating veneer rather than a genuine limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative committer framings of this reading relative to its two closest siblings produce different plausible ε assignments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(negotiated_agency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nego_tr_t0, negotiated_agency_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(nego_tr_t8, negotiated_agency_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(nego_tr_t16, negotiated_agency_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(nego_tr_t24, negotiated_agency_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(nego_tr_t32, negotiated_agency_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(nego_tr_t40, negotiated_agency_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(nego_be_t0, negotiated_agency_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nego_be_t8, negotiated_agency_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(nego_be_t16, negotiated_agency_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(nego_be_t24, negotiated_agency_reading, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(nego_be_t32, negotiated_agency_reading, base_extractiveness, 32, 0.31).
narrative_ontology:measurement(nego_be_t40, negotiated_agency_reading, base_extractiveness, 40, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(negotiated_agency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(negotiated_agency_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(negotiated_agency_reading, 0.1).
narrative_ontology:affects_constraint(negotiated_agency_reading, sovereign_agency_reading).
narrative_ontology:affects_constraint(negotiated_agency_reading, structural_conditions_reading).
narrative_ontology:affects_constraint(negotiated_agency_reading, relational_obligation_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of freedom_locus_kernel, each authored as a separate constraint per the eps-invariance principle: sovereign_agency_reading (control is near-total; highest eps, sharpest victim set among the structurally trapped), structural_conditions_reading (structure dominates; treats agency-talk itself as the extraction mechanism), relational_obligation_reading (freedom is constituted through obligation, not merely bounded by it — different axis entirely), and this negotiated_agency_reading (lowest eps among the four, because it denies no pole outright but is exactly for that reason the hardest to falsify when its boundary-drawing serves commercial interests). All four link to each other via affects_constraints; none is the 'correct' resolution of the kernel — each is a structurally distinct constraint with its own beneficiary/victim map.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
