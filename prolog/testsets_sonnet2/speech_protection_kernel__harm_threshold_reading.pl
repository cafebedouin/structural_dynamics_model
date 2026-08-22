% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Harm-Threshold Reading of Speech Protection
 *   domain: constitutional/political philosophy
 *
 * SUMMARY:
 *   This story instantiates the harm-threshold reading of the speech
 *   protection kernel: protection is conditional, not categorical — a
 *   speaker's expression loses its presumptive protection once a victim can
 *   demonstrate concrete harm. This is structurally distinct from the
 *   absolutist reading (which treats listener harm as never sufficient
 *   grounds for restriction), the marketplace reading (which trusts
 *   counter-speech rather than legal remedy to correct harm), the dignity
 *   reading (which keys the boundary to structural subordination rather than
 *   case-by-case demonstrable injury), and the democratic-participation
 *   reading (which keys protection strength to political self-governance
 *   value rather than harm avoidance). Each of those is a separate constraint
 *   with its own ε; this file authors only the harm-threshold reading's
 *   operation as the standing arrangement under contest.
 *
 * KEY AGENTS:
 *   - demonstrable_harm_victims: primary beneficiary — gains legal recourse
 *   - targeted_minority_groups: beneficiary — gains standing against subordinating speech
 *   - adjudicating_courts_and_tribunals: agenda_setter — defines and applies the threshold
 *   - speakers_facing_harm_liability: primary payer — bears litigation risk and chilling uncertainty
 *   - controversial_advocacy_organizations: payer — elevated exposure due to provocative content
 *   - satirists_and_provocateurs: most vulnerable payer — powerless, trapped, cannot afford to litigate the boundary
 *   - absolutist_and_marketplace_theorists: excluded — critique the doctrine from outside its administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.44).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.52).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Harm-Threshold Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional/political philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, 'd083accf-d5f9-46fd-969f-230a1874ed4a').
narrative_ontology:cs_kernel_codification('d083accf-d5f9-46fd-969f-230a1874ed4a', distributed).
narrative_ontology:cs_authority_grounding('d083accf-d5f9-46fd-969f-230a1874ed4a', practice).
narrative_ontology:cs_interpretation_layer_present('d083accf-d5f9-46fd-969f-230a1874ed4a').
narrative_ontology:cs_reading_relation('d083accf-d5f9-46fd-969f-230a1874ed4a', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('d083accf-d5f9-46fd-969f-230a1874ed4a', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('d083accf-d5f9-46fd-969f-230a1874ed4a', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d083accf-d5f9-46fd-969f-230a1874ed4a', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('d083accf-d5f9-46fd-969f-230a1874ed4a', foundational, demonstrable_victim_harm_defeats_speaker_autonomy).
narrative_ontology:cs_axiom_status(demonstrable_victim_harm_defeats_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('d083accf-d5f9-46fd-969f-230a1874ed4a', demonstrable_victim_harm_defeats_speaker_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('d083accf-d5f9-46fd-969f-230a1874ed4a', secondary, case_by_case_injury_finding_over_categorical_rule).
narrative_ontology:cs_axiom_status(case_by_case_injury_finding_over_categorical_rule, holdable).
narrative_ontology:cs_axiom_grounding('d083accf-d5f9-46fd-969f-230a1874ed4a', case_by_case_injury_finding_over_categorical_rule, instrumental).
narrative_ontology:cs_reference_frame('d083accf-d5f9-46fd-969f-230a1874ed4a', harm_conditional_protection_baseline).
narrative_ontology:cs_drift_state('d083accf-d5f9-46fd-969f-230a1874ed4a', contemporary_online_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d083accf-d5f9-46fd-969f-230a1874ed4a', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, demonstrable_harm_victims).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, adjudicating_courts_and_tribunals).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, speakers_facing_harm_liability).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, controversial_advocacy_organizations).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, satirists_and_provocateurs).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, harm_principle_as_speech_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups who can document concrete injury from targeted speech (defamation, incitement, harassment campaigns, discriminatory targeting) gain a legal pathway to restrict or seek remedy against the speech that harmed them. Their protection depends on being able to prove harm to an evidentiary standard set by courts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, demonstrable_harm_victims, beneficiary,
    moderate, biographical, constrained, national).

% Groups historically subject to speech-enabled subordination (hate speech, harassment, targeted disinformation) gain standing to argue that certain speech categories cross the harm threshold and lose protection. They benefit from a doctrine that takes their claimed injury seriously as a legal fact rather than dismissing it as offense.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, targeted_minority_groups, beneficiary,
    organized, generational, constrained, national).

% Set and apply the harm threshold case by case: what counts as demonstrable, what evidentiary showing suffices, which harms (physical, psychological, reputational, dignitary) qualify. They administer the boundary and could redraw it broader or narrower; their discretion is the operative mechanism of the whole reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, adjudicating_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, national).

% Individuals whose speech is alleged to cause harm bear the burden of litigation, potential liability, and chilling uncertainty about where the threshold sits. Because harm is contested and fact-specific, they cannot know in advance whether their speech will be protected until after the cost of a claim has been incurred.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, speakers_facing_harm_liability, payer,
    moderate, biographical, constrained, national).

% Groups engaged in provocative political, religious, or social advocacy face elevated exposure because their speech is more likely to be alleged as harmful by opposing constituencies. They can self-censor to avoid liability or continue and absorb litigation risk; genuine exit from the public arena defeats their purpose.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, controversial_advocacy_organizations, payer,
    organized, biographical, constrained, national).

% Individual satirists, comedians, and provocateurs lack institutional backing to defend harm claims and face the threshold's chilling effect most acutely: any claimed offense can be reframed as demonstrable psychological harm, and they typically cannot afford to litigate the boundary even when they would likely prevail.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, satirists_and_provocateurs, payer,
    powerless, biographical, trapped, national).

% Legal scholars and civil libertarians who hold that the harm threshold imports a heckler's veto and invites courts to substitute contested value judgments for objective harm findings are not the ones administering the doctrine; their objection surfaces mainly in dissents, law review critique, and losing briefs rather than in the operative rule.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, absolutist_and_marketplace_theorists, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for weighing speaker autonomy against concrete injury to identifiable victims, allowing courts to restrict specific harmful speech acts (targeted harassment, defamation, incitement, non-consensual disclosure) without categorically restricting speech as such.
% TRANSFER_FUNCTION: Moves the burden of proof and litigation risk from harmed parties (who would otherwise have no recourse against speech-based injury) onto speakers whose expression is alleged to cross the harm threshold; moves adjudicative discretion to courts and tribunals who define what counts as demonstrable harm.
% ABSENT_VOICES: Absolutist and marketplace-of-ideas theorists who view the threshold as a standing invitation for courts and complainants to relabel disagreement or offense as harm are structurally excluded from administering the doctrine — they appear as dissenting voices and academic critics, not as the seats setting the rule.
% DISAPPEARANCE_RATIONALE: If the harm threshold vanished overnight, victims of targeted harassment, defamation, and discriminatory speech campaigns would lose their primary legal pathway to remedy or restriction, while speakers currently facing liability exposure or self-censoring under uncertainty would gain unambiguous protection — both litigation patterns and speech behavior would visibly shift.
% FOUNDING_PROBLEM: Pure speaker-autonomy doctrines left victims of demonstrably injurious speech (defamation, targeted harassment, incitement to violence) with no legal recourse, treating all speech as categorically equivalent regardless of its concrete effects on identifiable people.
% FOUNDING_PROBLEM_CORROBORATION: Tort and civil rights litigators and harm-claiming plaintiffs attest the problem remains live — courts continue to see cases where speech causes documented injury. Civil liberties organizations and free-expression scholars, from outside the beneficiary set, attest that the doctrine has drifted from remedying concrete injury toward validating subjective offense as harm, corroborated by documented chilling-effect studies and appellate reversals narrowing overbroad harm findings.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).
:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.44 by interval end) because the harm threshold genuinely screens out some speech-based injury claims and does not operate as blanket suppression, but it steadily transfers litigation risk and self-censorship costs onto speakers whose expression is contestable rather than clearly injurious. Suppression sits at 0.52 because the threshold's vagueness (what counts as 'demonstrable') itself functions as a chilling mechanism independent of any given case's outcome — speakers self-censor against uncertain future liability. Theater ratio is comparatively low (0.28) because the doctrine does perform real adjudicative work in genuine harm cases; the theatrical component is the residual share of cases where the harm claim functions more as a proxy for viewpoint suppression than genuine injury-finding. All three series share one time grid (0/8/16/24/32/40) so no metric is projected from an off-grid endpoint.
 *
 * PERSPECTIVAL GAP:
 *   From the victim/beneficiary seat this reading looks like coordination correcting a real gap in an autonomy-only framework — a rope repairing a mountain-shaped blind spot. From the payer seat (particularly powerless speakers without litigation resources) the same threshold looks like an extraction mechanism where any claimed offense can be relabeled as demonstrable harm, converting a narrow remedial doctrine into broad discretionary suppression. The engine computes both seat types from the same structural data; the divergence is exactly the point of a harm-conditional reading — it is designed to coordinate for some and extract from others by the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Demonstrable harm victims and targeted minority groups are structural beneficiaries: the doctrine exists to give them standing, so their directionality sits near the subsidized end. Adjudicating courts hold agenda-setting power with analytical exit — they administer the boundary and bear no direct cost from it either way. Speakers facing harm liability and advocacy organizations are structural targets: the same threshold that grants victims recourse is the mechanism that exposes them to liability and chilling pressure. Satirists and provocateurs are the most exposed payer class — powerless and trapped, since they lack institutional resources to litigate a threshold that is, for well-resourced speakers, merely a cost of doing advocacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (victims of speech-caused injury lacking legal recourse under absolute-protection regimes) remains partially live — genuine harassment, defamation, and incitement cases still occur — but the doctrine's application has, per critics outside the beneficiary set, drifted toward validating contested offense claims as demonstrable harm, which is a different function than the one that founded it. This is not full mandatrophy (the founding problem has not fully disappeared) but a partial function-drift that the tangled_rope classification is built to hold: real coordination (screening genuine injury) coexists with real extraction (chilling contestable but non-injurious speech) inside the same enforced structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_evidentiary_standard,
    'Where exactly does ''demonstrable'' sit on the spectrum from documented physical/economic injury to subjective reported psychological distress, and does that placement vary systematically by who is bringing the claim?',
    'Empirical audit of case outcomes: compare evidentiary showings actually accepted as sufficient across claimant types (institutional plaintiffs vs. individual claimants, majority vs. minority claimants) to detect whether the threshold is applied consistently or discretion tracks claimant power.',
    'If the threshold is applied consistently regardless of claimant power, the doctrine functions closer to genuine coordination (rope-like). If acceptance systematically tracks claimant power or popularity of the underlying viewpoint, the doctrine functions closer to viewpoint-selective extraction (snare-like), even while retaining its coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrable_harm_evidentiary_standard, empirical, 'Whether the harm threshold''s evidentiary bar is applied uniformly or tracks claimant power/popularity.').

omega_variable(
    chilling_effect_vs_actual_liability,
    'How much of the measured suppression comes from actual adverse judgments versus anticipatory self-censorship by speakers uncertain where the threshold sits?',
    'Survey data on self-reported self-censorship among advocacy organizations and individual speakers, cross-referenced against actual case win/loss rates for harm claims in the same jurisdiction and period.',
    'If chilling vastly exceeds actual liability, the doctrine''s suppressive effect is substantially disproportionate to its genuine remedial function, strengthening the case that the threshold''s vagueness is doing extractive work independent of any individual adjudication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_vs_actual_liability, empirical, 'Whether suppression is driven mainly by actual judgments or by anticipatory self-censorship under threshold uncertainty.').

omega_variable(
    reading_selection_as_framing_choice,
    'Is the harm-threshold reading itself a neutral doctrinal choice among the five kernel readings, or does selecting it (over absolutist or marketplace readings) already encode a contestable value judgment about which harms count as speech-defeating?',
    'Comparative jurisprudential analysis: track which jurisdictions adopt harm-threshold doctrine versus absolutist or marketplace doctrine, and whether the adoption correlates with independent variables (political culture, degree of minority-group political power) rather than emerging from harm-avoidance reasoning alone.',
    'If reading-selection correlates with political power distribution rather than principled harm-avoidance reasoning, that supports treating the choice among kernel readings as itself a site of contest rather than a settled interpretive question — reinforcing the need to keep the five readings as separate constraints rather than resolving them into one ''correct'' speech-protection doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_framing_choice, conceptual, 'Whether adopting the harm-threshold reading over its siblings is itself a value-laden, power-correlated choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__harm_threshold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__harm_threshold_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__harm_threshold_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__harm_threshold_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__harm_threshold_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__harm_threshold_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 40, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__harm_threshold_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language label 'speech protection kernel.' Each reading (absolutist, marketplace, dignity, harm_threshold, democratic_participation) instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle. They are linked bidirectionally via affects_constraints because a shift in any one reading's doctrinal dominance (e.g. courts moving toward harm-threshold reasoning) structurally pressures the operating space of the others (narrowing what absolutist or marketplace reasoning can still claim as protected).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
