% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Irrevocable Natural/Divine Mandate Excluding Female Succession
 *   domain: constitutional/dynastic/religious-political
 *
 * SUMMARY:
 *   This story instantiates the 'immutable mandate' reading of the Salic
 *   prohibition kernel: the claim that exclusion of women (and transmission
 *   through women) from dynastic succession is not a revisable statute of
 *   positive law but an irrevocable feature of the fundamental constitution
 *   of the realm, grounded in natural or divine law. Under this reading, no
 *   sovereign — however absolute — has the legal power to waive, suspend, or
 *   legislate around the exclusion; attempted female succession is not merely
 *   disfavored but categorically void, and war to enforce agnatic priority
 *   against a female or cognatic claimant is not aggression but restoration
 *   of lawful order. This is a distinct constraint from the
 *   'sovereign_override_reading' (in which the rule is ordinary positive law
 *   a king could in principle repeal) and from the
 *   'cognatic_reversion_reading' (in which the rule is a Frankish
 *   parochialism with no binding force outside Francia) — those are separate
 *   stories with separate epsilon values, linked here by network edges, not
 *   folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.68).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.79).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Irrevocable Natural/Divine Mandate Excluding Female Succession").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic/religious-political").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'cd646e5c-94b8-45c6-97c1-e13de96927ab').
narrative_ontology:cs_kernel_codification('cd646e5c-94b8-45c6-97c1-e13de96927ab', formalized).
narrative_ontology:cs_authority_grounding('cd646e5c-94b8-45c6-97c1-e13de96927ab', lineage).
narrative_ontology:cs_interpretation_layer_present('cd646e5c-94b8-45c6-97c1-e13de96927ab').
narrative_ontology:cs_reading_relation('cd646e5c-94b8-45c6-97c1-e13de96927ab', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('cd646e5c-94b8-45c6-97c1-e13de96927ab', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('cd646e5c-94b8-45c6-97c1-e13de96927ab', foundational, succession_law_binds_sovereign_as_natural_law).
narrative_ontology:cs_axiom_status(succession_law_binds_sovereign_as_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('cd646e5c-94b8-45c6-97c1-e13de96927ab', succession_law_binds_sovereign_as_natural_law, deontological).
narrative_ontology:cs_axiom('cd646e5c-94b8-45c6-97c1-e13de96927ab', foundational, female_incapacity_for_sovereign_transmission).
narrative_ontology:cs_axiom_status(female_incapacity_for_sovereign_transmission, holdable).
narrative_ontology:cs_axiom_grounding('cd646e5c-94b8-45c6-97c1-e13de96927ab', female_incapacity_for_sovereign_transmission, conventional).
narrative_ontology:cs_axiom('cd646e5c-94b8-45c6-97c1-e13de96927ab', secondary, preventive_war_licensed_to_restore_lawful_agnatic_order).
narrative_ontology:cs_axiom_status(preventive_war_licensed_to_restore_lawful_agnatic_order, holdable).
narrative_ontology:cs_axiom_grounding('cd646e5c-94b8-45c6-97c1-e13de96927ab', preventive_war_licensed_to_restore_lawful_agnatic_order, instrumental).
narrative_ontology:cs_reference_frame('cd646e5c-94b8-45c6-97c1-e13de96927ab', agnatic_natural_law_supremacy).
narrative_ontology:cs_drift_state('cd646e5c-94b8-45c6-97c1-e13de96927ab', post_hundred_years_war_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cd646e5c-94b8-45c6-97c1-e13de96927ab', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, royal_council_incumbents).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, clerical_jurists_of_succession).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs_and_their_lineages).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_claimant_territories).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, subjects_conscripted_for_succession_wars).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, fundamental_law_of_the_realm_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, gender_incapacity_for_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the throne or a strengthened claim to it purely by virtue of unbroken male-line descent. They invoke the mandate reading to foreclose rival claims from sisters, daughters, or cognatic cousins without having to argue merit, precedent flexibility, or sovereign discretion — the exclusion does the argumentative work for them.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants, beneficiary,
    powerful, civilizational, arbitrage, national).

% Jurists, peers, and court theologians who administer the fundamental-law doctrine, ruling on genealogies and issuing the theological/legal opinions that declare the prohibition binding on the crown itself, not merely on statute. They control the interpretive machinery and benefit from the stability and deference their gatekeeping role commands.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, royal_council_incumbents, agenda_setter,
    institutional, generational, arbitrage, national).

% Supply the natural-law and divine-law reasoning that elevates a Frankish inheritance custom into an unamendable feature of the constitutional order. Their theological authority is enhanced by the claim that this is a matter of divine ordination rather than a revisable statute — it makes their interpretive monopoly permanent rather than contingent on royal favor.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, clerical_jurists_of_succession, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, clerical_jurists_of_succession, agenda_setter).

% Categorically barred from the succession regardless of birth order, personal capacity, or the explicit wishes of a dying monarch. Their exclusion is total and admits no case-by-case appeal, since the mandate reading holds the bar to be a fact of the constitutional order rather than a policy that could be waived for a particular candidate.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs_and_their_lineages, payer,
    powerless, biographical, trapped, national).

% Neighboring or annexed territories whose own succession customs permit female or cognatic inheritance find their claims delegitimized whenever they intersect with a throne governed by the mandate reading. They can contest militarily or diplomatically but cannot appeal to a sovereign's discretion, since the reading denies any sovereign the power to waive it.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_claimant_territories, payer,
    moderate, generational, constrained, continental).

% Ordinary soldiers and taxpayers bear the cost of wars fought to enforce agnatic priority against female or cognatic claimants (the archetype being the Hundred Years' War succession dispute). They have no voice in the doctrinal dispute but pay in blood and treasure for its enforcement.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, subjects_conscripted_for_succession_wars, payer,
    powerless, biographical, trapped, national).

% Houses whose claim runs through a female line are excluded from the succession conversation entirely under this reading — not defeated on the merits, but ruled structurally ineligible to be heard. Their objection is that the 'immutability' framing was itself a later invention layered onto a customary practice.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, rival_dynastic_houses, excluded,
    powerful, generational, constrained, continental).

% Trace how the exclusionary custom was retroactively elevated into 'fundamental law' rhetoric centuries after its practical origin, and compare it against jurisdictions that never adopted the mandate framing at all.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an unambiguous, non-negotiable succession rule that forecloses the very succession disputes and civil wars that ambiguous or contestable inheritance rules tend to produce — a bright-line test removes bargaining space that could otherwise be exploited by rival claimants.
% TRANSFER_FUNCTION: Moves the crown, its lands, and its revenues along the male line exclusively, transferring political standing, marriage-market value as consort rather than sovereign, and inheritance rights away from female descendants and toward male collaterals, sometimes quite distant ones, ahead of closer female relations.
% ABSENT_VOICES: Female heirs themselves are never seated at the interpretive table that declares the exclusion divinely or naturally mandated — the doctrine is elaborated entirely by male jurists and clergy who are also its beneficiaries. Cognatic-claimant territories are heard only through the arbitration of war, not through any recognized legal forum this reading admits.
% DISAPPEARANCE_RATIONALE: If the immutable-mandate framing collapsed, female and cognatic claimants would immediately become legally cognizable, succession disputes would be litigated on genealogical proximity rather than sex, and the entire apparatus of preventive war to enforce agnatic priority would lose its legal justification overnight — inheritance patterns, marriage alliances, and the war-making calculus of every neighboring dynasty would reorganize around the changed rule.
% FOUNDING_PROBLEM: Medieval dynasties faced chronic succession crises when multiple claimants of comparable legitimacy could contest a throne; a rigid rule that removed an entire class of otherwise-qualified claimants (women) reduced the number of viable claimants and, in principle, reduced war.
% FOUNDING_PROBLEM_CORROBORATION: The clerical jurists and agnatic claimants who administer and benefit from the doctrine attest that it remains necessary to prevent succession chaos. Independent constitutional historians, writing from outside the beneficiary class, document that the 'immutable natural/divine law' framing was a later theological gloss added centuries after the original Frankish customary rule, and that it in fact generated some of the bloodiest succession wars in European history (the Hundred Years' War chief among them) rather than preventing them — undercutting the corroboration offered by the doctrine's own administrators.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs across the interval (0.35 to 0.68) as the doctrine hardens from a customary practical rule into an entrenched theological claim used to justify escalating wars of enforcement — the mandate framing itself becomes an instrument that jurists and agnatic claimants deploy, rather than a neutral description of prior custom. Theater ratio rises in parallel (0.15 to 0.42) as the 'natural/divine law' rhetoric increasingly substitutes learned theological argument for the original practical rationale (avoiding succession crises), a rationale the doctrine's own history (the Hundred Years' War) undermines. Suppression is high and rising (0.79 at interval end) because the mandate reading's defining feature is that it forecloses appeal altogether — no discretion, no waiver, no negotiated exception — which requires active theological and military policing to sustain against constant pressure from disinherited female-line claimants.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the royal council and clerical jurists, the mandate reading looks like Mountain-adjacent natural law — an unchangeable feature of dynastic order that simply describes how legitimate succession works, defended because it is true, not because anyone profits from it. From the seat of a disinherited female heir or a cognatic-claimant territory facing invasion to enforce the exclusion, the same structure is naked extraction dressed in theological language: a coordination story (avoid succession chaos) whose real function is to guarantee a fixed set of beneficiaries regardless of outcome. The engine's per-seat computation is expected to diverge sharply here, and that divergence is the finding, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male claimants and the clerical/legal apparatus that elaborates the mandate doctrine sit near the full-beneficiary end: they receive the throne or the durable interpretive monopoly, and their exit options are effectively arbitrage-grade since they can always fall back on the doctrine to settle a contest in their favor. Female heirs and cognatic-territory claimants sit near the full-target end: trapped or constrained, with no legal forum available to contest an exclusion the doctrine holds to be a fact of nature rather than a policy. Conscripted subjects are targets of a second order — they pay in war casualties for a dispute in whose resolution they have no interpretive standing at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reducing succession disputes among comparable claimants — was at least plausible when first practiced as informal custom. By the era in which jurists elevate it to 'irrevocable natural/divine law,' the doctrine has demonstrably failed its own stated purpose (the Hundred Years' War is the paradigm counter-case: the mandate framing did not prevent a succession war, it fueled one for over a century). Treating the doctrine as tangled_rope rather than snare acknowledges that a genuine coordination function existed at origin and that some current beneficiaries sincerely believe the doctrine still serves it; but the requirement of active theological and military enforcement to sustain a categorical, non-negotiable exclusion — one from which the enforcing class exclusively benefits — is exactly the asymmetric-extraction signature the tangled_rope gate is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_theological_retrofit,
    'Is the exclusion of female succession a genuine feature of natural or divine law, as the mandate reading asserts, or a later theological retrofit onto a much narrower and more contingent Frankish inheritance custom?',
    'Comparative legal history: trace the documentary record of when ''immutable natural law'' language first appears relative to the original Salic Law text (a Frankish civil code with no explicit succession-to-the-crown provision), and compare against jurisdictions with cognatic or absolute-primogeniture succession that faced comparable or lower rates of succession crisis.',
    'If the natural-law claim is a retrofit, the mandate reading is a false summit — a constructed extraction mechanism dressed as an eternal feature of reality — reinforcing the tangled_rope/false-summit reading over any mountain framing. If genuine natural-law content is found, the classification would need to weigh a real coordination floor against the extraction evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_theological_retrofit, empirical, 'Whether the immutable-mandate framing reflects genuine natural law or a later constructed doctrine.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the immutable_mandate_reading''s core premise conflict with the sovereign_override_reading''s core premise, and is that conflict total or partial?',
    'Compare the two readings'' treatment of a single historical test case (e.g., a monarch''s attempt to legitimate a daughter''s succession by decree) — under the mandate reading such a decree is void ab initio; under the override reading it is a valid, if contested, exercise of sovereign legislative power. The disagreement is located precisely at the question of whether sovereignty includes the power to alter fundamental succession law.',
    'If the conflict is total (as this story treats it), the two readings genuinely foreclose one another and cannot be held simultaneously within a single legal framework, which is the basis for the forecloses relation declared in cs_structure. If the conflict is only partial (e.g., the mandate reading permits narrow sovereign discretion at the margins), the relation should soften toward influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Precise location and totality of the doctrinal conflict between the mandate and override readings.').

omega_variable(
    preventive_war_legitimacy_ambiguity,
    'Does the mandate reading''s licensing of preventive war to enforce agnatic priority constitute a genuine feature of the doctrine, or a post-hoc justification that particular claimants invoked opportunistically?',
    'Examine whether wars framed as enforcing Salic priority were initiated by parties who stood to gain the throne directly, versus disinterested third parties acting purely on doctrinal principle — the pattern of self-interested invocation would support a debunking reading.',
    'If preventive war was invoked only by self-interested claimants, this strengthens the case that the ''immutable mandate'' framing is functionally indistinguishable from ordinary dynastic war-making dressed in legal language, deepening the tangled_rope/snare-adjacent reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preventive_war_legitimacy_ambiguity, empirical, 'Whether preventive-war invocation tracks doctrine or self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(sali_tr_t100, observed).
narrative_ontology:measurement(sali_tr_t200, salic_prohibition__immutable_mandate_reading, theater_ratio, 200, 0.31).
narrative_ontology:measurement_basis(sali_tr_t200, observed).
narrative_ontology:measurement(sali_tr_t300, salic_prohibition__immutable_mandate_reading, theater_ratio, 300, 0.36).
narrative_ontology:measurement_basis(sali_tr_t300, observed).
narrative_ontology:measurement(sali_tr_t400, salic_prohibition__immutable_mandate_reading, theater_ratio, 400, 0.4).
narrative_ontology:measurement_basis(sali_tr_t400, observed).
narrative_ontology:measurement(sali_tr_t500, salic_prohibition__immutable_mandate_reading, theater_ratio, 500, 0.42).
narrative_ontology:measurement_basis(sali_tr_t500, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement_basis(sali_be_t100, observed).
narrative_ontology:measurement(sali_be_t200, salic_prohibition__immutable_mandate_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement_basis(sali_be_t200, observed).
narrative_ontology:measurement(sali_be_t300, salic_prohibition__immutable_mandate_reading, base_extractiveness, 300, 0.63).
narrative_ontology:measurement_basis(sali_be_t300, observed).
narrative_ontology:measurement(sali_be_t400, salic_prohibition__immutable_mandate_reading, base_extractiveness, 400, 0.66).
narrative_ontology:measurement_basis(sali_be_t400, observed).
narrative_ontology:measurement(sali_be_t500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 500, 0.68).
narrative_ontology:measurement_basis(sali_be_t500, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement_basis(sali_su_t100, observed).
narrative_ontology:measurement(sali_su_t200, salic_prohibition__immutable_mandate_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement_basis(sali_su_t200, observed).
narrative_ontology:measurement(sali_su_t300, salic_prohibition__immutable_mandate_reading, suppression_requirement, 300, 0.75).
narrative_ontology:measurement_basis(sali_su_t300, observed).
narrative_ontology:measurement(sali_su_t400, salic_prohibition__immutable_mandate_reading, suppression_requirement, 400, 0.78).
narrative_ontology:measurement_basis(sali_su_t400, observed).
narrative_ontology:measurement(sali_su_t500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 500, 0.79).
narrative_ontology:measurement_basis(sali_su_t500, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.1).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the salic_prohibition kernel. immutable_mandate_reading (this file) claims the exclusion is irrevocable natural/divine law binding even on sovereigns; sovereign_override_reading claims it is ordinary positive law a sovereign can revoke by legislative act; cognatic_reversion_reading claims it was never properly binding outside Frankish territory, so cognatic succession is the true default elsewhere. Each carries its own epsilon and its own stakeholder set per the ε-invariance principle; they are linked via network edges rather than merged into a single observer-relative constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
