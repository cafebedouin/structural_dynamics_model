% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Reading of Article 51 Self-Defense
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   Since 2001, several militarily capable states have advanced readings of
 *   Article 51 that extend self-defense to preemptive or preventive strikes
 *   against non-state actors and 'emerging' threats, with the necessity
 *   determination made unilaterally by the acting state rather than certified
 *   by the Security Council or any external body. The doctrine is presented
 *   as a coordination solution to a genuine gap in the classical framework
 *   (fast-moving non-state threats), but its self-judged necessity standard,
 *   combined with structural asymmetry between capable and target states,
 *   produces a persistent extraction pattern: capable states gain expanded
 *   discretionary authority and defense-sector benefit, while target-region
 *   populations and the multilateral system absorb the costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.71).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.68).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Reading of Article 51 Self-Defense").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '87e0b6c5-4d7a-4f1c-b525-1e2627018c5a').
narrative_ontology:cs_kernel_codification('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', fixed_text).
narrative_ontology:cs_authority_grounding('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', distributed).
narrative_ontology:cs_reading_relation('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', foundational, necessity_is_self_judged_by_acting_state).
narrative_ontology:cs_axiom_status(necessity_is_self_judged_by_acting_state, holdable).
narrative_ontology:cs_axiom_grounding('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', necessity_is_self_judged_by_acting_state, conventional).
narrative_ontology:cs_axiom('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', foundational, emerging_threats_qualify_without_imminent_armed_attack).
narrative_ontology:cs_axiom_status(emerging_threats_qualify_without_imminent_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', emerging_threats_qualify_without_imminent_armed_attack, instrumental).
narrative_ontology:cs_reference_frame('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', un_charter_collective_security_primacy).
narrative_ontology:cs_drift_state('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', post_9_11_practice_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87e0b6c5-4d7a-4f1c-b525-1e2627018c5a', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, domestic_defense_sectors).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, executive_war_powers_offices).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_civilian_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, weaker_states_facing_reciprocal_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes 'necessity' and 'imminence' as self-determined thresholds to justify strikes against non-state actors or emerging threats before an armed attack occurs. Controls the intelligence assessments that establish the factual predicate, faces no binding external adjudication before acting, and can act first and defend the legal characterization after the fact through diplomatic and legal channels it also dominates.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Benefits from sustained procurement, doctrine development, and operational tempo justified by an expansive preventive posture. Has no exposure to the legal or civilian costs of the doctrine's application and every incentive to see the necessity threshold interpreted permissively.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, domestic_defense_sectors, beneficiary,
    organized, biographical, arbitrage, national).

% Gains discretionary authority to authorize force without prior legislative or multilateral sign-off by characterizing action as anticipatory self-defense. The looser the necessity standard, the wider this office's unilateral decision space; it is both the interpreter and the primary beneficiary of the interpretation.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, executive_war_powers_offices, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, executive_war_powers_offices, beneficiary).

% Bears the direct costs of strikes justified under a self-judged necessity standard: casualties, displacement, and destruction of infrastructure in territories that are not themselves party to the necessity determination. Has no forum in which to contest the factual or legal basis of the action before it occurs and limited recourse afterward.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_civilian_populations, payer,
    powerless, immediate, trapped, regional).

% Is structurally bypassed each time a state acts preventively and self-certifies necessity rather than seeking Council authorization. The more this reading is normalized by state practice, the weaker the Council's collective-security monopoly becomes as a matter of customary practice, even though its formal Charter authority is untouched.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_authority, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(article_51_self_defense__expansive_preventive_reading, un_security_council_authority).

% Faces the precedent that any state can claim preventive self-defense against threats it perceives as emerging on or near their territory. Cannot symmetrically project force to invoke the same doctrine against militarily capable states, so the norm's permissiveness runs one direction in practice even though it is framed as generally applicable.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, weaker_states_facing_reciprocal_claims, payer,
    moderate, generational, constrained, regional).

% Documents state practice and opinio juris to assess whether this reading is crystallizing into customary international law or remains a contested minority position. Testifies in fora and publishes analysis that neither side of the practice can fully control.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides militarily capable states a legal vocabulary for acting against genuinely fast-moving, diffuse threats (terrorist networks, proliferation risks) that the classical armed-attack-response model was not designed to address, where waiting for an attack to occur or become imminent in the narrow sense could mean absorbing a catastrophic first blow.
% TRANSFER_FUNCTION: Moves the practical burden of proof and the costs of miscalculation from the acting state (which self-certifies necessity) to the target-region population (which absorbs the consequences) and to the multilateral system (which loses adjudicative primacy over the use of force).
% ABSENT_VOICES: Target-region populations and the states in which preventive strikes occur are not parties to the necessity determination and have no seat in the doctrinal debate conducted primarily among capable states' foreign ministries, defense establishments, and allied legal academies.
% DISAPPEARANCE_RATIONALE: Capable states and their defense establishments would say the world becomes more dangerous overnight, since they would lose a claimed tool against fast-emerging non-state threats. Target populations, weaker states, and multilateralists would say the world becomes safer and more rule-governed, since the Security Council's gatekeeping role over the use of force would be restored to its Charter-intended primacy. There is no neutral answer to which counterfactual is correct.
% FOUNDING_PROBLEM: The classical Article 51 framework, built around interstate armed attack, appeared inadequate to septmber 11-era threats: non-state actors capable of catastrophic harm, operating from territories outside any single state's effective control, where waiting for an 'imminent' attack in the traditional sense could mean waiting until mass casualties had already occurred.
% FOUNDING_PROBLEM_CORROBORATION: Capable states' own defense and legal establishments attest the problem remains live and justifies the expansive reading. Independent international law scholars, UN Special Rapporteurs on extrajudicial killing, and target-state governments attest from outside the beneficiary set that the doctrine has been invoked well beyond genuine imminent-threat scenarios, functioning as a general license for preventive force rather than a narrow emergency exception.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, contested).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as substantial and rising (0.48 to 0.71 over the interval) because each invocation without external adjudication further normalizes the self-judged necessity standard as customary practice, compounding the doctrine's discretionary reach. Suppression is high and rising because sustaining the doctrine as legally defensible requires active diplomatic, legal, and institutional work to forestall Security Council censure and to insulate the practice from binding review — this is a raw structural property, not scaled by scope. Theater ratio is moderate and rising (0.25 to 0.42): a genuine coordination problem exists (fast-emerging non-state threats), but a growing share of the doctrinal apparatus functions to legitimate strikes decided on other grounds, using necessity language as post-hoc cover.
 *
 * PERSPECTIVAL GAP:
 *   From the acting state's seat, the doctrine reads as necessary adaptation of self-defense law to real threats it must respond to under time pressure. From the target-region population's seat, the same structure reads as unreviewable license for foreign force on their territory. From the Security Council's seat (via institutional observers and scholars), the pattern reads as gradual erosion of the Charter's use-of-force gatekeeping regime through accumulating unilateral practice. The engine computes these as distinct per-seat classifications from the same structural data; the divergence is not a contradiction to resolve but the measurement itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states and their defense sectors sit near the beneficiary end: they gain the discretionary capacity to act, and the mechanism that grants this capacity (self-certified necessity) is also the mechanism that shields them from binding review. Target-region populations sit at the full-target end: trapped exit, immediate time horizon, no role in the necessity determination that licenses force against them. The UN Security Council is a structural payer despite being a non-agent institutional entity — its collective-security monopoly erodes with each unreviewed invocation, which is why it is listed as a victim rather than omitted; the erosion is a cost even though the Council collects nothing and cannot itself be extracted from in the ordinary sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophic non-state threats outpacing the classical armed-attack-response framework) was genuinely live in 2001 and remains partially live today, which is why founding_problem_status is authored as contested rather than dead. But the doctrine's persistence and expansion beyond narrowly imminent scenarios — into strikes against merely 'emerging' threats — suggests the mandate has outrun the founding problem's actual scope. Classifying this as tangled_rope rather than snare preserves the genuine coordination kernel (a real gap existed) while registering the asymmetric extraction that has grown around it; classifying it as mountain or rope would erase the victim structure entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_certification_legitimacy,
    'Can a necessity and imminence standard be legally meaningful when the same party that would use force is also the sole judge of whether the standard is met, with no binding external review prior to action?',
    'State practice and opinio juris review by the International Court of Justice or a comparable body assessing whether self-certified necessity claims are functionally distinguishable from a general license, and whether any invocation has ever been retroactively found unlawful with consequence.',
    'If self-certification is found functionally unreviewable in practice, the doctrine''s coordination claim weakens substantially and the extraction/suppression profile should be read as dominant; if meaningful ex post accountability exists, the coordination function is more genuine than the metrics here assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_certification_legitimacy, conceptual, 'Whether self-judged necessity can function as a legal constraint at all.').

omega_variable(
    reading_selection_under_determination,
    'Is the choice of the expansive_preventive_reading over the narrow_armed_attack_reading or the unable_unwilling_doctrine_reading determined by legal materials, or by the interpreting state''s relative military capability?',
    'Comparative analysis of which states/blocs invoke which reading, cross-referenced against relative military capability and historical practice; convergence of reading choice with capability would support a power-driven rather than legally-driven account of doctrinal selection.',
    'If reading choice tracks capability rather than legal reasoning, this reading''s claimed doctrinal legitimacy is substantially undermined and the tangled_rope classification''s coordination component should be weighted lower relative to the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the kernel''s readings are chosen on legal or power-political grounds.').

omega_variable(
    customary_law_crystallization,
    'Has sufficiently widespread and consistent state practice accumulated to crystallize this reading into binding customary international law, or does it remain a persistent-objector-contested minority position?',
    'Systematic survey of state practice and opinio juris across the UN General Assembly, ICJ pleadings, and state legal justifications issued contemporaneously with strikes, tracked over the interval.',
    'Crystallization into custom would mean the doctrine''s suppression of the Security Council''s role is durable and structural rather than contested and reversible; failure to crystallize would support continued treatment as an unsettled, actively resisted claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_crystallization, empirical, 'Whether the doctrine has hardened into binding custom or remains genuinely contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(arti_tr_t2005, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(arti_tr_t2009, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2009, 0.34).
narrative_ontology:measurement(arti_tr_t2013, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2013, 0.37).
narrative_ontology:measurement(arti_tr_t2018, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.48).
narrative_ontology:measurement(arti_be_t2005, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(arti_be_t2009, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2009, 0.6).
narrative_ontology:measurement(arti_be_t2013, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2013, 0.64).
narrative_ontology:measurement(arti_be_t2018, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(arti_su_t2005, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(arti_su_t2009, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2009, 0.56).
narrative_ontology:measurement(arti_su_t2013, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(arti_su_t2018, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2018, 0.64).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the article_51_self_defense kernel. narrow_armed_attack_reading holds self-defense to responses against actual or imminent armed attack attributable to a state — the classical, most restrictive reading. unable_unwilling_doctrine_reading occupies a hybrid position, triggering self-defense against non-state actors only where the host state is shown unwilling or unable to suppress the threat. This story (expansive_preventive_reading) is the most permissive of the three, extending self-defense to preventive action against emerging threats on a self-judged necessity standard. The three stories share no single ε — each has a distinct beneficiary/victim structure and distinct extraction profile, and are linked here rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
