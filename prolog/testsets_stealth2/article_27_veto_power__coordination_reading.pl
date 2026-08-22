% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: P5 Veto as Great-Power Coordination Gate (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   Article 27(3) of the UN Charter gives each of the five permanent members
 *   of the Security Council an unconditional negative vote on all
 *   non-procedural resolutions. This story instantiates the
 *   coordination_reading of that arrangement: the unanimity gate as the
 *   mechanism that keeps the great powers inside collective security by
 *   guaranteeing that no Council resolution can compel a nuclear-armed state
 *   into a military confrontation it rejects. The reading's genealogy is the
 *   League of Nations, whose members defied or abandoned the institution when
 *   its rules bound them against their core interests; the 1945 drafters
 *   built the gate so that membership could never obligate a great power to a
 *   war. The epsilon referent is the standing veto arrangement as assessed by
 *   this reading's own lights: extraction prices near coordination cost, the
 *   guarantee of non-compulsion runs to the whole membership, and no victim
 *   class is declared. The costs this reading acknowledges, blocked action in
 *   mass-atrocity crises and a Council that cannot act against any of five
 *   states regardless of conduct, are carried as the measured price of the
 *   gate; the reading's principal blind spot is carried as an excluded
 *   stakeholder seat and an omega variable, not as an authored fact.
 *
 * KEY AGENTS:
 *   - p5_permanent_members: Holders of the concentrated blocking right (institutional/arbitrage) — receive the non-compulsion guarantee, anchor the amendment rules, and accrue the arrangement's concentrated operative gain
 *   - non_p5_un_member_states: Universal guarantee recipients (organized/constrained) — hold no blocking right and no exit from the security architecture
 *   - elected_security_council_members: Conditional participants (moderate/constrained) — negotiate and draft but are overridable by one negative vote
 *   - g4_aspirant_powers: Structurally excluded from the design decision (powerful/constrained) — campaign for permanent seats they cannot ratify their way into
 *   - civilian_populations_in_vetoed_crises: Absent seat (powerless/trapped) — bear the deadweight of blocked action with no channel into the decision
 *   - institutional_design_scholars: Analytical observers (analytical/analytical) — attest the founding problem from the League-era record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.22).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.15).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "P5 Veto as Great-Power Coordination Gate (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'b7fde785-debd-4004-8185-1bb60201b539').
narrative_ontology:cs_kernel_codification('b7fde785-debd-4004-8185-1bb60201b539', fixed_text).
narrative_ontology:cs_authority_grounding('b7fde785-debd-4004-8185-1bb60201b539', lineage).
narrative_ontology:cs_interpretation_layer_present('b7fde785-debd-4004-8185-1bb60201b539').
narrative_ontology:cs_reading_relation('b7fde785-debd-4004-8185-1bb60201b539', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7fde785-debd-4004-8185-1bb60201b539', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('b7fde785-debd-4004-8185-1bb60201b539', foundational, great_power_war_prevention_outweighs_council_paralysis).
narrative_ontology:cs_axiom_status(great_power_war_prevention_outweighs_council_paralysis, holdable).
narrative_ontology:cs_axiom_grounding('b7fde785-debd-4004-8185-1bb60201b539', great_power_war_prevention_outweighs_council_paralysis, instrumental).
narrative_ontology:cs_axiom('b7fde785-debd-4004-8185-1bb60201b539', secondary, ratified_1945_settlement_allocates_blocking_rights).
narrative_ontology:cs_axiom_status(ratified_1945_settlement_allocates_blocking_rights, holdable).
narrative_ontology:cs_axiom_grounding('b7fde785-debd-4004-8185-1bb60201b539', ratified_1945_settlement_allocates_blocking_rights, conventional).
narrative_ontology:cs_reference_frame('b7fde785-debd-4004-8185-1bb60201b539', great_power_inclusion_bargain_1945).
narrative_ontology:cs_drift_state('b7fde785-debd-4004-8185-1bb60201b539', contemporary_multipolar_strain, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b7fde785-debd-4004-8185-1bb60201b539', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_un_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, elected_security_council_members).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, collective_security_requires_great_power_participation).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, league_unanimity_failure_lesson).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the unconditional blocking right over Security Council resolutions under Article 27(3): each can unilaterally prevent any binding resolution it rejects, which guarantees that none of them can be compelled by the Council into a military confrontation against its will. Their ratification is also required to amend the Charter, so the arrangement persists by their consent. When the Council blocks them they act outside it through national forces or ad hoc coalitions, which is precisely the outside option that makes continued participation worth the bargain; the concentrated operative gain of the arrangement, the blocking right itself, accrues entirely to these five seats.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_permanent_members, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_permanent_members, agenda_setter).

% The remaining member states. They receive the same guarantee that no Council resolution can drag a nuclear great power into a war it rejects, and they retain a universal forum where their security concerns are heard, but they hold no blocking right and cannot opt out of the security architecture. When the Council deadlocks their only channels are General Assembly recommendations, regional organizations, and treaty bodies outside the Council. They organize as blocs, including the ACT group and the Code of Conduct signatories, to press for voluntary restraint in mass-atrocity situations, which is the main lever available short of Charter amendment.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_un_member_states, beneficiary,
    organized, generational, constrained, global).

% The ten rotating members serving two-year terms. They sit at the table, draft and negotiate resolutions, and share in the Council's information flows and legitimacy, but a single permanent member's negative vote can erase their work. Their influence is real but conditional on the permanent members' tolerance, and their seat expires by design.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, elected_security_council_members, beneficiary,
    moderate, biographical, constrained, global).

% India, Brazil, Germany, and Japan: major powers whose economic and military weight has grown far past their 1945 standing. They campaign openly for permanent seats, but Charter amendment requires ratification by the existing five, so the decision about who holds blocking rights is structurally closed to them; their voice on the arrangement's design is heard in debate but carries no weight in the ratification calculus.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, g4_aspirant_powers, excluded,
    powerful, generational, constrained, global).

% People in the theaters of crises where Council action has been blocked, such as Rwanda in 1994, Srebrenica in 1995, and Syria from 2011 onward. They have no seat and no state representing their interest when a permanent member casts a negative vote on intervention, referral, or arms measures. They bear the consequences of the deadlock directly and immediately, and nothing in the arrangement gives them a channel into the decision.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, civilian_populations_in_vetoed_crises, excluded,
    powerless, immediate, trapped, regional).

% Historians of the San Francisco conference and institutional-design researchers who study why the League of Nations failed and whether the 1945 bargain contributes to the absence of direct great-power war since. They compare the unanimity gate against counterfactuals such as majority rule and great-power withdrawal, and their findings feed reform debates without holding any decision rights.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, institutional_design_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the great-power participation problem in collective security: without a unanimity gate, the Council could adopt binding resolutions against nuclear-armed states, and those states would either defy the Council, destroying its authority as the League's fate showed, or leave it as Japan, Italy, and Germany left the League. The gate keeps the great powers inside a shared security institution by guaranteeing that membership can never obligate them to a war they reject.
% TRANSFER_FUNCTION: Allocates an unconditional blocking right over binding Council action to five named states, and in exchange distributes to the whole membership the guarantee that the Council will never authorize war against a nuclear power that objects. What moves is decision rights, concentrated in five capitals, and war-risk exposure, diffused across all states: the institution never has to attempt enforcement against a great power because it never adopts an authorization one would block.
% ABSENT_VOICES: Populations in vetoed crises have no seat and no representation of their interest in the blocking decision. The G4 aspirant powers are present in debate but structurally absent from the ratification calculus that decides who holds blocking rights. General Assembly majorities can be set aside by a single negative vote and can respond only through non-binding recommendations or bodies outside the Council.
% DISAPPEARANCE_RATIONALE: If the unanimity gate lapsed overnight, the Council could adopt binding resolutions over the objection of nuclear-armed states. Those states would face a choice between compliance against their core security interests, open defiance that would break the Council's authority as definitively as the League's failures broke it, or withdrawal. Security coordination among the great powers would reorganize around raw power through bilateral deterrence, ad hoc coalitions, or a revived concert outside the UN, and the universal forum's security role would collapse to debate. No seat's position survives the arrangement's disappearance unchanged.
% FOUNDING_PROBLEM: The League of Nations demonstrated that a collective security institution cannot survive on great-power sufferance: it could not bind Japan, Italy, or Germany, and those states defied it and walked out, after which the League was irrelevant to the wars that followed. The UN's founders designed the veto to solve great-power defection: build the institution so that the states capable of fighting a world war are never put in the position of being bound against their will.
% FOUNDING_PROBLEM_CORROBORATION: The League-era diplomatic record, including Japan's withdrawal after the Manchurian report, Italy's after the Ethiopia sanctions, and Germany's exit, corroborates that the founding problem was great-power defection, and it is attested by diplomatic historians of the period rather than by any current beneficiary. The recurring modern pattern of great powers acting outside the Council when authorization fails, as with Kosovo in 1999 and Iraq in 2003, is observed behavior independent of any permanent member's assertion. No living constituency is wholly outside the beneficiary set under this reading, since the guarantee runs to all member states; the corroboration available is historical and behavioral rather than contemporaneous and disinterested.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.22, above but near the enforcement_mechanism coordination floor of 0.10, because even on this reading the gate imposes real deadweight: crises where action is blocked, and a Council that cannot act against any of five states regardless of conduct. Suppression is low (0.15) because the veto coerces no one; it withholds authorization while leaving every other channel, General Assembly recommendations, regional organizations, ad hoc coalitions, open. Theater_ratio at interval end is 0.28, moderate-low but rising: the blocking right is mechanically functional, yet a growing share of Council activity is positioning for audiences when vetoes are pre-announced, a pattern that dipped after the Cold War opened space for genuine bargaining and resumed as permanent-member alignment deteriorated. Accessibility_collapse (0.55) reflects that the principal alternatives, unanimity rules and majority rule, were tried or priced and failed against the League record, while a great-power concert outside the UN remains conceivable. Resistance (0.60) is real and organized: the ACT group's Code of Conduct, the France-Mexico restraint initiative, and the Liechtenstein veto-debate initiative. The veto is self-executing, a negative vote blocks with no enforcement machinery, so the enforcement picture is static and is carried by base_properties.suppression; no suppression_requirement series is authored per the static-enforcement rule. Both temporal series run on one shared six-point grid, with t measured in years since 1945, so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same arrangement. From the permanent members' seat the gate is the guarantee that makes participation rational: no resolution can ever bind them to a war, and the arrangement is the institution they would insist on before joining any. From the non-permanent beneficiary seats the same structure is a guarantee they receive without controlling: they benefit from avoided great-power war but hold no blocking right and cannot exit the architecture. The excluded seats are where the reading's blind spot is visible without being authored as fact: populations in vetoed crises bear the gate's deadweight directly, and the G4 are shut out of the design decision entirely; the engine computes their exposure from the structural data while the no-victim-class claim remains this reading's authored position.
 *
 * DIRECTIONALITY LOGIC:
 *   The permanent members are declared beneficiaries and hold arbitrage-grade exit: they can act outside the Council when blocked, which is exactly why the guarantee is what keeps them inside, so their derived directionality sits nearest the beneficiary end and effective extraction is damped toward subsidy, the arrangement pays them. Non-permanent member states are beneficiaries with constrained exit: they receive the guarantee but cannot leave the security system, so their d sits low but above the permanent members'. The elected members are conditional beneficiaries with biographical horizons and constrained exit. The excluded seat, civilian populations in vetoed crises, carries no beneficiary or victim declaration under this reading, so its directionality falls back to the power-atom default for a powerless, trapped seat, which prices its exposure honestly even though the reading declines to call it extraction; that gap between the computed seat and the reading's claim is exactly what the blocked_action_cost_incidence omega tracks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, great-power defection from collective security, is still live: nuclear-armed states still refuse to be bound into war, and the League's record still stands as the natural experiment. Because founding_problem_status is live and disappearance_verdict is world_rearranges, the mismatch consumer finds no zombie flag; the arrangement is not persisting past its function. The mandatrophy risk on this kernel runs in the other direction: the oligopoly reading would date the founding problem's death to the Cold War's end and read the veto as a mandate outlived, whereas this reading keeps the genealogy question open by pointing at the defection pattern, Kosovo and Iraq 2003, as evidence the problem never closed. The classification discipline cuts both ways: authoring the reading honestly, low epsilon and no victims, without tuning it to a predicted engine output leaves the per-seat computations free to diverge, and a computed divergence toward a hybrid would be the corpus's measurement, not an authoring error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the coordination_reading of the article_27_veto_power kernel, and the sibling readings, oligopoly_reading and sovereignty_reading, instantiate different constraints over the same standing arrangement: do the beneficiary set, the no-victim-class claim, and epsilon survive translation into those readings, or does each sibling produce a different beneficiary/victim structure and a materially different epsilon?',
    'Author the sibling stories over the same referent and compare per-seat classifications, epsilon, and beneficiary/victim declarations across the family; the disagreement is located in the beneficiary/victim structure and in whether the unanimity gate prices as coordination cost or as extraction.',
    'If the oligopoly reading''s structure dominates the evidence, this reading''s no-victim-class claim is revealed as a seat-relative blindness rather than a structural fact, and the family classification moves toward a hybrid with concentrated gains in the P5 seat and diffuse costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of the Article 27 kernel; sibling readings restructure beneficiaries and victims over the same arrangement.').

omega_variable(
    counterfactual_war_prevention_attribution,
    'How much of the absence of direct great-power war since 1945 is attributable to the unanimity gate itself, rather than to nuclear deterrence, bipolarity, or the other candidate causes of the long peace?',
    'Comparative institutional analysis: great-power war frequency under the League''s unanimity-and-absence regime, near-miss case studies such as Cuba in 1962 and Able Archer in 1983 where gate and deterrence interacted, and great-power behavior when the Council is blocked, whether escalation or routing around.',
    'If nuclear deterrence does the causal work, the gate is a redundant rider on deterrence, its coordination justification weakens, and the residual function of entrenching blocking rights carries the extraction weight; if the gate genuinely gates confrontation, the coordination claim holds and epsilon prices near coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_war_prevention_attribution, empirical, 'Whether the veto or nuclear deterrence explains the long peace, the causal load the coordination claim rests on.').

omega_variable(
    blocked_action_cost_incidence,
    'Are the costs borne by populations in crises where action is vetoed, such as Rwanda, Srebrenica, and Syria, coordination costs of the unanimity gate borne diffusely as the price of the system, or extraction borne by an identifiable victim class this reading does not count?',
    'Cost-incidence analysis across vetoed crises: if the same populations systematically bear the costs while the blocking right and its guarantee accrue to five seats, the structure is asymmetric; if costs are episodic and diffuse across all states, they price as coordination.',
    'Asymmetric incidence reclassifies the family toward a hybrid with civilian populations in vetoed crises as victims, and this reading''s epsilon understates the arrangement''s extraction; diffuse incidence confirms the rope structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocked_action_cost_incidence, empirical, 'Whether vetoed-crisis costs are coordination price or an uncounted victim class, this reading''s principal blind spot.').

omega_variable(
    voluntary_restraint_substitutability,
    'Would voluntary veto-restraint regimes such as the ACT Code of Conduct and the France-Mexico initiative deliver the gate''s coordination benefit without the formal blocking right, or does the guarantee require the unconditional formal right?',
    'Observe whether restraint pledges hold under crisis conditions and compare great-power behavior in pledged versus unpledged scenarios; a pledge abandoned at the first hard case shows the formal right is load-bearing.',
    'If substitutable, the formal veto''s residual function is entrenchment and epsilon rises above coordination cost; if not substitutable, the formal gate is doing the work and the coordination reading is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_restraint_substitutability, empirical, 'Whether norms could substitute for the formal gate, testing the necessity half of the coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t16, article_27_veto_power__coordination_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement_basis(arti_tr_t16, observed).
narrative_ontology:measurement(arti_tr_t32, article_27_veto_power__coordination_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement_basis(arti_tr_t32, observed).
narrative_ontology:measurement(arti_tr_t48, article_27_veto_power__coordination_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement_basis(arti_tr_t48, observed).
narrative_ontology:measurement(arti_tr_t64, article_27_veto_power__coordination_reading, theater_ratio, 64, 0.22).
narrative_ontology:measurement_basis(arti_tr_t64, observed).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__coordination_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t16, article_27_veto_power__coordination_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement_basis(arti_be_t16, observed).
narrative_ontology:measurement(arti_be_t32, article_27_veto_power__coordination_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement_basis(arti_be_t32, observed).
narrative_ontology:measurement(arti_be_t48, article_27_veto_power__coordination_reading, base_extractiveness, 48, 0.18).
narrative_ontology:measurement_basis(arti_be_t48, observed).
narrative_ontology:measurement(arti_be_t64, article_27_veto_power__coordination_reading, base_extractiveness, 64, 0.2).
narrative_ontology:measurement_basis(arti_be_t64, observed).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__coordination_reading, base_extractiveness, 80, 0.22).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'P5 veto' covers at least three structurally distinct claims over one standing arrangement, Article 27(3) as ratified. This file is the coordination_reading: epsilon priced near coordination cost (0.22 at interval end), beneficiaries declared, no victim class. The sibling files instantiate the oligopoly_reading, the same arrangement priced as high-extraction entrenchment with states whose institutional evolution is blocked as the victim class, and the sovereignty_reading, the same arrangement priced as consent made institutional, with a different beneficiary structure. The referent is shared; the epsilon values are reading-indexed per the epsilon-invariance principle. Family members link to each other through their own network declarations; the coordination reading is upstream in official discourse, and its observable failures, blocked action in mass-atrocity crises, are the evidence base the oligopoly critique draws on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
