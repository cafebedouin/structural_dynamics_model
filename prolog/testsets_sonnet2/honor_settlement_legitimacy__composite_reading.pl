% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Duel Settlement as Legitimate Dispute Resolution (Composite/Overdetermined Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   In the century preceding dueling's effective disappearance from
 *   mainstream elite practice in several Western societies, the institution
 *   did not collapse from a single cause. Cultural frameworks shifted such
 *   that lethal ritual combat over insult became increasingly difficult to
 *   imagine as a rational response (the contraction mechanism);
 *   simultaneously, and largely independently, courts began prosecuting
 *   seconds as accessories to manslaughter, life-insurance underwriters began
 *   denying payouts on duel-related deaths, and the press reframed duels from
 *   honorable affairs into embarrassing scandals. A rising commercial middle
 *   class, never invested in the aristocratic status economy the code served,
 *   built alternative reputation and dispute-resolution institutions that
 *   made the code's settlement function progressively redundant regardless of
 *   what anyone believed about honor. This story treats the decline as
 *   jointly produced: the practice would very likely have died even had any
 *   single mechanism been absent, because the others were independently
 *   sufficient on a similar timescale.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.42).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.55).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Duel Settlement as Legitimate Dispute Resolution (Composite/Overdetermined Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae').
narrative_ontology:cs_kernel_codification('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', distributed).
narrative_ontology:cs_authority_grounding('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', practice).
narrative_ontology:cs_interpretation_layer_present('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae').
narrative_ontology:cs_reading_relation('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', foundational, decline_is_multiply_overdetermined).
narrative_ontology:cs_axiom_status(decline_is_multiply_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', decline_is_multiply_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', secondary, material_institutional_withdrawal_independently_sufficient).
narrative_ontology:cs_axiom_status(material_institutional_withdrawal_independently_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', material_institutional_withdrawal_independently_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', honor_code_as_functioning_status_adjudication_practice).
narrative_ontology:cs_drift_state('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', post_abolition_consolidation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('324f74ab-6a70-4edf-bcf5-c89bc3c2f1ae', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, declining_aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, professional_officer_corps_holdouts).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, duelists_and_their_dependents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, seconds_and_witnesses_exposed_to_prosecution).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, gentlemanly_equality_before_the_code).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, private_settlement_superior_to_state_adjudication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continues to invoke the code of honor as the legitimate mechanism for settling affronts among social equals, deriving residual status from being seen to observe it, even as the surrounding legal, insurance, and social-mobility structures that once made it functional have eroded. Increasingly performs adherence rather than relying on it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, declining_aristocratic_honor_class, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, declining_aristocratic_honor_class, agenda_setter).

% Military and quasi-military institutions retain informal tolerance for code-of-honor settlement among officers longer than civil society, using it to police intra-corps status disputes without airing them publicly. Benefits from the residual mechanism's continued (if attenuated) legitimacy inside the institution.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, professional_officer_corps_holdouts, beneficiary,
    organized, biographical, constrained, national).

% Individuals who accept or issue challenges bear mortal risk, legal exposure, and financial ruin (widowed families, orphaned children, forfeited property in some jurisdictions) for what is, by the period's own emerging standards, an increasingly optional and socially unnecessary performance. Their exit is nominally available (refuse the challenge) but constrained by the social cost of refusal within the still-operating status economy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, duelists_and_their_dependents, payer,
    moderate, biographical, trapped, local).

% Friends and associates drawn in as seconds face criminal liability as accessories under increasingly enforced statutes, absorbing legal risk generated by a practice whose social payoff accrues mainly to the principals and the honor class's continued self-image.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, seconds_and_witnesses_exposed_to_prosecution, payer,
    moderate, immediate, constrained, local).

% Grows in numbers and economic weight throughout the relevant period, structurally excluded from and increasingly indifferent to the honor code's status economy, developing alternative dispute-resolution and reputation mechanisms (courts, press, credit-reporting, professional associations) that make dueling's settlement function progressively redundant. Their preferences were never solicited by the code but their institutions are what displace it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, expanding_commercial_middle_class, excluded,
    organized, generational, mobile, national).

% Courts, prosecutors, life-insurance underwriters, and press institutions independently tighten around dueling across the period — criminalizing seconds, denying payouts to duel-related deaths, publicizing outcomes as scandal rather than honor. Each acts on its own institutional logic; none coordinates with the others, but together they remove the material scaffolding the practice depended on regardless of what happens to cultural belief.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_and_insurance_institutions, agenda_setter,
    institutional, civilizational, analytical, national).

% Assess the decline of dueling after the fact, weighing whether cultural unthinkability, material/institutional suppression, or their combination best accounts for the timing and geography of decline. This composite reading is the observer position that multiple independently sufficient mechanisms converged.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, declining_aristocratic_honor_class).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code, where it still operated, coordinated status disputes among nominal equals through a shared, mutually recognized ritual that avoided both private vendetta escalation and reliance on state courts many gentlemen regarded as beneath their dignity or captured by commercial interests.
% TRANSFER_FUNCTION: Moves physical risk and legal exposure from the honor class's status economy onto individual duelists, their dependents, and their seconds; moves reputational capital toward those who observe the code correctly regardless of outcome, and increasingly moves practical relevance away from the code entirely toward courts, insurers, and the press as those institutions harden.
% ABSENT_VOICES: The expanding commercial middle class, whose growing economic and institutional weight is a primary displacing force, is never a party to the code's internal legitimacy debates — it simply builds alternative institutions the honor class eventually cannot avoid using. Widows and dependents of duelists are similarly absent from any account of why the practice was worth continuing.
% DISAPPEARANCE_RATIONALE: If the honor-settlement mechanism's residual legitimacy had disappeared abruptly rather than eroding across decades, the officer corps and the declining aristocratic class would have lost their last socially recognized non-judicial status-adjudication ritual overnight, with disputes either going unresolved, escalating informally, or being forced immediately into courts that were, at the outset of the period, not yet fully prepared institutionally to absorb them — the gradual, overdetermined character of the actual decline is itself evidence that multiple independent props were load-bearing simultaneously.
% FOUNDING_PROBLEM: Provided a way for social equals to settle affronts to honor without either escalating into open feud or submitting to courts perceived as illegitimate arbiters of gentlemanly status, in a period before robust civil libel remedies, professional reputation systems, or reliable state monopolies on legitimate violence existed.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary court records, insurance-industry archives, and press coverage from outside the dueling class itself (prosecutors who pursued seconds, underwriters who denied claims, newspaper editors who reframed duels as scandal rather than honor) corroborate that the practical dispute-resolution function had become unnecessary well before its final legal suppression; the composite reading treats this external corroboration as evidence for convergent rather than singular causation, since courts, insurers, and press each independently attest the founding problem was already solved by their own institutions before formal abolition.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly (0.28 to 0.42) reflecting a practice whose costs to duelists and seconds become steadily less justified as its coordination function erodes — the risk borne stays roughly constant while the social payoff diminishes, which reads as increasing net extraction even without any actor increasing coercive pressure. Theater ratio climbs sharply (0.10 to 0.40) because a growing share of what remains of the code becomes performative maintenance of status among the shrinking honor class and officer-corps holdouts, rather than functional dispute resolution — this is the piton signature. Suppression is authored moderately and rising (0.30 to 0.55) reflecting the institutional side of the composite story: courts and insurers actively withdraw and criminalize rather than merely observers watching belief change, so some genuine suppressive pressure is present alongside contraction, distinguishing this reading's profile from a pure-contraction account where suppression would plausibly fall as norms simply update.
 *
 * DIRECTIONALITY LOGIC:
 *   The declining aristocratic honor class and officer-corps holdouts are coded as beneficiaries because they extract residual status value from continued observance even as the practice's material justification dissolves — their exit is constrained rather than trapped because they retain other status resources. Duelists and their dependents and seconds are coded as payers/targets: they bear escalating relative cost (same mortal and legal risk, declining social payoff) with limited real exit given the still-operative status economy at the story's midpoint. The expanding commercial middle class is excluded rather than a payer or beneficiary in the code's own terms — it is outside the honor economy altogether, which is precisely why its parallel institution-building operates as an independent causal channel rather than a contest within the honor code's own logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling affronts among status equals without escalation or reliance on possibly-illegitimate courts) is corroborated as dead well before formal legal abolition, by parties outside the honor class itself — prosecutors, underwriters, editors. Classifying this as piton rather than snare or mountain matters for mandatrophy: no concentrated beneficiary captures rents from continued observance (ruling out snare), and the practice is manifestly not a feature of natural law (ruling out mountain) — what persists after the founding problem dies is inertial performance by a shrinking class defending status through ritual, exactly the piton signature, corroborated externally rather than self-declared by the honor class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    composite_vs_contraction_weighting,
    'Does the composite reading correctly treat material/institutional mechanisms as independently sufficient, or are they themselves downstream effects of the same cultural contraction that the contraction_reading identifies as the sole driver — i.e., did courts and insurers act because elite belief had already shifted, making the institutional channel epiphenomenal rather than independent?',
    'Comparative timing analysis across jurisdictions: if institutional suppression (prosecution rates, insurance denials) preceded measurable shifts in elite cultural attitudes in some regions but followed them in others, that would support genuine multi-causal independence; uniform sequencing (institutions always following belief shift) would collapse the composite account back into the contraction_reading.',
    'If institutional mechanisms are epiphenomenal, this composite_reading constraint and the contraction_reading sibling describe the same underlying causal structure at different levels of description rather than genuinely distinct mechanisms, weakening the case for authoring them as separate constraints with separate epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_vs_contraction_weighting, empirical, 'Whether institutional suppression is causally independent of or downstream from cultural contraction.').

omega_variable(
    fringe_persistence_boundary_case,
    'At what point does the drop_reading''s fringe-persistence account become the more accurate description for specific subpopulations (e.g., military officer corps, certain regional aristocracies) even while the composite account holds for the society-wide trend?',
    'Disaggregated survival analysis by subpopulation and region rather than a single national or civilizational trend line.',
    'If certain bounded populations (the professional_officer_corps_holdouts stakeholder group) show a genuinely different survival curve, the composite_reading''s society-wide overdetermination account and the drop_reading''s residual-practice account may both be correct simultaneously for different strata, rather than being competing accounts of the same population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_persistence_boundary_case, conceptual, 'Whether composite and drop readings partition by subpopulation rather than genuinely competing.').

omega_variable(
    honor_class_natural_law_framing_ambiguity,
    'Did the declining honor class experience the code''s obsolescence as a constructed social fact they could see eroding, or did they experience ''honor requires this response'' as itself a near-natural-law-like compulsion — i.e., is there a false-summit-adjacent dynamic where the code''s naturalness claim persisted psychologically even as its material supports visibly collapsed?',
    'First-person accounts (diaries, correspondence, honor-code literature) from duelists and their seconds analyzed for language of compulsion versus language of choice or performance across the interval.',
    'If compulsion language persists even as material scaffolding visibly erodes, that supports treating the honor class''s own self-understanding as internally naturalized (mountain-like from their own seat) even though the composite reading classifies the constraint as piton from the analytical seat — a genuine seat divergence rather than a simple error.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_class_natural_law_framing_ambiguity, conceptual, 'Whether the honor class''s own experience of the code''s compulsion diverges structurally from the analytical piton classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__composite_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__composite_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__composite_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(hono_tr_t80, honor_settlement_legitimacy__composite_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__composite_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__composite_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__composite_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__composite_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(hono_be_t80, honor_settlement_legitimacy__composite_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__composite_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__composite_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__composite_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__composite_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(hono_su_t80, honor_settlement_legitimacy__composite_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__composite_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, drop_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the honor_settlement_legitimacy kernel. contraction_reading asserts cultural unthinkability as the sole/dominant sufficient cause (a near-mountain framing of the shift in what counts as thinkable). drop_reading asserts the code persisted as a genuine, non-negligible fringe practice rather than disappearing. This composite_reading treats decline as jointly produced by contraction plus independently sufficient material/institutional withdrawal (courts, insurers, press) plus demographic displacement (rising commercial class), while acknowledging contraction as the dominant edge. All three share the same underlying kernel — the standing of honor-based dueling settlement as a legitimate dispute-resolution mechanism — but author different epsilon, different beneficiary/victim structures, and different classifications because they identify different dominant causal mechanisms and therefore different residual persistence profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
