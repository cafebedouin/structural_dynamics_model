% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousios Compromise Formula (Semi-Arian Reading of the Substance Controversy)
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   Between the Dedication Council at Antioch (341) and the Council of
 *   Constantinople (381), the imperial church was governed by a succession of
 *   mediating formulas of which homoiousios ('of similar substance') is the
 *   representative case: a deliberately imprecise settlement that anti-Arian
 *   and anti-Nicene bishops could both sign, enforced by the court through
 *   deposition and exile. This story instantiates ONE reading of the
 *   homoousios_christology kernel — the semi_arian_reading — and authors
 *   epsilon for THAT standing arrangement by its own lights: the compromise
 *   regime as it operated, not the pro-Nicene settlement that replaced it and
 *   not the Arian position it excluded. The claim/metric gap is deliberate:
 *   the arrangement is CLAIMED as tangled_rope (genuine coordination function
 *   plus asymmetric extraction) while the metrics are authored from its
 *   documented operation, including its terminal dissolution. CONSTRAINT
 *   FAMILY NOTE: per the epsilon-invariance principle, 'the substance
 *   controversy' decomposes into three stories — arian_reading (authors
 *   epsilon for the created-and-subordinate arrangement), this story (epsilon
 *   for the mediating-compromise arrangement), and pro_nicene_reading
 *   (epsilon for the consubstantial settlement). Their epsilon values differ
 *   because they are different constraints, not one constraint viewed from
 *   angles. KEY AGENTS (by structural relationship): - imperial_authority:
 *   Agenda-setter and principal collector (institutional/arbitrage) —
 *   convenes, promulgates, enforces, and revises the settlement -
 *   homoiousian_bishops: Middle-party beneficiaries with drafting power
 *   (organized/constrained) — keep their sees by supplying the formula -
 *   nicene_confessors: Primary target (moderate/identity_locked) — bear exile
 *   for refusing the minimum - anomoean_strict_arians: Secondary target
 *   (moderate/constrained) — condemned as the extremity the middle defines
 *   itself against - ordinary_clergy_and_laity: Diffuse payers with a
 *   coordination stake (powerless/trapped) - ecclesiastical_historians:
 *   Analytical observer — attests the structure from outside every party
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.25).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousios Compromise Formula (Semi-Arian Reading of the Substance Controversy)").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '9d6869da-8437-4ed5-952e-6fd9fc701519').
narrative_ontology:cs_kernel_codification('9d6869da-8437-4ed5-952e-6fd9fc701519', fixed_text).
narrative_ontology:cs_authority_grounding('9d6869da-8437-4ed5-952e-6fd9fc701519', lineage).
narrative_ontology:cs_interpretation_layer_present('9d6869da-8437-4ed5-952e-6fd9fc701519').
narrative_ontology:cs_reading_relation('9d6869da-8437-4ed5-952e-6fd9fc701519', homoousios_christology__pro_nicene_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d6869da-8437-4ed5-952e-6fd9fc701519', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_axiom('9d6869da-8437-4ed5-952e-6fd9fc701519', foundational, son_like_in_substance_not_identical).
narrative_ontology:cs_axiom_status(son_like_in_substance_not_identical, overridden).
narrative_ontology:cs_axiom_grounding('9d6869da-8437-4ed5-952e-6fd9fc701519', son_like_in_substance_not_identical, theological).
narrative_ontology:cs_axiom('9d6869da-8437-4ed5-952e-6fd9fc701519', foundational, scriptural_minimum_suffices_for_communion).
narrative_ontology:cs_axiom_status(scriptural_minimum_suffices_for_communion, holdable).
narrative_ontology:cs_axiom_grounding('9d6869da-8437-4ed5-952e-6fd9fc701519', scriptural_minimum_suffices_for_communion, instrumental).
narrative_ontology:cs_reference_frame('9d6869da-8437-4ed5-952e-6fd9fc701519', conciliar_mediating_settlement).
narrative_ontology:cs_drift_state('9d6869da-8437-4ed5-952e-6fd9fc701519', post_constantinopolitan_settlement, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9d6869da-8437-4ed5-952e-6fd9fc701519', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, homoiousian_bishops).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, nicene_confessors).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, anomoean_strict_arians).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, ordinary_clergy_and_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, ordinary_clergy_and_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes councils, promulgates the resulting formulas as tests of clerical standing, and enforces subscription through deposition and exile. Shifts between successive formulas (Dedication creed, Sirmium drafts, the Homoean formula of Ariminum-Seleucia) as political needs change, since no formula binds the throne. Collects a nominally unified church and the legitimacy that comes with it; absorbs unrest whenever a formula fails to hold.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_authority, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Draft and promote the mediating formulas (Basil of Ancyra's circle after Ancyra 358, the council drafters at Sirmium, Ariminum, and Seleucia). The formula keeps their sees secure without requiring them to endorse either the Nicene term or the Arian denial. They supply the theological vocabulary and the council majorities, pay in perpetual renegotiation as each emperor revises the settlement, and face a forced choice when the final pro-Nicene settlement arrives.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, homoiousian_bishops, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, homoiousian_bishops, agenda_setter).

% Refuse every formula that omits or replaces the Nicene term, regarding the exact confession as inseparable from the faith itself. Bear deposition, exile, and police pursuit (Athanasius through five exiles, Hilary banished to Phrygia, Lucifer of Cagliari deposed). Their exit is not available to them: signing a likeness-formula is, from where they stand, surrendering the gospel. They write the polemical and dogmatic literature that eventually wins the settlement.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, nicene_confessors, payer,
    moderate, generational, identity_locked, continental).

% Hold that the Son is unlike the Father in essence (Aetius, Eunomius) and are condemned by the middle party at Ancyra in 358 precisely for saying so. The compromise is built against them as much as for anyone: they supply the extremity that makes the middle formula look necessary, and they are excluded from its umbrella while remaining its occasion.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, anomoean_strict_arians, payer,
    moderate, biographical, constrained, continental).

% Required to subscribe, or to accept clergy who subscribe, to whichever formula currently prevails; congregations in Alexandria, Antioch, and Constantinople experience changing doctrines and changing clergy as councils succeed one another. They bear the disruption and the catechetical instability, yet they also receive what the arrangement maintains: continuing communion, sacramental access, and a church that has not split into warring communions. Leaving is not a live option; there is nowhere else to be Christian.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, ordinary_clergy_and_laity, payer,
    powerless, immediate, trapped, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, ordinary_clergy_and_laity, beneficiary).

% Reconstruct the controversy from council acts, letters, and the surviving histories (Socrates Scholasticus, Sozomen, Philostorgius, Ammianus Marcellinus from outside the dispute). They attest what the formulas accomplished, whom each revision exiled, and how the middle party ended, from a seat that belongs to no party.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__semi_arian_reading, imperial_authority).
narrative_ontology:fixing_cost_class(homoousios_christology__semi_arian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal formula that enough bishops on both anti-Arian and anti-Nicene sides can subscribe to keep the imperial church in communion: one eucharist, one episcopal body, one enforceable settlement, while leaving the underlying metaphysical dispute unresolved.
% TRANSFER_FUNCTION: Moves doctrinal assent from the whole episcopate toward the imperial center (each formula is a loyalty test administered from the court); moves sees, exiles, and career security according to formula loyalty; and moves theological precision downward to a deliberately vague minimum that all parties must accept.
% ABSENT_VOICES: Lay congregations had no seat at any council that set the formulas they were required to accept; rural clergy learned each new settlement by decree. Both would have objected to repeated re-definition of the faith by closed assemblies. The strict parties were present but coerced, which is a different defect: their objection was heard and answered with exile.
% DISAPPEARANCE_RATIONALE: If the mediating-formula regime vanished overnight (say, in 359), the church splits immediately into Nicene and Arian communions with no common subscription, the emperor must openly persecute one side instead of managing both, eucharistic fellowship ruptures across the East, and the sequence of events that produced the 381 settlement — including the reconciliation of the middle party — does not happen in the same shape.
% FOUNDING_PROBLEM: The Arian controversy threatened to split the church and destabilize the empire; Nicaea's homoousios commanded deep but minority adherence in the Greek-speaking East; some formula was needed that a working majority of Eastern bishops could sign without either surrendering the Son's real divinity or endorsing the Nicene term they feared conflated Father and Son.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Athanasius's De Synodis — a pro-Nicene source hostile to the compromise — concedes the homoiousians' sincere anti-Arian intent; the pagan Ammianus Marcellinus independently attests the strife the formulas were managing; and modern critical scholarship on the Homoian and Homoiousian parties confirms both that the founding problem was real and that it was resolved by pro-Nicene victory and middle-party absorption, not by the compromise succeeding on its own terms.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction peaks mid-interval (0.53-0.55) when the formula regime narrows and enforcement hardens under Constantius and then Valens, then falls to 0.38 by 381 as the arrangement dissolves into the pro-Nicene settlement; base_properties values reflect the terminal state at interval end, with the series carrying the operating history. Suppression tracks enforcement capacity rather than doctrine: it rises with each imperial crackdown (0.35 to 0.62 across the Constantius era), and its two collapses (Julian's 361-363 recall of exiles, Theodosius's redirection of enforcement after 380) are visible in the series. Theater rises steadily (0.22 to 0.46) as successive councils increasingly ratify predetermined court outcomes rather than negotiate — by the Ariminum-Seleucia twin councils of 359 the drafting was done before the bishops assembled. Accessibility_collapse is low (0.35) because alternatives never collapsed: Nicenes wrote from exile, Anomoeans preached openly, and the compromise's failure to close exits is precisely why it lost. Resistance is high (0.65): five Athanasian exiles, Hilary's De Synodis, the Luciferian schism, rioting in Alexandria. The enforcement cycle is driven by reign changes, not intermittent reinforcement as a technique — though each re-rise exploited accumulated subscription fatigue, and the flat stretch under Valens (t=24 to t=32) shows enforcement straining against growing resistance. All three series share one six-point grid so no metric's end-state is silently substituted into earlier times.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the imperial seat the arrangement is a governance instrument performing as designed: a subscribable church bought at the price of hounding a minority. From the homoiousian bishops' seat it is a genuine theological achievement — the only position that honored both the Son's divinity and the Father-Son distinction — maintained under constant pressure from both flanks. From the confessors' seat it is enforced apostasy-light: a demand to sign away the exact term on which salvation was thought to turn. From the pews it is mostly weather: changing clergy, changing words, uninterrupted communion. The engine computes these per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial authority sits near the beneficiary end (declared beneficiary, agenda-setting power, arbitrage-grade exit — it can and does replace the formula at will). The homoiousian bishops derive low-to-moderate d: declared beneficiaries with constrained exit, paying in perpetual renegotiation. The nicene_confessors sit near the full-target end: declared victims, identity_locked exit — their refusal is constitutive of who they are, so the extraction lands at full weight. The anomoean_strict_arians are likewise targets with constrained exit. Ordinary_clergy_and_laity carry a directionality override (powerless atom, d=0.60): the derivation from their victim declaration alone would push them toward the full-target end, but they demonstrably share the coordination benefit (continued communion, no schism), so the override pulls them to a genuinely dual position. Ecclesiastical_historians take the analytical seat and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — bridge the substance dispute with a subscribable formula — died twice: first in practice, when the pro-Nicene settlement made precision mandatory again after 381, and finally in existence, when the middle party's own members signed that settlement. Crucially, the arrangement dissolved rather than degenerating: absorption prevented the zombie outcome in which a dead formula is maintained theatrically by administrators who profit from its persistence. The classification guards against two mislabelings. Calling this pure coordination erases the exiled confessors and the condemned Anomoeans — the people the middle was built on. Calling it pure extraction erases what it actually held together: four decades of imperfect but real communion across an empire-spanning church, at lower enforcement cost than either flank's program would have required. The tangled-rope claim keeps both facts load-bearing. Mandatrophy is declared resolved: the mandate has outlived its function, and the honest terminal state of this constraint is nonexistence-by-absorption, not persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_disagreement,
    'This constraint instantiates the semi_arian_reading of the homoousios_christology kernel: how would the sibling readings (pro_nicene_reading, arian_reading) restructure the beneficiary and victim surface, and is the inter-reading disagreement located in the salvation-relevance of substance-language or in the underlying metaphysics itself?',
    'Generate the sibling stories and compare: pro_nicene_reading locates the dispute in whether identical substance is required for salvation; arian_reading locates it in whether the Son is derivative. Cross-reading comparison of epsilon, victim sets, and enforcement profiles localizes the disagreement.',
    'If the disagreement is located in salvation-relevance rather than metaphysics, the three readings form a spectrum with transferable middle positions and this compromise was a stable resting point; if in the metaphysics, the readings are rigidly separated and the compromise was inherently transient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Committer-frame omega: one reading of the homoousios kernel, naming siblings and the structural location of the disagreement.').

omega_variable(
    verbal_vs_substantive_dispute,
    'Was the homoiousios/homoousios disagreement verbal (resolvable by glossing, as the post-381 absorption of the middle party suggests) or substantive?',
    'Compare the middle party''s pre-merger objections to homoousios (documented fear that it conflated Father and Son) with the glosses under which its members accepted it at and after Constantinople 381; if the same concerns are answered by clarification rather than concession, the dispute was verbal.',
    'If verbal, the enforcement-era punishment of dissenters extracted compliance over a wording difference — sharpening the extraction assessment of the arrangement; if substantive, part of the coordination cost was irreducible and the compromise''s failure was overdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verbal_vs_substantive_dispute, conceptual, 'Whether the compromise''s central dispute was terminological or metaphysical.').

omega_variable(
    imperial_capture_ambiguity,
    'Was the mediating-formula regime primarily ecclesial coordination or an instrument of imperial consolidation wearing coordination''s clothing?',
    'Track formula stability across reigns: formulas that shift with each emperor''s preference (as they did — Dedication creed, Sirmium drafts, Homoean formula) indicate the throne, not the church, held the pen; correlate drafting venues with court residence.',
    'If captured, the beneficiary structure skews imperial, effective extraction amplifies for all non-court seats, and the coordination-function half of the tangled-rope claim weakens toward pure enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_capture_ambiguity, empirical, 'Degree to which the compromise served imperial consolidation rather than ecclesial unity.').

omega_variable(
    absorption_voluntariness,
    'Was the middle party''s post-381 absorption into the pro-Nicene settlement a voluntary convergence (its axioms genuinely overridden by argument) or coerced conformity under Theodosian legal pressure?',
    'Compare the timing and content of middle-party signatures at Constantinople 381 against the sequencing of the Edict of Thessalonica and subsequent anti-heresy legislation; distinguish bishops who signed before enforcement reached them from those who signed after.',
    'If voluntary, the semi-Arian reading died by persuasion and its foundational axiom is honestly marked overridden within its own tradition; if coerced, the reading was suppressed rather than refuted, raising the arrangement''s terminal suppression and complicating the absorption narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_voluntariness, empirical, 'Whether the reading''s historical dissolution was persuasive or coercive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__semi_arian_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(homo_tr_t8, homoousios_christology__semi_arian_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(homo_tr_t16, homoousios_christology__semi_arian_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(homo_tr_t24, homoousios_christology__semi_arian_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(homo_tr_t32, homoousios_christology__semi_arian_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__semi_arian_reading, theater_ratio, 40, 0.46).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__semi_arian_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(homo_be_t8, homoousios_christology__semi_arian_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(homo_be_t16, homoousios_christology__semi_arian_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(homo_be_t24, homoousios_christology__semi_arian_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(homo_be_t32, homoousios_christology__semi_arian_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__semi_arian_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__semi_arian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(homo_su_t8, homoousios_christology__semi_arian_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(homo_su_t16, homoousios_christology__semi_arian_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(homo_su_t24, homoousios_christology__semi_arian_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(homo_su_t32, homoousios_christology__semi_arian_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__semi_arian_reading, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, arian_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the homoousios_christology kernel per the epsilon-invariance principle: the colloquial label 'the substance controversy' covers three structurally distinct arrangements. arian_reading (upstream, earliest) authors epsilon for the created-and-subordinate arrangement; this story authors epsilon for the mediating-compromise arrangement (lower enforcement epsilon than either flank's program, genuine coordination function, asymmetric extraction of the strict parties); pro_nicene_reading (downstream, victorious) authors epsilon for the consubstantial settlement that absorbed this one after 381. The upstream Arian claim is cited as the occasion for the compromise; the compromise's absorption is cited as evidence of the pro-Nicene settlement's breadth. Each file links the other two through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__semi_arian_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
