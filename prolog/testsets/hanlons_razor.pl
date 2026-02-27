% ============================================================================
% CONSTRAINT STORY: hanlons_razor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanlons_razor, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanlons_razor
 *   human_readable: Hanlon's Razor: Constraint on Intentionality Attribution
 *   domain: social/cognitive
 *
 * SUMMARY:
 *   Hanlon's Razor is a heuristic constraint that models how we attribute
 *   causality to others' harmful behavior. The principle — 'never attribute
 *   to malice that which is adequately explained by stupidity' — functions as
 *   both a coordination norm (reducing blame escalation and social friction)
 *   and an extraction mechanism (enabling institutions to evade
 *   accountability by claiming incompetence rather than negligence). The
 *   constraint exhibits all six classification types from different
 *   perspectives because it simultaneously solves a genuine coordination
 *   problem (preventing conflict spirals from malice attribution) and enables
 *   a genuine extraction problem (preventing detection of systematic
 *   negligence). The theater ratio has increased from 0.35 to 0.58 over the
 *   interval as the norm has shifted from a genuine epistemic heuristic to a
 *   performative appeal to charity in contexts of documented institutional
 *   failure. The extractiveness has increased from 0.28 to 0.52 as
 *   accountability mechanisms have become available, making negligence more
 *   detectable; institutions now invoke Hanlon's Razor specifically to block
 *   investigation despite available evidence.
 *
 * KEY AGENTS:
 *   - Epistemically Trapped Victim: Primary victim (powerless/trapped) — bears uncertainty about whether harm was intentional or negligent; cannot escape this cognitive trap without external verification
 *   - Institutional Negligent Actor: Primary beneficiary (institutional/arbitrage) — captures plausible deniability and deflects accountability investigation; can exit only by demonstrating competence
 *   - Collective Social Coordination Function: Moderate/constrained — genuine benefit from reducing blame escalation, but bears extraction risk when applied to documented negligence
 *   - Accountability and Evidence Movement: Organized/constrained — constructing alternative pathways (transparency, regulatory duty of care, algorithmic audits) that sunset the razor's protective function
 *   - Rationalist Epistemic Community: Analytical actor — traditional guardian of the razor as epistemic method; now at risk of weaponizing it as theater
 *   - Information Theorist (Civilizational View): Analytical/analytical — risks treating a contingent social norm as though it were a Bayesian law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanlons_razor, 0.52).
domain_priors:suppression_score(hanlons_razor, 0.65).
domain_priors:theater_ratio(hanlons_razor, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanlons_razor, extractiveness, 0.52).
narrative_ontology:constraint_metric(hanlons_razor, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hanlons_razor, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanlons_razor, tangled_rope).
narrative_ontology:human_readable(hanlons_razor, "Hanlon's Razor: Constraint on Intentionality Attribution").
narrative_ontology:topic_domain(hanlons_razor, "social/cognitive").

domain_priors:requires_active_enforcement(hanlons_razor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanlons_razor, incompetent_actors).
narrative_ontology:constraint_beneficiary(hanlons_razor, institutional_negligence).
narrative_ontology:constraint_beneficiary(hanlons_razor, benefit_of_doubt_norm).
narrative_ontology:constraint_victim(hanlons_razor, harm_detection_epistemic_reliability).
narrative_ontology:constraint_victim(hanlons_razor, coordination_of_response).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMICALLY TRAPPED VICTIM (SNARE) — A person harmed by systematic negligence, incompetence, or institutional failure faces a snare when Hanlon's Razor is applied to their situation. They are forced to remain uncertain whether the harm was intentional malice or mere stupidity. Cannot exit this epistemic trap; bears full cost of misattribution. The constraint extracts their ability to construct accurate causal models of their own harm.
constraint_indexing:constraint_classification(hanlons_razor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COLLECTIVE COORDINATION FUNCTION (TANGLED ROPE) — Hanlon's Razor serves a genuine coordination function: it lowers social friction by discouraging malice attribution when simpler explanations fit the evidence. This reduces costly blame cycles and conflict escalation in everyday interaction. But it also enables extraction: institutions and individuals benefit from the ambiguity it preserves about their negligence. Constrained exit — one cannot abandon the norm without social cost, but the norm itself creates asymmetric risk allocation.
constraint_indexing:constraint_classification(hanlons_razor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL NEGLIGENT ACTOR (ROPE) — An organization or powerful actor benefits from Hanlon's Razor as a pure coordination mechanism: it provides plausible deniability and deflects scrutiny of systematic failures. The constraint functions as a norm that solves the 'blame allocation' problem: others will interpret failures charitably, assuming incompetence rather than malice. Net beneficiary with full arbitrage — can exit by demonstrating competence, but has no incentive to do so.
constraint_indexing:constraint_classification(hanlons_razor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RATIONALIST EPISTEMIC COMMUNITY (PITON) — Rationalism and skepticism traditionally invoked Hanlon's Razor as a methodological norm: don't attribute to conspiracy what incompetence explains. This served an epistemic function (filtering implausible conspiracy theories) but has become largely performative. In contexts of systematic institutional failure, invoking Hanlon's Razor now functions as theater — a ritualistic appeal to charity that substitutes for actual investigation. The cognitive tool has degraded; institutional inertia maintains it as a norm even when it prevents truth-seeking.
constraint_indexing:constraint_classification(hanlons_razor, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: ACCOUNTABILITY AND EVIDENCE MOVEMENT (SCAFFOLD) — Organized actors pushing for transparency, harm documentation, and evidence-based causality attribution are constructing alternative pathways that sunset the utility of Hanlon's Razor. Regulatory requirements (adverse event reporting, algorithmic audits, duty of care standards) make negligence more expensive and more detectable than before. These actors see the razor as a temporary coordination failure being solved by institutional maturation — as accountability systems improve, the need to assume good faith in the absence of evidence declines. Has sunset logic: as transparency increases, the razor's protective function diminishes.
constraint_indexing:constraint_classification(hanlons_razor, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a pure information-theoretic perspective, Hanlon's Razor reflects an immutable principle: stupidity (variance in competence) always has greater prior probability than coordinated malice (requires intentional alignment). The razor is a Bayesian heuristic, not a social norm — it follows from the base rates of human error vs. organized conspiracy. This perspective risks treating a contingent social norm as though it were a natural law of cognition.
constraint_indexing:constraint_classification(hanlons_razor, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanlons_razor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanlons_razor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hanlons_razor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanlons_razor, TR),
    TR >= 0.70.

:- end_tests(hanlons_razor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Hanlon's Razor provides genuine coordination benefit (lowers blame cycles, reduces conflict escalation) worth ~0.25-0.30, but the extraction mechanism (protecting negligent actors from accountability) has grown as institutional contexts have become more complex and as evidence-gathering capabilities have improved. When invoked in contexts where investigation is feasible (regulated industries, platform accountability, medical errors), the razor now functions primarily as a barrier to detection, not as a conflict-reduction norm. Suppression (0.65): High. The norm is difficult to challenge without appearing hostile to charity and good faith. Direct questioning of Hanlon's Razor in institutional settings triggers social punishment. Theater ratio (0.58): Moderate-high. The norm began as genuine epistemology (skepticism toward conspiracy theories) but has become performative in institutional contexts. Invoking it is now often a substitute for investigation rather than a principle guiding investigation. The shift from 0.35 to 0.58 reflects this degradation — what was once a methodological principle is now a ritualistic appeal.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap exists between the victim (snare: χ very high) and the institutional actor (rope: χ minimal/negative). Both experience the same constraint, but their structural positions invert its meaning. For the victim, Hanlon's Razor blocks investigation and accountability. For the institution, it provides coordination (reduces blame spirals). The scaffold perspective reveals that this gap is not permanent — as evidence-gathering and accountability mechanisms mature, the razor's protective function decays. The piton perspective reveals that the norm persists through institutional inertia despite reduced function in contexts where investigation is feasible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by exit options and structural position. Beneficiaries (negligent institutions) have arbitrage exit — they can evade accountability if they demonstrate competence, but have no incentive to do so. Their d is low (~0.15), producing negative/minimal χ from their perspective. Victims have trapped exit and victim status — their d is high (~0.90), producing high χ. The coordination function has constrained exit and mixed beneficiary/victim status — d ~0.50, producing moderate χ. The organized accountability movement has constrained exit but some agency — d ~0.45, with declining χ over time as their sunset logic materializes. The rationalist community has analytical exit — d ~0.72, deriving a middle χ that reflects their risk of weaponizing the tool.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PARTIALLY UNRESOLVED: Hanlon's Razor resolves the first layer of mandatrophy (distinguishing genuine coordination from extraction) by showing that both functions are structurally real. The norm does solve a genuine problem (preventing malice attribution spirals) and does enable extraction (protecting negligence from investigation). But it does not fully resolve whether the net effect is coordination or extraction — that depends on empirical facts about base rates and harm outcomes that are not yet measured. Omega variables directly address this: if the base rate of malice is truly low and the harm reduction from the norm is documented, then coordination dominates. If investigation reveals that negligence is systematically hidden behind the norm, then extraction dominates. The progression of extractiveness from 0.28 to 0.52 suggests a temporal shift — the norm may have begun as genuine coordination but transformed into extraction as institutions learned to weaponize it. This is the lifecycle pattern of tangled rope: a real coordination function that attracts parasitic extraction, causing the theater ratio to rise and the true coordination value to become occluded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_detection_baseline,
    'What is the true base rate of negligence vs. malice in contexts where Hanlon''s Razor is invoked? Does actual outcome distribution match the assumed prior?',
    'Historical case analysis: audit documented outcomes in organizational failures, accidents, regulatory violations, and interpersonal harms; compare assumed stupidity explanation vs. discovered malice or negligence after investigation.',
    'If stupidity base rate < 0.60: Hanlon''s Razor systematically over-extends charity, enabling extraction. If base rate > 0.85: the razor is well-calibrated and coordination justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_detection_baseline, empirical, 'True base rate of negligence vs. malice in harm scenarios').

omega_variable(
    detection_cost_asymmetry,
    'Who bears the cost of investigation to distinguish negligence from malice — the victim (who must prove malice) or the actor (who must prove competence)?',
    'Comparative analysis of burden of proof in civil/criminal law, regulatory contexts, and institutional accountability; measurement of actual investigation costs allocated to each party.',
    'If victim bears cost: Hanlon''s Razor is extraction mechanism (snare). If actor bears cost: it is coordination incentive (rope). If asymmetric: tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(detection_cost_asymmetry, empirical, 'Asymmetry in cost of proving negligence vs. malice').

omega_variable(
    harm_distribution_temporal,
    'Over time, does invoking Hanlon''s Razor reduce actual harm (by lowering conflict escalation) or increase it (by enabling undetected negligence)?',
    'Longitudinal comparison: scenarios where Hanlon''s Razor was applied as a norm vs. scenarios where accountability investigations occurred; measurement of harm recurrence rates, institutional improvement, and victim outcomes.',
    'If harm decreases: coordination function genuine, rope classification justified. If harm increases or recurs: extraction mechanism dominates, snare/tangled rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_distribution_temporal, empirical, 'Net harm reduction from applying Hanlon''s Razor vs. requiring accountability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanlons_razor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanlon_tr_t0, hanlons_razor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hanlon_tr_t5, hanlons_razor, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hanlon_tr_t10, hanlons_razor, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hanlon_be_t0, hanlons_razor, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hanlon_be_t5, hanlons_razor, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(hanlon_be_t10, hanlons_razor, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanlons_razor, enforcement_mechanism).
narrative_ontology:affects_constraint(hanlons_razor, institutional_negligence_concealment).
narrative_ontology:affects_constraint(hanlons_razor, blame_attribution_equilibrium).
narrative_ontology:affects_constraint(hanlons_razor, epistemic_charity_norm).

% DUAL FORMULATION NOTE:
% Hanlon's Razor decomposes into two structurally distinct claims: (1) a Bayesian prior claim (stupidity has higher base rate than malice), which is a mathematical observation; and (2) a social norm claim (we should preferentially attribute harm to stupidity to reduce conflict), which is a coordination mechanism that can enable extraction. The first claim is mountain-adjacent (information-theoretic limit); the second is tangled rope (coordination with extraction). This story focuses on the second claim, where the constraint operates at the social/institutional level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanlons_razor, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
