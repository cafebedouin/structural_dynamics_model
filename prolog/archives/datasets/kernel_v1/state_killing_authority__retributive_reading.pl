% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__retributive_reading
 *   human_readable: State Killing Authority (Retributive Reading): Legitimacy Through Proportional Desert
 *   domain: criminal_justice/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint models the retributive reading of state killing
 *   authority: the claim that legitimate execution requires proportionality
 *   between the severity of the crime and the severity of the punishment,
 *   grounded in the principle that justice itself demands restoration of
 *   moral balance through proportional retribution. The retributive tradition
 *   treats proportional desert as the only legitimate ground for capital
 *   punishment. This is ONE READING of a contested kernel
 *   (state_killing_authority) that includes deterrence and abolition readings
 *   as sibling alternatives. The retributive reading instantiates a
 *   tangled_rope constraint because it combines genuine coordination
 *   (restoring moral balance for victims' families, manifesting justice
 *   authority) with asymmetric extraction (when proportionality thresholds
 *   are calibrated loosely or applied arbitrarily, condemned persons bear
 *   costs disproportionate to legitimate desert). The constraint exhibits
 *   high theater ratio (0.65) because proportionality assessment in capital
 *   cases is substantially performative: judges articulate desert doctrine
 *   while sentences are driven by sentencing guidelines, plea bargains, and
 *   precedent. The theater ratio has risen over the measurement interval as
 *   appellate proportionality review has formalized, creating an appearance
 *   of careful calibration that masks underlying arbitrariness. The
 *   measurement trajectory shows both theater_ratio and base_extractiveness
 *   rising, indicating institutional hardening: the proportionality doctrine
 *   is becoming more entrenched as formal justification while actual
 *   proportional calibration remains contested.
 *
 * KEY AGENTS:
 *   - Murder Victims' Families: Primary beneficiary (moderate/constrained) — experience moral restoration when state executes murderer proportionally; constrained by procedural entanglement and finality
 *   - Condemned Persons (Disproportionate Cases): Primary victim (powerless/trapped) — when execution exceeds proportional desert, they bear maximum extraction with no exit; the retributive framework becomes mechanism of their extraction
 *   - State Retributive Authority: Institutional beneficiary (institutional/arbitrage) — derives legitimacy from retributive doctrine; experiences constraint as coordination of its proper role
 *   - Murder Victims' Families (Disproportionate Cases): Secondary victim (moderate/constrained) — when state exceeds proportionality in their name, their justice claim becomes co-opted for excessive extraction
 *   - Proportionality Advocates: Organized actor (organized/constrained) — appellate courts, clemency boards, constitutional doctrine attempting to scaffold proportionality review; see sunset in narrowing death penalty
 *   - Retributive Doctrine as Institutional Practice: Institutional actor (institutional/arbitrage) — formal justification system maintained through inertia despite empirical critiques; high theater ratio indicates performative character
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing retributive desert as immutable law of justice rather than contingent moral tradition; false-summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_reading, 0.58).
domain_priors:suppression_score(state_killing_authority__retributive_reading, 0.72).
domain_priors:theater_ratio(state_killing_authority__retributive_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_reading, "State Killing Authority (Retributive Reading): Legitimacy Through Proportional Desert").
narrative_ontology:topic_domain(state_killing_authority__retributive_reading, "criminal_justice/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_reading, 'c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8').
narrative_ontology:cs_kernel_codification('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', fixed_text).
narrative_ontology:cs_authority_grounding('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', lineage).
narrative_ontology:cs_interpretation_layer_present('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8').
narrative_ontology:cs_reading_relation('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', state_killing_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', state_killing_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', foundational, moral_desert_determines_proportional_punishment).
narrative_ontology:cs_axiom_status(moral_desert_determines_proportional_punishment, holdable).
narrative_ontology:cs_axiom_grounding('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', moral_desert_determines_proportional_punishment, deontological).
narrative_ontology:cs_axiom('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', foundational, state_authority_derives_from_justice_restoration).
narrative_ontology:cs_axiom_status(state_authority_derives_from_justice_restoration, holdable).
narrative_ontology:cs_axiom_grounding('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', state_authority_derives_from_justice_restoration, deontological).
narrative_ontology:cs_reference_frame('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', proportional_moral_desert).
narrative_ontology:cs_drift_state('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', contemporary_capital_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c4c8aa7c-7e7d-4011-be39-7d8e64b4c2d8', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_reading, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_reading, murder_victims_moral_restoration).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_reading, state_moral_authority).
narrative_ontology:constraint_victim(state_killing_authority__retributive_reading, condemned_persons_excess_execution).
narrative_ontology:constraint_victim(state_killing_authority__retributive_reading, epistemic_reliability_proportionality_assessment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON (SNARE) — When execution exceeds proportional desert for the crime, the condemned faces maximum extraction and suppression. No exit options exist. The retributive framework itself becomes the extraction mechanism: the proportionality requirement is stated but unenforceable (no appellate body can reverse execution). The condemned experiences pure extraction — death penalty beyond what justice requires. Powerless to challenge; trapped by finality of execution.
constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MURDER VICTIM'S FAMILY (TANGLED ROPE) — When execution is proportional to the offense, the family experiences coordination: the retributive mechanism restores moral balance, fulfilling a legitimate claim for justice. But they are also constrained by the execution's finality and by dependence on state machinery to calibrate and carry out proportionality. They benefit from moral restoration (coordination function) while bearing costs of procedural entanglement, testimony burden, and the psychological toll of witnessing state violence. Moderate power; high constraint cost; mixed extraction.
constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RETRIBUTIVE JUSTICE AUTHORITY (ROPE) — The state apparatus implementing retributive justice claims pure coordination: it exists to restore moral balance through proportional punishment. From this perspective, execution when calibrated to desert is a legitimate institutional function, not extraction. The state authority experiences the constraint as coordination itself — executing its proper role. High power; arbitrage options (can redefine desert thresholds within retributive logic); sees extraction as minimal or absent (benefits from legitimacy of moral authority).
constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROPORTIONALITY ADVOCATES (SCAFFOLD) — Organized agents (appellate courts, proportionality doctrines, clemency boards) view the constraint as temporary: proportionality review mechanisms are scaffolding designed to prevent disproportionate execution. This perspective expects proportionality thresholds to mature (through case law and constitutional doctrine) toward a sunset where execution becomes rare or impossible because few crimes meet the refined proportionality bar. Low effective extraction because the coalition has agency and sees an institutional exit path (narrowed death penalty, life without parole alternatives).
constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RETRIBUTIVE DOCTRINE AS PITON — Across most of the globe, retributive desert theory persists as formal justification for criminal punishment despite widespread empirical critiques (deterrence not proven, rehabilitation neglected, recidivism unchanged). The doctrine is maintained through institutional inertia: it provides legitimacy for punishment systems that would require wholesale reconstruction if abandoned. High theater ratio because proportionality assessment is largely performative — judges apply desert doctrine as ritual justification for sentences determined by sentencing guidelines, precedent, and plea bargains. The constraint persists not because retributivism works but because institutional actors have become the doctrine they were meant to serve.
constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL JUSTICE MOUNTAIN (FALSE SUMMIT) — From a civilizational perspective, retributive desert is presented as a natural law of justice: moral wrongs inherently require proportional punishment; this is a universal human moral intuition transcending culture and law. Retributivists claim this constraint is immutable because rooted in the very meaning of justice and moral responsibility. However, the structural data reveals beneficiaries and victims, making this a false summit: the constraint naturalizes a contingent moral and institutional commitment held by one tradition, not a law of justice itself. The 'natural justice' framing conceals that retributivism is one reading among contested alternatives.
constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_killing_authority__retributive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_killing_authority__retributive_reading, TR),
    TR >= 0.70.

:- end_tests(state_killing_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The retributive framework provides genuine coordination (moral restoration) for victims' families and legitimate state authority, but also enables extraction. When proportionality is calibrated loosely or applied arbitrarily, condemned persons bear costs exceeding legitimate desert. The measurement trajectory (0.42 → 0.58) reflects institutional hardening: as proportionality doctrine has formalized, it has become more extractive, not less, because formal review creates an appearance of proportionality without substantive improvement in calibration. Suppression (0.72): High. Multiple barriers prevent exit from retributive sentences: appellate review is limited, clemency is discretionary and rarely granted, proportionality standards are jurisdiction-dependent and often loose, and execution itself is irreversible. Condemned persons face maximum suppression. Theater ratio (0.65): Moderate-high. Proportionality assessment in capital cases is substantially ritualistic: judges apply desert doctrine as formal justification while sentences are determined by sentencing guidelines, prosecutorial discretion, and plea bargains. Appellate review adds theater without substantive proportionality improvement. The trajectory (0.45 → 0.65) reflects increasing institutionalization of the doctrine without corresponding calibration precision.
 *
 * PERSPECTIVAL GAP:
 *   The retributive reading produces substantial perspectival divergence. The murder victims' family sees coordination and moral restoration (Rope/Tangled Rope perspectives). The condemned person sees pure extraction if disproportionate execution occurs (Snare). The state authority sees coordination of its legitimate role (Rope). The proportionality advocates see a temporary coordination problem being solved by appellate doctrine (Scaffold). The retributive institution itself, viewed through historical analysis, appears as degraded doctrine maintained through inertia (Piton). The analytical observer risks seeing natural law (Mountain — retributive desert as immutable justice principle). The perspectival gap reveals the constraint's vulnerability: it claims legitimacy through coordination but functions partially as extraction when proportionality standards are loose. The gap between beneficiary and victim perspectives shows that the same retributive structure restores justice in some cases and extracts excess punishment in others, depending on calibration.
 *
 * DIRECTIONALITY LOGIC:
 *   The retributive reading generates different directionality values depending on whether execution is proportional to offense or exceeds proportional desert. When execution is calibrated within legitimate desert bounds, murder victims' families (moderate power, constrained exit) experience low-to-moderate d (0.55-0.65) because they benefit from moral restoration while bearing constraint costs. The state authority (institutional power, arbitrage exit) experiences very low d (0.10-0.20) because they benefit from legitimacy without constraint. When execution EXCEEDS proportional desert, the condemned person (powerless, trapped) experiences maximum d (0.95) and the family experiences moderate-high d (0.60-0.70) because their justice claim is co-opted for excessive extraction. The constraint's χ value depends entirely on whether proportionality is actually enforced — the same institutional structure produces different extractiveness depending on calibration. This indeterminacy is itself evidence of extraction: the framework provides no enforceable constraint on proportionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that retributive legitimacy depends on actual proportionality enforcement, not merely the proportionality principle. If proportionality is indeterminate (omega_1), the constraint becomes extraction disguised as justice. If retributivism coexists with rather than forecloses alternative legitimacy grounds (omega_2), then retributive execution of someone who would not be executed under purely utilitarian analysis reveals the death penalty is serving retributive principle, not justice itself. If condemned persons are sacrificed specifically to the retributive principle (omega_3), then retributivism is extractive relative to alternative moral frameworks. The mandatrophy resolves: the retributive reading is legitimate ONLY IF proportionality is measurable, enforceable, and does not produce different outcomes than what alternative legitimacy frameworks would permit. Current evidence suggests partial fulfillment at best — proportionality doctrine provides legitimacy appearance with uncertain calibration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_indeterminacy,
    'How is proportional desert measured and verified? What makes a punishment ''proportional'' rather than arbitrary?',
    'Comparative law analysis of proportionality doctrine across jurisdictions; examination of appellate decisions reversing sentences as disproportionate; identification of whether proportionality judgments converge or diverge',
    'If proportionality is indeterminate or jurisdiction-dependent: the constraint is extraction disguised as justice doctrine (ε rises toward 0.70+, Snare from multiple perspectives). If proportionality doctrine achieves consensus and appellate enforceability: the constraint is genuine coordination (ε falls toward 0.35, Tangled Rope confirmed). Current evidence suggests partial convergence with significant residual dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_measurement_indeterminacy, empirical, 'Whether proportional desert can be measured consistently or becomes arbitrary calibration').

omega_variable(
    retributivism_vs_competing_legitimacy_claims,
    'Is retributive desert the only legitimate ground for state killing authority, or do deterrence, social protection, or rehabilitation provide equally valid grounds?',
    'Philosophical analysis of whether retributivism logically forecloses or merely coexists with alternative theories; empirical comparison of outcomes under retributive vs utilitarian penalty regimes',
    'If retributivism forecloses alternatives: condemned under disproportionate sentences are extracted via illegitimate authority (ε → Snare). If retributivism coexists with alternatives: the constraint is one interpretive reading of state punishment authority, not the only legitimate one. This directly affects whether sibling readings (deterrence, abolition) have standing within a single moral framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributivism_vs_competing_legitimacy_claims, conceptual, 'Whether retributive justice logically forecloses alternative grounds for legitimate punishment').

omega_variable(
    desert_counterfactual_asymmetry,
    'Do retributive systems execute persons whose offenses, under a proportionality standard applied *without* the retributive desert premise, would not warrant death penalty?',
    'Comparison of sentence outcomes under purely retributive vs. hybrid systems (retributive + proportionality caps); identification of cases where life sentence would result if utilitarian cost-benefit analysis were applied instead of desert principle',
    'If asymmetry exists: condemned persons are sacrificed to the retributive principle itself, making them victims of the doctrine, not the crime. This raises ε and strengthens Snare classification from victim perspective. If sentences converge across theories: retributivism is not extracting additional punishment beyond what legitimate justice requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_counterfactual_asymmetry, empirical, 'Whether retributive sentencing produces different (higher) penalties than alternatives').

omega_variable(
    natural_law_disguise_diagnosis,
    'Is retributive desert presented as a natural law of justice (immutable, universal, transcultural) when in fact it is a contingent moral tradition? Does this naturalization serve the interests of those who benefit from retributive institutional structures?',
    'Historical analysis of retributivism''s origins and maintenance; identification of beneficiaries from the retributive framing; cross-cultural comparison of justice concepts to assess universality claims; examination of whether retributivism naturalizes institutional arrangements that would require justified defense if presented as contingent choices',
    'If false-summit diagnosis confirmed: the mountain classification is concealing a tangled_rope or snare. The constraint''s ε-invariance principle would require separate stories for ''retributive justice as contingent doctrine'' and ''justice as natural law of proportionality.'' This omega documents the grounds for that decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_disguise_diagnosis, conceptual, 'Whether retributive desert naturalizes a contingent institutional commitment as immutable justice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ska_ret_tr_t0, state_killing_authority__retributive_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ska_ret_tr_t10, state_killing_authority__retributive_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(ska_ret_tr_t20, state_killing_authority__retributive_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(ska_ret_be_t0, state_killing_authority__retributive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ska_ret_be_t10, state_killing_authority__retributive_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ska_ret_be_t20, state_killing_authority__retributive_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_reading, state_killing_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_authority__retributive_reading, state_killing_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel has three distinct readings, each instantiating different constraint types and ε values. The retributive reading (this file) models legitimacy through proportional desert and produces Tangled Rope with ε=0.58. The deterrence reading would model legitimacy through crime prevention and produces different beneficiary/victim relationships. The abolition reading would model state killing authority as categorically illegitimate and produces Snare or Mountain (depending on whether killing is treated as immutable or contingent). All three readings share the kernel (state killing authority justification) but instantiate different constraints because they define 'legitimate execution' differently. Sibling stories should be linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__retributive_reading, powerless, 0.95).
constraint_indexing:directionality_override(state_killing_authority__retributive_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
