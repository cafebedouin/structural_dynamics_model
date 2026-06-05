% ============================================================================
% CONSTRAINT STORY: intermediate_scrutiny_tier__real_differences_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intermediate_scrutiny_tier__real_differences_doctrine, []).

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
 *   constraint_id: intermediate_scrutiny_tier__real_differences_doctrine
 *   human_readable: Intermediate Scrutiny Tier: Real Differences Doctrine
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   The intermediate scrutiny tier's real differences doctrine creates a
 *   structured opportunity for sex classification to escape heightened
 *   review. The constitutional commitment (the intermediate tier itself)
 *   generates a safety valve: where the legislature credits a real difference
 *   between the sexes, the state need not show an exceedingly persuasive
 *   justification or a narrow tailoring that would satisfy strict scrutiny.
 *   The doctrine is doctrinally sound in principle — law may rationally
 *   respond to actual differences — but structurally vulnerable in practice
 *   because 'real differences' is an empirical claim that legislatures make,
 *   courts accept or reject, and science sometimes invalidates. The
 *   constraint exhibits the tangled-rope structure: genuine coordination
 *   function (aligning law with actual sex differences) paired with
 *   asymmetric extraction (burden of proof on the claimant, not the
 *   legislature; legislative deference to difference claims; suppression of
 *   alternative readings). The real differences doctrine has become more
 *   theatrical over time (1976–2016): courts cite real differences rhetoric
 *   while outcomes follow political trends, and the doctrine's predictive
 *   power has degraded. The analytics see the doctrine as potentially a false
 *   summit — naturalizing what may be constructed or temporary empirical
 *   claims into a permanent constitutional boundary.
 *
 * KEY AGENTS:
 *   - Legislative Sex Classifiers: Primary beneficiary (institutional/arbitrage) — gains access to a category of sex-based classifications that survive intermediate scrutiny by invoking real differences
 *   - Sex Discrimination Claimants: Primary victim (powerless/trapped) — cannot exit the doctrine's gatekeeping; if legislature credits a difference, claimant must disprove it to win
 *   - Women's Rights Advocacy Coalition: Secondary victim (organized/constrained) — can mount empirical challenges to difference premises but faces high burden of proof and legislative deference
 *   - Scientific Community on Sex Differences: Structural actor (organized/constrained) — as empirical understanding shifts, the factual premises underlying the doctrine erode, creating a built-in sunset dynamic
 *   - Courts Applying Intermediate Scrutiny: Institutional mediator (institutional/arbitrage) — accepts or rejects difference premises but has deferred to legislatures more than the doctrine's text suggests
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the doctrine as potentially a false summit that naturalizes contested empirical claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intermediate_scrutiny_tier__real_differences_doctrine, 0.48).
domain_priors:suppression_score(intermediate_scrutiny_tier__real_differences_doctrine, 0.52).
domain_priors:theater_ratio(intermediate_scrutiny_tier__real_differences_doctrine, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__real_differences_doctrine, extractiveness, 0.48).
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__real_differences_doctrine, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__real_differences_doctrine, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intermediate_scrutiny_tier__real_differences_doctrine, tangled_rope).
narrative_ontology:human_readable(intermediate_scrutiny_tier__real_differences_doctrine, "Intermediate Scrutiny Tier: Real Differences Doctrine").
narrative_ontology:topic_domain(intermediate_scrutiny_tier__real_differences_doctrine, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(intermediate_scrutiny_tier__real_differences_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(intermediate_scrutiny_tier__real_differences_doctrine, '0b5946f1-1a92-48e8-b720-a5673cb14afb').
narrative_ontology:cs_kernel_codification('0b5946f1-1a92-48e8-b720-a5673cb14afb', formalized).
narrative_ontology:cs_authority_grounding('0b5946f1-1a92-48e8-b720-a5673cb14afb', lineage).
narrative_ontology:cs_interpretation_layer_present('0b5946f1-1a92-48e8-b720-a5673cb14afb').
narrative_ontology:cs_reading_relation('0b5946f1-1a92-48e8-b720-a5673cb14afb', intermediate_scrutiny_tier__tier_drift_question, coexists_with).
narrative_ontology:cs_reading_relation('0b5946f1-1a92-48e8-b720-a5673cb14afb', intermediate_scrutiny_tier__vmi_exceedingly_persuasive, influences).
narrative_ontology:cs_axiom('0b5946f1-1a92-48e8-b720-a5673cb14afb', foundational, real_sex_difference_justifies_classification).
narrative_ontology:cs_axiom_status(real_sex_difference_justifies_classification, holdable).
narrative_ontology:cs_axiom_grounding('0b5946f1-1a92-48e8-b720-a5673cb14afb', real_sex_difference_justifies_classification, empirically_contingent).
narrative_ontology:cs_axiom('0b5946f1-1a92-48e8-b720-a5673cb14afb', secondary, legislature_identifies_and_credits_difference).
narrative_ontology:cs_axiom_status(legislature_identifies_and_credits_difference, holdable).
narrative_ontology:cs_axiom_grounding('0b5946f1-1a92-48e8-b720-a5673cb14afb', legislature_identifies_and_credits_difference, conventional).
narrative_ontology:cs_reference_frame('0b5946f1-1a92-48e8-b720-a5673cb14afb', difference_justified_classification_permissible).
narrative_ontology:cs_drift_state('0b5946f1-1a92-48e8-b720-a5673cb14afb', contemporary_postmodern_sex_research, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0b5946f1-1a92-48e8-b720-a5673cb14afb', '').
narrative_ontology:cs_kernel_id(intermediate_scrutiny_tier__real_differences_doctrine, intermediate_scrutiny_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intermediate_scrutiny_tier__real_differences_doctrine, legislative_sex_classifiers).
narrative_ontology:constraint_victim(intermediate_scrutiny_tier__real_differences_doctrine, sex_discrimination_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX DISCRIMINATION CLAIMANT (SNARE) — Trapped within the doctrine's real differences escape valve. Where the legislature credits a real biological or social difference between the sexes, the claimant cannot escape the classification even if it perpetuates harm. No exit: the claimant cannot challenge the legislature's factual premise (real difference) without mounting an implausible denial of biology. The doctrine's suppression is effective — it shifts the burden from the state to the claimant, who must now contest empirical facts about sex differences rather than challenging the classification itself.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE SEX CLASSIFIER (ROPE) — Experiences the doctrine as coordination: legislating on the basis of credited real differences is the mechanism for tailoring law to actual circumstances. The legislature benefits from the escape valve — it can classify by sex where judges accept the difference premise, avoiding stricter scrutiny. The legislature experiences this as legitimate law-making (coordinating with actual differences) rather than extraction. Arbitrage exit option: the legislature can choose when to invoke the real differences doctrine and when to use other classification rationales.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WOMEN'S RIGHTS ADVOCACY COALITION (TANGLED ROPE) — Organized victims with constrained exit options. The coalition can litigate the factual difference claim (the coordination function: testing whether credited differences are real), but faces significant barriers: scientific complexity, legislative deference in finding facts, and burden-shifting to the challenger. The doctrine both enables and constrains: it provides a litigation pathway (if you can disprove the difference, you win) but loads the burden of proof on the claimant. Extraction and coordination coexist.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SCIENTIFIC CONSENSUS ON SEX DIFFERENCE (SCAFFOLD) — The doctrine's escape valve is functionally temporary because the empirical ground shifts. As biological and social science demonstrates that credited 'real differences' are smaller, more contested, or more socially constructed than the doctrine assumes, the legislative rationale erodes. The doctrine has a built-in sunset: if the empirical premise (real difference) fails, the classification collapses. Theater ratio lower here because the litigation is substantive — not performative testing but genuine empirical inquiry into whether the difference is real.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE DOCTRINAL APPARATUS ITSELF (PITON) — Intermediate scrutiny and the real differences escape valve persist through institutional inertia despite degraded function. The doctrine was designed (Mississippi University for Women, mid-1980s) to create a middle ground between strict scrutiny and rational basis, with real differences as the safety valve. But the tier has drifted: applied with varying rigor across eras and benches, its predictive power has degraded (from the doctrinal perspective). Courts cite 'real differences' rhetoric while reaching results that follow the political moment rather than doctrine. High theater ratio: the doctrinal language performs legitimacy while the underlying classification logic follows other paths.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the doctrine appears to be applying immutable facts about human sexual dimorphism: some biological differences between the sexes are real and legally relevant, and tailoring law to actual circumstances (rather than to formal equality) is rational and fair. This perspective treats the real differences doctrine as a natural limit of equal protection — the point where the doctrine correctly defers to biological fact. However, the structural data reveals this as a false summit: the 'immutable fact' framing naturalizes a contested empirical claim and a contestable allocation of burden-shifting.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intermediate_scrutiny_tier__real_differences_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intermediate_scrutiny_tier__real_differences_doctrine, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intermediate_scrutiny_tier__real_differences_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intermediate_scrutiny_tier__real_differences_doctrine, TR),
    TR >= 0.70.

:- end_tests(intermediate_scrutiny_tier__real_differences_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The real differences doctrine permits sex classifications that would not survive strict scrutiny. The legislature captures the benefit of this permissive tier (can classify by sex where differences are credited), while claimants bear the cost of defending against difference claims. The extractiveness is not as severe as pure snare (0.66+) because the doctrine is not purely coercive — it provides a coordination function (aligning law with real differences) and a litigation pathway (claimants can challenge the difference premise). But it is higher than pure coordination (0.35 or lower) because the burden-shifting mechanism systematically advantages legislators and disadvantages claimants. The 0.48 reflects that the extraction mechanism is real but bounded by the doctrine's empirical gatekeeping. Suppression (0.52): Moderate-high. The doctrine suppresses claimants' ability to mount formal equal protection challenges by shifting the terrain to factual disputes about whether differences are real. This suppresses the structural challenge (that sex classification is categorically problematic) in favor of an empirical challenge (that the difference premise is false). Suppression is not complete (claimants can and do win by contesting premises) but is systematic (the burden falls on them, not the legislature). Theater ratio (0.58): Moderate-high, rising. The doctrine performs legitimacy through reference to 'real differences' while actual judicial outcomes follow political composition and era. Courts cite difference rhetoric while accepting premises with varying rigor across times and benches. Theater has increased over the 40-year interval as the doctrine has aged and its predictive power has declined relative to other factors (Justice composition, political moment). The measurement trajectory shows the doctrine becoming increasingly performative while extractiveness remains stable or slightly rising.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the contested boundary of intermediate scrutiny. The legislative sex classifier sees coordination (rope) — tailoring law to actual differences is legitimate lawmaking. The sex discrimination claimant sees pure extraction (snare) — the escape valve prevents them from winning even where the classification harms. The women's rights coalition sees tangled rope — they can litigate but the burden is wrong. The scientific consensus sees a temporary constraint with a sunset (scaffold) — as understanding of sex differences changes, the factual premises erode. The doctrinal apparatus sees its own degradation (piton) — the tier persists through inertia while its predictive power declines. The civilizational analytical observer risks seeing immutable constitutional law (mountain) — the doctrine correctly defers to real biological facts — but structural analysis reveals this as a false summit: the 'real difference' framing naturalizes what are contested empirical claims and contestable burden allocations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for legislative sex classifiers (beneficiary + institutional + arbitrage): Low d → low f(d) → low or negative experienced χ. Legislators experience the doctrine as enabling, not extractive. Directionality for sex discrimination claimants (victim + powerless + trapped): High d → high f(d) → high χ. Claimants experience the doctrine as maximally extractive — they face suppression and burden-shifting with no exit option. Directionality for organized advocates (victims + organized + constrained): Moderate-high d → moderate f(d) → moderate χ. The coalition has litigation agency but faces resource and evidentiary barriers. The burden-shifting asymmetry (legislatures make difference claims and courts defer; claimants must disprove them) is the key mechanism. This asymmetry is suppression in action — it is not that exit is impossible, but that the cost of exercising the existing exit (litigation) is systematically higher for claimants than for legislators.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by identifying where the tangled-rope classification applies: the doctrine contains both coordination (real differences matter) and extraction (burden-shifting favors legislators). The false summit risk is significant: if the analytical observer treats 'real differences' as a natural law (immutable biological fact requiring classification), the doctrine's extracted status is hidden. The resolution requires acknowledging that 'real differences' are empirical claims subject to change, burden-shifting is a structural feature that can be altered, and the doctrine is not a natural boundary but a constructed one that courts maintain. The mandatrophy is resolved by the staging of the tiers: intermediate scrutiny is not a natural law but a doctrinal choice, and the real differences escape valve is a secondary choice within that tier. Neither is immutable; both are contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_difference_evidentiary_standard,
    'What empirical evidence qualifies a sex difference as ''real'' enough to justify classification under intermediate scrutiny?',
    'Systematic analysis of cases invoking the real differences escape valve: comparison of accepted vs rejected difference claims; identification of implicit evidence standards across decisions; correlation with changes in scientific understanding of sex differences',
    'If standard is tight (strong empirical evidence required): doctrine operates as intended, enforcing scrutiny of the legislature''s factual premises. If standard is loose (legislative judgment sufficient): doctrine becomes a rebrand of rational basis review, and extractiveness increases sharply (0.48 → 0.62+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_difference_evidentiary_standard, empirical, 'Evidentiary standard for crediting real sex differences').

omega_variable(
    difference_premise_manipulation_vulnerability,
    'How often do courts credit legislative difference claims that subsequent scientific work demonstrates to be overstated, constructed, or contingent on social factors rather than inherent biology?',
    'Longitudinal case analysis: track difference premises from landmark cases (Rostker v. Goldberg, Michael M. v. Superior Court, Craig v. Boren, etc.) and compare to contemporary scientific understanding; measure rate of empirical invalidation',
    'If high rate of invalidation: the doctrine creates systematic extraction (legislature states a difference premise, courts defer, claimants lose, science later disproves the premise). If low rate: the doctrine''s empirical gatekeeping works and suppression is justified as enforcing accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(difference_premise_manipulation_vulnerability, empirical, 'Rate at which credited sex difference premises are later invalidated').

omega_variable(
    burden_shifting_asymmetry,
    'Does the real differences doctrine systematically shift burden of proof to claimants challenging the difference claim, even when the legislature bears burden of proof for other factual premises underlying rational basis review?',
    'Comparative doctrine analysis: examine burden allocation in rational basis cases (legislature establishes facts) vs intermediate scrutiny real differences cases (claimant must disprove difference); identify whether this asymmetry is acknowledged or implicit',
    'If asymmetry is real and unacknowledged: doctrine contains hidden suppression mechanism — claimants must prove negatives while legislatures prove positives. If explicit and justified: suppression is transparent and claimants can anticipate burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_shifting_asymmetry, conceptual, 'Asymmetric burden of proof regarding difference premises').

omega_variable(
    kernel_reading_contest__real_vs_tier_drift,
    'Is the intermediate scrutiny tier stable around the real differences doctrine (this reading), or does the tier itself drift as a function of bench composition and era, treating real differences as one factor among others rather than a fixed escape valve?',
    'This omega documents the contest between this reading (real differences as a doctrinal boundary) and the sibling tier_drift_question reading (the tier as a calibration dial). Historical analysis of doctrinal application: does the real differences premise remain constant while scrutiny level varies? Or does the real differences premise itself slide as scrutiny ratchets?',
    'If real differences is stable: this reading is defensible and the tier_drift_question reading coexists as an observational artifact. If the premise slides: the real differences doctrine is being used to rationalize outcomes determined by scrutiny drift, and this reading foreclosed the tier_drift reading (they cannot both be true — the tier is either a stable multi-level structure or a dial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__real_vs_tier_drift, conceptual, 'Whether real differences doctrine is stable or drifts with tier application').

omega_variable(
    kernel_reading_contest__real_vs_vmi_persuasive,
    'After VMI (requiring exceedingly persuasive justification for sex classifications), did the real differences doctrine become stricter (higher evidentiary bar for accepting differences) or was it simply rebranded (same gatekeeping, stronger rhetoric)?',
    'Longitudinal case outcome analysis: compare acceptance/rejection rates for difference premises before and after VMI; measure whether VMI''s ''exceedingly persuasive'' language shifted judicial behavior or was absorbed into existing practice',
    'If VMI tightened the standard: this reading (real differences) and the vmi_exceedingly_persuasive reading influence each other (vmi raised the bar for accepting differences under this doctrine). If VMI was rhetorical only: the readings coexist with identical practical effect, and the vmi reading is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__real_vs_vmi_persuasive, empirical, 'Whether VMI''s stricter language changed real differences gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intermediate_scrutiny_tier__real_differences_doctrine, 1976, 2016).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(realiff_tr_t0, intermediate_scrutiny_tier__real_differences_doctrine, theater_ratio, 0, 0.48).
narrative_ontology:measurement(realiff_tr_t20, intermediate_scrutiny_tier__real_differences_doctrine, theater_ratio, 20, 0.55).
narrative_ontology:measurement(realiff_tr_t40, intermediate_scrutiny_tier__real_differences_doctrine, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(realiff_be_t0, intermediate_scrutiny_tier__real_differences_doctrine, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(realiff_be_t20, intermediate_scrutiny_tier__real_differences_doctrine, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(realiff_be_t40, intermediate_scrutiny_tier__real_differences_doctrine, base_extractiveness, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intermediate_scrutiny_tier__real_differences_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__real_differences_doctrine, intermediate_scrutiny_tier__tier_drift_question).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__real_differences_doctrine, intermediate_scrutiny_tier__vmi_exceedingly_persuasive).

% DUAL FORMULATION NOTE:
% The intermediate scrutiny tier is a kernel with multiple readings. This story instantiates the real_differences_doctrine reading. The tier_drift_question reading observes that the tier itself drifts (intermediate scrutiny becomes a dial rather than a box). The vmi_exceedingly_persuasive reading follows Virginia Military Institute and reads the tier as stricter. All three readings are linked via the kernel constraint. They are not separate constraints but different interpretations of the same constitutional commitment (the tiered scrutiny structure). The network links show the genealogical and argumentative relationships: real differences doctrine is the baseline reading of intermediate scrutiny's escape valve; vmi reading arose to strengthen it; tier drift reading observes degradation or flexibility in all three tiers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
