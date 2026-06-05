% ============================================================================
% CONSTRAINT STORY: intermediate_scrutiny_tier__vmi_exceedingly_persuasive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intermediate_scrutiny_tier__vmi_exceedingly_persuasive, []).

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
 *   constraint_id: intermediate_scrutiny_tier__vmi_exceedingly_persuasive
 *   human_readable: VMI Intermediate Scrutiny: Exceedingly Persuasive Justification Standard for Sex Classification
 *   domain: constitutional_law/equal_protection/doctrinal
 *
 * SUMMARY:
 *   The intermediate scrutiny tier for sex classification was established in
 *   Craig v Boren (1976) and significantly heightened in United States v
 *   Virginia (VMI, 1996). This constraint instantiates the VMI reading of the
 *   contested kernel 'intermediate_scrutiny_tier': the state's justification
 *   for sex-based classification must be 'exceedingly persuasive,' not merely
 *   'important,' and cannot rest on overbroad generalizations about the
 *   sexes. This reading constrains the state by requiring tailored,
 *   empirically grounded justifications for any sex line, while creating
 *   extraction pressure on single-sex institutional arrangements that relied
 *   on tradition or assumed biological differences. The constraint exhibits a
 *   strong perspectival gap: the excluded member sees a snare (trapped by
 *   classification, bearing justification burden); the sex-equality coalition
 *   sees a tangled rope (beneficiary post-1996, but constrained by remaining
 *   escape valves); the state legislator sees a rope (clear framework,
 *   navigable if differences are real); the federal judge sees a tangled rope
 *   (powerful but constrained by ambiguous 'exceedingly persuasive'
 *   boundary); the single-sex institution sees a piton (tradition-based
 *   identity degraded, persisting through institutional inertia); the
 *   civilizational analyst risks seeing a mountain (sex categories as
 *   natural) but this is likely a false summit that naturalizes
 *   constitutional doctrine. The measurement trajectory shows clear doctrine
 *   shift at VMI (1996): extraction pressure nearly doubles (0.25 → 0.42) and
 *   suppression of stereotype-based justification rises from 0.35 to 0.62,
 *   remaining stable through 2006. Theater ratio increases modestly (0.28 →
 *   0.38), reflecting post-hoc justification language replacing
 *   straightforward tradition-based defense. The 'exceedingly persuasive
 *   justification' standard is the doctrinal mechanism that instantiates this
 *   extraction and suppression: it formally requires states to do discursive
 *   work that was previously implicit.
 *
 * KEY AGENTS:
 *   - Sex-equality claimants (beneficiary, organized/constrained): Coalition of individuals and advocacy groups challenging sex classifications post-Craig; positioned as beneficiaries by VMI's heightened standard; constrained by real-differences escape valve and litigation costs
 *   - Single-sex institutional arrangements (victim, institutional/arbitrage): Schools, military academies, gender-based programs relying on tradition; must now meet 'exceedingly persuasive' standard; have exit options (integrate, restructure) but institutional inertia maintains single-sex model
 *   - Disadvantaged sex groups (victim, powerless/trapped): Members excluded from single-sex institutions on basis of sex; trapped by classification; bear burden of challenge; must absorb exclusion while state produces justification
 *   - Federal judges (powerful/mobile): Interpret and apply 'exceedingly persuasive' standard; control the justification gate; experience constraint as both coordination (stable doctrine post-VMI) and asymmetry (burden ambiguity creates discretion)
 *   - State legislators (institutional/arbitrage): Defend single-sex programs; have options to integrate or meet standard; experience VMI as either navigable (if real differences exist) or extractive (if justification is impossible without relying on stereotypes)
 *   - Analytical observer (analytical/analytical): Risks naturalizing the constraint as inherent to sex categories rather than recognizing it as constitutional doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, 0.48).
domain_priors:suppression_score(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, 0.62).
domain_priors:theater_ratio(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, extractiveness, 0.48).
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, tangled_rope).
narrative_ontology:human_readable(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, "VMI Intermediate Scrutiny: Exceedingly Persuasive Justification Standard for Sex Classification").
narrative_ontology:topic_domain(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, "constitutional_law/equal_protection/doctrinal").

domain_priors:requires_active_enforcement(intermediate_scrutiny_tier__vmi_exceedingly_persuasive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, '76c02348-821c-4973-ab32-85247147afa8').
narrative_ontology:cs_kernel_codification('76c02348-821c-4973-ab32-85247147afa8', formalized).
narrative_ontology:cs_authority_grounding('76c02348-821c-4973-ab32-85247147afa8', lineage).
narrative_ontology:cs_interpretation_layer_present('76c02348-821c-4973-ab32-85247147afa8').
narrative_ontology:cs_reading_relation('76c02348-821c-4973-ab32-85247147afa8', intermediate_scrutiny_tier__real_differences_doctrine, influences).
narrative_ontology:cs_reading_relation('76c02348-821c-4973-ab32-85247147afa8', intermediate_scrutiny_tier__tier_drift_question, coexists_with).
narrative_ontology:cs_axiom('76c02348-821c-4973-ab32-85247147afa8', foundational, exceedingly_persuasive_justification_required).
narrative_ontology:cs_axiom_status(exceedingly_persuasive_justification_required, holdable).
narrative_ontology:cs_axiom_grounding('76c02348-821c-4973-ab32-85247147afa8', exceedingly_persuasive_justification_required, deontological).
narrative_ontology:cs_axiom('76c02348-821c-4973-ab32-85247147afa8', foundational, overbroad_generalizations_suppressed).
narrative_ontology:cs_axiom_status(overbroad_generalizations_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('76c02348-821c-4973-ab32-85247147afa8', overbroad_generalizations_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('76c02348-821c-4973-ab32-85247147afa8', post_vmi_heightened_tier).
narrative_ontology:cs_drift_state('76c02348-821c-4973-ab32-85247147afa8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('76c02348-821c-4973-ab32-85247147afa8', '').
narrative_ontology:cs_kernel_id(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, intermediate_scrutiny_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, sex_equality_claimants).
narrative_ontology:constraint_beneficiary(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, disadvantaged_sex_groups).
narrative_ontology:constraint_victim(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, single_sex_institutional_arrangements).
narrative_ontology:constraint_victim(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, tradition_based_gender_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MEMBER (SNARE) — Cannot exit the classification system; bears full cost of the 'exceedingly persuasive justification' gate. Must absorb institutional exclusion while the burden of justification sits with the state. Trapped by legal status and institutional design. Maximum experienced extraction — no alternatives, no exit cost mitigation.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SEX-EQUALITY ADVOCACY COALITION (TANGLED ROPE) — Beneficiary post-1996 (VMI raised the middle tier and constrained state justifications); but constrained by ongoing doctrinal contests and real-differences escape valve. Experiences genuine coordination (precedent creates stable framework for challenging classifications) and asymmetric extraction (must litigate each institution separately; burden of proof asymmetry still extracts resources and time). Significant agency but not full mobility.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE LEGISLATOR / INSTITUTIONAL DEFENDER (ROPE) — Experiences VMI constraint as coordination mechanism: the 'exceedingly persuasive justification' standard provides a clear framework for tailoring programs. If genuine real differences exist, the constraint is navigable without contradiction. Arbitrage exit available (restructure program, integrate, or meet the justification bar). Net beneficiary when the state can articulate authentic educational differences.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL JUDGE (TANGLED ROPE) — Mobile (can interpret precedent with discretion within the frame); powerful (controls the justification gate). Experiences both coordination (VMI provides binding standard post-1996) and extraction (the 'exceedingly persuasive' framing is internally contested — judges disagree on what constitutes sufficient justification; burden asymmetry favors challengers but leaves room for doctrinal resistance). Moderate chi — the judge has significant agency and benefits from precedential clarity, but is constrained by the heightened standard's ambiguity.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SINGLE-SEX INSTITUTION (PITON) — Institutional actor with arbitrage exit (merge, restructure, redefine educational mission) but experiencing the constraint as increasingly performative. The institution must now produce 'exceedingly persuasive' justifications for practices previously naturalized as tradition. Theater ratio high because the justification process often becomes post-hoc rationalization of inherited arrangements rather than original educational reasoning. The institution sees its own tradition-based identity as degraded (must be defended anew) but persists through inertia and alumni commitment.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, sex-based classification might appear as a fundamental and immutable feature of human organization — a category that cannot be questioned without collapsing into confusion. The 'exceedingly persuasive justification' standard, from this view, is merely recognizing that some sex lines are natural and require no justification beyond their biological substrate. However, this classification is likely a false summit: the VMI ruling explicitly rejects overbroad generalizations about the sexes and demands tailored justification, which reveals the constraint as doctrinal (constructed) rather than natural. The engine's false-summit detector should flag this perspective as naturalizing a contingent legal doctrine.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intermediate_scrutiny_tier__vmi_exceedingly_persuasive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, TR),
    TR >= 0.70.

:- end_tests(intermediate_scrutiny_tier__vmi_exceedingly_persuasive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, rising from 0.25 pre-VMI. The VMI constraint extracts from single-sex institutions by requiring them to justify arrangements that were previously naturalized. The state must now produce detailed, empirically grounded reasons for sex classifications. However, extraction is not maximal (not 0.66+) because the 'real differences' escape valve provides a legitimate exit path — if genuine differences exist, the constraint is navigable. The 0.48 value reflects that VMI raised the bar but did not eliminate sex-based classification. Measurement shows clear shift at 1996 (VMI decision). Suppression (0.62): High. The constraint explicitly suppresses 'overbroad generalizations about the sexes' and requires tailored justification. States cannot defend sex lines on the basis of broad, undifferentiated claims about male/female capabilities or roles. This suppression is substantial and post-VMI is durable (stable 0.62 through 2006). Suppression is enforced by appellate review scrutiny and the discursive requirement to articulate 'exceedingly persuasive' rationales. Theater ratio (0.38): Moderate. VMI creates significant discursive burden, and post-hoc justifications (articulating historical practice as intentional policy based on real differences) are increasingly performative. However, the constraint is not primarily a theater mechanism — the functional constraint (requirement to meet the justification standard) is real. The theater derives from the gap between the constraint's formal requirement and the difficulty of articulating non-stereotypical 'real differences,' especially in older institutions built on tradition rather than empirical distinction. Single-sex institutions must now engage in discursive rationalization where practice previously stood alone.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a full spectrum of classification from the same structural base. The excluded member sees snare: trapped by sex classification, bearing justification burden, no exit. The sex-equality coalition sees tangled rope: beneficiary post-VMI, constrained by real-differences escape valve and ongoing litigation costs. The state legislator sees rope: clear framework post-VMI, navigable if genuine differences exist, coordination function clear. The federal judge sees tangled rope: powerful position but constrained by ambiguous standard ('exceedingly persuasive' is not mathematically specified), experiences both coordination (doctrine clarifies expectations) and extraction asymmetry (burden on state but discretion remains). The single-sex institution sees piton: tradition-based identity degraded by requirement to justify arrangements, persisting through institutional inertia and alumni/donor support. The civilizational analyst risks seeing mountain: sex categories as immutable and therefore requiring no doctrinal justification. The gap reveals that perspectival classification is not a flaw but a feature — the constraint's structure genuinely supports different readings depending on the observer's structural position and temporal horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Sex-equality claimants (beneficiaries + organized/constrained exit) derive lower d, indicating they benefit and have some agency. Single-sex institutions and excluded members (victims + institutional/trapped exit) derive higher d, indicating they bear extraction and have limited exit. The federal judge (powerful/mobile) derives moderate d, reflecting significant discretion but also constraint from binding precedent. State legislators (institutional/arbitrage) derive lower-to-moderate d, depending on whether genuine differences exist (high arbitrage exit reduces d). The effective extractiveness chi is computed from ε × f(d) × σ(S), where f(d) scales the extractiveness by the sigmoid directionality function and σ(S) scales by national scope. Excluded members experience highest chi (trapped exit, victim status). Sex-equality claimants experience lower chi (beneficiary status, organized exit). The piton classification derives from high theater ratio (0.38) rather than high chi, reflecting that single-sex institutions experience the constraint as performative justification work rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the VMI reading instantiates a genuine tangled-rope constraint with both coordination and asymmetric extraction functions. The coordination function is real: post-VMI, states and institutions have a clear framework for evaluating sex classifications — the 'exceedingly persuasive justification' standard provides stable doctrine. The asymmetric extraction is also real: the burden of proof is on the state; sex-equality claimants benefit from the heightened standard; single-sex institutions and excluded members bear the justification burden. The constraint is neither pure extraction (snare) nor pure coordination (rope), but a hybrid that achieves sex-equality advancement (coordination benefit for claimants) through asymmetric imposition of discursive burden on states and institutions (extraction mechanism). The false-summit perspective (mountain/analytical/civilizational) is a diagnostic signal — it reveals the risk of naturalizing constitutional doctrine as immutable law. The real debate is not 'is intermediate scrutiny natural?' but 'is the VMI heightening of the middle tier a permanent doctrinal ceiling or a contingent application that will drift in future eras?' Omegas on tier drift and real-differences boundary acknowledge the ongoing contestation within the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_differences_boundary_ambiguity,
    'What constitutes a ''real difference'' between the sexes sufficient to justify differential treatment under VMI''s exceedingly persuasive standard?',
    'Longitudinal analysis of Supreme Court and federal appellate decisions post-VMI: which institutional differences (physical capability, leadership pipeline, cultural tradition, statistical tendency) courts have accepted vs rejected as ''real'' and ''exceedingly persuasive''',
    'If ''real difference'' standard becomes permissive: VMI tier collapses toward intermediate scrutiny baseline (lower extraction). If standard remains strict: VMI tier''s heightened suppression of stereotype-based justification persists (higher extraction from single-sex institutions). The doctrinal boundary determines whether the constraint is tangled_rope or transitions toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_differences_boundary_ambiguity, empirical, 'Permissiveness of ''real difference'' doctrine as escape valve').

omega_variable(
    tier_drift_versus_stable_doctrine,
    'Is the VMI standard a stable heightened tier for sex classification, or does it drift in application across institutional contexts (single-sex education, military, healthcare, sports)?',
    'Doctrinal analysis of circuit splits and institutional variations in VMI application; measurement of suppression burden across contexts (education vs military vs athletics); identification of whether courts apply ''exceedingly persuasive'' uniformly or contextually',
    'If standard drifts: VMI is not a stable doctrinal ceiling but a rhetorical frame that shifts with institutional pressure (renders the constraint piton-like, increasingly performative). If standard stabilizes: VMI is a durable constitutional threshold that meaningfully constrains state justifications (tangled_rope with sustained extraction suppression). Drift measurement goes to whether the reading instantiates a real doctrinal tier or a contested domain where application varies by bench and era.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tier_drift_versus_stable_doctrine, empirical, 'Whether VMI standard drifts across institutional and temporal contexts').

omega_variable(
    stereotype_suppression_mechanism,
    'Does the VMI constraint''s rejection of ''overbroad generalizations about the sexes'' actually suppress stereotype-based justification, or does it merely require states to articulate stereotypes as ''real differences''?',
    'Rhetorical and doctrinal analysis of post-VMI state justifications: do states continue to rely on implicit stereotypes while framing them as empirical differences? Comparison of pre-VMI and post-VMI language in institutional briefs and state legislative records.',
    'If suppression is effective: VMI constraint genuinely suppresses crude stereotype-based reasoning (supporting tangled_rope classification — asymmetric but real extraction). If suppression is nominal: states simply re-label stereotypes as ''real differences'' and the constraint becomes piton (performative — the ritual of justification matters more than its content). This determines whether suppression value (0.62) reflects actual barriers or theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stereotype_suppression_mechanism, empirical, 'Effectiveness of stereotype suppression under VMI standard').

omega_variable(
    false_summit_detection_signal,
    'Is this constraint''s mountain perspective (analytical/civilizational) a genuine natural law about sex categories, or a false summit that naturalizes a contingent constitutional doctrine?',
    'Doctrinal genealogy: trace whether the ''exceedingly persuasive justification'' standard emerges from discovered natural limits of sex categories or from constitutional doctrine (equal protection jurisprudence post-Craig v Boren, refined by VMI 1996). If genealogy is doctrinal, mountain perspective is false summit and should be reclassified by engine signature override.',
    'If true natural law: sex classification does rest on immutable biological or structural facts (supports mountain from analytical perspective). If false summit: the constraint is doctrinal construction that benefits certain agents (sex-equality claimants) and constrains others (single-sex institutions); engine''s false-summit detector should reclassify to tangled_rope or snare. This omega documents the ambiguity that triggers FSM in the engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_detection_signal, conceptual, 'Whether analytical mountain perspective instantiates false summit').

omega_variable(
    reading_specificity_vmi_versus_tier_baseline,
    'Does VMI (1996) represent a genuine elevation of the intermediate scrutiny tier, or a clarification of the tier''s pre-existing standard?',
    'Doctrinal genealogy: comparison of intermediate scrutiny language in Craig v Boren (1976) and subsequent sex-classification cases versus VMI''s ''exceedingly persuasive justification'' formulation. Analysis of whether courts pre-VMI were already applying this standard or whether VMI introduced new doctrinal content.',
    'If VMI is elevation: the constraint marks a real doctrinal shift (1996 turning point) that increased extraction pressure on single-sex arrangements. If VMI is clarification: the constraint describes the tier''s standing rules more precisely but does not constitute a new structural burden. This determines whether the extractiveness value (0.48) reflects a VMI-specific doctrinal shift or the baseline intermediate scrutiny tier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specificity_vmi_versus_tier_baseline, empirical, 'Whether VMI elevated or clarified the intermediate scrutiny standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, 1976, 2006).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmi_theater_1976_baseline, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, theater_ratio, 0, 0.28).
narrative_ontology:measurement(vmi_theater_1996_post_vmi, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vmi_theater_2006, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(vmi_extract_1976_baseline, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vmi_extract_1996_post, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(vmi_extract_2006, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(vmi_suppress_1976_baseline, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vmi_suppress_1996_post_vmi, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(vmi_suppress_2006, intermediate_scrutiny_tier__vmi_exceedingly_persuasive, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, enforcement_mechanism).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, real_differences_doctrine).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, tier_drift_question).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, single_sex_education_viability).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__vmi_exceedingly_persuasive, military_gender_integration).

% DUAL FORMULATION NOTE:
% VMI reading of intermediate_scrutiny_tier is one of three structurally distinct constraint stories describing the contested kernel. The three readings have different ε values and different beneficiary/victim structures. This story (vmi_exceedingly_persuasive, ε=0.48) instantiates the elevation thesis post-1996. The real_differences_doctrine story emphasizes the escape valve and may carry lower extraction. The tier_drift_question story emphasizes instability and contestation across contexts. All three are linked as siblings representing competing readings of the same doctrinal kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
