% ============================================================================
% CONSTRAINT STORY: balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balancing_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balancing_reading
 *   human_readable: Balancing Reading of Speech Protection Boundary
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The balancing reading of speech protection holds that the First
 *   Amendment's scope varies with context. Speech receives protection
 *   determined through case-by-case adjudication that weighs First Amendment
 *   interests (speaker autonomy, democratic participation, truth-seeking)
 *   against other constitutional values (security, privacy, reputation,
 *   safety) and demonstrated concrete harms. This reading treats the
 *   protected/unprotected boundary as fluid rather than categorical. It
 *   produces a tangled-rope constraint: the framework includes genuine
 *   coordination of judicial authority (judges allocate cases, refine
 *   doctrine, calibrate application) alongside extractive features
 *   (unpredictability generates chilling effects, litigation costs become
 *   gating mechanisms, marginalized speakers face asymmetric uncertainty).
 *   The balancing reading is one of three structurally distinct commitments
 *   about how the speech-protection kernel should be interpreted. The
 *   absolutist reading holds that certain speech categories (political
 *   speech, core First Amendment expression) must be categorically protected
 *   regardless of consequences. The harm-limited reading holds that only
 *   speech causing direct, imminent, provable harm can be restricted. The
 *   balancing reading coexists with these alternatives — different factions
 *   in constitutional law, different judicial coalitions, and different
 *   historical periods instantiate each reading. The key structural
 *   difference: the balancing reading vests discretionary authority in the
 *   judiciary to determine application; it generates uncertainty that
 *   extracts chilling effects from risk-averse speakers; it requires
 *   continuous doctrinal performance to maintain legitimacy; it allocates
 *   relative benefit to institutional actors who navigate the doctrine
 *   (courts, regulators, repeat litigators) and relative cost to marginalized
 *   speakers who lack resources to litigate boundary cases.
 *
 * KEY AGENTS:
 *   - Marginalized Speaker: Primary victim (powerless/trapped) — speech rights are contingent on unpredictable case-by-case outcomes; cannot opt out of uncertainty; chilling effect from inability to predict judicial application
 *   - Organized Advocacy Group: Secondary victim/beneficiary (moderate/constrained) — faces litigation costs and timeline uncertainty; sometimes benefits from sympathetic judicial panels; sufficient resources to navigate the framework but at significant cost
 *   - Institutional Judiciary: Primary beneficiary (institutional/arbitrage) — balancing test allocates authority to courts; provides flexibility for outcome-calibration; coordinates judicial role without constraining outcomes via categorical rules
 *   - Speech Regulator: Secondary beneficiary (institutional/arbitrage) — government entities can defend restrictions through balancing framework; intermediate scrutiny permits substantial government interests to justify content regulation
 *   - Doctrine Administrator: Institutional actor (institutional/arbitrage) — law schools and judicial tradition maintain framework through doctrinal refinement; significant theatrical performance in applying three-tier framework; theater ratio increasing over time as doctrine becomes more complex
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the balancing choice as inevitable or necessary when it is actually one reading of how to interpret the speech-protection kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balancing_reading, 0.58).
domain_priors:suppression_score(balancing_reading, 0.68).
domain_priors:theater_ratio(balancing_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(balancing_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(balancing_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balancing_reading, tangled_rope).
narrative_ontology:human_readable(balancing_reading, "Balancing Reading of Speech Protection Boundary").
narrative_ontology:topic_domain(balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balancing_reading, '27004156-07f3-4cbb-87b2-1741744f17f4').
narrative_ontology:cs_created_at('27004156-07f3-4cbb-87b2-1741744f17f4', '').
narrative_ontology:cs_kernel_codification('27004156-07f3-4cbb-87b2-1741744f17f4', formalized).
narrative_ontology:cs_authority_grounding('27004156-07f3-4cbb-87b2-1741744f17f4', lineage).
narrative_ontology:cs_interpretation_layer_present('27004156-07f3-4cbb-87b2-1741744f17f4').
narrative_ontology:cs_kernel_id(balancing_reading, speech_protection_boundary).
narrative_ontology:cs_reading_relation('27004156-07f3-4cbb-87b2-1741744f17f4', absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('27004156-07f3-4cbb-87b2-1741744f17f4', harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('27004156-07f3-4cbb-87b2-1741744f17f4', foundational, context_dependent_protection_boundary).
narrative_ontology:cs_axiom_status(context_dependent_protection_boundary, holdable).
narrative_ontology:cs_axiom_grounding('27004156-07f3-4cbb-87b2-1741744f17f4', context_dependent_protection_boundary, instrumental).
narrative_ontology:cs_axiom('27004156-07f3-4cbb-87b2-1741744f17f4', foundational, judicial_discretion_required_for_application).
narrative_ontology:cs_axiom_status(judicial_discretion_required_for_application, holdable).
narrative_ontology:cs_axiom_grounding('27004156-07f3-4cbb-87b2-1741744f17f4', judicial_discretion_required_for_application, conventional).
narrative_ontology:cs_reference_frame('27004156-07f3-4cbb-87b2-1741744f17f4', post_brandenburg_balancing_doctrine).
narrative_ontology:cs_drift_state('27004156-07f3-4cbb-87b2-1741744f17f4', contemporary_complexity_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(balancing_reading, speech_regulators).
narrative_ontology:constraint_victim(balancing_reading, marginalized_speakers).
narrative_ontology:constraint_victim(balancing_reading, predictability_of_speech_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED SPEAKER (SNARE) — Speech rights are contingent on case-by-case judicial determination. The balancing test lacks predictable rules; speakers cannot know in advance whether their expression will be protected. No exit: speakers cannot opt out of the constitutional framework or avoid judicial review. Maximum uncertainty extraction — the constraint generates chilling effects through unpredictability.
constraint_indexing:constraint_classification(balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED ADVOCACY GROUP (TANGLED ROPE) — Faces uncertainty about which speech will be protected, but also benefits from the flexibility of case-by-case review: controversial positions sometimes win protection through sympathetic judicial panels, and the balancing framework has occasionally expanded rights beyond what categorical rules might permit. Constrained by litigation cost and timeline uncertainty, but not trapped. Benefits and costs both present.
constraint_indexing:constraint_classification(balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL JUDICIARY (ROPE) — The balancing test is a coordination mechanism that allocates adjudication authority to courts rather than categorical rules. Courts benefit from doctrinal flexibility and the ability to calibrate outcomes to specific circumstances. Experiences minimal extraction — the constraint coordinates the judiciary's own institutional role. Net beneficiary of the framework's authority allocation.
constraint_indexing:constraint_classification(balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SPEECH REGULATOR (ROPE) — Government entities seeking to restrict speech (schools, employers, public officials) benefit from the balancing framework's flexibility. When courts apply intermediate scrutiny to content regulation, regulators can often defend restrictions by demonstrating substantial government interests. The balancing test enables coordinated enforcement without needing categorical prohibitions on regulation. Net beneficiary.
constraint_indexing:constraint_classification(balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINE ADMINISTRATOR (PITON) — Law schools, casebooks, and judicial tradition maintain the balancing test through iterative refinement, but much of the work is theatrical: generating three-tier frameworks (strict scrutiny / intermediate / rational basis), labeling cases by test, and performing doctrinal consistency without resolving underlying disagreement about what speech deserves protection. The high theater ratio (0.65) reflects that doctrine work involves significant performance — writing opinions that 'apply the test' without the test meaningfully constraining outcomes.
constraint_indexing:constraint_classification(balancing_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the balancing of speech rights against other values is an inherent feature of any legal system that must simultaneously protect expression and prevent harm. No society can protect all speech absolutely while also protecting security, privacy, reputation, or safety. This structural feature appears unchangeable — balancing is what law necessarily does. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that the appearance of inevitability masks institutional choices about who wields the balancing power and what factors count.
constraint_indexing:constraint_classification(balancing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(balancing_reading, TR),
    TR >= 0.70.

:- end_tests(balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The balancing framework extracts from speakers through three mechanisms: (1) unpredictability of outcomes generates chilling effects even for legally protected speech; (2) litigation costs become gating mechanisms, excluding speakers without resources to litigate boundary cases; (3) the doctrine itself legitimates suppression by making regulation appear rationally reviewed rather than categorical discrimination. The 0.58 value reflects that extraction is real but not total — the framework sometimes expands protection beyond what categorical rules might permit, and some speakers benefit from sympathetic judicial interpretation. Over the 30-year interval (measuring from ~1995 to ~2025), extractiveness has increased from 0.42 to 0.58 as the doctrine has grown more complex and doctrinal performance has intensified. Suppression (0.68): Moderate-high. Speakers face genuine barriers to exercise of protected speech: uncertainty about outcomes, litigation cost, time required for adjudication, reputation risk from being classified as outside protection boundary, organizational barriers to advocacy that challenges regulatory authority. Suppression is partially structural (doctrinal rules create the uncertainty) and partially internalized (speakers avoid litigation through pre-litigation self-censorship based on uncertainty about outcomes). Theater ratio (0.65): Moderate-high. Doctrinal work involves significant performative content: three-tier scrutiny frameworks impose a ritual structure that suggests outcomes are constrained by the tests when in fact outcomes track judge ideology and case facts more strongly than doctrinal categories. The theater ratio has increased from 0.40 to 0.65 over the interval as judicial doctrine has become more baroque and the gap between doctrinal narrative and actual decision-making has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits all six types from different perspectives, making it a diagnostic exemplar for contested constitutional readings. The marginalized speaker sees a snare — unpredictability combined with inability to exit generates pure extraction. The organized advocacy group sees tangled rope — genuine benefits (doctrinal flexibility sometimes enables protection beyond categorical rules) coexist with extraction (litigation costs, uncertainty). The judiciary sees rope — balancing coordinates the judicial role and provides legitimate authority allocation. The regulator sees rope — balancing enables them to defend restrictions through rational review. The doctrine administrator sees piton — the framework persists through institutional inertia and theatrical performance despite declining functional verification that doctrine actually constrains outcomes. The civilizational analytical observer risks seeing mountain — the appearance that balancing is inevitable or necessary given the impossibility of protecting all speech against all harms. But the structural data reveals false-summit properties: the apparent inevitability of balancing masks that this is one optional reading of the speech-protection kernel, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position: the powerless speaker with no exit bears maximum extraction (d ≈ 0.95, high f(d)); the moderate organized group with constrained exit experiences moderate extraction (d ≈ 0.60, moderate f(d)); the institutional beneficiaries with arbitrage options experience low or negative extraction (d ≈ 0.15, low/negative f(d)); the analytical observer derives d from the canonical analyst value (d ≈ 0.72). The judiciary's arbitrage exit reflects that courts can interpret the doctrine to achieve preferred outcomes without formally violating the framework — they can apply balancing to reach almost any conclusion. Speech regulators similarly have arbitrage exit because the balancing framework permits them to defend regulation by identifying substantial government interests. The marginalized speaker has no arbitrage exit — they cannot reinterpret the framework to protect themselves; they must either self-censor or litigate, bearing both costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing reading resolves mandatrophy by showing that its tangled-rope classification is not a confusion between rope and snare, but a genuine hybrid: the framework contains real coordination (judicial authority allocation, doctrinal stability enabling law school teaching and legal practice) AND real extraction (unpredictability generating chilling effects, litigation costs gating participation, uncertainty asymmetries favoring institutional actors). The classification is not indeterminate — it is genuinely both. What could appear as mandatrophy (is this rope or snare?) is resolved by recognizing that it is tangled rope — a constraint that serves coordination functions for institutional actors while extracting from marginalized speakers. The perspectival gap (snare for powerless, rope for institutional, piton for doctrine administrator) is not classification confusion; it is the actual structure of the constraint. Different agents experience genuinely different constraint types because the constraint's functional properties vary across power positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_determinacy,
    'Does the balancing test produce predictable outcomes, or is it sufficiently indeterminate that similar cases yield inconsistent results?',
    'Longitudinal analysis of case clustering: do speakers/litigants with similar profiles receive consistent classifications across different circuits and time periods? Measurement of variance in outcomes for structurally similar cases.',
    'If determinacy is high (r² > 0.7 across cases): balancing might be rope (genuine coordination through stable doctrine). If determinacy is low (r² < 0.4): balancing is snare (extraction through unpredictability disguised as flexibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_test_determinacy, empirical, 'Whether balancing test produces predictable outcomes').

omega_variable(
    judicial_preference_capture,
    'To what extent do balancing test outcomes reflect the ideological composition of the court rather than stable principles?',
    'Regression analysis of outcomes against judge ideology scores (Segal-Cover or DW-NOMINATE); comparison of same-case reasoning across different panels.',
    'High correlation with judge ideology: the balancing test is a vehicle for extracting doctrinal legitimacy from outcome-driven reasoning (snare properties). Low correlation: balancing reflects genuine interpretive pluralism (rope properties).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_preference_capture, empirical, 'Extent to which outcomes reflect judge ideology').

omega_variable(
    speech_category_reification,
    'Does the balancing framework actually treat speech categories (political, commercial, obscene, etc.) as legally meaningful, or do the category labels function as post-hoc narrative cover for outcome-driven decisions?',
    'Doctrinal archaeology: trace how speech categories emerge historically; analyze whether category membership predicts outcomes independent of judge ideology and case facts.',
    'If categories are reified: balancing coordinates the judiciary around stable doctrinal categories (rope). If categories are post-hoc: the balancing test is a performance of objectivity masking discretionary power (piton or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_category_reification, conceptual, 'Whether speech categories are meaningful or performative').

omega_variable(
    alternative_framework_availability,
    'Would categorical rules (absolutist protection for some speech, strict categorical exclusions for other speech) actually reduce uncertainty and extraction compared to balancing, or do absolutist frameworks create different extraction mechanisms?',
    'Comparative analysis: examine jurisdictions or periods that used categorical rules vs balancing (e.g., pre-Brandenburg categorical approach vs post-Brandenburg balancing); measure speaker confidence, litigation cost, and regulatory compliance under each framework.',
    'If categorical rules reduce uncertainty: balancing_reading is snare (extraction through unpredictability) and the absolutist_reading offers genuine alternative. If categorical rules create different unpredictability: balancing_reading and absolutist_reading both have extraction mechanisms; choice between them is value-dependent, not empirical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_availability, empirical, 'Whether categorical rules would reduce extraction compared to balancing').

omega_variable(
    committer_reading_ambiguity,
    'Is this constraint one reading of the speech-protection-boundary kernel, or is it an attempt to naturalize one reading as constitutional law itself?',
    'Doctrinal history: trace whether balancing was explicitly adopted as a methodological choice or emerges as a post-hoc description of what judges do. Examine whether the balancing framework is defended as ''better doctrine'' or as inevitable given First Amendment text.',
    'If balancing is an explicit methodological choice: the constraint is a genuine reading that coexists with alternatives. If balancing is naturalized: the constraint exhibits false-summit properties — it presents contingent institutional arrangements as logical necessities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_ambiguity, conceptual, 'Whether balancing is an explicit reading or naturalized inevitability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balancing_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bala_tr_t0, balancing_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bala_tr_t15, balancing_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(bala_tr_t30, balancing_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(bala_be_t0, balancing_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bala_be_t15, balancing_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(bala_be_t30, balancing_reading, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(balancing_reading, absolutist_reading).
narrative_ontology:affects_constraint(balancing_reading, harm_limited_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel has three distinct readings: balancing_reading (this file), absolutist_reading, and harm_limited_reading. Each reading has its own ε value and perspectival classification structure. They form a kernel family linked by reading_relations (coexists_with / influences). The balancing_reading generates tangled_rope as primary type; the absolutist_reading would generate rope or mountain; the harm_limited_reading would generate rope or tangled_rope with different victim/beneficiary structure. Each reading treats the protected/unprotected boundary differently: balancing (context-dependent), absolutist (categorical protection), harm-limited (harm-triggered restriction). Decomposition per ε-invariance: the ε values differ because each reading's structural mechanism for determining speech protection operates differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(balancing_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
